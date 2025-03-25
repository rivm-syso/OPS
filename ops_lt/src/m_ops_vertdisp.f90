!-------------------------------------------------------------------------------------------------------------------------------
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
!

!-------------------------------------------------------------------------------------------------------------------------------
! DESCRIPTION        : Calculation of vertical dispersion coefficient as a function of stability parameters and downwind distance
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_vertdisp

implicit none

contains

SUBROUTINE ops_vertdisp(varin, z0, zi, ol, uster, hh, x, ircp, istab, debugnam, uh, zu, sz, error)

use m_ops_varin, only: TVarin
use m_commonconst_lt                                                              ! EPS_DELTA only
use m_error
use m_ops_convec
use m_ops_neutral
use m_ops_surface

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_vertdisp')

! SUBROUTINE ARGUMENTS - INPUT
TYPE(Tvarin), INTENT(IN)                         :: varin                ! input variables for meteo
REAL,      INTENT(IN)                            :: z0                         ! roughness length (m)
REAL,      INTENT(IN)                            :: zi                         ! mixing height (m)
REAL,      INTENT(IN)                            :: ol                         ! Monin-Obukhov length  (m)
REAL,      INTENT(IN)                            :: uster                      ! friction velocity (m)
REAL,      INTENT(IN)                            :: hh                         ! source heigth, including plume rise (m)
REAL,      INTENT(IN)                            :: x                          ! downwind distance  (m)
INTEGER,   INTENT(IN)                            :: ircp                       ! index of receptorpoint (here only used in debug write statement)
INTEGER,   INTENT(IN)                            :: istab                      ! index of stability class (here only used in debug write statement)
CHARACTER*(*), INTENT(IN)                        :: debugnam                   ! name for debug write statement since ops_vertdisp is called 3 times in various circumstances: either at_areasource, at_rcp_with_meteo_src, at_rcp_with_meteo_rcp  
! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: uh                         ! windspeed at downwind distance x and height zu (m/s)
REAL,      INTENT(OUT)                           :: zu                         ! representative plume height (m), taking into account reflection 
                                                                               ! at the top of the mixing layer and at the ground surface
REAL,      INTENT(OUT)                           :: sz                         ! vertical dispersion coefficient (m)

TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record


! LOCAL VARIABLES
REAL                                             :: h                          ! bronhoogte (m)
REAL                                             :: szc                        ! sigma_z for convective layer (m)
REAL                                             :: szn                        ! sigma_z for near neutral layer (m)
REAL                                             :: szs                        ! sigma_z for surface layer (m)
REAL                                             :: fm                         ! interpolation factor between surface layer/mixing layer. 0 < fm < 1 -> transition layer between surface layer (fm = 0) and upper layer (fm = 1; convective or neutral).
REAL                                             :: fs                         ! interpolation factor between convective/near neutral layer. 0 < fs < 1 -> linear interpolation between convective mixing layer (fs = 1) and near neutral upper layer (fs = 0).

!-------------------------------------------------------------------------------------------------------------------------------

! Compute vertical dispersion coefficient sigma_z for different conditions as in figure 3.1 OPS report

! Initialize fs, szs, szc, szn as they need to be defined for writing output when error%debug = .TRUE.
fs = 0.0
szs = 0.0
szc = 0.0
szn = 0.0

! Dispersion above mixing height zi equals dispersion at z=0.9*zi
IF (hh .GT. (0.9*zi + EPS_DELTA)) THEN
   h = 0.9*zi
ELSE
   h = hh
ENDIF


!
! Compute fm = interpolation factor between surface layer/mixing layer 960202
!
!     1 |                   ................
!       |                  .
!       |                 .
!    fm |                .
!       |               .
!       |              .
!       |             .
!       |            .
!       |           .
!       |          .             
!   0.5 |         x     x denotes the point h/zi = 0.1, fm = 0.5 
!       |        .      (central in the transition layer between surface and upper layer)              
!       |       .                  
!       |      .                      
!       |     .                         
!     0 |....|----|----|----|----|----|-------------
!       0   0.05 0.1  0.15 0.2  0.25 0.3    h/zi ->
!
! 1. h/zi <= 0.05       -> fm = 0     -> 'pure' surface layer
! 2. h/zi > 0.15        -> fm = 1     -> upper layer
! 3. 0.05 < h/zi < 0.15 -> 0 < fm < 1 -> transition layer between surface layer and upper layer (convective or neutral).
!
fm = AMIN1(AMAX1((h/zi/0.1)-0.5, 0.), 1.)

IF (fm .GT. 0. ) THEN

   ! fm > 0 -> h/(zi*0.1) > 0.5 -> h > 0.05*zi -> stack_height > 0.05*mixing_height, so stack in transition layer or in upper layer

   ! Compute sigma_z for upper layer
   IF ( zi/ol .LT. 0 - EPS_DELTA ) THEN
                                           
      ! zi/L < 0 -> combination of convective and near neutral layer
      ! Compute fs = interpolation factor between convective/near neutral layer, fs = -0.05*zi/L          960118
      ! and limit fs such that 0 <= fs <= 1.
      ! 1. zi/L <= -20    -> fs = 1     -> convective mixing layer
      ! 2. zi/L = 0       -> fs = 0     -> near neutral upper layer
      ! 3. -20 < zi/L < 0 -> 0 < fs < 1 -> linear interpolation between convective mixing layer and near neutral upper layer
      fs = (zi/ol)/(-10)*0.5
      IF ( fs .GT. 1. + EPS_DELTA) THEN
        fs = 1.
      ELSE IF ( fs .LT. 0 - EPS_DELTA) THEN
        fs = 0.
      ELSE
        CONTINUE
      ENDIF

      ! Compute vertical dispersion coefficient sigma_z for convective and near neutral layer 
      ! and interpolate:
      CALL ops_convec(varin%varin_meteo, z0,zi,ol,uster,h,x, uh, zu, szc)
      CALL ops_neutral(varin%varin_meteo, z0,zi,ol,uster,h,x, uh, zu, szn)
      sz = fs*szc + (1. - fs)*szn
   ELSE
    
     ! zi/L > 0 -> compute sigma_z for near neutral upper layer:
     CALL ops_neutral(varin%varin_meteo, z0,zi,ol,uster,h,x, uh, zu, szn)
     sz = szn
   ENDIF ! {sz for upper layer has been omputed}

   IF ( fm .LT. 1. - EPS_DELTA) THEN
     ! 0 < fm < 1 or 0.05 < h/zi < 0.15: interpolate between sigma_z for upper layer (sz) and sigma_z for surface layer (szs):
     CALL ops_surface(varin%varin_meteo, z0,zi,ol,uster,h,x, uh, zu, szs)
     sz = fm*sz + (1. - fm)*szs
   ENDIF
ELSE
  
  !  fm = 0 -> stack_height <= 0.05*mixing_height, so stack in 'pure' surface layer:
  CALL ops_surface(varin%varin_meteo, z0,zi,ol,uster,h,x, uh, zu, szs)
  sz = szs
ENDIF

!Edit
! Adjustment relevant for sensitivity analyses. Multiplication factor is 1.0 by default.
sz = varin%varin_unc%sigz_fact * sz
!End Edit

! Note: not all local variables are defined always (fs, szc)
if (error%debug) write(*,'(5a,2(1x,i6,";"),99(1x,e12.5,";"))') trim(ROUTINENAAM),',',debugnam,'; ', &
   'ircp; istab; x; h; zi; ol; h/zi/0.1; zi/ol; fm; fs; szs; szc; szn; varin%varin_unc%sigz_fact; sz; ', &
    ircp, istab, x, h, zi, ol, h/zi/0.1, zi/ol, fm, fs, szs, szc, szn, varin%varin_unc%sigz_fact, sz

RETURN
END SUBROUTINE ops_vertdisp

end module m_ops_vertdisp
