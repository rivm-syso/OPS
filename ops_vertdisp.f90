!-------------------------------------------------------------------------------------------------------------------------------
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
!                       Copyright (C) 2002 by
!   National Institute of Public Health and Environment
!           Laboratory for Air Research (RIVM/LLO)
!                      The Netherlands
!
! SUBROUTINE
! NAME               : %M%
! SCCS (SOURCE)      : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH - SEQUENCE  : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : HvJ/Franka Loeve (Cap Volmac)
! FIRM/INSTITUTE     : RIVM/LLO
! LANGUAGE           : FORTRAN-77/90
! DESCRIPTION        : Calculation of vertical dispersion coefficient as a function of stability parameters and downwind distance
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP Fortran
! CALLED FUNCTIONS   :
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_vertdisp(z0, zi, ol, uster, hh, x, uh, zu, sz, error)

USE m_commonconst                                                              ! EPS_DELTA only
USE m_error

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_vertdisp')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: z0                         ! roughness length (m)
REAL*4,    INTENT(IN)                            :: zi                         ! mixing height (m)
REAL*4,    INTENT(IN)                            :: ol                         ! Monin-Obukhov length  (m)
REAL*4,    INTENT(IN)                            :: uster                      ! friction velocity (m)
REAL*4,    INTENT(IN)                            :: hh                         ! source heigth, including plume rise (m)
REAL*4,    INTENT(IN)                            :: x                          ! downwind distance  (m)

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: uh                         ! windspeed at downwind distance x and height zu (m/s)
REAL*4,    INTENT(OUT)                           :: zu                         ! representative plume height (m), taking into account reflection 
                                                                               ! at the top of the mixing layer and at the ground surface
REAL*4,    INTENT(OUT)                           :: sz                         ! vertical dispersion coefficient (m)

TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record


! LOCAL VARIABLES
REAL*4                                           :: h                          ! bronhoogte (m)
REAL*4                                           :: szc                        ! convexe dispersie (m)
REAL*4                                           :: szn                        ! neutrale dispersie
REAL*4                                           :: szs                        ! oppervlakte dispersie
REAL*4                                           :: fm                         ! 
REAL*4                                           :: fs                         ! 

! SUBROUTINE AND FUNCTION CALLS
EXTERNAL ops_surface
EXTERNAL ops_convec
EXTERNAL ops_neutral

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------

! Compute vertical dispersion coefficient sigma_z for different conditions as in figure 3.1 OPS report

! Dispersion above mixing height zi equals dispersion at z=0.9*zi
IF (hh .GT. (0.9*zi + EPS_DELTA)) THEN
   h = 0.9*zi
ELSE
   h = hh
ENDIF

!
! Compute fm = interpolation factor between surface layer/mixing layer 960202
!
fm = AMIN1(AMAX1((h/zi/0.1)-0.5, 0.), 1.)  ! plaatje

IF (fm .GT. 0. ) THEN
!
!  fm > 0 -> h/(zi*0.1) > 0.5 -> h > 0.2*zi -> stack_height > 0.2*mixing_height, so stack in mixing layer
!
   ! zi/L < 0 -> combination of convective and near neutral layer
   IF ( zi/ol .LT. 0 - EPS_DELTA ) THEN
                                           
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
      CALL ops_convec(z0,zi,ol,uster,h,x, uh, zu, szc)
      CALL ops_neutral(z0,zi,ol,uster,h,x, uh, zu, szn)
      sz = fs*szc + (1. - fs)*szn
   ELSE
    
     ! zi/L > 0 -> near neutral upper layer:
     CALL ops_neutral(z0,zi,ol,uster,h,x, uh, zu, szn)
     sz = szn
   ENDIF

   ! Interpolate between surface layer and mixing layer:
   IF ( fm .LT. 1. - EPS_DELTA) THEN
     CALL ops_surface(z0,zi,ol,uster,h,x, uh, zu, szs)
     sz = fm*sz + (1. - fm)*szs
   ENDIF
ELSE
  
  !  fm = 0 -> stack_height <= 0.2*mixing_height, so stack in surface layer:
  CALL ops_surface(z0,zi,ol,uster,h,x, uh, zu, szs)
  sz = szs
ENDIF
if (error%debug) write(*,'(3a,2(1x,i6),99(1x,e12.5))') trim(ROUTINENAAM),',A,', &
   ' ircp,istab,h,zi,ol,h/zi/0.1,zi/ol,fm,fs,szs,szc,szn:', &
     -999,-999,h,zi,ol,h/zi/0.1,zi/ol,fm,fs,szs,szc,szn

RETURN
END SUBROUTINE ops_vertdisp
