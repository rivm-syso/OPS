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
! DESCRIPTION        : This routine calculates sigmaz for near neutral cases according to Gryning et al. (1987).
!                      Formally defined for (-10<z/L<1 and 0.1<h/zi<0.8) but also applied for (-10<z/L) and 0.1<h/zi<1)
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_neutral

implicit none

contains

SUBROUTINE ops_neutral(varin_meteo, z0, zi, ol, uster, h, x, uh, zu, szn)

use m_ops_varin
use m_commonconst_lt, only: EPS_DELTA 
USE m_ops_meteo

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_neutral')

! CONSTANTS
REAL                                             :: A                          ! correctiefactor to obtain equal limit values |L| > $ 
REAL                                             :: K                          ! von Karmanconstante
PARAMETER   (A = 1. )
PARAMETER   (K = 0.4)

! SUBROUTINE ARGUMENTS - INPUT
TYPE(Tvarin_meteo), INTENT(IN)                   :: varin_meteo                ! input variables for meteo
REAL,      INTENT(IN)                            :: z0                         ! roughness length (m)
REAL,      INTENT(IN)                            :: zi                         ! mixing height (m)
REAL,      INTENT(IN)                            :: ol                         ! Monin-Obukhov length  (m)
REAL,      INTENT(IN)                            :: uster                      ! friction velocity (m)
REAL,      INTENT(IN)                            :: h                          ! source heigth (including plume rise) (m)
REAL,      INTENT(IN)                            :: x                          ! downwind distance  (m)

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: uh                         ! windspeed at downwind distance x and height zu (m/s)
REAL,      INTENT(OUT)                           :: zu                         ! representative plume height (m), taking into account reflection 
                                                                               ! at the top of the mixing layer and at the ground surface
REAL,      INTENT(OUT)                           :: szn                        ! vertical dispersion coefficient for near neutral upper layer (m)

! LOCAL VARIABLES
INTEGER                                          :: last                       ! 
REAL                                             :: fz                         ! 
REAL                                             :: s                          ! 
REAL                                             :: sw                         ! 
REAL                                             :: tl                         ! 
LOGICAL                                          :: finished                   ! 

!-------------------------------------------------------------------------------------------------------------------------------
!
! Compute representative plume height at downwind distance x;
! start 'iteration' (maximal only one iteration step (last = 0)) over representative plume height zu;
! initial value zu = h = stack height.
!
last = 0
zu   = h
finished = .FALSE.
DO WHILE (.NOT. finished)
!
!  calculate the wind velocity at a certain height, starting from the friction velocity
!
   CALL ops_wv_log_profile(z0, zu, uster, ol, varin_meteo, uh)
!
!  Lagrangian time scale tau_L, Gryning et al., 1987 ?
!
   tl = 150. - (2000./ol)
   IF (tl .GT. (400. + EPS_DELTA)) THEN
      tl = 400.
   ELSE IF (tl .LT. (10. - EPS_DELTA)) THEN
      tl = 10.
   ELSE
      CONTINUE
   ENDIF
!
!  Compute vertical dispersion coefficient szn, for L < 0 and L >= 0
!  (3.21), (3.22), (3.24) OPS report
!
   IF (ol .LT. (0. - EPS_DELTA)) THEN
      sw  = uster*SQRT(1.5*(zu/( -K*ol))**.667*EXP( -2.*zu/zi) + (1.7 - zu/zi))
      fz  = 1/(1 + (x/(uh*2*tl))**0.5)
      szn = (sw*x)/uh*fz
   ELSE
      sw  = uster*SQRT(A*1.7*(1. - h/zi)**1.5) 
      fz  = 1/(1 + (x/(uh*2*tl))**0.5)
      szn = (sw*x)/uh*fz
   ENDIF
!
!  s is a representative height
!
   s = 0.69*szn 
!
!  For low sources (h < z1/2), the ground surface forces the centre of mass of the plume upwards. 
!  Three cases
!      1.  s < h,        relatively small plume that does not touch the ground               -> no action anymore, zu = h = stack height
!      2.  s > h
!      2a. s > zi/2,     very broad plume that touches both the ground and the mixing height -> zu = zi/2 = 1/2 mixing height
!      2b. h < s < zi/2, relatively broad plume that touches only the ground                 -> zu = s (higher than h)
!
   IF ((h .LT. (zi/2. - EPS_DELTA)) .AND. (s .GT. (h + EPS_DELTA)) .AND. (last .EQ. 0)) THEN
      IF (s .GT. (zi/2. + EPS_DELTA)) THEN
         zu = zi/2.
      ELSE
         zu = s
      ENDIF
      last = 1
      
!
!  For high sources (h > z1/2), the inversion at the mixing height forces the centre of mass of the plume downwards. 
!  Three cases
!      1.  s < zi-h,                 relatively small plume that does not touch the mixing height        -> no action anymore, zu = h = stack height
!      2.  s > zi-h
!      2a. zi-s < zi/2 <=> s > zi/2, very broad plume that touches both the ground and the mixing height -> zu = zi/2 = 1/2 mixing height
!      2b. zi-s > zi/2 <=> s < zi/2, relatively broad plume that touches only the mixing height          -> zu = zi - s (lower than h, 
!                                                                                                           because zi - s < zi - (zi-h) = h)
!
   ELSE IF ((h .GT. (zi/2. + EPS_DELTA)) .AND. (s .GT. (zi - h + EPS_DELTA)) .AND. (last .EQ. 0)) THEN
      IF (s .GE. (zi/2. - EPS_DELTA)) THEN
         zu = zi/2.
      ELSE
         zu = zi - s
      ENDIF
      last = 1
   ELSE
      ! Relatively small plumes; no action needed anymore
      finished = .TRUE.
   ENDIF
ENDDO ! end of iteration

RETURN
END SUBROUTINE ops_neutral

end module m_ops_neutral
