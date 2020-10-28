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
!                       Copyright by
!   National Institute of Public Health and Environment
!           Laboratory for Air Research (RIVM/LLO)
!                      The Netherlands
!   No part of this software may be used, copied or distributed without permission of RIVM/LLO (2002)
!
! SUBROUTINE
! NAME               : %M%
! SCCS(SOURCE)       : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH -SEQUENCE   : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : OPS-support
! FIRM/INSTITUTE     : RIVM/LLO
! LANGUAGE           : FORTRAN(HP-UX, HP-F77)
! DESCRIPTION        : This routine calculates sigmaz for convective cases according to Weil and Brower (1982) formally defined
!                      for (-h/L> 10 and 0.1<h/zi<0.8), but also applied for (-h/L>10 and 0.1<h/zi<1)
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: NON-ANSI F77
! CALLED FUNCTIONS   : ops_wvprofile
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_convec(z0, zi, ol, uster, h, x, uh, zu, szc)

USE m_commonconst                                                              ! EPS_DELTA only

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER    (ROUTINENAAM = 'ops_convec')

! CONSTANTS
REAL*4                                           :: K                          ! von Karmanconstante
PARAMETER   (K = 0.4)

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: z0                         ! roughness length (m)
REAL*4,    INTENT(IN)                            :: zi                         ! mixing height (m)
REAL*4,    INTENT(IN)                            :: ol                         ! Monin-Obukhov length  (m)
REAL*4,    INTENT(IN)                            :: uster                      ! friction velocity (m)
REAL*4,    INTENT(IN)                            :: h                          ! source height (including plume rise) (m)
REAL*4,    INTENT(IN)                            :: x                          ! downwind distance  (m)

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: uh                         ! windspeed at representative plume height (m/s)
REAL*4,    INTENT(OUT)                           :: zu                         ! representative plume height (m), taking into account reflection
                                                                               ! at the top of the mixing layer and at the ground surface
REAL*4,    INTENT(OUT)                           :: szc                        ! convective vertical dispersion coefficient (m)

! LOCAL VARIABLES
INTEGER*4                                        :: last
REAL*4                                           :: s
REAL*4                                           :: wster
REAL*4                                           :: xs
LOGICAL                                          :: finished

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Compute representative plume height at downwind distance x;
! start 'iteration' (maximal only one iteration step (last = 0)) over representative plume height zu;
! initial value zu = h = stack height.
!
last = 0
zu   = h
finished = .false.
DO WHILE (.not. finished)
!
!  calculate the wind velocity at height zu
!
   CALL ops_wvprofile(z0, zu, uster, ol, uh)
!
! Compute convective velocity scale w* and vertical dispersion coefficient sigma_z
! (2.1, 3.19, 3.20 OPS report)
!
!                     3         3
!      T rho_a cp (u*)      (u*)          g H0
! L = ---------------- => ---------  = ------------
!        g H0 kappa        L kappa      T rho_a cp
!
!                                   3
!         g H0 zi   1/3      zi (u*)   1/3
! w* = (-----------)     = (----------)
!        T rho_a cp          L kappa
!
!  (sigma_wc/w*)^2 = 0.56^2 = 0.314 (Kaimal et al , 1976)
!  (sigma_wm/w*)^2 = (1.26 u*/w*)^2 (Panowski et al , 1977)
!  Extra factor 0.7 for szc from calibration on Gryning and Holtslag data;
!
   wster = (zi*uster**3/(-ol*0.4))**.333
   xs    = (wster*x)/(uh*zi)
   szc   = (zi*xs*(0.314 + (1.26*uster/wster)**2)**0.5)*0.7
!

!  s is a representative height
!
   s = 0.69*szc
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
   ELSE IF ((h .GT. (zi/2. + EPS_DELTA)) .AND. (s .GT. (zi - h + EPS_DELTA)) .AND. (last .EQ. 1)) THEN
      IF ((zi - s) .LT. (zi/2. - EPS_DELTA)) THEN
         zu = zi/2.
      ELSE
         zu = zi - s
      ENDIF
      last = 1
   ELSE
     ! Relatively small plumes; no action needed anymore
      finished = .true.
   ENDIF
ENDDO

RETURN
END SUBROUTINE ops_convec
