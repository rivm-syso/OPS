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
! LANGUAGE           : FORTRAN-77/90
! DESCRIPTION        : This routine calculates the wind velocity at a certain height, assuming a logarithmic wind profile.
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP Fortran
! CALLED FUNCTIONS   :
! UPDATE HISTORY     :
!
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_wvprofile(z0, zu, uster, ol, uz)

USE m_commonconst

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER      (ROUTINENAAM = 'ops_wvprofile')

! CONSTANTS
REAL*4                                           :: K                          ! von Karman constante
PARAMETER    (K = 0.4)

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: z0                         ! roughness length (m)
REAL*4,    INTENT(IN)                            :: zu
REAL*4,    INTENT(IN)                            :: uster                      ! friction velocity (m)
REAL*4,    INTENT(IN)                            :: ol                         ! Monin-Obukhov length  (m)

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: uz                         ! wind velocity (m/s)

! LOCAL VARIABLES
REAL*4                                           :: phim
REAL*4                                           :: y                          ! hulpvariabele voor berekening phim

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! stability functions:
!
IF (ol .GT. (0. + EPS_DELTA)) THEN
   phim = -17.*(1. - EXP( -0.29*zu/ol)) + 17.*(1. - EXP( -0.29*z0/ol))
!
!  v Ulden and Holtslag
!
ELSE
   y    = (1. - 15.*zu/ol)**0.25
   phim = 2.*LOG((1. + y)/2.) + LOG((1. + y*y)/2.) - 2.*ATAN(y) + PI/2.
ENDIF

! Compute wind speed (2.3 OPS report)
uz = uster/K*(ALOG(zu/z0) - phim)

! Set lower limit for wind at 0.75 m/s
IF (uz .LT. (0.75 - EPS_DELTA)) THEN
  uz = 0.75
ENDIF                                                                          ! 950310

RETURN
END SUBROUTINE ops_wvprofile
