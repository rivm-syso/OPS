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
! FILENAME           : %M%
! SCCS(SOURCE)       : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH - SEQUENCE  : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : OPS-support
! FIRM/INSTITUTE     : RIVM/LLO/IS
! LANGUAGE           : FORTRAN(HP-UX, HP-F77, HP-F90)
! DESCRIPTION        : Returns grid value at a specific location.
!                      Originally made for background concentrations (bgcon), but now also used for other grids.
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES:
! CALLED FUNCTIONS   :
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_bgcon

contains

SUBROUTINE ops_bgcon(x, y, bgdata, bgcon, fieldnumber)

USE m_aps
USE m_commonconst                                                              ! EPS_DELTA only

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER    (ROUTINENAAM = 'ops_bgcon')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: x                          ! x coordinate of specific location
REAL*4,    INTENT(IN)                            :: y                          ! y coordinate of specific location
TYPE (TApsGridReal), INTENT(IN)                  :: bgdata                     ! APS-grid with background concentrations
INTEGER, OPTIONAL                                :: fieldnumber                ! field number in APS-grid

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: bgcon                      ! background concentration at (x,y)

! LOCAL VARIABLES
LOGICAL                                          :: iscell                     ! whether (x,y) is inside APS-grid bgdata
!-------------------------------------------------------------------------------------------------------------------------------
!
! Get value of background concentration bgdata at location (x,y); return grid average if point is outside grid or if value is negative
! Note: arguments of GridValue must be in km
!
if (present(fieldnumber)) then
   CALL GridValue(x/1000., y/1000., bgdata, bgcon, iscell, fieldnumber)
   IF (iscell .AND. bgcon < 0.+EPS_DELTA) THEN
     bgcon = bgdata%average(fieldnumber)
   ENDIF
else
   CALL GridValue(x/1000., y/1000., bgdata, bgcon, iscell)
   IF (iscell .AND. bgcon < 0.+EPS_DELTA) THEN
     bgcon = bgdata%average(1)
   ENDIF
endif
! write(*,'(a,3(1x,e12.5),L3,1x,e12.5)') 'ops_bgcon: ',x,y,bgcon,iscell,bgdata%average

RETURN
END SUBROUTINE ops_bgcon

end module m_ops_bgcon
