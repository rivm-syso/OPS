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
! DESCRIPTION        : Returns grid value at a specific location. 
!                      Originally made for background concentrations (bgcon), but now also used for other grids. 
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_bgcon

contains

SUBROUTINE ops_bgcon(x, y, in_trajectory, bgdata, bgcon, error, fieldnumber)

USE m_aps
USE m_commonconst_lt, only: EPS_DELTA

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_bgcon')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: x                          ! x coordinate of specific location
REAL*4,    INTENT(IN)                            :: y                          ! y coordinate of specific location
LOGICAL,   INTENT(IN)                            :: in_trajectory              ! point is in a trajectory (opposed to separate receptor point)
TYPE (TApsGridReal), INTENT(IN)                  :: bgdata                     ! APS-grid with background concentrations
INTEGER, OPTIONAL                                :: fieldnumber                ! field number in APS-grid

! SUBROUTINE ARGUMENTS - INPUT/OUTPUT
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: bgcon                      ! background concentration at (x,y)

! LOCAL VARIABLES
LOGICAL                                          :: iscell                     ! whether (x,y) is inside APS-grid bgdata
!-------------------------------------------------------------------------------------------------------------------------------
!
! Get value of background concentration bgdata at location (x,y); return grid average if point is outside grid or if value is negative 
! Note1: arguments of GridValue must be in km
! Note2: GridValue returns grid average if point is outside grid
!
IF (present(fieldnumber)) THEN
   CALL GridValue(x/1000., y/1000., bgdata, bgcon, iscell, fieldnumber)
   IF (iscell .AND. bgcon < 0.+EPS_DELTA) THEN 
     bgcon = bgdata%average(fieldnumber)
   ENDIF
ELSE
   CALL GridValue(x/1000., y/1000., bgdata, bgcon, iscell)
   IF (iscell .AND. bgcon < 0.+EPS_DELTA) THEN 
     bgcon = bgdata%average(1)
   ENDIF
ENDIF

! If separate receptor (not in trajectory) is outside background grid -> error 
! IF (.not. in_trajectory .and. .not. iscell) THEN
!    CALL SetError('point outside background concentration grid', error)
!    CALL ErrorParam('coordinates [m]', (/ x, y /), error)
!    CALL ErrorParam('upper left corner background grid [m]' , 1000.0*(/ bgdata%gridheader%xul_corner,  bgdata%gridheader%yul_corner /), error)
!    CALL ErrorParam('lower right corner background grid [m]', 1000.0*(/ bgdata%gridheader%xul_corner + bgdata%gridheader%nrcol*bgdata%gridheader%grixl, &
!                                                                      bgdata%gridheader%yul_corner - bgdata%gridheader%nrrow*bgdata%gridheader%griyl/), error)
!    GOTO 9999
! ENDIF
! write(*,'(a,3(1x,e12.5),L3,1x,e12.5)') 'ops_bgcon: ',x,y,bgcon,iscell,bgdata%average  

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)

RETURN
END SUBROUTINE ops_bgcon

end module m_ops_bgcon
