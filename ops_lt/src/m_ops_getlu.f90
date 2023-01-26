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
! DESCRIPTION        : Retrieve dominant landuse class and percentages of each landuse class for a specific point.
!                      If point is part of trajectory and point is outside land use grid -> return default land use grass
!                      If point is not part of trajectory (separate receptor) and point is outside land use grid -> error
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_getlu

implicit none

contains

SUBROUTINE ops_getlu(xr, yr, in_trajectory, lugrid, landuse, error)

use m_aps
USE m_commonconst_lt
Use m_error
USE m_commonconst_lib, only: NLU

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'getlu')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: xr                         ! x-coordinate of point (RDM)
REAL*4,    INTENT(IN)                            :: yr                         ! y-coordinate of point (RDM)
LOGICAL,   INTENT(IN)                            :: in_trajectory              ! point is in a trajectory (opposed to separate receptor point)
TYPE (TApsGridInt), INTENT(IN)                   :: lugrid                     ! grid with land use class information (1: dominant land use, 2:NLU+1: percentages land use class)

! SUBROUTINE ARGUMENTS - INPUT/OUTPUT
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: landuse(NLU+1)             ! land-use value at (xr,yr); 
																			   ! landuse(1)    = index of dominant landuse
																			   ! landuse(lu+1) = percentage of grid cell with landuse class lu, lu = 1,NLU
                                                                               ! For locations outside lugrid, a fatal error is triggered
!
! LOCAL VARIABLES
!
LOGICAL                                          :: iscell                     ! whether point is inside landuse grid
INTEGER*4                                        :: ilu                        ! index of land-use class
!-------------------------------------------------------------------------------------------------------------------------------
landuse = 0

! Retrieve landuse information for each land use class from lugrid;
! note that GridValue expects km as input:
DO ilu = 1,NLU+1
  CALL GridValue(xr/1000, yr/1000, lugrid, landuse(ilu), iscell, ilu)
ENDDO


!IF (in_trajectory) THEN
   ! Point is part of trajectory: point outside land use grid -> take default grass (code 1):
   IF (.NOT. iscell) THEN
      landuse(1) = 1        ! dominant landuse class = grass
      landuse(2) = 100      ! 100 % grass
      landuse(3:NLU+1) = 0  ! 0% for the rest
   ENDIF
!ELSE
!   ! Point is not part of trajectory (separate receptor): point outside land use grid -> error:
!   IF (.NOT. iscell) THEN
!      CALL SetError('point outside land use grid', error)
!      CALL ErrorParam('coordinates [m]', (/ xr, yr /), error)
!      CALL ErrorParam('upper left corner land use grid [m]' , 1000.0*(/ lugrid%gridheader%xul_corner, lugrid%gridheader%yul_corner /), error)
!      CALL ErrorParam('lower right corner land use grid [m]', 1000.0*(/ lugrid%gridheader%xul_corner + lugrid%gridheader%nrcol*lugrid%gridheader%grixl, &
!                                                                        lugrid%gridheader%yul_corner - lugrid%gridheader%nrrow*lugrid%gridheader%griyl/), error)
!      GOTO 9999
!   ENDIF
!ENDIF

! Check for valid land use code read from land use grid file:
IF (landuse(1).LE.0 .OR. landuse(1).GT.NLU) THEN
   
   landuse(1) = 1        ! dominant landuse class = grass
   landuse(2) = 100      ! 100 % grass
   landuse(3:NLU+1) = 0  ! 0% for the rest
   !CALL SetError('dominant land use code found in land use grid is out of range', error)
   !CALL ErrorParam('dominant land use code in land use grid', landuse(1), error)
   !CALL ErrorParam('range land use codes', (/ 1, NLU /), error)
   !CALL ErrorParam('receptor coordinates [m]', (/ xr, yr /), error)
   !GOTO 9999
ENDIF

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE ops_getlu

end module m_ops_getlu
