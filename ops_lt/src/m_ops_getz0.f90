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
! DESCRIPTION        : Retrieve roughness length z0 for a specific location.
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_getz0

implicit none

contains

SUBROUTINE ops_getz0(xr, yr, z0nlgrid, z0eurgrid, z0, error)

use m_aps
USE m_geoutils
USE m_commonconst_lt
Use m_error
Use m_commonfile, only: fu_log

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_getz0')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: xr                         ! x-coordinate of point (RDM)
REAL*4,    INTENT(IN)                            :: yr                         ! y-coordinate of point (RDM)
TYPE (TApsGridInt), INTENT(IN)                   :: z0nlgrid                   ! map of roughness lengths in NL [m]
TYPE (TApsGridInt), INTENT(IN)                   :: z0eurgrid                  ! map of roughness lengths in Europe [m]

! SUBROUTINE ARGUMENTS - INPUT/OUTPUT
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: z0                         ! value of roughnes length z0 [m]

! LOCAL VARIABLES
INTEGER*4                                        :: cellvalue                  ! value of z0 grid in grid cell
REAL*4                                           :: gxr                        ! x-coordinate of point (lon-lat)
REAL*4                                           :: gyr                        ! y-coordinate of point (lon-lat)
REAL*4                                           :: lamx                       ! x-coordinate of point (lambert)
REAL*4                                           :: lamy                       ! y-coordinate of point (lambert)
LOGICAL                                          :: iscell                     ! whether point is inside z0 grid
!-------------------------------------------------------------------------------------------------------------------------------
!
! Retrieve z0 from NL grid (z0nlgrid). 
! Note: arguments to GridValue must be in km.
!
cellvalue = 0
CALL GridValue(xr/1000, yr/1000, z0nlgrid, cellvalue, iscell)

! Convert z0 to m (z0 NL grid in mm (LGN3) or in 0.1 mm (later LGN versions))
IF (iscell) THEN
  z0 = FLOAT(cellvalue)/z0_FACT_NL   ! z0 map in 0.1mm or mm
ENDIF

IF (.NOT. iscell .OR. z0 < (0. - EPS_DELTA)) THEN
!
! Could not retrieve z0 from NL grid, so try European grid (z0eurgrid).
! the new z0eurgrid is in lambert azimuthal coordinate system.
! Before this was a geographical coordinate system. 
! The gridsize of the geographical map was 0.5x0.5 degrees, now it is 10x10 km.
!
  IF (z0eurgrid%gridheader%grixl == 0.5) THEN
!
! Retrieve Z0eur from map with geographical coordinatesystem
!
    CALL amc2geo(xr/1000, yr/1000, gxr, gyr)
    ! write(*,*) 'getz0 1: ',gxr,gyr
    CALL GridValue(gxr, gyr, z0eurgrid, cellvalue, iscell)
  ELSE
!
! Retrieve Z0eur from map with lambert azimuthal coordinatesystem
!
    CALL amc2lam(xr/1000, yr/1000, lamx, lamy)
    ! write(*,*) 'getz0 2: ',lamx,lamy
    CALL GridValue(lamx, lamy, z0eurgrid, cellvalue, iscell)
  ENDIF

  ! Convert z0 to m (z0 EUR grid in 0.1 mm)
  IF (iscell) THEN
    z0 = FLOAT(cellvalue)/z0_FACT_EUR
  ELSE
  
    ! ! Error point outside z0-grid; points far away -> use default value for z0; warning not needed
    ! CALL SetError('point outside z0 grid', error)
    ! CALL ErrorParam('coordinates [m RDM]', (/ xr, yr /), error)
    ! write(*,'(a,3(1x,e13.6))') 'error outside z0 grid; coordinates [m RDM]', xr, yr, z0
    ! IF (z0eurgrid%gridheader%grixl == 0.5) THEN
    !     CALL ErrorParam('coordinates [degrees lon lat]', (/ gxr, gyr /), error)
    !     CALL ErrorParam('upper left corner z0 grid [degrees]' , (/ z0eurgrid%gridheader%xul_corner, z0eurgrid%gridheader%yul_corner /), error)
    !     CALL ErrorParam('lower right corner z0 grid [degrees]', (/ z0eurgrid%gridheader%xul_corner + z0eurgrid%gridheader%nrcol*z0eurgrid%gridheader%grixl, &
    !                                                                z0eurgrid%gridheader%yul_corner - z0eurgrid%gridheader%nrrow*z0eurgrid%gridheader%griyl/), error)
    ! ELSE
    !     CALL ErrorParam('coordinates [m Lambert azimuthal]', 1000.0*(/ lamx, lamy /), error)
    !     CALL ErrorParam('upper left corner z0 grid [m Lambert azimuthal]' , 1000.0*(/ z0eurgrid%gridheader%xul_corner, z0eurgrid%gridheader%yul_corner /), error)
    !     CALL ErrorParam('lower right corner z0 grid [m Lambert azimuthal]', 1000.0*(/ z0eurgrid%gridheader%xul_corner + z0eurgrid%gridheader%nrcol*z0eurgrid%gridheader%grixl, &
    !                                                                                   z0eurgrid%gridheader%yul_corner - z0eurgrid%gridheader%nrrow*z0eurgrid%gridheader%griyl/), error)
    ! ENDIF
    !  
    ! ! Warning only (to log file):
    ! error%haserror = .false.
    ! CALL WriteError(fu_log, error)
  ENDIF
  IF (z0 < EPS_DELTA) THEN
     ! Error invalid value for z0
     CALL SetError('invalid z0 value in z0 grid; z0 must be greater than zero', error)
     CALL ErrorParam('z0 [m]', z0, error)

    ! Warning only (to log file):
    error%haserror = .false.
    CALL WriteError(fu_log, error)
  ENDIF
  
  
  IF (.NOT. iscell .OR. ABS(z0) < EPS_DELTA) THEN
    ! Could not retrieve z0 from any grid, so set at default value.
    
    IF (xr.gt.4000000.or.xr.lt.-4000000.or.yr.gt.4000000.or.yr.lt.-4000000) THEN  ! limits for 'AMCGEO' routine
       z0 = 0.15   ! Average roughness NL
    ELSE
       z0 = 0.003  ! very low value
    ENDIF
  ENDIF
ENDIF

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE ops_getz0

end module m_ops_getz0
