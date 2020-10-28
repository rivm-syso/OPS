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
! DESCRIPTION        : Retrieve roughness length z0 for a specific location.
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP-Fortran
! CALLED FUNCTIONS   : 
! UPDATE HISTORY :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_getz0(xr, yr, z0nlgrid, z0eurgrid, z0)

USE m_aps
USE m_geoutils
USE m_commonconst

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_getz0')

! SUBROUTINE ARGUMENTS - INPUT
real,      INTENT(IN)                            :: xr                         ! x-coordinate of point (RDM)
real,      INTENT(IN)                            :: yr                         ! y-coordinate of point (RDM)
TYPE (TApsGridInt), INTENT(IN)                   :: z0nlgrid                   ! map of roughness lengths in NL [m]
TYPE (TApsGridInt), INTENT(IN)                   :: z0eurgrid                  ! map of roughness lengths in Europe [m]

! SUBROUTINE ARGUMENTS - OUTPUT
real,      INTENT(OUT)                           :: z0                         ! value of roughnes length z0 [m]

! LOCAL VARIABLES
INTEGER*4                                        :: i
INTEGER*4                                        :: cellvalue                  ! value of z0 grid in grid cell
real                                             :: gxr                        ! x-coordinate of point (lon-lat)
real                                             :: gyr                        ! y-coordinate of point (lon-lat)
real                                             :: lamx                       ! x-coordinate of point (lambert)
real                                             :: lamy                       ! y-coordinate of point (lambert)
LOGICAL                                          :: iscell                     ! whether point is inside z0 grid
!-------------------------------------------------------------------------------------------------------------------------------
!
! Retrieve z0 from NL grid (z0nlgrid). 
! Note: arguments to GridValue must be in km.
!
cellvalue = 0
CALL GridValue(xr/1000, yr/1000, z0nlgrid, cellvalue, iscell)

! Convert z0 to m (z0 NL grid in mm)
IF (iscell) THEN
  z0 = FLOAT(cellvalue)/z0_FACT_NL                                                 ! z0 map in 0.1mm or mm
ENDIF

IF (.NOT. iscell .OR. z0 < (0. - EPS_DELTA)) THEN
!
! Could not retrieve z0 from NL grid, so try European grid (z0eurgrid).
! the new z0eurgrid is in lambert azimuthal coordinatensystem.
! Before this was a geographical coordinatesystem. 
! The gridsize of the geographical map was 0.5x0.5 grade, now it is 10x10 km.
!
  IF (z0eurgrid%gridheader%grixl == 0.5) THEN
!
! Retrieve Z0eur from map with geographical coordinatesystem
!
    CALL amc2geo(xr/1000, yr/1000, gxr, gyr)
    CALL GridValue(gxr, gyr, z0eurgrid, cellvalue, iscell)
  ELSE
!
! Retrieve Z0eur from map with lambert azimuthal coordinatesystem
!
    CALL amc2lam(xr/1000, yr/1000, lamx, lamy)
    CALL GridValue(lamx, lamy, z0eurgrid, cellvalue, iscell)
  ENDIF

! Convert z0 to m (z0 EUR grid in 0.1 mm)
  IF (iscell) THEN
    z0 = FLOAT(cellvalue)/z0_FACT_EUR                                              ! z0 map in 0.1mm
  ENDIF

  IF (.NOT. iscell .OR. ABS(z0) < EPS_DELTA) THEN
!
!   Could not retrieve z0 from any grid, so set at default value.

!
    IF (xr.gt.4000000.or.xr.lt.-4000000.or.yr.gt.4000000.or.yr.lt.-4000000) THEN  ! limits for 'AMCGEO' routine
       z0 = 0.15
    ELSE
       z0 = 0.003
    ENDIF
  ENDIF
ENDIF

RETURN
END SUBROUTINE ops_getz0
