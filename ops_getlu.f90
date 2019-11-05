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
! SCCS(SOURCE)       : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH -SEQUENCE   : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             :
! FIRM/INSTITUTE     : RIVM/LLO
! LANGUAGE           : FORTRAN-77/90
! DESCRIPTION        : Retrieve dominant landuse class and percentages of each landuse class for a specific point.
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP-Fortran
! CALLED FUNCTIONS   : GridValue
! UPDATE HISTORY :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_getlu(xr, yr, lugrid, landuse)

USE m_aps
USE m_commonconst

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'getlu')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: xr                         ! x-coordinate of point (RDM)
REAL*4,    INTENT(IN)                            :: yr                         ! y-coordinate of point (RDM)
TYPE (TApsGridInt), INTENT(IN)                   :: lugrid                     ! grid with landuse information

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: landuse(NLU+1)             ! land-use value at (xr,yr); 
																			   ! landuse(1) = index of dominant landuse
																			   ! landuse(lu+1) = percentage of grid cell with landuse class lu, lu = 1,NLU
                                                                               ! For locations outside lugrid, a default land use class = 1 (grass) is taken.
!
! LOCAL VARIABLES
!
LOGICAL                                          :: iscell                     ! whether point is inside landuse grid
!-------------------------------------------------------------------------------------------------------------------------------
landuse = 0
!
! Retrieve landuse information from lugrid;
! note that GridValue expects km as input
!
DO i=1,NLU+1
  CALL GridValue(xr/1000, yr/1000, lugrid, landuse(i), iscell, i)
ENDDO
!
IF (.NOT. iscell) THEN
  landuse(1) = -1
ENDIF
!
! Take grass (code 1) for undefined landuse code of dominant class (i.e. <=0 and > NLU)
!
IF (landuse(1).LE.0 .OR. landuse(1).GT.NLU) THEN
   landuse(1) = 1        ! dominant landuse class = grass
   landuse(2) = 100      ! 100 % grass
   landuse(3:NLU+1) = 0  ! 0% for the rest
ENDIF

RETURN
END SUBROUTINE ops_getlu
