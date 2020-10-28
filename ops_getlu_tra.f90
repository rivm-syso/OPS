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
! DESCRIPTION        : Compute dominant land use class and percentage of each land use
!                      class over a trajectory.
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP-Fortran
! CALLED FUNCTIONS   : ops_getlu
! UPDATE HISTORY :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_getlu_tra(xr, yr, xb, yb, lugrid, domlu, lu_tra_per)

USE m_aps
USE m_commonconst

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER    (ROUTINENAAM = 'ops_getlu_tra')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: xr                         ! x-coordinate receptor (RDM)
REAL*4,    INTENT(IN)                            :: yr                         ! y-coordinate receptor (RDM)
REAL*4,    INTENT(IN)                            :: xb                         ! x-coordinate source (RDM) (b << "bron" = source)
REAL*4,    INTENT(IN)                            :: yb                         ! y-coordinate source (RDM)
TYPE (TApsGridInt), INTENT(IN)                   :: lugrid                     ! grid with land use class information (1: dominant land use, 2:NLU+1: percentages land use class)
LOGICAL, INTENT(IN)                              :: domlu

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: lu_tra_per(NLU)            ! percentages of land use classes over trajectorie (over intermediate points)

! LOCAL VARIABLES
REAL*4                                           :: x                          ! x-coordinate intermediate point
REAL*4                                           :: y                          ! y-coordinate intermediate point
INTEGER*4                                        :: lu_tra_per_sum(NLU)        ! sum of percentages of land use classes over trajectorie (over intermediate points)
INTEGER*4                                        :: lu_tra_dom                 ! dominant land use class over trajectory source-receptor
INTEGER*4                                        :: is                         ! index of intermediate point
INTEGER*4                                        :: lu                         ! index of land use class
INTEGER*4                                        :: landuse(NLU+1)             ! land use information at intermediate point; for locations outside lugrid
                                                                               ! a default land use class = 1 (grass) is taken.
INTEGER*4                                        :: ludom                      ! maximum of dominant land use classes over intermediate points
INTEGER*4, DIMENSION(NLU)                        :: lu_count                   ! total number of intermediate points that have a certain land use class

INTEGER*4                                        :: ns                         ! number of sub sectors between intermediate points
!-------------------------------------------------------------------------------------------------------------------------------
!
! Calculate lu-average using ns = 20 steps over a line between source and receptor
!
ns              = 20
lu_count        = 0
lu_tra_per_sum  = 0
lu_tra_dom      = 0
!
! Loop over intermediate points
!
DO is = 0,ns
!
! Coordinates of intermediate point
!
  x=xr+(xb-xr)/ns*is
  y=yr+(yb-yr)/ns*is
!
! Get dominant land use class for this point (1 is default) and add 1 to
! total number of points with this land use class
!
  CALL ops_getlu(x, y, lugrid, landuse)
  lu_count(landuse(1)) = lu_count(landuse(1)) + 1
!
! Add contribution of current intermediate point to summed percentages per land use class
!
  DO lu = 1,NLU
    lu_tra_per_sum(lu) = lu_tra_per_sum(lu) + landuse(lu+1)
  ENDDO
ENDDO
!
! If option for dominant land use is set, set land use percentage of dominant land use class to 100:
!
IF (domlu) THEN
  ludom = 0
!
! Get lu_tra_dom = dominant land use (land use class where maximum of lu_count occurs)
!
  DO lu = 1,NLU
    IF (lu_count(lu) > ludom) THEN
      ludom = lu_count(lu)
      lu_tra_dom = lu
    ENDIF
  ENDDO
  lu_tra_per_sum=0
  lu_tra_per_sum(lu_tra_dom)=100
ENDIF
!
!  IF no landuse information available in standard grid or rcp-file, we asume it's grass
!
IF (sum(lu_tra_per_sum(1:NLU)) .le. 0) THEN
  lu_tra_per_sum=0
  lu_tra_per_sum(1)=100
ENDIF

!
!  Compute percentages per land use class
!
lu_tra_per = float(lu_tra_per_sum)/(ns+1)

RETURN

END SUBROUTINE ops_getlu_tra
