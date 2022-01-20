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
! DESCRIPTION        : Compute average roughness length z0 over a trajectory.
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_getz0_tra

implicit none

contains

SUBROUTINE ops_getz0_tra(xr, yr, xb, yb, z0nlgrid, z0eurgrid, z0_tra, error)

use m_aps
use m_geoutils
use m_commonconst_lt
use m_ops_getz0

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_getz0_tra')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: xr                         ! x coordinate receptor (RDM)
REAL*4,    INTENT(IN)                            :: yr                         ! y coordinate receptor (RDM)
REAL*4,    INTENT(IN)                            :: xb                         ! x coordinate source (RDM)
REAL*4,    INTENT(IN)                            :: yb                         ! y coordinate source (RDM)
TYPE (TApsGridInt), INTENT(IN)                   :: z0nlgrid                   ! map of roughness lengths in NL [m]
TYPE (TApsGridInt), INTENT(IN)                   :: z0eurgrid                  ! map of roughness lengths in Europe [m]

! SUBROUTINE ARGUMENTS - INPUT/OUTPUT
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: z0_tra                     ! roughness length representative for trajectory [m]

! LOCAL VARIABLES
REAL*4                                           :: x                          ! x-coordinate intermediate point (RDM)
REAL*4                                           :: y                          ! y-coordinate intermediate point (RDM)
REAL*4                                           :: total                      ! summed total of log(1/z0) over intermediate points
REAL*4                                           :: z0                         ! roughness length in intermediate point
INTEGER*4                                        :: ns                         ! number of sub sectors between intermediate points
INTEGER*4                                        :: i                          ! index of intermediate point
!-------------------------------------------------------------------------------------------------------------------------------
!
! Calculate z0-average using ns = 20 sub sectors over a line between source and receptor,
! using logarithmic averaging of 1/z0
!
ns=20
total=0.
! 
! Loop over intermediate points
!
DO i=0,ns
!
! Coordinates of intermediate point
!
  x=xr+(xb-xr)/ns*i
  y=yr+(yb-yr)/ns*i
!
! Get z0 for specific point
!
  CALL ops_getz0(x, y, z0nlgrid, z0eurgrid, z0, error)
  if (error%haserror) goto 9999

!
! Add log(1/z0) to total 
!
  total = total + alog(1/z0)
ENDDO
!
! Average rougness length
!
z0_tra = 1/(exp(total/(ns+1)))

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE ops_getz0_tra

end module m_ops_getz0_tra
