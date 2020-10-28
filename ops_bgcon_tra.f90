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
! DESCRIPTION        : This routine reads for a given location the background conc. and
!                      calculates the average conc. between the receptor and the source location
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES:
! CALLED FUNCTIONS   :
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   ops_bgcon_tra
! Purpose      This routine reads for a given location the background conc. and calculates the average conc. between the
!              receptor and the source location
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_bgcon_tra(xr, yr, xb, yb, bgdata, bgcon)

USE m_aps
USE m_ops_bgcon

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER    (ROUTINENAAM = 'ops_bgcon_tra')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: xr                         ! x coordinate receptor
REAL*4,    INTENT(IN)                            :: yr                         ! y coordinate receptor
REAL*4,    INTENT(IN)                            :: xb                         ! x coordinate source (b << "bron" = source)
REAL*4,    INTENT(IN)                            :: yb                         ! y coordinate source
TYPE (TApsGridReal), INTENT(IN)                  :: bgdata                     ! grid with background concentrations

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: bgcon                      ! background concentration averaged over trajecory
                                                                               ! between source and receptor

! LOCAL VARIABLES
REAL*4                                           :: x                          ! x coordinate of intermediate point between source and receptor
REAL*4                                           :: y                          ! y coordinate of intermediate point between source and receptor
REAL*4                                           :: total                      ! summed total of background concentration in intermediate points
INTEGER*4                                        :: ns                         ! number of trajectory sectors between intermediate points
INTEGER*4                                        :: i                          ! index of intermediate point
!-------------------------------------------------------------------------------------------------------------------------------

! Set number of sub sectors of trajectory and initialise total
ns=20
total=0.

! Loop over intermediate points of trajectory
DO i=0,ns
!
! Coordinates of intermediate point on trajectory
!
  x=xr+(xb-xr)/ns*i
  y=yr+(yb-yr)/ns*i

!
! Calculate background concentration contribution at this point and add to total
!
  CALL ops_bgcon(x, y, bgdata, bgcon)
  total = total + bgcon
ENDDO
!
! Compute average over trajectory
!
bgcon = total/(ns+1)

RETURN

END SUBROUTINE ops_bgcon_tra
