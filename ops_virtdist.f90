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
! FUNCTION
! NAME               : %M%
! SCCS (SOURCE)      : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH - SEQUENCE  : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : OPS-support 
! FIRM/INSTITUTE     : RIVM/LLO
! LANGUAGE           : FORTRAN-77/90
! DESCRIPTION        : Compute distance between (virtual point source) and (centre of area source);
!                      for a point source, ops_virtdist = 0.
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP Fortran
! CALLED FUNCTIONS   :
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION ops_virtdist (radius, rond)

USE m_commonconst

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: radius                     ! 
INTEGER*4, INTENT(IN)                            :: rond                       ! 

! RESULT
REAL*4                                           :: ops_virtdist               ! 

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Compute distance between (virtual point source) and (centre of area source);
! for radius = 0 (point source), ops_virtdist = 0.
! 3.33 OPS report
!
IF (rond .EQ. 1) THEN
   ! Circular area source
   ops_virtdist = (radius*12.)/PI 
ELSE
   ! Square area source is represented by a circular area source with the same area;
   ! (area circle with radius r) = (area square with 1/2 side = radius) <=> pi*r**2 = (2*radius)**2 <=> 
   ! <=> r = (2/sqrt(pi))*radius <=> r = 1.128*radius
   ops_virtdist = (radius*12.)/PI*1.128
ENDIF

RETURN
END
