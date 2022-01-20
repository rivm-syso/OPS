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
! DESCRIPTION        : Compute distance between (virtual point source) and (centre of area source);
!                      for a point source, ops_virtdist = 0.
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_virtdist 

implicit none

contains

FUNCTION ops_virtdist (radius, rond)

use m_commonconst_lt

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: radius                     ! 
INTEGER*4, INTENT(IN)                            :: rond                       ! 

! RESULT
REAL*4                                           :: ops_virtdist               ! 

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

end module m_ops_virtdist 
