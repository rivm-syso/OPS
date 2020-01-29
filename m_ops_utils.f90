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
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_utils

! Different utility routines and functions
!
! logical function is_missing(x)       : determine whether x is a missing value (-999).
! logical function between_angle(n,a,b): between_angle is true, if angle n is between angles a and b (all angles in radians).
! real function angle180(a)            : return angle in interval (-pi,pi].
! subroutine proj_point                : get projected point P' of P on a line segment.

implicit none

contains

!----------------------------------------------------------------------------------------------
logical function is_missing(x)

real, intent(in) :: x

! bandwidth for checking (in)equalities of floats
real, parameter :: EPS = 1.0e-5

is_missing = (abs(x + 999.) .le. EPS)

end function is_missing

!----------------------------------------------------------------------------------------------
logical function between_angle(n,a,b)

use Binas, only: pi

! between_angle is true, if angle n is between angles a and b (all angles in radians).

! Input:
real, intent(in) :: n       ! angle to be checked
real, intent(in) :: a
real, intent(in) :: b

! Local
real :: a180         ! angle a transformed to interval (-pi,pi]
real :: b180         ! angle b transformed to interval (-pi,pi]

!-------
! http://www.monkey-x.com/Community/posts.php?topic=2866
!
! Function betweenAngle:Bool(n:Float, a:Float, b:Float)
!
! 	a = Angle180( a - n )
! 	b = Angle180( b - n )
! 	Return ( Sgn( a ) <> Sgn( b ) ) And ( Abs( a ) + Abs( b ) <= 180 ) Or a = 0 Or b = 0
!
! End Function
!
! Function Angle180:Float( a:Float )
!
! 	a Mod= 360
! 	If a > 180 Then Return a-360
! 	If a <= -180 Then Return a+360
! 	Return a
!
! End Function
!-------

! Check whether angle n-n is between a-n and b-n; first subtract n from a and b (result between -pi and +pi):
a180 = angle180( a - n )
b180 = angle180( b - n )

!       b                        b
!     /                           \
!    / in_between = true           \ in_between = false         in_between = false
!   /                               \
!  /                                 \
! ----------- n                       ------------- n           ---------------- n
!  \                                 /                          \.
!   \                               /                            \   .
!    \                             /                              \      .
!     \                           /                                \        .
!      a                        a                                   a         b

! 1. a and b on opposite sides (sign(a) .ne. sign(b)) and (angle between a and b) < pi -> in_between = true.
! 2. n is the same angle as a or b -> in_between also true (result is not critical?? otherwise use EPS)
between_angle = (( sign(1.0,a180) .ne. sign(1.0,b180) ) .and. ( abs(a180) + abs(b180) <= pi ) .or. a180 .eq. 0.0 .or. b180 .eq. 0.0)

! write(*,'(in between f6.1 f6.1 f6.1 !l4)') (180/pi)*n,(180/pi)*a,(180/pi)*b,in_between

end function between_angle

!-----------------------------------------------------------------------------------
real function angle180(a)

use Binas, only: pi

! Return angle in interval (-pi,pi]
! Input
real, intent(in) ::  a            ! angle [radians]

! Local
real ::  aa                       ! angle in interval (-pi,pi] [radians]

aa = mod(a,2*pi)
if (aa  >  pi) aa = aa-2*pi
if (aa <= -pi) aa = aa+2*pi

angle180 = aa

end function angle180

!-----------------------------------------------------------------------------------
subroutine proj_point(v1x,v1y,v2x,v2y,px,py,p_projx,p_projy,fac,len2)


! Get projected point P' of P on line segment [v1 v2] and interpolation factor fac
! between v1 and v2, from v1 (fac = 0) to v2 (fac = 1); len2 is squared length of (v2-v1).
!
!         P
!         |
!         |
!         |
!  v1 ----|--------- v2
!         P'
!
! http://www.sunshine2k.de/coding/java/PointOnLine/PointOnLine.html

! Input:
real,   intent(in)  :: v1x                       ! x-coordinate begin point of segment
real,   intent(in)  :: v1y                       ! y-coordinate begin point of segment
real,   intent(in)  :: v2x                       ! x-coordinate end point of segment
real,   intent(in)  :: v2y                       ! y-coordinate end point of segment
real,   intent(in)  :: px                        ! x-coordinate point P
real,   intent(in)  :: py                        ! y-coordinate point P

! Output:
real,   intent(out) :: p_projx                   ! x-coordinate point P'
real,   intent(out) :: p_projy                   ! y-coordinate point P'
real,   intent(out) :: fac                       ! interpolation factor between v1 and v2, from v1 (fac = 0) to v2 (fac = 1)
real,   intent(out) :: len2                      ! squared length of e1 = v2-v1

! Local
real :: e1x,e1y                                  ! coordinates of e1 = v2-v1
real :: e2x,e2y                                  ! coordinates of e2 = p-v1
real :: dot_prod                                 ! dot product of e1,e2

! e1 = v2-v1:
e1x = v2x - v1x;
e1y = v2y - v1y;

! e2 = p-v1:
e2x = px - v1x;
e2y = py - v1y;

! Dot product of e1, e2:
dot_prod = e1x*e2x + e1y*e2y;

! Squared length of e1:
len2 = e1x*e1x + e1y*e1y;

! Interpolation factor between v1 and v2, from v1 (fac = 0) and v2 (fac = 1):
fac = dot_prod/len2;

! Projected point:
! p_proj.x = (v1.x + (dot_product * e1.x) / len2);
! p_proj.y = (v1.y + (dot_product * e1.y) / len2);
p_projx = v1x + fac*e1x;
p_projy = v1y + fac*e1y;

end subroutine proj_point

end module m_ops_utils


