module m_tst_m_ops_utils
contains
   subroutine tst_is_missing()
   use no_pfunit
   use m_ops_utils
   implicit none
   
      double precision   :: missing8
      
      ! -999. is missing, 999. or another number is OK
      call assertTrue(is_missing(-999.),'Test 1: is_missing')
      call assertFalse(is_missing(999.),'Test 2: is_missing')
      call assertFalse(is_missing(0.),'Test 3: is_missing')
      
      ! double precision   version
      missing8 = -999.
      call assertTrue(is_missing(missing8),'Test 4: is_missing')
      missing8 = 999.
      call assertFalse(is_missing(missing8),'Test 5: is_missing')
      missing8 = 0.
      call assertFalse(is_missing(missing8),'Test 6: is_missing')
   
   end subroutine tst_is_missing

   subroutine tst_between_angle()
   use no_pfunit
   use m_ops_utils
   use Binas, only: pi
 
      ! between_angle is true, if angle n is between angles a and b (all angles in radians).
      call assertTrue(between_angle(pi/4., pi/6., pi/3.), 'Test 1: between_angle')
      
      ! what if we have a stretched angle?
      call assertTrue(between_angle(0., -pi/2., pi/2.), 'Test 2: between_angle')
      ! the opposite angle is not in between
      call assertFalse(between_angle(pi, -pi/2., pi/2.), 'Test 3: between_angle')
      
      ! two other false cases
      call assertFalse(between_angle(7./6.*pi, 2./3.*pi, pi), 'Test 4: between_angle')
      call assertFalse(between_angle(pi/2., 4./3.*pi, 5./3.*pi), 'Test 5: between_angle')
      
      ! n == a
      call assertTrue(between_angle(5./6.*pi, 5./6.*pi, pi), 'Test 6: between_angle')
      ! n == b
      call assertTrue(between_angle(5./6.*pi, 3./4.*pi, 5./6.*pi), 'Test 7: between_angle')
      
      ! wrong order of a,b ; still True
      call assertTrue(between_angle(5./6.*pi, pi, 3./4.*pi), 'Test 8: between_angle')
   
   end subroutine tst_between_angle

   subroutine tst_angle180()
   use no_pfunit
   use m_ops_utils
   use Binas, only: pi
   implicit none
   
       real                :: tol = 1.0e-5      ! tolerance in equality checks
       
       ! Goal: return angle in interval (-pi,pi]
       call assertEqual(pi, angle180(pi), tol, 'Test 1: angle180')
       call assertEqual(pi, angle180(-pi), tol, 'Test 2: angle180')
       call assertEqual(0., angle180(-2.*pi), tol, 'Test 3: angle180')
       call assertEqual(0., angle180(2.*pi), tol, 'Test 4: angle180')
       call assertEqual(-0.5*pi, angle180(1.5*pi), tol, 'Test 5: angle180')
       call assertEqual(0., angle180(8.*pi), tol, 'Test 6: angle180')
       call assertEqual(0.5*pi, angle180(8.5*pi), tol, 'Test 7: angle180')
   end subroutine tst_angle180

   subroutine tst_proj_point()
   use no_pfunit
   implicit none
   
      real :: v1x          ! x-coordinate begin point of segment
      real :: v1y          ! y-coordinate begin point of segment
      real :: v2x          ! x-coordinate end point of segment
      real :: v2y          ! y-coordinate end point of segment
      real :: px           ! x-coordinate point P
      real :: py           ! y-coordinate point P
      
      real :: fac          ! interpolation factor between v1 and v2, from v1 (fac = 0) to v2 (fac = 1)
      
      !------------------------------------
      ! Test 1: point "above" line
      !------------------------------------
      v1x = 0.5; v1y = 1.8; v2x = 2.1; v2y = 2.2; px = 1.7; py = 3.1
      call tst_check_proj_point(1,v1x,v1y,v2x,v2y,px,py,fac)
      
      !------------------------------------
      ! Test 2: point "below" line
      !------------------------------------
      v1x = 0.5; v1y = 1.8; v2x = 2.1; v2y = 2.2; px = 1.7; py = 0.9
      call tst_check_proj_point(2,v1x,v1y,v2x,v2y,px,py,fac)
      
      !------------------------------------
      ! Test 3: point not on line segment (near v2)
      !------------------------------------
      v1x = 0.5; v1y = 1.8; v2x = 2.1; v2y = 2.2; px = 2.8; py = 3.1
      call tst_check_proj_point(3,v1x,v1y,v2x,v2y,px,py,fac)
      call assertGreaterThan(fac, 1.0, 'Test 3: fac > 1.0')
      
      !------------------------------------
      ! Test 4: point not on line segment (near v1)
      !------------------------------------
      v1x = 0.5; v1y = 1.8; v2x = 2.1; v2y = 2.2; px = 0.4; py = 1.7
      call tst_check_proj_point(4,v1x,v1y,v2x,v2y,px,py,fac)
      call assertLessThan(fac, 0.0, 'Test 4: fac < 0.0')
      
      !------------------------------------
      ! Test 5: point "above" line, "mirrored"
      !------------------------------------
      v1x = 0.5; v1y = 1.8; v2x = -1.6; v2y = 2.2; px = -1.2; py = 3.1
      call tst_check_proj_point(5,v1x,v1y,v2x,v2y,px,py,fac)
      
      !------------------------------------
      ! Test 6: point not on line segment but on line V1-V2
      !------------------------------------
      v1x = 0.5; v1y = 1.8; v2x = 2.1; v2y = 2.2; 
      px = 2.6
      py = v2y + (v2y-v1y)/(v2x-v1x)*(px-v2x)
      call tst_check_proj_point(6,v1x,v1y,v2x,v2y,px,py,fac)
   
   end subroutine tst_proj_point

   subroutine tst_check_proj_point(itest,v1x,v1y,v2x,v2y,px,py,fac)
   use m_ops_utils, only: proj_point
   use no_pfunit
   implicit none
   
      integer, intent(in) :: itest             ! index of test run
      real, intent(in) :: v1x                       ! x-coordinate begin point of segment
      real, intent(in) :: v1y                       ! y-coordinate begin point of segment
      real, intent(in) :: v2x                       ! x-coordinate end point of segment
      real, intent(in) :: v2y                       ! y-coordinate end point of segment
      real, intent(in) :: px                        ! x-coordinate point P
      real, intent(in) :: py                        ! y-coordinate point P
      real, intent(out) :: fac                      ! interpolation factor between v1 and v2, from v1 (fac = 0) to v2 (fac = 1)
   
      real                :: tol = 1.0e-5      ! tolerance in equality checks
      character(len = 8)  :: str1              ! help string

      real :: p_projx                   ! x-coordinate point P'
      real :: p_projy                   ! y-coordinate point P'
      real :: len2                      ! squared length of e1 = v2-v1

      call proj_point(v1x,v1y,v2x,v2y,px,py,p_projx,p_projy,fac,len2)
      
      str1 = 'Test    '; write(str1(6:8),'(i3)') itest
      call assertEqual((v1x-v2x)**2+(v1y-v2y)**2, len2, tol, &
                       str1 // ': proj_point, len2',__LINE__, __FILE__)
      call assertEqual(((v2x-v1x)*(px-v1x) + (v2y-v1y)*(py-v1y))/len2, fac, tol, &
                       str1 // ': proj_point, fac',__LINE__, __FILE__)
      call assertEqual(((v1x-v2x)*(px-v2x) + (v1y-v2y)*(py-v2y))/len2, 1.-fac, tol, &
                       str1 // ': proj_point, inverse fac', __LINE__,__FILE__)
      call assertEqual(v1x+fac*(v2x-v1x), p_projx, tol, &
                       str1 // ': proj_point, p_projx', __LINE__, __FILE__)
      call assertEqual(v1y+fac*(v2y-v1y), p_projy, tol, &
                       str1 // ': proj_point, p_projy', __LINE__, __FILE__)
   
   end subroutine tst_check_proj_point

end module m_tst_m_ops_utils

program test_m_ops_utils
use no_pfunit, only: conclusion
use m_tst_m_ops_utils
    call tst_is_missing()
    call tst_between_angle()
    call tst_angle180()
    call tst_proj_point()
    call conclusion()
end program test_m_ops_utils 
