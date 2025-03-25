program tst_virtdist
use m_ops_virtdist
use no_pfunit
use m_commonconst_lt, only: PI
implicit none
   real, parameter :: tol = 1e-5
   integer, parameter :: nradii = 3
   real, parameter :: radii(nradii) = (/ 2.0, 18e5, 16e-8 /)
   integer :: itest 
   do itest = 1, nradii
      call assertEqual( ops_virtdist( radii(itest), rond = 1), radii(itest)*12./PI, 'rond')
      call assertEqual( ops_virtdist( radii(itest), rond = 7), radii(itest)*12.*1.128/PI, 'vierkant')
   end do
end program tst_virtdist

