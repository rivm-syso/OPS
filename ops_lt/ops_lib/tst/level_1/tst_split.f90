program test_split
use m_ops_building 
use m_error
use no_pfunit
implicit none
    integer, parameter :: my_mClass = 17
    character(len=100) :: pName
    real :: pVals(my_mClass), Val
    real :: qVals(3) = (/ 1.2, 1.4, 6.089 /)
    integer :: n, iClassRead(3)
    call split1( my_mClass, "rainfall 1.2 1.4 6.089", pName , pVals, n )
    call assertEqual(pName, "rainfall", "split1: name")
    call assertEqual(pVals(1:n), qvals, 1e-5, "split1: pVals")
    pName = ' '
    call split1( my_mClass, "rainfall", pName , pVals, n )
    call assertEqual(pName, "rainfall", "split1: name (2)")
    call assertEqual(n, 0, "split1: n")

    call split2( "4 5 6 1.2", 3, iClassRead, val)
    call assertEqual(iClassRead, (/4,5,6/), "split2: iClassRead")
    call assertEqual(val, 1.2, 1e-5, "split2: val")
end program test_split

