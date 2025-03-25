module m_tst_monitornames

implicit none
contains

subroutine tst_monitornames
use no_pfunit
use m_commonfile, only: ctrnam, lognam, indnam, errnam, MakeMonitorNames 
use m_error,     only: TError
implicit none
    TYPE (TError)      :: error


    ctrnam = 'path_to_ctr/input.control'
    call MakeMonitorNames(error)
    print *,'error%haserror = ',error%haserror
    call assertFalse(error%haserror,'MakeMonitorNames returned with an error',__LINE__,__FILE__)
    print *,'lognam="'//trim(lognam)//'"'
    print *,'indnam="'//trim(indnam)//'"'
    print *,'errnam="'//trim(errnam)//'"'
    call assertEqual(lognam,"path_to_ctr/input.log",'wrong lognam',__LINE__,__FILE__)
    call assertEqual(indnam,"path_to_ctr/input.ind",'wrong indnam',__LINE__,__FILE__)
    call assertEqual(errnam,"path_to_ctr/input.err",'wrong errnam',__LINE__,__FILE__)

    ctrnam = 'path_to_ctr/i.n.p.u.t.'
    call MakeMonitorNames(error)
    print *,'error%haserror = ',error%haserror
    call assertFalse(error%haserror,'MakeMonitorNames returned with an error',__LINE__,__FILE__)
    print *,'lognam="'//trim(lognam)//'"'
    print *,'indnam="'//trim(indnam)//'"'
    print *,'errnam="'//trim(errnam)//'"'
    call assertEqual(lognam,"path_to_ctr/i.n.p.u.t.log",'wrong lognam',__LINE__,__FILE__)
    call assertEqual(indnam,"path_to_ctr/i.n.p.u.t.ind",'wrong indnam',__LINE__,__FILE__)
    call assertEqual(errnam,"path_to_ctr/i.n.p.u.t.err",'wrong errnam',__LINE__,__FILE__)

    ctrnam = ''
    call MakeMonitorNames(error)
    print *,'error%haserror = ',error%haserror
    call assertFalse(error%haserror,'MakeMonitorNames returned with an error',__LINE__,__FILE__)
    print *,'lognam="'//trim(lognam)//'"'
    print *,'indnam="'//trim(indnam)//'"'
    print *,'errnam="'//trim(errnam)//'"'
    call assertEqual(lognam,".log",'wrong lognam',__LINE__,__FILE__)
    call assertEqual(indnam,".ind",'wrong indnam',__LINE__,__FILE__)
    call assertEqual(errnam,".err",'wrong errnam',__LINE__,__FILE__)

end subroutine tst_monitornames

end module m_tst_monitornames

program p_tst_monitornames

use no_pfunit
use m_tst_monitornames

implicit none

call tst_monitornames
call conclusion

end program p_tst_monitornames
