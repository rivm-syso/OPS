module m_tst_commonpath

implicit none

contains

subroutine tst_commonpath
use no_pfunit
use m_commonfile, only: MakeCommonPath, datadir
use m_fileutils, only: fix_slashes
use m_error,     only: TError
implicit none
    
    character(len=200) :: filepath
    TYPE (TError)      :: error

    datadir = '.\level_1/resources/' 
    call fix_slashes(datadir)
    print *,'datadir = "'//trim(datadir)//'"'
    call MakeCommonPath('template3x2_nf3.ops', filepath, error)
    call assertFalse(error%haserror,'MakeCommonPath returned with an error',__LINE__,__FILE__)
    print *,'file="'//trim(filepath)//'" exists'
    call MakeCommonPath('template3x2_nf3_not_available.ops', filepath, error)
    call assertTrue(error%haserror,'MakeCommonPath did not return with an error, but it should have',__LINE__,__FILE__)
    print *,'file="'//trim(filepath)//'" does not exist'

end subroutine tst_commonpath

end module m_tst_commonpath

program p_tst_commonpath

use no_pfunit
use m_tst_commonpath

implicit none

call tst_commonpath
call conclusion

end program p_tst_commonpath
