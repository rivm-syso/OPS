module m_tst_fileutils

implicit none

contains

subroutine tst_fileutils

use m_error, only: TError
use m_fileutils
use no_pfunit
implicit none
   TYPE (TError)      :: error                      ! error handling record
   character(len=100) :: path, str, directory, filename, contents
   logical            :: lresult, end_of_file
   integer, parameter :: iu = 12
   real, parameter    :: tol = 1e-5
   real, parameter    :: constants(3) = (/ 12.0, 11.0, 10.0 /)
   real               :: reals(3)

   path = '/road/to/nowhere'
   call fix_slashes(path)
   call get_directory(path, directory, error)
   call assertFalse(error%haserror, "get_directory should pass without an error")
   call get_filename(path, filename, error)
   call assertFalse(error%haserror, "get_filename should pass without an error")
   str = '/road/to/'
   call fix_slashes(str)
   call assertEqual(directory, str, 'directory is expected to be "'//trim(str)//'"')
   str = 'nowhere'
   call assertEqual(filename, str, 'file is expected to be "'//trim(str)//'"')

   path = 'road_to_nowhere'
   call get_directory(path, directory, error)
   call assertFalse(error%haserror, "get_directory should pass without an error")
   str = ' '
   call assertEqual(directory, str, 'directory is expected to be "'//trim(str)//'"')
   call get_filename(path, filename, error)
   call assertFalse(error%haserror, "get_filename should pass without an error")
   str = 'road_to_nowhere'
   call assertEqual(filename, str, 'file is expected to be "'//trim(str)//'"')

   lresult = sys_open_file(iu=iu, filename='', rw='', filetype='nameless file', error=error)
   call assertTrue(error%haserror, "sys_open_file should pass with an error")
   call assertFalse(lresult, "sys_open_file should return False")
   error%haserror=.false.

   lresult = sys_open_file(iu=iu, filename='/forbidden', rw='', filetype='forbidden file', error=error)
   call assertTrue(error%haserror, "sys_open_file should pass with an error")
   call assertFalse(lresult, "sys_open_file should return False")
   error%haserror=.false.

   lresult = sys_open_file(iu=iu, filename='nonexistent.file', rw='r', filetype='text file', error=error)
   call assertTrue(error%haserror, "sys_open_file should pass with an error")
   error%haserror=.false.

   lresult = sys_open_file(iu=iu, filename='./level_1/resources/testfile.txt', rw='w', filetype='text file', error=error)
   call assertFalse(error%haserror, "sys_open_file should pass without an error")
   lresult = sys_open_file(iu=iu, filename='./level_1/resources/testfile.txt', rw='w', filetype='text file', error=error)
   call assertTrue(error%haserror, "sys_open_file should pass with an error for an open file")
   error%haserror=.false.
   call assertFalse(lresult, "sys_open_file should return False")
   write(unit=iu,fmt='(a)') 'test uitvoer'
   write(unit=iu,fmt='(a)') 'line 2'
   call sys_close_file(iu, filename='./level_1/resources/testfile.txt', error=error)
   call assertFalse(error%haserror, "sys_close_file should pass without an error", __LINE__, __FILE__)
   lresult = sys_open_file(iu=iu, filename='./level_1/resources/testfile.txt', rw='r', filetype='text file', error=error)
   call assertFalse(error%haserror, "sys_open_file should pass without an error")
   call assertTrue(lresult, "sys_open_file should return True")
   call sys_read_string(iu, contents, end_of_file, error)
   call assertFalse(error%haserror, "reading a string should pass without an error")
   call assertFalse(end_of_file, "file is not yet at an end")
   str = "test uitvoer"
   call assertEqual(contents, str, 'file contents is expected to be "'//trim(str)//'"')
   call sys_read_string(iu, contents, end_of_file, error)
   call assertFalse(error%haserror, "reading a string should pass without an error")
   call assertFalse(end_of_file, "file is not yet at an end")
   str = "line 2"
   call assertEqual(contents, str, 'file contents is expected to be "'//trim(str)//'"')
   call sys_close_file(iu, filename='./level_1/resources/testfile.txt', error=error)
   call assertFalse(error%haserror, "sys_close_file should pass without an error", __LINE__, __FILE__)
   call sys_read_string(iu, contents, end_of_file, error)
   call assertTrue(error%haserror, "reading from a closed file should return an error",__LINE__, __FILE__)
   error%haserror=.false.
   str = " "
   call assertEqual(contents, str, 'file contents is expected to be "'//trim(str)//'"')
   call sys_close_file(iu, filename='./level_1/resources/testfile.txt', error=error)
   ! call assertTrue(error%haserror, "closing a closed file should return an error", __LINE__, __FILE__)  ! TODO:
   error%haserror=.false. ! reset haserror

   call sys_read_string(89, contents, end_of_file, error)
   call assertTrue(error%haserror, "reading a string from a closed file should return an error",__LINE__, __FILE__)
   error%haserror=.false.
   str = " "
   call assertEqual(contents, str, 'file contents is expected to be "'//trim(str)//'"')
   error%haserror=.false.

   lresult = sys_open_file(iu=iu, filename='./level_1/resources/testfile.bin', rw='wb', filetype='binary file', error=error)
   call assertFalse(error%haserror, "sys_open_file should pass without an error")
   call assertTrue(lresult, "sys_open_file should return True")
   write(unit=iu) constants
   call sys_close_file(iu, filename='./level_1/resources/testfile.bin', error=error)
   call assertFalse(error%haserror, "sys_close_file should pass without an error", __LINE__, __FILE__)
   lresult = sys_open_file(iu=iu, filename='./level_1/resources/testfile.bin', rw='rb', filetype='binary file', error=error)
   call assertFalse(error%haserror, "sys_open_file should pass without an error")
   call assertTrue(lresult, "sys_open_file should return True")
   reals(:) = 7.6
   read(unit=iu) reals
   call sys_close_file(iu, filename='./level_1/resources/testfile.bin', error=error)
   call assertFalse(error%haserror, "sys_close_file should pass without an error", __LINE__, __FILE__)
   call assertEqual(constants, reals, tol, 'contents of "./level_1/resources/testfile.bin" are expected to be the constants array')

   lresult = sys_open_file(iu=iu, filename='./level_1/resources/testfile.direct', rw='wd', filetype='direct access file', error=error)
   call assertFalse(error%haserror, "sys_open_file should pass without an error")
   call assertTrue(lresult, "sys_open_file should return True")
   write(unit=iu, rec=14) constants
   call sys_close_file(iu, filename='./level_1/resources/testfile.direct', error=error)
   call assertFalse(error%haserror, "sys_close_file should pass without an error")
   lresult = sys_open_file(iu=iu, filename='./level_1/resources/testfile.direct', rw='d', filetype='direct access file', error=error)
   call assertFalse(error%haserror, "sys_open_file should pass without an error")
   call assertTrue(lresult, "sys_open_file should return True")
   reals(:) = 7.6
   read(unit=iu, rec=14) reals
   call sys_close_file(iu, filename='./level_1/resources/testfile.direct', error=error)
   call assertFalse(error%haserror, "sys_close_file should pass without an error")
   call assertEqual(constants, reals, tol, 'direct access file contents is expected to be the constants array')

   lresult = sys_open_file(iu=iu, filename='./level_1/resources/testfile.direct.128', rw='wd', filetype='direct access file 128', error=error, LREC=128)
   call assertFalse(error%haserror, "sys_open_file should pass without an error")
   call assertTrue(lresult, "sys_open_file should return True")
   write(unit=iu, rec=14) constants
   call sys_close_file(iu, filename='./level_1/resources/testfile.direct.128', error=error)
   call assertFalse(error%haserror, "sys_close_file should pass without an error")
   lresult = sys_open_file(iu=iu, filename='./level_1/resources/testfile.direct.128', rw='d', filetype='direct access file', error=error, LREC=128)
   call assertFalse(error%haserror, "sys_open_file should pass without an error")
   call assertTrue(lresult, "sys_open_file should return True")
   reals(:) = 7.6
   read(unit=iu, rec=14) reals
   call sys_close_file(iu, filename='./level_1/resources/testfile.direct.128', error=error)
   call assertFalse(error%haserror, "sys_close_file should pass without an error")
   call assertEqual(constants, reals, tol, 'direct access file contents is expected to be the constants array')

end subroutine tst_fileutils

!-----------------------------------------------------------------
subroutine tst_getfilename

use no_pfunit
use m_commonfile, only: IOB_STDOUT
use m_fileutils
use m_error

implicit none

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER(len=512)                  :: fullpath                   ! full name of file (incl. path)

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER(len=128)                  :: filename                   ! file name (path stripped)
TYPE (TError)                       :: error                      ! error handling record
CHARACTER(len=128)                  :: str1                       ! string

fullpath = './inv/test1.inv'
call getfilename(fullpath, filename, error)
if (error%haserror) goto 9999
call assertEqual('test1.inv',filename,'tst_getfilename 1',__LINE__,__FILE__)

fullpath = 'S:/OPS/Users/sauterf/tmp/test1.inv'
call getfilename(fullpath, filename, error)
if (error%haserror) goto 9999
call assertEqual('test1.inv',filename,'tst_getfilename 2',__LINE__,__FILE__)

! Check error (target string too small):
fullpath = 'S:/OPS/Users/sauterf/tmp/test1.inv'
call getfilename(fullpath, filename(1:2), error)
if (error%haserror) then
   str1 = 'Capacity output string less than required '
   call assertEqual(str1,error%message,'tst_getfilename 3',__LINE__,__FILE__)
else
   ! failure because haserror = F, it should be T
   call assertTrue(error%haserror,'Error has not been trapped; tst_getfilename 4',__LINE__,__FILE__)
   error%haserror=.false.
   write(*,*) 'aaaaaaaaaaaaa'
endif



return

9999 CALL ErrorCall('tst_getfilename', error)
call write_error(IOB_STDOUT,error)

end subroutine tst_getfilename

!---------------------------------------------------------------
subroutine tst_getbasename

use no_pfunit
use m_fileutils
use m_error

implicit none

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER(len=512)                  :: fullpath                   ! full path and name of file

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER(len=128)                  :: basename_spaces            ! base name of file (path and extension stripped); including trailing spaces
TYPE (TError)                       :: error                      ! error handling record

CHARACTER(len=:), allocatable       :: basename                   ! base name of file (path and extension stripped); no trailing spaces



fullpath = './inv/test1.inv'
call getbasename(fullpath, basename_spaces, error)
basename = trim(basename_spaces)
call assertEqual('test1.err',basename//'.err','tst_getbasename 1',__LINE__,__FILE__)

fullpath = 'S:/OPS/Users/sauterf/tmp/test1.inv'
call getbasename(fullpath, basename_spaces, error)
basename = trim(basename_spaces)
call assertEqual('test1.err',basename//'.err','tst_getbasename 2',__LINE__,__FILE__)

fullpath = 'S:/OPS/Users/sauterf/tmp/test1'
call getbasename(fullpath, basename_spaces, error)
basename = trim(basename_spaces)
call assertEqual('test1.err',basename//'.err','tst_getbasename 3',__LINE__,__FILE__)



end subroutine tst_getbasename

end module m_tst_fileutils


program p_tst_fileutils

use no_pfunit, only: conclusion
use m_tst_fileutils

implicit none

call tst_fileutils
call tst_getfilename
call tst_getbasename
call conclusion
    
end program p_tst_fileutils


