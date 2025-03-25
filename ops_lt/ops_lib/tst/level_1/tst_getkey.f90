module m_test_getkey
implicit none
   integer, parameter ::  &
      KEY_INTEGER=1, KEY_INTEGER_STRING=2, KEY_REAL=3, KEY_LOGICAL=4, KEY_STRING=5, &
      KEY_RANGED_REAL=6, KEY_RANGED_INTEGER=7, KEY_RANGED_INTEGER_ARRAY=8, KEY_FILENAME=9

   ! Set names of routines to test
   character(len=25), parameter :: rname(9) = (/ &
       'get_key_integer          ', &
       'get_key_integer_string   ', &
       'get_key_real             ', &
       'get_key_logical          ', &
       'get_key_string           ', &
       'check_range_real         ', &
       'check_range_integer      ', &
       'check_range_integer_array', &
       'check_exist_file         '/)

   integer :: i_end = 0
   integer, parameter :: mtest = 100, nintarray=1000
   integer :: intarray(nintarray)
   integer :: nintarray_used=0

   character(len=100) :: str_(mtest)        ! strings written to/read from file
   character(len=100) :: keyname(mtest)     ! reference value for string value read from string, for each test

   ! values and/or ranges:
   character(len=100) :: strval0(mtest)
   real          :: rval0(mtest), rupper(mtest), rlower(mtest)
   integer       :: ival0(mtest), iupper(mtest), ilower(mtest)
   integer       :: intarrstart(mtest), intarrend(mtest)

   logical       :: lval0(mtest)            ! reference value for logical value read from string, for each test
   logical       :: error0(mtest)           ! reference value for error%haserror, for each test
   logical       :: isrequired0(mtest)      ! logical is required
   logical       :: must_exist0(mtest)       ! logical: file must exist
   integer       :: i_rname(mtest)          ! index into routine names for each test

interface create_key
   module procedure create_integer_key 
   module procedure create_integer_array_key 
   module procedure create_integer_string_key 
   module procedure create_real_key 
   module procedure create_logical_key 
   module procedure create_string_key 
end interface create_key

contains

   function error_string(error, keyfound) result(errstr)
   use m_error, only: Terror, write_error
   type(Terror), intent(in)  :: error
   logical,      intent(in) :: keyfound
   character(len=40) :: errstr
      if (error%haserror .and. keyfound) errstr = '***error***'
      if (error%haserror .and. .not. keyfound) errstr = '**key unknown**, ***error***'
      if (.not.error%haserror .and. .not. keyfound) errstr = '**key unknown**'
      if (.not.error%haserror .and. keyfound) errstr = ' ' 
   end function error_string

   subroutine check_keys()
   use no_pfunit
   use m_getkey
   use m_error, only: Terror, write_error
   use m_commonfile, only: IOB_STDOUT
   logical :: lval
   real    :: rval
   character(len=200) :: strval
   integer :: ival

   integer :: j, nword
   integer :: i
   type(Terror) :: error
   character(len=200) :: msg
   logical :: keyfound
   integer :: ivalues(nintarray) 

      do i = 1,i_end
         error%haserror = .false. 
         write(msg,'(a,i2.2,3a)') '=== test ',i,' ',rname(i_rname(i)),' === '//str_(i)

         ! Output to screen:
         write(*,*) msg
         
         ! Get key - value:
         if (i_rname(i) == KEY_INTEGER) then
            keyfound = GetKeyValue(keyname(i), ival, error)
            write(*,'(i8,a)') &
              ival,trim(error_string(error,keyfound))
            call assertEqual(ival0(i),ival,msg,__LINE__,__FILE__)
         else if (i_rname(i) == KEY_RANGED_INTEGER_ARRAY) then
            keyfound = GetCheckedKey(keyname(i), ilower(i), iupper(i), &
                           nword, ivalues, error)
            if (.not. error%haserror) then
               call assertEqual( intarrend(i) - intarrstart(i)+1, nword, 'array length',__LINE__, __FILE__)
               do j = 1,nword
                  write(*,'(3(i8,a))') &
                    ilower(i),'<=',ivalues(j),'<=',iupper(i),trim(error_string(error,keyfound))
                  call assertEqual(intarray(intarrstart(i)+j-1), ivalues(j),msg,__LINE__, __FILE__)
               end do
            end if
         else if (i_rname(i) == KEY_FILENAME) then
            keyfound = check_exist_file(keyname(i), isrequired0(i), must_exist0(i), strval, error)
            write(*,'(3(a,a))') strval
            call assertEqual(strval0(i),strval,msg,__LINE__, __FILE__)
         else if (i_rname(i) == KEY_RANGED_INTEGER) then
            keyfound = GetCheckedKey(keyname(i), ilower(i), iupper(i), isrequired0(i), ival, error)
            write(*,'(3(i8,a))') &
              ilower(i),'<=',ival,'<=',iupper(i),trim(error_string(error,keyfound))
            call assertEqual(ival0(i),ival,msg,__LINE__, __FILE__)
         elseif (i_rname(i) == KEY_INTEGER_STRING) then
            keyfound = GetKeyValue(keyname(i), ival, strval, error)
            write(*,'(i8,a20,a)') &
              ival,strval,trim(error_string(error,keyfound))
            call assertEqual(ival0(i),ival,msg,__LINE__, __FILE__)
            call assertEqual(strval0(i),strval,msg,__LINE__, __FILE__)
         elseif (i_rname(i) == KEY_REAL) then
            keyfound = GetKeyValue(keyname(i), rval, error)
            write(*,'(f8.1,a)') &
              rval,trim(error_string(error,keyfound))
            call assertEqual(rval0(i),rval,msg,__LINE__, __FILE__)
         elseif (i_rname(i) == KEY_RANGED_REAL) then
            keyfound = GetCheckedKey(keyname(i), rlower(i), rupper(i), isrequired0(i), rval, error)
            write(*,'(3(f8.1,a))') &
              rlower(i), '<=', rval,'<=', rupper(i), trim(error_string(error,keyfound))
            call assertEqual(rval0(i),rval,msg,__LINE__, __FILE__)
         elseif (i_rname(i)==KEY_LOGICAL) then
            keyfound = GetKeyValue(keyname(i), isrequired0(i), lval, error)
            strval='FALSE'
            if (lval) strval='TRUE'
            write(*,'(a8,a)') &
              trim(strval),trim(error_string(error,keyfound))
            call assertEqual(lval0(i),lval,msg,__LINE__, __FILE__)
         elseif (i_rname(i)== KEY_STRING ) then
            keyfound = GetKeyValue(keyname(i), strval, error)
            write(*,'(a20,a8,a)') &
              trim(strval),trim(error_string(error,keyfound))
            call assertEqual(strval0(i),strval,msg,__LINE__, __FILE__)
         endif
         
         ! Check if error has been trapped:
         call assertEqual(error0(i),error%haserror,'checking whether getkey returns an error',__LINE__,__FILE__)
         if (.not.error0(i) .and. error%haserror) call write_error(IOB_STDOUT, error)
      enddo
   end subroutine check_keys

   subroutine print_keyfile(iu)
   integer, intent(in) :: iu
   integer :: i
      write(iu,'(a)') '   '
      do i = 1,i_end
         write(iu,'(a)') trim(str_(i))
      enddo
   end subroutine print_keyfile

   subroutine create_new_key(kname, incorrect_value, correct_value)
   character(len=*), intent(in)  :: kname
   character(len=*), intent(in), optional :: incorrect_value, correct_value
      i_end = i_end + 1
      keyname(i_end) = kname
      error0(i_end)  = present(incorrect_value)
      if (present(incorrect_value)) then
         str_(i_end) = trim(kname)//' '//incorrect_value
      else if (present(correct_value)) then
         str_(i_end) = trim(kname)//' '//trim(correct_value)
      end if
   end subroutine create_new_key

   subroutine create_integer_key(kname, ivalue, incorrect_value, correct_value, ranges, required)
   character(len=*), intent(in)  :: kname
   integer,          intent(in) :: ivalue
   character(len=*), intent(in), optional :: incorrect_value, correct_value
   integer,          intent(in), optional :: ranges(2)
   logical,          intent(in), optional :: required

      call create_new_key(kname, incorrect_value, correct_value)
      ival0(i_end)   = ivalue
      i_rname(i_end) = KEY_INTEGER
      if (present(ranges) .or. present(required)) then
         i_rname(i_end) = KEY_RANGED_INTEGER
         if (present(ranges)) then
            ilower(i_end) = ranges(1)
            iupper(i_end) = ranges(2)
         else
            ilower(i_end) = -1e8
            iupper(i_end) = +1e8
         end if
         isrequired0(i_end) = .false.
         if (present(required)) isrequired0(i_end) = required
      end if

      if (present(correct_value) .or. present(incorrect_value)) return 
      write(str_(i_end),'(a,i0)') trim(kname)//' ',ivalue
   end subroutine create_integer_key

   subroutine create_integer_array_key(kname, ivalues, ranges, required, incorrect_value, correct_value)
   character(len=*), intent(in) :: kname
   integer,          intent(in) :: ivalues(:)
   integer,          intent(in) :: ranges(2)
   logical,          intent(in), optional :: required
   character(len=*), intent(in), optional :: incorrect_value, correct_value

      call create_new_key(kname, incorrect_value, correct_value)
      i_rname(i_end) = KEY_RANGED_INTEGER_ARRAY
      ilower(i_end) = ranges(1)
      iupper(i_end) = ranges(2)
      isrequired0(i_end) = .false.
      if (present(required)) isrequired0(i_end) = required
      
      intarrstart(i_end) = nintarray_used+1
      intarrend(i_end)  = nintarray_used + size(ivalues)
      nintarray_used = nintarray_used + size(ivalues) 
      if (nintarray_used > nintarray) then
         print *,'too many integers in arrays'
         stop 1
      end if
      intarray( intarrstart(i_end): intarrend(i_end)) = ivalues

      if (present(correct_value) .or. present(incorrect_value)) return 
      write(str_(i_end),'(a,10i5)') trim(kname)//' ',ivalues
   end subroutine create_integer_array_key

   subroutine create_real_key(kname, rvalue, incorrect_value, correct_value, ranges, required)
   character(len=*), intent(in) :: kname
   real,             intent(in) :: rvalue
   character(len=*), intent(in), optional :: incorrect_value, correct_value
   real,             intent(in), optional :: ranges(2)
   logical,          intent(in), optional :: required

      call create_new_key(kname, incorrect_value, correct_value)
      rval0(i_end)   = rvalue
      i_rname(i_end) = KEY_REAL
      if (present(ranges) .or. present(required)) then
         i_rname(i_end) = KEY_RANGED_REAL
         if (present(ranges)) then
            rlower(i_end) = ranges(1)
            rupper(i_end) = ranges(2)
         else
            rlower(i_end) = -1e30
            rupper(i_end) = +1e30
         end if
         isrequired0(i_end) = .false.
         if (present(required)) isrequired0(i_end) = required
      end if

      if (present(correct_value) .or. present(incorrect_value)) return 
      write(str_(i_end),'(a,g20.10)') trim(kname)//' ',rvalue
   end subroutine create_real_key

   subroutine create_file_key(kname, fname, required, must_exist, incorrect_value, correct_value)
   character(len=*), intent(in) :: kname
   character(len=*), intent(in) :: fname
   logical,          intent(in), optional :: required, must_exist
   character(len=*), intent(in), optional :: incorrect_value, correct_value

      call create_new_key(kname, incorrect_value, correct_value)
      strval0(i_end)   = fname
      isrequired0(i_end) = .false.
      if (present(required)) isrequired0(i_end) = required
      must_exist0(i_end) = .false.
      if (present(must_exist)) must_exist0(i_end) = must_exist
      i_rname(i_end) = KEY_FILENAME

      if (present(correct_value) .or. present(incorrect_value)) return 
      str_(i_end) = trim(kname)//' '//trim(fname)
   end subroutine create_file_key

   subroutine create_logical_key(kname, lvalue, required, incorrect_value, correct_value)
   character(len=*), intent(in) :: kname
   logical,          intent(in) :: lvalue
   logical,          intent(in), optional :: required
   character(len=*), intent(in), optional :: incorrect_value, correct_value

      call create_new_key(kname, incorrect_value, correct_value)
      lval0(i_end)   = lvalue
      isrequired0(i_end) = .false.
      if (present(required)) isrequired0(i_end) = required
      i_rname(i_end) = KEY_LOGICAL

      if (present(correct_value) .or. present(incorrect_value)) return 
      str_(i_end) = trim(kname)//' 0'
      if (lvalue) str_(i_end) = trim(kname)//' 1'
   end subroutine create_logical_key

   subroutine create_integer_string_key(kname, string, ivalue, incorrect_value, correct_value)
   character(len=*), intent(in) :: kname
   character(len=*), intent(in) :: string
   integer,          intent(in) :: ivalue
   character(len=*), intent(in), optional :: incorrect_value, correct_value

      call create_new_key(kname, incorrect_value, correct_value)
      ival0(i_end)   = ivalue
      strval0(i_end) = string
      i_rname(i_end) = KEY_INTEGER_STRING

      if (present(correct_value) .or. present(incorrect_value)) return 
      write(str_(i_end),'(a,i0,a)') trim(kname)//' ',ivalue,' '//trim(string)
   end subroutine create_integer_string_key

   subroutine create_string_key(kname, string, incorrect_value, correct_value)
   character(len=*), intent(in) :: kname
   character(len=*), intent(in) :: string
   character(len=*), intent(in), optional :: incorrect_value, correct_value

      call create_new_key(kname, incorrect_value, correct_value)
      strval0(i_end) = string
      i_rname(i_end) = KEY_STRING
      if (present(correct_value) .or. present(incorrect_value)) return 
      str_(i_end) = trim(kname)//' '//trim(string)
   end subroutine create_string_key
  
end module m_test_getkey

program test_getkey
use m_test_getkey
use no_pfunit
use m_commonfile, only: IOB_STDOUT, fu_input
use m_commonconst_lib, only: MISVALNUM
use m_fileutils, only: fix_slashes
IMPLICIT NONE
   character(len=100) :: filename
   filename='./level_1/resources\tst_m_getkey.in'
   call fix_slashes(filename)

   ! Define records to be read later on and set reference data (ival0,rval0,lval0 or strval0) for each record:
   call create_file_key('TEST', fname = filename, required=.true., must_exist=.true.)
   call create_file_key('TEST', fname = '/not_existing/file.ext', required=.true., must_exist=.true., &
                        incorrect_value = '/not_existing/file.ext')

   call create_key('TEST', ivalues=(/1,2,10/), ranges = (/1,10/))
   call create_key('TEST', ivalues=(/1,12,10/), ranges = (/1,10/), incorrect_value = '1 12 10')
   call create_key('TEST', ivalues=(/1,7,0/), ranges = (/1,10/), incorrect_value = '1 7 0')
   call create_key('TEST', ivalues=(/1,0,10/), ranges = (/1,10/), incorrect_value = '1 0 10')

   call create_key('STRING', string='this is a string')
   call create_key('STRING', string='this is a string2',     correct_value='this is a string2 ! this is a comment')
   call create_key('XSTRING', string='XSTRING this is a string',      incorrect_value='this is a string')
   keyname(i_end) = 'STRING'

   call create_key('TEST', lvalue=.true.)
   call create_key('TEST', lvalue=.false.)
   call create_key('TEST', lvalue=.false., correct_value='-9999')
   call create_key('TEST', lvalue=.false., required=.true., incorrect_value='-9999 ! isrequired')
   call create_key('TEST', lvalue=.false.,                  incorrect_value='2')

   call create_key('TEST', ivalue=10)
   call create_key('TEST', ivalue=MISVALNUM, correct_value = ' ')
   call create_key('TEST', ivalue=12, correct_value = '12 hallo1')
   call create_key('TEST', ivalue=12,                           incorrect_value = '99hallo2')
   call create_key('TEST', ivalue=27, ranges = (/ 26, 29 /))
   call create_key('TEST', ivalue=MISVALNUM, required = .true., incorrect_value= ' ')
   call create_key('TEST', ivalue=MISVALNUM, ranges = (/ 26, 29 /))
   call create_key('TEST', ivalue=27, ranges = (/ 16, 19 /),    incorrect_value='27')
   call create_key('TEST', ivalue=27, ranges = (/ 36, 39 /),    incorrect_value='27')

   call create_key('TEST', rvalue=16.0)
   call create_key('TEST', rvalue=17.0, correct_value='17.0 ! comment')
   call create_key('TEST', rvalue=real(MISVALNUM), correct_value = ' ')
   call create_key('TEST', rvalue=12.8, ranges = (/ 6.0, 19.0 /))
   call create_key('TEST', rvalue=real(MISVALNUM), required = .true., incorrect_value= ' ')
   call create_key('TEST', rvalue=real(MISVALNUM), ranges = (/ 6.0, 19.0 /))
   call create_key('TEST', rvalue=12.8, ranges = (/ 16.0, 19.0 /), incorrect_value='12.8')
   call create_key('TEST', rvalue=12.8, ranges = (/  6.0,  9.0 /), incorrect_value='12.8')

   call create_key('TEST', ivalue=13, string='hallo3')
   call create_key('TEST', ivalue=14, string='hallo4', correct_value = '14 hallo4 ! comment')
   call create_key('TEST', ivalue=15, string=' ',      correct_value = '15 ! comment2')
   call create_key('TEST', ivalue=MISVALNUM, string=' ',      correct_value = ' ')


   print *,'Contents of keyfile:'
   print *
   call print_keyfile(IOB_STDOUT) 
   print *

   ! GetKeyValue reads from fu_input:
   open(fu_input, file=filename)
   call print_keyfile(fu_input) 
   close(fu_input)
   open(fu_input, file=filename)

   call check_keys()

   call conclusion()
end program test_getkey


