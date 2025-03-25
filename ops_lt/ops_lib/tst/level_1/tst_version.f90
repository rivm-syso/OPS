module m_test_version
contains

    logical function is_digit(a)
    implicit none
       character, intent(in) :: a 
       is_digit = index('0123456789',a) > 0
    end function is_digit

    subroutine check_version_3numbers(dll_version, message)
    use no_pfunit
    implicit none
      CHARACTER(len=*), intent(in) :: dll_version, message

      CHARACTER(len=100) :: wrk
      integer :: idx_dot, nversion, idx
      integer :: dll_digits(3)

      print *,'testing '//trim(message)//' version = "'//trim(dll_version)//'"'
      wrk = dll_version
      idx_dot = 10
      nversion = 0
      do while (wrk /= ' ') 
         idx_dot = index(wrk,'.') 
         call assertTrue(idx_dot/=1, 'illegal '//trim(message)//'"'//trim(dll_version)//'"') 
         if (idx_dot == 0) idx_dot = len_trim(wrk)+1
         nversion = nversion + 1
         call assertTrue(nversion<=3, 'illegal '//trim(message)//'"'//trim(dll_version)//'"') 
         do idx = 1,idx_dot-1
            call assertTrue(is_digit(wrk(idx:idx)) , 'illegal '//trim(message)//'"'//trim(dll_version)//'"') 
         end do
         read(wrk(1:idx_dot-1),*) dll_digits(nversion)
         wrk = wrk(idx_dot+1:)
      end do
      call assertEqual(nversion, 3, 'illegal '//trim(message)//' "'//trim(dll_version)//'"') 
      print '(10(a,i0))',trim(message)//' version OK : ',dll_digits(1),'.',dll_digits(2),'.',dll_digits(3)
   end subroutine check_version_3numbers

   subroutine check_date(date, message)
   use no_pfunit
   implicit none
      CHARACTER(len=*), intent(in) :: date, message

      integer :: idx_space, idx, iday, imonth, jmonth, iyear
      character(len=3), parameter :: months(12) = (/ 'jan', 'feb', 'mrt', 'apr', 'mei', 'jun', 'jul', 'aug', 'sep', 'okt', 'nov', 'dec' /)
      integer, parameter ::  days_per_month(12) = (/    31,    29,    31,    30,    31,    30,    31,    31,    30,    31,    30,    31 /)
      CHARACTER(len=100) :: wrk

      wrk = adjustl(date)
      idx_space = index(wrk,' ')
      do idx = 1,idx_space-1
          call assertTrue(is_digit(wrk(idx:idx)), 'not a day-number in '//trim(message)//'"'//trim(date)//'"') 
      end do
      read(wrk(:idx_space), *) iday
      wrk = adjustl(wrk(idx_space:))
      idx_space = index(wrk,' ')
      call assertEqual(idx_space, 4, 'not a month in '//trim(message)//'"'//trim(date)//'"') 
      imonth = 0
      do jmonth = 1,12
         if (months(jmonth) == wrk(:3)) then
            imonth = jmonth
            call assertTrue(iday > 0 .and. iday <= days_per_month(imonth), 'day is not in the month in '//trim(message)//'"'//trim(date)//'"') 
            exit
         end if
      end do
      call assertTrue(imonth > 0 , 'month '//wrk(:3)//' in '//trim(message)//'"'//trim(date)//'" does not exist')
      wrk = adjustl(wrk(idx_space:))
      idx_space = index(wrk,' ')
      call assertEqual(idx_space, 5, 'not a year in '//trim(message)//'"'//trim(date)//'"') 
      do idx = 1,idx_space-1
          call assertTrue(is_digit(wrk(idx:idx)), 'year in '//trim(message)//'"'//trim(date)//'" is not valid') 
      end do
      read(wrk(:idx_space), *) iyear
      call assertTrue(iyear >= 1970 .and. iyear <= 2100, 'year in '//trim(message)//'"'//trim(date)//'" is not valid') 

      print '(a)', 'date "'//trim(message)//'" is OK'
      print '(10(a,i0))','   year  = ',iyear
      print '(10(a,i0))','   month = ',imonth,': '//months(imonth)//' (',days_per_month(imonth),' days)'
      print '(10(a,i0))','   day   = ',iday
   
   end subroutine check_date


end module m_test_version


program test_version
use no_pfunit, only: conclusion
use m_commonfile, only: get_version_core
use m_utils, only: get_version_utils
use m_test_version
implicit none
   CHARACTER(len=100) :: dll_version, dll_date

   call get_version_core(dll_version, dll_date)
   call check_version_3numbers(dll_version, 'core version')
   call check_date(dll_date, 'core version')

   dll_version = ' '
   dll_date = ' '
   call get_version_utils(dll_version, dll_date)
   call check_version_3numbers(dll_version, 'depac version')
   call check_date(dll_date, 'depac version')

   call conclusion()
end program test_version

