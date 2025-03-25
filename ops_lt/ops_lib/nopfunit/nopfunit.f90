module no_pfunit
Use iso_fortran_env, only : int16
implicit none

    integer :: nok = 0, nerror = 0
    logical :: verbose = .false.
    logical :: errors_are_fatal = .false.  ! keep running when an error is found, but remember the error for subroutine conclusion
    character(len=400) :: test_name = ' '

    interface assertRelativelyEqual
       module procedure  assertRelativelyEqual_scalar 
       module procedure  assertRelativelyEqual_dble_scalar 
    end interface assertRelativelyEqual

    interface assertEqual
       module procedure  assertEqual_array_string
       module procedure  assertEqual_scalar_string
       module procedure  assertEqual_scalar_bool
       module procedure  assertEqual_scalar_int
       module procedure  assertEqual_scalar_double_notol
       module procedure  assertEqual_scalar_dble
       module procedure  assertEqual_scalar 
       module procedure  assertEqual_scalar_notol
       module procedure  assertEqual_array
       module procedure  assertEqual_array_int
       module procedure  assertEqual_array_int16
       
       
       ! module procedure  assertEqual_array_real_1d
       ! module procedure  assertEqual_array_real_2d
       ! module procedure  assertEqual_array_real_3d
       module procedure  assertEqual_array_bool
       module procedure  assertEqual_array_5d
       module procedure  assertEqual_array_4d
       module procedure  assertEqual_array_2d
       module procedure  assertEqual_array_3d
       module procedure  assertEqual_array_2d_notol
       module procedure  assertEqual_array_int16_2d
    end interface assertEqual

    interface assertGreaterThan
       module procedure  assertGreaterThan_int
       module procedure  assertGreaterThan_float
    end interface assertGreaterThan

    interface assertLessThan
       module procedure  assertLessThan_int
       module procedure  assertLessThan_float
    end interface assertLessThan

contains

    subroutine CompareTextFiles(fname, fname_ref)
    Use, intrinsic :: iso_fortran_env, Only : iostat_end
       character(len=*), intent(in) :: fname, fname_ref

       integer :: fid, fid_ref, iostat, iline, iostat_ref
       character(len=200) :: line, line_ref, msg

       open(newunit=fid, file = fname, action = 'read', iostat=iostat)
       call AssertEqual(iostat,0,'Opening file "'//fname//'"', __LINE__, __FILE__)
       open(newunit=fid_ref, file = fname_ref, action = 'read', iostat=iostat)
       call AssertEqual(iostat,0,'Opening file "'//fname_ref//'"', __LINE__, __FILE__)

       iline = 0
       do while(.true.) 
          iline = iline + 1
          write(msg,'(10(a,i0))') 'line ',iline
          read(fid, '(a)', iostat=iostat) line
          read(fid_ref, '(a)', iostat=iostat_ref) line_ref
          if (iostat_ref == iostat_end .and. iostat == iostat_end) exit
          call AssertEqual(iostat,0,'error reading '//trim(msg), __LINE__, __FILE__)
          call AssertEqual(iostat_ref,0,'error reading reference '//trim(msg), __LINE__, __FILE__)
          call AssertEqual(trim(line),trim(line_ref),'difference in '//trim(msg), __LINE__, __FILE__)
          if (nerror>2) exit
       end do
       close(fid)
       close(fid_ref)
    end subroutine CompareTextFiles

    subroutine check_options()
        integer :: iarg
        character(len=500) :: arg
        if (nok + nerror > 0) return
        do iarg = 1, command_argument_count()
           call get_command_argument(iarg, arg)
           if (arg=='--verbose') verbose = .true.
           if (arg=='--silent') verbose = .false.
           if (arg=='--errors-are-fatal') errors_are_fatal = .true.
           if (arg=='--errors-are-not-fatal') errors_are_fatal = .false.
        end do
    end subroutine check_options

    subroutine conclusion()
       print *
       if (nerror == 0) then
          print '(10(a,i0))', 'test '//trim(test_name)//' finished successfully: ',nok,' assessments passed'
       else
          print '(10(a,i0))', 'test '//trim(test_name)//' failed: ',nok,' assessments passed, but ',nerror,' failed'
          stop 1
       end if
    end subroutine conclusion
    
    subroutine print_result(message, lineno, filename, msg, success)
    character(len=*), intent(in), optional :: message
    integer,          intent(in), optional :: lineno
    character(len=*), intent(in), optional :: filename
    logical,          intent(in), optional :: success
    character(len=*), intent(in), optional :: msg
    character(len=512) :: str

       call check_options()
       if (.not. success) then
          nerror = nerror + 1
       else
          nok = nok + 1
       end if
       if (success .and. .not. verbose) return

       if (.not. success) then
          str = 'ERROR was detected'
       else
          str = 'test was SUCCESSFUL'
       end if

       if (test_name /= ' ') str = trim(str) // ' in test '// trim(test_name)
       print '(a)',trim(str)

       if (present(message)) print '(a)','    '//trim(message)

       if (present(filename) .and. present(lineno)) then
          str = filename
          do while (index(str,'/') > 0) 
             str = str(index(str,'/')+1:)
          end do
          do while (index(str,'\') > 0) 
             str = str(index(str,'\')+1:)
          end do
          print '(10(a,i0))','    in line ',lineno,' of file '//trim(str)
       end if

       print '(a)','     '//trim(msg)
       print *
       if (errors_are_fatal) stop 1
    end subroutine print_result


    subroutine assertTrue(val1, message, lineno, filename)
    logical, intent(in) :: val1
    character(len=*), intent(in), optional :: message
    integer,          intent(in), optional :: lineno
    character(len=*), intent(in), optional :: filename
    character(len=512) :: msg
       write(msg,*) 'checking whether value ',val1,' is .true.'
       call print_result(message, lineno, filename, msg, success=val1)
    end subroutine assertTrue

    subroutine assertFalse(val1, message, lineno, filename)
    logical, intent(in) :: val1
    character(len=*), intent(in), optional :: message
    integer,          intent(in), optional :: lineno
    character(len=*), intent(in), optional :: filename
    character(len=512) :: msg
       write(msg,*) 'checking whether value ',val1,' is .false.'
       call print_result(message, lineno, filename, msg, success=.not.val1)
    end subroutine assertFalse


#   define subroutine_name  assertEqual_scalar_string
#   define typ              character(len=*)
#   define my_fmt           *
#   define compare          ==
#   define printline        'checking whether value "'//trim(val1)//'" is the same as "'//trim(val2)//'"' 
#   include "assert_simple.inc"

!--------------------------------------------------------------------

#   define subroutine_name  assertEqual_scalar_bool
#   define typ              logical
#   define my_fmt           *
#   define compare          .eqv.
#   define printline        'checking whether value ',val1,' is the same as ',val2 
#   include "assert_simple.inc"

!--------------------------------------------------------------------

#   define subroutine_name  assertlessThan_int
#   define typ              integer
#   define my_fmt           '(10(a,i0))'
#   define compare          <
#   define printline        'checking whether value ',val1,' is less than ',val2 
#   include "assert_simple.inc"

#   define subroutine_name  assertEqual_scalar_int
#   define typ              integer
#   define my_fmt           '(10(a,i0))'
#   define compare          ==
#   define printline        'checking whether value ',val1,' is the same as ',val2 
#   include "assert_simple.inc"

#   define subroutine_name  assertGreaterThan_int
#   define typ              integer
#   define my_fmt           '(10(a,i0))'
#   define compare          >
#   define printline        'checking whether value ',val1,' is greater than ',val2 
#   include "assert_simple.inc"

!--------------------------------------------------------------------

#   define subroutine_name  assertGreaterThan_float
#   define typ              real
#   define my_fmt           '(1p,10(a,g20.10))'
#   define compare          >
#   define printline        'checking whether value ',val1,' is greater than ',val2 
#   include "assert_simple.inc"

#   define subroutine_name  assertLessThan_float
#   define typ              real
#   define my_fmt           '(1p,10(a,g20.10))'
#   define compare          <
#   define printline        'checking whether value ',val1,' is less than ',val2 
#   include "assert_simple.inc"

#   define subroutine_name  assertEqual_scalar_notol
#   define typ              real
#   define my_fmt           '(1p,10(a,g20.10))'
#   define compare          ==
#   define printline        'checking whether value ',val1,' is equal to ',val2 
#   include "assert_simple.inc"

!--------------------------------------------------------------------
#   define subroutine_name  assertEqual_scalar_double_notol
#   define typ              double precision
#   define my_fmt           '(1p,10(a,g20.10))'
#   define compare          ==
#   define printline        'checking whether value ',val1,' is equal to ',val2 
#   include "assert_simple.inc"

!--------------------------------------------------------------------

    function compare_dimensions(shape1, shape2, message, lineno, filename) result(success)
    integer, intent(in) :: shape1(:), shape2(:)
    character(len=*), intent(in), optional :: message
    integer,          intent(in), optional :: lineno
    character(len=*), intent(in), optional :: filename
    logical :: success
    character(len=512) :: msg
    integer :: j
       if (size(shape1)==size(shape2)) then
          write(msg,'(20(a,i0))') 'checking whether val1(',shape1(1),(',',shape1(j),j=2,size(shape1)),') and '// &
                    'val2(',shape2(1),(',',shape2(j),j=2,size(shape2)),') have the same size'
       else
          print *,'compare_dimensions programming error: val1 has different number of dimensions than val2'
          stop 1
       end if
       success= all(shape1==shape2)
       call print_result(message, lineno, filename, msg, success)
    end function compare_dimensions

    subroutine assertEqual_array_string(val1, val2, message, lineno, filename) 
    character(len=*), intent(in) :: val1(:), val2(:)
    character(len=*), intent(in), optional :: message
    integer,          intent(in), optional :: lineno
    character(len=*), intent(in), optional :: filename
    character(len=512) :: msg
    character(len=1), parameter :: eol = new_line('a')
    integer :: i, locerr
    logical :: success
       success = compare_dimensions(shape(val1),shape(val2))
       if (.not. success .or. size(val1)==0) return

       locerr = 0
       do i = 1,size(val1)
          if (val1(i)/=val2(i)) locerr = i
       end do

       if (locerr==0) then
          msg = 'string-arrays are identical'
       else
          write(msg,'(20(a,i0))') &
               'val1(',locerr,')="'//trim(val1(locerr))//'" differs from '// &
               'val2(',locerr,')="'//trim(val2(locerr))//'"'
       end if
       call print_result(message, lineno, filename, msg, success=locerr==0)
    end subroutine assertEqual_array_string

#   define subroutine_name assertEqual_array_int16 
#   define typ integer(int16)
#   include "assert_array_1d.inc"

#   define subroutine_name assertEqual_array_int
#   define typ integer
#   include "assert_array_1d.inc"

#   define subroutine_name assertEqual_array_real_1d
#   define typ real
#   include "assert_array_1d.inc"

#   define subroutine_name assertEqual_array_real_2d
#   define typ real
#   include "assert_array_2d.inc"





    subroutine assertRelativelyEqual_dble_scalar(val1, val2, tol, message, lineno, filename) 
    double precision, intent(in) :: val1, val2
    real,             intent(in) :: tol
    character(len=*), intent(in), optional :: message
    integer,          intent(in), optional :: lineno
    character(len=*), intent(in), optional :: filename
    logical :: success
    character(len=512) :: msg
       write(msg,'(1p,10(a,g20.12))') 'checking whether ',abs(val1-val2),'= |',val1,'-',val2,'| <= ',tol,'*',abs(val1),'=',tol*abs(val1)
       if (val1==0 .and. val2==0) then
          success = .true.
       else if (val1==0) then
          success = .false.
       else
          success= abs(val1-val2)<=tol*abs(val1)
       end if

       call print_result(message, lineno, filename, msg, success=success)
    end subroutine assertRelativelyEqual_dble_scalar

    subroutine assertRelativelyEqual_scalar(val1, val2, tol, message, lineno, filename) 
    real,             intent(in) :: val1, val2, tol
    character(len=*), intent(in), optional :: message
    integer,          intent(in), optional :: lineno
    character(len=*), intent(in), optional :: filename
    logical :: success
    character(len=512) :: msg
       write(msg,'(1p,10(a,g20.12))') 'checking whether ',abs(val1-val2),'= |',val1,'-',val2,'| <= ',tol,'*',abs(val1),'=',tol*abs(val1)
       if (val1==0 .and. val2==0) then
          success = .true.
       else if (val1==0) then
          success = .false.
       else
          success= abs(val1-val2)<=tol*abs(val1)
       end if

       call print_result(message, lineno, filename, msg, success=success)
    end subroutine assertRelativelyEqual_scalar

    subroutine assertEqual_scalar_dble(val1, val2, tol, message, lineno, filename) 
    double precision, intent(in) :: val1, val2
    real,             intent(in) :: tol
    character(len=*), intent(in), optional :: message
    integer,          intent(in), optional :: lineno
    character(len=*), intent(in), optional :: filename
    character(len=512) :: msg
       write(msg,'(1p,10(a,g20.12))') 'checking whether ',abs(val1-val2),'= |',val1,'-',val2,'| <= ',tol
       call print_result(message, lineno, filename, msg, success= abs(val1-val2)<=tol)
    end subroutine assertEqual_scalar_dble

    subroutine assertEqual_scalar(val1, val2, tol, message, lineno, filename) 
    real,             intent(in) :: val1, val2, tol
    character(len=*), intent(in), optional :: message
    integer,          intent(in), optional :: lineno
    character(len=*), intent(in), optional :: filename
    character(len=512) :: msg
       write(msg,'(1p,10(a,g20.12))') 'checking whether ',abs(val1-val2),'= |',val1,'-',val2,'| <= ',tol
       call print_result(message, lineno, filename, msg, success= abs(val1-val2)<=tol)
    end subroutine assertEqual_scalar

    subroutine assertEqual_array(val1, val2, tol, message, lineno, filename) 
    real, intent(in) :: val1(:), val2(:), tol
    character(len=*), intent(in), optional :: message
    integer,          intent(in), optional :: lineno
    character(len=*), intent(in), optional :: filename
    real :: maxerr
    character(len=512) :: msg
    character(len=1), parameter :: eol = new_line('a')
    integer :: i, locerr
    logical :: success
       success = compare_dimensions(shape(val1),shape(val2))
       if (.not. success .or. size(val1)==0) return

       locerr = 1
       do i = 1,size(val1)
          if (.not. abs(val1(i)-val2(i)) < abs(val1(locerr)-val2(locerr))) then
             locerr = i
             if (isnan(val1(i)) .or. isnan(val2(i))) exit
          end if
       end do

       maxerr = val1(locerr)-val2(locerr)

       write(msg,'(2(a,i0),1p,10(a,g20.12))') &
                               'checking whether maximal grid error is larger than the tolerance'//eol// &
                               '     maxabs(val1-val2) = abs(val1(',locerr,')-val2(',locerr,'))'//eol// &
                               '                       = abs(',val1(locerr),'-',val2(locerr),')'//eol// &
                               '                       = abs(',maxerr,')'//eol//&
                               '                       <= tol = ',tol
       call print_result(message, lineno, filename, msg, success=abs(maxerr)<=tol)
    end subroutine assertEqual_array

    subroutine assertEqual_array_bool(val1, val2, message, lineno, filename) 
      logical, intent(in) :: val1(:), val2(:)
      character(len=*), intent(in), optional :: message
      integer,          intent(in), optional :: lineno
      character(len=*), intent(in), optional :: filename
      character(len=512) :: msg
      character(len=1), parameter :: eol = new_line('a')
      integer :: i, locerr
      logical :: success
         success = compare_dimensions(shape(val1),shape(val2))
         if (.not. success .or. size(val1)==0) return
  
         locerr = 1
         do i = 1,size(val1)
            if (val1(i) .neqv. val2(i)) then
               locerr = i
               success = .false.
               exit
            end if
         end do
  
         write(msg,'(2(a,i0),1p,10(a,g20.12))') &
                                 'checking whether maximal grid error is larger than the tolerance'//eol// &
                                 '            val1=val2  = val1(',locerr,')=val2(',locerr,'))'
         call print_result(message, lineno, filename, msg, success=success)
      end subroutine assertEqual_array_bool

    subroutine assertEqual_array_2d_notol(val1, val2, message, lineno, filename)
    real, intent(in) :: val1(:,:), val2(:,:)
    character(len=*), intent(in), optional :: message
    integer,          intent(in), optional :: lineno
    character(len=*), intent(in), optional :: filename
    real :: maxerr
    character(len=512) :: msg
    character(len=1), parameter :: eol = new_line('a')
    integer :: i, j, imax, jmax
    integer :: shape1(2)
    real, parameter :: tol = 0
    logical :: success
       success = compare_dimensions(shape(val1),shape(val2))
       if (.not. success .or. size(val1)==0) return


       shape1 = shape(val1)
       imax = 1
       jmax = 1
       do i = 1,shape1(1)
          do j = 1,shape1(2)
             if (.not. abs(val1(i,j)-val2(i,j)) < abs(val1(imax,jmax)-val2(imax,jmax))) then
                imax = i
                jmax = j
                if (isnan(val1(i,j)) .or. isnan(val2(i,j))) exit
             end if
          end do
       end do

       maxerr = val1(imax,jmax)-val2(imax,jmax)

       write(msg,'(4(a,i0),1p,10(a,g20.12))') &
                               'checking whether maximal grid error is larger than the tolerance'//eol// &
                               '     maxabs(val1-val2) = abs(val1(',imax,',',jmax,')-val2(',imax,',',jmax,'))'//eol// &
                               '                       = abs(',val1(imax,jmax),'-',val2(imax,jmax),')'//eol// &
                               '                       = abs(',maxerr,')'//eol//&
                               '                       <= tol = ',tol
       call print_result(message, lineno, filename, msg, success=abs(maxerr)<=tol)

    end subroutine assertEqual_array_2d_notol

    subroutine assertEqual_array_5d(val1, val2, tol, message, lineno, filename)
    real, intent(in) :: val1(:,:,:,:,:), val2(:,:,:,:,:), tol
    character(len=*), intent(in), optional :: message
    integer,          intent(in), optional :: lineno
    character(len=*), intent(in), optional :: filename
    real :: maxerr
    character(len=512) :: msg
    character(len=1), parameter :: eol = new_line('a')
    integer :: i, j, k, l, m, imax, jmax, kmax, lmax, mmax
    integer :: shape1(5)
    logical :: success
       success = compare_dimensions(shape(val1),shape(val2))
       if (.not. success .or. size(val1)==0) return

       shape1 = shape(val1)
       imax = 1
       jmax = 1
       lmax = 1
       kmax = 1
       mmax = 1
       do m = 1,shape1(5)
          do l = 1,shape1(4)
             do k = 1,shape1(3)
                do j = 1,shape1(2)
                   do i = 1,shape1(1)
                      if (.not. abs(val1(i,j,k,l,m)-val2(i,j,k,l,m)) < abs(val1(imax,jmax,kmax,lmax,mmax)-val2(imax,jmax,kmax,lmax,mmax))) then
                         imax = i
                         jmax = j
                         kmax = k
                         lmax = l
                         mmax = m
                         if (isnan(val1(i,j,k,l,m)) .or. isnan(val2(i,j,k,l,m))) exit
                      end if
                   end do
                end do
             end do
          end do
       end do
         
       maxerr = val1(imax,jmax,kmax,lmax,mmax)-val2(imax,jmax,kmax,lmax,mmax)

       write(msg,'(10(a,i0),1p,10(a,g20.12))') &
                               'checking whether'//eol// &
                               '     maxabs(val1-val2) = abs(val1(',imax,',',jmax,',',kmax,',',lmax,',',mmax,')-val2(',imax,',',jmax,',',kmax,',',lmax,',',mmax,'))'//eol// &
                               '                       = abs(',val1(imax,jmax,kmax,lmax,mmax),'-',val2(imax,jmax,kmax,lmax,mmax),')'//eol// &
                               '                       = abs(',maxerr,')'//eol//&
                               '                       <= tol = ',tol
       call print_result(message, lineno, filename, msg, success=abs(maxerr)<=tol)

    end subroutine assertEqual_array_5d

    subroutine assertEqual_array_4d(val1, val2, tol, message, lineno, filename)
    real, intent(in) :: val1(:,:,:,:), val2(:,:,:,:), tol
    character(len=*), intent(in), optional :: message
    integer,          intent(in), optional :: lineno
    character(len=*), intent(in), optional :: filename
    real :: maxerr
    character(len=512) :: msg
    character(len=1), parameter :: eol = new_line('a')
    integer :: i, j, k, l, imax, jmax, kmax, lmax
    integer :: shape1(4)
    logical :: success
       success = compare_dimensions(shape(val1),shape(val2))
       if (.not. success .or. size(val1)==0) return

       shape1 = shape(val1)
       imax = 1
       jmax = 1
       lmax = 1
       kmax = 1
       do l = 1,shape1(4)
          do k = 1,shape1(3)
             do j = 1,shape1(2)
                do i = 1,shape1(1)
                   if (.not. abs(val1(i,j,k,l)-val2(i,j,k,l)) < abs(val1(imax,jmax,kmax,lmax)-val2(imax,jmax,kmax,lmax))) then
                      imax = i
                      jmax = j
                      kmax = k
                      lmax = l
                      if (isnan(val1(i,j,k,l)) .or. isnan(val2(i,j,k,l))) exit
                   end if
                end do
             end do
          end do
       end do
         
       maxerr = val1(imax,jmax,kmax,lmax)-val2(imax,jmax,kmax,lmax)

       write(msg,'(8(a,i0),1p,10(a,g20.12))') &
                               'checking whether'//eol// &
                               '     maxabs(val1-val2) = abs(val1(',imax,',',jmax,',',kmax,',',lmax,')-val2(',imax,',',jmax,',',kmax,',',lmax,'))'//eol// &
                               '                       = abs(',val1(imax,jmax,kmax,lmax),'-',val2(imax,jmax,kmax,lmax),')'//eol// &
                               '                       = abs(',maxerr,')'//eol//&
                               '                       <= tol = ',tol
       call print_result(message, lineno, filename, msg, success=abs(maxerr)<=tol)

    end subroutine assertEqual_array_4d

    subroutine assertEqual_array_2d(val1, val2, tol, message, lineno, filename)
    real, intent(in) :: val1(:,:), val2(:,:), tol
    character(len=*), intent(in), optional :: message
    integer,          intent(in), optional :: lineno
    character(len=*), intent(in), optional :: filename
    real :: maxerr
    character(len=512) :: msg
    character(len=1), parameter :: eol = new_line('a')
    integer :: i, j, imax, jmax
    integer :: shape1(2)
    logical :: success
       success = compare_dimensions(shape(val1),shape(val2))
       if (.not. success .or. size(val1)==0) return

       shape1 = shape(val1)
       imax = 1
       jmax = 1
       do i = 1,shape1(1)
          do j = 1,shape1(2)
             if (.not. abs(val1(i,j)-val2(i,j)) < abs(val1(imax,jmax)-val2(imax,jmax))) then
                imax = i
                jmax = j
                if (isnan(val1(i,j)) .or. isnan(val2(i,j))) exit
             end if
          end do
       end do

       maxerr = val1(imax,jmax)-val2(imax,jmax)

       write(msg,'(4(a,i0),1p,10(a,g20.12))') &
                               'checking whether maximal grid error is larger than the tolerance'//eol// &
                               '     maxabs(val1-val2) = abs(val1(',imax,',',jmax,')-val2(',imax,',',jmax,'))'//eol// &
                               '                       = abs(',val1(imax,jmax),'-',val2(imax,jmax),')'//eol// &
                               '                       = abs(',maxerr,')'//eol//&
                               '                       <= tol = ',tol
       call print_result(message, lineno, filename, msg, success=abs(maxerr)<=tol)

    end subroutine assertEqual_array_2d

    subroutine assertEqual_array_int16_2d(val1, val2, message, lineno, filename)
    Use iso_fortran_env, only : int16
    integer(int16), intent(in) :: val1(:,:), val2(:,:)
    character(len=*), intent(in), optional :: message
    integer,          intent(in), optional :: lineno
    character(len=*), intent(in), optional :: filename
    integer :: maxerr
    character(len=512) :: msg
    character(len=1), parameter :: eol = new_line('a')
    integer :: i, j, imax, jmax
    integer :: shape1(2)
    logical :: success
       success = compare_dimensions(shape(val1),shape(val2))
       if (.not. success .or. size(val1)==0) return

       shape1 = shape(val1)
       imax = 1
       jmax = 1
       do i = 1,shape1(1)
          do j = 1,shape1(2)
             if (.not. abs(val1(i,j)-val2(i,j)) < abs(val1(imax,jmax)-val2(imax,jmax))) then
                imax = i
                jmax = j
             end if
          end do
       end do

       maxerr = val1(imax,jmax)-val2(imax,jmax)

       write(msg,'(20(a,i0))') &
                               'checking whether maximal grid error is larger than the tolerance'//eol// &
                               '     maxabs(val1-val2) = abs(val1(',imax,',',jmax,')-val2(',imax,',',jmax,'))'//eol// &
                               '                       = abs(',val1(imax,jmax),'-',val2(imax,jmax),')'//eol// &
                               '                       = abs(',maxerr,') = 0'
       call print_result(message, lineno, filename, msg, success=abs(maxerr)==0)

    end subroutine assertEqual_array_int16_2d
    
    subroutine assertEqual_array_3d(val1, val2, tol, message, lineno, filename)
    real, intent(in) :: val1(:,:,:), val2(:,:,:), tol
    character(len=*), intent(in), optional :: message
    integer,          intent(in), optional :: lineno
    character(len=*), intent(in), optional :: filename
    real :: maxerr
    character(len=512) :: msg
    character(len=1), parameter :: eol = new_line('a')
    integer :: i, j, k, imax, jmax, kmax
    integer :: shape1(3)
    logical :: success
       success = compare_dimensions(shape(val1),shape(val2))
       if (.not. success .or. size(val1)==0) return

       shape1 = shape(val1)
       imax = 1
       jmax = 1
       kmax = 1
       do i = 1,shape1(1)
          do j = 1,shape1(2)
             do k = 1,shape1(3)
                if (.not. abs(val1(i,j,k)-val2(i,j,k)) < abs(val1(imax,jmax,kmax)-val2(imax,jmax,kmax))) then
                   imax = i
                   jmax = j
                   kmax = k
                   if (isnan(val1(i,j,k)) .or. isnan(val2(i,j,k))) exit
                end if
             end do
          end do
       end do

       maxerr = val1(imax,jmax,kmax)-val2(imax,jmax,kmax)

       write(msg,'(6(a,i0),1p,10(a,g20.12))') &
                               'checking whether maximal grid error is larger than the tolerance'//eol// &
                               '     maxabs(val1-val2) = abs(val1(',imax,',',jmax,',',kmax,')-val2(',imax,',',jmax,',',kmax,'))'//eol// &
                               '                       = abs(',val1(imax,jmax,kmax),'-',val2(imax,jmax,kmax),')'//eol// &
                               '                       = abs(',maxerr,')'//eol//&
                               '                       <= tol = ',tol
       call print_result(message, lineno, filename, msg, success=abs(maxerr)<=tol)

    end subroutine assertEqual_array_3d

end module no_pfunit
