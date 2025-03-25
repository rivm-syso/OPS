module m_test_log_call
implicit none
contains 
    subroutine replace_pid(fname)
#   ifndef __GFORTRAN__
       use ifport, only: getpid
#   endif
    use no_pfunit
    use m_log_call, only: replace_string
    Use, intrinsic :: iso_fortran_env, Only : iostat_end
       character(len=*), intent(in) :: fname
       character(len=20) :: pidstr
       integer :: fid_pid, fid, iline, iostat
       character(len=200) :: line, msg
       open(newunit=fid_pid, file = trim(fname)//'.pid', action = 'read', iostat=iostat)
       call AssertEqual(iostat,0,'Opening file "'//trim(fname)//'.pid"', __LINE__, __FILE__)
       open(newunit=fid, file = trim(fname), action = 'write', iostat=iostat)
       call AssertEqual(iostat,0,'Opening file "'//trim(fname)//'"', __LINE__, __FILE__)

       iline = 0
       write(pidstr,'(a,i0,a)') '_',getpid(),'_'
       do while(.true.) 
          iline = iline + 1
          write(msg,'(10(a,i0))') 'line ',iline
          read(fid_pid, '(a)', iostat=iostat) line
          if (iostat == iostat_end) exit

          line = replace_string(line, trim(pidstr), '_pid_')
          write(fid, '(a)', iostat=iostat) trim(line)
          call AssertEqual(iostat,0,'error writing '//trim(msg), __LINE__, __FILE__)
       end do
       close(fid)
       close(fid_pid)
    end subroutine replace_pid

    subroutine init_iapsgrid(m,n,dx, dy, val, imasker)
    use m_aps
    implicit none
       integer, intent(in) :: m,n
       real, intent(in) :: dx, dy
       integer, intent(in) :: val
       type(TApsGridInt), intent(out) :: imasker

       imasker%gridheader%grixl = dx
       imasker%gridheader%griyl = dy
       imasker%gridheader%nrrow = m
       imasker%gridheader%nrcol = n
       ALLOCATE(imasker%value(m,n,1))
       imasker%value = val
    end subroutine init_iapsgrid

    subroutine init_apsgrid(m,n,dx, dy, val,avg, rmasker)
    use m_aps
    implicit none
       integer, intent(in) :: m,n
       real, intent(in) :: dx, dy, val, avg
       type(TApsGridReal), intent(out) :: rmasker

       rmasker%gridheader%grixl = dx
       rmasker%gridheader%griyl = dy
       rmasker%gridheader%nrrow = m
       rmasker%gridheader%nrcol = n
       ALLOCATE(rmasker%value(m,n,1))
       ALLOCATE(rmasker%average(1))
       rmasker%value = val
       rmasker%average = avg
    end subroutine init_apsgrid

    subroutine test_log_call3()
    use no_pfunit
    Use, intrinsic :: iso_fortran_env, Only : iostat_end
    use m_aps
    use m_log_call_ops_lib
       character(len=*), parameter :: routinenaam = 'test_log_call'
       character(len=*), parameter :: fname = 'generated_derived_code.f90'
       type(tcall_pattern) :: call_pattern
       integer :: iostat, old_print_unit, fid, fid_ref, iostat_ref, iline
       character(len=200) :: line, line_ref, msg
       type(TApsGridInt) :: imasker
       type(TApsGridReal) :: rmasker
       TYPE (TError)      :: error

       call forget_generated_files()
       call SetError('need positive number of rows and columns in APS header', error)

       call init_apsgrid(m=10, n=10, dx = 1e10, dy=1e10, val=7.25, avg = 4.125, rmasker=rmasker)
       call init_iapsgrid(m=10, n=10, dx = 1e10, dy=1e10, val=7, imasker=imasker)

       call_pattern = init_call(routinenaam)
       call add_input( call_pattern, "input_apsgrid_real", rmasker)
       call add_output( call_pattern, "output_apsgrid_real", rmasker)
       call add_inout( call_pattern, "inout_apsgrid_real", rmasker)

       call add_input( call_pattern, "input_apsgrid_real", imasker)
       call add_output( call_pattern, "output_apsgrid_real", imasker)
       call add_inout( call_pattern, "inout_apsgrid_real", imasker)

       call add_input( call_pattern, "input_error_real", error)
       call add_output( call_pattern, "output_error_real", error)
       call add_inout( call_pattern, "inout_error_real", error)

       call print_test(call_pattern, fname//'.pid')
        
       call replace_pid(fname)
       call remember_generated_file(fname)
       call CompareTextFiles(fname, './level_1/resources/'//fname)
       call remove_generated_files()
    end subroutine test_log_call3


    subroutine test_log_call2()
    use no_pfunit
    Use, intrinsic :: iso_fortran_env, Only : iostat_end
    use m_log_call
       character(len=*), parameter :: routinenaam = 'test_log_call'
       character(len=*), parameter :: fname = 'generated_long_code.f90'
       type(tcall_pattern) :: call_pattern
       integer, parameter :: intval1(1010) = 7, intval2(101,10) = 8
       real,    parameter :: realval1(1008) =1.0, realval2(11,100) = 2.0, &
                             realval3(20,20,3) = 2.625, realval4(5,20,3,4) =1.25, realval5(1,20,3,4,5) = 2.5
       logical, parameter :: boolval0 = .false., boolval1(3) = .true.
       character(len=7), parameter :: stringval1(101) = 'OPS-lt'
       integer :: iostat, old_print_unit, fid, fid_ref, iostat_ref, iline
       character(len=200) :: line, line_ref, msg
       call forget_generated_files()

       call_pattern = init_call(routinenaam)
       call add_input( call_pattern, "input_int1", intval1)
       call add_input( call_pattern, "input_int2", intval2)

       call add_output( call_pattern, "output_int1", intval1)
       call add_output( call_pattern, "output_int2", intval2)

       call add_inout( call_pattern, "inout_int1", intval1)
       call add_inout( call_pattern, "inout_int2", intval2)

       call add_inout( call_pattern, "inout_real1", realval1)
       call add_inout( call_pattern, "inout_real2", realval2)
       call add_inout( call_pattern, "inout_real3", realval3)
       call add_inout( call_pattern, "inout_real4", realval4)
       call add_inout( call_pattern, "inout_real5", realval5)

       call add_output( call_pattern, "output_real1", realval1)
       call add_output( call_pattern, "output_real2", realval2)
       call add_output( call_pattern, "output_real3", realval3)
       call add_output( call_pattern, "output_real4", realval4)
       call add_output( call_pattern, "output_real5", realval5)

       call add_input( call_pattern, "input_real1", realval1)
       call add_input( call_pattern, "input_real2", realval2)
       call add_input( call_pattern, "input_real3", realval3)
       call add_input( call_pattern, "input_real4", realval4)
       call add_input( call_pattern, "input_real5", realval5)

       call add_output( call_pattern, "out_bool1", boolval1)

       call add_input( call_pattern, "in_bool1", boolval1)

       call add_inout( call_pattern, "inout_bool1", boolval1)

       call add_output( call_pattern, "out_string1", stringval1)

       call add_input( call_pattern, "in_string1", stringval1)

       call add_inout( call_pattern, "inout_string1", stringval1)

       call print_test(call_pattern, fname//'.pid')

       call replace_pid(fname)
       call remember_generated_file(fname)
       call CompareTextFiles(fname, './level_1/resources/'//fname)
       call remove_generated_files()
    end subroutine test_log_call2

    subroutine test_log_call1()
    use no_pfunit
    Use, intrinsic :: iso_fortran_env, Only : iostat_end
    use m_log_call
       character(len=*), parameter :: routinenaam = 'test_log_call'
       character(len=*), parameter :: fname = 'generated_code.f90'
       type(tcall_pattern) :: call_pattern
       integer, parameter :: intval0 = 6, intval1(10) = 7, intval2(2,3) = 8
       real,    parameter :: realval0 = 2.0, realval1(8) =1.0, realval2(3,2) = 2.0, &
                             realval3(2,2,3) = 2.625, realval4(1,2,1,1) =1.25, realval5(1,2,1,2,1) = 2.5
       double precision, parameter :: doubleval0 = 5.25
       integer*2, parameter :: shortval0(2) = 5.25
       logical, parameter :: boolval0 = .false., boolval1(3) = .true.
       character(len=7), parameter :: stringval0 = 'OPS-lib', stringval1(3) = 'OPS-lt'
       integer :: iostat, old_print_unit, fid, fid_ref, iostat_ref, iline
       character(len=200) :: line, line_ref, msg
       call forget_generated_files()

       call_pattern = init_call(routinenaam)
       call add_input( call_pattern, "input_int0", intval0)
       call add_input( call_pattern, "input_int1", intval1)
       call add_input( call_pattern, "input_int2", intval2)

       call add_output( call_pattern, "output_int0", intval0)
       call add_output( call_pattern, "output_int1", intval1)
       call add_output( call_pattern, "output_int2", intval2)

       call add_inout( call_pattern, "inout_int0", intval0)
       call add_inout( call_pattern, "inout_int1", intval1)
       call add_inout( call_pattern, "inout_int2", intval2)

       call add_inout( call_pattern, "inout_real0", realval0)
       call add_inout( call_pattern, "inout_real1", realval1)
       call add_inout( call_pattern, "inout_real2", realval2)
       call add_inout( call_pattern, "inout_real3", realval3)
       call add_inout( call_pattern, "inout_real4", realval4)
       call add_inout( call_pattern, "inout_real5", realval5)

       call add_output( call_pattern, "output_real0", realval0)
       call add_output( call_pattern, "output_real1", realval1)
       call add_output( call_pattern, "output_real2", realval2)
       call add_output( call_pattern, "output_real3", realval3)
       call add_output( call_pattern, "output_real4", realval4)
       call add_output( call_pattern, "output_real5", realval5)

       call add_input( call_pattern, "input_real0", realval0)
       call add_input( call_pattern, "input_real1", realval1)
       call add_input( call_pattern, "input_real2", realval2)
       call add_input( call_pattern, "input_real3", realval3)
       call add_input( call_pattern, "input_real4", realval4)
       call add_input( call_pattern, "input_real5", realval5)

       call add_output( call_pattern, "out_bool0", boolval0)
       call add_output( call_pattern, "out_bool1", boolval1)

       call add_input( call_pattern, "in_bool0", boolval0)
       call add_input( call_pattern, "in_bool1", boolval1)

       call add_inout( call_pattern, "inout_bool0", boolval0)
       call add_inout( call_pattern, "inout_bool1", boolval1)

       call add_output( call_pattern, "out_string0", stringval0)
       call add_output( call_pattern, "out_string1", stringval1)

       call add_input( call_pattern, "in_string0", stringval0)
       call add_input( call_pattern, "in_string1", stringval1)

       call add_inout( call_pattern, "inout_string0", stringval0)
       call add_inout( call_pattern, "inout_string1", stringval1)

       call add_output( call_pattern, "out_double0", doubleval0)
       call add_input( call_pattern, "in_double0", doubleval0)
       call add_inout( call_pattern, "inout_double0", doubleval0)

       call add_output( call_pattern, "out_short0", shortval0)
       call add_input( call_pattern, "in_short0", shortval0)
       call add_inout( call_pattern, "inout_short0", shortval0)

       call print_test(call_pattern, fname)
       call CompareTextFiles(fname, './level_1/resources/'//fname)
       call remove_generated_files()
    end subroutine test_log_call1
    
end module m_test_log_call


program test_log_call
use m_test_log_call
use no_pfunit
   ! Create test output with lots of variables
   call test_log_call1()

   ! Create test output with lots of variables that do not fit in the code
   ! Their content will be written to separate files.
   call test_log_call2()

   ! Create test output with variables of derived type
   call test_log_call3()

   call conclusion()
end program test_log_call
