module m_log_call_ops_lib

! This module adds support for the following types to m_log_call (see documentation there)
! – TApsGridReal, TApsGridInt, Terror

use m_log_call, only: tcall_pattern, init_call0 => init_call, print_unit, replace_string, &
    add_derived_output, add_string_output, add_int2d_output, add_short1d_output, add_int1d_output, add_logical_output, &
    add_logical1d_output, add_int_output, add_double_output, add_real_output, add_string1d_output, add_real1d_output, add_real2d_output, add_real3d_output, add_real4d_output, add_real5d_output,&
    add_derived_input, add_string_input, add_int2d_input, add_short1d_input, add_int1d_input, add_logical_input, &
    add_int_input, add_double_input, add_real_input, add_string1d_input, add_real1d_input, add_real2d_input, add_real3d_input, add_real4d_input, add_real5d_input,&
    add_derived_inout, add_string_inout, add_int2d_inout, add_short1d_inout, add_int1d_inout, add_logical_inout, &
    add_int_inout, add_double_inout, add_real_inout, add_string1d_inout, add_real1d_inout, add_real2d_inout, add_real3d_inout, add_real4d_inout, add_real5d_inout, &
    forget_generated_files, remember_generated_file, open_generated_file, remove_generated_files, &
    print_test
implicit none
   interface add_input
      module procedure add_derived_input
      module procedure add_string_input
      module procedure add_int2d_input
      module procedure add_short1d_input
      module procedure add_int1d_input
      module procedure add_logical_input
      module procedure add_int_input
      module procedure add_double_input
      module procedure add_real_input
      module procedure add_string1d_input
      module procedure add_real1d_input
      module procedure add_real2d_input
      module procedure add_real3d_input
      module procedure add_real4d_input
      module procedure add_real5d_input
      module procedure add_error_input
      module procedure add_apsgrid_real_input
      module procedure add_apsgrid_int_input
   end interface add_input
   interface add_output
      module procedure add_derived_output
      module procedure add_string_output
      module procedure add_int2d_output
      module procedure add_short1d_output
      module procedure add_int1d_output
      module procedure add_logical_output
      module procedure add_logical1d_output
      module procedure add_int_output
      module procedure add_double_output
      module procedure add_real_output
      module procedure add_string1d_output
      module procedure add_real1d_output
      module procedure add_real2d_output
      module procedure add_real3d_output
      module procedure add_real4d_output
      module procedure add_real5d_output
      module procedure add_error_output
      module procedure add_apsgrid_real_output
      module procedure add_apsgrid_int_output
   end interface add_output
   interface add_inout
      module procedure add_derived_inout
      module procedure add_string_inout
      module procedure add_int2d_inout
      module procedure add_short1d_inout
      module procedure add_int1d_inout
      module procedure add_logical_inout
      module procedure add_int_inout
      module procedure add_double_inout
      module procedure add_real_inout
      module procedure add_string1d_inout
      module procedure add_real1d_inout
      module procedure add_real2d_inout
      module procedure add_real3d_inout
      module procedure add_real4d_inout
      module procedure add_real5d_inout
      module procedure add_error_inout
      module procedure add_apsgrid_real_inout
      module procedure add_apsgrid_int_inout
   end interface add_inout
   interface get_derived
      module procedure get_error_derived
      module procedure get_apsgrid_real_derived
      module procedure get_apsgrid_int_derived
   end interface get_derived
   
contains

    function init_call(routinenaam) result(call_pattern) 
    implicit none
       character(len=*), intent(in) :: routinenaam
       type(tcall_pattern) :: call_pattern
       call_pattern = init_call0(routinenaam)
       call_pattern%uses(1) = 'no_pfunit_ops_lib'
    end function init_call

    function get_error_derived(error, name) result(derived)
    use m_error
    use m_log_call, only: tderived
    implicit none
       type(TError),     intent(in) :: error
       character(len=*), intent(in) :: name
       type(tderived)               :: derived
       derived%type_name = 'TError'
       allocate(derived%assert%strings(1))
       derived%assert%strings(1) = 'call assertEqual(${name}, ref_${name}, "'//trim(name)//'", __LINE__, __FILE__)'

       allocate(derived%initialization%strings(2))
       if (error%haserror) then
           derived%initialization%strings(1) = '${name}%haserror = .true.'
       else
           derived%initialization%strings(1) = '${name}%haserror = .false.'
       endif
       derived%initialization%strings(2) = '${name}%message = "'//trim(error%message)//'"'

       allocate(derived%uses%strings(1))
       derived%uses%strings(1) = 'm_error, only: TError'
    end function get_error_derived

    function get_apsgrid_int_derived(grid) result(derived)
#   ifndef __GFORTRAN__
       use ifport, only: getpid
#   endif
    use m_commonfile, only: IOB_STDOUT
    use m_error
    use m_aps
    use m_log_call, only: tderived
    implicit none
       type(TApsGridInt), intent(in) :: grid
       type(tderived)                :: derived

       integer, save :: idx = 0
       character(len=300) :: fname
       type(TError) :: error

       derived%type_name = 'TApsGridInt'

       idx = idx + 1
       write(fname, '(a,i0,a,i0,a)') 'apsgrid_',getpid(),'_',idx,'.int'
       call remember_generated_file(fname)

       call WriteAps( fname, fname, grid, error, zero_dxy_allowed = .true.)
       if (error%haserror) then
           call write_error(IOB_STDOUT, error)
           stop 1
       end if

       allocate(derived%assert%strings(1))
       derived%assert%strings(1) = 'call assertEqual(${name}, ref_${name}, tol, "${name}", __LINE__, __FILE__)'

       allocate(derived%initialization%strings(1))
       derived%initialization%strings(1) = 'call InitAps("'//trim(fname)//'", "InitApsInt", ${name}, __LINE__, __FILE__)'

       allocate(derived%uses%strings(1))
       derived%uses%strings(1) = 'm_aps, only: TApsGridInt'
    end function get_apsgrid_int_derived

    function get_apsgrid_real_derived(grid) result(derived)
    use m_commonfile, only: IOB_STDOUT
#   ifndef __GFORTRAN__
       use ifport, only: getpid
#   endif
    use m_error
    use m_aps
    use m_log_call, only: tderived
    implicit none
       type(TApsGridReal), intent(in) :: grid
       type(tderived)                 :: derived

       integer, save :: idx = 0
       character(len=300) :: fname
       type(TError) :: error

       derived%type_name = 'TApsGridReal'

       idx = idx + 1
       write(fname, '(a,i0,a,i0,a)') 'apsgrid_',getpid(),'_',idx,'.real'
       call remember_generated_file(fname)

       call WriteAps( fname, fname, grid, error, zero_dxy_allowed = .true.)
       if (error%haserror) then
           call write_error(IOB_STDOUT, error)
           stop 1
       end if

       allocate(derived%assert%strings(1))
       derived%assert%strings(1) = 'call assertEqual(${name}, ref_${name}, tol, "${name}", __LINE__, __FILE__)'

       allocate(derived%initialization%strings(1))
       derived%initialization%strings(1) = 'call InitAps("'//trim(fname)//'", "InitApsReal", ${name}, __LINE__, __FILE__)'

       allocate(derived%uses%strings(1))
       derived%uses%strings(1) = 'm_aps, only: TApsGridReal'
    end function get_apsgrid_real_derived

    subroutine add_error_inout(call_pattern, name, value)
    use m_log_call, only: add_arg
    use m_log_call, only: intent_INOUT
    use m_error
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       type(TError),        intent(in)    :: value
       call add_arg(call_pattern, name, intent_INOUT, get_derived(value, call_pattern%routinenaam) )
    end subroutine add_error_inout
    subroutine add_apsgrid_real_inout(call_pattern, name, value)
    use m_log_call, only: add_arg
    use m_log_call, only: intent_INOUT
    use m_aps
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       type(TApsGridReal),   intent(in)    :: value
       call add_arg(call_pattern, name, intent_INOUT, get_derived(value))
    end subroutine add_apsgrid_real_inout
    subroutine add_apsgrid_int_inout(call_pattern, name, value)
    use m_log_call, only: add_arg
    use m_log_call, only: intent_INOUT
    use m_aps
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       type(TApsGridInt),   intent(in)    :: value
       call add_arg(call_pattern, name, intent_INOUT, get_derived(value))
    end subroutine add_apsgrid_int_inout

    subroutine add_error_output(call_pattern, name, value)
    use m_log_call, only: add_arg
    use m_log_call, only: intent_OUT
    use m_error
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       type(TError),        intent(in)    :: value
       call add_arg(call_pattern, name, intent_OUT, get_derived(value, call_pattern%routinenaam) )
    end subroutine add_error_output
    subroutine add_apsgrid_real_output(call_pattern, name, value)
    use m_log_call, only: add_arg
    use m_log_call, only: intent_OUT
    use m_aps
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       type(TApsGridReal),   intent(in)    :: value
       call add_arg(call_pattern, name, intent_OUT, get_derived(value))
    end subroutine add_apsgrid_real_output
    subroutine add_apsgrid_int_output(call_pattern, name, value)
    use m_log_call, only: add_arg
    use m_log_call, only: intent_OUT
    use m_aps
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       type(TApsGridInt),   intent(in)    :: value
       call add_arg(call_pattern, name, intent_OUT, get_derived(value))
    end subroutine add_apsgrid_int_output

    subroutine add_error_input(call_pattern, name, value)
    use m_log_call, only: add_arg
    use m_log_call, only: intent_IN
    use m_error
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       type(TError),        intent(in)    :: value
       call add_arg(call_pattern, name, intent_IN, get_derived(value, call_pattern%routinenaam) )
    end subroutine add_error_input
    subroutine add_apsgrid_real_input(call_pattern, name, value)
    use m_log_call, only: add_arg
    use m_log_call, only: intent_IN
    use m_aps
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       type(TApsGridReal),   intent(in)    :: value
       call add_arg(call_pattern, name, intent_IN, get_derived(value))
    end subroutine add_apsgrid_real_input
    subroutine add_apsgrid_int_input(call_pattern, name, value)
    use m_log_call, only: add_arg
    use m_aps
    use m_log_call, only: intent_IN
    use m_error
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       type(TApsGridInt),   intent(in)    :: value
       call add_arg(call_pattern, name, intent_IN, get_derived(value))
    end subroutine add_apsgrid_int_input

end module m_log_call_ops_lib

