module m_log_call_ops_lt

! This module adds support for the following types to m_log_call (see documentation there)
! – TvChem, TDo_proc, TBuilding(:), TBuildingEffect

use m_log_call_ops_lib, only: tcall_pattern, init_call0 => init_call, print_unit, &
    add_derived_output, add_string_output, add_int2d_output, add_short1d_output, add_int1d_output, add_logical_output, add_logical1d_output, &
    add_int_output, add_double_output, add_real_output, add_string1d_output, add_real1d_output, add_real2d_output, add_real3d_output, add_real4d_output, add_real5d_output, &
    add_error_output, add_apsgrid_real_output, add_apsgrid_int_output, &
    add_derived_input, add_string_input, add_int2d_input, add_short1d_input, add_int1d_input, add_logical_input, &
    add_int_input, add_double_input, add_real_input, add_string1d_input, add_real1d_input, add_real2d_input, add_real3d_input, add_real4d_input, add_real5d_input, &
    add_error_input, add_apsgrid_real_input, add_apsgrid_int_input, &
    add_derived_inout, add_string_inout, add_int2d_inout, add_short1d_inout, add_int1d_inout, add_logical_inout, &
    add_int_inout, add_double_inout, add_real_inout, add_string1d_inout, add_real1d_inout, add_real2d_inout, add_real3d_inout, add_real4d_inout, add_real5d_inout, &
    add_error_inout, add_apsgrid_real_inout, add_apsgrid_int_inout, &
    get_error_derived, get_apsgrid_real_derived, get_apsgrid_int_derived, &
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
      module procedure add_tvchem_input
      module procedure add_tdo_proc_input
      module procedure add_building1d_input
      module procedure add_building_effect_input
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
      module procedure add_tvchem_output
      module procedure add_tdo_proc_output
      module procedure add_building1d_output
      module procedure add_building_effect_output
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
      module procedure add_tvchem_inout
      module procedure add_tdo_proc_inout
      module procedure add_building1d_inout
      module procedure add_building_effect_inout
   end interface add_inout
   interface get_derived
      module procedure get_error_derived
      module procedure get_apsgrid_real_derived
      module procedure get_apsgrid_int_derived
      module procedure get_tvchem_derived
      module procedure get_tdo_proc_derived
      module procedure get_building1d_derived
      module procedure get_building_effect_derived
   end interface get_derived
   
contains

    function init_call(routinenaam) result(call_pattern) 
    implicit none
       character(len=*), intent(in) :: routinenaam
       type(tcall_pattern) :: call_pattern
       call_pattern = init_call0(routinenaam)
       call_pattern%uses(1) = 'no_pfunit_ops_lt'
    end function init_call

    function get_tdo_proc_derived(do_proc) result(derived)
    use m_commonfile
    use m_error
    use m_ops_tdo_proc
    use m_log_call, only: tderived
    implicit none
       type(tdo_proc), intent(in) :: do_proc
       type(tderived)           :: derived
       derived%type_name = 'tdo_proc'

       allocate(derived%assert%strings(1))
       derived%assert%strings(1) = 'call assertEqual(${name}, ref_${name}, tol, "${name}", __LINE__, __FILE__)'

       allocate(derived%initialization%strings(4))
       if (do_proc%chem) then
          write(derived%initialization%strings(1),'(a,1p,e16.8)') '${name}%chem = .true.'
       else
          write(derived%initialization%strings(1),'(a,1p,e16.8)') '${name}%chem = .false.'
       end if
       if (do_proc%depl_drydep) then
          write(derived%initialization%strings(2),'(a,1p,e16.8)') '${name}%depl_drydep = .true.'
       else
          write(derived%initialization%strings(2),'(a,1p,e16.8)') '${name}%depl_drydep = .false.'
       end if
       if (do_proc%depl_wetdep) then
          write(derived%initialization%strings(3),'(a,1p,e16.8)') '${name}%depl_wetdep = .true.'
       else
          write(derived%initialization%strings(3),'(a,1p,e16.8)') '${name}%depl_wetdep = .false.'
       end if
       if (do_proc%grad_drydep) then
          write(derived%initialization%strings(4),'(a,1p,e16.8)') '${name}%grad_drydep = .true.'
       else
          write(derived%initialization%strings(4),'(a,1p,e16.8)') '${name}%grad_drydep = .false.'
       end if

       allocate(derived%uses%strings(1))
       derived%uses%strings(1) = 'm_ops_tdo_proc, only: tdo_proc'
    end function get_tdo_proc_derived

    function get_building1d_derived(building) result(derived)
      use m_ops_building
      use m_log_call, only: tderived
      implicit none
         type(TBuilding),  intent(in) :: building(:)
         type(tderived)               :: derived
         derived%type_name = 'TBuilding'
         allocate(derived%assert%strings(1))
         derived%assert%strings(1) = 'call assertEqual(${name}, ref_${name}, tol, "${name}", __LINE__, __FILE__)'

         allocate(derived%initialization%strings(8))
         write(derived%initialization%strings(1),'(a,1p,e16.8)') '${name}(1)%length = ',building(1)%length
         write(derived%initialization%strings(2),'(a,1p,e16.8)') '${name}(1)%width = ',building(1)%width
         write(derived%initialization%strings(3),'(a,1p,e16.8)') '${name}(1)%height = ',building(1)%height
         write(derived%initialization%strings(4),'(a,1p,e16.8)') '${name}(1)%orientation = ',building(1)%orientation
         write(derived%initialization%strings(5),'(a,1p,e16.8)') '${name}(1)%x = ',building(1)%x
         write(derived%initialization%strings(6),'(a,1p,e16.8)') '${name}(1)%y = ',building(1)%y
         if (allocated(building(1)%buildingFactFunction)) then
            write(derived%initialization%strings(7),'(a,1p,*(e16.8,:,","))') '${name}(1)%buildingFactFunction = ',building(1)%buildingFactFunction
         endif
         write(derived%initialization%strings(8),'(a,1p,i)') '${name}(1)%type = ',building(1)%type
         
         
         allocate(derived%uses%strings(1))
         derived%uses%strings(1) = 'm_ops_building, only: TBuilding'
      end function get_building1d_derived

     function get_building_effect_derived(building_effect) result(derived)
      use m_ops_building
      use m_log_call, only: tderived
      implicit none
         type(TBuildingEffect),     intent(in) :: building_effect
         type(tderived)               :: derived
         integer :: nParam, j, n
         derived%type_name = 'TBuildingEffect'
         allocate(derived%assert%strings(1))
         derived%assert%strings(1) = 'call assertEqual(${name}, ref_${name}, tol, "${name}", __LINE__, __FILE__)'
   
         allocate(derived%initialization%strings(8))
         nParam = building_effect%nParam
         write(derived%initialization%strings(1),'(a,1p,i)') '${name}%nParam = ',nParam
         n = size(building_effect%classdefinitionArray, 1) 
         write(derived%initialization%strings(2),'(a,1p,100(e16.8,a))') &
                 '${name}%classdefinitionArray = [',building_effect%classdefinitionArray(1),(',',building_effect%classdefinitionArray(j),j=2,n),']'
         write(derived%initialization%strings(3),'(a,100(i0,a))') &
                 '${name}%nClass = [',building_effect%nClass(1),(',',building_effect%nClass(j),j=2,nParam),']'
         write(derived%initialization%strings(4),'(a,1p,100(e16.8,a))') &
                 '${name}%minClass = [',building_effect%minClass(1),(',',building_effect%minClass(1),j=2,nParam),']'
         write(derived%initialization%strings(5),'(a,1p,100(e16.8,a))') &
                 '${name}%maxClass = [',building_effect%maxClass(1),(',',building_effect%maxClass(1),j=2,nParam),']'
         n = size(building_effect%buildingFactArray , 1) 
         write(derived%initialization%strings(6),'(a,1p,100(e16.8,a))') &
                 '${name}%buildingFactArray = [',building_effect%buildingFactArray(1),(',',building_effect%buildingFactArray(j),j=2,n),']'
         n = size(building_effect%buildingFactAngleSRxaxis, 1) 
         write(derived%initialization%strings(7),'(a,1p,100(e16.8,a))') &
                 '${name}%buildingFactAngleSRxaxis = [',building_effect%buildingFactAngleSRxaxis(1),(',',building_effect%buildingFactAngleSRxaxis(j),j=2,n),']'
         n = size(building_effect%buildingFactDistances, 1) 
         write(derived%initialization%strings(8),'(a,1p,100(e16.8,a))') &
               '${name}%buildingFactDistances = [',building_effect%buildingFactDistances(1),(',',building_effect%buildingFactDistances(j),j=2,n),']'
         
         allocate(derived%uses%strings(1))
         derived%uses%strings(1) = 'm_ops_building, only: TBuildingEffect'
      end function get_building_effect_derived

    function get_tvchem_derived(chem) result(derived)
    use m_commonfile
    use m_error
#   ifndef __GFORTRAN__
       use ifport, only: getpid
#   endif
    use m_ops_vchem
    use m_log_call, only: tderived
    implicit none
       type(tvChem), intent(in) :: chem
       type(tderived)           :: derived
       integer, save :: idx = 0
       character(len=300) :: fname1, fname2
       type(TError) :: error
       derived%type_name = 'tvChem'
       idx = idx + 1
       write(fname1, '(a,i0,a,i0,a)') 'mass_prec_grid_',getpid(),'_',idx,'.real'
       call remember_generated_file(fname1)

       call WriteAps( fname1, fname1, chem%mass_prec_grid, error, zero_dxy_allowed = .true.)
       if (error%haserror) then
           call write_error(IOB_STDOUT, error)
           stop 1
       end if

       write(fname2, '(a,i0,a,i0,a)') 'mass_conv_dtfac_grid_',getpid(),'_',idx,'.real'
       call remember_generated_file(fname2)

       call WriteAps( fname2, fname2, chem%mass_conv_dtfac_grid, error, zero_dxy_allowed = .true.)
       if (error%haserror) then
           call write_error(IOB_STDOUT, error)
           stop 1
       end if

       allocate(derived%assert%strings(1))
       derived%assert%strings(1) = 'call assertEqual(${name}, ref_${name}, tol, "${name}", __LINE__, __FILE__)'

       allocate(derived%initialization%strings(2))
       derived%initialization%strings(1) = 'call InitAps("'//trim(fname1)//'", "Init mass_prec_grid", ${name}%mass_prec_grid, __LINE__, __FILE__)'
       derived%initialization%strings(2) = 'call InitAps("'//trim(fname2)//'", "Init mass_conv_dtfac_grid", ${name}%mass_conv_dtfac_grid, __LINE__, __FILE__)'

       allocate(derived%uses%strings(1))
       derived%uses%strings(1) = 'm_ops_vchem, only: tvChem'
    end function get_tvchem_derived


    subroutine add_tvchem_inout(call_pattern, name, value)
    use m_log_call, only: add_arg
    use m_log_call, only: intent_INOUT
    use m_ops_vchem
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       type(tvChem),        intent(in)    :: value
       call add_arg(call_pattern, name, intent_INOUT, get_derived(value))
    end subroutine add_tvchem_inout
    subroutine add_tvchem_output(call_pattern, name, value)
    use m_log_call, only: add_arg
    use m_log_call, only: intent_OUT
    use m_ops_vchem
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       type(tvChem),        intent(in)    :: value
       call add_arg(call_pattern, name, intent_OUT, get_derived(value))
    end subroutine add_tvchem_output
    subroutine add_tvchem_input(call_pattern, name, value)
    use m_log_call, only: add_arg
    use m_log_call, only: intent_IN
    use m_ops_vchem
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       type(tvChem),        intent(in)    :: value
       call add_arg(call_pattern, name, intent_IN, get_derived(value))
    end subroutine add_tvchem_input

    subroutine add_tdo_proc_inout(call_pattern, name, value)
    use m_log_call, only: add_arg
    use m_log_call, only: intent_INOUT
    use m_ops_tdo_proc
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       type(tdo_proc),        intent(in)    :: value
       call add_arg(call_pattern, name, intent_INOUT, get_derived(value))
    end subroutine add_tdo_proc_inout
    subroutine add_tdo_proc_output(call_pattern, name, value)
    use m_log_call, only: add_arg
    use m_log_call, only: intent_OUT
    use m_ops_tdo_proc
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       type(tdo_proc),        intent(in)    :: value
       call add_arg(call_pattern, name, intent_OUT, get_derived(value))
    end subroutine add_tdo_proc_output
    subroutine add_tdo_proc_input(call_pattern, name, value)
    use m_log_call, only: add_arg
    use m_log_call, only: intent_IN
    use m_ops_tdo_proc
       type(tcall_pattern), intent(inout) :: call_pattern
       character(len=*),    intent(in)    :: name
       type(Tdo_proc),        intent(in)    :: value
       call add_arg(call_pattern, name, intent_IN, get_derived(value))
    end subroutine add_tdo_proc_input

    subroutine add_building1d_inout(call_pattern, name, value)
      use m_log_call, only: add_arg
      use m_log_call, only: intent_INOUT
      use m_ops_building
         type(tcall_pattern), intent(inout) :: call_pattern
         character(len=*),    intent(in)    :: name
         type(TBuilding),     intent(in)    :: value(:)
         call add_arg(call_pattern, name, intent_INOUT, get_derived(value))
      end subroutine add_building1d_inout
      subroutine add_building1d_output(call_pattern, name, value)
      use m_log_call, only: add_arg
      use m_log_call, only: intent_OUT
      use m_ops_building
         type(tcall_pattern), intent(inout) :: call_pattern
         character(len=*),    intent(in)    :: name
         type(TBuilding),     intent(in)    :: value(:)
         call add_arg(call_pattern, name, intent_OUT, get_derived(value))
      end subroutine add_building1d_output
      subroutine add_building1d_input(call_pattern, name, value)
      use m_log_call, only: add_arg
      use m_log_call, only: intent_IN
      use m_ops_building
         type(tcall_pattern), intent(inout) :: call_pattern
         character(len=*),    intent(in)    :: name
         type(TBuilding),     intent(in)    :: value(:)
         call add_arg(call_pattern, name, intent_IN, get_derived(value))
      end subroutine add_building1d_input

      subroutine add_building_effect_inout(call_pattern, name, value)
         use m_log_call, only: add_arg
         use m_log_call, only: intent_INOUT
         use m_ops_building
            type(tcall_pattern),   intent(inout) :: call_pattern
            character(len=*),      intent(in)    :: name
            type(TBuildingEffect), intent(in)    :: value
            call add_arg(call_pattern, name, intent_INOUT, get_derived(value))
         end subroutine add_building_effect_inout
         subroutine add_building_effect_output(call_pattern, name, value)
         use m_log_call, only: add_arg
         use m_log_call, only: intent_OUT
         use m_ops_building
            type(tcall_pattern),   intent(inout) :: call_pattern
            character(len=*),      intent(in)    :: name
            type(TBuildingEffect), intent(in)    :: value
            call add_arg(call_pattern, name, intent_OUT, get_derived(value))
         end subroutine add_building_effect_output
         subroutine add_building_effect_input(call_pattern, name, value)
         use m_log_call, only: add_arg
         use m_log_call, only: intent_IN
         use m_ops_building
            type(tcall_pattern),   intent(inout) :: call_pattern
            character(len=*),      intent(in)    :: name
            type(TBuildingEffect), intent(in)    :: value
            call add_arg(call_pattern, name, intent_IN, get_derived(value))
         end subroutine add_building_effect_input

end module m_log_call_ops_lt


