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
    use m_log_call_ops_lt
    use m_ops_vchem
    use m_ops_tdo_proc
    use m_ops_building
       character(len=*), parameter :: routinenaam = 'test_log_call'
       character(len=*), parameter :: fname = 'generated_ops_lt_code.f90'
       type(tcall_pattern) :: call_pattern
       integer :: iostat, old_print_unit, fid, fid_ref, iostat_ref, iline
       character(len=200) :: line, line_ref, msg
       type(Tdo_proc)   :: do_proc
       type(tvChem)     :: vChem
       type(TBuilding)  :: building1d(2)
       type(TBuildingEffect) :: building_effect

      call forget_generated_files()
      building_effect%nParam = 4
      building_effect%nClass = 5
      building_effect%minClass = 6
      building_effect%maxClass = 7

      allocate( building_effect%classdefinitionArray(3), &
                building_effect%buildingFactArray(4),    &
                building_effect%buildingFactAngleSRxaxis(3), &
                building_effect%buildingFactDistances(2) )

       building_effect%classdefinitionArray = 8
       building_effect%buildingFactArray = 9
       building_effect%buildingFactAngleSRxaxis = 7
       building_effect%buildingFactDistances = 6

       call init_apsgrid(m=10, n=10, dx = 1e10, dy=1e10, val=7.25, avg = 4.125, &
              rmasker=vChem%mass_prec_grid)

       call init_apsgrid(m=10, n=10, dx = 1e10, dy=1e10, val=7.25, avg = 4.125, &
              rmasker=vChem%mass_conv_dtfac_grid)

       do_proc%chem          = .true.
       do_proc%depl_drydep   = .false.
       do_proc%depl_wetdep   = .true.
       do_proc%grad_drydep   = .false.

       building1d(1)%length = 1.0 
       building1d(1)%width  = 2.0  
       building1d(1)%height = 3.0   
       building1d(1)%orientation = 4.0
       building1d(1)%x      = 5.0
       building1d(1)%y      = 6.0 
       allocate(building1d(1)%buildingFactFunction(2,3))
       building1d(1)%buildingFactFunction = 9.0
       building1d(1)%type = 2

       building1d(2)%length = 10.0 
       building1d(2)%width  = 11.0  
       building1d(2)%height = 12.0   
       building1d(2)%orientation = 13.0
       building1d(2)%x      = 14.0
       building1d(2)%y      = 15.0 
       allocate(building1d(2)%buildingFactFunction(3,4))
       building1d(2)%buildingFactFunction = 18.0
       building1d(2)%type = 19

       call_pattern = init_call(routinenaam)
       call add_input( call_pattern, "input_vchem", vchem)
       call add_output( call_pattern, "output_vchem", vchem)
       call add_inout( call_pattern, "inout_vchem", vchem)

       call add_input( call_pattern, "input_do_proc", do_proc)
       call add_output( call_pattern, "output_do_proc", do_proc)
       call add_inout( call_pattern, "inout_do_proc", do_proc)

       call add_input( call_pattern, "input_buildings_real", building1d)
       call add_output( call_pattern, "output_buildings_real", building1d)
       call add_inout( call_pattern, "inout_buildings_real", building1d)

       call add_input( call_pattern, "input_building_effect_real", building_effect)
       call add_output( call_pattern, "output_building_effect_real", building_effect)
       call add_inout( call_pattern, "inout_building_effect_real", building_effect)

       call print_test(call_pattern, fname//'.pid' )
        
       call replace_pid(fname)
       call remember_generated_file(fname)
       call CompareTextFiles(fname, './level_1/resources/'//fname)
       call remove_generated_files()
    end subroutine test_log_call3

end module m_test_log_call


program test_log_call
use m_test_log_call
use no_pfunit
   ! Create test output with variables of derived type
   call test_log_call3()

   call conclusion()
end program test_log_call
