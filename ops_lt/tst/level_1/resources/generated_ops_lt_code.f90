
module m_test_test_log_call
implicit none
contains
   subroutine test_test_log_call()
   use no_pfunit_ops_lt
   use m_ops_vchem, only: tvChem
   use m_ops_tdo_proc, only: tdo_proc
   use m_ops_building, only: TBuilding
   use m_ops_building, only: TBuildingEffect
       integer         :: log_call_stat, i1, i2, i3, i4, i5, fid
       real, parameter :: tol = 1e-5
        type(tvChem) :: input_vchem
        type(tvChem) :: output_vchem
        type(tvChem) :: ref_output_vchem
        type(tdo_proc) :: input_do_proc
        type(tdo_proc) :: output_do_proc
        type(tdo_proc) :: ref_output_do_proc
        type(TBuilding) :: input_buildings_real
        type(TBuilding) :: output_buildings_real
        type(TBuilding) :: ref_output_buildings_real
        type(TBuildingEffect) :: input_building_effect_real
        type(TBuildingEffect) :: output_building_effect_real
        type(TBuildingEffect) :: ref_output_building_effect_real
        call InitAps("mass_prec_grid_pid_1.real", "Init mass_prec_grid", input_vchem%mass_prec_grid, __LINE__, __FILE__)
        call InitAps("mass_conv_dtfac_grid_pid_1.real", "Init mass_conv_dtfac_grid", input_vchem%mass_conv_dtfac_grid, __LINE__, __FILE__)
        call InitAps("mass_prec_grid_pid_2.real", "Init mass_prec_grid", ref_output_vchem%mass_prec_grid, __LINE__, __FILE__)
        call InitAps("mass_conv_dtfac_grid_pid_2.real", "Init mass_conv_dtfac_grid", ref_output_vchem%mass_conv_dtfac_grid, __LINE__, __FILE__)
        call InitAps("mass_prec_grid_pid_3.real", "Init mass_prec_grid", inout_vchem%mass_prec_grid, __LINE__, __FILE__)
        call InitAps("mass_conv_dtfac_grid_pid_3.real", "Init mass_conv_dtfac_grid", inout_vchem%mass_conv_dtfac_grid, __LINE__, __FILE__)
        input_do_proc%chem = .true.
        input_do_proc%depl_drydep = .false.
        input_do_proc%depl_wetdep = .true.
        input_do_proc%grad_drydep = .false.
        ref_output_do_proc%chem = .true.
        ref_output_do_proc%depl_drydep = .false.
        ref_output_do_proc%depl_wetdep = .true.
        ref_output_do_proc%grad_drydep = .false.
        inout_do_proc%chem = .true.
        inout_do_proc%depl_drydep = .false.
        inout_do_proc%depl_wetdep = .true.
        inout_do_proc%grad_drydep = .false.
        input_buildings_real(1)%length =   1.00000000E+00
        input_buildings_real(1)%width =   2.00000000E+00
        input_buildings_real(1)%height =   3.00000000E+00
        input_buildings_real(1)%orientation =   4.00000000E+00
        input_buildings_real(1)%x =   5.00000000E+00
        input_buildings_real(1)%y =   6.00000000E+00
        input_buildings_real(1)%buildingFactFunction =   9.00000000E+00,  9.00000000E+00,  9.00000000E+00,  9.00000000E+00,  9.00000000E+00,  9.00000000E+00
        input_buildings_real(1)%type =            2
        ref_output_buildings_real(1)%length =   1.00000000E+00
        ref_output_buildings_real(1)%width =   2.00000000E+00
        ref_output_buildings_real(1)%height =   3.00000000E+00
        ref_output_buildings_real(1)%orientation =   4.00000000E+00
        ref_output_buildings_real(1)%x =   5.00000000E+00
        ref_output_buildings_real(1)%y =   6.00000000E+00
        ref_output_buildings_real(1)%buildingFactFunction =   9.00000000E+00,  9.00000000E+00,  9.00000000E+00,  9.00000000E+00,  9.00000000E+00,  9.00000000E+00
        ref_output_buildings_real(1)%type =            2
        inout_buildings_real(1)%length =   1.00000000E+00
        inout_buildings_real(1)%width =   2.00000000E+00
        inout_buildings_real(1)%height =   3.00000000E+00
        inout_buildings_real(1)%orientation =   4.00000000E+00
        inout_buildings_real(1)%x =   5.00000000E+00
        inout_buildings_real(1)%y =   6.00000000E+00
        inout_buildings_real(1)%buildingFactFunction =   9.00000000E+00,  9.00000000E+00,  9.00000000E+00,  9.00000000E+00,  9.00000000E+00,  9.00000000E+00
        inout_buildings_real(1)%type =            2
        input_building_effect_real%nParam =            4
        input_building_effect_real%classdefinitionArray = [  8.00000000E+00,  8.00000000E+00,  8.00000000E+00]
        input_building_effect_real%nClass = [5,5,5,5]
        input_building_effect_real%minClass = [  6.00000000E+00,  6.00000000E+00,  6.00000000E+00,  6.00000000E+00]
        input_building_effect_real%maxClass = [  7.00000000E+00,  7.00000000E+00,  7.00000000E+00,  7.00000000E+00]
        input_building_effect_real%buildingFactArray = [  9.00000000E+00,  9.00000000E+00,  9.00000000E+00,  9.00000000E+00]
        input_building_effect_real%buildingFactAngleSRxaxis = [  7.00000000E+00,  7.00000000E+00,  7.00000000E+00]
        input_building_effect_real%buildingFactDistances = [  6.00000000E+00,  6.00000000E+00]
        ref_output_building_effect_real%nParam =            4
        ref_output_building_effect_real%classdefinitionArray = [  8.00000000E+00,  8.00000000E+00,  8.00000000E+00]
        ref_output_building_effect_real%nClass = [5,5,5,5]
        ref_output_building_effect_real%minClass = [  6.00000000E+00,  6.00000000E+00,  6.00000000E+00,  6.00000000E+00]
        ref_output_building_effect_real%maxClass = [  7.00000000E+00,  7.00000000E+00,  7.00000000E+00,  7.00000000E+00]
        ref_output_building_effect_real%buildingFactArray = [  9.00000000E+00,  9.00000000E+00,  9.00000000E+00,  9.00000000E+00]
        ref_output_building_effect_real%buildingFactAngleSRxaxis = [  7.00000000E+00,  7.00000000E+00,  7.00000000E+00]
        ref_output_building_effect_real%buildingFactDistances = [  6.00000000E+00,  6.00000000E+00]
        inout_building_effect_real%nParam =            4
        inout_building_effect_real%classdefinitionArray = [  8.00000000E+00,  8.00000000E+00,  8.00000000E+00]
        inout_building_effect_real%nClass = [5,5,5,5]
        inout_building_effect_real%minClass = [  6.00000000E+00,  6.00000000E+00,  6.00000000E+00,  6.00000000E+00]
        inout_building_effect_real%maxClass = [  7.00000000E+00,  7.00000000E+00,  7.00000000E+00,  7.00000000E+00]
        inout_building_effect_real%buildingFactArray = [  9.00000000E+00,  9.00000000E+00,  9.00000000E+00,  9.00000000E+00]
        inout_building_effect_real%buildingFactAngleSRxaxis = [  7.00000000E+00,  7.00000000E+00,  7.00000000E+00]
        inout_building_effect_real%buildingFactDistances = [  6.00000000E+00,  6.00000000E+00]
        call test_log_call( input_vchem, output_vchem, input_do_proc, output_do_proc, input_buildings_real, &
            output_buildings_real, input_building_effect_real, output_building_effect_real,)
        call assertEqual(output_vchem, ref_output_vchem, tol, "output_vchem", __LINE__, __FILE__)
        call assertEqual(output_do_proc, ref_output_do_proc, tol, "output_do_proc", __LINE__, __FILE__)
        call assertEqual(output_buildings_real, ref_output_buildings_real, tol, "output_buildings_real", __LINE__, __FILE__)
        call assertEqual(output_building_effect_real, ref_output_building_effect_real, tol, "output_building_effect_real", __LINE__, __FILE__)
   end subroutine test_test_log_call
end module m_test_test_log_call

program p_test_test_log_call
use m_test_test_log_call
use no_pfunit
implicit none
   call test_test_log_call()
   call conclusion()
end program p_test_test_log_call
