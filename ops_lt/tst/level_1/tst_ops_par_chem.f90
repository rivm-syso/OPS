module m_test_ops_par_chem
   implicit none
contains
   subroutine test_ops_par_chem()
   use m_ops_par_chem
   use no_pfunit_ops_lt
   use m_ops_vchem, only: tvChem
   use m_ops_varin, only: Tvarin_unc

       real, parameter :: tol = 1e-5
       integer, parameter :: icm = 2
       integer, parameter :: icm1 = 1
       integer, parameter :: icm3 = 3
       integer, parameter :: iopt_vchem = 0
       logical, parameter :: isec = .true.
       integer, parameter :: isec_prelim = 6
       real, parameter :: so2sek(12) = (/   7.69999981E-01,   7.30000019E-01,   8.79999995E-01,   1.09000003E+00, &
            1.29999995E+00,   1.34000003E+00,   1.27999997E+00,   1.13999999E+00,   9.70000029E-01,   9.39999998E-01, &
            8.99999976E-01,   7.69999981E-01/)
       real, parameter :: no2sek(12) = (/   8.10000002E-01,   8.79999995E-01,   1.08000004E+00,   1.29999995E+00, &
            1.33000004E+00,   1.39999998E+00,   1.25000000E+00,   1.02999997E+00,   8.29999983E-01,   7.09999979E-01, &
            6.99999988E-01,   6.80000007E-01/)
       real, parameter :: so2bgtra =   0.00000000E+00
       real, parameter :: no2bgtra =   1.15086336E+01
       real, parameter :: nh3bgtra =   1.32029724E+01
       real, parameter :: o3bgtra(12) = (/  -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02, &
           -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02, &
           -9.99000000E+02,  -9.99000000E+02/)
       type(tvChem) :: vchem2
       type(tvChem) :: ref_vchem2
       real, parameter :: disx =   2.11424922E+04
       real, parameter :: diameter =   0.00000000E+00
       type(Tvarin_unc) :: varin_unc
       real :: vchemnh3
       real, parameter :: ref_vchemnh3(2) = (/  0.00000000E+00,   3.31040954590 /)
       real :: rhno3_trj
       real, parameter :: ref_rhno3_trj =   1.61107481E-01
       real :: r_no2_nox_year_bg_tra
       real, parameter :: ref_r_no2_nox_year_bg_tra(2) =   (/ 0.900318980,   1.15106201172 /)
       real :: rations
       real, parameter :: ref_rations(2) =  (/ 0.0, 5.64229536057 /)
       real mass_prec_tra, mass_conv_dtfac_tra
       real :: vchem, ref_vchem
       mass_prec_tra =   0.00000000E+00
       call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_prec_grid", vchem2%mass_prec_grid, __LINE__, __FILE__)
       call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_conv_dtfac_grid", vchem2%mass_conv_dtfac_grid, __LINE__, __FILE__)
       call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_prec_grid", ref_vchem2%mass_prec_grid, __LINE__, __FILE__)
       call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_conv_dtfac_grid", ref_vchem2%mass_conv_dtfac_grid, __LINE__, __FILE__)

      r_no2_nox_year_bg_tra = 0  
      vchemnh3 = 0
      rations = 0
      vchem =   0.00000000E+00
      ref_vchem =   0.00000000E+00
      call ops_par_chem( &
         icm, isec, iopt_vchem, isec_prelim, so2sek, no2sek, so2bgtra, no2bgtra, nh3bgtra, &
         o3bgtra, mass_prec_tra, mass_conv_dtfac_tra, disx, diameter, varin_unc, vchem, &
         vchemnh3, rhno3_trj, r_no2_nox_year_bg_tra, rations)
      call assertEqual( vchem, ref_vchem, tol, "vchem2", __LINE__, __FILE__)
      call assertEqual( ref_vchemnh3(1), vchemnh3, tol, "vchemnh3", __LINE__, __FILE__)
      call assertEqual( ref_rhno3_trj, rhno3_trj, tol, "rhno3_trj", __LINE__, __FILE__)
      call assertEqual( ref_r_no2_nox_year_bg_tra(1), r_no2_nox_year_bg_tra, tol, "r_no2_nox_year_bg_tra", __LINE__, __FILE__)
      call assertEqual( ref_rations(1), rations, tol, "rations", __LINE__, __FILE__)

      r_no2_nox_year_bg_tra = 0  
      vchemnh3 = 0
      rations = 0
      call ops_par_chem( icm, isec, iopt_vchem, isec_prelim, so2sek, no2sek, so2bgtra, no2bgtra-100, nh3bgtra, &
          o3bgtra, mass_prec_tra, mass_conv_dtfac_tra, &
          disx, diameter, varin_unc, vchem, vchemnh3, rhno3_trj, r_no2_nox_year_bg_tra, rations)
      call assertEqual(vchem, ref_vchem, tol, "vchem2", __LINE__, __FILE__)
      call assertEqual( ref_vchemnh3(1), vchemnh3, tol, "vchemnh3", __LINE__, __FILE__)
      call assertEqual( ref_rhno3_trj, rhno3_trj, tol, "rhno3_trj", __LINE__, __FILE__)
      call assertEqual( ref_r_no2_nox_year_bg_tra(2), r_no2_nox_year_bg_tra, tol, "r_no2_nox_year_bg_tra", __LINE__, __FILE__)
      call assertEqual( ref_rations(1), rations, tol, "rations", __LINE__, __FILE__)

      r_no2_nox_year_bg_tra = 0  
      vchemnh3 = 0
      rations = 0
      call ops_par_chem( &
         icm3, isec, iopt_vchem, isec_prelim, so2sek, no2sek, so2bgtra, no2bgtra, nh3bgtra, &
         o3bgtra, mass_prec_tra, mass_conv_dtfac_tra, disx, diameter+2*disx, varin_unc, &
         vchem, vchemnh3, rhno3_trj, r_no2_nox_year_bg_tra, rations)
      call assertEqual(vchem, ref_vchem, tol, "vchem2", __LINE__, __FILE__)
      call assertEqual( ref_vchemnh3(2), vchemnh3, tol, "vchemnh3", __LINE__, __FILE__)
      call assertEqual( ref_rhno3_trj, rhno3_trj, tol, "rhno3_trj", __LINE__, __FILE__)
      call assertEqual( 0., r_no2_nox_year_bg_tra, tol, "r_no2_nox_year_bg_tra", __LINE__, __FILE__)
      call assertEqual( ref_rations(1), rations, tol, "rations", __LINE__, __FILE__)

      r_no2_nox_year_bg_tra = 0  
      vchemnh3 = 0
      call ops_par_chem( &
         icm1, isec, iopt_vchem, isec_prelim, so2sek, no2sek, so2bgtra+1, no2bgtra, nh3bgtra, &
         o3bgtra, mass_prec_tra, mass_conv_dtfac_tra, disx, diameter+2*disx, varin_unc, vchem, &
         vchemnh3, rhno3_trj, r_no2_nox_year_bg_tra, rations)
      call assertEqual(vchem, ref_vchem, tol, "vchem2", __LINE__, __FILE__)
      call assertEqual( ref_vchemnh3(1), vchemnh3, tol, "vchemnh3", __LINE__, __FILE__)
      call assertEqual( ref_rhno3_trj, rhno3_trj, tol, "rhno3_trj", __LINE__, __FILE__)
      call assertEqual( 0., r_no2_nox_year_bg_tra, tol, "r_no2_nox_year_bg_tra", __LINE__, __FILE__)
      call assertEqual( ref_rations(2), rations, tol, "rations", __LINE__, __FILE__)
   end subroutine test_ops_par_chem
end module m_test_ops_par_chem
 
program p_test_ops_par_chem
use m_test_ops_par_chem
use no_pfunit
implicit none
   call test_ops_par_chem()
   call conclusion()
end program p_test_ops_par_chem

