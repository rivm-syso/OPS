module m_test_ops_tra_char
implicit none
contains
   subroutine test_ops_tra_char2()
   use m_ops_tra_char
   use no_pfunit_ops_lt
   use m_aps, only: TApsGridInt
   use m_aps, only: TApsGridReal
   use m_ops_vchem, only: tvChem
   use m_error, only: TError
       real, parameter :: tol = 1e-5
        integer, parameter :: icm = 3
        integer, parameter :: iopt_vchem = 0
        logical, parameter :: f_z0user = .false.
        real, parameter :: z0_user =   0.00000000E+00
        real, parameter :: x_rcp =   2.37500000E+05
        real, parameter :: y_rcp =   6.21500000E+05
        integer, parameter :: x_src = 155000
        integer, parameter :: y_src = 463000
        type(TApsGridInt) :: lugrid
        type(TApsGridInt) :: z0nlgrid
        type(TApsGridInt) :: z0eurgrid
        type(TApsGridReal) :: so2bggrid
        type(TApsGridReal) :: no2bggrid
        type(TApsGridReal) :: nh3bggrid
        type(TApsGridReal) :: gwgrid
        type(TApsGridReal) :: o3bggrid
        type(tvChem) :: vchem2
        type(tvChem) :: ref_vchem2
        logical, parameter :: domlu = .false.
        real :: z0_tra
        real, parameter :: ref_z0_tra =   4.47591841E-02
        real :: lu_tra_per(9)
        real, parameter :: ref_lu_tra_per(9) = (/   3.43333321E+01,   1.96190472E+01,   2.85714269E+00, &
             4.28571403E-01,   6.42857122E+00,   2.47619038E+01,   8.52380943E+00,   3.04761887E+00,   0.00000000E+00/)
        real :: so2bgtra
        real, parameter :: ref_so2bgtra =   6.07199967E-01
        real :: no2bgtra
        real, parameter :: ref_no2bgtra =   7.85854387E+00
        real :: gwtra
        real, parameter :: ref_gwtra =   430.0E+00
        real :: nh3bgtra
        real, parameter :: ref_nh3bgtra =   6.48552752E+00
        real :: o3bgtra(12)
        real :: mass_prec_tra, mass_conv_dtfac_tra
        real, parameter :: ref_o3bgtra(12) = (/  -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02, &
            -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02, &
            -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02/)
        type(TError) :: error
        type(TError) :: ref_error
        call InitAps("./level_1/resources/apsgrid_46547_4.int", "InitApsInt", lugrid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/apsgrid_46547_5.int", "InitApsInt", z0nlgrid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/apsgrid_46547_6.int", "InitApsInt", z0eurgrid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/apsgrid_46547_5.real", "InitApsReal", so2bggrid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/apsgrid_46547_6.real", "InitApsReal", no2bggrid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/apsgrid_46547_7.real", "InitApsReal", nh3bggrid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/gammawater.ops", "InitApsReal", gwgrid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/apsgrid_1.real", "InitApsReal", o3bggrid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_prec_grid", vchem2%mass_prec_grid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_conv_dtfac_grid", vchem2%mass_conv_dtfac_grid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_prec_grid", ref_vchem2%mass_prec_grid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_conv_dtfac_grid", ref_vchem2%mass_conv_dtfac_grid, __LINE__, __FILE__)
        ref_error%haserror = .false.
        ref_error%message = ""
        lu_tra_per = 0
        so2bgtra = 0
        no2bgtra = 0 
        nh3bgtra = 0
        call ops_tra_char( icm, iopt_vchem, f_z0user, z0_user, x_rcp, y_rcp, x_src, y_src, lugrid, z0nlgrid, &
            z0eurgrid, so2bggrid, no2bggrid, nh3bggrid, gwgrid, o3bggrid, vchem2, domlu, &
            mass_prec_tra, mass_conv_dtfac_tra, &
            z0_tra, lu_tra_per, so2bgtra, no2bgtra, nh3bgtra, gwtra, o3bgtra, error)
        call assertEqual(vchem2, ref_vchem2, tol, "vchem2", __LINE__, __FILE__)
        call assertEqual( ref_z0_tra, z0_tra, tol, "z0_tra", __LINE__, __FILE__)
        call assertEqual( ref_lu_tra_per, lu_tra_per, tol, "lu_tra_per", __LINE__, __FILE__)
        call assertEqual( ref_so2bgtra, so2bgtra, tol, "so2bgtra", __LINE__, __FILE__)
        call assertEqual( ref_no2bgtra, no2bgtra, tol, "no2bgtra", __LINE__, __FILE__)
        call assertEqual( ref_nh3bgtra, nh3bgtra, tol, "nh3bgtra", __LINE__, __FILE__)
        call assertEqual( ref_gwtra, gwtra, tol, "gwtra", __LINE__, __FILE__)
        call assertEqual( ref_o3bgtra, o3bgtra, tol, "o3bgtra", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_tra_char", __LINE__, __FILE__)
   end subroutine test_ops_tra_char2

   subroutine test_ops_tra_char()
   use m_ops_tra_char
   use no_pfunit_ops_lt
   use m_aps, only: TApsGridInt
   use m_aps, only: TApsGridReal
   use m_ops_vchem, only: tvChem
   use m_error, only: TError
       real, parameter :: tol = 1e-5
        integer, parameter :: icm = 4
        integer, parameter :: iopt_vchem = 0
        logical, parameter :: f_z0user = .true.
        real, parameter :: z0_user =   2.00000003E-01
        real, parameter :: x_rcp =   1.43050000E+05
        real, parameter :: y_rcp =   3.99750000E+05
        integer, parameter :: x_src = 155000
        integer, parameter :: y_src = 385000
        type(TApsGridInt) :: lugrid
        type(TApsGridInt) :: z0nlgrid
        type(TApsGridInt) :: z0eurgrid
        type(TApsGridReal) :: so2bggrid
        type(TApsGridReal) :: no2bggrid
        type(TApsGridReal) :: nh3bggrid
        type(TApsGridReal) :: gwgrid
        type(TApsGridReal) :: o3bggrid
        type(tvChem) :: vchem2
        type(tvChem) :: ref_vchem2
        logical, parameter :: domlu = .false.
        real :: z0_tra
        real, parameter :: ref_z0_tra =   2.00000003E-01
        real :: lu_tra_per(9)
        real, parameter :: ref_lu_tra_per(9) = (/   1.00000000E+02,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00/)
        real :: so2bgtra
        real, parameter :: ref_so2bgtra =   0.00000000E+00
        real :: no2bgtra
        real, parameter :: ref_no2bgtra =   0.00000000E+00
        real :: gwtra
        real, parameter :: ref_gwtra =   0.00000000E+00
        real :: nh3bgtra
        real, parameter :: ref_nh3bgtra =   0.00000000E+00
        real :: o3bgtra(12)
        real, parameter :: ref_o3bgtra(12) = (/  -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02, &
            -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02, &
            -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02/)
        real :: mass_prec_tra, mass_conv_dtfac_tra
        type(TError) :: error
        type(TError) :: ref_error
        call InitAps("./level_1/resources/apsgrid_30491_1891.int", "InitApsInt", lugrid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/apsgrid_30491_1892.int", "InitApsInt", z0nlgrid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/apsgrid_30491_1893.int", "InitApsInt", z0eurgrid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/apsgrid_1.real", "InitApsReal", so2bggrid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/apsgrid_1.real", "InitApsReal", no2bggrid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/apsgrid_1.real", "InitApsReal", nh3bggrid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/apsgrid_1.real", "InitApsReal", gwgrid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/apsgrid_1.real", "InitApsReal", o3bggrid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_prec_grid", vchem2%mass_prec_grid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_conv_dtfac_grid", vchem2%mass_conv_dtfac_grid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_prec_grid", ref_vchem2%mass_prec_grid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_conv_dtfac_grid", ref_vchem2%mass_conv_dtfac_grid, __LINE__, __FILE__)
        ref_error%haserror = .false.
        ref_error%message = ""
        lu_tra_per = 0
        so2bgtra = 0
        no2bgtra = 0 
        nh3bgtra = 0
        gwtra = 0
        call ops_tra_char( icm, iopt_vchem, f_z0user, z0_user, x_rcp, y_rcp, x_src, y_src, lugrid, z0nlgrid, &
            z0eurgrid, so2bggrid, no2bggrid, nh3bggrid, gwgrid, o3bggrid, vchem2, domlu, &
            mass_prec_tra, mass_conv_dtfac_tra, &
            z0_tra, lu_tra_per, so2bgtra, no2bgtra, nh3bgtra, gwtra, o3bgtra, error)
        call assertEqual(vchem2, ref_vchem2, tol, "vchem2", __LINE__, __FILE__)
        call assertEqual( ref_z0_tra, z0_tra, tol, "z0_tra", __LINE__, __FILE__)
        call assertEqual( ref_lu_tra_per, lu_tra_per, tol, "lu_tra_per", __LINE__, __FILE__)
        call assertEqual( ref_so2bgtra, so2bgtra, tol, "so2bgtra", __LINE__, __FILE__)
        call assertEqual( ref_no2bgtra, no2bgtra, tol, "no2bgtra", __LINE__, __FILE__)
        call assertEqual( ref_nh3bgtra, nh3bgtra, tol, "nh3bgtra", __LINE__, __FILE__)
        call assertEqual( ref_gwtra, gwtra, tol, "gwtra", __LINE__, __FILE__)
        call assertEqual( ref_o3bgtra, o3bgtra, tol, "o3bgtra", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_tra_char", __LINE__, __FILE__)
   end subroutine test_ops_tra_char
end module m_test_ops_tra_char
 
program p_test_ops_tra_char
use m_test_ops_tra_char
use no_pfunit
implicit none
   call test_ops_tra_char2()
   call test_ops_tra_char()
   call conclusion()
end program p_test_ops_tra_char
