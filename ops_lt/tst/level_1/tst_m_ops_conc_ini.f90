module m_test_ops_conc_ini
implicit none
contains
   subroutine test_ops_conc_ini2()
   use m_ops_conc_ini
   use no_pfunit_ops_lt
   use m_error, only: TError
   use m_ops_varin
   
        real, parameter :: tol = 1e-5
        real, parameter :: zm = 2.0
        logical, parameter :: gasv = .false.
        real, parameter :: vw10 =   2.01143265E+00
        real, parameter :: htt =   5.00000000E+00
        real, parameter :: pcoef =   2.94000000E-01
        real, parameter :: disx = -999.0
        real, parameter :: disxx =   2.11424922E+04
        integer, parameter :: kdeel = 1, kdeel6=6
        real, parameter :: qbpri =   6.30000010E-02
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: sigz0 =   0.00000000E+00
		logical, parameter :: road_disp = .FALSE.
		logical, parameter :: lroad_corr = .FALSE.
        integer, parameter :: rond = 0
        real, parameter :: uster_src =   2.52821952E-01
        real, parameter :: ol_src =  -3.29010468E+01
        integer, parameter :: ircp = -999
        integer, parameter :: istab = 1
        integer, parameter :: iwd = 135
        real, parameter :: qww =   0.00000000E+00
        real, parameter :: hbron =   5.00000000E+00
        real, parameter :: dispg =   2.14678213E-01
        real, parameter :: in_radius =   0.00000000E+00
        real, parameter :: in_xl =   2.96321716E+02
        real, parameter :: in_onder =   1.00000000E+00
        real :: radius
        real, parameter :: ref_radius =   0.00000000E+00
        real :: xl
        real, parameter :: ref_xl(2) = (/  296.321716, 10.0 /)
        real :: onder
        real, parameter :: ref_onder =   1.00000000E+00
        type(TError) :: error
        type(TError) :: ref_error
        real :: htot
        real, parameter :: ref_htot(2) =  (/ 5.0, -737.598571777 /)
        real :: grof
        real, parameter :: ref_grof(2) =  (/ 0.0, 1.0 /)
        real :: c0_undepl_total, c0_undepl_mix
        real, parameter :: ref_c(3) =  (/ 4.32236400E-03, 1.816762232920E-04, 4.849631513935E-04/)
        real :: sigz
        real, parameter :: ref_sigz(2) = (/  755.815918,  250.332061768 /) 
        real :: ueff
        real, parameter :: ref_ueff(2) = (/  4.44325399, 1.45841169357  /)
        real :: virty
        real, parameter :: ref_virty =   0.00000000E+00
        real :: c_zrcp_undepl_mix
        real, parameter :: ref_ccc(3) = ref_c
        TYPE(Tvarin_meteo) :: varin_meteo  ! input variables for meteo
        type(Tvarin_unc) :: varin_unc

        radius = in_radius
        xl = in_xl
        onder = in_onder
        ref_error%haserror = .false.
        ref_error%message = ""

        call ops_conc_ini(varin_meteo, varin_unc, gasv, vw10, htt, pcoef, disx, disxx, zm, kdeel, qbpri, z0_src, sigz0, road_disp, lroad_corr, rond, uster_src, ol_src, ircp, istab, iwd, qww,    &
                     &  hbron,dispg, radius, xl, onder, htot, grof, c0_undepl_total, c0_undepl_mix, c_zrcp_undepl_mix, sigz, ueff, virty, error)
        call assertEqual( ref_radius, radius, tol, "radius", __LINE__, __FILE__)
        call assertEqual( ref_xl(1), xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_onder, onder, tol, "onder", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_conc_ini", __LINE__, __FILE__)
        call assertEqual( ref_htot(1), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_grof(1), grof, tol, "grof", __LINE__, __FILE__)
        call assertEqual( ref_c(1), c_zrcp_undepl_mix, tol, "c", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_sigz(1), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_ueff(1), ueff, tol, "ueff", __LINE__, __FILE__)
        call assertEqual( ref_virty, virty, tol, "virty", __LINE__, __FILE__)
        call assertEqual( ref_ccc(1), c_zrcp_undepl_mix, tol, "ccc", __LINE__, __FILE__)

        radius = in_radius
        xl = in_xl
        onder = in_onder
        ref_error%haserror = .false.
        ref_error%message = ""
        call ops_conc_ini(varin_meteo, varin_unc, gasv, vw10, htt, pcoef, disx, disxx, zm, kdeel6, qbpri, z0_src, sigz0, road_disp, lroad_corr, rond, uster_src, ol_src, ircp, istab, iwd, qww,    &
                     &  hbron,dispg, radius, xl, onder, htot, grof, c0_undepl_total, c0_undepl_mix, c_zrcp_undepl_mix, sigz, ueff, virty, error)
        call assertEqual( ref_radius, radius, tol, "radius", __LINE__, __FILE__)
        call assertEqual( ref_xl(1), xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_onder, onder, tol, "onder", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_conc_ini", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_htot(2), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_grof(2), grof, tol, "grof", __LINE__, __FILE__)
        call assertEqual( ref_c(2), c_zrcp_undepl_mix, tol, "c", __LINE__, __FILE__)
        call assertEqual( ref_sigz(2), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_ueff(2), ueff, tol, "ueff", __LINE__, __FILE__)
        call assertEqual( ref_virty, virty, tol, "virty", __LINE__, __FILE__)
        call assertEqual( ref_ccc(2), c_zrcp_undepl_mix, tol, "ccc", __LINE__, __FILE__)

        radius = in_radius
        xl = in_xl-1000
        onder = in_onder
        ref_error%haserror = .false.
        ref_error%message = ""
        call ops_conc_ini(varin_meteo, varin_unc, gasv, vw10, htt, pcoef, disx, disxx, zm, kdeel6, qbpri, z0_src, sigz0, road_disp, lroad_corr, rond, uster_src, ol_src, ircp, istab, iwd, qww,    &
                     &  hbron,dispg, radius, xl, onder, htot, grof, c0_undepl_total, c0_undepl_mix, c_zrcp_undepl_mix, sigz, ueff, virty, error)
        call assertEqual( ref_radius, radius, tol, "radius", __LINE__, __FILE__)
        call assertEqual( ref_xl(2), xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_onder, onder, tol, "onder", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_conc_ini", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_htot(2), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_grof(2), grof, tol, "grof", __LINE__, __FILE__)
        call assertEqual( ref_c(3), c_zrcp_undepl_mix, tol, "c", __LINE__, __FILE__)
        call assertEqual( ref_sigz(2), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_ueff(2), ueff, tol, "ueff", __LINE__, __FILE__)
        call assertEqual( ref_virty, virty, tol, "virty", __LINE__, __FILE__)
        call assertEqual( ref_ccc(3), c_zrcp_undepl_mix, tol, "ccc", __LINE__, __FILE__)
   end subroutine test_ops_conc_ini2

   subroutine test_ops_conc_ini()
   use m_ops_conc_ini
   use no_pfunit_ops_lt
   use m_error, only: TError
   use m_ops_varin
   
       real, parameter :: tol = 1e-5
        logical, parameter :: gasv = .true.
        real, parameter :: zm = 2.0
        real, parameter :: vw10 =   2.01143265E+00
        real, parameter :: htt =   5.00000000E+00
        real, parameter :: pcoef =   2.94000000E-01
        real, parameter :: disx = -999.0
        real, parameter :: disxx =   2.11424922E+04
        integer, parameter :: kdeel = 1
        real, parameter :: qbpri =   1.00000000E+00
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: sigz0 =   0.00000000E+00
		logical, parameter :: road_disp = .FALSE.
		logical, parameter :: lroad_corr = .FALSE.
        integer, parameter :: rond = 0
        real, parameter :: uster_src =   2.52821952E-01
        real, parameter :: ol_src =  -3.29010468E+01
        integer, parameter :: ircp = -999
        integer, parameter :: istab = 1
        integer, parameter :: iwd = 135
        real, parameter :: qww =   0.00000000E+00
        real, parameter :: hbron =   5.00000000E+00
        real, parameter :: dispg =   2.14678213E-01
        real, parameter :: in_radius =   0.00000000E+00
        real, parameter :: in_xl =   2.96321716E+02
        real, parameter :: in_onder =   1.00000000E+00
        real :: radius
        real, parameter :: ref_radius =   0.00000000E+00
        real :: xl
        real, parameter :: ref_xl =   2.96321716E+02
        real :: onder
        real, parameter :: ref_onder =   1.00000000E+00
        type(TError) :: error
        type(TError) :: ref_error
        real :: htot
        real, parameter :: ref_htot =   5.00000000E+00
        real :: grof
        real, parameter :: ref_grof =   0.00000000E+00
        real :: c0_undepl_total, c0_undepl_mix
        real, parameter :: ref_c =   6.86089471E-02
        real :: sigz
        real, parameter :: ref_sigz =   7.55815918E+02
        real :: ueff
        real, parameter :: ref_ueff =   4.44325399E+00
        real :: virty
        real, parameter :: ref_virty =   0.00000000E+00
        real :: c_zrcp_undepl_mix
        real, parameter :: ref_ccc =   6.86089471E-02
        TYPE(Tvarin_meteo) :: varin_meteo  ! input variables for meteo
        type(Tvarin_unc) :: varin_unc

        radius = in_radius
        xl = in_xl
        onder = in_onder
        ref_error%haserror = .false.
        ref_error%message = ""
        call ops_conc_ini(varin_meteo, varin_unc, gasv, vw10, htt, pcoef, disx, disxx, zm, kdeel, qbpri, z0_src, sigz0, road_disp, lroad_corr, rond, uster_src, ol_src, ircp, istab, iwd, qww,    &
                     &  hbron,dispg, radius, xl, onder, htot, grof, c0_undepl_total, c0_undepl_mix, c_zrcp_undepl_mix, sigz, ueff, virty, error)
        call assertEqual( ref_radius, radius, tol, "radius", __LINE__, __FILE__)
        call assertEqual( ref_xl, xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_onder, onder, tol, "onder", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_conc_ini", __LINE__, __FILE__)
        call assertEqual( ref_htot, htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_grof, grof, tol, "grof", __LINE__, __FILE__)
        call assertEqual( ref_c, c_zrcp_undepl_mix, tol, "c", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_sigz, sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_ueff, ueff, tol, "ueff", __LINE__, __FILE__)
        call assertEqual( ref_virty, virty, tol, "virty", __LINE__, __FILE__)
        call assertEqual( ref_ccc, c_zrcp_undepl_mix, tol, "ccc", __LINE__, __FILE__)
   end subroutine test_ops_conc_ini
end module m_test_ops_conc_ini
 
program p_test_ops_conc_ini
use m_test_ops_conc_ini
use no_pfunit
implicit none
   call test_ops_conc_ini2()
   call test_ops_conc_ini()
   call conclusion()
end program p_test_ops_conc_ini

