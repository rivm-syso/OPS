module m_test_m_ops_conltexp
implicit none
contains
   subroutine test_ops_conltexp()
   use m_ops_conltexp
   use no_pfunit_ops_lt
   use m_error, only: TError
   use m_ops_varin

        TYPE(Tvarin_meteo) :: varin_meteo ! input variables for meteo
        type(Tvarin_unc) :: varin_unc
        real, parameter :: tol = 1e-5
        integer, parameter :: rond = 0
        real, parameter :: ol =  -3.29010468E+01
        real, parameter :: qbron =   1.00000000E+00
        real, parameter :: sigz0 =   0.00000000E+00
		logical, parameter :: lroad_corr = .FALSE.
		logical, parameter :: road_disp = .FALSE.
        real, parameter :: uster =   2.52821952E-01
        real, parameter :: z0 =   2.00000003E-01
        real, parameter :: htt =   5.00000000E+00
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: vw10 =   2.01143265E+00
        real, parameter :: pcoef =   2.94000000E-01
        integer, parameter :: ircp = -999
        integer, parameter :: istab = 1
        real, parameter :: disx = -999.0
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: grof =   0.00000000E+00
        integer, parameter :: iwd = 135
        real, parameter :: qww =   0.00000000E+00
        real, parameter :: hbron =   5.00000000E+00
        real, parameter :: dispg(6) = (/   2.14678213E-01,   2.80000001E-01,   2.00000003E-01,   2.00000003E-01, &
             1.19999997E-01,   2.00000003E-01/)
        real, parameter :: in_radius =   0.00000000E+00
        real, parameter :: in_htot =   5.00000000E+00
        real, parameter :: in_xl =   2.96321716E+02
        real, parameter :: in_zrcp =   2.0
        real :: radius
        real, parameter :: ref_radius(4) =   (/ 0.0, 10000., 4.21733665466,  1405.77893066/)
        real :: htot
        real, parameter :: ref_htot(2) =   (/ 5.0,  1000005./)
        real :: c0_undepl_total
        real, parameter :: ref_c0(10) =   (/ 6.86089471E-02,  0.0,  0.06542897224426, 0.221354499459, 0.161547631025, 0.122378714383, 0.122916728258,  2.409760782029E-04, 11.0018739700, 3.59033560753 /)
        real :: c_zrcp_undepl_total 
        real, parameter :: ref_c_zrcp(10) =   (/ 6.86089471E-02,  0.0,  0.06542897224426, 0.221354499459, 0.161547631025, 0.122378714383, 0.122916728258,  2.409760782029E-04, 11.0018739700, 3.59033560753 /)
        real :: sigz
        real, parameter :: ref_sigz(7) =   (/ 755.815918, 250.332061768, 1.0, 391.579925537, 375.923400879, 3034.75244141, 14.5435609818 /)
        real :: ueff
        real, parameter :: ref_ueff(6) =  (/ 4.44325399,   1.64059984684, 1.45841169357, 6.35334825516, 2.48249697685, 2.08232736588 /)
        real :: xl
        real, parameter :: ref_xl(4) =  (/ 296.321716,   1511.63183594, 80.0,  10296.3212891 /)
        real :: virty
        real, parameter :: ref_virty(4) =   (/ 0.0,  43086.4257812, 12.9259281158, 4308.64257812 /)
        real :: zrcp
        
        type(TError) :: error
        type(TError) :: ref_error

        ref_error%haserror = .false.
        ref_error%message = ""
        radius = in_radius
        htot = in_htot
        xl = in_xl
        zrcp = in_zrcp
        call ops_conltexp( varin_meteo, varin_unc, zrcp, rond, ol, qbron, sigz0, road_disp, lroad_corr, uster, z0, htt, onder, vw10, pcoef, ircp, istab, disx, disxx, grof, &
            iwd, qww, hbron, dispg(istab), radius, htot, c0_undepl_total, c_zrcp_undepl_total, sigz, ueff, xl, virty, error)

        call assertEqual( ref_radius(1), radius, tol, "radius", __LINE__, __FILE__)
        call assertEqual( ref_htot(1), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_c0(1), c0_undepl_total, tol, "c0_undepl_total", __LINE__, __FILE__)
        call assertEqual( ref_c_zrcp(1), c_zrcp_undepl_total, tol, "c_zrcp_undepl_total", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_sigz(1), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_ueff(1), ueff, tol, "ueff", __LINE__, __FILE__)
        call assertEqual( ref_xl(1), xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_virty(1), virty, tol, "virty", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_conltexp", __LINE__, __FILE__)

        ref_error%haserror = .false.
        ref_error%message = ""
        radius = in_radius
        htot = in_htot
        xl = in_xl
        call ops_conltexp( varin_meteo, varin_unc, zrcp, rond, ol, qbron, sigz0, road_disp, lroad_corr, uster, z0, htt, onder*0, vw10, pcoef, ircp, istab, disx, disxx, grof, &
            iwd, qww, hbron, dispg(istab), radius, htot, c0_undepl_total, c_zrcp_undepl_total, sigz, ueff, xl, virty, error)
        call assertEqual( ref_radius(1), radius, tol, "radius", __LINE__, __FILE__)
        call assertEqual( ref_htot(1), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_c0(2), c0_undepl_total, tol, "c0_undepl_total", __LINE__, __FILE__)
        call assertEqual( ref_c_zrcp(2), c_zrcp_undepl_total, tol, "c_zrcp_undepl_total", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_sigz(1), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_ueff(2), ueff, tol, "ueff", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_xl(2), xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_virty(1), virty, tol, "virty", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_conltexp", __LINE__, __FILE__)

        ref_error%haserror = .false.
        ref_error%message = ""
        radius = in_radius
        htot = in_htot
        xl = in_xl
        call ops_conltexp( varin_meteo, varin_unc, zrcp, rond, ol, qbron, sigz0, road_disp, lroad_corr, uster, z0, htt, onder, vw10, pcoef, ircp, istab, disx, disxx, grof+3, &
            iwd, qww, hbron, dispg(istab), radius, htot, c0_undepl_total, c_zrcp_undepl_total, sigz, ueff, xl, virty, error)
        call assertEqual( ref_radius(1), radius, tol, "radius", __LINE__, __FILE__)
        call assertEqual( ref_htot(1), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_c0(5), c0_undepl_total, tol, "c0_undepl_total", __LINE__, __FILE__)
        call assertEqual( ref_c_zrcp(5), c_zrcp_undepl_total, tol, "c_zrcp_undepl_total", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_sigz(1), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_ueff(3), ueff, tol, "ueff", __LINE__, __FILE__)
        call assertEqual( ref_xl(1), xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_virty(1), virty, tol, "virty", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_conltexp", __LINE__, __FILE__)

        ref_error%haserror = .false.
        ref_error%message = ""
        radius = in_radius 
        htot = in_htot
        xl = in_xl
        call ops_conltexp( varin_meteo, varin_unc, zrcp, rond, ol, qbron, sigz0, road_disp, lroad_corr, uster, z0, htt, onder, vw10, pcoef, ircp, istab, disx, disxx, grof+1, &
            iwd, qww, hbron, dispg(istab), radius, htot, c0_undepl_total, c_zrcp_undepl_total, sigz, ueff, xl, virty, error)
        call assertEqual( ref_radius(1), radius, tol, "radius", __LINE__, __FILE__)
        call assertEqual( ref_htot(1), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_c0(4), c0_undepl_total, tol, "c0_undepl_total", __LINE__, __FILE__)
        call assertEqual( ref_c_zrcp(4), c_zrcp_undepl_total, tol, "c_zrcp_undepl_total", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_sigz(2), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_ueff(3), ueff, tol, "ueff", __LINE__, __FILE__)
        call assertEqual( ref_xl(1), xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_virty(1), virty, tol, "virty", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_conltexp", __LINE__, __FILE__)

        ref_error%haserror = .false.
        ref_error%message = ""
        radius = in_radius + 1e4
        htot = in_htot + 1e6
        xl = in_xl
        call ops_conltexp( varin_meteo, varin_unc, zrcp, rond, ol, qbron, sigz0, road_disp, lroad_corr, uster, z0, htt, onder*0, vw10, pcoef, ircp, istab, disx, disxx, grof, &
            iwd, qww, hbron, dispg(istab), radius, htot, c0_undepl_total, c_zrcp_undepl_total, sigz, ueff, xl, virty, error)
        call assertEqual( ref_radius(2), radius, tol, "radius", __LINE__, __FILE__)
        call assertEqual( ref_htot(2), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_c0(2), c0_undepl_total, tol, "c0_undepl_total", __LINE__, __FILE__)
        call assertEqual( ref_c_zrcp(2), c_zrcp_undepl_total, tol, "c_zrcp_undepl_total", __LINE__, __FILE__)        
        call assertRelativelyEqual( ref_sigz(1), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_ueff(4), ueff, tol, "ueff", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_xl(2), xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_virty(2), virty, tol, "virty", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_conltexp", __LINE__, __FILE__)

        ref_error%haserror = .false.
        ref_error%message = ""
        radius = in_radius + 1e4
        htot = in_htot + 1e6
        xl = in_xl
        call ops_conltexp( varin_meteo, varin_unc, zrcp, rond, ol, qbron, sigz0, road_disp, lroad_corr, uster, z0, htt, onder*0, vw10, pcoef, ircp, istab, disx, disxx*0, grof, &
            iwd, qww, hbron, dispg(istab), radius, htot, c0_undepl_total, c_zrcp_undepl_total, sigz, ueff, xl, virty, error)
        call assertEqual( ref_radius(2), radius, tol, "radius", __LINE__, __FILE__)
        call assertEqual( ref_htot(2), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_c0(2), c0_undepl_total, tol, "c0_undepl_total", __LINE__, __FILE__)
        call assertEqual( ref_c_zrcp(2), c_zrcp_undepl_total, tol, "c_zrcp_undepl_total", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_sigz(3), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_ueff(4), ueff, tol, "ueff", __LINE__, __FILE__)
        call assertEqual( ref_xl(3), xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_virty(2), virty, tol, "virty", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_conltexp", __LINE__, __FILE__)

        ref_error%haserror = .false.
        ref_error%message = ""
        radius = in_radius
        htot = in_htot 
        xl = in_xl
        call ops_conltexp( varin_meteo, varin_unc, zrcp, rond, ol, qbron, sigz0, road_disp, lroad_corr, uster, z0, htt+50, onder, vw10, pcoef, ircp, istab, disx, disxx, grof+1, &
            iwd, qww, hbron, dispg(istab), radius, htot, c0_undepl_total, c_zrcp_undepl_total, sigz, ueff, xl, virty, error)
        call assertEqual( ref_radius(1), radius, tol, "radius", __LINE__, __FILE__)
        call assertEqual( ref_htot(1), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_c0(6), c0_undepl_total, tol, "c0_undepl_total", __LINE__, __FILE__)
        call assertEqual( ref_c_zrcp(6), c_zrcp_undepl_total, tol, "c_zrcp_undepl_total", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_sigz(4), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_ueff(5), ueff, tol, "ueff", __LINE__, __FILE__)
        call assertEqual( ref_xl(1), xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_virty(1), virty, tol, "virty", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_conltexp", __LINE__, __FILE__)

        ref_error%haserror = .false.
        ref_error%message = ""
        radius = in_radius  + 3
        htot = in_htot 
        xl = in_xl
        call ops_conltexp( varin_meteo, varin_unc, zrcp, rond, ol, qbron, sigz0, road_disp, lroad_corr, uster, z0, htt+50, onder, vw10, pcoef, ircp, istab, disx, disxx, grof+1, &
            iwd, qww, hbron, dispg(istab), radius, htot, c0_undepl_total, c_zrcp_undepl_total, sigz, ueff, xl, virty, error)
        call assertEqual( ref_radius(3), radius, tol, "radius", __LINE__, __FILE__)
        call assertEqual( ref_htot(1), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_c0(7), c0_undepl_total, tol, "c0_undepl_total", __LINE__, __FILE__)
        call assertEqual( ref_c_zrcp(7), c_zrcp_undepl_total, tol, "c_zrcp_undepl_total", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_sigz(5), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_ueff(5), ueff, tol, "ueff", __LINE__, __FILE__)
        call assertEqual( ref_xl(1), xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_virty(3), virty, tol, "virty", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_conltexp", __LINE__, __FILE__)

        ref_error%haserror = .false.
        ref_error%message = ""
        radius = in_radius  + 3
        htot = in_htot 
        xl = in_xl + 1e4
        call ops_conltexp( varin_meteo, varin_unc, zrcp, rond, ol, qbron, sigz0, road_disp, lroad_corr, uster, z0, htt+50, onder, vw10, pcoef, ircp, istab, disx, disxx+1e5, grof+0.1, &
            iwd, qww, hbron, dispg(istab), radius, htot, c0_undepl_total, c_zrcp_undepl_total, sigz, ueff, xl, virty, error)
        call assertEqual( ref_radius(3), radius, tol, "radius", __LINE__, __FILE__)
        call assertEqual( ref_htot(1), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_c0(8), c0_undepl_total, tol, "c0_undepl_total", __LINE__, __FILE__)
        call assertEqual( ref_c_zrcp(8), c_zrcp_undepl_total, tol, "c_zrcp_undepl_total", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_sigz(6), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_ueff(4), ueff, tol, "ueff", __LINE__, __FILE__)
        call assertEqual( ref_xl(4), xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_virty(3), virty, tol, "virty", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_conltexp", __LINE__, __FILE__)

        ref_error%haserror = .false.
        ref_error%message = ""
        radius = in_radius + 1000.0
        htot = in_htot 
        xl = in_xl 
        call ops_conltexp( varin_meteo, varin_unc, zrcp, rond, ol, qbron, sigz0, road_disp, lroad_corr, uster, z0, htt, onder, vw10, pcoef, ircp, istab, disx, disxx/1000., grof+0.1, &
            iwd, qww, hbron, dispg(istab), radius, htot, c0_undepl_total, c_zrcp_undepl_total, sigz, ueff, xl, virty, error)
        call assertRelativelyEqual( ref_radius(4), radius, tol, "radius", __LINE__, __FILE__)
        call assertEqual( ref_htot(1), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_c0(9), c0_undepl_total, tol, "c0_undepl_total", __LINE__, __FILE__)
        call assertEqual( ref_c_zrcp(9), c_zrcp_undepl_total, tol, "c_zrcp_undepl_total", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_sigz(7), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_ueff(6), ueff, tol, "ueff", __LINE__, __FILE__)
        call assertEqual( ref_xl(1), xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_virty(4), virty, tol, "virty", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_conltexp", __LINE__, __FILE__)

        ref_error%haserror = .false.
        ref_error%message = ""
        radius = in_radius + 1000.0
        htot = in_htot 
        xl = in_xl 
        call ops_conltexp( varin_meteo, varin_unc, zrcp, rond, ol, qbron, sigz0, road_disp, lroad_corr, uster, z0, htt+1e6, onder, vw10, pcoef, ircp, istab, disx, disxx/1000., grof+0.1, &
            iwd, qww, hbron, dispg(istab), radius, htot, c0_undepl_total, c_zrcp_undepl_total, sigz, ueff, xl, virty, error)
        call assertRelativelyEqual( ref_radius(4), radius, tol, "radius", __LINE__, __FILE__)
        call assertEqual( ref_htot(1), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_c0(10), c0_undepl_total, tol, "c0_undepl_total", __LINE__, __FILE__)
        call assertEqual( ref_c_zrcp(10), c_zrcp_undepl_total, tol, "c_zrcp_undepl_total", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_sigz(7), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_ueff(4), ueff, tol, "ueff", __LINE__, __FILE__)
        call assertEqual( ref_xl(1), xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_virty(4), virty, tol, "virty", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_conltexp", __LINE__, __FILE__)
   end subroutine test_ops_conltexp

   subroutine test_ops_conlt_par_oppbr()
   use m_ops_conltexp
   use no_pfunit_ops_lt
   use m_ops_varin

        real, parameter :: tol = 1e-5
        integer, parameter :: rond = 1
        integer, parameter :: iwd = 135
        real, parameter :: disxx =   2.11424922E+04
        integer, parameter :: istab = 1
        real, parameter :: disp =   8.19999993E-01
        real, parameter :: htt =   5.00000000E+00
        real, parameter :: grof =   0.00000000E+00
        real, parameter :: sigz0 =   0.00000000E+00
        real, parameter :: dispg(6) = (/   1.94816008E-01,   2.80000001E-01,   2.00000003E-01,   2.00000003E-01, &
             1.19999997E-01,   2.00000003E-01/)
        real, parameter :: in_radius =   1.00000000E+03
        real :: radius
        real, parameter :: ref_radius(2) =   (/  994.035767,  1405.77893066 /)
        real :: virty
        real, parameter :: ref_virty(2) =  (/ 3819.71851,  4308.64257812 /)
        real :: sigz
        real, parameter :: ref_sigz(3) =   (/ 657.454468,   651.920898438, 13.3857336044 /)
        real, parameter :: in_pld =   0.00000000E+00
        real :: pld
        real, parameter :: ref_pld(2) =  (/ 0.00000000E+00,  16.0628814697 /)
        real, parameter :: in_htot =   5.00000000E+00
        real :: htot
        real, parameter :: ref_htot(2) =   (/  5.00000000E+00,  -11.0628814697 /)
        radius = in_radius
        pld = in_pld
        htot = in_htot
        call ops_conlt_par_oppbr( rond, iwd, disxx, istab, disp, htt, grof, dispg(istab), sigz0, radius, virty, &
            sigz, pld, htot)
        call assertRelativelyEqual( ref_radius(1), radius, tol, "radius", __LINE__, __FILE__)
        call assertEqual( ref_virty(1), virty, tol, "virty", __LINE__, __FILE__)
        call assertEqual( ref_sigz(1), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_pld(1), pld, tol, "pld", __LINE__, __FILE__)
        call assertEqual( ref_htot(1), htot, tol, "htot", __LINE__, __FILE__)

        radius = in_radius
        pld = in_pld
        htot = in_htot
        call ops_conlt_par_oppbr( rond+1, iwd, disxx, istab, disp, htt, grof, dispg(istab), sigz0, radius, virty, &
            sigz, pld, htot)
        call assertRelativelyEqual( ref_radius(2), radius, tol, "radius", __LINE__, __FILE__)
        call assertEqual( ref_virty(2), virty, tol, "virty", __LINE__, __FILE__)
        call assertEqual( ref_sigz(2), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_pld(1), pld, tol, "pld", __LINE__, __FILE__)
        call assertEqual( ref_htot(1), htot, tol, "htot", __LINE__, __FILE__)

        radius = in_radius
        pld = in_pld
        htot = in_htot
        call ops_conlt_par_oppbr( rond+1, iwd, disxx/100, istab, disp, htt, grof+1, dispg(istab), sigz0, radius, virty, &
            sigz, pld, htot)
        call assertRelativelyEqual( ref_radius(2), radius, tol, "radius", __LINE__, __FILE__)
        call assertEqual( ref_virty(2), virty, tol, "virty", __LINE__, __FILE__)
        call assertEqual( ref_sigz(3), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_pld(2), pld, tol, "pld", __LINE__, __FILE__)
        call assertEqual( ref_htot(2), htot, tol, "htot", __LINE__, __FILE__)
   end subroutine test_ops_conlt_par_oppbr

   subroutine test_ops_conlt_par_puntbr()
   use m_ops_conltexp, only: ops_conlt_par_puntbr
   use m_commonconst_lt, only: NSTAB
   use no_pfunit_ops_lt
   use m_ops_varin

        real, parameter :: tol = 1e-5 
        real, parameter :: qww =   0.00000000E+00
        integer, parameter :: istab = 2
        real, parameter :: disxx =   2.01098242E+04
        real, parameter :: disp =   8.19999993E-01
        real, parameter :: htt =   5.00000000E+00
        real, parameter :: htot =   5.00000000E+00
        real, parameter :: hbron =   5.00000000E+00
        real, parameter :: dispg(6) = (/   1.71423271E-01,   4.09651458E-01,   7.88364708E-02,   1.33686513E-01, &
             5.04859425E-02,   3.54026780E-02/)
        real :: sigz
        real, parameter :: ref_sigz(2) = (/ 1384.23315430, 1384.23571777 /)
        real :: virty
        real, parameter :: ref_virty =   0.00000000E+00
        real :: sigz0 =   0.00000000E+00
        
        call ops_conlt_par_puntbr( qww, disxx, disp, htot, hbron, dispg(istab), sigz0, sigz, virty)
        call assertEqual( ref_sigz(1), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_virty, virty, tol, "virty", __LINE__, __FILE__)

        call ops_conlt_par_puntbr( qww+1.0, disxx, disp, htot*3, hbron, dispg(istab), sigz0, sigz, virty)
        call assertEqual( ref_sigz(2), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_virty, virty, tol, "virty", __LINE__, __FILE__)

        sigz0 = 20.0
        call ops_conlt_par_puntbr( qww, disxx, disp, htot, hbron, dispg(istab), sigz0, sigz, virty)
        call assertEqual( sqrt(ref_sigz(1)**2 + sigz0**2), sigz, tol, "sigz", __LINE__, __FILE__)
        call assertEqual( ref_virty, virty, tol, "virty", __LINE__, __FILE__)

   end subroutine test_ops_conlt_par_puntbr
end module m_test_m_ops_conltexp
 
program p_test_ops_conlt_par_puntbr
use m_test_m_ops_conltexp
use no_pfunit
implicit none
   call test_ops_conlt_par_oppbr()
   call test_ops_conlt_par_puntbr()
   call test_ops_conltexp()
   call conclusion()
end program p_test_ops_conlt_par_puntbr
