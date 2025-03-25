module m_test_ops_resist_rek
implicit none
contains
   subroutine test_ops_resist_rek4()
   use m_ops_resist_rek
   use no_pfunit_ops_lt
   use m_ops_vchem, only: tvChem
   use m_ops_varin, only: Tvarin_meteo, Tvarin_unc

        TYPE(Tvarin_meteo) :: varin_meteo ! input variables for meteo
        type(Tvarin_unc) :: varin_unc
        real, parameter :: tol = 1e-5
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: vchemc =  -9.99900000E+03
        integer, parameter :: iopt_vchem = 0
        real, parameter :: vchemv =  -9.99900000E+03
        real, parameter :: rad =   9.25333252E+01
        logical, parameter :: isec = .true.
        integer, parameter :: icm = 2, icm5 = 5
        real, parameter :: rc_so2_ms =   6.55333252E+01
        real, parameter :: regenk =   1.70000009E-02
        real, parameter :: rc_aer_ms =   3.99799988E+02
        integer, parameter :: iseiz = 1
        integer, parameter :: istab = 1
        integer, parameter :: itra = 1
        integer, parameter :: isec_prelim = 6
        real, parameter :: ar =   6.19999980E-07
        real, parameter :: r_no2_nox_sec(12) = (/  -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00, &
            -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00, &
            -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00/)
        real, parameter :: r_no2_nox_season =   6.49999976E-01
        integer, parameter :: ibroncat = 1
        integer, parameter :: nemcat_road = 0
		logical, parameter :: road_chem = .false.
        integer, parameter :: emcat_road(199) = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/)
        real, parameter :: vchemnh3 =   0.00000000E+00
        type(tvChem) :: vchem2
        real, parameter :: hum =   7.58666534E+01
        real, parameter :: uster_rcp =   2.39550680E-01
        real, parameter :: ol_rcp =  -2.59285927E+01
        real, parameter :: uster_tra =   2.52821952E-01
        real, parameter :: z0_rcp =   2.00000003E-01
        real, parameter :: z0_metreg_rcp =   2.89999992E-01
        integer, parameter :: kdeel = 1
        integer, parameter :: mb = 1
        real, parameter :: vw10 =   2.01143265E+00
        real, parameter :: temp_C =   1.25771332E+01
        real, parameter :: zm =   4.00000000E+00
        real, parameter :: koh =   9.18000000E+02
        real, parameter :: rations =   0.00000000E+00
        real, parameter :: rhno3_trj =   1.61107481E-01
        real, parameter :: rc_no =   2.00000000E+03
        real, parameter :: rhno2 =   2.99999993E-02
        real, parameter :: rc_hno3 =   1.00000000E+01
        real, parameter :: croutpri =   2.00000000E+04
        real, parameter :: r_no2_nox_year_bg_tra =   9.00318980E-01
        real, parameter :: rhno3_rcp =   1.90969050E-01
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: ol_src =  -3.29010468E+01
        real, parameter :: uster_src =   2.52821952E-01
        real, parameter :: z0_tra =   2.00000003E-01
        real, parameter :: nh3bg_rcp =   0.00000000E+00
        real, parameter :: nh3bgtra =   1.32029724E+01
        real, parameter :: gw_rcp =   0.00000000E+00
        real, parameter :: gwtra =   0.00000000E+00
        real, parameter :: so2bg_rcp =   0.00000000E+00
        real, parameter :: so2bgtra =   0.00000000E+00
        real, parameter :: gym =   5.15891228E+01
        logical, parameter :: gasv = .true.
        real, parameter :: lu_rcp_per(9) = (/   1.00000000E+02,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00/)
        real, parameter :: lu_tra_per(9) = (/   1.00000000E+02,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00/)
        real, parameter :: rc_user =   0.00000000E+00
        real, parameter :: ra_ms_4 =   2.01143246E+01
        real, parameter :: ra_ms_zra =   3.24400864E+01
        real, parameter :: in_rb_ms =   2.72685947E+01
        real, parameter :: in_rc_sec_rcp =  -3.58125571E-18
        logical, parameter :: in_depudone = .false.
        logical, parameter :: in_lroad_corr = .false.
        real, parameter :: in_ra_src_4 =   1.12103877E-44
        real, parameter :: in_rb_src =   1.85252151E-25
        real, parameter :: in_ra_trj_4 =   0.00000000E+00
        real, parameter :: in_ra_trj_zra =   2.17059991E+20
        real, parameter :: in_rb_trj =   3.29853686E+28
        real, parameter :: in_ra_rcp_zrcp =   0.00000000E+00
        real, parameter :: in_ra_rcp_4 =   0.00000000E+00
        real, parameter :: in_ra_rcp_zra =  -3.58125571E-18
        real, parameter :: in_rb_rcp =   0.00000000E+00
        real, parameter :: in_routpri =   0.00000000E+00
        real, parameter :: in_r_no2_nox =   3.91322437E-34
        real, parameter :: in_rc_sec_trj =   1.54488945E+00
        real :: rb_ms
        real, parameter :: ref_rb_ms =   2.72685947E+01
        logical :: depudone
        logical, parameter :: ref_depudone = .true.
        real :: ra_src_4
        real, parameter :: ref_ra_src_4 =   2.40340176E+01
        real :: rb_src
        real, parameter :: ref_rb_src =   2.85675869E+01
        real :: ra_trj_4
        real, parameter :: ref_ra_trj_4 =   2.29623680E+01
        real :: ra_trj_zra
        real, parameter :: ref_ra_trj_zra =   3.47806282E+01
        real :: rb_trj
        real, parameter :: ref_rb_trj =   2.85675850E+01
        real :: ra_rcp_zrcp
        real, parameter :: ref_ra_rcp_zrcp =   2.43975487E+01
        real :: ra_rcp_4
        real, parameter :: ref_ra_rcp_4 =   2.43975487E+01
        real :: ra_rcp_zra
        real, parameter :: ref_ra_rcp_zra =   3.31806717E+01
        real :: rb_rcp
        real, parameter :: ref_rb_rcp =   3.01502495E+01
        real :: routpri
        real, parameter :: ref_routpri =   1.17041465E+04
        real :: vchem
        real, parameter :: ref_vchem =   3.08206844E+00
        real :: uh
        real, parameter :: ref_uh =   2.01143265E+00
        real :: r_no2_nox
        real, parameter :: ref_r_no2_nox =   5.85207343E-01
        logical :: lroad_corr
        logical, parameter :: ref_lroad_corr = .false.
        real :: rc_eff_src_4_pos
        real, parameter :: ref_rc_eff_src_4_pos =   6.59993958E+02
        real :: rc_eff_trj_4_pos
        real, parameter :: ref_rc_eff_trj_4_pos =   6.59993958E+02
        real :: rc_eff_rcp_4_pos
        real, parameter :: ref_rc_eff_rcp_4_pos =   6.59993835E+02
        real :: rc_eff_rcp_4
        real, parameter :: ref_rc_eff_rcp_4 =   6.59993835E+02
        real :: rc_sec_trj
        real, parameter :: ref_rc_sec_trj =   1.66130997E+02
        real :: rc_sec_rcp
        real, parameter :: ref_rc_sec_rcp =   1.37478745E+02
        vchem =   0.00000000E+00
        call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_prec_grid", vchem2%mass_prec_grid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_conv_dtfac_grid", vchem2%mass_conv_dtfac_grid, __LINE__, __FILE__)
        rb_ms = in_rb_ms
        rc_sec_rcp = in_rc_sec_rcp
        depudone = in_depudone
        lroad_corr = in_lroad_corr
        ra_src_4 = in_ra_src_4
        rb_src = in_rb_src
        ra_trj_4 = in_ra_trj_4
        ra_trj_zra = in_ra_trj_zra
        rb_trj = in_rb_trj
        ra_rcp_zrcp = in_ra_rcp_zrcp
        ra_rcp_4 = in_ra_rcp_4
        ra_rcp_zra = in_ra_rcp_zra
        rb_rcp = in_rb_rcp
        routpri = in_routpri
        r_no2_nox = in_r_no2_nox
        call ops_resist_rek(varin_meteo, varin_unc, disxx, vchemc, iopt_vchem, vchemv, rad, isec, icm, rc_so2_ms, regenk, rc_aer_ms, iseiz, istab, itra, isec_prelim, ar, &
                          r_no2_nox_season, ibroncat, nemcat_road, road_chem, emcat_road, vchemnh3, hum, uster_rcp, ol_rcp, uster_tra, &
                          z0_rcp, z0_metreg_rcp, kdeel, mb, vw10, temp_C, zm, koh, &
                          rations, rhno3_trj, rc_no, rhno2, rc_hno3, croutpri, r_no2_nox_sec, r_no2_nox_year_bg_tra, rhno3_rcp, &
                          rb_ms, ra_ms_4, ra_ms_zra, rc_user, routpri, vchem, rc_sec_trj, uh, rc_sec_rcp, rc_eff_rcp_4_pos, rb_rcp, &
                          ra_rcp_4, ra_rcp_zra, ra_rcp_zrcp, z0_src, ol_src, uster_src, z0_tra, rc_eff_trj_4_pos, rc_eff_src_4_pos, ra_src_4, &
                          rb_src, ra_trj_4, ra_trj_zra, rb_trj, rc_eff_rcp_4, nh3bg_rcp, nh3bgtra, &
                          so2bg_rcp, so2bgtra, gw_rcp, gwtra, gym, depudone, gasv, lu_rcp_per, lu_tra_per, r_no2_nox, lroad_corr)
        call assertEqual( ref_rb_ms, rb_ms, tol, "rb_ms", __LINE__, __FILE__)
        call assertEqual( ref_depudone, depudone, "depudone", __LINE__, __FILE__)
        call assertEqual( ref_ra_src_4, ra_src_4, tol, "ra_src_4", __LINE__, __FILE__)
        call assertEqual( ref_rb_src, rb_src, tol, "rb_src", __LINE__, __FILE__)
        call assertEqual( ref_ra_trj_4, ra_trj_4, tol, "ra_trj_4", __LINE__, __FILE__)
        call assertEqual( ref_ra_trj_zra, ra_trj_zra, tol, "ra_trj_zra", __LINE__, __FILE__)
        call assertEqual( ref_rb_trj, rb_trj, tol, "rb_trj", __LINE__, __FILE__)
        call assertEqual( ref_ra_rcp_zrcp, ra_rcp_zrcp, tol, "ra_rcp_zrcp", __LINE__, __FILE__)
        call assertEqual( ref_ra_rcp_4, ra_rcp_4, tol, "ra_rcp_4", __LINE__, __FILE__)
        call assertEqual( ref_ra_rcp_zra, ra_rcp_zra, tol, "ra_rcp_zra", __LINE__, __FILE__)
        call assertEqual( ref_rb_rcp, rb_rcp, tol, "rb_rcp", __LINE__, __FILE__)
        call assertEqual( ref_routpri, routpri, tol, "routpri", __LINE__, __FILE__)
        call assertEqual( ref_vchem, vchem, tol, "vchem", __LINE__, __FILE__)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_r_no2_nox, r_no2_nox, tol, "r_no2_nox", __LINE__, __FILE__)
        call assertEqual( ref_lroad_corr, lroad_corr, "lroad_corr", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_eff_src_4_pos, rc_eff_src_4_pos, tol, "rc_eff_src_4_pos", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_eff_trj_4_pos, rc_eff_trj_4_pos, tol, "rc_eff_trj_4_pos", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_eff_rcp_4_pos, rc_eff_rcp_4_pos, tol, "rc_eff_rcp_4_pos", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_eff_rcp_4, rc_eff_rcp_4, tol, "rc_eff_rcp_4", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_sec_trj, rc_sec_trj, tol, "rc_sec_trj", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_sec_rcp, rc_sec_rcp, tol, "rc_sec_rcp", __LINE__, __FILE__)
   end subroutine test_ops_resist_rek4
   
   subroutine test_ops_resist_rek3()
   use m_ops_resist_rek
   use no_pfunit_ops_lt
   use m_ops_vchem, only: tvChem
   use m_ops_varin, only: Tvarin_meteo, Tvarin_unc

        TYPE(Tvarin_meteo) :: varin_meteo ! input variables for meteo
        type(Tvarin_unc) :: varin_unc
        real, parameter :: tol = 1e-5
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: vchemc =  -9.99900000E+03
        integer, parameter :: iopt_vchem = 0
        real, parameter :: vchemv =  -9.99900000E+03
        real, parameter :: rad =   9.25333252E+01
        logical, parameter :: isec = .true.
        integer, parameter :: icm = 1
        real, parameter :: rc_so2_ms =   6.55333252E+01
        real, parameter :: regenk =   1.70000009E-02
        real, parameter :: rc_aer_ms =   3.99799988E+02
        integer, parameter :: iseiz = 1
        integer, parameter :: istab = 1
        integer, parameter :: itra = 1
        integer, parameter :: isec_prelim = 6
        real, parameter :: ar =   0.00000000E+00
        real, parameter :: r_no2_nox_sec(12) = (/  -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00, &
            -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00, &
            -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00/)
        real, parameter :: r_no2_nox_season =   0.00000000E+00
        integer, parameter :: ibroncat = 1
        integer, parameter :: nemcat_road = 0
		logical, parameter :: road_chem = .false.
        integer, parameter :: emcat_road(199) = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/)
        real, parameter :: vchemnh3 =   0.00000000E+00
        type(tvChem) :: vchem2
        real, parameter :: hum =   7.58666534E+01
        real, parameter :: uster_rcp =   2.39550680E-01
        real, parameter :: ol_rcp =  -2.59285927E+01
        real, parameter :: uster_tra =   2.52821952E-01
        real, parameter :: z0_rcp =   2.00000003E-01
        real, parameter :: z0_metreg_rcp =   2.89999992E-01
        integer, parameter :: kdeel = 1
        integer, parameter :: mb = 1
        real, parameter :: vw10 =   2.01143265E+00
        real, parameter :: temp_C =   1.25771332E+01
        real, parameter :: zm =   4.00000000E+00
        real, parameter :: koh =   0.00000000E+00
        real, parameter :: rations =   4.48240423E+00
        real, parameter :: rhno3_trj =   0.00000000E+00
        real, parameter :: rc_no =   0.00000000E+00
        real, parameter :: rhno2 =   0.00000000E+00
        real, parameter :: rc_hno3 =   0.00000000E+00
        real, parameter :: croutpri =   1.00000000E+05
        real, parameter :: r_no2_nox_year_bg_tra =   0.00000000E+00
        real, parameter :: rhno3_rcp =   0.00000000E+00
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: ol_src =  -3.29010468E+01
        real, parameter :: uster_src =   2.52821952E-01
        real, parameter :: z0_tra =   2.00000003E-01
        real, parameter :: nh3bg_rcp =   6.35440063E+00
        real, parameter :: nh3bgtra =   1.32029724E+01
        real, parameter :: gw_rcp =   0.00000000E+00
        real, parameter :: gwtra =   0.00000000E+00
        real, parameter :: so2bg_rcp =   3.08429980E+00
        real, parameter :: so2bgtra =   1.09907138E+00
        real, parameter :: gym =   5.15891228E+01
        logical, parameter :: gasv = .true.
        real, parameter :: lu_rcp_per(9) = (/   1.00000000E+02,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00/)
        real, parameter :: lu_tra_per(9) = (/   1.00000000E+02,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00/)
        real, parameter :: rc_user =   0.00000000E+00
        real, parameter :: ra_ms_4 =   2.01143246E+01
        real, parameter :: ra_ms_zra =   3.24400864E+01
        real, parameter :: in_rb_ms =   2.72685947E+01
        real, parameter :: in_rc_sec_rcp =  -3.58125571E-18
        logical, parameter :: in_depudone = .false.
        logical, parameter :: in_lroad_corr = .false.
        real, parameter :: in_ra_src_4 =   1.12103877E-44
        real, parameter :: in_rb_src =   3.85347508E-25
        real, parameter :: in_ra_trj_4 =   0.00000000E+00
        real, parameter :: in_ra_trj_zra =  -1.56009759E+35
        real, parameter :: in_rb_trj =   1.13618843E+20
        real, parameter :: in_ra_rcp_zrcp =   0.00000000E+00
        real, parameter :: in_ra_rcp_4 =   0.00000000E+00
        real, parameter :: in_ra_rcp_zra =  -3.58125571E-18
        real, parameter :: in_rb_rcp =   0.00000000E+00
        real, parameter :: in_routpri =   0.00000000E+00
        real, parameter :: in_r_no2_nox =   3.91322437E-34
        real, parameter :: in_rc_sec_trj =  -1.08482666E+01
        real :: rb_ms
        real, parameter :: ref_rb_ms =   2.72685947E+01
        logical :: depudone
        logical, parameter :: ref_depudone = .true.
        real :: ra_src_4
        real, parameter :: ref_ra_src_4 =   2.40340176E+01
        real :: rb_src
        real, parameter :: ref_rb_src =   2.85675869E+01
        real :: ra_trj_4
        real, parameter :: ref_ra_trj_4 =   2.29623680E+01
        real :: ra_trj_zra
        real, parameter :: ref_ra_trj_zra =   3.47806282E+01
        real :: rb_trj
        real, parameter :: ref_rb_trj =   2.85675850E+01
        real :: ra_rcp_zrcp
        real, parameter :: ref_ra_rcp_zrcp =   2.43975487E+01
        real :: ra_rcp_4
        real, parameter :: ref_ra_rcp_4 =   2.43975487E+01
        real :: ra_rcp_zra
        real, parameter :: ref_ra_rcp_zra =   3.31806717E+01
        real :: rb_rcp
        real, parameter :: ref_rb_rcp =   3.01502495E+01
        real :: routpri
        real, parameter :: ref_routpri =   4.48240438E+05
        real :: vchem
        real, parameter :: ref_vchem =   2.62144017E+00
        real :: uh
        real, parameter :: ref_uh =   2.01143265E+00
        real :: r_no2_nox
        real, parameter :: ref_r_no2_nox =   3.91322437E-34
        logical :: lroad_corr
        logical, parameter :: ref_lroad_corr = .false.
        real :: rc_eff_src_4_pos
        real, parameter :: ref_rc_eff_src_4_pos =   1.14446976E+02
        real :: rc_eff_trj_4_pos
        real, parameter :: ref_rc_eff_trj_4_pos =   1.14446976E+02
        real :: rc_eff_rcp_4_pos
        real, parameter :: ref_rc_eff_rcp_4_pos =   1.14446945E+02
        real :: rc_eff_rcp_4
        real, parameter :: ref_rc_eff_rcp_4 =   1.14446945E+02
        real :: rc_sec_trj
        real, parameter :: ref_rc_sec_trj =   3.19839996E+02
        real :: rc_sec_rcp
        real, parameter :: ref_rc_sec_rcp =   3.41298981E+02
        call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_prec_grid", vchem2%mass_prec_grid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_conv_dtfac_grid", vchem2%mass_conv_dtfac_grid, __LINE__, __FILE__)
        rb_ms = in_rb_ms
        rc_sec_rcp = in_rc_sec_rcp
        depudone = in_depudone
        lroad_corr = in_lroad_corr
        ra_src_4 = in_ra_src_4
        rb_src = in_rb_src
        ra_trj_4 = in_ra_trj_4
        ra_trj_zra = in_ra_trj_zra
        rb_trj = in_rb_trj
        ra_rcp_zrcp = in_ra_rcp_zrcp
        ra_rcp_4 = in_ra_rcp_4
        ra_rcp_zra = in_ra_rcp_zra
        rb_rcp = in_rb_rcp
        routpri = in_routpri
        r_no2_nox = in_r_no2_nox
        call ops_resist_rek(varin_meteo, varin_unc, disxx, vchemc, iopt_vchem, vchemv, rad, isec, icm, rc_so2_ms, regenk, rc_aer_ms, iseiz, istab, itra, isec_prelim, ar, &
                          r_no2_nox_season, ibroncat, nemcat_road, road_chem, emcat_road, vchemnh3, hum, uster_rcp, ol_rcp, uster_tra, &
                          z0_rcp, z0_metreg_rcp, kdeel, mb, vw10, temp_C, zm, koh, &
                          rations, rhno3_trj, rc_no, rhno2, rc_hno3, croutpri, r_no2_nox_sec, r_no2_nox_year_bg_tra, rhno3_rcp, &
                          rb_ms, ra_ms_4, ra_ms_zra, rc_user, routpri, vchem, rc_sec_trj, uh, rc_sec_rcp, rc_eff_rcp_4_pos, rb_rcp, &
                          ra_rcp_4, ra_rcp_zra, ra_rcp_zrcp, z0_src, ol_src, uster_src, z0_tra, rc_eff_trj_4_pos, rc_eff_src_4_pos, ra_src_4, &
                          rb_src, ra_trj_4, ra_trj_zra, rb_trj, rc_eff_rcp_4, nh3bg_rcp, nh3bgtra, &
                          so2bg_rcp, so2bgtra, gw_rcp, gwtra, gym, depudone, gasv, lu_rcp_per, lu_tra_per, r_no2_nox, lroad_corr)
        call assertEqual( ref_rb_ms, rb_ms, tol, "rb_ms", __LINE__, __FILE__)
        call assertEqual( ref_depudone, depudone, "depudone", __LINE__, __FILE__)
        call assertEqual( ref_ra_src_4, ra_src_4, tol, "ra_src_4", __LINE__, __FILE__)
        call assertEqual( ref_rb_src, rb_src, tol, "rb_src", __LINE__, __FILE__)
        call assertEqual( ref_ra_trj_4, ra_trj_4, tol, "ra_trj_4", __LINE__, __FILE__)
        call assertEqual( ref_ra_trj_zra, ra_trj_zra, tol, "ra_trj_zra", __LINE__, __FILE__)
        call assertEqual( ref_rb_trj, rb_trj, tol, "rb_trj", __LINE__, __FILE__)
        call assertEqual( ref_ra_rcp_zrcp, ra_rcp_zrcp, tol, "ra_rcp_zrcp", __LINE__, __FILE__)
        call assertEqual( ref_ra_rcp_4, ra_rcp_4, tol, "ra_rcp_4", __LINE__, __FILE__)
        call assertEqual( ref_ra_rcp_zra, ra_rcp_zra, tol, "ra_rcp_zra", __LINE__, __FILE__)
        call assertEqual( ref_rb_rcp, rb_rcp, tol, "rb_rcp", __LINE__, __FILE__)
        call assertEqual( ref_routpri, routpri, tol, "routpri", __LINE__, __FILE__)
        call assertEqual( ref_vchem, vchem, tol, "vchem", __LINE__, __FILE__)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_r_no2_nox, r_no2_nox, tol, "r_no2_nox", __LINE__, __FILE__)
        call assertEqual( ref_lroad_corr, lroad_corr, "lroad_corr", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_eff_src_4_pos, rc_eff_src_4_pos, tol, "rc_eff_src_4_pos", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_eff_trj_4_pos, rc_eff_trj_4_pos, tol, "rc_eff_trj_4_pos", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_eff_rcp_4_pos, rc_eff_rcp_4_pos, tol, "rc_eff_rcp_4_pos", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_eff_rcp_4, rc_eff_rcp_4, tol, "rc_eff_rcp_4", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_sec_trj, rc_sec_trj, tol, "rc_sec_trj", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_sec_rcp, rc_sec_rcp, tol, "rc_sec_rcp", __LINE__, __FILE__)
   end subroutine test_ops_resist_rek3

   subroutine test_ops_resist_rek2()
   use m_ops_resist_rek
   use no_pfunit_ops_lt
   use m_ops_vchem, only: tvChem
   use m_ops_varin, only: Tvarin_meteo, Tvarin_unc

        TYPE(Tvarin_meteo) :: varin_meteo ! input variables for meteo
        type(Tvarin_unc) :: varin_unc
        real, parameter :: tol = 1e-5
        real, parameter :: disxx =   1.63008297E+05
        real, parameter :: vchemc =  -9.99900000E+03
        integer, parameter :: iopt_vchem = 0
        real, parameter :: vchemv =  -9.99900000E+03
        real, parameter :: rad =   2.55666656E+01
        logical, parameter :: isec = .true.
        integer, parameter :: icm = 3
        real, parameter :: rc_so2_ms =   3.31000023E+01
        real, parameter :: regenk =   5.60666583E-02
        real, parameter :: rc_aer_ms =   1.50363318E+03
        integer, parameter :: iseiz = 1
        integer, parameter :: istab = 1
        integer, parameter :: itra = 3
        integer, parameter :: isec_prelim = 8
        real, parameter :: ar =   0.00000000E+00
        real, parameter :: r_no2_nox_sec(12) = (/  -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00, &
            -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00, &
            -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00/)
        real, parameter :: r_no2_nox_season =   0.00000000E+00
        integer, parameter :: ibroncat = 1
        integer, parameter :: nemcat_road = 0
		logical, parameter :: road_chem = .false.
        integer, parameter :: emcat_road(199) = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/)
        real, parameter :: vchemnh3 =   5.88523722E+00
        type(tvChem) :: vchem2
        real, parameter :: hum =   8.77999954E+01
        real, parameter :: uster_rcp =   3.16434801E-01
        real, parameter :: ol_rcp =  -5.84130920E+02
        real, parameter :: uster_tra =   3.12583953E-01
        real, parameter :: z0_rcp =   5.27899981E-01
        real, parameter :: z0_metreg_rcp =   2.89999992E-01
        integer, parameter :: kdeel = 1
        integer, parameter :: mb = 1
        real, parameter :: vw10 =   3.83380127E+00
        real, parameter :: temp_C =   9.70774746E+00
        real, parameter :: zm =   4.00000000E+00
        real, parameter :: koh =   0.00000000E+00
        real, parameter :: rations =   0.00000000E+00
        real, parameter :: rhno3_trj =   0.00000000E+00
        real, parameter :: rc_no =   0.00000000E+00
        real, parameter :: rhno2 =   0.00000000E+00
        real, parameter :: rc_hno3 =   0.00000000E+00
        real, parameter :: croutpri =   1.40000000E+06
        real, parameter :: r_no2_nox_year_bg_tra =   0.00000000E+00
        real, parameter :: rhno3_rcp =   0.00000000E+00
        real, parameter :: z0_src =   1.47459996E+00
        real, parameter :: ol_src =  -5.27021118E+02
        real, parameter :: uster_src =   5.48500597E-01
        real, parameter :: z0_tra =   7.75348321E-02
        real, parameter :: nh3bg_rcp =   1.37240016E+00
        real, parameter :: nh3bgtra =   6.57589293E+00
        real, parameter :: gw_rcp =   4.30E+02
        real, parameter :: gwtra =   4.30E+02
        real, parameter :: so2bg_rcp =   1.46279991E+00
        real, parameter :: so2bgtra =   6.25189245E-01
        real, parameter :: gym =   5.34971504E+01
        logical, parameter :: gasv = .true.
        real, parameter :: lu_rcp_per(9) = (/   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   1.80000000E+01, &
             5.70000000E+01,   7.00000000E+00,   0.00000000E+00,   1.30000000E+01,   5.00000000E+00/)
        real, parameter :: lu_tra_per(9) = (/   3.69047585E+01,   2.99999981E+01,   4.76190448E-02,   1.14285707E+00, &
             9.57142830E+00,   1.14761896E+01,   9.66666603E+00,   9.52380896E-01,   2.38095239E-01/)
        real, parameter :: rc_user =   0.00000000E+00
        real, parameter :: ra_ms_4 =   2.29225140E+01
        real, parameter :: ra_ms_zra =   4.67675400E+01
        real, parameter :: in_rb_ms =   2.46302624E+01
        real, parameter :: in_rc_sec_rcp =   5.85915405E+02
        logical, parameter :: in_depudone = .false.
        logical, parameter :: in_lroad_corr = .false.
        real, parameter :: in_ra_src_4 =   7.26074457E+00
        real, parameter :: in_rb_src =   1.34176807E+01
        real, parameter :: in_ra_trj_4 =   3.64636803E+01
        real, parameter :: in_ra_trj_zra =   6.23245087E+01
        real, parameter :: in_rb_trj =   5.89208984E+01
        real, parameter :: in_ra_rcp_zrcp =   2.00167561E+01
        real, parameter :: in_ra_rcp_4 =   2.00167561E+01
        real, parameter :: in_ra_rcp_zra =   3.26409569E+01
        real, parameter :: in_rb_rcp =   1.74143467E+01
        real, parameter :: in_routpri =   1.40000000E+06
        real, parameter :: in_r_no2_nox =   0.00000000E+00
        real, parameter :: in_rc_sec_trj =   4.87733307E+02
        real:: rc_sec_trj 
        real :: rb_ms
        real, parameter :: ref_rb_ms =   1.57633677E+01
        logical :: depudone
        logical, parameter :: ref_depudone = .true.
        real :: ra_src_4
        real, parameter :: ref_ra_src_4 =   4.38376093E+00
        real :: rb_src
        real, parameter :: ref_rb_src =   8.86075211E+00
        real :: ra_trj_4
        real, parameter :: ref_ra_trj_4 =   3.44455185E+01
        real :: ra_trj_zra
        real, parameter :: ref_ra_trj_zra =   5.87471085E+01
        real :: rb_trj
        real, parameter :: ref_rb_trj =   1.55482340E+01
        real :: ra_rcp_zrcp
        real, parameter :: ref_ra_rcp_zrcp =   1.56404619E+01
        real :: ra_rcp_4
        real, parameter :: ref_ra_rcp_4 =   1.56404619E+01
        real :: ra_rcp_zra
        real, parameter :: ref_ra_rcp_zra =   3.22384872E+01
        real :: rb_rcp
        real, parameter :: ref_rb_rcp =   1.53590193E+01
        real :: routpri
        real, parameter :: ref_routpri =   1.40000000E+06
        real :: vchem
        real, parameter :: ref_vchem =   5.88523722E+00
        real :: uh
        real, parameter :: ref_uh =   1.62426114E+00
        real :: r_no2_nox
        real, parameter :: ref_r_no2_nox =   0.00000000E+00
        logical :: lroad_corr
        logical, parameter :: ref_lroad_corr = .false.
        real :: rc_eff_src_4_pos
        real, parameter :: ref_rc_eff_src_4_pos =   34.4314956665
        real :: rc_eff_trj_4_pos
        real, parameter :: ref_rc_eff_trj_4_pos =   34.4314956665
        real :: rc_eff_rcp_4_pos
        real, parameter :: ref_rc_eff_rcp_4_pos =   6.84055424E+00
        real :: rc_eff_rcp_4
        real, parameter :: ref_rc_eff_rcp_4 =   6.84055424E+00
        real, parameter :: ref_rc_sec_trj =   1.20290662E+03
        real :: rc_sec_rcp
        real, parameter :: ref_rc_sec_rcp =   2.54730988E+02
        vchem =   0.00000000E+00
        call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_prec_grid", vchem2%mass_prec_grid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_conv_dtfac_grid", vchem2%mass_conv_dtfac_grid, __LINE__, __FILE__)
        rb_ms = in_rb_ms
        rc_sec_rcp = in_rc_sec_rcp
        depudone = in_depudone
        lroad_corr = in_lroad_corr
        ra_src_4 = in_ra_src_4
        rb_src = in_rb_src
        ra_trj_4 = in_ra_trj_4
        ra_trj_zra = in_ra_trj_zra
        rb_trj = in_rb_trj
        ra_rcp_zrcp = in_ra_rcp_zrcp
        ra_rcp_4 = in_ra_rcp_4
        ra_rcp_zra = in_ra_rcp_zra
        rb_rcp = in_rb_rcp
        routpri = in_routpri
        r_no2_nox = in_r_no2_nox
        call ops_resist_rek(varin_meteo, varin_unc, disxx, vchemc, iopt_vchem, vchemv, rad, isec, icm, rc_so2_ms, regenk, rc_aer_ms, iseiz, istab, itra, isec_prelim, ar, &
                          r_no2_nox_season, ibroncat, nemcat_road, road_chem, emcat_road, vchemnh3, hum, uster_rcp, ol_rcp, uster_tra, &
                          z0_rcp, z0_metreg_rcp, kdeel, mb, vw10, temp_C, zm, koh, &
                          rations, rhno3_trj, rc_no, rhno2, rc_hno3, croutpri, r_no2_nox_sec, r_no2_nox_year_bg_tra, rhno3_rcp, &
                          rb_ms, ra_ms_4, ra_ms_zra, rc_user, routpri, vchem, rc_sec_trj, uh, rc_sec_rcp, rc_eff_rcp_4_pos, rb_rcp, &
                          ra_rcp_4, ra_rcp_zra, ra_rcp_zrcp, z0_src, ol_src, uster_src, z0_tra, rc_eff_trj_4_pos, rc_eff_src_4_pos, ra_src_4, &
                          rb_src, ra_trj_4, ra_trj_zra, rb_trj, rc_eff_rcp_4, nh3bg_rcp, nh3bgtra, &
                          so2bg_rcp, so2bgtra, gw_rcp, gwtra, gym, depudone, gasv, lu_rcp_per, lu_tra_per, r_no2_nox, lroad_corr)
        call assertEqual( ref_rb_ms, rb_ms, tol, "rb_ms", __LINE__, __FILE__)
        call assertEqual( ref_depudone, depudone, "depudone", __LINE__, __FILE__)
        call assertEqual( ref_ra_src_4, ra_src_4, tol, "ra_src_4", __LINE__, __FILE__)
        call assertEqual( ref_rb_src, rb_src, tol, "rb_src", __LINE__, __FILE__)
        call assertEqual( ref_ra_trj_4, ra_trj_4, tol, "ra_trj_4", __LINE__, __FILE__)
        call assertEqual( ref_ra_trj_zra, ra_trj_zra, tol, "ra_trj_zra", __LINE__, __FILE__)
        call assertEqual( ref_rb_trj, rb_trj, tol, "rb_trj", __LINE__, __FILE__)
        call assertEqual( ref_ra_rcp_zrcp, ra_rcp_zrcp, tol, "ra_rcp_zrcp", __LINE__, __FILE__)
        call assertEqual( ref_ra_rcp_4, ra_rcp_4, tol, "ra_rcp_4", __LINE__, __FILE__)
        call assertEqual( ref_ra_rcp_zra, ra_rcp_zra, tol, "ra_rcp_zra", __LINE__, __FILE__)
        call assertEqual( ref_rb_rcp, rb_rcp, tol, "rb_rcp", __LINE__, __FILE__)
        call assertEqual( ref_routpri, routpri, tol, "routpri", __LINE__, __FILE__)
        call assertEqual( ref_vchem, vchem, tol, "vchem", __LINE__, __FILE__)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_r_no2_nox, r_no2_nox, tol, "r_no2_nox", __LINE__, __FILE__)
        call assertEqual( ref_lroad_corr, lroad_corr, "lroad_corr", __LINE__, __FILE__)
        call assertEqual( ref_rc_eff_src_4_pos, rc_eff_src_4_pos, tol, "rc_eff_src_4_pos", __LINE__, __FILE__)
        call assertEqual( ref_rc_eff_trj_4_pos, rc_eff_trj_4_pos, tol, "rc_eff_trj_4_pos", __LINE__, __FILE__)
        call assertEqual( ref_rc_eff_rcp_4_pos, rc_eff_rcp_4_pos, tol, "rc_eff_rcp_4_pos", __LINE__, __FILE__)
        call assertEqual( ref_rc_eff_rcp_4, rc_eff_rcp_4, tol, "rc_eff_rcp_4", __LINE__, __FILE__)
        call assertEqual( ref_rc_sec_trj, rc_sec_trj, tol, "rc_sec_trj", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_sec_rcp, rc_sec_rcp, tol, "rc_sec_rcp", __LINE__, __FILE__)
   end subroutine test_ops_resist_rek2

   subroutine test_ops_resist_rek()
   use m_ops_resist_rek
   use no_pfunit_ops_lt
   use m_ops_vchem, only: tvChem
   use m_ops_varin, only: Tvarin_meteo, Tvarin_unc

        TYPE(Tvarin_meteo) :: varin_meteo ! input variables for meteo
        type(Tvarin_unc) :: varin_unc
        real, parameter :: tol = 1e-5
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: vchemc =   0.00000000E+00
        integer, parameter :: iopt_vchem = 0
        real, parameter :: vchemv =   0.00000000E+00
        real, parameter :: rad =   9.25333252E+01
        logical, parameter :: isec = .false.
        integer, parameter :: icm = 4
        real, parameter :: rc_so2_ms =   6.55333252E+01
        real, parameter :: regenk =   1.70000009E-02
        real, parameter :: rc_aer_ms =   3.99799988E+02
        integer, parameter :: iseiz = 1
        integer, parameter :: istab = 1
        integer, parameter :: itra = 1
        integer, parameter :: isec_prelim = 6
        real, parameter :: ar =   0.00000000E+00
        real, parameter :: r_no2_nox_sec(12) = (/  -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00, &
            -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00, &
            -1.00000000E+00,  -1.00000000E+00,  -1.00000000E+00/)
        real, parameter :: r_no2_nox_season =   0.00000000E+00
        integer, parameter :: ibroncat = 1
        integer, parameter :: nemcat_road = 0
		logical, parameter :: road_chem = .false.
        integer, parameter :: emcat_road(199) = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/)
        real, parameter :: vchemnh3 =   0.00000000E+00
        type(tvChem) :: vchem2
        real, parameter :: hum =   7.58666534E+01
        real, parameter :: uster_rcp =   2.39550680E-01
        real, parameter :: ol_rcp =  -2.59285927E+01
        real, parameter :: uster_tra =   2.52821952E-01
        real, parameter :: z0_rcp =   2.00000003E-01
        real, parameter :: z0_metreg_rcp =   2.89999992E-01
        integer, parameter :: kdeel = 1
        integer, parameter :: mb = 1
        real, parameter :: vw10 =   2.01143265E+00
        real, parameter :: temp_C =   1.25771332E+01
        real, parameter :: zm =   4.00000000E+00
        real, parameter :: koh =   0.00000000E+00
        real, parameter :: rations =   0.00000000E+00
        real, parameter :: rhno3_trj =   0.00000000E+00
        real, parameter :: rc_no =   0.00000000E+00
        real, parameter :: rhno2 =   0.00000000E+00
        real, parameter :: rc_hno3 =   0.00000000E+00
        real, parameter :: croutpri =   0.00000000E+00
        real, parameter :: r_no2_nox_year_bg_tra =   0.00000000E+00
        real, parameter :: rhno3_rcp =   0.00000000E+00
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: ol_src =  -3.29010468E+01
        real, parameter :: uster_src =   2.52821952E-01
        real, parameter :: z0_tra =   2.00000003E-01
        real, parameter :: nh3bg_rcp =   0.00000000E+00
        real, parameter :: nh3bgtra =   0.00000000E+00
        real, parameter :: gw_rcp =   0.00000000E+00
        real, parameter :: gwtra =   0.00000000E+00
        real, parameter :: so2bg_rcp =   0.00000000E+00
        real, parameter :: so2bgtra =   0.00000000E+00
        real, parameter :: gym =   5.15891228E+01
        logical, parameter :: gasv = .true.
        real, parameter :: lu_rcp_per(9) = (/   1.00000000E+02,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00/)
        real, parameter :: lu_tra_per(9) = (/   1.00000000E+02,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00/)
        real, parameter :: rc_user =   1.30000000E+01
        real, parameter :: ra_ms_4 =   2.01143246E+01
        real, parameter :: ra_ms_zra =   3.24400864E+01
        real, parameter :: in_rb_ms =   2.72685947E+01
        logical, parameter :: in_depudone = .false.
        real, parameter :: in_ra_src_4 =   1.12103877E-44
        real, parameter :: in_rb_src =   7.76955416E-25
        real, parameter :: in_ra_trj_4 =   0.00000000E+00
        real, parameter :: in_ra_trj_zra =   7.57077942E-07
        real, parameter :: in_rb_trj =   5.49734150E+26
        real, parameter :: in_ra_rcp_zrcp =   0.00000000E+00
        real, parameter :: in_ra_rcp_4 =   0.00000000E+00
        real, parameter :: in_ra_rcp_zra =  -3.58125571E-18
        real, parameter :: in_rb_rcp =   0.00000000E+00
        real :: rb_ms
        real, parameter :: ref_rb_ms =   2.72685947E+01
        logical :: depudone
        logical, parameter :: ref_depudone = .true.
        real :: ra_src_4
        real, parameter :: ref_ra_src_4 =   2.40340176E+01
        real :: rb_src
        real, parameter :: ref_rb_src =   2.85675869E+01
        real :: ra_trj_4
        real, parameter :: ref_ra_trj_4 =   2.29623680E+01
        real :: ra_trj_zra
        real, parameter :: ref_ra_trj_zra =   3.47806282E+01
        real :: rb_trj
        real, parameter :: ref_rb_trj =   2.85675850E+01
        real :: ra_rcp_zrcp
        real, parameter :: ref_ra_rcp_zrcp =   2.43975487E+01
        real :: ra_rcp_4
        real, parameter :: ref_ra_rcp_4 =   2.43975487E+01
        real :: ra_rcp_zra
        real, parameter :: ref_ra_rcp_zra =   3.31806717E+01
        real :: rb_rcp
        real, parameter :: ref_rb_rcp =   3.01502495E+01
        real :: routpri
        real, parameter :: ref_routpri =   1.00000000E+06
        real :: vchem
        real, parameter :: ref_vchem =   0.00000000E+00
        real :: uh
        real, parameter :: ref_uh =   2.01143265E+00
        real :: r_no2_nox
        real, parameter :: ref_r_no2_nox =   3.91322437E-34
        logical :: lroad_corr
        logical, parameter :: ref_lroad_corr = .false.
        real :: rc_eff_src_4_pos
        real, parameter :: ref_rc_eff_src_4_pos =   1.30000000E+01
        real :: rc_eff_trj_4_pos
        real, parameter :: ref_rc_eff_trj_4_pos =   1.30000000E+01
        real :: rc_eff_rcp_4_pos
        real, parameter :: ref_rc_eff_rcp_4_pos =   1.30000000E+01
        real :: rc_eff_rcp_4
        real, parameter :: ref_rc_eff_rcp_4 =   1.30000000E+01
        real :: rc_sec_trj
        real, parameter :: ref_rc_sec_trj =   6.27125122E+20
        real :: rc_sec_rcp
        real, parameter :: ref_rc_sec_rcp =  -3.58125571E-18
        vchem =   0.00000000E+00
        call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_prec_grid", vchem2%mass_prec_grid, __LINE__, __FILE__)
        call InitAps("./level_1/resources/mass_grid_1.real", "Init mass_conv_dtfac_grid", vchem2%mass_conv_dtfac_grid, __LINE__, __FILE__)
        rb_ms = in_rb_ms
        depudone = in_depudone
        ra_src_4 = in_ra_src_4
        rb_src = in_rb_src
        ra_trj_4 = in_ra_trj_4
        ra_trj_zra = in_ra_trj_zra
        rb_trj = in_rb_trj
        ra_rcp_zrcp = in_ra_rcp_zrcp
        ra_rcp_4 = in_ra_rcp_4
        ra_rcp_zra = in_ra_rcp_zra
        rb_rcp = in_rb_rcp
        routpri = ref_routpri
        r_no2_nox = ref_r_no2_nox 
        rc_sec_trj =  ref_rc_sec_trj
        lroad_corr = ref_lroad_corr
        rc_sec_rcp = ref_rc_sec_rcp  
        call ops_resist_rek(varin_meteo, varin_unc, disxx, vchemc, iopt_vchem, vchemv, rad, isec, icm, rc_so2_ms, regenk, rc_aer_ms, iseiz, istab, itra, isec_prelim, ar, &
                          r_no2_nox_season, ibroncat, nemcat_road, road_chem, emcat_road, vchemnh3, hum, uster_rcp, ol_rcp, uster_tra, &
                          z0_rcp, z0_metreg_rcp, kdeel, mb, vw10, temp_C, zm, koh, &
                          rations, rhno3_trj, rc_no, rhno2, rc_hno3, croutpri, r_no2_nox_sec, r_no2_nox_year_bg_tra, rhno3_rcp, &
                          rb_ms, ra_ms_4, ra_ms_zra, rc_user, routpri, vchem, rc_sec_trj, uh, rc_sec_rcp, rc_eff_rcp_4_pos, rb_rcp, &
                          ra_rcp_4, ra_rcp_zra, ra_rcp_zrcp, z0_src, ol_src, uster_src, z0_tra, rc_eff_trj_4_pos, rc_eff_src_4_pos, ra_src_4, &
                          rb_src, ra_trj_4, ra_trj_zra, rb_trj, rc_eff_rcp_4, nh3bg_rcp, nh3bgtra, &
                          so2bg_rcp, so2bgtra, gw_rcp, gwtra, gym, depudone, gasv, lu_rcp_per, lu_tra_per, r_no2_nox, lroad_corr)
        call assertEqual( ref_rb_ms, rb_ms, tol, "rb_ms", __LINE__, __FILE__)
        call assertEqual( ref_depudone, depudone, "depudone", __LINE__, __FILE__)
        call assertEqual( ref_ra_src_4, ra_src_4, tol, "ra_src_4", __LINE__, __FILE__)
        call assertEqual( ref_rb_src, rb_src, tol, "rb_src", __LINE__, __FILE__)
        call assertEqual( ref_ra_trj_4, ra_trj_4, tol, "ra_trj_4", __LINE__, __FILE__)
        call assertEqual( ref_ra_trj_zra, ra_trj_zra, tol, "ra_trj_zra", __LINE__, __FILE__)
        call assertEqual( ref_rb_trj, rb_trj, tol, "rb_trj", __LINE__, __FILE__)
        call assertEqual( ref_ra_rcp_zrcp, ra_rcp_zrcp, tol, "ra_rcp_zrcp", __LINE__, __FILE__)
        call assertEqual( ref_ra_rcp_4, ra_rcp_4, tol, "ra_rcp_4", __LINE__, __FILE__)
        call assertEqual( ref_ra_rcp_zra, ra_rcp_zra, tol, "ra_rcp_zra", __LINE__, __FILE__)
        call assertEqual( ref_rb_rcp, rb_rcp, tol, "rb_rcp", __LINE__, __FILE__)
        call assertEqual( ref_routpri, routpri, tol, "routpri", __LINE__, __FILE__)
        call assertEqual( ref_vchem, vchem, tol, "vchem", __LINE__, __FILE__)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_r_no2_nox, r_no2_nox, tol, "r_no2_nox", __LINE__, __FILE__)
        call assertEqual( ref_lroad_corr, lroad_corr, "lroad_corr", __LINE__, __FILE__)
        call assertEqual( ref_rc_eff_src_4_pos, rc_eff_src_4_pos, tol, "rc_eff_src_4_pos", __LINE__, __FILE__)
        call assertEqual( ref_rc_eff_trj_4_pos, rc_eff_trj_4_pos, tol, "rc_eff_trj_4_pos", __LINE__, __FILE__)
        call assertEqual( ref_rc_eff_rcp_4_pos, rc_eff_rcp_4_pos, tol, "rc_eff_rcp_4_pos", __LINE__, __FILE__)
        call assertEqual( ref_rc_eff_rcp_4, rc_eff_rcp_4, tol, "rc_eff_rcp_4", __LINE__, __FILE__)
        call assertEqual( ref_rc_sec_trj, rc_sec_trj, tol, "rc_sec_trj", __LINE__, __FILE__)
        call assertEqual( ref_rc_sec_rcp, rc_sec_rcp, tol, "rc_sec_rcp", __LINE__, __FILE__)
   end subroutine test_ops_resist_rek

   subroutine test_ops_resist_ra_all()
   use m_ops_resist_rek
   use no_pfunit_ops_lt

        real, parameter :: tol = 1e-5
        real, parameter :: zm =   4.00000000E+00
        real, parameter :: zra =   5.00000000E+01
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: z0_tra =   2.00000003E-01
        real, parameter :: z0_rcp =   2.00000003E-01
        real, parameter :: z0_metreg_rcp =   2.89999992E-01
        real, parameter :: ol_src =  -3.29010468E+01
        real, parameter :: ol_rcp =  -2.59285927E+01
        real, parameter :: uster_src =   2.52821952E-01
        real, parameter :: uster_rcp =   2.39550680E-01
        real, parameter :: ra_ms_4 =   2.01143246E+01
        real, parameter :: ra_ms_zra =   3.24400864E+01
        real :: ra_src_4
        real, parameter :: ref_ra_src_4 =   2.40340176E+01
        real :: ra_trj_4
        real, parameter :: ref_ra_trj_4 =   2.29623680E+01
        real :: ra_trj_zra
        real, parameter :: ref_ra_trj_zra =   3.47806282E+01
        real :: ra_rcp_zrcp
        real, parameter :: ref_ra_rcp_zrcp =   2.43975487E+01
        real :: ra_rcp_4
        real, parameter :: ref_ra_rcp_4 =   2.43975487E+01
        real :: ra_rcp_zra
        real, parameter :: ref_ra_rcp_zra =   3.31806717E+01
        call ops_resist_ra_all( zm, zra, z0_src, z0_tra, z0_rcp, z0_metreg_rcp, ol_src, ol_rcp, uster_src, &
            uster_rcp, ra_ms_4, ra_ms_zra, ra_src_4, ra_trj_4, ra_trj_zra, ra_rcp_zrcp, ra_rcp_4, ra_rcp_zra)
        call assertEqual( ref_ra_src_4, ra_src_4, tol, "ra_src_4", __LINE__, __FILE__)
        call assertEqual( ref_ra_trj_4, ra_trj_4, tol, "ra_trj_4", __LINE__, __FILE__)
        call assertEqual( ref_ra_trj_zra, ra_trj_zra, tol, "ra_trj_zra", __LINE__, __FILE__)
        call assertEqual( ref_ra_rcp_zrcp, ra_rcp_zrcp, tol, "ra_rcp_zrcp", __LINE__, __FILE__)
        call assertEqual( ref_ra_rcp_4, ra_rcp_4, tol, "ra_rcp_4", __LINE__, __FILE__)
        call assertEqual( ref_ra_rcp_zra, ra_rcp_zra, tol, "ra_rcp_zra", __LINE__, __FILE__)
   end subroutine test_ops_resist_ra_all

   subroutine test_ops_resist_rb_all()
   use m_ops_resist_rek
   use no_pfunit_ops_lt
       real, parameter :: tol = 1e-5
        integer, parameter :: icm = 4, icm3=3
        real, parameter :: uster_src =   2.52821952E-01
        real, parameter :: uster_tra =   2.52821952E-01
        real, parameter :: uster_rcp =   2.39550680E-01
        real :: rb_src
        real, parameter :: ref_rb_src(2) =  (/ 28.5675869,  19.2235221863 /)
        real :: rb_trj
        real, parameter :: ref_rb_trj(2) =  (/ 28.5675869,  19.2235221863 /)
        real :: rb_rcp
        real, parameter :: ref_rb_rcp(2) =   (/ 30.1502495, 20.2885169983 /)
        call ops_resist_rb_all( icm, uster_src, uster_tra, uster_rcp, rb_src, rb_trj, rb_rcp)
        call assertEqual( ref_rb_src(1), rb_src, tol, "rb_src", __LINE__, __FILE__)
        call assertEqual( ref_rb_trj(1), rb_trj, tol, "rb_trj", __LINE__, __FILE__)
        call assertEqual( ref_rb_rcp(1), rb_rcp, tol, "rb_rcp", __LINE__, __FILE__)

        call ops_resist_rb_all( icm3, uster_src, uster_tra, uster_rcp, rb_src, rb_trj, rb_rcp)
        call assertEqual( ref_rb_src(2), rb_src, tol, "rb_src", __LINE__, __FILE__)
        call assertEqual( ref_rb_trj(2), rb_trj, tol, "rb_trj", __LINE__, __FILE__)
        call assertEqual( ref_rb_rcp(2), rb_rcp, tol, "rb_rcp", __LINE__, __FILE__)
   end subroutine test_ops_resist_rb_all

   subroutine test_ops_resist_rb()
   use no_pfunit_ops_lt
   use m_ops_resist_rek
       real, parameter :: tol = 1e-5
        integer, parameter :: icnr = 1, icnr5=5, icnr6=6, icnr8=8, icnr9=9
        real, parameter :: uster =   2.52821952E-01
        real :: rb
        real, parameter :: ref_rb(4) = (/  28.5675869,  22.6076469421,  25.7546749115, 19.2235221863   /)
        call ops_resist_rb( icnr, uster, rb)
        call assertEqual( ref_rb(1), rb, tol, "rb", __LINE__, __FILE__)
        call ops_resist_rb( icnr5, uster, rb)
        call assertEqual( ref_rb(2), rb, tol, "rb", __LINE__, __FILE__)
        call ops_resist_rb( icnr6, uster, rb)
        call assertEqual( ref_rb(3), rb, tol, "rb", __LINE__, __FILE__)
        call ops_resist_rb( icnr8, uster, rb)
        call assertEqual( ref_rb(1), rb, tol, "rb", __LINE__, __FILE__)
        call ops_resist_rb( icnr9, uster, rb)
        call assertEqual( ref_rb(4), rb, tol, "rb", __LINE__, __FILE__)
   end subroutine test_ops_resist_rb

   subroutine test_ops_resist_rc_all3()
   use m_ops_resist_rek
   use no_pfunit_ops_lt
       real, parameter :: tol = 1e-5
        logical, parameter :: isec = .false.
        integer, parameter :: icm = 24
        real, parameter :: rc_so2_ms =   6.55333252E+01
        integer, parameter :: iseiz = 1
        integer, parameter :: nwet = 0
        real, parameter :: hum =   7.58666534E+01
        real, parameter :: uster_rcp =   2.39550680E-01
        real, parameter :: uster_tra =   2.52821952E-01
        integer, parameter :: kdeel = 1
        integer, parameter :: mb = 1
        real, parameter :: temp_C =   1.25771332E+01
        real, parameter :: rad_W_m2 =   2.57242645E+02
        real, parameter :: rc_no =   0.00000000E+00
        real, parameter :: rhno2 =   0.00000000E+00
        real, parameter :: nh3bg_rcp =   0.00000000E+00
        real, parameter :: nh3bgtra =   0.00000000E+00
        real, parameter :: gw_rcp =   0.00000000E+00
        real, parameter :: gwtra =   0.00000000E+00
        real, parameter :: so2bg_rcp =   0.00000000E+00
        real, parameter :: so2bgtra =   0.00000000E+00
        real, parameter :: gym =   5.15891228E+01
        logical, parameter :: gasv = .false.
        real, parameter :: lu_rcp_per(9) = (/   1.00000000E+02,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00/)
        real, parameter :: lu_tra_per(9) = (/   1.00000000E+02,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00/)
        real, parameter :: rc_user =   0.00000000E+00
        real, parameter :: r_no2_nox =   3.91322437E-34
        real, parameter :: ra_ms_4 =   2.01143246E+01
        real, parameter :: ra_trj_4 =   2.29623680E+01
        real, parameter :: ra_rcp_4 =   2.43975487E+01
        real, parameter :: rb_ms =   2.72685947E+01
        real, parameter :: rb_trj =   2.85675850E+01
        real, parameter :: rb_rcp =   3.01502495E+01
        real :: rc_eff_src_4_pos
        real, parameter :: ref_rc_eff_src_4_pos =   3.20000000E+03
        real :: rc_eff_trj_4_pos
        real, parameter :: ref_rc_eff_trj_4_pos =   3.20000000E+03
        real :: rc_eff_rcp_4_pos
        real, parameter :: ref_rc_eff_rcp_4_pos =   3.20000000E+03
        real :: rc_eff_rcp_4
        real, parameter :: ref_rc_eff_rcp_4 =   3.20000000E+03
        call ops_resist_rc_all(icm,isec,gasv,kdeel,iseiz,mb,gym,uster_tra,uster_rcp, &
                             nwet,hum,temp_C,rad_W_m2,lu_tra_per,lu_rcp_per,so2bgtra,nh3bgtra,so2bg_rcp,nh3bg_rcp, gwtra, gw_rcp, &
                             ra_ms_4,ra_trj_4,ra_rcp_4,rb_trj,rb_ms,rb_rcp,rc_user,rc_so2_ms,rc_no,r_no2_nox,rhno2, &
                             rc_eff_src_4_pos,rc_eff_trj_4_pos,rc_eff_rcp_4,rc_eff_rcp_4_pos)
        call assertRelativelyEqual( ref_rc_eff_src_4_pos, rc_eff_src_4_pos, tol, "rc_eff_src_4_pos", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_eff_trj_4_pos, rc_eff_trj_4_pos, tol, "rc_eff_trj_4_pos", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_eff_rcp_4_pos, rc_eff_rcp_4_pos, tol, "rc_eff_rcp_4_pos", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_eff_rcp_4, rc_eff_rcp_4, tol, "rc_eff_rcp_4", __LINE__, __FILE__)
   end subroutine test_ops_resist_rc_all3
   subroutine test_ops_resist_rc_all2()
   use m_ops_resist_rek
   use no_pfunit_ops_lt
       real, parameter :: tol = 1e-5
        logical, parameter :: isec = .true.
        integer, parameter :: icm = 2
        real, parameter :: rc_so2_ms =   6.55333252E+01
        integer, parameter :: iseiz = 1
        integer, parameter :: nwet = 0
        real, parameter :: hum =   7.58666534E+01
        real, parameter :: uster_rcp =   2.39550680E-01
        real, parameter :: uster_tra =   2.52821952E-01
        integer, parameter :: kdeel = 1
        integer, parameter :: mb = 1
        real, parameter :: temp_C =   1.25771332E+01
        real, parameter :: rad_W_m2 =   2.57242645E+02
        real, parameter :: rc_no =   2.00000000E+03
        real, parameter :: rhno2 =   2.99999993E-02
        real, parameter :: nh3bg_rcp =   0.00000000E+00
        real, parameter :: nh3bgtra =   1.32029724E+01
        real, parameter :: gw_rcp =   0.00000000E+00
        real, parameter :: gwtra =   0.00000000E+00
        real, parameter :: so2bg_rcp =   0.00000000E+00
        real, parameter :: so2bgtra =   0.00000000E+00
        real, parameter :: gym =   5.15891228E+01
        logical, parameter :: gasv = .true.
        real, parameter :: lu_rcp_per(9) = (/   1.00000000E+02,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00/)
        real, parameter :: lu_tra_per(9) = (/   1.00000000E+02,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00/)
        real, parameter :: rc_user =   0.00000000E+00
        real, parameter :: r_no2_nox =   5.85207343E-01
        real, parameter :: ra_ms_4 =   2.01143246E+01
        real, parameter :: ra_trj_4 =   2.29623680E+01
        real, parameter :: ra_rcp_4 =   2.43975487E+01
        real, parameter :: rb_ms =   2.72685947E+01
        real, parameter :: rb_trj =   2.85675850E+01
        real, parameter :: rb_rcp =   3.01502495E+01
        real :: rc_eff_src_4_pos
        real, parameter :: ref_rc_eff_src_4_pos =   6.59993958E+02
        real :: rc_eff_trj_4_pos
        real, parameter :: ref_rc_eff_trj_4_pos =   6.59993958E+02
        real :: rc_eff_rcp_4_pos
        real, parameter :: ref_rc_eff_rcp_4_pos =   6.59993835E+02
        real :: rc_eff_rcp_4
        real, parameter :: ref_rc_eff_rcp_4 =   6.59993835E+02
        call ops_resist_rc_all(icm,isec,gasv,kdeel,iseiz,mb,gym,uster_tra,uster_rcp, &
                             nwet,hum,temp_C,rad_W_m2,lu_tra_per,lu_rcp_per,so2bgtra,nh3bgtra,so2bg_rcp,nh3bg_rcp, gwtra, gw_rcp,&
                             ra_ms_4,ra_trj_4,ra_rcp_4,rb_trj,rb_ms,rb_rcp,rc_user,rc_so2_ms,rc_no,r_no2_nox,rhno2, &
                             rc_eff_src_4_pos,rc_eff_trj_4_pos,rc_eff_rcp_4,rc_eff_rcp_4_pos)
        call assertRelativelyEqual( ref_rc_eff_src_4_pos, rc_eff_src_4_pos, tol, "rc_eff_src_4_pos", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_eff_trj_4_pos, rc_eff_trj_4_pos, tol, "rc_eff_trj_4_pos", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_eff_rcp_4_pos, rc_eff_rcp_4_pos, tol, "rc_eff_rcp_4_pos", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_eff_rcp_4, rc_eff_rcp_4, tol, "rc_eff_rcp_4", __LINE__, __FILE__)
   end subroutine test_ops_resist_rc_all2

   subroutine test_ops_resist_rc_all()
   use m_ops_resist_rek
   use no_pfunit_ops_lt
       real, parameter :: tol = 1e-5
        logical, parameter :: isec = .false.
        integer, parameter :: icm = 4
        real, parameter :: rc_so2_ms =   6.55333252E+01
        integer, parameter :: iseiz = 1
        integer, parameter :: nwet = 0
        real, parameter :: hum =   7.58666534E+01
        real, parameter :: uster_rcp =   2.39550680E-01
        real, parameter :: uster_tra =   2.52821952E-01
        integer, parameter :: kdeel = 1
        integer, parameter :: mb = 1
        real, parameter :: temp_C =   1.25771332E+01
        real, parameter :: rad_W_m2 =   2.57242645E+02
        real, parameter :: rc_no =   0.00000000E+00
        real, parameter :: rhno2 =   0.00000000E+00
        real, parameter :: nh3bg_rcp =   0.00000000E+00
        real, parameter :: nh3bgtra =   0.00000000E+00
        real, parameter :: gw_rcp =   0.00000000E+00
        real, parameter :: gwtra =   0.00000000E+00
        real, parameter :: so2bg_rcp =   0.00000000E+00
        real, parameter :: so2bgtra =   0.00000000E+00
        real, parameter :: gym =   5.15891228E+01
        logical, parameter :: gasv = .true.
        real, parameter :: lu_rcp_per(9) = (/   1.00000000E+02,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00/)
        real, parameter :: lu_tra_per(9) = (/   1.00000000E+02,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00/)
        real, parameter :: rc_user =   1.30000000E+01
        real, parameter :: r_no2_nox =   3.91322437E-34
        real, parameter :: ra_ms_4 =   2.01143246E+01
        real, parameter :: ra_trj_4 =   2.29623680E+01
        real, parameter :: ra_rcp_4 =   2.43975487E+01
        real, parameter :: rb_ms =   2.72685947E+01
        real, parameter :: rb_trj =   2.85675850E+01
        real, parameter :: rb_rcp =   3.01502495E+01
        real :: rc_eff_src_4_pos
        real, parameter :: ref_rc_eff_src_4_pos =   1.30000000E+01
        real :: rc_eff_trj_4_pos
        real, parameter :: ref_rc_eff_trj_4_pos =   1.30000000E+01
        real :: rc_eff_rcp_4_pos
        real, parameter :: ref_rc_eff_rcp_4_pos =   1.30000000E+01
        real :: rc_eff_rcp_4
        real, parameter :: ref_rc_eff_rcp_4 =   1.30000000E+01
        call ops_resist_rc_all(icm,isec,gasv,kdeel,iseiz,mb,gym,uster_tra,uster_rcp, &
                             nwet,hum,temp_C,rad_W_m2,lu_tra_per,lu_rcp_per,so2bgtra,nh3bgtra,so2bg_rcp,nh3bg_rcp, gwtra, gw_rcp, &
                             ra_ms_4,ra_trj_4,ra_rcp_4,rb_trj,rb_ms,rb_rcp,rc_user,rc_so2_ms,rc_no,r_no2_nox,rhno2, &
                             rc_eff_src_4_pos,rc_eff_trj_4_pos,rc_eff_rcp_4,rc_eff_rcp_4_pos)
        call assertEqual( ref_rc_eff_src_4_pos, rc_eff_src_4_pos, tol, "rc_eff_src_4_pos", __LINE__, __FILE__)
        call assertEqual( ref_rc_eff_trj_4_pos, rc_eff_trj_4_pos, tol, "rc_eff_trj_4_pos", __LINE__, __FILE__)
        call assertEqual( ref_rc_eff_rcp_4_pos, rc_eff_rcp_4_pos, tol, "rc_eff_rcp_4_pos", __LINE__, __FILE__)
        call assertEqual( ref_rc_eff_rcp_4, rc_eff_rcp_4, tol, "rc_eff_rcp_4", __LINE__, __FILE__)
   end subroutine test_ops_resist_rc_all
   subroutine test_ops_resist_rc_2()
   use m_ops_resist_rek
   use no_pfunit_ops_lt
       real, parameter :: tol = 1e-5
        integer, parameter :: icm = 3
        integer, parameter :: iseiz = 1
        integer, parameter :: mb = 1
        integer, parameter :: nwet = 0
        real, parameter :: hum =   5.69999962E+01
        real, parameter :: uster =   3.22116375E-01
        real, parameter :: temp_C =   1.99999981E+01
        real, parameter :: gym =   5.14773140E+01
        real, parameter :: rad_W_m2 =   4.50359955E+02
        integer, parameter :: iratns = 2
        real, parameter :: catm =   8.40539169E+00
        real, parameter :: c_ave_prev_nh3 =   8.40539169E+00
        real, parameter :: c_ave_prev_so2 =   2.84674287E+00
        real, parameter :: gammawater =   430
        real, parameter :: ra =   1.66256733E+01
        real, parameter :: rb =   1.50881138E+01
        real, parameter :: lu_per(9) = (/   1.00000000E+02,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00/)
        real :: rc_eff_pos1
        real :: rc_eff_pos2
        real, parameter :: ref_rc_eff_pos =   9.99999939E+02
        real :: rc_eff1
        real :: rc_eff2
        real, parameter :: ref_rc_eff =  -5.40599121E+02
        call ops_resist_rc_trj_rcp(icm, iseiz, mb, gym ,temp_C, rad_W_m2, hum, nwet, iratns, catm, c_ave_prev_nh3, c_ave_prev_so2, gammawater,&
                                   uster, lu_per, ra, rb, rc_eff_pos1, rc_eff1, catm, c_ave_prev_nh3, c_ave_prev_so2, gammawater, uster, lu_per, ra, rb, rc_eff_pos2, rc_eff2)

        call assertRelativelyEqual( ref_rc_eff_pos, rc_eff_pos1, tol, "rc_eff_pos", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_eff_pos, rc_eff_pos2, tol, "rc_eff_pos", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_eff, rc_eff1, tol, "rc_eff", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_eff, rc_eff2, tol, "rc_eff", __LINE__, __FILE__)
   end subroutine test_ops_resist_rc_2

 subroutine test_ops_resist_rc()
    use m_ops_resist_rek
    use m_commonfile
    use no_pfunit
        real, parameter :: tol = 1e-5
        integer :: itest
        integer, parameter :: ntests = 2
        integer, parameter :: iseiz = 1 , iseiz2=2, iseiz3=3
        integer, parameter :: mb = 1   
        integer, parameter :: iratns = 2         
        integer, parameter :: icm(ntests)         = (/   2,            3           /)
        integer, parameter :: nwet(ntests)        = (/   0,            1           /)
        real, parameter :: hum(ntests)            = (/  75.8666534,   93.4666595   /)
        real, parameter :: uster(ntests)          = (/    .252821952,   .106380522 /)
        real, parameter :: temp_C(ntests)         = (/  12.5771332,    8.92433739  /)
        real, parameter :: gym(ntests)            = (/  51.5891228,   52.3753700   /)
        real, parameter :: rad_W_m2(ntests)       = (/ 257.242645,     2.77999973  /)
        real, parameter :: catm(ntests)           = (/   9.35210514,   8.50207615  /)
        real, parameter :: c_ave_prev_nh3(ntests) = (/   9.35210514,   8.50207615  /)
        real, parameter :: c_ave_prev_so2(ntests) = (/   0.0,          1.87121403  /)
        real, parameter :: gammawater(ntests)     = (/   0.0,          430.0  /)
        real, parameter :: ra(ntests)             = (/  22.9623680,   89.3671646   /)
        real, parameter :: rb(ntests)             = (/  28.5675850,   45.6862602   /)
        real, parameter :: ref_rc_eff_pos(ntests) = (/ 571.626404,   117.352036    /)
        real, parameter :: ref_rc_eff(ntests)     = (/ 571.626404,   117.352036    /)
        real, parameter :: ref_rc_eff_pos2(ntests) = (/ 639.444396973, 108.879135132   /)
        real, parameter :: ref_rc_eff2(ntests)     = (/ 639.444396973, 108.879135132   /)
        real, parameter :: ref_rc_eff_pos3(ntests) = (/ 568.916503906, 128.839004517 /)
        real, parameter :: ref_rc_eff3(ntests)     = (/ 568.916503906, 128.839004517 /)
        real, parameter :: lu_per(9,ntests) = reshape ( (/   100.,   0.0, 0.0,   0.0, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             4.69047585E+01,   8.57142830E+00,   0.00000000E+00,   9.66666603E+00,   1.29523802E+01, &
             2.14285707E+00,   1.82857132E+01,   1.47619033E+00,   0.00000000E+00/), (/9,ntests/) )
        real :: rc_eff_pos1(ntests)
        real :: rc_eff_pos2(ntests)
        real :: rc_eff1(ntests) 
        real :: rc_eff2(ntests) 
        do itest = 1,ntests

            call ops_resist_rc_trj_rcp(icm(itest), iseiz, mb, gym(itest), temp_C(itest), rad_W_m2(itest), hum(itest), &
                                       nwet(itest), iratns, catm(itest), c_ave_prev_nh3(itest), c_ave_prev_so2(itest), gammawater(itest), &
                                       uster(itest), lu_per(:,itest), ra(itest), rb(itest), rc_eff_pos1(itest), rc_eff1(itest), &
                                       catm(itest), c_ave_prev_nh3(itest), c_ave_prev_so2(itest), gammawater(itest), &
                                       uster(itest), lu_per(:,itest), ra(itest), rb(itest), rc_eff_pos2(itest), rc_eff2(itest))
           call assertRelativelyEqual( ref_rc_eff_pos(itest), rc_eff_pos1(itest), tol, "rc_eff_pos", __LINE__, __FILE__)
           call assertRelativelyEqual( ref_rc_eff_pos(itest), rc_eff_pos2(itest), tol, "rc_eff_pos", __LINE__, __FILE__)
           call assertRelativelyEqual( ref_rc_eff(itest), rc_eff1(itest), tol, "rc_eff", __LINE__, __FILE__)
           call assertRelativelyEqual( ref_rc_eff(itest), rc_eff2(itest), tol, "rc_eff", __LINE__, __FILE__)

           call ops_resist_rc_trj_rcp(icm(itest), iseiz2, mb, gym(itest), temp_C(itest), rad_W_m2(itest), hum(itest), &
                                      nwet(itest), iratns, catm(itest), c_ave_prev_nh3(itest), c_ave_prev_so2(itest), gammawater(itest), &
                                      uster(itest), lu_per(:,itest), ra(itest), rb(itest), rc_eff_pos1(itest), rc_eff1(itest), &
                                      catm(itest), c_ave_prev_nh3(itest), c_ave_prev_so2(itest), gammawater(itest), &
                                      uster(itest), lu_per(:,itest), ra(itest), rb(itest), rc_eff_pos2(itest), rc_eff2(itest))
           call assertRelativelyEqual( ref_rc_eff_pos2(itest), rc_eff_pos1(itest), tol, "rc_eff_pos", __LINE__, __FILE__)
           call assertRelativelyEqual( ref_rc_eff_pos2(itest), rc_eff_pos2(itest), tol, "rc_eff_pos", __LINE__, __FILE__)
           call assertRelativelyEqual( ref_rc_eff2(itest), rc_eff1(itest), tol, "rc_eff", __LINE__, __FILE__)
           call assertRelativelyEqual( ref_rc_eff2(itest), rc_eff2(itest), tol, "rc_eff", __LINE__, __FILE__)

           call ops_resist_rc_trj_rcp(icm(itest), iseiz3, mb, gym(itest), temp_C(itest), rad_W_m2(itest), hum(itest), &
                                      nwet(itest), iratns, catm(itest), c_ave_prev_nh3(itest), c_ave_prev_so2(itest), gammawater(itest), &
                                      uster(itest), lu_per(:,itest), ra(itest), rb(itest), rc_eff_pos1(itest), rc_eff1(itest), &
                                      catm(itest), c_ave_prev_nh3(itest), c_ave_prev_so2(itest), gammawater(itest), &
                                      uster(itest), lu_per(:,itest), ra(itest), rb(itest), rc_eff_pos2(itest), rc_eff2(itest))
           call assertRelativelyEqual( ref_rc_eff_pos3(itest), rc_eff_pos1(itest), tol, "rc_eff_pos", __LINE__, __FILE__)
           call assertRelativelyEqual( ref_rc_eff_pos3(itest), rc_eff_pos2(itest), tol, "rc_eff_pos", __LINE__, __FILE__)
           call assertRelativelyEqual( ref_rc_eff3(itest), rc_eff1(itest), tol, "rc_eff", __LINE__, __FILE__)
           call assertRelativelyEqual( ref_rc_eff3(itest), rc_eff2(itest), tol, "rc_eff", __LINE__, __FILE__)
        end do
   end subroutine test_ops_resist_rc

   subroutine tst_ops_resist_ra()
   use no_pfunit
   use m_ops_resist_rek
   implicit none
      ! input
      REAL   :: z0     ! surface roughness length in meters.
      REAL   :: z_dep  ! height for which deposition velocity is calculated (m)
      REAL   :: d      ! displacement height (usually 0.7 * vegetation height) (m)
      REAL   :: ol     ! monin-obukhov length (m)
      REAL   :: uster  ! friction velocity u* (m/s)
      
      ! output
      REAL   :: ra     ! aerodynamic resistance at z_dep (s/m)
      real   :: tol = 0.1
      
      ! First test
      z0 = 0.1; z_dep = 2.0; d = 1.5; ol = 40.0; uster = 2.0
      call ops_resist_ra(z0, z_dep, d, ol, uster, ra)
      
      ! value of 2021-08-05
      call assertEqual(2.076650, ra, tol, message='Test 1')
   end subroutine tst_ops_resist_ra
   
   subroutine tst_ops_resist_rb()
   use no_pfunit
   use m_ops_resist_rek
   implicit none
      ! SUBROUTINE ARGUMENTS - INPUT
      INTEGER   :: icnr                       ! component number for calculation of rb
      REAL      :: uster                      ! friction velocity u* (m/s)
      
      ! output
      REAL      :: rb                         ! laminar layer resistance for component incr (s/m)
      real      :: tol = 0.1
      
      ! First test, NO2
      icnr = 7; uster = 2.0
      call ops_resist_rb(icnr, uster, rb)
      
      ! value of 2021-08-09
      call assertEqual(3.255674, rb, tol, message='Test 1')
   end subroutine tst_ops_resist_rb
   

   subroutine tst_ops_resist_rain_out_scav_ratio()
   ! Compute in-cloud scavenging ratio (rout << rain-out = in-cloud) [-])
   use no_pfunit
   use m_ops_resist_rek
   use m_ops_varin, only: Tvarin_unc
   implicit none

      ! SUBROUTINE ARGUMENTS - INPUT
      INTEGER   :: icm       ! component number
      LOGICAL   :: gasv      ! TRUE for gasuous component
      LOGICAL   :: isec      ! TRUE if component=[SO2, NOx, NH3] (acidifying components)
      INTEGER   :: kdeel     ! index of particle size class
      REAL      :: croutpri  ! constant (initial) in-cloud scavenging ratio [-]
                             !    for primary component (rout << rain-out = in-cloud) 
      REAL      :: rations   ! NH3/SO2 ratio over trajectory
      REAL      :: r_no2_nox ! NO2/NOx ratio
      
      ! SUBROUTINE ARGUMENTS - OUTPUT
      REAL      :: routpri   ! in-cloud scavenging ratio for primary component
      real      :: tol = 0.1
      type(Tvarin_unc) :: varin_unc
      
      icm       = 1
      gasv      = .true.
      isec      = .true. 
      kdeel     = 1
      croutpri  = 100000.0
      rations   = 3.0
      r_no2_nox = 0.6
      
      ! icm = 1, SO2; routpri = croutpri*rations
      call ops_resist_rain_out_scav_ratio(icm,gasv,isec,kdeel,croutpri,rations,r_no2_nox,routpri, varin_unc)  
      call assertEqual(300000.0, routpri, tol, message='assert routpri SO2') 
      
      ! icm = 2: NOx; routpri = croutpri*r_no2_nox
      icm = 2
      call ops_resist_rain_out_scav_ratio(icm,gasv,isec,kdeel,croutpri,rations,r_no2_nox,routpri, varin_unc)  
      call assertEqual(60000.0, routpri, tol, message='assert routpri NOx') 
      
      !   icm = 3: NH3; routpri = croutpri
      icm = 3
      call ops_resist_rain_out_scav_ratio(icm,gasv,isec,kdeel,croutpri,rations,r_no2_nox,routpri, varin_unc)  
      call assertEqual(100000.0, routpri, tol, message='assert routpri NH3') 
      
      ! Particles; routpri = RORATIO(kdeel)
      isec = .false.; gasv = .false.; kdeel = 1
      call ops_resist_rain_out_scav_ratio(icm,gasv,isec,kdeel,croutpri,rations,r_no2_nox,routpri, varin_unc)  
      call assertEqual(240000.0, routpri, tol, message='assert routpri particles') 
      
      ! gas, non-acidifying ; routpri keeps initial value
      isec = .false.; gasv = .true.; routpri = 1234567.8
      call ops_resist_rain_out_scav_ratio(icm,gasv,isec,kdeel,croutpri,rations,r_no2_nox,routpri, varin_unc)  
      call assertEqual(1234567.8, routpri, tol, message='assert routpri non-acidifying gas') 
   end subroutine tst_ops_resist_rain_out_scav_ratio   
   
   subroutine tst_ops_resist_rc_nox()
   ! Compute canopy resistance for NOx mixture.                            
   ! The primary substance is calculated as NO2 (because emissions are specified as such) but contains
   ! in reality a mixture of NO, NO2 and HNO2.
   ! The whole is (finally) mentioned NOx and specified in ppb. Therefore dry deposition velocities have to
   ! be calculated as representative for the NO-NO2-HNO2 mixture
   ! Rc for NOx is, uptil now, Rc for NO2 (from DEPAC);
   ! now we compute the effective Rc for the NOx mixture as a weighed mean of the Rc-values for NO, NO2, HNO2.
   use no_pfunit
   use m_ops_resist_rek
   implicit none
   
      ! Input arguments:
      REAL   :: ra_ms_4    ! aerodynamic resistance at 4 m from meteo statistics [s/m]
      REAL   :: rb_ms      ! boundary layer resistance from meteo statistics [s/m]
      REAL   :: rc_so2_ms  ! Rc(SO2) from meteo statistics [s/m] 
      REAL   :: r_no2_nox  ! NO2/NOx ratio
      REAL   :: rc_no      ! canopy resistance for NO [s/m]
      REAL   :: rhno2      ! ratio HNO2/NOx [-]
      
      ! Input/output:
      REAL   :: rc_eff_rcp_4_pos ! effective canopy resistance at receptor, 4 m height, 
                                 ! no re-emission (Rc is positive) [s/m]
                                 ! (i) for NO2, (o): for NOx
      REAL   :: rc_eff_trj_4_pos ! effective canopy resistance for trajectory, 4 m height,
                                 ! no re-emission (Rc is positive) [s/m]
                                 ! (i) for NO2, (o): for NOx
   
      ! Output:
      REAL   :: rc_eff_rcp_4     ! effective canopy resistance at receptor, 4 m height,
                                 ! re-emission allowed [s/m]
      REAL   :: rc_eff_src_4_pos ! effective canopy resistance at source, 4 m height,
                                 ! no re-emission (Rc is positive) [s/m]
      
      ! Local:
      real :: tol = 0.1   ! tolerance for assert
      
      ra_ms_4   = 100       ! aerodynamic resistance at 4 m from meteo statistics [s/m]
      rb_ms     = 20        ! boundary layer resistance from meteo statistics [s/m]
      rc_so2_ms = 30        ! Rc(SO2) from meteo statistics [s/m] 
      r_no2_nox = 0.6       ! NO2/NOx ratio
      rc_no     = 600       ! canopy resistance for NO [s/m]
      rhno2     = 0.04      ! ratio HNO2/NOx [-]
      rc_eff_rcp_4_pos = 80 ! effective canopy resistance at receptor, 4 m height,
                            ! no re-emission (Rc is positive) [s/m]
                            ! (i) for NO2, (o): for NOx
      rc_eff_trj_4_pos = 70 ! effective canopy resistance for trajectory, 4 m height,
                            ! no re-emission (Rc is positive) [s/m]
                            ! (i) for NO2, (o): for NOx
      
      ! Compute canopy resistance for mixture NOx, based on weighed average of deposition velocities of NO2, NO and HNO2
      !
      !        1                [NO2]/[NOx]         (1-[NO2]/[NOx])        [HNO2]/[NOx]
      ! ------------------- = ------------------ + -------------------- + --------------------
      !  Rc(NOx) + Rb + Ra     Rc(NO2)+ Rb + Ra      Rc(NO) + Rb + Ra      Rc(HNO2) + Rb + Ra
      !
      
      ! r                = rb_ms + ra_ms_4
      ! rc_hno2          = rc_so2_ms         ! Rc(HNO2) = Rc(SO2); here Rc(SO2) from meteo statistics is used,
      !                                         so not Rc(SO2) of DEPAC
      ! rc_eff_rcp_4_pos =   1./(r_no2_nox/(rc_eff_rcp_4_pos+r)
      !                    + (1.-r_no2_nox)/(rc_no+r)
      !                    + rhno2/(rc_hno2+r)) - r   
      ! rc_eff_rcp_4     = rc_eff_rcp_4_pos  ! for NOx no re-emission, only for NH3
      ! rc_eff_trj_4_pos =   1./(r_no2_nox/(rc_eff_trj_4_pos+r)
      !                    + (1.-r_no2_nox)/(rc_no+r)
      !                    + rhno2/(rc_hno2+r)) - r
      ! rc_eff_src_4_pos = rc_eff_trj_4_pos  
      
      ! Rc_NO2 = 80; Rinv = 0.6/(Rc_NO2+20+100) + 0.4/(600+20+100)+0.04/(30+20+100); Rc_NOx = 1/Rinv - 20 - 100;
      call ops_resist_rc_nox(ra_ms_4, rb_ms, rc_so2_ms, r_no2_nox, rc_no, rhno2, &
                             rc_eff_rcp_4_pos, rc_eff_trj_4_pos, rc_eff_rcp_4, rc_eff_src_4_pos)
      call assertEqual(1.416279e+02,rc_eff_rcp_4_pos, tol, message='assert1 rc_eff_rcp_4_pos') 
      
      ! Rc_NO2 = 70; Rinv = 0.6/(Rc_NO2+20+100) + 0.4/(600+20+100)+0.04/(30+20+100); Rc_NOx = 1/Rinv - 20 - 100;
      call assertEqual(1.31248898e+02,rc_eff_trj_4_pos  , tol, message='assert1 rc_eff_trj_4_pos') 
      call assertEqual(rc_eff_rcp_4_pos,rc_eff_rcp_4    , tol, message='assert2 rc_eff_rcp_4    ') 
      call assertEqual(rc_eff_trj_4_pos,rc_eff_src_4_pos, tol, message='assert3 rc_eff_src_4_pos') 
                                
   end subroutine tst_ops_resist_rc_nox
   
   subroutine tst_ops_resist_rc_sec_trj
   ! Compute rc_sec_trj: canopy resistance secondary component representative for trajectory
   use no_pfunit
   use m_ops_resist_rek
   implicit none
   
      ! SUBROUTINE ARGUMENTS - INPUT
      INTEGER   :: icm        ! component number
      REAL      :: ra_ms_4    ! aerodynamic resistance at 4 m from meteo statistics [s/m]
      REAL      :: rb_ms      ! boundary layer resistance from meteo statistics [s/m]
      REAL      :: rc_aer_ms  ! Rc(SO4-aerosol) from meteo statistics [s/m]
      REAL      :: rc_hno3    ! canopy resistance for HNO3 [s/m]
      REAL      :: rhno3_trj  ! ratio [HNO3]/[NO3_totaal] [-]
      
      ! SUBROUTINE ARGUMENTS - OUTPUT
      REAL      :: rc_sec_trj !  canopy resistance secondary aerosol (SO4, NO3, NH4) for trajectory [s/m]
      
      ! LOCAL VARIABLES
      real      :: tol = 0.1  ! tolerance for assert
        
      ! icm = 1: SO2, secondary component = SO4
      ! rc_sec_trj: canopy resistance secondary aerosol representative for trajectory;
      !             taken as 0.8*Rc(SO4_aerosol) = rc_aer_ms*0.8
      icm = 1
      ra_ms_4   = 100         ! aerodynamic resistance at 4 m from meteo statistics [s/m]
      rb_ms     = 20          ! boundary layer resistance from meteo statistics [s/m]
      rc_aer_ms = 60          ! Rc(SO4-aerosol) from meteo statistics [s/m]
      rc_hno3   = 80          ! canopy resistance for HNO3 [s/m]
      rhno3_trj = 0.2         ! ratio [HNO3]/[NO3_totaal] [-]
      call ops_resist_rc_sec_trj(icm,ra_ms_4,rb_ms,rc_aer_ms,rc_hno3,rhno3_trj,rc_sec_trj)
      call assertEqual(48.0,rc_sec_trj,tol,message='assert rc_sec_trj for SO4')
      
      ! icm = 2: NOx, secondary component = NO3
      !
      !            1             [HNO3]/[NO3]_totaal     (1-[HNO3]/[NO3]_totaal)
      ! ------------------- = ----------------------- + ----------------------------
      !  Rc(NO3) + Rb + Ra       Rc(HNO3)+ Rb + Ra        Rc(NO3_aerosol) + Rb + Ra      
      !
      !   = 0.2/(80+20+100) + 0.8/(60+20+100) = 0.2/200 + 0.8/180 = 0.0054444 
      !
      ! Rc(NO3) = 1/0.00544444 - 20 - 100 = 63.673469
      
      icm = 2 
      call ops_resist_rc_sec_trj(icm,ra_ms_4,rb_ms,rc_aer_ms,rc_hno3,rhno3_trj,rc_sec_trj)
      call assertEqual(63.673469,rc_sec_trj,tol,message='assert rc_sec_trj for NO3')
      
      ! icm = 3: NH3, secondary component = NH4 
      ! rc_sec_trj = 0.8*Rc(SO4_aerosol) 
      icm = 3
      call ops_resist_rc_sec_trj(icm,ra_ms_4,rb_ms,rc_aer_ms,rc_hno3,rhno3_trj,rc_sec_trj)
      call assertEqual(48.0,rc_sec_trj,tol,message='assert rc_sec_trj for NH4')
   
   end subroutine tst_ops_resist_rc_sec_trj
   
   !----------------------------------------------------------------------------------------
   subroutine tst_ops_resist_rcaer 
   ! SUBROUTINE         : ops_resist_rcaer
   ! DESCRIPTION        : calculation of canopy resistance for aerosols SO4, NO3, NH4 according
   !                      to Wesely et al. (1985) for low vegetation and other areas with
   !                      a roughness length (znul) < 0.5m and to Erisman et al (1994) and
   !                      Ruijgrok et al. (1994) for forest and other areas with a roughness
   !                      length above 0.5m.
   use no_pfunit
   use m_ops_resist_rek
   implicit none
      ! SUBROUTINE ARGUMENTS - INPUT
      INTEGER   :: icmpsec ! component number (11 = SO4, 12 = NO3, 13 = NH4)
      REAL      :: znul    ! roughness length [m]
      REAL      :: ust     ! friction velocity [m/s]
      REAL      :: ol      ! Monin-Obukhov length [m][m]
      REAL      :: hum     ! relative humidity [%]
      INTEGER   :: nwet    ! wetness indicator depending on percipitation probability and humidity
      REAL      :: Uh      ! wind speed used in parametrisation of vd for aerosols [m/s]
      REAL      :: ra_ms_4 ! aerodynamic resistance at 4 m from meteo statistics [s/m]
      REAL      :: rb_ms   ! boundary layer resistance SO2 from meteo statistics [s/m]
      REAL      :: rc_hno3 ! canopy resistance for HNO3 [s/m]
      REAL      :: rhno3   ! ratio [HNO3]/[NO3_total] [-]
      
      ! SUBROUTINE ARGUMENTS - OUTPUT
      REAL      :: rc_aer  ! canopy resistance aerosols SO4, NO3, NH4 [s/m]
      
      ! LOCAL VARIABLES
      real      :: tol = 0.1 ! tolerance in assert
      
      ! components:
      ! 11 = SO4
      ! 12 = NO3
      ! 13 = NH4
      !
      ust     = 1.1        ! friction velocity [m/s]
      hum     = 80.0       ! relative humidity [%]
      nwet    = 0          ! wetness indicator depending on percipitation probability and humidity
      Uh      = 11.0       ! wind speed used in parametrisation of vd for aerosols [m/s]
      ra_ms_4 = 100        ! aerodynamic resistance at 4 m from meteo statistics [s/m]
      rb_ms   = 20         ! boundary layer resistance SO2 from meteo statistics [s/m]
      rc_hno3 = 80         ! canopy resistance for HNO3 [s/m]
      rhno3   = 0.2        ! ratio [HNO3]/[NO3_total] [-]
      
      ! A. znul.lt.0.5
      !    A1. (ol.lt.0) -> rc_aer = 1.0/((ust/500.)*(1.+((300./(-1.*ol))**(2./3.)))) 
      !    A2. (ol.ge.0) -> rc_aer = 1.0/(ust/500) 
      !
      ! B. znul.ge.0.5 -> rc_aer = 1.0/(E*ust**2/Uh) 
      !    SO4/B1. hum.le.80, (nwet.ge.1) -> E = 0.08*ust**0.45  
      !    SO4/B2. hum.le.80, (nwet.lt.1) -> E = 0.05*ust**0.28  
      !    SO4/B3. hum.gt.80, (nwet.ge.1) -> E = 0.08*ust**0.45*(1+0.37*EXP((hum-80)/20))  
      !    SO4/B4. hum.gt.80, (nwet.lt.1) -> E = 0.05*ust**0.28*(1+0.18*EXP((hum-80)/20))  
      
      !---------------------------------
      ! Sulphate
      !---------------------------------
      ! A1. znul.lt.0.5, ol.lt.0
      !                         1
      ! rc_aer = --------------------------------------------
      !          ((ust/500)*(1 + ((300/(-L))**(2./3.))))
      !
      !        = (300/((-L))**(2/3) = 1.5**(2/3) = 1.310370697104448;
      ! ust/500 = 0.0022;
      ! rc_aer = 1/(0.0022*(1 + 1.310370697104448)) = 1.967413519895874e+02
      icmpsec = 11; znul = 0.03; ol = -200.0;  
      call ops_resist_rcaer (icmpsec, znul, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, rc_aer)  
      call assertEqual( 1.9674135e+02,rc_aer,tol,message='assert A1 rc_aer SO4')
      
      ! A2. znul.lt.0.5, (ol.ge.0) -> rc_aer = 1.0/(ust/500) = 454.5454...
      icmpsec = 11; znul = 0.03; ol = 200.0
      call ops_resist_rcaer (icmpsec, znul, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, rc_aer)  
      call assertEqual(4.54545e+02,rc_aer,tol,message='assert A2 rc_aer SO4')
      
      ! B. znul.ge.0.5 -> rc_aer = 1.0/(E*ust**2/Uh) 
      !    SO4/B1. hum.le.80, (nwet.ge.1) -> E = 0.08*ust**0.45  
      icmpsec = 11; znul = 0.6; hum = 70; nwet = 2; ! E = 0.083505810443527; rc_aer = 1.088655872282932e+02  
      call ops_resist_rcaer (icmpsec, znul, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, rc_aer)  
      call assertEqual(1.088656e+02,rc_aer,tol,message='assert B1 rc_aer SO4')
      
      !    SO4/B2. hum.le.80, (nwet.lt.1) -> E = 0.05*ust**0.28  
      icmpsec = 11; znul = 0.6; hum = 70; nwet = 0; ! E = 0.051352306662934; rc_aer = 1.770301994529647e+02
      call ops_resist_rcaer (icmpsec, znul, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, rc_aer)  
      call assertEqual(1.77030e+02,rc_aer,tol,message='assert B2 rc_aer SO4')
      
      !    SO4/B3. hum.gt.80, (nwet.ge.1) -> E = 0.08*ust**0.45*(1+0.37*EXP((hum-80)/20))  
      icmpsec = 11; znul = 0.6; hum = 85; nwet = 2; ! E = 0.123178536172248; rc_aer = 73.802704378599998
      call ops_resist_rcaer (icmpsec, znul, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, rc_aer)  
      call assertEqual(73.802704,rc_aer,tol,message='assert B3 rc_aer SO4')
      
      !    SO4/B4. hum.gt.80, (nwet.lt.1) -> E = 0.05*ust**0.28*(1+0.18*EXP((hum-80)/20))  
      icmpsec = 11; znul = 0.6; hum = 85; nwet = 0; ! E = 0.063221086715870; rc_aer = 1.437955208167453e+02
      call ops_resist_rcaer (icmpsec, znul, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, rc_aer)  
      call assertEqual(1.4379552e+02,rc_aer,tol,message='assert B4 rc_aer SO4')
      
      !---------------------------------
      ! Nitrate
      !---------------------------------
      !  rc_aer is valid for NO3 aerosol. Calculate now a weighted value for the NO3+HNO3 mixture 
      !
      !          1               [HNO3]/[NO3]_totaal     (1-[HNO3]/[NO3]_totaal)
      ! ------------------- = ----------------------- + ---------------------------- 
      !  Rc(NO3) + Rb + Ra       Rc(HNO3)+ Rb + Ra        Rc(NO3_aerosol) + Rb + Ra      
      !
      ! Rinv = 0.2/(80+20+100) + 0.8/(rc_aer+20+100); rc_aer = 1/Rinv -20 - 100
      
      ! B. znul.ge.0.5 -> rc_aer = 1.0/(E*ust**2/Uh) 
      !    NO3/B1. hum.le.80, nwet.ge.1 -> E = 0.10*ust**0.43
      icmpsec = 12; znul = 0.6; hum = 70; nwet = 2; ! E = 0.10418; rc_aer1 =  87.25864; rc_aer2 = 85.76507
      call ops_resist_rcaer (icmpsec, znul, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, rc_aer)  
      call assertEqual(85.76507,rc_aer,tol,message='assert B1 rc_aer NO3')
      
      !    NO3/B2. hum.le.80, nwet.lt.1 -> E = 0.063*ust**0.25
      icmpsec = 12; znul = 0.6; hum = 70; nwet = 0; ! E = 0.06452; rc_aer1 = 140.90247; rc_aer2 = 125.92503
      call ops_resist_rcaer (icmpsec, znul, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, rc_aer)  
      call assertEqual(125.92503,rc_aer,tol,message='assert B2 rc_aer NO3')
      
      !    NO3/B3. hum.gt.80, nwet.ge.1 -> E = 0.10*ust**0.43*(1+0.37*EXP((hum-80)/20))
      icmpsec = 12; znul = 0.6; hum = 85; nwet = 2; ! E = 0.15368; rc_aer1 = 59.15482; rc_aer2 = 62.96884
      call ops_resist_rcaer (icmpsec, znul, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, rc_aer)  
      call assertEqual(62.96884,rc_aer,tol,message='assert B3 rc_aer NO3')
      
      !    NO3/B4. hum.gt.80, nwet.lt.1 -> E = 0.063*ust**0.25*(1+0.18*EXP((hum-80)/20))
      icmpsec = 12; znul = 0.6; hum = 85; nwet = 0; ! E = 0.07943; rc_aer1 = 114.45021; rc_aer2 = 106.64233
      call ops_resist_rcaer (icmpsec, znul, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, rc_aer)  
      call assertEqual(106.64233,rc_aer,tol,message='assert B4 rc_aer NO3')
      
      !----------------------------
      ! Ammonium
      !----------------------------!!
      
      ! B. znul.ge.0.5 -> rc_aer = 1.0/(E*ust**2/Uh) 
      ! NH4/B1. hum.le.80, nwet.ge.1 -> E = 0.066*ust**0.41
      icmpsec = 13; znul = 0.6; hum = 70; nwet = 2; ! E = 0.06863; rc_aer = 132.46233
      call ops_resist_rcaer (icmpsec, znul, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, rc_aer)  
      call assertEqual(132.46233,rc_aer,tol,message='assert B1 rc_aer NH4')
      
      ! NH4/B2. hum.le.80, nwet.lt.1 -> E = 0.05*ust**0.23
      icmpsec = 13; znul = 0.6; hum = 70; nwet = 0; ! E = 0.05111; rc_aer = 177.87585
      call ops_resist_rcaer (icmpsec, znul, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, rc_aer)  
      call assertEqual(177.87585,rc_aer,tol,message='assert B2 rc_aer NH4')
        
      ! NH4/B3. hum.gt.80, nwet.ge.1 -> E = 0.066*ust**0.41*(1+0.37*EXP((hum-80)/20))
      icmpsec = 13; znul = 0.6; hum = 85; nwet = 2; ! E = 0.10124; rc_aer =  89.79952
      call ops_resist_rcaer (icmpsec, znul, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, rc_aer)  
      call assertEqual(89.79952,rc_aer,tol,message='assert B3 rc_aer NH4')
      
      ! NH4/B4. hum.gt.80, nwet.lt.1 -> E = 0.05*ust**0.23*(1+0.18*EXP((hum-80)/20))
      icmpsec = 13; znul = 0.6; hum = 85; nwet = 0; ! E = 0.06292; rc_aer = 144.48242
      call ops_resist_rcaer (icmpsec, znul, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, rc_aer)  
      call assertEqual(144.48242,rc_aer,tol,message='assert B4 rc_aer NH4')
   end subroutine tst_ops_resist_rcaer

   subroutine tst_fpsih()
   use no_pfunit
   use m_ops_resist_rek
   implicit none
   
      REAL    :: eta                        ! stability parameter z/L
      
      ! output
      REAL    :: output                     ! psi_h = stability correction function for heat
      real    :: tol = 0.1
      
      ! First test, eta < 0
      eta = -2.0
      output = fpsih(eta)
      
      ! value of 2021-08-09
      call assertEqual(2.431179, output, tol, message='Test 1')
      
      ! eta > 0
      eta = 2.0
      output = fpsih(eta)
      
      ! value of 2021-08-09
      call assertEqual(-7.541484, output, tol, message='Test 2')
      
   end subroutine tst_fpsih

   subroutine test_ops_resist_rc_nox()
   use m_ops_resist_rek
   use no_pfunit_ops_lt
       real, parameter :: tol = 1e-5
        real, parameter :: ra_ms_4 =   2.01143246E+01
        real, parameter :: rb_ms =   2.72685947E+01
        real, parameter :: rc_so2_ms =   6.55333252E+01
        real, parameter :: r_no2_nox =   5.85207343E-01
        real, parameter :: rc_no =   2.00000000E+03
        real, parameter :: rhno2 =   2.99999993E-02
        real, parameter :: in_rc_eff_rcp_4_pos =   5.71626282E+02
        real, parameter :: in_rc_eff_trj_4_pos =   5.71626404E+02
        real :: rc_eff_rcp_4_pos
        real, parameter :: ref_rc_eff_rcp_4_pos =   6.59993835E+02
        real :: rc_eff_trj_4_pos
        real, parameter :: ref_rc_eff_trj_4_pos =   6.59993958E+02
        real :: rc_eff_rcp_4
        real, parameter :: ref_rc_eff_rcp_4 =   6.59993835E+02
        real :: rc_eff_src_4_pos
        real, parameter :: ref_rc_eff_src_4_pos =   6.59993958E+02
        rc_eff_rcp_4_pos = in_rc_eff_rcp_4_pos
        rc_eff_trj_4_pos = in_rc_eff_trj_4_pos
        call ops_resist_rc_nox( ra_ms_4, rb_ms, rc_so2_ms, r_no2_nox, rc_no, rhno2, rc_eff_rcp_4_pos, &
            rc_eff_trj_4_pos, rc_eff_rcp_4, rc_eff_src_4_pos)
        call assertEqual( ref_rc_eff_rcp_4_pos, rc_eff_rcp_4_pos, tol, "rc_eff_rcp_4_pos", __LINE__, __FILE__)
        call assertEqual( ref_rc_eff_trj_4_pos, rc_eff_trj_4_pos, tol, "rc_eff_trj_4_pos", __LINE__, __FILE__)
        call assertEqual( ref_rc_eff_rcp_4, rc_eff_rcp_4, tol, "rc_eff_rcp_4", __LINE__, __FILE__)
        call assertEqual( ref_rc_eff_src_4_pos, rc_eff_src_4_pos, tol, "rc_eff_src_4_pos", __LINE__, __FILE__)
   end subroutine test_ops_resist_rc_nox

   subroutine test_fpsih()
   use m_ops_resist_rek
   use no_pfunit_ops_lt
       real, parameter :: tol = 1e-5
        real, parameter :: eta =  -1.21576682E-01
        real :: y
        real, parameter :: ref_y(2) =  (/ 0.612148643, -0.619074821472 /)
        y= fpsih(eta)
        call assertEqual( ref_y(1), y, tol, "y", __LINE__, __FILE__)
        y= fpsih(-eta)
        call assertEqual( ref_y(2), y, tol, "y", __LINE__, __FILE__)
   end subroutine test_fpsih

   subroutine test_ops_resist_rain_out_scav_ratio()
        use m_ops_resist_rek
        use no_pfunit_ops_lt
        use m_ops_varin, only: Tvarin_unc

        real, parameter :: tol = 1e-5
        integer, parameter :: icm = 4, icm1 = 1, icm2=2, icm3 = 3
        logical, parameter :: gasv = .true., gasv_no = .false.
        logical, parameter :: isec = .false., isec_yes = .true.
        integer, parameter :: kdeel = 1
        real, parameter :: croutpri =   12.00000000E+00
        real, parameter :: rations =   0.70000000E+00
        real, parameter :: r_no2_nox =   3.91322437E-3
        real :: routpri
        real, parameter :: ref_routpri(5) = (/  1E+06, 8.4, 0.04695869237185, 12.0,  24e4   /)
        type(Tvarin_unc) :: varin_unc

        routpri = ref_routpri(1)
        call ops_resist_rain_out_scav_ratio( icm, gasv, isec, kdeel, croutpri, rations, r_no2_nox, routpri, varin_unc)
        call assertEqual( ref_routpri(1), routpri, tol, "routpri", __LINE__, __FILE__)

        routpri = ref_routpri(1)
        call ops_resist_rain_out_scav_ratio( icm1, gasv, isec_yes, kdeel, croutpri, rations, r_no2_nox, routpri, varin_unc)
        call assertEqual( ref_routpri(2), routpri, tol, "routpri", __LINE__, __FILE__)

        routpri = ref_routpri(1)
        call ops_resist_rain_out_scav_ratio( icm2, gasv, isec_yes, kdeel, croutpri, rations, r_no2_nox, routpri, varin_unc)
        call assertEqual( ref_routpri(3), routpri, tol, "routpri", __LINE__, __FILE__)

        routpri = ref_routpri(1)
        call ops_resist_rain_out_scav_ratio( icm3, gasv, isec_yes, kdeel, croutpri, rations, r_no2_nox, routpri, varin_unc)
        call assertEqual( ref_routpri(4), routpri, tol, "routpri", __LINE__, __FILE__)

        routpri = ref_routpri(1)
        call ops_resist_rain_out_scav_ratio( icm3, gasv_no, isec, kdeel, croutpri, rations, r_no2_nox, routpri, varin_unc)
        call assertEqual( ref_routpri(5), routpri, tol, "routpri", __LINE__, __FILE__)
   end subroutine test_ops_resist_rain_out_scav_ratio
 
   subroutine test_ops_resist_rc_sec_trj()
   use m_ops_resist_rek
   use no_pfunit_ops_lt
       real, parameter :: tol = 1e-5
        integer, parameter :: icm = 2, icm1=1, icm3=3
        real, parameter :: ra_ms_4 =   2.01143246E+01
        real, parameter :: rb_ms =   2.72685947E+01
        real, parameter :: rc_aer_ms =   3.99799988E+02
        real, parameter :: rc_hno3 =   1.00000000E+01
        real, parameter :: rhno3_trj =   1.61107481E-01
        real :: rc_sec_trj
        real, parameter :: ref_rc_sec_trj(2) =   (/ 166.130997,  319.839996338 /)
        call ops_resist_rc_sec_trj( icm, ra_ms_4, rb_ms, rc_aer_ms, rc_hno3, rhno3_trj, rc_sec_trj)
        call assertEqual( ref_rc_sec_trj(1), rc_sec_trj, tol, "rc_sec_trj", __LINE__, __FILE__)
        call ops_resist_rc_sec_trj( icm1, ra_ms_4, rb_ms, rc_aer_ms, rc_hno3, rhno3_trj, rc_sec_trj)
        call assertEqual( ref_rc_sec_trj(2), rc_sec_trj, tol, "rc_sec_trj", __LINE__, __FILE__)
        call ops_resist_rc_sec_trj( icm3, ra_ms_4, rb_ms, rc_aer_ms, rc_hno3, rhno3_trj, rc_sec_trj)
        call assertEqual( ref_rc_sec_trj(2), rc_sec_trj, tol, "rc_sec_trj", __LINE__, __FILE__)
   end subroutine test_ops_resist_rc_sec_trj

   subroutine test_ops_resist_rcaer()
   use m_ops_resist_rek
   use no_pfunit_ops_lt
       real, parameter :: tol = 1e-5
        integer, parameter :: icmpsec = 12
        real, parameter :: znul =   2.00000003E-01
        real, parameter :: ust =   2.39550680E-01
        real, parameter :: ol =  -2.59285927E+01
        real, parameter :: hum =   7.58666534E+01
        integer, parameter :: nwet = 0
        real, parameter :: Uh =   2.01143265E+00
        real, parameter :: ra_ms_4 =   2.01143246E+01
        real, parameter :: rb_ms =   2.72685947E+01
        real, parameter :: rc_hno3 =   1.00000000E+01
        real, parameter :: rhno3 =   1.90969050E-01
        real :: rc_aer
        real, parameter :: ref_rc_aer(15) =  (/ 137.478745,   341.298950195, 222.37837219, 1045.94641113, &
              833.473571777, 575.239440918, 858.464843750, 175.654708862, 154.080612183, 175.263702393, & 
              185.822540283, 973.821289062, 954.143737793, 658.522460938, 799.267944336  /)

        call ops_resist_rcaer( icmpsec, znul, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, &
            rc_aer)
        call assertRelativelyEqual( ref_rc_aer(1), rc_aer, tol, "rc_aer", __LINE__, __FILE__)

        call ops_resist_rcaer( icmpsec, znul, ust, -ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, &
            rc_aer)
        call assertRelativelyEqual( ref_rc_aer(3), rc_aer, tol, "rc_aer", __LINE__, __FILE__)

        call ops_resist_rcaer( icmpsec-1, znul, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, &
            rc_aer)
        call assertRelativelyEqual( ref_rc_aer(2), rc_aer, tol, "rc_aer", __LINE__, __FILE__)

        call ops_resist_rcaer( icmpsec-1, znul*3, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, &
            rc_aer)
        call assertRelativelyEqual( ref_rc_aer(4), rc_aer, tol, "rc_aer", __LINE__, __FILE__)

        call ops_resist_rcaer( icmpsec-1, znul*3, ust, ol, hum, nwet+1, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, &
            rc_aer)
        call assertRelativelyEqual( ref_rc_aer(5), rc_aer, tol, "rc_aer", __LINE__, __FILE__)

        call ops_resist_rcaer( icmpsec-1, znul*3, ust, ol, hum+8, nwet+1, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, &
            rc_aer)
        call assertRelativelyEqual( ref_rc_aer(6), rc_aer, tol, "rc_aer", __LINE__, __FILE__)

        call ops_resist_rcaer( icmpsec-1, znul*3, ust, ol, hum+8, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, &
            rc_aer)
        call assertRelativelyEqual( ref_rc_aer(7), rc_aer, tol, "rc_aer", __LINE__, __FILE__)

        call ops_resist_rcaer( icmpsec, znul*3, ust, ol, hum+8, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, &
            rc_aer)
        call assertRelativelyEqual( ref_rc_aer(8), rc_aer, tol, "rc_aer", __LINE__, __FILE__)

        call ops_resist_rcaer( icmpsec, znul*3, ust, ol, hum+8, nwet+1, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, &
            rc_aer)
        call assertRelativelyEqual( ref_rc_aer(9), rc_aer, tol, "rc_aer", __LINE__, __FILE__)

        call ops_resist_rcaer( icmpsec, znul*3, ust, ol, hum, nwet+1, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, &
            rc_aer)
        call assertRelativelyEqual( ref_rc_aer(10), rc_aer, tol, "rc_aer", __LINE__, __FILE__)

        call ops_resist_rcaer( icmpsec, znul*3, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, &
            rc_aer)
        call assertRelativelyEqual( ref_rc_aer(11), rc_aer, tol, "rc_aer", __LINE__, __FILE__)

        call ops_resist_rcaer( icmpsec+1, znul*3, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, &
            rc_aer)
        call assertRelativelyEqual( ref_rc_aer(12), rc_aer, tol, "rc_aer", __LINE__, __FILE__)

        call ops_resist_rcaer( icmpsec+1, znul*3, ust, ol, hum, nwet+1, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, &
            rc_aer)
        call assertRelativelyEqual( ref_rc_aer(13), rc_aer, tol, "rc_aer", __LINE__, __FILE__)

        call ops_resist_rcaer( icmpsec+1, znul*3, ust, ol, hum+8, nwet+1, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, &
            rc_aer)
        call assertRelativelyEqual( ref_rc_aer(14), rc_aer, tol, "rc_aer", __LINE__, __FILE__)

        call ops_resist_rcaer( icmpsec+1, znul*3, ust, ol, hum+8, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, &
            rc_aer)
        call assertRelativelyEqual( ref_rc_aer(15), rc_aer, tol, "rc_aer", __LINE__, __FILE__)

   end subroutine test_ops_resist_rcaer
end module m_test_ops_resist_rek
 
program p_test_ops_resist_rek
use m_test_ops_resist_rek
use no_pfunit
implicit none
   call test_ops_resist_rc_nox()
   call test_fpsih()
   call tst_fpsih()
   call test_ops_resist_rain_out_scav_ratio()
   call test_ops_resist_rc_sec_trj()
   call test_ops_resist_rcaer()
   call tst_ops_resist_ra()
   call tst_ops_resist_rb()
   call tst_ops_resist_rain_out_scav_ratio()
   call tst_ops_resist_rc_nox()
   call test_ops_resist_rc_2()
   call test_ops_resist_rc()
   call test_ops_resist_rc_all()
   call test_ops_resist_rc_all2()
   call test_ops_resist_rc_all3()
   call test_ops_resist_rb()
   call test_ops_resist_rb_all()
   call test_ops_resist_ra_all()
   call test_ops_resist_rek()
   call test_ops_resist_rek2()
   call test_ops_resist_rek3()
   call test_ops_resist_rek4()
   call conclusion()
end program p_test_ops_resist_rek

