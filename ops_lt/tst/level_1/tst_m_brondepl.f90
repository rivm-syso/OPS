module m_test_ops_brondepl
implicit none
contains
   subroutine test_ops_brondepl9()
   use m_ops_tdo_proc, only: tdo_proc
        type(tdo_proc) :: do_proc
        real, parameter :: disx = -999.0
        real, parameter :: disxx =   3.95000000E+04
        real, parameter :: xg =   1.33049072E+04
        real, parameter :: c =   7.66579853E-03
        real, parameter :: ux0 =   2.65974998E+00
        real, parameter :: ueff =   5.44849825E+00
        real, parameter :: sigz =   2.28469336E+03
        real, parameter :: vd_eff_trj_zra =   6.19776081E-03
        real, parameter :: xl =   1.05555701E+03
        integer, parameter :: ircp = -999
        integer, parameter :: istab = 2
        real, parameter :: xloc =   9.75999939E+02
        real, parameter :: xl100 =   1.15999988E+03
        real, parameter :: vw10 =   3.27650404E+00
        real, parameter :: pcoef =   1.29999995E-01
        real, parameter :: virty =   3.81971851E+03
        real, parameter :: radius =   9.94035767E+02
        real, parameter :: ra_rcp_4 =   2.28871861E+01
        real, parameter :: ra_rcp_zrcp =   2.28871861E+01
        real, parameter :: rc_eff_rcp_4_pos =   1.29713776E+02
        real, parameter :: rb_rcp =   1.86522083E+01
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: ol_src =  -4.67883873E+01
        real, parameter :: uster_src =   3.62503409E-01
        real, parameter :: htot =   5.00000000E+00
        real, parameter :: ra_src_4 =   1.75998764E+01
        real, parameter :: rb_src =   1.34071236E+01
        real, parameter :: rc_eff_src_4_pos =   1.16286469E+02
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: vd_trj_z0 =   7.71048153E-03
        real, parameter :: onder =   1.00000000E+00
        integer, parameter :: flag = 0
        real, parameter :: vchem =   5.18968821E+00
        real, parameter :: vnatpri =   2.73047662E+00
        real, parameter :: diameter =   1.77304956E+03
        real, parameter :: dispg(6) = (/   1.73502252E-01,   4.05284643E-01,   6.84554204E-02,   1.04914546E-01, &
             4.78821136E-02,   1.20438904E-01/)
        real, parameter :: zm =   4.00000000E+00
        real, parameter :: in_cgt =   4.72620726E-02
        real, parameter :: in_cgt_z =   4.72620726E-02
        real, parameter :: ref_cgt =   4.72620316E-02
        real, parameter :: ref_cgt_z =   4.72620316E-02
        real, parameter :: ref_cdn(2) = (/  0.972165644, 1.0 /)
        real, parameter :: ref_ugem =   4.41050482E+00
        real, parameter :: ref_hf =   1.49766464E+01
        real, parameter :: ref_cq1(3) =  (/ 0.988336742, 1.0, 0.980268955231 /)
        real, parameter :: ref_cq2(3) = (/  0.960090637, 1.0, 0.953068435192 /)
        real, parameter :: ref_uxr =   3.37251163E+00
        real, parameter :: ref_zu =   2.02089172E+02
        real, parameter :: ref_sigzr(2) = (/  75.8131714, 11.5999984741 /)
        real, parameter :: ref_dxeff(3) =  (/ 441.820526, 0.0,  114.832580566 /)
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call test_ops_brondepl( do_proc, disx, disxx, xg, c, ux0, ueff, sigz, &
             vd_eff_trj_zra, xl, ircp, istab, xloc, xl100, vw10, pcoef, virty, radius, &
             ra_rcp_4, ra_rcp_zrcp, rc_eff_rcp_4_pos, rb_rcp, z0_src, ol_src, &
             uster_src, htot, ra_src_4, rb_src, rc_eff_src_4_pos, qbstf, &
             vd_trj_z0, onder, flag, vchem, vnatpri, diameter, dispg, zm, &
             in_cgt, in_cgt_z, ref_cgt, ref_cgt_z, ref_cdn(1), ref_ugem, ref_hf, &
             ref_cq1(1), ref_cq2(1), ref_uxr, ref_zu, ref_sigzr(1), ref_dxeff(1), &
             __LINE__, __FILE__)

        do_proc%depl_drydep = .false.
        call test_ops_brondepl( do_proc, disx, disxx, xg, c, ux0, ueff, sigz, &
             vd_eff_trj_zra, xl, ircp, istab, xloc, xl100, vw10, pcoef, virty, radius, &
             ra_rcp_4, ra_rcp_zrcp, rc_eff_rcp_4_pos, rb_rcp, z0_src, ol_src, &
             uster_src, htot, ra_src_4, rb_src, rc_eff_src_4_pos, qbstf, &
             vd_trj_z0, onder, flag, vchem, vnatpri, diameter, dispg, zm, &
             in_cgt, in_cgt_z, ref_cgt, ref_cgt_z, ref_cdn(2), ref_ugem, ref_hf, &
             ref_cq1(2), ref_cq2(2), ref_uxr, ref_zu, ref_sigzr(1), ref_dxeff(2), &
             __LINE__, __FILE__)

        call test_ops_brondepl( do_proc, disx, disxx, xg, c, ux0, ueff, sigz, &
             vd_eff_trj_zra, xl, ircp, istab, xloc, xl100/100.0, vw10, pcoef, virty, radius, &
             ra_rcp_4, ra_rcp_zrcp, rc_eff_rcp_4_pos, rb_rcp, z0_src, ol_src, &
             uster_src, htot, ra_src_4, rb_src, rc_eff_src_4_pos, qbstf, &
             vd_trj_z0, onder, flag, vchem, vnatpri, diameter, dispg, zm, &
             in_cgt, in_cgt_z, ref_cgt, ref_cgt_z, ref_cdn(2), ref_ugem, ref_hf, &
             ref_cq1(2), ref_cq2(2), ref_uxr, ref_zu, ref_sigzr(2), ref_dxeff(2), &
             __LINE__, __FILE__)

        do_proc%depl_drydep = .true.
        call test_ops_brondepl( do_proc, disx, disxx, xg, c, ux0, ueff, sigz, &
             vd_eff_trj_zra, xl, ircp, istab, xloc, xl100/100.0, vw10, pcoef, virty, radius, &
             ra_rcp_4, ra_rcp_zrcp, rc_eff_rcp_4_pos, rb_rcp, z0_src, ol_src, &
             uster_src, htot, ra_src_4, rb_src, rc_eff_src_4_pos, qbstf, &
             vd_trj_z0, onder, flag, vchem, vnatpri*10000.0, diameter, dispg, zm, &
             in_cgt, in_cgt_z, ref_cgt, ref_cgt_z, ref_cdn(1), ref_ugem, ref_hf, &
             ref_cq1(3), ref_cq2(3), ref_uxr, ref_zu, ref_sigzr(2), ref_dxeff(3), &
             __LINE__, __FILE__)
   end subroutine test_ops_brondepl9

   subroutine test_ops_brondepl8()
   use m_ops_tdo_proc, only: tdo_proc
        type(tdo_proc) :: do_proc
        real, parameter :: disx = -999.0
        real, parameter :: disxx =   9.82344116E+02
        real, parameter :: xg =   2.89221802E+03
        real, parameter :: c =   6.48795366E+00
        real, parameter :: ux0 =   1.83884811E+00
        real, parameter :: ueff =   2.22042251E+00
        real, parameter :: sigz =   2.23733616E+01
        real, parameter :: vd_eff_trj_zra =   9.13905795E-04
        real, parameter :: xl =   2.26793579E+02
        integer, parameter :: ircp = -999
        integer, parameter :: istab = 1
        real, parameter :: xloc =   2.43999985E+02
        real, parameter :: xl100 =   5.91999939E+02
        real, parameter :: vw10 =   2.10983324E+00
        real, parameter :: pcoef =   3.05333316E-01
        real, parameter :: virty =   3.81971851E+03
        real, parameter :: radius =   9.94035767E+02
        real, parameter :: ra_rcp_4 =   2.44737968E+01
        real, parameter :: ra_rcp_zrcp =   2.44737968E+01
        real, parameter :: rc_eff_rcp_4_pos =   1.03781860E+03
        real, parameter :: rb_rcp =   2.76735859E+01
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: ol_src =  -3.79837418E+01
        real, parameter :: uster_src =   2.54701376E-01
        real, parameter :: htot =   5.00000000E+00
        real, parameter :: ra_src_4 =   2.43657837E+01
        real, parameter :: rb_src =   2.83567886E+01
        real, parameter :: rc_eff_src_4_pos =   1.03781860E+03
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: vd_trj_z0 =   9.37931938E-04
        real, parameter :: onder =   1.00000000E+00
        integer, parameter :: flag = 0
        real, parameter :: vchem =   3.03287721E+00
        real, parameter :: vnatpri =   1.16256726E-09
        real, parameter :: diameter =   1.77304956E+03
        real, parameter :: dispg(6) = (/   3.54144454E-01,   4.67322886E-01,   1.63794056E-01,   2.25280777E-01, &
             9.69264284E-02,   8.87324288E-02/)
        real, parameter :: zm =   4.00000000E+00
        real, parameter :: in_cgt =   9.35822725E-03
        real, parameter :: in_cgt_z =   9.35822725E-03
        real, parameter :: ref_cgt =   2.59155466E-04
        real, parameter :: ref_cgt_z =   2.59155466E-04
        real, parameter :: ref_cdn =   1.00000000E+00
        real, parameter :: ref_ugem =   2.02963543E+00
        real, parameter :: ref_hf =   5.00000000E+00
        real, parameter :: ref_cq1 =   9.91869271E-01
        real, parameter :: ref_cq2 =   1.00000000E+00
        real, parameter :: ref_uxr =   2.22042251E+00
        real, parameter :: ref_zu =   1.40129846E-45
        real, parameter :: ref_sigzr =   2.23733616E+01
        real, parameter :: ref_dxeff =   4.42291870E+02
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call test_ops_brondepl( do_proc, disx, disxx, xg, c, ux0, ueff, sigz, &
             vd_eff_trj_zra, xl, ircp, istab, xloc, xl100, vw10, pcoef, virty, radius, &
             ra_rcp_4, ra_rcp_zrcp, rc_eff_rcp_4_pos, rb_rcp, z0_src, ol_src, &
             uster_src, htot, ra_src_4, rb_src, rc_eff_src_4_pos, qbstf, &
             vd_trj_z0, onder, flag, vchem, vnatpri, diameter, dispg, zm, &
             in_cgt, in_cgt_z, ref_cgt, ref_cgt_z, ref_cdn, ref_ugem, ref_hf, &
             ref_cq1, ref_cq2, ref_uxr, ref_zu, ref_sigzr, ref_dxeff, &
             __LINE__, __FILE__)
   end subroutine test_ops_brondepl8

   subroutine test_ops_brondepl7()
   use m_ops_tdo_proc, only: tdo_proc
        type(tdo_proc) :: do_proc
        real, parameter :: disx = -999.0
        real, parameter :: disxx =   9.82344116E+02
        real, parameter :: xg =   2.89221802E+03
        real, parameter :: c =   6.48795366E+00
        real, parameter :: ux0 =   1.83884811E+00
        real, parameter :: ueff =   2.22042251E+00
        real, parameter :: sigz =   2.23733616E+01
        real, parameter :: vd_eff_trj_zra =   9.13905795E-04
        real, parameter :: xl =   2.26793579E+02
        integer, parameter :: ircp = -999
        integer, parameter :: istab = 1
        real, parameter :: xloc =   2.43999985E+02
        real, parameter :: xl100 =   5.91999939E+02
        real, parameter :: vw10 =   2.10983324E+00
        real, parameter :: pcoef =   3.05333316E-01
        real, parameter :: virty =   3.81971851E+03
        real, parameter :: radius =   9.94035767E+02
        real, parameter :: ra_rcp_4 =   2.44737968E+01
        real, parameter :: ra_rcp_zrcp =   2.44737968E+01
        real, parameter :: rc_eff_rcp_4_pos =   1.03781860E+03
        real, parameter :: rb_rcp =   2.76735859E+01
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: ol_src =  -3.79837418E+01
        real, parameter :: uster_src =   2.54701376E-01
        real, parameter :: htot =   5.00000000E+00
        real, parameter :: ra_src_4 =   2.43657837E+01
        real, parameter :: rb_src =   2.83567886E+01
        real, parameter :: rc_eff_src_4_pos =   1.03781860E+03
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: vd_trj_z0 =   9.37931938E-04
        real, parameter :: onder =   1.00000000E+00
        integer, parameter :: flag = 0
        real, parameter :: vchem =   3.03287721E+00
        real, parameter :: vnatpri =   1.16256726E-09
        real, parameter :: diameter =   1.77304956E+03
        real, parameter :: dispg(6) = (/   3.54144454E-01,   4.67322886E-01,   1.63794056E-01,   2.25280777E-01, &
             9.69264284E-02,   8.87324288E-02/)
        real, parameter :: zm =   4.00000000E+00
        real, parameter :: in_cgt =   9.35822725E-03
        real, parameter :: in_cgt_z =   9.35822725E-03
        real, parameter :: ref_cgt =   2.59155466E-04
        real, parameter :: ref_cgt_z =   2.59155466E-04
        real, parameter :: ref_cdn =   1.00000000E+00
        real, parameter :: ref_ugem =   2.02963543E+00
        real, parameter :: ref_hf =   5.00000000E+00
        real, parameter :: ref_cq1 =   9.91869271E-01
        real, parameter :: ref_cq2 =   1.00000000E+00
        real, parameter :: ref_uxr =   2.22042251E+00
        real, parameter :: ref_zu =   1.40129846E-45
        real, parameter :: ref_sigzr =   2.23733616E+01
        real, parameter :: ref_dxeff =   4.42291870E+02
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call test_ops_brondepl( do_proc, disx, disxx, xg, c, ux0, ueff, sigz, &
             vd_eff_trj_zra, xl, ircp, istab, xloc, xl100, vw10, pcoef, virty, radius, &
             ra_rcp_4, ra_rcp_zrcp, rc_eff_rcp_4_pos, rb_rcp, z0_src, ol_src, &
             uster_src, htot, ra_src_4, rb_src, rc_eff_src_4_pos, qbstf, &
             vd_trj_z0, onder, flag, vchem, vnatpri, diameter, dispg, zm, &
             in_cgt, in_cgt_z, ref_cgt, ref_cgt_z, ref_cdn, ref_ugem, ref_hf, &
             ref_cq1, ref_cq2, ref_uxr, ref_zu, ref_sigzr, ref_dxeff, &
             __LINE__, __FILE__)
   end subroutine test_ops_brondepl7

   subroutine test_ops_brondepl6()
   use m_ops_tdo_proc, only: tdo_proc
        type(tdo_proc) :: do_proc
        real, parameter :: disx = -999.0
        real, parameter :: disxx =   9.82344116E+02
        real, parameter :: xg =   2.89221802E+03
        real, parameter :: c =   6.48795366E+00
        real, parameter :: ux0 =   1.83884811E+00
        real, parameter :: ueff =   2.22042251E+00
        real, parameter :: sigz =   2.23733616E+01
        real, parameter :: vd_eff_trj_zra =   9.13905795E-04
        real, parameter :: xl =   2.26793579E+02
        integer, parameter :: ircp = -999
        integer, parameter :: istab = 1
        real, parameter :: xloc =   2.43999985E+02
        real, parameter :: xl100 =   5.91999939E+02
        real, parameter :: vw10 =   2.10983324E+00
        real, parameter :: pcoef =   3.05333316E-01
        real, parameter :: virty =   3.81971851E+03
        real, parameter :: radius =   9.94035767E+02
        real, parameter :: ra_rcp_4 =   2.44737968E+01
        real, parameter :: ra_rcp_zrcp =   2.44737968E+01
        real, parameter :: rc_eff_rcp_4_pos =   1.03781860E+03
        real, parameter :: rb_rcp =   2.76735859E+01
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: ol_src =  -3.79837418E+01
        real, parameter :: uster_src =   2.54701376E-01
        real, parameter :: htot =   5.00000000E+00
        real, parameter :: ra_src_4 =   2.43657837E+01
        real, parameter :: rb_src =   2.83567886E+01
        real, parameter :: rc_eff_src_4_pos =   1.03781860E+03
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: vd_trj_z0 =   9.37931938E-04
        real, parameter :: onder =   1.00000000E+00
        integer, parameter :: flag = 0
        real, parameter :: vchem =   3.03287721E+00
        real, parameter :: vnatpri =   1.16256726E-09
        real, parameter :: diameter =   1.77304956E+03
        real, parameter :: dispg(6) = (/   3.54144454E-01,   4.67322886E-01,   1.63794056E-01,   2.25280777E-01, &
             9.69264284E-02,   8.87324288E-02/)
        real, parameter :: zm =   4.00000000E+00
        real, parameter :: in_cgt =   9.35822725E-03
        real, parameter :: in_cgt_z =   9.35822725E-03
        real, parameter :: ref_cgt =   2.59155466E-04
        real, parameter :: ref_cgt_z =   2.59155466E-04
        real, parameter :: ref_cdn =   1.00000000E+00
        real, parameter :: ref_ugem =   2.02963543E+00
        real, parameter :: ref_hf =   5.00000000E+00
        real, parameter :: ref_cq1 =   9.91869271E-01
        real, parameter :: ref_cq2 =   1.00000000E+00
        real, parameter :: ref_uxr =   2.22042251E+00
        real, parameter :: ref_zu =   1.40129846E-45
        real, parameter :: ref_sigzr =   2.23733616E+01
        real, parameter :: ref_dxeff =   4.42291870E+02
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call test_ops_brondepl( do_proc, disx, disxx, xg, c, ux0, ueff, sigz, &
             vd_eff_trj_zra, xl, ircp, istab, xloc, xl100, vw10, pcoef, virty, radius, &
             ra_rcp_4, ra_rcp_zrcp, rc_eff_rcp_4_pos, rb_rcp, z0_src, ol_src, &
             uster_src, htot, ra_src_4, rb_src, rc_eff_src_4_pos, qbstf, &
             vd_trj_z0, onder, flag, vchem, vnatpri, diameter, dispg, zm, &
             in_cgt, in_cgt_z, ref_cgt, ref_cgt_z, ref_cdn, ref_ugem, ref_hf, &
             ref_cq1, ref_cq2, ref_uxr, ref_zu, ref_sigzr, ref_dxeff, &
             __LINE__, __FILE__)
   end subroutine test_ops_brondepl6

   subroutine test_ops_brondepl5()
   use m_ops_tdo_proc, only: tdo_proc
        type(tdo_proc) :: do_proc
        real, parameter :: disx = -999.0
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: xg =   4.96315820E+03
        real, parameter :: c =   6.26347139E-02
        real, parameter :: ux0 =   1.85474563E+00
        real, parameter :: ueff =   4.36820602E+00
        real, parameter :: sigz =   6.57454468E+02
        real, parameter :: vd_eff_trj_zra =   1.79345731E-03
        real, parameter :: xl =   2.79640625E+02
        integer, parameter :: ircp = -999
        integer, parameter :: istab = 1
        real, parameter :: xloc =   2.08999985E+02
        real, parameter :: xl100 =   6.21999939E+02
        real, parameter :: vw10 =   2.01143265E+00
        real, parameter :: pcoef =   2.94000000E-01
        real, parameter :: virty =   3.81971851E+03
        real, parameter :: radius =   9.94035767E+02
        real, parameter :: ra_rcp_4 =   2.43878994E+01
        real, parameter :: ra_rcp_zrcp =   2.43878994E+01
        real, parameter :: rc_eff_rcp_4_pos =   5.38726685E+02
        real, parameter :: rb_rcp =   2.87707176E+01
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: ol_src =  -3.51529274E+01
        real, parameter :: uster_src =   2.58591831E-01
        real, parameter :: htot =   5.00000000E+00
        real, parameter :: ra_src_4 =   2.37326260E+01
        real, parameter :: rb_src =   2.79301662E+01
        real, parameter :: rc_eff_src_4_pos =   5.38726685E+02
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: vd_trj_z0 =   1.76473637E-03
        real, parameter :: onder =   1.00000000E+00
        integer, parameter :: flag = 0
        real, parameter :: vchem =   3.03763580E+00
        real, parameter :: vnatpri =   1.53959602E-01
        real, parameter :: diameter =   1.77304956E+03
        real, parameter :: dispg(6) = (/   1.94816008E-01,   2.80000001E-01,   2.00000003E-01,   2.00000003E-01, &
             1.19999997E-01,   2.00000003E-01/)
        real, parameter :: zm =   4.00000000E+00
        real, parameter :: in_cgt =   1.57241225E-02
        real, parameter :: in_cgt_z =   1.57241225E-02
        real, parameter :: ref_cgt =   1.48439445E-02
        real, parameter :: ref_cgt_z =   1.48439445E-02
        real, parameter :: ref_cdn =   9.76525307E-01
        real, parameter :: ref_ugem =   3.31581259E+00
        real, parameter :: ref_hf =   1.24740782E+01
        real, parameter :: ref_cq1 =   9.94082332E-01
        real, parameter :: ref_cq2 =   9.82553720E-01
        real, parameter :: ref_uxr =   2.26341939E+00
        real, parameter :: ref_zu =   1.39820312E+02
        real, parameter :: ref_sigzr =   5.57926254E+01
        real, parameter :: ref_dxeff =   4.42506378E+02
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call test_ops_brondepl( do_proc, disx, disxx, xg, c, ux0, ueff, sigz, &
             vd_eff_trj_zra, xl, ircp, istab, xloc, xl100, vw10, pcoef, virty, radius, &
             ra_rcp_4, ra_rcp_zrcp, rc_eff_rcp_4_pos, rb_rcp, z0_src, ol_src, &
             uster_src, htot, ra_src_4, rb_src, rc_eff_src_4_pos, qbstf, &
             vd_trj_z0, onder, flag, vchem, vnatpri, diameter, dispg, zm, &
             in_cgt, in_cgt_z, ref_cgt, ref_cgt_z, ref_cdn, ref_ugem, ref_hf, &
             ref_cq1, ref_cq2, ref_uxr, ref_zu, ref_sigzr, ref_dxeff, &
             __LINE__, __FILE__)
   end subroutine test_ops_brondepl5

   subroutine test_ops_brondepl4()
   use m_ops_tdo_proc, only: tdo_proc

        type(tdo_proc) :: do_proc
        real, parameter :: disx = -999.0
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: xg =   4.96315820E+03
        real, parameter :: c =   6.26347139E-02
        real, parameter :: ux0 =   1.85474563E+00
        real, parameter :: ueff =   4.36820602E+00
        real, parameter :: sigz =   6.57454468E+02
        real, parameter :: vd_eff_trj_zra =   1.79345731E-03
        real, parameter :: xl =   2.79640625E+02
        integer, parameter :: ircp = -999
        integer, parameter :: istab = 1
        real, parameter :: xloc =   2.08999985E+02
        real, parameter :: xl100 =   6.21999939E+02
        real, parameter :: vw10 =   2.01143265E+00
        real, parameter :: pcoef =   2.94000000E-01
        real, parameter :: virty =   3.81971851E+03
        real, parameter :: radius =   9.94035767E+02
        real, parameter :: ra_rcp_4 =   2.43878994E+01
        real, parameter :: ra_rcp_zrcp =   2.43878994E+01
        real, parameter :: rc_eff_rcp_4_pos =   5.38726685E+02
        real, parameter :: rb_rcp =   2.87707176E+01
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: ol_src =  -3.51529274E+01
        real, parameter :: uster_src =   2.58591831E-01
        real, parameter :: htot =   5.00000000E+00
        real, parameter :: ra_src_4 =   2.37326260E+01
        real, parameter :: rb_src =   2.79301662E+01
        real, parameter :: rc_eff_src_4_pos =   5.38726685E+02
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: vd_trj_z0 =   1.76473637E-03
        real, parameter :: onder =   1.00000000E+00
        integer, parameter :: flag = 0
        real, parameter :: vchem =   3.03763580E+00
        real, parameter :: vnatpri =   1.53959602E-01
        real, parameter :: diameter =   1.77304956E+03
        real, parameter :: dispg(6) = (/   1.94816008E-01,   2.80000001E-01,   2.00000003E-01,   2.00000003E-01, &
             1.19999997E-01,   2.00000003E-01/)
        real, parameter :: zm =   4.00000000E+00
        real, parameter :: in_cgt =   1.57241225E-02
        real, parameter :: in_cgt_z =   1.57241225E-02
        real, parameter :: ref_cgt =   1.48439445E-02
        real, parameter :: ref_cgt_z =   1.48439445E-02
        real, parameter :: ref_cdn =   9.76525307E-01
        real, parameter :: ref_ugem =   3.31581259E+00
        real, parameter :: ref_hf =   1.24740782E+01
        real, parameter :: ref_cq1 =   9.94082332E-01
        real, parameter :: ref_cq2 =   9.82553720E-01
        real, parameter :: ref_uxr =   2.26341939E+00
        real, parameter :: ref_zu =   1.39820312E+02
        real, parameter :: ref_sigzr =   5.57926254E+01
        real, parameter :: ref_dxeff =   4.42506378E+02
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call test_ops_brondepl( do_proc, disx, disxx, xg, c, ux0, ueff, sigz, &
             vd_eff_trj_zra, xl, ircp, istab, xloc, xl100, vw10, pcoef, virty, radius, &
             ra_rcp_4, ra_rcp_zrcp, rc_eff_rcp_4_pos, rb_rcp, z0_src, ol_src, &
             uster_src, htot, ra_src_4, rb_src, rc_eff_src_4_pos, qbstf, &
             vd_trj_z0, onder, flag, vchem, vnatpri, diameter, dispg, zm, &
             in_cgt, in_cgt_z, ref_cgt, ref_cgt_z, ref_cdn, ref_ugem, ref_hf, &
             ref_cq1, ref_cq2, ref_uxr, ref_zu, ref_sigzr, ref_dxeff, &
             __LINE__, __FILE__)
   end subroutine test_ops_brondepl4

   subroutine test_ops_brondepl3()
   use m_ops_tdo_proc, only: tdo_proc

        type(tdo_proc) :: do_proc
        real, parameter :: disx = -999.0
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: xg =   2.06896621E+04
        real, parameter :: c =   2.72247706E-19
        real, parameter :: ux0 =   2.75813675E+00
        real, parameter :: ueff =   2.72372031E+00
        real, parameter :: sigz =   5.33004904E+00
        real, parameter :: vd_eff_trj_zra =   2.96495913E-04
        real, parameter :: xl =   5.02630997E+01
        integer, parameter :: ircp = -999
        integer, parameter :: istab = 5
        real, parameter :: xloc =   3.79999962E+01
        real, parameter :: xl100 =   9.59999924E+01
        real, parameter :: vw10 =   1.14800286E+00
        real, parameter :: pcoef =   4.79999989E-01
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: ra_rcp_4 =   1.94800186E+02
        real, parameter :: ra_rcp_zrcp =   1.94800186E+02
        real, parameter :: rc_eff_rcp_4_pos =   1.62684412E+03
        real, parameter :: rb_rcp =   1.20375221E+02
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: ol_src =   1.00844126E+01
        real, parameter :: uster_src =   5.99999987E-02
        real, parameter :: htot =   5.00000000E+01
        real, parameter :: ra_src_4 =   2.00794220E+02
        real, parameter :: rb_src =   1.20375221E+02
        real, parameter :: rc_eff_src_4_pos =   1.62684436E+03
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: vd_trj_z0 =   5.72337885E-04
        real, parameter :: onder =   5.05234420E-01
        integer, parameter :: flag = 1
        real, parameter :: vchem =   2.04107952E+00
        real, parameter :: vnatpri =   5.31854868E-01
        real, parameter :: diameter =   0.00000000E+00
        real, parameter :: dispg(6) = (/   4.94118273E-01,   4.66203898E-01,   1.13315932E-01,   1.24283135E-01, &
             6.74336217E-03,   2.00000003E-01/)
        real, parameter :: zm =   4.00000000E+00
        real, parameter :: in_cgt =   2.20134437E-01
        real, parameter :: in_cgt_z =   2.20134437E-01
        real, parameter :: ref_cgt =   1.53758690E-01
        real, parameter :: ref_cgt_z =   1.53758690E-01
        real, parameter :: ref_cdn =   9.99019742E-01
        real, parameter :: ref_ugem =   2.27016377E+00
        real, parameter :: ref_hf =   2.50000000E+01
        real, parameter :: ref_cq1 =   1.00000000E+00
        real, parameter :: ref_cq2 =   1.00000000E+00
        real, parameter :: ref_uxr =   2.72372031E+00
        real, parameter :: ref_zu =   1.40129846E-45
        real, parameter :: ref_sigzr =   0.00000000E+00
        real, parameter :: ref_dxeff =   0.00000000E+00
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call test_ops_brondepl( do_proc, disx, disxx, xg, c, ux0, ueff, sigz, &
             vd_eff_trj_zra, xl, ircp, istab, xloc, xl100, vw10, pcoef, virty, radius, &
             ra_rcp_4, ra_rcp_zrcp, rc_eff_rcp_4_pos, rb_rcp, z0_src, ol_src, &
             uster_src, htot, ra_src_4, rb_src, rc_eff_src_4_pos, qbstf, &
             vd_trj_z0, onder, flag, vchem, vnatpri, diameter, dispg, zm, &
             in_cgt, in_cgt_z, ref_cgt, ref_cgt_z, ref_cdn, ref_ugem, ref_hf, &
             ref_cq1, ref_cq2, ref_uxr, ref_zu, ref_sigzr, ref_dxeff, &
             __LINE__, __FILE__)
   end subroutine test_ops_brondepl3

   subroutine test_ops_brondepl2()
   use m_ops_tdo_proc, only: tdo_proc
        type(tdo_proc) :: do_proc
        real, parameter :: disx = -999.0
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: xg =   5.43321328E+04
        real, parameter :: c =   9.30537879E-02
        real, parameter :: ux0 =   2.71924520E+00
        real, parameter :: ueff =   5.62392044E+00
        real, parameter :: sigz =   1.37633621E+02
        real, parameter :: vd_eff_trj_zra =   1.18779764E-02
        real, parameter :: xl =   2.85942810E+02
        integer, parameter :: ircp = -999
        integer, parameter :: istab = 3
        real, parameter :: xloc =   2.81999969E+02
        real, parameter :: xl100 =   2.81999969E+02
        real, parameter :: vw10 =   3.46342969E+00
        real, parameter :: pcoef =   3.54666650E-01
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: ra_rcp_4 =   2.72117901E+01
        real, parameter :: ra_rcp_zrcp =   2.72117901E+01
        real, parameter :: rc_eff_rcp_4_pos =   1.30000000E+01
        real, parameter :: rb_rcp =   2.44440670E+01
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: ol_src =   1.39790878E+02
        real, parameter :: uster_src =   3.21115643E-01
        real, parameter :: htot =   5.00000000E+00
        real, parameter :: ra_src_4 =   2.44179420E+01
        real, parameter :: rb_src =   2.24919357E+01
        real, parameter :: rc_eff_src_4_pos =   1.30000000E+01
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: vd_trj_z0 =   2.81754136E-02
        real, parameter :: onder =   1.00000000E+00
        integer, parameter :: flag = 0
        real, parameter :: vchem =   0.00000000E+00
        real, parameter :: vnatpri =   3.92703915E+00
        real, parameter :: diameter =   0.00000000E+00
        real, parameter :: dispg(6) = (/   2.14678213E-01,   5.00924468E-01,   7.10568428E-02,   2.00000003E-01, &
             1.19999997E-01,   2.00000003E-01/)
        real, parameter :: zm =   4.00000000E+00
        real, parameter :: in_cgt =   3.93019617E-01
        real, parameter :: in_cgt_z =   3.93019617E-01
        real, parameter :: ref_cgt =   3.93019617E-01
        real, parameter :: ref_cgt_z =   3.93019617E-01
        real, parameter :: ref_cdn =   1.00000000E+00
        real, parameter :: ref_ugem =   4.17158270E+00
        real, parameter :: ref_hf =   5.00000000E+00
        real, parameter :: ref_cq1 =   1.00000000E+00
        real, parameter :: ref_cq2 =   3.83049458E-01
        real, parameter :: ref_uxr =   5.62392044E+00
        real, parameter :: ref_zu =   1.40129846E-45
        real, parameter :: ref_sigzr =   0.00000000E+00
        real, parameter :: ref_dxeff =   0.00000000E+00
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call test_ops_brondepl( do_proc, disx, disxx, xg, c, ux0, ueff, sigz, &
             vd_eff_trj_zra, xl, ircp, istab, xloc, xl100, vw10, pcoef, virty, radius, &
             ra_rcp_4, ra_rcp_zrcp, rc_eff_rcp_4_pos, rb_rcp, z0_src, ol_src, &
             uster_src, htot, ra_src_4, rb_src, rc_eff_src_4_pos, qbstf, &
             vd_trj_z0, onder, flag, vchem, vnatpri, diameter, dispg, zm, &
             in_cgt, in_cgt_z, ref_cgt, ref_cgt_z, ref_cdn, ref_ugem, ref_hf, &
             ref_cq1, ref_cq2, ref_uxr, ref_zu, ref_sigzr, ref_dxeff, &
             __LINE__, __FILE__)
   end subroutine test_ops_brondepl2

   subroutine test_ops_brondepl1()
   use m_ops_tdo_proc, only: tdo_proc
        type(tdo_proc) :: do_proc
        real, parameter :: disx = -999.0
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: xg =   4.40899365E+03
        real, parameter :: c =   6.86089471E-02
        real, parameter :: ux0 =   1.80278969E+00
        real, parameter :: ueff =   4.44325399E+00
        real, parameter :: sigz =   7.55815918E+02
        real, parameter :: vd_eff_trj_zra =   1.41225327E-02
        real, parameter :: xl =   2.96321716E+02
        integer, parameter :: ircp = -999
        integer, parameter :: istab = 1
        real, parameter :: xloc =   2.08999985E+02
        real, parameter :: xl100 =   6.21999939E+02
        real, parameter :: vw10 =   2.01143265E+00
        real, parameter :: pcoef =   2.94000000E-01
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: ra_rcp_4 =   2.43975487E+01
        real, parameter :: ra_rcp_zrcp =   2.43975487E+01
        real, parameter :: rc_eff_rcp_4_pos =   1.30000000E+01
        real, parameter :: rb_rcp =   3.01502495E+01
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: ol_src =  -3.29010468E+01
        real, parameter :: uster_src =   2.52821952E-01
        real, parameter :: htot =   5.00000000E+00
        real, parameter :: ra_src_4 =   2.40340176E+01
        real, parameter :: rb_src =   2.85675869E+01
        real, parameter :: rc_eff_src_4_pos =   1.30000000E+01
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: vd_trj_z0 =   2.40572076E-02
        real, parameter :: onder =   1.00000000E+00
        integer, parameter :: flag = 0
        real, parameter :: vchem =   0.00000000E+00
        real, parameter :: vnatpri =   3.32806897E+00
        real, parameter :: diameter =   0.00000000E+00
        real, parameter :: dispg(6) = (/   2.14678213E-01,   2.80000001E-01,   2.00000003E-01,   2.00000003E-01, &
             1.19999997E-01,   2.00000003E-01/)
        real, parameter :: zm =   4.00000000E+00
        real, parameter :: in_cgt =   1.15066350E-01
        real, parameter :: in_cgt_z =   1.15066350E-01
        real, parameter :: ref_cgt =   1.15066350E-01
        real, parameter :: ref_cgt_z =   1.15066350E-01
        real, parameter :: ref_cdn =   8.35698187E-01
        real, parameter :: ref_ugem =   2.95615840E+00
        real, parameter :: ref_hf =   1.13604561E+02
        real, parameter :: ref_cq1 =   1.00000000E+00
        real, parameter :: ref_cq2 =   7.62434900E-01
        real, parameter :: ref_uxr =   4.44325399E+00
        real, parameter :: ref_zu =   1.40129846E-45
        real, parameter :: ref_sigzr =   0.00000000E+00
        real, parameter :: ref_dxeff =   0.00000000E+00
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call test_ops_brondepl( do_proc, disx, disxx, xg, c, ux0, ueff, sigz, &
             vd_eff_trj_zra, xl, ircp, istab, xloc, xl100, vw10, pcoef, virty, radius, &
             ra_rcp_4, ra_rcp_zrcp, rc_eff_rcp_4_pos, rb_rcp, z0_src, ol_src, &
             uster_src, htot, ra_src_4, rb_src, rc_eff_src_4_pos, qbstf, &
             vd_trj_z0, onder, flag, vchem, vnatpri, diameter, dispg, zm, &
             in_cgt, in_cgt_z, ref_cgt, ref_cgt_z, ref_cdn, ref_ugem, ref_hf, &
             ref_cq1, ref_cq2, ref_uxr, ref_zu, ref_sigzr, ref_dxeff, &
             __LINE__, __FILE__)
   end subroutine test_ops_brondepl1

   subroutine test_ops_brondepl( do_proc, disx, disxx, xg, c, ux0, ueff, sigz, &
       vd_eff_trj_zra, xl, ircp, istab, xloc, xl100, vw10, pcoef, virty, radius, &
       ra_rcp_4, ra_rcp_zrcp, rc_eff_rcp_4_pos, rb_rcp, z0_src, ol_src, &
       uster_src, htot, ra_src_4, rb_src, rc_eff_src_4_pos, qbstf, &
       vd_trj_z0, onder, flag, vchem, vnatpri, diameter, dispg, zm, &
       in_cgt, in_cgt_z, ref_cgt, ref_cgt_z, ref_cdn, ref_ugem, ref_hf, &
       ref_cq1, ref_cq2, ref_uxr, ref_zu, ref_sigzr, ref_dxeff, &
       lineno, filename)
   use m_ops_brondepl
   use no_pfunit_ops_lt
   use m_ops_tdo_proc, only: tdo_proc
   use m_error, only: TError
   use m_ops_varin, only: Tvarin
   
        type(Tvarin) :: varin
        type(tdo_proc), intent(in) :: do_proc
        real,    intent(in) :: disx, disxx, xg, c, ux0, ueff, sigz, vd_eff_trj_zra, xl
        integer, intent(in) :: ircp
        integer, intent(in) :: istab
        real,    intent(in) :: xloc, xl100, vw10, pcoef, virty, radius, ra_rcp_4, &
           ra_rcp_zrcp, rc_eff_rcp_4_pos, rb_rcp, z0_src, ol_src, uster_src, htot, &
           ra_src_4, rb_src, rc_eff_src_4_pos, qbstf, vd_trj_z0, onder
        integer, intent(in) :: flag
        real,    intent(in) :: vchem, vnatpri, diameter, dispg(:), zm, in_cgt, in_cgt_z, &
           ref_cgt, ref_cgt_z, ref_cdn, ref_ugem, ref_hf, ref_cq1, ref_cq2, &
           ref_uxr, ref_zu, ref_sigzr, ref_dxeff
        integer, intent(in) :: lineno
        character(len=*), intent(in) :: filename

        real, parameter :: tol = 1e-5
        type(TError) :: error, ref_error 
        real :: dxeff, cgt, cgt_z, cdn, ugem, hf, cq1, cq2, uxr, zu, sigzr
        cgt = in_cgt
        cgt_z = in_cgt_z
        ref_error%haserror = .false.
        ref_error%message = ""
        zu = ref_zu
        uxr = ref_uxr
        sigzr = ref_sigzr
        hf = ref_hf
        call ops_brondepl( varin, do_proc, disx, disxx, xg, c, ux0, ueff, sigz, vd_eff_trj_zra, xl, ircp, istab, xloc, xl100, vw10, pcoef, virty, radius, zm, &
                     &  ra_rcp_4, ra_rcp_zrcp, rc_eff_rcp_4_pos, rb_rcp, z0_src, ol_src, uster_src, htot, ra_src_4, rb_src, rc_eff_src_4_pos, qbstf, &
                     &  vd_trj_z0, onder, flag, vchem, vnatpri, diameter, dispg(istab), cgt, cgt_z, cdn, ugem, hf, cq1, cq2, uxr, zu, &
                     &  sigzr, dxeff, error)
        call assertEqual( ref_cgt, cgt, tol, "cgt", lineno, filename)
        call assertEqual( ref_cgt_z, cgt_z, tol, "cgt_z", lineno, filename)
        call assertEqual(error, ref_error, "ops_brondepl", lineno, filename)
        call assertEqual( ref_cdn, cdn, tol, "cdn", lineno, filename)
        call assertEqual( ref_ugem, ugem, tol, "ugem", lineno, filename)
        call assertEqual( ref_hf, hf, tol, "hf", lineno, filename)
        call assertEqual( ref_cq1, cq1, tol, "cq1", lineno, filename)
        call assertEqual( ref_cq2, cq2, tol, "cq2", lineno, filename)
        call assertEqual( ref_uxr, uxr, tol, "uxr", lineno, filename)
        call assertRelativelyEqual( ref_zu, zu, tol, "zu", lineno, filename)
        call assertEqual( ref_sigzr, sigzr, tol, "sigzr", lineno, filename)
        call assertRelativelyEqual( ref_dxeff, dxeff, tol, "dxeff", lineno, filename)
   end subroutine test_ops_brondepl
end module m_test_ops_brondepl

program p_test_ops_brondepl
use m_test_ops_brondepl
use no_pfunit
implicit none
   call test_ops_brondepl9()
   call test_ops_brondepl8()
   call test_ops_brondepl7()
   call test_ops_brondepl6()
   call test_ops_brondepl5()
   call test_ops_brondepl4()
   call test_ops_brondepl3()
   call test_ops_brondepl2()
   call test_ops_brondepl1()
   call conclusion()
end program p_test_ops_brondepl

