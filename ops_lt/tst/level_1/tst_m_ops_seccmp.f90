module m_test_m_ops_seccmp
implicit none
contains
   subroutine test_ops_seccmp10()
   use m_ops_seccmp
   use no_pfunit_ops_lt
   use m_ops_varin, only: Tvarin_unc
   use m_ops_tdo_proc, only: tdo_proc
       real, parameter :: tol = 1e-5
        type(Tvarin_unc) :: varin_unc
        type(tdo_proc) :: do_proc
        real, parameter :: qbpri =   1.00000000E+00
        real, parameter :: ueff =   2.22042251E+00
        real, parameter :: rc_sec_trj =   1.66445114E+02
        real, parameter :: routsec =   1.40000000E+07
        real, parameter :: ccc =   6.48795366E+00
        real, parameter :: vv =   9.90206182E-01
        real, parameter :: amol1 =   4.60000000E+01
        real, parameter :: amol2 =   6.20000000E+01
        real, parameter :: xvg =   1.00000000E+00
        real, parameter :: sigz =   2.23733616E+01
        real, parameter :: grad =   9.90641773E-01, grad1 = 1.0
        real, parameter :: utr =   2.22042251E+00
        real, parameter :: radius =   9.94035767E+02
        real, parameter :: disxx =   9.82344116E+02
        real, parameter :: xl =   2.26793579E+02
        real, parameter :: xloc =   2.43999985E+02
        real, parameter :: vw10 =   2.10983324E+00
        real, parameter :: pcoef =   3.05333316E-01
        real, parameter :: virty =   3.81971851E+03
        real, parameter :: regenk =   3.08684688E-02
        real, parameter :: htot =   5.00000000E+00
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: twt =   2.19858810E-02
        real, parameter :: ri =   1.43999994E+00
        real, parameter :: cgt =   2.59155466E-04
        real, parameter :: xvghbr =   1.00078666E+00
        real, parameter :: xvglbr =   1.00363839E+00
        real, parameter :: vnatpri =   1.16256726E-09
        real, parameter :: vchem =   3.03287721E+00
        real, parameter :: ra_rcp_4 =   2.44737968E+01
        real, parameter :: ra_rcp_zra =   3.47703209E+01
        real, parameter :: rb_rcp =   2.76735859E+01
        real, parameter :: rc_sec_rcp =   1.63827576E+02
        real, parameter :: ra_trj_zra =   3.20103760E+01
        real, parameter :: rb_trj =   2.83567867E+01
        real :: pr
        real, parameter :: ref_pr =   4.81824827E-04
        real :: vnatsec
        real, parameter :: ref_vnatsec(2) =  (/ 1.75550699E+00,  0.851711273193 /)
        real :: cgtsec
        real, parameter :: ref_cgtsec(3) = (/  1.26016513E-03, 0.0, 4.550516605377E-02/)
        real :: qsec
        real, parameter :: ref_qsec(2) = (/  4.43630200E-03, 4.482293967158E-03 /)
        real :: consec
        real, parameter :: ref_consec(2) = (/  1.35856187E-02,  1.372646354139E-02 /)
        real, parameter :: in_vd_eff_trj_zra =   9.13905795E-04
        real :: vd_eff_trj_zra
        real, parameter :: ref_vd_eff_trj_zra =   9.13905795E-04
        real :: routsec2
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        vd_eff_trj_zra = in_vd_eff_trj_zra
        call ops_seccmp(varin_unc, do_proc, qbpri, ueff, rc_sec_trj, routsec, ccc, vv, amol1, amol2, xvg, sigz, grad, utr, radius, disxx, xl, xloc, vw10,  &
                   &  pcoef, virty, regenk, htot, onder, twt, ri, cgt, xvghbr, xvglbr, vnatpri, vchem, ra_rcp_4,  &
                   &  ra_rcp_zra, rb_rcp, rc_sec_rcp, pr, vnatsec, cgtsec, qsec, consec, vd_eff_trj_zra, ra_trj_zra, rb_trj)
        call assertEqual( ref_pr, pr, tol, "pr", __LINE__, __FILE__)
        call assertEqual( ref_vnatsec(1), vnatsec, tol, "vnatsec", __LINE__, __FILE__)
        call assertEqual( ref_cgtsec(1), cgtsec, tol, "cgtsec", __LINE__, __FILE__)
        call assertEqual( ref_qsec(1), qsec, tol, "qsec", __LINE__, __FILE__)
        call assertEqual( ref_consec(1), consec, tol, "consec", __LINE__, __FILE__)
        call assertEqual( ref_vd_eff_trj_zra, vd_eff_trj_zra, tol, "vd_eff_trj_zra", __LINE__, __FILE__)

        routsec2 = xl * 0.31*1.326*1000/(2*ri**.184)
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        vd_eff_trj_zra = in_vd_eff_trj_zra
        call ops_seccmp(varin_unc, do_proc, qbpri, ueff, rc_sec_trj, routsec2, ccc, vv, amol1, amol2, xvg, sigz, grad, utr, radius, disxx, xl, xloc, vw10,  &
                   &  pcoef, virty, regenk, htot, onder, twt, ri, cgt, xvghbr, xvglbr, vnatpri, vchem, ra_rcp_4,  &
                   &  ra_rcp_zra, rb_rcp, rc_sec_rcp, pr, vnatsec, cgtsec, qsec, consec, vd_eff_trj_zra, ra_trj_zra, rb_trj)
        call assertEqual( ref_pr, pr, tol, "pr", __LINE__, __FILE__)
        call assertEqual( ref_vnatsec(2), vnatsec, tol, "vnatsec", __LINE__, __FILE__)
        call assertEqual( ref_cgtsec(1), cgtsec, tol, "cgtsec", __LINE__, __FILE__)
        call assertEqual( ref_qsec(1), qsec, tol, "qsec", __LINE__, __FILE__)
        call assertEqual( ref_consec(1), consec, tol, "consec", __LINE__, __FILE__)
        call assertEqual( ref_vd_eff_trj_zra, vd_eff_trj_zra, tol, "vd_eff_trj_zra", __LINE__, __FILE__)

        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .false.
        vd_eff_trj_zra = in_vd_eff_trj_zra
        call ops_seccmp(varin_unc, do_proc, qbpri, ueff, rc_sec_trj, routsec, ccc, vv, amol1, amol2, xvg, sigz, grad, utr, radius, disxx, xl, xloc, vw10,  &
                   &  pcoef, virty, regenk, htot, onder, twt, ri, cgt, xvghbr, xvglbr, vnatpri, vchem, ra_rcp_4,  &
                   &  ra_rcp_zra, rb_rcp, rc_sec_rcp, pr, vnatsec, cgtsec, qsec, consec, vd_eff_trj_zra, ra_trj_zra, rb_trj)
        call assertEqual( ref_pr, pr, tol, "pr", __LINE__, __FILE__)
        call assertEqual( ref_vnatsec(1), vnatsec, tol, "vnatsec", __LINE__, __FILE__)
        call assertEqual( ref_cgtsec(2), cgtsec, tol, "cgtsec", __LINE__, __FILE__)
        call assertEqual( ref_qsec(1), qsec, tol, "qsec", __LINE__, __FILE__)
        call assertEqual( ref_consec(1), consec, tol, "consec", __LINE__, __FILE__)
        call assertEqual( ref_vd_eff_trj_zra, vd_eff_trj_zra, tol, "vd_eff_trj_zra", __LINE__, __FILE__)

        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        vd_eff_trj_zra = in_vd_eff_trj_zra
        call ops_seccmp(varin_unc, do_proc, qbpri, ueff, rc_sec_trj, routsec, ccc, vv, amol1, amol2, xvg, sigz, grad1, utr, radius, disxx, xl, xloc, vw10,  &
                   &  pcoef, virty, regenk, htot, onder, twt, ri, cgt, xvghbr, xvglbr, vnatpri, vchem, ra_rcp_4,  &
                   &  ra_rcp_zra, rb_rcp, rc_sec_rcp, pr, vnatsec, cgtsec, qsec, consec, vd_eff_trj_zra, ra_trj_zra, rb_trj)
        call assertEqual( ref_pr, pr, tol, "pr", __LINE__, __FILE__)
        call assertEqual( ref_vnatsec(1), vnatsec, tol, "vnatsec", __LINE__, __FILE__)
        call assertEqual( ref_cgtsec(3), cgtsec, tol, "cgtsec", __LINE__, __FILE__)
        call assertEqual( ref_qsec(1), qsec, tol, "qsec", __LINE__, __FILE__)
        call assertEqual( ref_consec(1), consec, tol, "consec", __LINE__, __FILE__)
        call assertEqual( ref_vd_eff_trj_zra, vd_eff_trj_zra, tol, "vd_eff_trj_zra", __LINE__, __FILE__)

   end subroutine test_ops_seccmp10

   subroutine test_ops_seccmp9()
   use m_ops_seccmp
   use no_pfunit_ops_lt
   use m_ops_varin, only: Tvarin_unc
   use m_ops_tdo_proc, only: tdo_proc
       real, parameter :: tol = 1e-5
        type(Tvarin_unc) :: varin_unc
        type(tdo_proc) :: do_proc
        real, parameter :: qbpri =   1.00000000E+00
        real, parameter :: ueff =   2.22042251E+00
        real, parameter :: rc_sec_trj =   1.66445114E+02
        real, parameter :: routsec =   1.40000000E+07
        real, parameter :: ccc =   6.48795366E+00
        real, parameter :: vv =   9.90206182E-01
        real, parameter :: amol1 =   4.60000000E+01
        real, parameter :: amol2 =   6.20000000E+01
        real, parameter :: xvg =   1.00000000E+00
        real, parameter :: sigz =   2.23733616E+01
        real, parameter :: grad =   9.90641773E-01
        real, parameter :: utr =   2.22042251E+00
        real, parameter :: radius =   9.94035767E+02
        real, parameter :: disxx =   9.82344116E+02
        real, parameter :: xl =   2.26793579E+02
        real, parameter :: xloc =   2.43999985E+02
        real, parameter :: vw10 =   2.10983324E+00
        real, parameter :: pcoef =   3.05333316E-01
        real, parameter :: virty =   3.81971851E+03
        real, parameter :: regenk =   3.08684688E-02
        real, parameter :: htot =   5.00000000E+00
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: twt =   2.19858810E-02
        real, parameter :: ri =   1.43999994E+00
        real, parameter :: cgt =   2.59155466E-04
        real, parameter :: xvghbr =   1.00078666E+00
        real, parameter :: xvglbr =   1.00363839E+00
        real, parameter :: vnatpri =   1.16256726E-09
        real, parameter :: vchem =   3.03287721E+00
        real, parameter :: ra_rcp_4 =   2.44737968E+01
        real, parameter :: ra_rcp_zra =   3.47703209E+01
        real, parameter :: rb_rcp =   2.76735859E+01
        real, parameter :: rc_sec_rcp =   1.63827576E+02
        real, parameter :: ra_trj_zra =   3.20103760E+01
        real, parameter :: rb_trj =   2.83567867E+01
        real :: pr
        real, parameter :: ref_pr =   4.81824827E-04
        real :: vnatsec
        real, parameter :: ref_vnatsec =   1.75550699E+00
        real :: cgtsec
        real, parameter :: ref_cgtsec =   1.26016513E-03
        real :: qsec
        real, parameter :: ref_qsec =   4.43630200E-03
        real :: consec
        real, parameter :: ref_consec =   1.35856187E-02
        real, parameter :: in_vd_eff_trj_zra =   9.13905795E-04
        real :: vd_eff_trj_zra
        real, parameter :: ref_vd_eff_trj_zra =   9.13905795E-04
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        vd_eff_trj_zra = in_vd_eff_trj_zra
        call ops_seccmp(varin_unc, do_proc, qbpri, ueff, rc_sec_trj, routsec, ccc, vv, amol1, amol2, xvg, sigz, grad, utr, radius, disxx, xl, xloc, vw10,  &
                   &  pcoef, virty, regenk, htot, onder, twt, ri, cgt, xvghbr, xvglbr, vnatpri, vchem, ra_rcp_4,  &
                   &  ra_rcp_zra, rb_rcp, rc_sec_rcp, pr, vnatsec, cgtsec, qsec, consec, vd_eff_trj_zra, ra_trj_zra, rb_trj)
        call assertEqual( ref_pr, pr, tol, "pr", __LINE__, __FILE__)
        call assertEqual( ref_vnatsec, vnatsec, tol, "vnatsec", __LINE__, __FILE__)
        call assertEqual( ref_cgtsec, cgtsec, tol, "cgtsec", __LINE__, __FILE__)
        call assertEqual( ref_qsec, qsec, tol, "qsec", __LINE__, __FILE__)
        call assertEqual( ref_consec, consec, tol, "consec", __LINE__, __FILE__)
        call assertEqual( ref_vd_eff_trj_zra, vd_eff_trj_zra, tol, "vd_eff_trj_zra", __LINE__, __FILE__)
   end subroutine test_ops_seccmp9

   subroutine test_ops_seccmp8()
   use m_ops_seccmp
   use no_pfunit_ops_lt
   use m_ops_varin, only: Tvarin_unc
   use m_ops_tdo_proc, only: tdo_proc
       real, parameter :: tol = 1e-5
        type(Tvarin_unc) :: varin_unc
        type(tdo_proc) :: do_proc
        real, parameter :: qbpri =   1.00000000E+00
        real, parameter :: ueff =   5.54939318E+00
        real, parameter :: rc_sec_trj =   2.20521317E+02
        real, parameter :: routsec =   1.40000000E+07
        real, parameter :: ccc =   8.10841024E-02
        real, parameter :: vv =   9.33501840E-01
        real, parameter :: amol1 =   4.60000000E+01
        real, parameter :: amol2 =   6.20000000E+01
        real, parameter :: xvg =   1.00000000E+00
        real, parameter :: sigz =   1.35575409E+02
        real, parameter :: grad =   9.68629777E-01
        real, parameter :: utr =   4.30189896E+00
        real, parameter :: radius =   9.94035767E+02
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: xl =   2.86299438E+02
        real, parameter :: xloc =   2.81999969E+02
        real, parameter :: vw10 =   3.46342969E+00
        real, parameter :: pcoef =   3.54666650E-01
        real, parameter :: virty =   3.81971851E+03
        real, parameter :: regenk =   6.19761683E-02
        real, parameter :: htot =   5.00000000E+00
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: twt =   3.22588056E-01
        real, parameter :: ri =   1.35000002E+00
        real, parameter :: cgt =   1.96368527E-02
        real, parameter :: xvghbr =   9.61942077E-01
        real, parameter :: xvglbr =   1.01691461E+00
        real, parameter :: vnatpri =   3.64702828E-02
        real, parameter :: vchem =   1.57000470E+00
        real, parameter :: ra_rcp_4 =   2.71470547E+01
        real, parameter :: ra_rcp_zra =   6.77638016E+01
        real, parameter :: rb_rcp =   2.45075760E+01
        real, parameter :: rc_sec_rcp =   1.92928848E+02
        real, parameter :: ra_trj_zra =   4.87524376E+01
        real, parameter :: rb_trj =   2.24378204E+01
        real :: pr
        real, parameter :: ref_pr =   1.07577607E-01
        real :: vnatsec
        real, parameter :: ref_vnatsec =   4.73849964E+00
        real :: cgtsec
        real, parameter :: ref_cgtsec =   8.91475901E-02
        real :: qsec
        real, parameter :: ref_qsec =   2.58038510E-02
        real :: consec
        real, parameter :: ref_consec =   2.09278823E-03
        real, parameter :: in_vd_eff_trj_zra =   7.98411085E-04
        real :: vd_eff_trj_zra
        real, parameter :: ref_vd_eff_trj_zra =   7.98411085E-04
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        vd_eff_trj_zra = in_vd_eff_trj_zra
        call ops_seccmp(varin_unc, do_proc, qbpri, ueff, rc_sec_trj, routsec, ccc, vv, amol1, amol2, xvg, sigz, grad, utr, radius, disxx, xl, xloc, vw10,  &
                   &  pcoef, virty, regenk, htot, onder, twt, ri, cgt, xvghbr, xvglbr, vnatpri, vchem, ra_rcp_4,  &
                   &  ra_rcp_zra, rb_rcp, rc_sec_rcp, pr, vnatsec, cgtsec, qsec, consec, vd_eff_trj_zra, ra_trj_zra, rb_trj)
        call assertEqual( ref_pr, pr, tol, "pr", __LINE__, __FILE__)
        call assertEqual( ref_vnatsec, vnatsec, tol, "vnatsec", __LINE__, __FILE__)
        call assertEqual( ref_cgtsec, cgtsec, tol, "cgtsec", __LINE__, __FILE__)
        call assertEqual( ref_qsec, qsec, tol, "qsec", __LINE__, __FILE__)
        call assertEqual( ref_consec, consec, tol, "consec", __LINE__, __FILE__)
        call assertEqual( ref_vd_eff_trj_zra, vd_eff_trj_zra, tol, "vd_eff_trj_zra", __LINE__, __FILE__)
   end subroutine test_ops_seccmp8

   subroutine test_ops_seccmp7()
   use m_ops_seccmp
   use no_pfunit_ops_lt
   use m_ops_varin, only: Tvarin_unc
   use m_ops_tdo_proc, only: tdo_proc
       real, parameter :: tol = 1e-5
        type(Tvarin_unc) :: varin_unc
        type(tdo_proc) :: do_proc
        real, parameter :: qbpri =   1.00000000E+00
        real, parameter :: ueff =   4.36820602E+00
        real, parameter :: rc_sec_trj =   1.70337524E+02
        real, parameter :: routsec =   1.40000000E+07
        real, parameter :: ccc =   6.26347139E-02
        real, parameter :: vv =   9.11775351E-01
        real, parameter :: amol1 =   4.60000000E+01
        real, parameter :: amol2 =   6.20000000E+01
        real, parameter :: xvg =   1.00000000E+00
        real, parameter :: sigz =   6.57454468E+02
        real, parameter :: grad =   9.84275877E-01
        real, parameter :: utr =   4.07167959E+00
        real, parameter :: radius =   9.94035767E+02
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: xl =   2.79640625E+02
        real, parameter :: xloc =   2.08999985E+02
        real, parameter :: vw10 =   2.01143265E+00
        real, parameter :: pcoef =   2.94000000E-01
        real, parameter :: virty =   3.81971851E+03
        real, parameter :: regenk =   2.30019055E-02
        real, parameter :: htot =   5.00000000E+00
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: twt =   4.48477149E-01
        real, parameter :: ri =   1.77999997E+00
        real, parameter :: cgt =   1.48439445E-02
        real, parameter :: xvghbr =   1.02960062E+00
        real, parameter :: xvglbr =   1.07823014E+00
        real, parameter :: vnatpri =   1.53959602E-01
        real, parameter :: vchem =   3.03763580E+00
        real, parameter :: ra_rcp_4 =   2.43878994E+01
        real, parameter :: ra_rcp_zra =   3.38434982E+01
        real, parameter :: rb_rcp =   2.87707176E+01
        real, parameter :: rc_sec_rcp =   1.41589478E+02
        real, parameter :: ra_trj_zra =   3.45451622E+01
        real, parameter :: rb_trj =   2.79301682E+01
        real :: pr
        real, parameter :: ref_pr =   9.13520694E-01
        real :: vnatsec
        real, parameter :: ref_vnatsec =   4.79869652E+00
        real :: cgtsec
        real, parameter :: ref_cgtsec =   4.37127352E-02
        real :: qsec
        real, parameter :: ref_qsec =   5.18524610E-02
        real :: consec
        real, parameter :: ref_consec =   3.24776396E-03
        real, parameter :: in_vd_eff_trj_zra =   1.79345731E-03
        real :: vd_eff_trj_zra
        real, parameter :: ref_vd_eff_trj_zra =   1.79345731E-03
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        vd_eff_trj_zra = in_vd_eff_trj_zra
        call ops_seccmp(varin_unc, do_proc, qbpri, ueff, rc_sec_trj, routsec, ccc, vv, amol1, amol2, xvg, sigz, grad, utr, radius, disxx, xl, xloc, vw10,  &
                   &  pcoef, virty, regenk, htot, onder, twt, ri, cgt, xvghbr, xvglbr, vnatpri, vchem, ra_rcp_4,  &
                   &  ra_rcp_zra, rb_rcp, rc_sec_rcp, pr, vnatsec, cgtsec, qsec, consec, vd_eff_trj_zra, ra_trj_zra, rb_trj)
        call assertEqual( ref_pr, pr, tol, "pr", __LINE__, __FILE__)
        call assertEqual( ref_vnatsec, vnatsec, tol, "vnatsec", __LINE__, __FILE__)
        call assertEqual( ref_cgtsec, cgtsec, tol, "cgtsec", __LINE__, __FILE__)
        call assertEqual( ref_qsec, qsec, tol, "qsec", __LINE__, __FILE__)
        call assertEqual( ref_consec, consec, tol, "consec", __LINE__, __FILE__)
        call assertEqual( ref_vd_eff_trj_zra, vd_eff_trj_zra, tol, "vd_eff_trj_zra", __LINE__, __FILE__)
   end subroutine test_ops_seccmp7

   subroutine test_ops_seccmp6()
   use m_ops_seccmp
   use no_pfunit_ops_lt
   use m_ops_varin, only: Tvarin_unc
   use m_ops_tdo_proc, only: tdo_proc
       real, parameter :: tol = 1e-5
        type(Tvarin_unc) :: varin_unc
        type(tdo_proc) :: do_proc
        real, parameter :: qbpri =   1.00000000E+00
        real, parameter :: ueff =   3.46691298E+00
        real, parameter :: rc_sec_trj =   1.57011548E+03
        real, parameter :: routsec =   1.40000000E+07
        real, parameter :: ccc =   0.00000000E+00
        real, parameter :: vv =   9.31268215E-01
        real, parameter :: amol1 =   4.60000000E+01
        real, parameter :: amol2 =   6.20000000E+01
        real, parameter :: xvg =   1.00000000E+00
        real, parameter :: sigz =   5.33004904E+00
        real, parameter :: grad =   7.80078471E-01
        real, parameter :: utr =   3.46691298E+00
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: xl =   8.00000000E+01
        real, parameter :: xloc =   3.79999962E+01
        real, parameter :: vw10 =   1.14800286E+00
        real, parameter :: pcoef =   4.79999989E-01
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: regenk =   3.55333388E-02
        real, parameter :: htot =   1.00000000E+02
        real, parameter :: onder =   0.00000000E+00
        real, parameter :: twt =   5.86001635E-01
        real, parameter :: ri =   1.25999999E+00
        real, parameter :: cgt =   0.00000000E+00
        real, parameter :: xvghbr =   7.88567185E-01
        real, parameter :: xvglbr =   1.09725904E+00
        real, parameter :: vnatpri =   2.16914892E+00
        real, parameter :: vchem =   2.03441787E+00
        real, parameter :: ra_rcp_4 =   1.94800186E+02
        real, parameter :: ra_rcp_zra =   7.42978516E+02
        real, parameter :: rb_rcp =   1.20375221E+02
        real, parameter :: rc_sec_rcp =   1.28872827E+03
        real, parameter :: ra_trj_zra =   9.12402771E+02
        real, parameter :: rb_trj =   1.20375221E+02
        real :: pr
        real, parameter :: ref_pr =   6.44039750E-01
        real :: vnatsec
        real, parameter :: ref_vnatsec =   4.45002794E+00
        real :: cgtsec
        real, parameter :: ref_cgtsec =   0.00000000E+00
        real :: qsec
        real, parameter :: ref_qsec =   4.31687422E-02
        real :: consec
        real, parameter :: ref_consec =   5.12495194E-29
        real, parameter :: in_vd_eff_trj_zra =   2.96227430E-04
        real :: vd_eff_trj_zra
        real, parameter :: ref_vd_eff_trj_zra =   0.00000000E+00
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        vd_eff_trj_zra = in_vd_eff_trj_zra
        call ops_seccmp(varin_unc, do_proc, qbpri, ueff, rc_sec_trj, routsec, ccc, vv, amol1, amol2, xvg, sigz, grad, utr, radius, disxx, xl, xloc, vw10,  &
                   &  pcoef, virty, regenk, htot, onder, twt, ri, cgt, xvghbr, xvglbr, vnatpri, vchem, ra_rcp_4,  &
                   &  ra_rcp_zra, rb_rcp, rc_sec_rcp, pr, vnatsec, cgtsec, qsec, consec, vd_eff_trj_zra, ra_trj_zra, rb_trj)
        call assertEqual( ref_pr, pr, tol, "pr", __LINE__, __FILE__)
        call assertEqual( ref_vnatsec, vnatsec, tol, "vnatsec", __LINE__, __FILE__)
        call assertEqual( ref_cgtsec, cgtsec, tol, "cgtsec", __LINE__, __FILE__)
        call assertEqual( ref_qsec, qsec, tol, "qsec", __LINE__, __FILE__)
        call assertEqual( ref_consec, consec, tol, "consec", __LINE__, __FILE__)
        call assertEqual( ref_vd_eff_trj_zra, vd_eff_trj_zra, tol, "vd_eff_trj_zra", __LINE__, __FILE__)
   end subroutine test_ops_seccmp6

   subroutine test_ops_seccmp5()
   use m_ops_seccmp
   use no_pfunit_ops_lt
   use m_ops_varin, only: Tvarin_unc
   use m_ops_tdo_proc, only: tdo_proc
       real, parameter :: tol = 1e-5
        type(Tvarin_unc) :: varin_unc
        type(tdo_proc) :: do_proc
        real, parameter :: qbpri =   1.00000000E+00
        real, parameter :: ueff =   3.46691298E+00
        real, parameter :: rc_sec_trj =   1.57011548E+03
        real, parameter :: routsec =   1.40000000E+07
        real, parameter :: ccc =   0.00000000E+00
        real, parameter :: vv =   9.31268215E-01
        real, parameter :: amol1 =   4.60000000E+01
        real, parameter :: amol2 =   6.20000000E+01
        real, parameter :: xvg =   1.00000000E+00
        real, parameter :: sigz =   5.33004904E+00
        real, parameter :: grad =   7.80078471E-01
        real, parameter :: utr =   3.46691298E+00
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: xl =   8.00000000E+01
        real, parameter :: xloc =   3.79999962E+01
        real, parameter :: vw10 =   1.14800286E+00
        real, parameter :: pcoef =   4.79999989E-01
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: regenk =   3.55333388E-02
        real, parameter :: htot =   1.00000000E+02
        real, parameter :: onder =   0.00000000E+00
        real, parameter :: twt =   5.86001635E-01
        real, parameter :: ri =   1.25999999E+00
        real, parameter :: cgt =   0.00000000E+00
        real, parameter :: xvghbr =   7.88567185E-01
        real, parameter :: xvglbr =   1.09725904E+00
        real, parameter :: vnatpri =   2.16914892E+00
        real, parameter :: vchem =   2.03441787E+00
        real, parameter :: ra_rcp_4 =   1.94800186E+02
        real, parameter :: ra_rcp_zra =   7.42978516E+02
        real, parameter :: rb_rcp =   1.20375221E+02
        real, parameter :: rc_sec_rcp =   1.28872827E+03
        real, parameter :: ra_trj_zra =   9.12402771E+02
        real, parameter :: rb_trj =   1.20375221E+02
        real :: pr
        real, parameter :: ref_pr =   6.44039750E-01
        real :: vnatsec
        real, parameter :: ref_vnatsec =   4.45002794E+00
        real :: cgtsec
        real, parameter :: ref_cgtsec =   0.00000000E+00
        real :: qsec
        real, parameter :: ref_qsec =   4.31687422E-02
        real :: consec
        real, parameter :: ref_consec =   5.12495194E-29
        real, parameter :: in_vd_eff_trj_zra =   2.96227430E-04
        real :: vd_eff_trj_zra
        real, parameter :: ref_vd_eff_trj_zra =   0.00000000E+00
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        vd_eff_trj_zra = in_vd_eff_trj_zra
        call ops_seccmp(varin_unc, do_proc, qbpri, ueff, rc_sec_trj, routsec, ccc, vv, amol1, amol2, xvg, sigz, grad, utr, radius, disxx, xl, xloc, vw10,  &
                   &  pcoef, virty, regenk, htot, onder, twt, ri, cgt, xvghbr, xvglbr, vnatpri, vchem, ra_rcp_4,  &
                   &  ra_rcp_zra, rb_rcp, rc_sec_rcp, pr, vnatsec, cgtsec, qsec, consec, vd_eff_trj_zra, ra_trj_zra, rb_trj)
        call assertEqual( ref_pr, pr, tol, "pr", __LINE__, __FILE__)
        call assertEqual( ref_vnatsec, vnatsec, tol, "vnatsec", __LINE__, __FILE__)
        call assertEqual( ref_cgtsec, cgtsec, tol, "cgtsec", __LINE__, __FILE__)
        call assertEqual( ref_qsec, qsec, tol, "qsec", __LINE__, __FILE__)
        call assertEqual( ref_consec, consec, tol, "consec", __LINE__, __FILE__)
        call assertEqual( ref_vd_eff_trj_zra, vd_eff_trj_zra, tol, "vd_eff_trj_zra", __LINE__, __FILE__)
   end subroutine test_ops_seccmp5

   subroutine test_ops_seccmp4()
   use m_ops_seccmp
   use no_pfunit_ops_lt
   use m_ops_varin, only: Tvarin_unc
   use m_ops_tdo_proc, only: tdo_proc
       real, parameter :: tol = 1e-5
        type(Tvarin_unc) :: varin_unc
        type(tdo_proc) :: do_proc
        real, parameter :: qbpri =   1.00000000E+00
        real, parameter :: ueff =   3.46691298E+00
        real, parameter :: rc_sec_trj =   1.57011548E+03
        real, parameter :: routsec =   1.40000000E+07
        real, parameter :: ccc =   0.00000000E+00
        real, parameter :: vv =   9.31268215E-01
        real, parameter :: amol1 =   4.60000000E+01
        real, parameter :: amol2 =   6.20000000E+01
        real, parameter :: xvg =   1.00000000E+00
        real, parameter :: sigz =   5.33004904E+00
        real, parameter :: grad =   7.80078471E-01
        real, parameter :: utr =   3.46691298E+00
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: xl =   8.00000000E+01
        real, parameter :: xloc =   3.79999962E+01
        real, parameter :: vw10 =   1.14800286E+00
        real, parameter :: pcoef =   4.79999989E-01
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: regenk =   3.55333388E-02
        real, parameter :: htot =   1.00000000E+02
        real, parameter :: onder =   0.00000000E+00
        real, parameter :: twt =   5.86001635E-01
        real, parameter :: ri =   1.25999999E+00
        real, parameter :: cgt =   0.00000000E+00
        real, parameter :: xvghbr =   7.88567185E-01
        real, parameter :: xvglbr =   1.09725904E+00
        real, parameter :: vnatpri =   2.16914892E+00
        real, parameter :: vchem =   2.03441787E+00
        real, parameter :: ra_rcp_4 =   1.94800186E+02
        real, parameter :: ra_rcp_zra =   7.42978516E+02
        real, parameter :: rb_rcp =   1.20375221E+02
        real, parameter :: rc_sec_rcp =   1.28872827E+03
        real, parameter :: ra_trj_zra =   9.12402771E+02
        real, parameter :: rb_trj =   1.20375221E+02
        real :: pr
        real, parameter :: ref_pr =   6.44039750E-01
        real :: vnatsec
        real, parameter :: ref_vnatsec =   4.45002794E+00
        real :: cgtsec
        real, parameter :: ref_cgtsec =   0.00000000E+00
        real :: qsec
        real, parameter :: ref_qsec =   4.31687422E-02
        real :: consec
        real, parameter :: ref_consec =   5.12495194E-29
        real, parameter :: in_vd_eff_trj_zra =   2.96227430E-04
        real :: vd_eff_trj_zra
        real, parameter :: ref_vd_eff_trj_zra =   0.00000000E+00
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        vd_eff_trj_zra = in_vd_eff_trj_zra
        call ops_seccmp(varin_unc, do_proc, qbpri, ueff, rc_sec_trj, routsec, ccc, vv, amol1, amol2, xvg, sigz, grad, utr, radius, disxx, xl, xloc, vw10,  &
                   &  pcoef, virty, regenk, htot, onder, twt, ri, cgt, xvghbr, xvglbr, vnatpri, vchem, ra_rcp_4,  &
                   &  ra_rcp_zra, rb_rcp, rc_sec_rcp, pr, vnatsec, cgtsec, qsec, consec, vd_eff_trj_zra, ra_trj_zra, rb_trj)
        call assertEqual( ref_pr, pr, tol, "pr", __LINE__, __FILE__)
        call assertEqual( ref_vnatsec, vnatsec, tol, "vnatsec", __LINE__, __FILE__)
        call assertEqual( ref_cgtsec, cgtsec, tol, "cgtsec", __LINE__, __FILE__)
        call assertEqual( ref_qsec, qsec, tol, "qsec", __LINE__, __FILE__)
        call assertEqual( ref_consec, consec, tol, "consec", __LINE__, __FILE__)
        call assertEqual( ref_vd_eff_trj_zra, vd_eff_trj_zra, tol, "vd_eff_trj_zra", __LINE__, __FILE__)
   end subroutine test_ops_seccmp4

   subroutine test_ops_seccmp3()
   use m_ops_seccmp
   use no_pfunit_ops_lt
   use m_ops_varin, only: Tvarin_unc
   use m_ops_tdo_proc, only: tdo_proc
       real, parameter :: tol = 1e-5
        type(Tvarin_unc) :: varin_unc
        type(tdo_proc) :: do_proc
        real, parameter :: qbpri =   1.00000000E+00
        real, parameter :: ueff =   8.04345226E+00
        real, parameter :: rc_sec_trj =   1.68729218E+02
        real, parameter :: routsec =   1.40000000E+07
        real, parameter :: ccc =   6.04695715E-02
        real, parameter :: vv =   9.71474230E-01
        real, parameter :: amol1 =   4.60000000E+01
        real, parameter :: amol2 =   6.20000000E+01
        real, parameter :: xvg =   1.00000000E+00
        real, parameter :: sigz =   1.80795395E+02
        real, parameter :: grad =   9.86378908E-01
        real, parameter :: utr =   5.99296141E+00
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: disxx =   1.73223848E+04
        real, parameter :: xl =   4.81590851E+02
        real, parameter :: xloc =   4.65999969E+02
        real, parameter :: vw10 =   4.77636433E+00
        real, parameter :: pcoef =   2.89999992E-01
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: regenk =   0.00000000E+00
        real, parameter :: htot =   5.00000000E+00
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: twt =   2.83734411E-01
        real, parameter :: ri =   1.35000002E+00
        real, parameter :: cgt =   5.45915402E-03
        real, parameter :: xvghbr =   9.91338372E-01
        real, parameter :: xvglbr =   1.21307540E+00
        real, parameter :: vnatpri =   0.00000000E+00
        real, parameter :: vchem =   1.41260922E+00
        real, parameter :: ra_rcp_4 =   1.75061207E+01
        real, parameter :: ra_rcp_zra =   3.69904633E+01
        real, parameter :: rb_rcp =   1.64628677E+01
        real, parameter :: rc_sec_rcp =   1.93088470E+02
        real, parameter :: ra_trj_zra =   2.81038532E+01
        real, parameter :: rb_trj =   1.50981655E+01
        real :: pr
        real, parameter :: ref_pr =   7.59050995E-02
        real :: vnatsec
        real, parameter :: ref_vnatsec =   0.00000000E+00
        real :: cgtsec
        real, parameter :: ref_cgtsec =   3.16744111E-02
        real :: qsec
        real, parameter :: ref_qsec =   1.47188818E-02
        real :: consec
        real, parameter :: ref_consec =   8.90019990E-04
        real, parameter :: in_vd_eff_trj_zra =   8.54153535E-04
        real :: vd_eff_trj_zra
        real, parameter :: ref_vd_eff_trj_zra =   8.54153535E-04
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        vd_eff_trj_zra = in_vd_eff_trj_zra
        pr = ref_pr
        call ops_seccmp(varin_unc, do_proc, qbpri, ueff, rc_sec_trj, routsec, ccc, vv, amol1, amol2, xvg, sigz, grad, utr, radius, disxx, xl, xloc, vw10,  &
                   &  pcoef, virty, regenk, htot, onder, twt, ri, cgt, xvghbr, xvglbr, vnatpri, vchem, ra_rcp_4,  &
                   &  ra_rcp_zra, rb_rcp, rc_sec_rcp, pr, vnatsec, cgtsec, qsec, consec, vd_eff_trj_zra, ra_trj_zra, rb_trj)
        call assertEqual( ref_pr, pr, tol, "pr", __LINE__, __FILE__)
        call assertEqual( ref_vnatsec, vnatsec, tol, "vnatsec", __LINE__, __FILE__)
        call assertEqual( ref_cgtsec, cgtsec, tol, "cgtsec", __LINE__, __FILE__)
        call assertEqual( ref_qsec, qsec, tol, "qsec", __LINE__, __FILE__)
        call assertEqual( ref_consec, consec, tol, "consec", __LINE__, __FILE__)
        call assertEqual( ref_vd_eff_trj_zra, vd_eff_trj_zra, tol, "vd_eff_trj_zra", __LINE__, __FILE__)
   end subroutine test_ops_seccmp3
   subroutine test_ops_seccmp2()
   use no_pfunit_ops_lt
   use m_ops_seccmp
   use m_ops_varin, only: Tvarin_unc
   use m_ops_tdo_proc, only: tdo_proc
       real, parameter :: tol = 1e-5
        type(Tvarin_unc) :: varin_unc
        type(tdo_proc) :: do_proc
        real, parameter :: qbpri =   1.00000000E+00
        real, parameter :: ueff =   5.62392044E+00
        real, parameter :: rc_sec_trj =   2.20048492E+02
        real, parameter :: routsec =   1.40000000E+07
        real, parameter :: ccc =   9.30537879E-02
        real, parameter :: vv =   9.35861409E-01
        real, parameter :: amol1 =   4.60000000E+01
        real, parameter :: amol2 =   6.20000000E+01
        real, parameter :: xvg =   1.00000000E+00
        real, parameter :: sigz =   1.37633621E+02
        real, parameter :: grad =   9.68886554E-01
        real, parameter :: utr =   4.17158270E+00
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: xl =   2.85942810E+02
        real, parameter :: xloc =   2.81999969E+02
        real, parameter :: vw10 =   3.46342969E+00
        real, parameter :: pcoef =   3.54666650E-01
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: regenk =   5.78000024E-02
        real, parameter :: htot =   5.00000000E+00
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: twt =   3.24517757E-01
        real, parameter :: ri =   1.35000002E+00
        real, parameter :: cgt =   1.93412919E-02
        real, parameter :: xvghbr =   9.61942077E-01
        real, parameter :: xvglbr =   1.01691461E+00
        real, parameter :: vnatpri =   3.65734473E-02
        real, parameter :: vchem =   1.60529709E+00
        real, parameter :: ra_rcp_4 =   2.72117901E+01
        real, parameter :: ra_rcp_zra =   6.90764313E+01
        real, parameter :: rb_rcp =   2.44440670E+01
        real, parameter :: rc_sec_rcp =   1.92986176E+02
        real, parameter :: ra_trj_zra =   5.01215210E+01
        real, parameter :: rb_trj =   2.24919376E+01
        real :: pr
        real, parameter :: ref_pr =   1.15540028E-01
        real :: vnatsec
        real, parameter :: ref_vnatsec =   4.52611446E+00
        real :: cgtsec
        real, parameter :: ref_cgtsec =   9.08343419E-02
        real :: qsec
        real, parameter :: ref_qsec =   2.72823069E-02
        real :: consec
        real, parameter :: ref_consec =   2.53956369E-03
        real, parameter :: in_vd_eff_trj_zra =   7.67690362E-04
        real :: vd_eff_trj_zra
        real, parameter :: ref_vd_eff_trj_zra =   7.67690362E-04
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        vd_eff_trj_zra = in_vd_eff_trj_zra
        call ops_seccmp(varin_unc, do_proc, qbpri, ueff, rc_sec_trj, routsec, ccc, vv, amol1, amol2, xvg, sigz, grad, utr, radius, disxx, xl, xloc, vw10,  &
                   &  pcoef, virty, regenk, htot, onder, twt, ri, cgt, xvghbr, xvglbr, vnatpri, vchem, ra_rcp_4,  &
                   &  ra_rcp_zra, rb_rcp, rc_sec_rcp, pr, vnatsec, cgtsec, qsec, consec, vd_eff_trj_zra, ra_trj_zra, rb_trj)
        call assertEqual( ref_pr, pr, tol, "pr", __LINE__, __FILE__)
        call assertEqual( ref_vnatsec, vnatsec, tol, "vnatsec", __LINE__, __FILE__)
        call assertEqual( ref_cgtsec, cgtsec, tol, "cgtsec", __LINE__, __FILE__)
        call assertEqual( ref_qsec, qsec, tol, "qsec", __LINE__, __FILE__)
        call assertEqual( ref_consec, consec, tol, "consec", __LINE__, __FILE__)
        call assertEqual( ref_vd_eff_trj_zra, vd_eff_trj_zra, tol, "vd_eff_trj_zra", __LINE__, __FILE__)
   end subroutine test_ops_seccmp2

   subroutine test_ops_seccmp()
   use m_ops_seccmp
   use no_pfunit_ops_lt
   use m_ops_varin, only: Tvarin_unc
   use m_ops_tdo_proc, only: tdo_proc
       real, parameter :: tol = 1e-5
        type(Tvarin_unc) :: varin_unc
        type(tdo_proc) :: do_proc
        real, parameter :: qbpri =   1.00000000E+00
        real, parameter :: ueff =   4.44325399E+00
        real, parameter :: rc_sec_trj =   1.66130997E+02
        real, parameter :: routsec =   1.40000000E+07
        real, parameter :: ccc =   6.86089471E-02
        real, parameter :: vv =   9.20733929E-01
        real, parameter :: amol1 =   4.60000000E+01
        real, parameter :: amol2 =   6.20000000E+01
        real, parameter :: xvg =   1.00000000E+00
        real, parameter :: sigz =   7.55815918E+02
        real, parameter :: grad =   9.87857282E-01
        real, parameter :: utr =   4.13313913E+00
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: xl =   2.96321716E+02
        real, parameter :: xloc =   2.08999985E+02
        real, parameter :: vw10 =   2.01143265E+00
        real, parameter :: pcoef =   2.94000000E-01
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: regenk =   1.70000009E-02
        real, parameter :: htot =   5.00000000E+00
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: twt =   4.50051814E-01
        real, parameter :: ri =   1.77999997E+00
        real, parameter :: cgt =   1.11482814E-02
        real, parameter :: xvghbr =   1.02960062E+00
        real, parameter :: xvglbr =   1.07823014E+00
        real, parameter :: vnatpri =   1.08947068E-01
        real, parameter :: vchem =   3.08206844E+00
        real, parameter :: ra_rcp_4 =   2.43975487E+01
        real, parameter :: ra_rcp_zra =   3.31806717E+01
        real, parameter :: rb_rcp =   3.01502495E+01
        real, parameter :: rc_sec_rcp =   1.37478745E+02
        real, parameter :: ra_trj_zra =   3.47806282E+01
        real, parameter :: rb_trj =   2.85675850E+01
        real :: pr
        real, parameter :: ref_pr =   9.26025271E-01
        real :: vnatsec
        real, parameter :: ref_vnatsec =   3.56953907E+00
        real :: cgtsec
        real, parameter :: ref_cgtsec =   4.01564911E-02
        real :: qsec
        real, parameter :: ref_qsec =   5.27338274E-02
        real :: consec
        real, parameter :: ref_consec =   3.61801242E-03
        real, parameter :: in_vd_eff_trj_zra =   1.49062264E-03
        real :: vd_eff_trj_zra
        real, parameter :: ref_vd_eff_trj_zra =   1.49062264E-03
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        vd_eff_trj_zra = in_vd_eff_trj_zra
        call ops_seccmp(varin_unc, do_proc, qbpri, ueff, rc_sec_trj, routsec, ccc, vv, amol1, amol2, xvg, sigz, grad, utr, radius, disxx, xl, xloc, vw10,  &
                   &  pcoef, virty, regenk, htot, onder, twt, ri, cgt, xvghbr, xvglbr, vnatpri, vchem, ra_rcp_4,  &
                   &  ra_rcp_zra, rb_rcp, rc_sec_rcp, pr, vnatsec, cgtsec, qsec, consec, vd_eff_trj_zra, ra_trj_zra, rb_trj)
        call assertEqual( ref_pr, pr, tol, "pr", __LINE__, __FILE__)
        call assertEqual( ref_vnatsec, vnatsec, tol, "vnatsec", __LINE__, __FILE__)
        call assertEqual( ref_cgtsec, cgtsec, tol, "cgtsec", __LINE__, __FILE__)
        call assertEqual( ref_qsec, qsec, tol, "qsec", __LINE__, __FILE__)
        call assertEqual( ref_consec, consec, tol, "consec", __LINE__, __FILE__)
        call assertEqual( ref_vd_eff_trj_zra, vd_eff_trj_zra, tol, "vd_eff_trj_zra", __LINE__, __FILE__)
   end subroutine test_ops_seccmp

   subroutine test_seccd4()
   use m_ops_seccmp
   use no_pfunit_ops_lt
   use m_ops_tdo_proc, only: tdo_proc
       real, parameter :: tol = 1e-5
        type(tdo_proc) :: do_proc
        real, parameter :: qbpri =   1.00000000E+00
        real, parameter :: disxx =   9.82344116E+02
        real, parameter :: radius =   9.94035767E+02
        real, parameter :: vw =   2.22042251E+00
        real, parameter :: xl =   2.26793579E+02
        real, parameter :: vd_eff_trj_zra =   9.13905795E-04
        real, parameter :: vnatpri =   1.16256726E-09
        real, parameter :: vchem =   3.03287721E+00
        real, parameter :: vd_sec_eff_trj_zra =   4.42497432E-03
        real, parameter :: vnatsec =   1.75550699E+00
        real, parameter :: amol1 =   4.60000000E+01
        real, parameter :: amol2 =   6.20000000E+01
        real, parameter :: diameter =   1.77304956E+03
        real, parameter :: sigz =   2.23733616E+01
        real :: qpri
        real, parameter :: ref_qpri(2) =  (/ 0.995086253,   0.998403906822 /)
        real :: qsec
        real, parameter :: ref_qsec(2) =  (/ 4.45816526E-03, 0.0 /)
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call seccd( do_proc, qbpri, disxx, radius, vw, xl, vd_eff_trj_zra, vnatpri, vchem, vd_sec_eff_trj_zra, &
            vnatsec, amol1, amol2, diameter, sigz, qpri, qsec)
        call assertEqual( ref_qpri(1), qpri, tol, "qpri", __LINE__, __FILE__)
        call assertEqual( ref_qsec(1), qsec, tol, "qsec", __LINE__, __FILE__)
        do_proc%chem = .false.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call seccd( do_proc, qbpri, disxx, radius, vw, xl, vd_eff_trj_zra, vnatpri, vchem, vd_sec_eff_trj_zra, &
            vnatsec, amol1, amol2, diameter, sigz, qpri, qsec)
        call assertEqual( ref_qpri(2), qpri, tol, "qpri", __LINE__, __FILE__)
        call assertEqual( ref_qsec(2), qsec, tol, "qsec", __LINE__, __FILE__)
   end subroutine test_seccd4

   subroutine test_seccd3()
   use no_pfunit_ops_lt
   use m_ops_seccmp
   use m_ops_tdo_proc, only: tdo_proc
       real, parameter :: tol = 1e-5
        type(tdo_proc) :: do_proc
        real, parameter :: qbpri =   1.00000000E+00
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: radius =   9.94035767E+02
        real, parameter :: vw =   4.30189896E+00
        real, parameter :: xl =   2.86299438E+02
        real, parameter :: vd_eff_trj_zra =   7.98411085E-04
        real, parameter :: vnatpri =   3.64702828E-02
        real, parameter :: vchem =   1.57000470E+00
        real, parameter :: vd_sec_eff_trj_zra =   3.48602794E-03
        real, parameter :: vnatsec =   4.73849964E+00
        real, parameter :: amol1 =   4.60000000E+01
        real, parameter :: amol2 =   6.20000000E+01
        real, parameter :: diameter =   1.77304956E+03
        real, parameter :: sigz =   1.35575409E+02
        real :: qpri
        real, parameter :: ref_qpri =   9.64991868E-01
        real :: qsec
        real, parameter :: ref_qsec =   2.66742986E-02
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call seccd( do_proc, qbpri, disxx, radius, vw, xl, vd_eff_trj_zra, vnatpri, vchem, vd_sec_eff_trj_zra, &
            vnatsec, amol1, amol2, diameter, sigz, qpri, qsec)
        call assertEqual( ref_qpri, qpri, tol, "qpri", __LINE__, __FILE__)
        call assertEqual( ref_qsec, qsec, tol, "qsec", __LINE__, __FILE__)
   end subroutine test_seccd3

   subroutine test_seccd2()
   use no_pfunit_ops_lt
   use m_ops_seccmp
   use m_ops_tdo_proc, only: tdo_proc
       real, parameter :: tol = 1e-5
        type(tdo_proc) :: do_proc
        real, parameter :: qbpri =   1.00000000E+00
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: radius =   9.94035767E+02
        real, parameter :: vw =   4.07167959E+00
        real, parameter :: xl =   2.79640625E+02
        real, parameter :: vd_eff_trj_zra =   1.79345731E-03
        real, parameter :: vnatpri =   1.53959602E-01
        real, parameter :: vchem =   3.03763580E+00
        real, parameter :: vd_sec_eff_trj_zra =   4.63131675E-03
        real, parameter :: vnatsec =   4.79869652E+00
        real, parameter :: amol1 =   4.60000000E+01
        real, parameter :: amol2 =   6.20000000E+01
        real, parameter :: diameter =   1.77304956E+03
        real, parameter :: sigz =   6.57454468E+02
        real :: qpri
        real, parameter :: ref_qpri =   9.23735321E-01
        real :: qsec
        real, parameter :: ref_qsec =   5.25326207E-02
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call seccd( do_proc, qbpri, disxx, radius, vw, xl, vd_eff_trj_zra, vnatpri, vchem, vd_sec_eff_trj_zra, &
            vnatsec, amol1, amol2, diameter, sigz, qpri, qsec)
        call assertEqual( ref_qpri, qpri, tol, "qpri", __LINE__, __FILE__)
        call assertEqual( ref_qsec, qsec, tol, "qsec", __LINE__, __FILE__)
   end subroutine test_seccd2

   subroutine test_seccd()
   use m_ops_seccmp
   use no_pfunit_ops_lt
   use m_ops_tdo_proc, only: tdo_proc
       real, parameter :: tol = 1e-5
        type(tdo_proc) :: do_proc
        real, parameter :: qbpri =   1.00000000E+00
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: vw =   4.13313913E+00
        real, parameter :: xl =   2.96321716E+02
        real, parameter :: vd_eff_trj_zra =   1.49062264E-03
        real, parameter :: vnatpri =   1.08947068E-01
        real, parameter :: vchem =   3.08206844E+00
        real, parameter :: vd_sec_eff_trj_zra =   4.69859643E-03
        real, parameter :: vnatsec =   3.56953907E+00
        real, parameter :: amol1 =   4.60000000E+01
        real, parameter :: amol2 =   6.20000000E+01
        real, parameter :: diameter =   0.00000000E+00
        real, parameter :: sigz =   7.55815918E+02
        real :: qpri
        real, parameter :: ref_qpri =   9.31392670E-01
        real :: qsec
        real, parameter :: ref_qsec =   5.33442944E-02
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call seccd( do_proc, qbpri, disxx, radius, vw, xl, vd_eff_trj_zra, vnatpri, vchem, vd_sec_eff_trj_zra, &
            vnatsec, amol1, amol2, diameter, sigz, qpri, qsec)
        call assertEqual( ref_qpri, qpri, tol, "qpri", __LINE__, __FILE__)
        call assertEqual( ref_qsec, qsec, tol, "qsec", __LINE__, __FILE__)
   end subroutine test_seccd
end module m_test_m_ops_seccmp
 
program p_test_seccd
use m_test_m_ops_seccmp
use no_pfunit
implicit none
  !call test_seccd4()
  !call test_seccd3()
  !call test_seccd2()
  !call test_seccd()
   call test_ops_seccmp10()
  !call test_ops_seccmp9()
  !call test_ops_seccmp8()
  !call test_ops_seccmp7()
  !call test_ops_seccmp6()
  !call test_ops_seccmp5()
  !call test_ops_seccmp4()
  !call test_ops_seccmp3()
  !call test_ops_seccmp2()
  !call test_ops_seccmp()
   call conclusion()
end program p_test_seccd

