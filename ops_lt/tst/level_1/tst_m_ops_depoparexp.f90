module m_test_m_ops_depoparexp
use m_ops_depoparexp
implicit none
contains
   subroutine test_ops_depoparexp7()
   use m_ops_tdo_proc, only: tdo_proc
        type(tdo_proc) :: do_proc
        integer, parameter :: kdeel = 1
        real, parameter :: c =   6.48795366E+00
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: ra_rcp_4 =   2.44737968E+01
        real, parameter :: ra_rcp_zra =   3.47703209E+01
        real, parameter :: ra_rcp_zrcp =   2.44737968E+01
        real, parameter :: rb_rcp =   2.76735859E+01
        real, parameter :: sigz =   2.23733616E+01
        real, parameter :: ueff =   2.22042251E+00
        real, parameter :: virty =   3.81971851E+03
        logical, parameter :: gasv = .true.
        integer, parameter :: ircp = -999
        integer, parameter :: istab = 1, istab5=5
        real, parameter :: grof =   0.0, grof1=1.0 
        real, parameter :: xvghbr =   1.00078666E+00
        real, parameter :: xvglbr =   1.00363839E+00
        real, parameter :: regenk =   3.08684688E-02
        real, parameter :: rint =   1.43999994E+00
        real, parameter :: buil =   1.78999996E+00
        real, parameter :: zf =   5.00000000E-01
        integer, parameter :: isec1 = 6
        integer, parameter :: iseiz = 1
        integer, parameter :: mb = 1
        real, parameter :: disx = -999.0
        real, parameter :: disxx =   9.82344116E+02
        real, parameter :: radius =   9.94035767E+02
        real, parameter :: xl =   2.26793579E+02
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: dg =   0.00000000E+00
        integer, parameter :: knatdeppar = 3
        real, parameter :: scavcoef =   0.00000000E+00
        logical, parameter :: irev = .false.
        real, parameter :: htt =   5.00000000E+00
        real, parameter :: xloc =   2.43999985E+02
        real, parameter :: xl100 =   5.91999939E+02
        real, parameter :: vw10 =   2.10983324E+00
        real, parameter :: pcoef =   3.05333316E-01
        real, parameter :: vchem =   3.03287721E+00
        real, parameter :: dispg =   3.54144454E-01
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: ol_src =  -3.79837418E+01
        real, parameter :: uster_src =   2.54701376E-01
        real, parameter :: ra_src_4 =   2.43657837E+01
        real, parameter :: rb_src =   2.83567886E+01
        real, parameter :: rb_trj =   2.83567867E+01
        real, parameter :: ra_trj_zra =   3.20103760E+01
        real, parameter :: xm =   1.54750000E+05
        real, parameter :: ym =   3.85950000E+05
        real, parameter :: zm =   4.00000000E+00
        integer, parameter :: bx = 155000
        integer, parameter :: by = 385000
        real, parameter :: rc_eff_rcp_4_pos =   1.03781860E+03
        real, parameter :: rc_eff_src_4_pos =   1.03781860E+03
        real, parameter :: rc_eff_trj_4_pos =   1.03781860E+03
        real, parameter :: routpri =   1.37694189E+04
        real, parameter :: in_htot(5) =  (/ 5.0, 400.0, 0.1, 1000.0, -100.0 /)
        real, parameter :: ref_htot(8) =    (/ 5.0, 395.0, 400.0, 52.50, 5.50, 0.25, 1000.0, -100.0  /)
        real, parameter :: ref_pr(3) =  (/ 4.31189529E-09, 0.121873848140, 1.398308459107E-17  /)
        real, parameter :: ref_twt =   2.19858810E-02
        real, parameter :: ref_cratio =   1.00000000E+00
        real, parameter :: ref_grad(2) =  (/ 0.990641773, 1.0 /)
        real, parameter :: ref_utr =   2.22042251E+00
        real, parameter :: ref_vd_coarse_part(2) =  (/ 4.59149455E-41, 3.000000142492E-04 /) 
        real, parameter :: ref_vd_eff_trj_zra(4) =  (/ 9.13905795E-04, 3.010915243067E-04, 2.101652062265E-04, 6.379163241945E-04  /)
        real, parameter :: ref_rkc =   1.00000000E+00
        real, parameter :: ref_ri =   1.43999994E+00
        real, parameter :: ref_vnatpri(3) = (/  1.16256726E-09, 3.285945951939E-02, 3.770100221247E-18  /)
        real, parameter :: ref_cgt(2) =  (/ 2.59155466E-04, 0.0 /)
        real, parameter :: ref_cgt_z(2) =  (/ 2.59155466E-04, 0.0 /)
        real, parameter :: ref_cq2(4) =  (/ 0.991869271, 1.0, 0.749332368374, 7.889037078712E-06  /)
        real, parameter :: ref_cdn =   1.00000000E+00
        real, parameter :: ref_cch(5) =  (/ 0.998323262, 0.998301446438, 0.998414695263, 0.998305141926, &
                                            0.998319625854 /) 
        
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call test_ops_depoparexp( do_proc, kdeel, c, qbstf, ra_rcp_4, ra_rcp_zra, &
             ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, ircp, istab, grof, xvghbr, xvglbr, regenk, &
             rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, xl, onder, dg, knatdeppar, &
             scavcoef, irev, htt, xloc, xl100, vw10, pcoef, vchem, dispg, z0_src, ol_src, &
             uster_src, ra_src_4, rb_src, rb_trj, ra_trj_zra, xm, ym, zm, bx, by, &
             rc_eff_rcp_4_pos, rc_eff_src_4_pos, rc_eff_trj_4_pos, routpri, in_htot(1), ref_htot(1), &
             ref_pr(1), ref_twt, ref_cratio, ref_grad(1), ref_utr, ref_vd_coarse_part(1), &
             ref_vd_eff_trj_zra(1), ref_rkc, ref_ri, ref_vnatpri(1), ref_cgt(1), ref_cgt_z(1), ref_cq2(1), &
             ref_cdn, ref_cch(1), __LINE__, __FILE__)
        call test_ops_depoparexp(do_proc, kdeel, c, qbstf, ra_rcp_4, ra_rcp_zra, &
             ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, ircp, istab, grof1, xvghbr, xvglbr, regenk, &
             rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, xl, onder, dg, knatdeppar, &
             scavcoef, irev, htt, xloc, xl100, vw10, pcoef, vchem, dispg, z0_src, ol_src, &
             uster_src, ra_src_4, rb_src, rb_trj, ra_trj_zra, xm, ym, zm, bx, by, &
             rc_eff_rcp_4_pos, rc_eff_src_4_pos, rc_eff_trj_4_pos, routpri, in_htot(1), ref_htot(1), &
             ref_pr(1), ref_twt, ref_cratio, ref_grad(2), ref_utr, ref_vd_coarse_part(2), &
             ref_vd_eff_trj_zra(2), ref_rkc, ref_ri, ref_vnatpri(1), ref_cgt(2), ref_cgt_z(2), ref_cq2(2), &
             ref_cdn, ref_cch(1), __LINE__, __FILE__)
        call test_ops_depoparexp(do_proc, kdeel, c, qbstf, ra_rcp_4, ra_rcp_zra, &
             ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, ircp, istab, grof1, xvghbr, xvglbr, regenk, &
             rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, xl, onder, dg, knatdeppar, &
             scavcoef, irev, htt, xloc, xl100, vw10, pcoef, vchem, dispg, z0_src, ol_src, &
             uster_src, ra_src_4, rb_src, rb_trj, ra_trj_zra, xm, ym, zm, bx, by, &
             rc_eff_rcp_4_pos, rc_eff_src_4_pos, rc_eff_trj_4_pos, routpri, in_htot(2), ref_htot(3), &
             ref_pr(2), ref_twt, ref_cratio, ref_grad(2), ref_utr, ref_vd_coarse_part(2), &
             ref_vd_eff_trj_zra(3), ref_rkc, ref_ri, ref_vnatpri(2), ref_cgt(2), ref_cgt_z(2), ref_cq2(2), &
             ref_cdn, ref_cch(2), __LINE__, __FILE__)
        do_proc%grad_drydep = .false.
        call test_ops_depoparexp(do_proc, kdeel, c, qbstf, ra_rcp_4, ra_rcp_zra, &
             ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, ircp, istab, grof, xvghbr, xvglbr, regenk, &
             rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, xl, onder, dg, knatdeppar, &
             scavcoef, irev, htt, xloc, xl100, vw10, pcoef, vchem, dispg, z0_src, ol_src, &
             uster_src, ra_src_4, rb_src, rb_trj, ra_trj_zra, xm, ym, zm, bx, by, &
             rc_eff_rcp_4_pos, rc_eff_src_4_pos, rc_eff_trj_4_pos, routpri, in_htot(1), ref_htot(1), &
             ref_pr(1), ref_twt, ref_cratio, ref_grad(1), ref_utr, ref_vd_coarse_part(1), &
             ref_vd_eff_trj_zra(1), ref_rkc, ref_ri, ref_vnatpri(1), ref_cgt(2), ref_cgt_z(2), ref_cq2(1), &
             ref_cdn, ref_cch(1), __LINE__, __FILE__)
        call test_ops_depoparexp(do_proc, kdeel, c, qbstf, ra_rcp_4, ra_rcp_zra, &
             ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, ircp, istab, grof, xvghbr, xvglbr, regenk, &
             rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, xl, onder, dg, knatdeppar, &
             scavcoef, irev, htt+100, xloc, xl100, vw10, pcoef, vchem, dispg, z0_src, ol_src, &
             uster_src, ra_src_4, rb_src, rb_trj, ra_trj_zra, xm, ym, zm, bx, by, &
             rc_eff_rcp_4_pos, rc_eff_src_4_pos, rc_eff_trj_4_pos, routpri, in_htot(1), ref_htot(4), &
             ref_pr(1), ref_twt, ref_cratio, ref_grad(1), ref_utr, ref_vd_coarse_part(1), &
             ref_vd_eff_trj_zra(1), ref_rkc, ref_ri, ref_vnatpri(1), ref_cgt(2), ref_cgt_z(2), ref_cq2(1), &
             ref_cdn, ref_cch(1), __LINE__, __FILE__)
        call test_ops_depoparexp(do_proc, kdeel, c, qbstf, ra_rcp_4, ra_rcp_zra, &
             ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, ircp, istab, grof, xvghbr, xvglbr, regenk, &
             rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, xl, onder, dg, knatdeppar, &
             scavcoef, irev, htt+1, xloc, xl100, vw10, pcoef, vchem, dispg, z0_src, ol_src, &
             uster_src, ra_src_4, rb_src, rb_trj, ra_trj_zra, xm, ym, zm, bx, by, &
             rc_eff_rcp_4_pos, rc_eff_src_4_pos, rc_eff_trj_4_pos, routpri, in_htot(1), ref_htot(5), &
             ref_pr(1), ref_twt, ref_cratio, ref_grad(1), ref_utr, ref_vd_coarse_part(1), &
             ref_vd_eff_trj_zra(1), ref_rkc, ref_ri, ref_vnatpri(1), ref_cgt(2), ref_cgt_z(2), ref_cq2(1), &
             ref_cdn, ref_cch(1), __LINE__, __FILE__)
        call test_ops_depoparexp(do_proc, kdeel, c, qbstf, ra_rcp_4, ra_rcp_zra, &
             ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, ircp, istab5, grof, xvghbr, xvglbr, regenk, &
             rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, xl, onder, dg, knatdeppar, &
             scavcoef, irev, htt+1, xloc, xl100/1000, vw10, pcoef, vchem, dispg, z0_src, ol_src, &
             uster_src, ra_src_4, rb_src, rb_trj, ra_trj_zra, xm, ym, zm, bx, by, &
             rc_eff_rcp_4_pos, rc_eff_src_4_pos, rc_eff_trj_4_pos, routpri, in_htot(2), ref_htot(3), &
             ref_pr(2), ref_twt, ref_cratio, ref_grad(1), ref_utr, ref_vd_coarse_part(1), &
             ref_vd_eff_trj_zra(4), ref_rkc, ref_ri, ref_vnatpri(2), ref_cgt(2), ref_cgt_z(2), ref_cq2(3), &
             ref_cdn, ref_cch(3), __LINE__, __FILE__)
        call test_ops_depoparexp(do_proc, kdeel, c, qbstf, ra_rcp_4, ra_rcp_zra, &
             ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, ircp, istab5, grof, xvghbr, xvglbr, regenk, &
             rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius*1000, xl, onder, dg, knatdeppar, &
             scavcoef, irev, htt+1, xloc, xl100/1000, vw10, pcoef, vchem, dispg, z0_src, ol_src, &
             uster_src, ra_src_4, rb_src, rb_trj, ra_trj_zra, xm, ym, zm, bx, by, &
             rc_eff_rcp_4_pos, rc_eff_src_4_pos, rc_eff_trj_4_pos, routpri, in_htot(2), ref_htot(3), &
             ref_pr(2), ref_twt, ref_cratio, ref_grad(1), ref_utr, ref_vd_coarse_part(1), &
             ref_vd_eff_trj_zra(4), ref_rkc, ref_ri, ref_vnatpri(2), ref_cgt(2), ref_cgt_z(2), ref_cq2(3), &
             ref_cdn, ref_cch(3), __LINE__, __FILE__)
        call test_ops_depoparexp( do_proc, kdeel, c, qbstf, ra_rcp_4, ra_rcp_zra, &
             ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, ircp, istab, grof, xvghbr, xvglbr, regenk, &
             rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, xl, onder, dg, knatdeppar, &
             scavcoef, irev, htt/10, xloc, xl100, vw10, pcoef, vchem, dispg, z0_src, ol_src, &
             uster_src, ra_src_4, rb_src, rb_trj, ra_trj_zra, xm, ym, zm, bx, by, &
             rc_eff_rcp_4_pos, rc_eff_src_4_pos, rc_eff_trj_4_pos, routpri, in_htot(3), ref_htot(6), &
             ref_pr(1), ref_twt, ref_cratio, ref_grad(1), ref_utr, ref_vd_coarse_part(1), &
             ref_vd_eff_trj_zra(1), ref_rkc, ref_ri, ref_vnatpri(1), ref_cgt(2), ref_cgt_z(2), ref_cq2(1), &
             ref_cdn, ref_cch(1), __LINE__, __FILE__)
        call test_ops_depoparexp( do_proc, kdeel, c, qbstf, ra_rcp_4, ra_rcp_zra, &
             ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, ircp, istab, grof, xvghbr, xvglbr, regenk, &
             rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, xl, onder, dg, knatdeppar, &
             scavcoef, irev, htt/10, xloc, xl100, vw10, pcoef, vchem, dispg, z0_src, ol_src, &
             uster_src, ra_src_4, rb_src, rb_trj, ra_trj_zra, xm, ym, zm, bx, by, &
             rc_eff_rcp_4_pos, rc_eff_src_4_pos, rc_eff_trj_4_pos, routpri, in_htot(4), ref_htot(7), &
             ref_pr(2), ref_twt, ref_cratio, ref_grad(1), ref_utr, ref_vd_coarse_part(1), &
             ref_vd_eff_trj_zra(4), ref_rkc, ref_ri, ref_vnatpri(2), ref_cgt(2), ref_cgt_z(2), ref_cq2(1), &
             ref_cdn, ref_cch(4), __LINE__, __FILE__)
        call test_ops_depoparexp( do_proc, kdeel, c, qbstf, ra_rcp_4, ra_rcp_zra, &
             ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, ircp, istab, grof1, xvghbr, xvglbr, regenk, &
             rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, xl, onder, dg, knatdeppar, &
             scavcoef, irev, htt/10, xloc, xl100, vw10, pcoef, vchem, dispg, z0_src, ol_src, &
             uster_src, ra_src_4, rb_src, rb_trj, ra_trj_zra, xm, ym, zm, bx, by, &
             rc_eff_rcp_4_pos, rc_eff_src_4_pos, rc_eff_trj_4_pos, routpri, in_htot(5), ref_htot(8), &
             ref_pr(3), ref_twt, ref_cratio, ref_grad(2), ref_utr, ref_vd_coarse_part(2), &
             ref_vd_eff_trj_zra(2), ref_rkc, ref_ri, ref_vnatpri(3), ref_cgt(2), ref_cgt_z(2), ref_cq2(4), &
             ref_cdn, ref_cch(5), __LINE__, __FILE__)
   end subroutine test_ops_depoparexp7
   
   subroutine test_ops_depoparexp6()
      use m_ops_tdo_proc, only: tdo_proc
      
        type(tdo_proc) :: do_proc
        integer, parameter :: kdeel = 1
        real, parameter :: c =   1.55599743E-19
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: ra_rcp_4 =   1.94696915E+02
        real, parameter :: ra_rcp_zra =   7.42573608E+02
        real, parameter :: ra_rcp_zrcp =   1.94696915E+02
        real, parameter :: rb_rcp =   1.20375221E+02
        real, parameter :: sigz =   5.27165604E+00
        real, parameter :: ueff =   2.72442698E+00
        real, parameter :: virty =   0.00000000E+00
        logical, parameter :: gasv = .true.
        integer, parameter :: ircp = -999
        integer, parameter :: istab = 5
        real, parameter :: grof =   0.00000000E+00
        real, parameter :: xvghbr =   7.93457508E-01
        real, parameter :: xvglbr =   1.09500957E+00
        real, parameter :: regenk =   3.22666653E-02
        real, parameter :: rint =   1.25999999E+00
        real, parameter :: buil =   3.25000000E+00
        real, parameter :: zf =   5.00000000E-01
        integer, parameter :: isec1 = 5
        integer, parameter :: iseiz = 1
        integer, parameter :: mb = 1
        real, parameter :: disx = -999.0
        real, parameter :: disxx =   2.06534492E+04
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: xl =   4.99794617E+01
        real, parameter :: onder =   4.99589056E-01
        real, parameter :: dg =   0.00000000E+00
        integer, parameter :: knatdeppar = 3
        real, parameter :: scavcoef =   0.00000000E+00
        logical, parameter :: irev = .false.
        real, parameter :: htt =   4.99794617E+01
        real, parameter :: xloc =   3.79999962E+01
        real, parameter :: xl100 =   9.59999924E+01
        real, parameter :: vw10 =   1.14457965E+00
        real, parameter :: pcoef =   4.79999989E-01
        real, parameter :: vchem =   2.04251504E+00
        real, parameter :: dispg =   6.77488558E-03
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: ol_src =   1.00713120E+01
        real, parameter :: uster_src =   5.99999987E-02
        real, parameter :: ra_src_4 =   2.00885895E+02
        real, parameter :: rb_src =   1.20375221E+02
        real, parameter :: rb_trj =   1.20375221E+02
        real, parameter :: ra_trj_zra =   9.15428101E+02
        real, parameter :: xm =   1.40750000E+05
        real, parameter :: ym =   3.99950000E+05
        real, parameter :: zm =   4.00000000E+00
        integer, parameter :: bx = 155000
        integer, parameter :: by = 385000
        real, parameter :: rc_eff_rcp_4_pos =   1.62676794E+03
        real, parameter :: rc_eff_src_4_pos =   1.62676794E+03
        real, parameter :: rc_eff_trj_4_pos =   1.62676794E+03
        real, parameter :: routpri =   1.17943867E+04
        real, parameter :: in_htot =   4.99794617E+01
        real, parameter :: ref_htot =   4.99794617E+01
        real, parameter :: ref_pr =   6.37758076E-01
        real, parameter :: ref_twt =   7.03970015E-01
        real, parameter :: ref_cratio =   1.00000000E+00
        real, parameter :: ref_grad =   7.79944181E-01
        real, parameter :: ref_utr =   2.74162817E+00
        real, parameter :: ref_vd_coarse_part =   1.26118263E-40
        real, parameter :: ref_vd_eff_trj_zra =   2.98004248E-04
        real, parameter :: ref_rkc =   1.00000000E+00
        real, parameter :: ref_ri =   1.25999999E+00
        real, parameter :: ref_vnatpri =   5.52082419E-01
        real, parameter :: ref_cgt =   1.36625007E-01
        real, parameter :: ref_cgt_z =   1.36625007E-01
        real, parameter :: ref_cq2 =   1.00000000E+00
        real, parameter :: ref_cdn =   1.00000000E+00
        real, parameter :: ref_cch =   9.47153628E-01
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call test_ops_depoparexp( do_proc, kdeel, c, qbstf, ra_rcp_4, ra_rcp_zra, &
             ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, ircp, istab, grof, xvghbr, xvglbr, regenk, &
             rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, xl, onder, dg, knatdeppar, &
             scavcoef, irev, htt, xloc, xl100, vw10, pcoef, vchem, dispg, z0_src, ol_src, &
             uster_src, ra_src_4, rb_src, rb_trj, ra_trj_zra, xm, ym, zm, bx, by, &
             rc_eff_rcp_4_pos, rc_eff_src_4_pos, rc_eff_trj_4_pos, routpri, in_htot, ref_htot, &
             ref_pr, ref_twt, ref_cratio, ref_grad, ref_utr, ref_vd_coarse_part, &
             ref_vd_eff_trj_zra, ref_rkc, ref_ri, ref_vnatpri, ref_cgt, ref_cgt_z, ref_cq2, &
             ref_cdn, ref_cch, __LINE__, __FILE__)
   end subroutine test_ops_depoparexp6

   subroutine test_ops_depoparexp5()
      use m_ops_tdo_proc, only: tdo_proc
         
        type(tdo_proc) :: do_proc
        integer, parameter :: kdeel = 1
        real, parameter :: c =   2.72247706E-19
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: ra_rcp_4 =   1.94800186E+02
        real, parameter :: ra_rcp_zra =   7.42978516E+02
        real, parameter :: ra_rcp_zrcp =   1.94800186E+02
        real, parameter :: rb_rcp =   1.20375221E+02
        real, parameter :: sigz =   5.33004904E+00
        real, parameter :: ueff =   2.72372031E+00
        real, parameter :: virty =   0.00000000E+00
        logical, parameter :: gasv = .true.
        integer, parameter :: ircp = -999
        integer, parameter :: istab = 5
        real, parameter :: grof =   0.00000000E+00
        real, parameter :: xvghbr =   7.88567185E-01
        real, parameter :: xvglbr =   1.09725904E+00
        real, parameter :: regenk =   3.27333361E-02
        real, parameter :: rint =   1.25999999E+00
        real, parameter :: buil =   3.25000000E+00
        real, parameter :: zf =   5.00000000E-01
        integer, parameter :: isec1 = 5
        integer, parameter :: iseiz = 1
        integer, parameter :: mb = 1
        real, parameter :: disx = -999.0
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: xl =   5.02630997E+01
        real, parameter :: onder =   5.05234420E-01
        real, parameter :: dg =   0.00000000E+00
        integer, parameter :: knatdeppar = 3
        real, parameter :: scavcoef =   0.00000000E+00
        logical, parameter :: irev = .false.
        real, parameter :: htt =   5.00000000E+01
        real, parameter :: xloc =   3.79999962E+01
        real, parameter :: xl100 =   9.59999924E+01
        real, parameter :: vw10 =   1.14800286E+00
        real, parameter :: pcoef =   4.79999989E-01
        real, parameter :: vchem =   2.04107952E+00
        real, parameter :: dispg =   6.74336217E-03
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: ol_src =   1.00844126E+01
        real, parameter :: uster_src =   5.99999987E-02
        real, parameter :: ra_src_4 =   2.00794220E+02
        real, parameter :: rb_src =   1.20375221E+02
        real, parameter :: rb_trj =   1.20375221E+02
        real, parameter :: ra_trj_zra =   9.12402771E+02
        real, parameter :: xm =   1.40050000E+05
        real, parameter :: ym =   3.99950000E+05
        real, parameter :: zm =   4.00000000E+00
        integer, parameter :: bx = 155000
        integer, parameter :: by = 385000
        real, parameter :: rc_eff_rcp_4_pos =   1.62684412E+03
        real, parameter :: rc_eff_src_4_pos =   1.62684436E+03
        real, parameter :: rc_eff_trj_4_pos =   1.62684436E+03
        real, parameter :: routpri =   1.17041465E+04
        real, parameter :: in_htot =   5.00000000E+01
        real, parameter :: ref_htot =   5.00000000E+01
        real, parameter :: ref_pr =   6.14148736E-01
        real, parameter :: ref_twt =   7.17836142E-01
        real, parameter :: ref_cratio =   1.00000000E+00
        real, parameter :: ref_grad =   7.79865563E-01
        real, parameter :: ref_utr =   2.27987814E+00
        real, parameter :: ref_vd_coarse_part =   1.26118263E-40
        real, parameter :: ref_vd_eff_trj_zra =   2.96495913E-04
        real, parameter :: ref_rkc =   1.00000000E+00
        real, parameter :: ref_ri =   1.25999999E+00
        real, parameter :: ref_vnatpri =   5.31854868E-01
        real, parameter :: ref_cgt =   1.53758690E-01
        real, parameter :: ref_cgt_z =   1.53758690E-01
        real, parameter :: ref_cq2 =   1.00000000E+00
        real, parameter :: ref_cdn =   9.99019742E-01
        real, parameter :: ref_cch =   9.35870469E-01
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call test_ops_depoparexp( do_proc, kdeel, c, qbstf, ra_rcp_4, ra_rcp_zra, &
             ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, ircp, istab, grof, xvghbr, xvglbr, regenk, &
             rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, xl, onder, dg, knatdeppar, &
             scavcoef, irev, htt, xloc, xl100, vw10, pcoef, vchem, dispg, z0_src, ol_src, &
             uster_src, ra_src_4, rb_src, rb_trj, ra_trj_zra, xm, ym, zm, bx, by, &
             rc_eff_rcp_4_pos, rc_eff_src_4_pos, rc_eff_trj_4_pos, routpri, in_htot, ref_htot, &
             ref_pr, ref_twt, ref_cratio, ref_grad, ref_utr, ref_vd_coarse_part, &
             ref_vd_eff_trj_zra, ref_rkc, ref_ri, ref_vnatpri, ref_cgt, ref_cgt_z, ref_cq2, &
             ref_cdn, ref_cch, __LINE__, __FILE__)
   end subroutine test_ops_depoparexp5

   subroutine test_ops_depoparexp4()
      use m_ops_tdo_proc, only: tdo_proc
      
        type(tdo_proc) :: do_proc
        integer, parameter :: kdeel = 1
        real, parameter :: c =   0.00000000E+00
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: ra_rcp_4 =   1.94800186E+02
        real, parameter :: ra_rcp_zra =   7.42978516E+02
        real, parameter :: ra_rcp_zrcp =   1.94800186E+02
        real, parameter :: rb_rcp =   1.20375221E+02
        real, parameter :: sigz =   5.33004904E+00
        real, parameter :: ueff =   3.46691298E+00
        real, parameter :: virty =   0.00000000E+00
        logical, parameter :: gasv = .true.
        integer, parameter :: ircp = -999
        integer, parameter :: istab = 5
        real, parameter :: grof =   0.00000000E+00
        real, parameter :: xvghbr =   7.88567185E-01
        real, parameter :: xvglbr =   1.09725904E+00
        real, parameter :: regenk =   3.55333388E-02
        real, parameter :: rint =   1.25999999E+00
        real, parameter :: buil =   3.25000000E+00
        real, parameter :: zf =   5.00000000E-01
        integer, parameter :: isec1 = 5
        integer, parameter :: iseiz = 1
        integer, parameter :: mb = 1
        real, parameter :: disx = -999.0
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: xl =   8.00000000E+01
        real, parameter :: onder =   0.00000000E+00
        real, parameter :: dg =   0.00000000E+00
        integer, parameter :: knatdeppar = 3
        real, parameter :: scavcoef =   0.00000000E+00
        logical, parameter :: irev = .false.
        real, parameter :: htt =   1.00000000E+02
        real, parameter :: xloc =   3.79999962E+01
        real, parameter :: xl100 =   9.59999924E+01
        real, parameter :: vw10 =   1.14800286E+00
        real, parameter :: pcoef =   4.79999989E-01
        real, parameter :: vchem =   2.03441787E+00
        real, parameter :: dispg =   6.74336217E-03
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: ol_src =   1.00844126E+01
        real, parameter :: uster_src =   5.99999987E-02
        real, parameter :: ra_src_4 =   2.00794220E+02
        real, parameter :: rb_src =   1.20375221E+02
        real, parameter :: rb_trj =   1.20375221E+02
        real, parameter :: ra_trj_zra =   9.12402771E+02
        real, parameter :: xm =   1.40050000E+05
        real, parameter :: ym =   3.99950000E+05
        real, parameter :: zm =   4.00000000E+00
        integer, parameter :: bx = 155000
        integer, parameter :: by = 385000
        real, parameter :: rc_eff_rcp_4_pos =   1.62925476E+03
        real, parameter :: rc_eff_src_4_pos =   1.62925488E+03
        real, parameter :: rc_eff_trj_4_pos =   1.62925488E+03
        real, parameter :: routpri =   1.17041465E+04
        real, parameter :: in_htot =   1.00000000E+02
        real, parameter :: ref_htot =   1.00000000E+02
        real, parameter :: ref_pr =   6.44039750E-01
        real, parameter :: ref_twt =   5.86001635E-01
        real, parameter :: ref_cratio =   1.00000000E+00
        real, parameter :: ref_grad =   7.80078471E-01
        real, parameter :: ref_utr =   3.46691298E+00
        real, parameter :: ref_vd_coarse_part =   1.26118263E-40
        real, parameter :: ref_vd_eff_trj_zra =   2.96227430E-04
        real, parameter :: ref_rkc =   1.00000000E+00
        real, parameter :: ref_ri =   1.25999999E+00
        real, parameter :: ref_vnatpri =   2.16914892E+00
        real, parameter :: ref_cgt =   0.00000000E+00
        real, parameter :: ref_cgt_z =   0.00000000E+00
        real, parameter :: ref_cq2 =   1.00000000E+00
        real, parameter :: ref_cdn =   1.00000000E+00
        real, parameter :: ref_cch =   9.31268215E-01
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call test_ops_depoparexp( do_proc, kdeel, c, qbstf, ra_rcp_4, ra_rcp_zra, &
             ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, ircp, istab, grof, xvghbr, xvglbr, regenk, &
             rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, xl, onder, dg, knatdeppar, &
             scavcoef, irev, htt, xloc, xl100, vw10, pcoef, vchem, dispg, z0_src, ol_src, &
             uster_src, ra_src_4, rb_src, rb_trj, ra_trj_zra, xm, ym, zm, bx, by, &
             rc_eff_rcp_4_pos, rc_eff_src_4_pos, rc_eff_trj_4_pos, routpri, in_htot, ref_htot, &
             ref_pr, ref_twt, ref_cratio, ref_grad, ref_utr, ref_vd_coarse_part, &
             ref_vd_eff_trj_zra, ref_rkc, ref_ri, ref_vnatpri, ref_cgt, ref_cgt_z, ref_cq2, &
             ref_cdn, ref_cch, __LINE__, __FILE__)
   end subroutine test_ops_depoparexp4

   subroutine test_ops_depoparexp3()
      use m_ops_tdo_proc, only: tdo_proc
      
        type(tdo_proc) :: do_proc
        integer, parameter :: kdeel = 1
        real, parameter :: c =   0.00000000E+00
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: ra_rcp_4 =   1.94800186E+02
        real, parameter :: ra_rcp_zra =   7.42978516E+02
        real, parameter :: ra_rcp_zrcp =   1.94800186E+02
        real, parameter :: rb_rcp =   1.20375221E+02
        real, parameter :: sigz =   5.33004904E+00
        real, parameter :: ueff =   3.46691298E+00
        real, parameter :: virty =   0.00000000E+00
        logical, parameter :: gasv = .true.
        integer, parameter :: ircp = -999
        integer, parameter :: istab = 5
        real, parameter :: grof =   0.00000000E+00
        real, parameter :: xvghbr =   7.88567185E-01
        real, parameter :: xvglbr =   1.09725904E+00
        real, parameter :: regenk =   3.55333388E-02
        real, parameter :: rint =   1.25999999E+00
        real, parameter :: buil =   3.25000000E+00
        real, parameter :: zf =   5.00000000E-01
        integer, parameter :: isec1 = 5
        integer, parameter :: iseiz = 1
        integer, parameter :: mb = 1
        real, parameter :: disx = -999.0
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: xl =   8.00000000E+01
        real, parameter :: onder =   0.00000000E+00
        real, parameter :: dg =   0.00000000E+00
        integer, parameter :: knatdeppar = 3
        real, parameter :: scavcoef =   0.00000000E+00
        logical, parameter :: irev = .false.
        real, parameter :: htt =   1.00000000E+02
        real, parameter :: xloc =   3.79999962E+01
        real, parameter :: xl100 =   9.59999924E+01
        real, parameter :: vw10 =   1.14800286E+00
        real, parameter :: pcoef =   4.79999989E-01
        real, parameter :: vchem =   2.03441787E+00
        real, parameter :: dispg =   6.74336217E-03
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: ol_src =   1.00844126E+01
        real, parameter :: uster_src =   5.99999987E-02
        real, parameter :: ra_src_4 =   2.00794220E+02
        real, parameter :: rb_src =   1.20375221E+02
        real, parameter :: rb_trj =   1.20375221E+02
        real, parameter :: ra_trj_zra =   9.12402771E+02
        real, parameter :: xm =   1.40050000E+05
        real, parameter :: ym =   3.99950000E+05
        real, parameter :: zm =   4.00000000E+00
        integer, parameter :: bx = 155000
        integer, parameter :: by = 385000
        real, parameter :: rc_eff_rcp_4_pos =   1.62925476E+03
        real, parameter :: rc_eff_src_4_pos =   1.62925488E+03
        real, parameter :: rc_eff_trj_4_pos =   1.62925488E+03
        real, parameter :: routpri =   1.17041465E+04
        real, parameter :: in_htot =   1.00000000E+02
        real, parameter :: ref_htot =   1.00000000E+02
        real, parameter :: ref_pr =   6.44039750E-01
        real, parameter :: ref_twt =   5.86001635E-01
        real, parameter :: ref_cratio =   1.00000000E+00
        real, parameter :: ref_grad =   7.80078471E-01
        real, parameter :: ref_utr =   3.46691298E+00
        real, parameter :: ref_vd_coarse_part =   1.26118263E-40
        real, parameter :: ref_vd_eff_trj_zra =   2.96227430E-04
        real, parameter :: ref_rkc =   1.00000000E+00
        real, parameter :: ref_ri =   1.25999999E+00
        real, parameter :: ref_vnatpri =   2.16914892E+00
        real, parameter :: ref_cgt =   0.00000000E+00
        real, parameter :: ref_cgt_z =   0.00000000E+00
        real, parameter :: ref_cq2 =   1.00000000E+00
        real, parameter :: ref_cdn =   1.00000000E+00
        real, parameter :: ref_cch =   9.31268215E-01
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call test_ops_depoparexp( do_proc, kdeel, c, qbstf, ra_rcp_4, ra_rcp_zra, &
             ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, ircp, istab, grof, xvghbr, xvglbr, regenk, &
             rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, xl, onder, dg, knatdeppar, &
             scavcoef, irev, htt, xloc, xl100, vw10, pcoef, vchem, dispg, z0_src, ol_src, &
             uster_src, ra_src_4, rb_src, rb_trj, ra_trj_zra, xm, ym, zm, bx, by, &
             rc_eff_rcp_4_pos, rc_eff_src_4_pos, rc_eff_trj_4_pos, routpri, in_htot, ref_htot, &
             ref_pr, ref_twt, ref_cratio, ref_grad, ref_utr, ref_vd_coarse_part, &
             ref_vd_eff_trj_zra, ref_rkc, ref_ri, ref_vnatpri, ref_cgt, ref_cgt_z, ref_cq2, &
             ref_cdn, ref_cch, __LINE__, __FILE__)
   end subroutine test_ops_depoparexp3

   subroutine test_ops_depoparexp2()
      use m_ops_tdo_proc, only: tdo_proc
      
        type(tdo_proc) :: do_proc
        integer, parameter :: kdeel = 1
        real, parameter :: c =   9.30537879E-02
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: ra_rcp_4 =   2.72117901E+01
        real, parameter :: ra_rcp_zra =   6.90764313E+01
        real, parameter :: ra_rcp_zrcp =   2.72117901E+01
        real, parameter :: rb_rcp =   2.44440670E+01
        real, parameter :: sigz =   1.37633621E+02
        real, parameter :: ueff =   5.62392044E+00
        real, parameter :: virty =   0.00000000E+00
        logical, parameter :: gasv = .true.
        integer, parameter :: ircp = -999
        integer, parameter :: istab = 3
        real, parameter :: grof =   0.00000000E+00
        real, parameter :: xvghbr =   9.61942077E-01
        real, parameter :: xvglbr =   1.01691461E+00
        real, parameter :: regenk =   5.78000024E-02
        real, parameter :: rint =   1.35000002E+00
        real, parameter :: buil =   1.13000000E+00
        real, parameter :: zf =   5.00000000E-01
        integer, parameter :: isec1 = 5
        integer, parameter :: iseiz = 1
        integer, parameter :: mb = 1
        real, parameter :: disx = -999.0
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: xl =   2.85942810E+02
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: dg =   2.29999989E-01
        integer, parameter :: knatdeppar = 3
        real, parameter :: scavcoef =   0.00000000E+00
        logical, parameter :: irev = .false.
        real, parameter :: htt =   5.00000000E+00
        real, parameter :: xloc =   2.81999969E+02
        real, parameter :: xl100 =   2.81999969E+02
        real, parameter :: vw10 =   3.46342969E+00
        real, parameter :: pcoef =   3.54666650E-01
        real, parameter :: vchem =   0.00000000E+00
        real, parameter :: dispg =   7.10568428E-02
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: ol_src =   1.39790878E+02
        real, parameter :: uster_src =   3.21115643E-01
        real, parameter :: ra_src_4 =   2.44179420E+01
        real, parameter :: rb_src =   2.24919357E+01
        real, parameter :: rb_trj =   2.24919376E+01
        real, parameter :: ra_trj_zra =   5.01215210E+01
        real, parameter :: xm =   1.40050000E+05
        real, parameter :: ym =   3.99950000E+05
        real, parameter :: zm =   4.00000000E+00
        integer, parameter :: bx = 155000
        integer, parameter :: by = 385000
        real, parameter :: rc_eff_rcp_4_pos =   1.30000000E+01
        real, parameter :: rc_eff_src_4_pos =   1.30000000E+01
        real, parameter :: rc_eff_trj_4_pos =   1.30000000E+01
        real, parameter :: routpri =   1.00000000E+06
        real, parameter :: in_htot =   5.00000000E+00
        real, parameter :: ref_htot =   5.00000000E+00
        real, parameter :: ref_pr =   1.15540028E-01
        real, parameter :: ref_twt =   3.24517757E-01
        real, parameter :: ref_cratio =   1.00000000E+00
        real, parameter :: ref_grad =   6.06980383E-01
        real, parameter :: ref_utr =   4.17158270E+00
        real, parameter :: ref_vd_coarse_part =   1.26118263E-40
        real, parameter :: ref_vd_eff_trj_zra =   1.18779764E-02
        real, parameter :: ref_rkc =   1.00000000E+00
        real, parameter :: ref_ri =   1.35000002E+00
        real, parameter :: ref_vnatpri =   3.92703915E+00
        real, parameter :: ref_cgt =   3.93019617E-01
        real, parameter :: ref_cgt_z =   3.93019617E-01
        real, parameter :: ref_cq2 =   3.83049458E-01
        real, parameter :: ref_cdn =   1.00000000E+00
        real, parameter :: ref_cch =   9.46214139E-01
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call test_ops_depoparexp( do_proc, kdeel, c, qbstf, ra_rcp_4, ra_rcp_zra, &
             ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, ircp, istab, grof, xvghbr, xvglbr, regenk, &
             rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, xl, onder, dg, knatdeppar, &
             scavcoef, irev, htt, xloc, xl100, vw10, pcoef, vchem, dispg, z0_src, ol_src, &
             uster_src, ra_src_4, rb_src, rb_trj, ra_trj_zra, xm, ym, zm, bx, by, &
             rc_eff_rcp_4_pos, rc_eff_src_4_pos, rc_eff_trj_4_pos, routpri, in_htot, ref_htot, &
             ref_pr, ref_twt, ref_cratio, ref_grad, ref_utr, ref_vd_coarse_part, &
             ref_vd_eff_trj_zra, ref_rkc, ref_ri, ref_vnatpri, ref_cgt, ref_cgt_z, ref_cq2, &
             ref_cdn, ref_cch, __LINE__, __FILE__)
   end subroutine test_ops_depoparexp2

   subroutine test_ops_depoparexp1()
      use m_ops_tdo_proc, only: tdo_proc
      
        type(tdo_proc) :: do_proc
        integer, parameter :: kdeel = 1
        real, parameter :: c =   6.86089471E-02
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: ra_rcp_4 =   2.43975487E+01
        real, parameter :: ra_rcp_zra =   3.31806717E+01
        real, parameter :: ra_rcp_zrcp =   2.43975487E+01
        real, parameter :: rb_rcp =   3.01502495E+01
        real, parameter :: sigz =   7.55815918E+02
        real, parameter :: ueff =   4.44325399E+00
        real, parameter :: virty =   0.00000000E+00
        logical, parameter :: gasv = .true.
        integer, parameter :: ircp = -999
        integer, parameter :: istab = 1
        real, parameter :: grof =   0.00000000E+00
        real, parameter :: xvghbr =   1.02960062E+00
        real, parameter :: xvglbr =   1.07823014E+00
        real, parameter :: regenk =   1.70000009E-02
        real, parameter :: rint =   1.77999997E+00
        real, parameter :: buil =   2.27999997E+00
        real, parameter :: zf =   5.00000000E-01
        integer, parameter :: isec1 = 5
        integer, parameter :: iseiz = 1
        integer, parameter :: mb = 1
        real, parameter :: disx = -999.0
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: xl =   2.96321716E+02
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: dg =   2.29999989E-01
        integer, parameter :: knatdeppar = 3
        real, parameter :: scavcoef =   0.00000000E+00
        logical, parameter :: irev = .false.
        real, parameter :: htt =   5.00000000E+00
        real, parameter :: xloc =   2.08999985E+02
        real, parameter :: xl100 =   6.21999939E+02
        real, parameter :: vw10 =   2.01143265E+00
        real, parameter :: pcoef =   2.94000000E-01
        real, parameter :: vchem =   0.00000000E+00
        real, parameter :: dispg =   2.14678213E-01
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: ol_src =  -3.29010468E+01
        real, parameter :: uster_src =   2.52821952E-01
        real, parameter :: ra_src_4 =   2.40340176E+01
        real, parameter :: rb_src =   2.85675869E+01
        real, parameter :: rb_trj =   2.85675850E+01
        real, parameter :: ra_trj_zra =   3.47806282E+01
        real, parameter :: xm =   1.40050000E+05
        real, parameter :: ym =   3.99950000E+05
        real, parameter :: zm =   4.00000000E+00
        integer, parameter :: bx = 155000
        integer, parameter :: by = 385000
        real, parameter :: rc_eff_rcp_4_pos =   1.30000000E+01
        real, parameter :: rc_eff_src_4_pos =   1.30000000E+01
        real, parameter :: rc_eff_trj_4_pos =   1.30000000E+01
        real, parameter :: routpri =   1.00000000E+06
        real, parameter :: in_htot =   5.00000000E+00
        real, parameter :: ref_htot =   5.00000000E+00
        real, parameter :: ref_pr =   9.26025271E-01
        real, parameter :: ref_twt =   4.50051814E-01
        real, parameter :: ref_cratio =   1.00000000E+00
        real, parameter :: ref_grad =   8.84933650E-01
        real, parameter :: ref_utr =   4.13313913E+00
        real, parameter :: ref_vd_coarse_part =   1.26118263E-40
        real, parameter :: ref_vd_eff_trj_zra =   1.41225327E-02
        real, parameter :: ref_rkc =   1.00000000E+00
        real, parameter :: ref_ri =   1.77999997E+00
        real, parameter :: ref_vnatpri =   3.32806897E+00
        real, parameter :: ref_cgt =   1.15066350E-01
        real, parameter :: ref_cgt_z =   1.15066350E-01
        real, parameter :: ref_cq2 =   7.62434900E-01
        real, parameter :: ref_cdn =   8.35698187E-01
        real, parameter :: ref_cch =   9.53811109E-01
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        call test_ops_depoparexp( do_proc, kdeel, c, qbstf, ra_rcp_4, ra_rcp_zra, &
             ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, ircp, istab, grof, xvghbr, xvglbr, regenk, &
             rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, xl, onder, dg, knatdeppar, &
             scavcoef, irev, htt, xloc, xl100, vw10, pcoef, vchem, dispg, z0_src, ol_src, &
             uster_src, ra_src_4, rb_src, rb_trj, ra_trj_zra, xm, ym, zm, bx, by, &
             rc_eff_rcp_4_pos, rc_eff_src_4_pos, rc_eff_trj_4_pos, routpri, in_htot, ref_htot, &
             ref_pr, ref_twt, ref_cratio, ref_grad, ref_utr, ref_vd_coarse_part, &
             ref_vd_eff_trj_zra, ref_rkc, ref_ri, ref_vnatpri, ref_cgt, ref_cgt_z, ref_cq2, &
             ref_cdn, ref_cch, __LINE__, __FILE__)
   end subroutine test_ops_depoparexp1

   subroutine test_ops_depoparexp( do_proc, kdeel, c, qbstf, ra_rcp_4, ra_rcp_zra, &
      ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, ircp, istab, grof, xvghbr, xvglbr, regenk, &
      rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, xl, onder, dg, knatdeppar, &
      scavcoef, irev, htt, xloc, xl100, vw10, pcoef, vchem, dispg, z0_src, ol_src, &
      uster_src, ra_src_4, rb_src, rb_trj, ra_trj_zra, xm, ym, zm, bx, by, &
      rc_eff_rcp_4_pos, rc_eff_src_4_pos, rc_eff_trj_4_pos, routpri, in_htot, ref_htot, &
      ref_pr, ref_twt, ref_cratio, ref_grad, ref_utr, ref_vd_coarse_part, &
      ref_vd_eff_trj_zra, ref_rkc, ref_ri, ref_vnatpri, ref_cgt, ref_cgt_z, ref_cq2, &
      ref_cdn, ref_cch, lineno, filename)
   use no_pfunit_ops_lt
   use m_ops_tdo_proc, only: tdo_proc
   use m_error, only: TError
   use m_ops_varin, only: Tvarin
        
        TYPE(Tvarin) :: varin
        type(tdo_proc), intent(in) :: do_proc
        integer, intent(in) :: kdeel
        real, intent(in) :: c, qbstf, ra_rcp_4, ra_rcp_zra, ra_rcp_zrcp, rb_rcp, sigz, ueff, virty
        logical, intent(in) :: gasv
        integer, intent(in) :: ircp
        integer, intent(in) :: istab
        real, intent(in) :: grof, xvghbr, xvglbr, regenk, rint, buil, zf
        integer, intent(in) :: isec1, iseiz, mb
        real, intent(in) :: disx, disxx, radius, xl, onder, dg
        integer, intent(in) :: knatdeppar
        real, intent(in) :: scavcoef
        logical, intent(in) :: irev
        real, intent(in) :: htt, xloc, xl100, vw10, pcoef, vchem, dispg, z0_src, ol_src, &
            uster_src, ra_src_4, rb_src, rb_trj, ra_trj_zra, xm, ym, zm
        integer, intent(in) :: bx, by
        real, intent(in) :: rc_eff_rcp_4_pos, rc_eff_src_4_pos, rc_eff_trj_4_pos, routpri, &
            in_htot, ref_htot, ref_pr, ref_twt, ref_cratio, ref_grad, ref_utr, &
            ref_vd_coarse_part, ref_vd_eff_trj_zra, ref_rkc, ref_ri, ref_vnatpri, ref_cgt, &
            ref_cgt_z, ref_cq2, ref_cdn, ref_cch
        integer, intent(in) :: lineno
        character(len=*), intent(in) :: filename

        real, parameter :: tol = 1e-5

        type(TError) :: error, ref_error
        real :: htot, pr, twt, cratio, grad, utr, vd_coarse_part, vd_eff_trj_zra, &
                rkc, ri, vnatpri, cgt, cgt_z, cq2, cdn, cch

        htot = in_htot
        vd_coarse_part = ref_vd_coarse_part
        call ops_depoparexp(varin, do_proc, kdeel, c, qbstf, ra_rcp_4, ra_rcp_zra, ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, &
                       &  ircp, istab, grof, xvghbr, xvglbr, regenk, rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, &
                       &  xl, onder, dg, knatdeppar, scavcoef, irev, htt, xloc, xl100, vw10, pcoef, vchem, dispg, htot, &
                       &  error, pr, twt, cratio, rc_eff_rcp_4_pos, grad, utr, routpri, vd_eff_trj_zra, rkc, ri, vnatpri, &
                       &  cgt, cgt_z, cq2, cdn, cch, z0_src, ol_src, uster_src, rc_eff_trj_4_pos, rc_eff_src_4_pos, ra_src_4, rb_src, &
                       &  ra_trj_zra, rb_trj, vd_coarse_part, xm, ym, zm, bx, by)
        call assertEqual( ref_htot, htot, tol, "htot", lineno, filename)
        call assertEqual(error, ref_error, "ops_depoparexp", lineno, filename)
        call assertEqual( ref_pr, pr, tol, "pr", lineno, filename)
        call assertEqual( ref_twt, twt, tol, "twt", lineno, filename)
        call assertEqual( ref_cratio, cratio, tol, "cratio", lineno, filename)
        call assertEqual( ref_grad, grad, tol, "grad", lineno, filename)
        call assertEqual( ref_utr, utr, tol, "utr", lineno, filename)
        call assertEqual( ref_vd_coarse_part, vd_coarse_part, tol, "vd_coarse_part", lineno, filename)
        call assertEqual( ref_vd_eff_trj_zra, vd_eff_trj_zra, tol, "vd_eff_trj_zra", lineno, filename)
        call assertEqual( ref_rkc, rkc, tol, "rkc", lineno, filename)
        call assertEqual( ref_ri, ri, tol, "ri", lineno, filename)
        call assertEqual( ref_vnatpri, vnatpri, tol, "vnatpri", lineno, filename)
        call assertEqual( ref_cgt, cgt, tol, "cgt", lineno, filename)
        call assertEqual( ref_cgt_z, cgt_z, tol, "cgt_z", lineno, filename)
        call assertEqual( ref_cq2, cq2, tol, "cq2", lineno, filename)
        call assertEqual( ref_cdn, cdn, tol, "cdn", lineno, filename)
        call assertEqual( ref_cch, cch, tol, "cch", lineno, filename)
   end subroutine test_ops_depoparexp

   subroutine test_par_nat11()
        real, parameter :: regenk =   3.08684688E-02
        real, parameter :: rint =   1.43999994, rint0 = 0.0
        real, parameter :: buil =   1.78999996
        real, parameter :: zf =   5.0
        integer, parameter :: isec1 = 6
        integer, parameter :: iseiz = 1, iseiz4=4
        integer, parameter :: mb = 1
        real, parameter :: disxx =   9.82344116E+02
        real, parameter :: radius =   9.94035767E+02
        real, parameter :: diameter =   1.77304956E+03
        real, parameter :: ueff =   2.22042251E+00
        real, parameter :: xl =   2.26793579E+02
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: sigz =   2.23733616E+01
        real, parameter :: htot =   5.00000000E+00
        logical, parameter :: gasv = .true.
        real, parameter :: dg =   0.00000000E+00
        integer, parameter :: knatdeppar = 3, knatdeppar1=1
        real, parameter :: scavcoef =   0.00000000E+00
        real, parameter :: routpri =   1.37694189E+04
        integer, parameter :: kdeel = 1
        logical, parameter :: irev = .false.
        real, parameter :: c =   6.48795366E+00
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: virty =   3.81971851E+03
        real, parameter :: ref_twt(2) =   (/ 2.19858810E-02,   2.242887578905E-02 /)
        real, parameter :: ref_pr =   4.31189529E-09
        real, parameter :: in_cratio =   1.00000000E+00
        real, parameter :: ref_cratio =   1.00000000E+00
        real, parameter :: ref_ri(2) =  (/ 1.43999994,  5.12000036240 /)
        real, parameter :: ref_a =   3.00000000E+00
        real, parameter :: ref_vnatpri =   1.16256726E-09
        call test_par_nat(regenk, rint, buil, zf, isec1, iseiz, mb, disxx, radius, diameter, &
             ueff, xl, onder, sigz, htot, gasv, dg, knatdeppar, scavcoef, routpri, kdeel, irev, c, &
             qbstf, virty, ref_twt(1), ref_pr, in_cratio, ref_cratio, ref_ri(1), ref_a, ref_vnatpri, &
             __LINE__, __FILE__)

        call test_par_nat(regenk, rint0, buil, zf, isec1, iseiz4, mb, disxx, radius, diameter, &
             ueff, xl, onder, sigz, htot, gasv, dg, knatdeppar, scavcoef, routpri, kdeel, irev, c, &
             qbstf, virty, ref_twt(2), ref_pr, in_cratio, ref_cratio, ref_ri(2), ref_a, ref_vnatpri, &
             __LINE__, __FILE__)

        call test_par_nat(regenk, rint0, buil, zf, isec1, iseiz4, mb, disxx, radius, diameter, &
             ueff, xl, onder, sigz, htot, gasv, dg, knatdeppar1, scavcoef, routpri, kdeel, irev, c, &
             qbstf, virty, ref_twt(2), ref_pr, in_cratio, ref_cratio, ref_ri(2), ref_a, ref_vnatpri, &
             __LINE__, __FILE__)
   end subroutine test_par_nat11

   subroutine test_par_nat10()
        real, parameter :: regenk =   2.30019055E-02
        real, parameter :: rint =   1.77999997E+00
        real, parameter :: buil =   2.27999997E+00
        real, parameter :: zf =   5.00000000E-01
        integer, parameter :: isec1 = 5
        integer, parameter :: iseiz = 1
        integer, parameter :: mb = 1
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: radius =   9.94035767E+02
        real, parameter :: diameter =   1.77304956E+03
        real, parameter :: ueff =   4.36820602E+00
        real, parameter :: xl =   2.79640625E+02
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: sigz =   6.57454468E+02
        real, parameter :: htot =   5.00000000E+00
        logical, parameter :: gasv = .true.
        real, parameter :: dg =   0.00000000E+00
        integer, parameter :: knatdeppar = 3
        real, parameter :: scavcoef =   0.00000000E+00
        real, parameter :: routpri =   1.17041465E+04
        integer, parameter :: kdeel = 1
        logical, parameter :: irev = .false.
        real, parameter :: c =   6.26347139E-02
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: virty =   3.81971851E+03
        real, parameter :: ref_twt =   4.48477149E-01
        real, parameter :: ref_pr =   9.13520694E-01
        real, parameter :: in_cratio =   1.00000000E+00
        real, parameter :: ref_cratio =   1.00000000E+00
        real, parameter :: ref_ri =   1.77999997E+00
        real, parameter :: ref_a =   1.00000000E+00
        real, parameter :: ref_vnatpri =   1.53959602E-01
        call test_par_nat( regenk, rint, buil, zf, isec1, iseiz, mb, disxx, radius, diameter, &
             ueff, xl, onder, sigz, htot, gasv, dg, knatdeppar, scavcoef, routpri, kdeel, irev, c, &
             qbstf, virty, ref_twt, ref_pr, in_cratio, ref_cratio, ref_ri, ref_a, ref_vnatpri, &
             __LINE__, __FILE__)
   end subroutine test_par_nat10

   subroutine test_par_nat9()
        real, parameter :: regenk =   2.15000007E-02
        real, parameter :: rint =   1.84999990E+00
        real, parameter :: buil =   2.75000000E+00
        real, parameter :: zf =   5.00000000E-01
        integer, parameter :: isec1 = 6
        integer, parameter :: iseiz = 1
        integer, parameter :: mb = 1
        real, parameter :: disxx =   1.54888672E+04
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: diameter =   0.00000000E+00
        real, parameter :: ueff =   5.44364977E+00
        real, parameter :: xl =   8.55794800E+02
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: sigz =   1.28272913E+03
        real, parameter :: htot =   5.00000000E+00
        logical, parameter :: gasv = .true.
        real, parameter :: dg =   1.36000007E-01
        integer, parameter :: knatdeppar = 3
        real, parameter :: scavcoef =   0.00000000E+00
        real, parameter :: routpri =   5.19087375E+05
        integer, parameter :: kdeel = 1
        logical, parameter :: irev = .true.
        real, parameter :: c =   2.56587472E-02
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: ref_twt =   2.91549683E-01
        real, parameter :: ref_pr =   6.32662594E-01
        real, parameter :: in_cratio =   1.00000000E+00
        real, parameter :: ref_cratio =   9.69423532E-01
        real, parameter :: ref_ri =   1.84999990E+00
        real, parameter :: ref_a =   1.00000000E+00
        real, parameter :: ref_vnatpri =   1.55350506E+00
        call test_par_nat( regenk, rint, buil, zf, isec1, iseiz, mb, disxx, radius, diameter, &
             ueff, xl, onder, sigz, htot, gasv, dg, knatdeppar, scavcoef, routpri, kdeel, irev, c, &
             qbstf, virty, ref_twt, ref_pr, in_cratio, ref_cratio, ref_ri, ref_a, ref_vnatpri, &
             __LINE__, __FILE__)
   end subroutine test_par_nat9

   subroutine test_par_nat8()
        real, parameter :: regenk =   1.70000009E-02
        real, parameter :: rint =   1.77999997E+00
        real, parameter :: buil =   2.27999997E+00
        real, parameter :: zf =   5.00000000E-01
        integer, parameter :: isec1 = 5
        integer, parameter :: iseiz = 1
        integer, parameter :: mb = 1
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: diameter =   0.00000000E+00
        real, parameter :: ueff =   4.44325399E+00
        real, parameter :: xl =   2.96321716E+02
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: sigz =   7.55815918E+02
        real, parameter :: htot =   5.00000000E+00
        logical, parameter :: gasv = .true.
        real, parameter :: dg =   1.36000007E-01
        integer, parameter :: knatdeppar = 3
        real, parameter :: scavcoef =   0.00000000E+00
        real, parameter :: routpri =   4.48240438E+05
        integer, parameter :: kdeel = 1
        logical, parameter :: irev = .true.
        real, parameter :: c =   6.86089471E-02
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: ref_twt =   4.50051814E-01
        real, parameter :: ref_pr =   9.26025271E-01
        real, parameter :: in_cratio =   1.00000000E+00
        real, parameter :: ref_cratio =   1.00000000E+00
        real, parameter :: ref_ri =   1.77999997E+00
        real, parameter :: ref_a =   1.00000000E+00
        real, parameter :: ref_vnatpri =   2.50211477E+00
        call test_par_nat( regenk, rint, buil, zf, isec1, iseiz, mb, disxx, radius, diameter, &
             ueff, xl, onder, sigz, htot, gasv, dg, knatdeppar, scavcoef, routpri, kdeel, irev, c, &
             qbstf, virty, ref_twt, ref_pr, in_cratio, ref_cratio, ref_ri, ref_a, ref_vnatpri, &
             __LINE__, __FILE__)
   end subroutine test_par_nat8

   subroutine test_par_nat7()
        real, parameter :: regenk =   3.55333388E-02
        real, parameter :: rint =   1.25999999E+00
        real, parameter :: buil =   3.25000000E+00
        real, parameter :: zf =   5.00000000E-01
        integer, parameter :: isec1 = 5
        integer, parameter :: iseiz = 1
        integer, parameter :: mb = 1
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: diameter =   0.00000000E+00
        real, parameter :: ueff =   3.46691298E+00
        real, parameter :: xl =   8.00000000E+01
        real, parameter :: onder =   0.00000000E+00
        real, parameter :: sigz =   5.33004904E+00
        real, parameter :: htot =   1.00000000E+02
        logical, parameter :: gasv = .true.
        real, parameter :: dg =   0.00000000E+00
        integer, parameter :: knatdeppar = 3
        real, parameter :: scavcoef =   0.00000000E+00
        real, parameter :: routpri =   1.17041465E+04
        integer, parameter :: kdeel = 1
        logical, parameter :: irev = .false.
        real, parameter :: c =   0.00000000E+00
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: ref_twt =   5.86001635E-01
        real, parameter :: ref_pr =   6.44039750E-01
        real, parameter :: in_cratio =   1.00000000E+00
        real, parameter :: ref_cratio =   1.00000000E+00
        real, parameter :: ref_ri =   1.25999999E+00
        real, parameter :: ref_a =   1.00000000E+00
        real, parameter :: ref_vnatpri =   2.16914892E+00
        call test_par_nat( regenk, rint, buil, zf, isec1, iseiz, mb, disxx, radius, diameter, &
             ueff, xl, onder, sigz, htot, gasv, dg, knatdeppar, scavcoef, routpri, kdeel, irev, c, &
             qbstf, virty, ref_twt, ref_pr, in_cratio, ref_cratio, ref_ri, ref_a, ref_vnatpri, &
             __LINE__, __FILE__)
   end subroutine test_par_nat7

   subroutine test_par_nat6()
        real, parameter :: regenk =   3.55333388E-02
        real, parameter :: rint =   1.25999999E+00
        real, parameter :: buil =   3.25000000E+00
        real, parameter :: zf =   5.00000000E-01
        integer, parameter :: isec1 = 5
        integer, parameter :: iseiz = 1
        integer, parameter :: mb = 1
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: diameter =   0.00000000E+00
        real, parameter :: ueff =   3.46691298E+00
        real, parameter :: xl =   8.00000000E+01
        real, parameter :: onder =   0.00000000E+00
        real, parameter :: sigz =   5.33004904E+00
        real, parameter :: htot =   1.00000000E+02
        logical, parameter :: gasv = .true.
        real, parameter :: dg =   0.00000000E+00
        integer, parameter :: knatdeppar = 3
        real, parameter :: scavcoef =   0.00000000E+00
        real, parameter :: routpri =   1.17041465E+04
        integer, parameter :: kdeel = 1
        logical, parameter :: irev = .false.
        real, parameter :: c =   0.00000000E+00
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: ref_twt =   5.86001635E-01
        real, parameter :: ref_pr =   6.44039750E-01
        real, parameter :: in_cratio =   1.00000000E+00
        real, parameter :: ref_cratio =   1.00000000E+00
        real, parameter :: ref_ri =   1.25999999E+00
        real, parameter :: ref_a =   1.00000000E+00
        real, parameter :: ref_vnatpri =   2.16914892E+00
        call test_par_nat( regenk, rint, buil, zf, isec1, iseiz, mb, disxx, radius, diameter, &
             ueff, xl, onder, sigz, htot, gasv, dg, knatdeppar, scavcoef, routpri, kdeel, irev, c, &
             qbstf, virty, ref_twt, ref_pr, in_cratio, ref_cratio, ref_ri, ref_a, ref_vnatpri, &
             __LINE__, __FILE__)
   end subroutine test_par_nat6

   subroutine test_par_nat5()
        real, parameter :: regenk =   1.70000009E-02
        real, parameter :: rint =   1.77999997E+00
        real, parameter :: buil =   2.27999997E+00
        real, parameter :: zf =   5.00000000E-01
        integer, parameter :: isec1 = 5
        integer, parameter :: iseiz = 1
        integer, parameter :: mb = 1
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: diameter =   0.00000000E+00
        real, parameter :: ueff =   4.44325399E+00
        real, parameter :: xl =   2.96321716E+02
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: sigz =   7.55815918E+02
        real, parameter :: htot =   5.00000000E+00
        logical, parameter :: gasv = .false.
        real, parameter :: dg =  -9.99900000E+03
        integer, parameter :: knatdeppar = -9999
        real, parameter :: scavcoef =   0.00000000E+00
        real, parameter :: routpri =   2.40000000E+05
        integer, parameter :: kdeel = 1
        logical, parameter :: irev = .false.
        real, parameter :: c =   4.32236400E-03
        real, parameter :: qbstf =   6.30000010E-02
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: ref_twt =   4.50051814E-01
        real, parameter :: ref_pr =   9.26025271E-01
        real, parameter :: in_cratio =   1.00000000E+00
        real, parameter :: ref_cratio =   1.00000000E+00
        real, parameter :: ref_ri =   1.77999997E+00
        real, parameter :: ref_a =   1.00000000E+00
        real, parameter :: ref_vnatpri =   1.66973603E+00
        call test_par_nat( regenk, rint, buil, zf, isec1, iseiz, mb, disxx, radius, diameter, &
             ueff, xl, onder, sigz, htot, gasv, dg, knatdeppar, scavcoef, routpri, kdeel, irev, c, &
             qbstf, virty, ref_twt, ref_pr, in_cratio, ref_cratio, ref_ri, ref_a, ref_vnatpri, &
             __LINE__, __FILE__)
   end subroutine test_par_nat5

   subroutine test_par_nat4()
        real, parameter :: regenk =   1.62500009E-01
        real, parameter :: rint =   1.00000000E+00
        real, parameter :: buil =   2.57999992E+00
        real, parameter :: zf =   5.00000000E-01
        integer, parameter :: isec1 = 7
        integer, parameter :: iseiz = 1
        integer, parameter :: mb = 1
        real, parameter :: disxx =   5.70087708E+02
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: diameter =   0.00000000E+00
        real, parameter :: ueff =   6.58759069E+00
        real, parameter :: xl =   5.22908569E+02
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: sigz =   2.89851418E+01
        real, parameter :: htot =   5.00000000E+00
        logical, parameter :: gasv = .true.
        real, parameter :: dg =   2.29999989E-01
        integer, parameter :: knatdeppar = 3
        real, parameter :: scavcoef =   0.00000000E+00
        real, parameter :: routpri =   1.00000000E+06
        integer, parameter :: kdeel = 1
        logical, parameter :: irev = .false.
        real, parameter :: c =   1.37922516E+01
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: ref_twt =   9.99999978E-03
        real, parameter :: ref_pr =   0.00000000E+00
        real, parameter :: in_cratio =   1.00000000E+00
        real, parameter :: ref_cratio =   1.00000000E+00
        real, parameter :: ref_ri =   1.00000000E+00
        real, parameter :: ref_a =   1.00000000E+00
        real, parameter :: ref_vnatpri =   6.57478714E+00
        call test_par_nat( regenk, rint, buil, zf, isec1, iseiz, mb, disxx, radius, diameter, &
             ueff, xl, onder, sigz, htot, gasv, dg, knatdeppar, scavcoef, routpri, kdeel, irev, c, &
             qbstf, virty, ref_twt, ref_pr, in_cratio, ref_cratio, ref_ri, ref_a, ref_vnatpri, &
             __LINE__, __FILE__)
   end subroutine test_par_nat4

   subroutine test_par_nat3()
        real, parameter :: regenk =   0.00000000E+00
        real, parameter :: rint =   0.00000000E+00
        real, parameter :: buil =   0.00000000E+00
        real, parameter :: zf =   5.00000000E-01
        integer, parameter :: isec1 = 5
        integer, parameter :: iseiz = 1
        integer, parameter :: mb = 1
        real, parameter :: disxx =   1.73223848E+04
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: diameter =   0.00000000E+00
        real, parameter :: ueff =   8.04345226E+00
        real, parameter :: xl =   4.81590851E+02
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: sigz =   1.80795395E+02
        real, parameter :: htot =   5.00000000E+00
        logical, parameter :: gasv = .true.
        real, parameter :: dg =   2.29999989E-01
        integer, parameter :: knatdeppar = 3
        real, parameter :: scavcoef =   0.00000000E+00
        real, parameter :: routpri =   1.00000000E+06
        integer, parameter :: kdeel = 1
        logical, parameter :: irev = .false.
        real, parameter :: c =   6.04695715E-02
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: ref_twt =   2.83734411E-01
        real, parameter :: ref_pr =   6.66849464E-02
        real, parameter :: in_cratio =   1.00000000E+00
        real, parameter :: ref_cratio =   1.00000000E+00
        real, parameter :: ref_ri =   1.35000002E+00
        real, parameter :: ref_a =   5.12470767E-38
        real, parameter :: ref_vnatpri =   0.00000000E+00
        call test_par_nat( regenk, rint, buil, zf, isec1, iseiz, mb, disxx, radius, diameter, &
             ueff, xl, onder, sigz, htot, gasv, dg, knatdeppar, scavcoef, routpri, kdeel, irev, c, &
             qbstf, virty, ref_twt, ref_pr, in_cratio, ref_cratio, ref_ri, ref_a, ref_vnatpri, &
             __LINE__, __FILE__)
   end subroutine test_par_nat3

   subroutine test_par_nat2()
        real, parameter :: regenk =   1.02200009E-01
        real, parameter :: rint =   0.00000000E+00
        real, parameter :: buil =   0.00000000E+00
        real, parameter :: zf =   5.00000000E-01
        integer, parameter :: isec1 = 5
        integer, parameter :: iseiz = 1
        integer, parameter :: mb = 1
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: diameter =   0.00000000E+00
        real, parameter :: ueff =   8.01123238E+00
        real, parameter :: xl =   4.85028931E+02
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: sigz =   1.98663025E+02
        real, parameter :: htot =   5.00000000E+00
        logical, parameter :: gasv = .true.
        real, parameter :: dg =   2.29999989E-01
        integer, parameter :: knatdeppar = 3
        real, parameter :: scavcoef =   0.00000000E+00
        real, parameter :: routpri =   1.00000000E+06
        integer, parameter :: kdeel = 1
        logical, parameter :: irev = .false.
        real, parameter :: c =   4.52720709E-02
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: ref_twt =   2.67932326E-01
        real, parameter :: ref_pr =   3.72214206E-02
        real, parameter :: in_cratio =   1.00000000E+00
        real, parameter :: ref_cratio =   1.00000000E+00
        real, parameter :: ref_ri =   1.39999998E+00
        real, parameter :: ref_a =   1.00000000E+00
        real, parameter :: ref_vnatpri =   5.37613344E+00
        call test_par_nat( regenk, rint, buil, zf, isec1, iseiz, mb, disxx, radius, diameter, &
             ueff, xl, onder, sigz, htot, gasv, dg, knatdeppar, scavcoef, routpri, kdeel, irev, c, &
             qbstf, virty, ref_twt, ref_pr, in_cratio, ref_cratio, ref_ri, ref_a, ref_vnatpri, &
             __LINE__, __FILE__)
   end subroutine test_par_nat2

   subroutine test_par_nat1()
        real, parameter :: regenk =   1.70000009E-02
        real, parameter :: rint =   1.77999997E+00
        real, parameter :: buil =   2.27999997E+00
        real, parameter :: zf =   5.00000000E-01
        integer, parameter :: isec1 = 5
        integer, parameter :: iseiz = 1
        integer, parameter :: mb = 1
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: diameter =   0.00000000E+00
        real, parameter :: ueff =   4.44325399E+00
        real, parameter :: xl =   2.96321716E+02
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: sigz =   7.55815918E+02
        real, parameter :: htot =   5.00000000E+00
        logical, parameter :: gasv = .true.
        real, parameter :: dg =   2.29999989E-01
        integer, parameter :: knatdeppar = 3
        real, parameter :: scavcoef =   0.00000000E+00
        real, parameter :: routpri =   1.00000000E+06
        integer, parameter :: kdeel = 1
        logical, parameter :: irev = .false.
        real, parameter :: c =   6.86089471E-02
        real, parameter :: qbstf =   1.00000000E+00
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: ref_twt =   4.50051814E-01
        real, parameter :: ref_pr =   9.26025271E-01
        real, parameter :: in_cratio =   1.00000000E+00
        real, parameter :: ref_cratio =   1.00000000E+00
        real, parameter :: ref_ri =   1.77999997E+00
        real, parameter :: ref_a =   1.00000000E+00
        real, parameter :: ref_vnatpri =   3.32806897E+00
        call test_par_nat( regenk, rint, buil, zf, isec1, iseiz, mb, disxx, radius, diameter, &
             ueff, xl, onder, sigz, htot, gasv, dg, knatdeppar, scavcoef, routpri, kdeel, irev, c, &
             qbstf, virty, ref_twt, ref_pr, in_cratio, ref_cratio, ref_ri, ref_a, ref_vnatpri, &
             __LINE__, __FILE__)
   end subroutine test_par_nat1

   subroutine test_par_nat(regenk, rint, buil, zf, isec1, iseiz, mb, disxx, radius, diameter, &
      ueff, xl, onder, sigz, htot, gasv, dg, knatdeppar, scavcoef, routpri, kdeel, irev, c, &
      qbstf, virty, ref_twt, ref_pr, in_cratio, ref_cratio, ref_ri, ref_a, ref_vnatpri, &
      lineno, filename)
      use no_pfunit_ops_lt
      use m_ops_varin, only: Tvarin_unc

      real, intent(in) :: regenk, rint, buil, zf
      integer, intent(in) :: isec1, iseiz, mb
      real, intent(in) :: disxx, radius, diameter, ueff, xl, onder, sigz, htot
      logical, intent(in) :: gasv
      real, intent(in) :: dg
      integer, intent(in) :: knatdeppar
      real, intent(in) :: scavcoef, routpri
      integer, intent(in) :: kdeel
      logical, intent(in) :: irev
      real, intent(in) :: c, qbstf, virty, ref_twt, ref_pr, in_cratio, ref_cratio, ref_ri, ref_a, ref_vnatpri
      integer, intent(in) :: lineno
      character(len=*), intent(in) :: filename

      type(Tvarin_unc) :: varin_unc
      real, parameter :: tol = 1e-5

      real :: twt, pr, cratio, ri, a, vnatpri

      ri = ref_ri
      twt = ref_twt
      pr = ref_pr
      a = ref_a
      cratio = in_cratio
      call par_nat(varin_unc, regenk, rint, buil, zf, isec1, iseiz, mb, disxx, radius, diameter, ueff, xl, onder, &
         sigz, htot, gasv, dg, knatdeppar, scavcoef, routpri, kdeel, irev, c, qbstf, virty, twt, pr, &
         cratio, ri, a, vnatpri)
      call assertEqual( ref_twt, twt, tol, "twt", lineno, filename)
      call assertEqual( ref_pr, pr, tol, "pr", lineno, filename)
      call assertEqual( ref_cratio, cratio, tol, "cratio", lineno, filename)
      call assertEqual( ref_ri, ri, tol, "ri", lineno, filename)
      call assertEqual( ref_a, a, tol, "a", lineno, filename)
      call assertEqual( ref_vnatpri, vnatpri, tol, "vnatpri", lineno, filename)
   end subroutine test_par_nat
end module m_test_m_ops_depoparexp
 
program p_test_m_ops_depoparexp
use m_test_m_ops_depoparexp
use no_pfunit
implicit none
   call test_par_nat11()
   call test_par_nat10()
   call test_par_nat9()
   call test_par_nat8()
   call test_par_nat7()
   call test_par_nat6()
   call test_par_nat5()
   call test_par_nat4()
   call test_par_nat3()
   call test_par_nat2()
   call test_par_nat1()
   call test_ops_depoparexp7()
   call test_ops_depoparexp6()
   call test_ops_depoparexp5()
   call test_ops_depoparexp4()
   call test_ops_depoparexp3()
   call test_ops_depoparexp2()
   call test_ops_depoparexp1()
   call conclusion()
end program p_test_m_ops_depoparexp

