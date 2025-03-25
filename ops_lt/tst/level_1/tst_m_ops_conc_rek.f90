module m_test_ops_conc_rek
implicit none
contains
   subroutine test_ops_conc_rek4()
   use m_ops_conc_rek
   use no_pfunit_ops_lt
   use m_ops_varin, only: Tvarin_unc
   use m_ops_tdo_proc, only: tdo_proc
       real, parameter :: tol = 1e-5
        type(Tvarin_unc) :: varin_unc
        type(tdo_proc) :: do_proc
        real, parameter :: ueff =   1.05651846E+01
        real, parameter :: qbpri =   1.00000000E+00
        logical, parameter :: isec = .true.
        real, parameter :: rc_sec_trj =   1.47900681E+02
        real, parameter :: routsec =   1.40000000E+07
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: c0_undepl_total   =   2.64936284E-23
        real, parameter :: c0_undepl_mix     =   onder*c0_undepl_total
        real, parameter :: c_zrcp_undepl_mix =   c0_undepl_mix 
        real, parameter :: amol1 =   4.60000000E+01
        real, parameter :: amol2 =   6.20000000E+01
        real, parameter :: sigz =   9.42695618E+00
        real, parameter :: utr =   1.04410610E+01
        real, parameter :: rc_sec_rcp =   1.53478928E+02
        real, parameter :: ra_rcp_4 =   1.51043720E+01
        real, parameter :: ra_rcp_zra =   3.18088875E+01
        real, parameter :: rb_rcp =   1.42140751E+01
        real, parameter :: amol21 =   1.34782612E+00
        real, parameter :: ugmoldep =   2.78000012E-10
        real, parameter :: cch =   9.99918699E-01
        real, parameter :: cgt =   4.75000998E-05
        real, parameter :: cgt_z =   4.75000998E-05
        real, parameter :: grof =   0.00000000E+00, grof1 = 1.0
        real, parameter :: percvk =   1.94347347E-03
        real, parameter :: regenk =   7.30000362E-02
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: ri =   1.39999998E+00
        real, parameter :: vw10 =   5.39232683E+00
        real, parameter :: hbron =   1.00000000E+02
        real, parameter :: pcoef =   2.89999992E-01
        real, parameter :: rkc =   1.00000000E+00
        real, parameter :: disxx =   2.12132034E+02
        real, parameter :: vnatpri =   0.00000000E+00
        real, parameter :: vchem =   1.44014645E+00
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: xl =   4.66191803E+02
        real, parameter :: xloc =   4.65999969E+02
        real, parameter :: htot =   1.00000000E+02
        real, parameter :: twt =   9.99999978E-03
        real, parameter :: xvghbr =   9.99893427E-01
        real, parameter :: xvglbr =   1.00262153E+00
        real, parameter :: grad =   9.87567365E-01
        real, parameter :: frac =   1.00000000E+00
        real, parameter :: ra_trj_zra =   2.68197346E+01
        real, parameter :: rb_trj =   1.35198984E+01
        real, parameter :: rc_eff_rcp_4 =   1.29757507E+03
        real, parameter :: vd_coarse_part =   4.59163468E-41
        real, parameter :: buildingFact =   1.00000000E+00
        integer, parameter :: nparout = 4
        logical, parameter :: parout_write = .false.
        real, parameter :: in_cdn =   1.00000000E+00
        real, parameter :: in_cq2 =   1.00000000E+00
        double precision, parameter :: in_sdrypri =   1.86017389E+03
        double precision, parameter :: in_sdrysec =   5.96986279E+01
        double precision, parameter :: in_snatsec =   2.59382162E+02
        double precision, parameter :: in_somvnsec =   2.59382162E+02
        double precision, parameter :: in_telvnsec =   2.70799703E+01
        double precision, parameter :: in_vvchem =   4.21246138E+03
        double precision, parameter :: in_vtel =   1.54338997E+03
        double precision, parameter :: in_snatpri =   4.01397443E+02
        double precision, parameter :: in_somvnpri =   4.01397443E+02
        double precision, parameter :: in_telvnpri =   1.54338997E+03
        double precision, parameter :: in_ddepri =   2.09909943E-10
        double precision, parameter :: in_wdepri =   3.29257688E-17
        double precision, parameter :: in_drydep =   2.10075736E-10
        double precision, parameter :: in_wetdep =   4.52234194E-14
        real, parameter :: in_parout_val(4) = (/   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00/)
        real :: cdn
        real, parameter :: ref_cdn =   1.00000000E+00
        real :: cq2
        real, parameter :: ref_cq2 =   1.00000000E+00
        real :: c_zrcp
        real, parameter :: ref_c =   2.64902174E-23
        double precision :: sdrypri
        double precision, parameter :: ref_sdrypri =   1.86017389E+03
        double precision :: sdrysec
        double precision, parameter :: ref_sdrysec =   5.96986279E+01
        double precision :: snatsec
        double precision, parameter :: ref_snatsec =   2.59382166E+02
        double precision :: somvnsec
        double precision, parameter :: ref_somvnsec =   2.59382166E+02
        double precision :: telvnsec
        double precision, parameter :: ref_telvnsec =   2.70799712E+01
        double precision :: vvchem
        double precision, parameter :: ref_vvchem =   4.21247319E+03
        double precision :: vtel
        double precision, parameter :: ref_vtel =   1.54339817E+03
        double precision :: snatpri
        double precision, parameter :: ref_snatpri =   4.01397443E+02
        double precision :: somvnpri
        double precision, parameter :: ref_somvnpri =   4.01397443E+02
        double precision :: telvnpri
        double precision, parameter :: ref_telvnpri =   1.54339817E+03
        double precision :: ddepri
        double precision, parameter :: ref_ddepri =   2.09909943E-10
        double precision :: wdepri
        double precision, parameter :: ref_wdepri =   3.29257688E-17
        double precision :: drydep
        double precision, parameter :: ref_drydep =   2.10075736E-10
        double precision :: wetdep
        double precision, parameter :: ref_wetdep =   4.62063155E-14
        real :: parout_val(4)
        real, parameter :: ref_parout_val(4) = (/   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00/)
        real :: qsec
        real, parameter :: ref_qsec =   1.09519249E-04
        real :: consec
        real, parameter :: ref_consec =   2.90055026E-27
        real :: pr
        real, parameter :: ref_pr =   0.00000000E+00
        real :: vd_eff_trj_zra
        real, parameter :: ref_vd_eff_trj_zra =   7.49391271E-04
        character(len=100), parameter :: in_parout_name(4) = (/ &
        "                                                                                                    ", &
        "                                                                                                    ", &
        "                                                                                                    ", &
        "                                                                                                    " /)
        character(len=40), parameter :: in_parout_unit(4) = (/ &
        "                                        ", &
        "                                        ", &
        "                                        ", &
        "                                        " /)
        character(len=128) :: parout_name(4)
        character(len=128), parameter :: ref_parout_name(4) = (/ &
        "                                                                                                                                ", &
        "                                                                                                                                ", &
        "                                                                                                                                ", &
        "                                                                                                                                " /)
        character(len=40) :: parout_unit(4)
        character(len=40), parameter :: ref_parout_unit(4) = (/ &
        "                                        ", &
        "                                        ", &
        "                                        ", &
        "                                        " /)
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        cdn = in_cdn
        cq2 = in_cq2
        sdrypri = in_sdrypri
        sdrysec = in_sdrysec
        snatsec = in_snatsec
        somvnsec = in_somvnsec
        telvnsec = in_telvnsec
        vvchem = in_vvchem
        vtel = in_vtel
        snatpri = in_snatpri
        somvnpri = in_somvnpri
        telvnpri = in_telvnpri
        ddepri = in_ddepri
        wdepri = in_wdepri
        drydep = in_drydep
        wetdep = in_wetdep
        parout_val = in_parout_val
        parout_name = in_parout_name
        parout_unit = in_parout_unit
        vd_eff_trj_zra = ref_vd_eff_trj_zra 
        pr = ref_pr
        qsec = ref_qsec
        consec = ref_consec
        
        call ops_conc_rek(varin_unc, do_proc, ueff, qbpri, isec, rc_sec_trj, routsec, c0_undepl_total, c0_undepl_mix, c_zrcp_undepl_mix, &
                     &  amol1, amol2, sigz, utr, rc_sec_rcp, ra_rcp_4, ra_rcp_zra, &
                     &  rb_rcp, amol21, ugmoldep, cch, cgt, cgt_z, grof, percvk, onder, regenk, virty, ri, vw10, hbron, pcoef, &
                     &  rkc, disxx, vnatpri, vchem, radius, xl, xloc, htot, twt, xvghbr, xvglbr, grad, frac, &
                     &  cdn, cq2, c_zrcp, sdrypri, sdrysec, snatsec, somvnsec, telvnsec, vvchem, vtel, snatpri, somvnpri, &
                     &  telvnpri, ddepri, wdepri, drydep, wetdep, qsec, consec, pr, vd_eff_trj_zra, ra_trj_zra, rb_trj, rc_eff_rcp_4, vd_coarse_part, &
                     &  buildingFact, nparout, parout_val, parout_name, parout_unit, parout_write)
        call assertEqual( ref_cdn, cdn, tol, "cdn", __LINE__, __FILE__)
        call assertEqual( ref_cq2, cq2, tol, "cq2", __LINE__, __FILE__)
        call assertEqual( ref_c, c_zrcp, tol, "c_zrcp", __LINE__, __FILE__)
        call assertEqual( ref_sdrypri, sdrypri, tol, "sdrypri", __LINE__, __FILE__)
        call assertEqual( ref_sdrysec, sdrysec, tol, "sdrysec", __LINE__, __FILE__)
        call assertEqual( ref_snatsec, snatsec, tol, "snatsec", __LINE__, __FILE__)
        call assertEqual( ref_somvnsec, somvnsec, tol, "somvnsec", __LINE__, __FILE__)
        call assertEqual( ref_telvnsec, telvnsec, tol, "telvnsec", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_vvchem, vvchem, tol, "vvchem", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_vtel, vtel, tol, "vtel", __LINE__, __FILE__)
        call assertEqual( ref_snatpri, snatpri, tol, "snatpri", __LINE__, __FILE__)
        call assertEqual( ref_somvnpri, somvnpri, tol, "somvnpri", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_telvnpri, telvnpri, tol, "telvnpri", __LINE__, __FILE__)
        call assertEqual( ref_ddepri, ddepri, tol, "ddepri", __LINE__, __FILE__)
        call assertEqual( ref_wdepri, wdepri, tol, "wdepri", __LINE__, __FILE__)
        call assertEqual( ref_drydep, drydep, tol, "drydep", __LINE__, __FILE__)
        call assertEqual( ref_wetdep, wetdep, tol, "wetdep", __LINE__, __FILE__)
        call assertEqual( ref_parout_val, parout_val, tol, "parout_val", __LINE__, __FILE__)
        call assertEqual( ref_qsec, qsec, tol, "qsec", __LINE__, __FILE__)
        call assertEqual( ref_consec, consec, tol, "consec", __LINE__, __FILE__)
        call assertEqual( ref_pr, pr, tol, "pr", __LINE__, __FILE__)
        call assertEqual( ref_vd_eff_trj_zra, vd_eff_trj_zra, tol, "vd_eff_trj_zra", __LINE__, __FILE__)
        call assertEqual( ref_parout_name, parout_name, "parout_name", __LINE__, __FILE__)
        call assertEqual( ref_parout_unit, parout_unit, "parout_unit", __LINE__, __FILE__)

        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        cdn = in_cdn
        cq2 = in_cq2
        sdrypri = in_sdrypri
        sdrysec = in_sdrysec
        snatsec = in_snatsec
        somvnsec = in_somvnsec
        telvnsec = in_telvnsec
        vvchem = in_vvchem
        vtel = in_vtel
        snatpri = in_snatpri
        somvnpri = in_somvnpri
        telvnpri = in_telvnpri
        ddepri = in_ddepri
        wdepri = in_wdepri
        drydep = in_drydep
        wetdep = in_wetdep
        parout_val = in_parout_val
        parout_name = in_parout_name
        parout_unit = in_parout_unit
        vd_eff_trj_zra = ref_vd_eff_trj_zra 
        pr = ref_pr
        qsec = ref_qsec
        consec = ref_consec
        call ops_conc_rek(varin_unc, do_proc, ueff, qbpri, isec, rc_sec_trj, routsec, c0_undepl_total, c0_undepl_mix, c_zrcp_undepl_mix, & 
                     &  amol1, amol2, sigz, utr, rc_sec_rcp, ra_rcp_4, ra_rcp_zra, &
                     &  rb_rcp, amol21, ugmoldep, cch, cgt, cgt_z, grof1, percvk, onder, regenk, virty, ri, vw10, hbron, pcoef, &
                     &  rkc, disxx, vnatpri, vchem, radius, xl, xloc, htot, twt, xvghbr, xvglbr, grad, frac, &
                     &  cdn, cq2, c_zrcp, sdrypri, sdrysec, snatsec, somvnsec, telvnsec, vvchem, vtel, snatpri, somvnpri, &
                     &  telvnpri, ddepri, wdepri, drydep, wetdep, qsec, consec, pr, vd_eff_trj_zra, ra_trj_zra, rb_trj, rc_eff_rcp_4, vd_coarse_part, &
                     &  buildingFact, nparout, parout_val, parout_name, parout_unit, parout_write)
        call assertEqual( ref_cdn, cdn, tol, "cdn", __LINE__, __FILE__)
        call assertEqual( ref_cq2, cq2, tol, "cq2", __LINE__, __FILE__)
        call assertEqual( ref_c, c_zrcp, tol, "c_zrcp", __LINE__, __FILE__)
        call assertEqual( ref_sdrypri, sdrypri, tol, "sdrypri", __LINE__, __FILE__)
        call assertEqual( ref_sdrysec, sdrysec, tol, "sdrysec", __LINE__, __FILE__)
        call assertEqual( ref_snatsec, snatsec, tol, "snatsec", __LINE__, __FILE__)
        call assertEqual( ref_somvnsec, somvnsec, tol, "somvnsec", __LINE__, __FILE__)
        call assertEqual( ref_telvnsec, telvnsec, tol, "telvnsec", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_vvchem, vvchem, tol, "vvchem", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_vtel, vtel, tol, "vtel", __LINE__, __FILE__)
        call assertEqual( ref_snatpri, snatpri, tol, "snatpri", __LINE__, __FILE__)
        call assertEqual( ref_somvnpri, somvnpri, tol, "somvnpri", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_telvnpri, telvnpri, tol, "telvnpri", __LINE__, __FILE__)
        call assertEqual( ref_ddepri, ddepri, tol, "ddepri", __LINE__, __FILE__)
        call assertEqual( ref_wdepri, wdepri, tol, "wdepri", __LINE__, __FILE__)
        call assertEqual( ref_drydep, drydep, tol, "drydep", __LINE__, __FILE__)
        call assertEqual( ref_wetdep, wetdep, tol, "wetdep", __LINE__, __FILE__)
        call assertEqual( ref_parout_val, parout_val, tol, "parout_val", __LINE__, __FILE__)
        call assertEqual( ref_qsec, qsec, tol, "qsec", __LINE__, __FILE__)
        call assertEqual( ref_consec, consec, tol, "consec", __LINE__, __FILE__)
        call assertEqual( ref_pr, pr, tol, "pr", __LINE__, __FILE__)
        call assertEqual( ref_vd_eff_trj_zra, vd_eff_trj_zra, tol, "vd_eff_trj_zra", __LINE__, __FILE__)
        call assertEqual( ref_parout_name, parout_name, "parout_name", __LINE__, __FILE__)
        call assertEqual( ref_parout_unit, parout_unit, "parout_unit", __LINE__, __FILE__)
   end subroutine test_ops_conc_rek4

   subroutine test_ops_conc_rek3()
   use m_ops_conc_rek
   use no_pfunit_ops_lt
   use m_ops_varin, only: Tvarin_unc
   use m_ops_tdo_proc, only: tdo_proc
       real, parameter :: tol = 1e-5
        type(Tvarin_unc) :: varin_unc
        type(tdo_proc) :: do_proc
        real, parameter :: ueff =   4.44325399E+00
        real, parameter :: qbpri =   1.00000000E+00
        logical, parameter :: isec = .true.
        real, parameter :: rc_sec_trj =   1.66130997E+02
        real, parameter :: routsec =   1.40000000E+07
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: c0_undepl_total   =   6.86089471E-02
        real, parameter :: c0_undepl_mix     =   onder*c0_undepl_total
        real, parameter :: c_zrcp_undepl_mix =   c0_undepl_mix 
        real, parameter :: amol1 =   4.60000000E+01
        real, parameter :: amol2 =   6.20000000E+01
        real, parameter :: sigz =   7.55815918E+02
        real, parameter :: utr =   4.13313913E+00
        real, parameter :: rc_sec_rcp =   1.37478745E+02
        real, parameter :: ra_rcp_4 =   2.43975487E+01
        real, parameter :: ra_rcp_zra =   3.31806717E+01
        real, parameter :: rb_rcp =   3.01502495E+01
        real, parameter :: amol21 =   1.34782612E+00
        real, parameter :: ugmoldep =   2.78000012E-10
        real, parameter :: cch =   9.55670416E-01
        real, parameter :: cgt =   1.11482814E-02
        real, parameter :: cgt_z =   1.11482814E-02
        real, parameter :: grof =   0.00000000E+00
        real, parameter :: percvk =   6.67061238E-03
        real, parameter :: regenk =   1.70000009E-02
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: ri =   1.77999997E+00
        real, parameter :: vw10 =   2.01143265E+00
        real, parameter :: hbron =   5.00000000E+00
        real, parameter :: pcoef =   2.94000000E-01
        real, parameter :: rkc =   1.00000000E+00
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: vnatpri =   1.08947068E-01
        real, parameter :: vchem =   3.08206844E+00
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: xl =   2.96321716E+02
        real, parameter :: xloc =   2.08999985E+02
        real, parameter :: htot =   5.00000000E+00
        real, parameter :: twt =   4.50051814E-01
        real, parameter :: xvghbr =   1.02960062E+00
        real, parameter :: xvglbr =   1.07823014E+00
        real, parameter :: grad =   9.87857282E-01
        real, parameter :: frac =   1.00000000E+00
        real, parameter :: ra_trj_zra =   3.47806282E+01
        real, parameter :: rb_trj =   2.85675850E+01
        real, parameter :: rc_eff_rcp_4 =   6.59993835E+02
        real, parameter :: vd_coarse_part =   1.26118263E-40
        real, parameter :: buildingFact =   1.00000000E+00
        integer, parameter :: nparout = 4
        logical, parameter :: parout_write = .false.
        real, parameter :: in_cdn =   9.81233537E-01
        real, parameter :: in_cq2 =   9.81869221E-01
        double precision, parameter :: in_sdrypri =   0.00000000E+00
        double precision, parameter :: in_sdrysec =   0.00000000E+00
        double precision, parameter :: in_snatsec =   0.00000000E+00
        double precision, parameter :: in_somvnsec =   0.00000000E+00
        double precision, parameter :: in_telvnsec =   0.00000000E+00
        double precision, parameter :: in_vvchem =   0.00000000E+00
        double precision, parameter :: in_vtel =   0.00000000E+00
        double precision, parameter :: in_snatpri =   0.00000000E+00
        double precision, parameter :: in_somvnpri =   0.00000000E+00
        double precision, parameter :: in_telvnpri =   0.00000000E+00
        double precision, parameter :: in_ddepri =   0.00000000E+00
        double precision, parameter :: in_wdepri =   0.00000000E+00
        double precision, parameter :: in_drydep =   0.00000000E+00
        double precision, parameter :: in_wetdep =   0.00000000E+00
        real, parameter :: in_parout_val(4) = (/   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00/)
        real :: cdn
        real, parameter :: ref_cdn =   9.81233537E-01
        real :: cq2
        real, parameter :: ref_cq2 =   9.81869221E-01
        real :: c_zrcp
        real, parameter :: ref_c =   6.24663495E-02
        double precision :: sdrypri
        double precision, parameter :: ref_sdrypri =   2.09935941E-03
        double precision :: sdrysec
        double precision, parameter :: ref_sdrysec =   4.32666857E-04
        double precision :: snatsec
        double precision, parameter :: ref_snatsec =   2.55220919E-04
        double precision :: somvnsec
        double precision, parameter :: ref_somvnsec =   2.55220919E-04
        double precision :: telvnsec
        double precision, parameter :: ref_telvnsec =   7.14996859E-05
        double precision :: vvchem
        double precision, parameter :: ref_vvchem =   3.84761207E-03
        double precision :: vtel
        double precision, parameter :: ref_vtel =   1.24838634E-03
        double precision :: snatpri
        double precision, parameter :: ref_snatpri =   1.36008020E-04
        double precision :: somvnpri
        double precision, parameter :: ref_somvnpri =   1.36008020E-04
        double precision :: telvnpri
        double precision, parameter :: ref_telvnpri =   1.24838634E-03
        double precision :: ddepri
        double precision, parameter :: ref_ddepri =   7.86620931E-13
        double precision :: wdepri
        double precision, parameter :: ref_wdepri =   5.09616187E-14
        double precision :: drydep
        double precision, parameter :: ref_drydep =   9.06902320E-13
        double precision :: wetdep
        double precision, parameter :: ref_wetdep =   1.21913032E-13
        real :: parout_val(4)
        real, parameter :: ref_parout_val(4) = (/   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00/)
        real :: qsec
        real, parameter :: ref_qsec =   5.27338274E-02
        real :: consec
        real, parameter :: ref_consec =   3.47272586E-03
        real :: pr
        real, parameter :: ref_pr =   9.26025271E-01
        real :: vd_eff_trj_zra
        real, parameter :: ref_vd_eff_trj_zra =   1.49062264E-03
        character(len=100), parameter :: in_parout_name(4) = (/ &
        "                                                                                                    ", &
        "                                                                                                    ", &
        "                                                                                                    ", &
        "                                                                                                    " /)
        character(len=40), parameter :: in_parout_unit(4) = (/ &
        "                                        ", &
        "                                        ", &
        "                                        ", &
        "                                        " /)
        character(len=128) :: parout_name(4)
        character(len=128), parameter :: ref_parout_name(4) = (/ &
        "                                                                                                                                ", &
        "                                                                                                                                ", &
        "                                                                                                                                ", &
        "                                                                                                                                " /)
        character(len=40) :: parout_unit(4)
        character(len=40), parameter :: ref_parout_unit(4) = (/ &
        "                                        ", &
        "                                        ", &
        "                                        ", &
        "                                        " /)
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        cdn = in_cdn
        cq2 = in_cq2
        sdrypri = in_sdrypri
        sdrysec = in_sdrysec
        snatsec = in_snatsec
        somvnsec = in_somvnsec
        telvnsec = in_telvnsec
        vvchem = in_vvchem
        vtel = in_vtel
        snatpri = in_snatpri
        somvnpri = in_somvnpri
        telvnpri = in_telvnpri
        ddepri = in_ddepri
        wdepri = in_wdepri
        drydep = in_drydep
        wetdep = in_wetdep
        parout_val = in_parout_val
        parout_name = in_parout_name
        parout_unit = in_parout_unit
        vd_eff_trj_zra = ref_vd_eff_trj_zra 
        pr = ref_pr
        qsec = ref_qsec
        consec = ref_consec
        call ops_conc_rek(varin_unc, do_proc, ueff, qbpri, isec, rc_sec_trj, routsec, c0_undepl_total, c0_undepl_mix, c_zrcp_undepl_mix, &
                     &  amol1, amol2, sigz, utr, rc_sec_rcp, ra_rcp_4, ra_rcp_zra, &
                     &  rb_rcp, amol21, ugmoldep, cch, cgt, cgt_z, grof, percvk, onder, regenk, virty, ri, vw10, hbron, pcoef, &
                     &  rkc, disxx, vnatpri, vchem, radius, xl, xloc, htot, twt, xvghbr, xvglbr, grad, frac, &
                     &  cdn, cq2, c_zrcp, sdrypri, sdrysec, snatsec, somvnsec, telvnsec, vvchem, vtel, snatpri, somvnpri, &
                     &  telvnpri, ddepri, wdepri, drydep, wetdep, qsec, consec, pr, vd_eff_trj_zra, ra_trj_zra, rb_trj, rc_eff_rcp_4, vd_coarse_part, &
                     &  buildingFact, nparout, parout_val, parout_name, parout_unit, parout_write)
        call assertEqual( ref_cdn, cdn, tol, "cdn", __LINE__, __FILE__)
        call assertEqual( ref_cq2, cq2, tol, "cq2", __LINE__, __FILE__)
        call assertEqual( ref_c, c_zrcp, tol, "c_zrcp", __LINE__, __FILE__)
        call assertEqual( ref_sdrypri, sdrypri, tol, "sdrypri", __LINE__, __FILE__)
        call assertEqual( ref_sdrysec, sdrysec, tol, "sdrysec", __LINE__, __FILE__)
        call assertEqual( ref_snatsec, snatsec, tol, "snatsec", __LINE__, __FILE__)
        call assertEqual( ref_somvnsec, somvnsec, tol, "somvnsec", __LINE__, __FILE__)
        call assertEqual( ref_telvnsec, telvnsec, tol, "telvnsec", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_vvchem, vvchem, tol, "vvchem", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_vtel, vtel, tol, "vtel", __LINE__, __FILE__)
        call assertEqual( ref_snatpri, snatpri, tol, "snatpri", __LINE__, __FILE__)
        call assertEqual( ref_somvnpri, somvnpri, tol, "somvnpri", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_telvnpri, telvnpri, tol, "telvnpri", __LINE__, __FILE__)
        call assertEqual( ref_ddepri, ddepri, tol, "ddepri", __LINE__, __FILE__)
        call assertEqual( ref_wdepri, wdepri, tol, "wdepri", __LINE__, __FILE__)
        call assertEqual( ref_drydep, drydep, tol, "drydep", __LINE__, __FILE__)
        call assertEqual( ref_wetdep, wetdep, tol, "wetdep", __LINE__, __FILE__)
        call assertEqual( ref_parout_val, parout_val, tol, "parout_val", __LINE__, __FILE__)
        call assertEqual( ref_qsec, qsec, tol, "qsec", __LINE__, __FILE__)
        call assertEqual( ref_consec, consec, tol, "consec", __LINE__, __FILE__)
        call assertEqual( ref_pr, pr, tol, "pr", __LINE__, __FILE__)
        call assertEqual( ref_vd_eff_trj_zra, vd_eff_trj_zra, tol, "vd_eff_trj_zra", __LINE__, __FILE__)
        call assertEqual( ref_parout_name, parout_name, "parout_name", __LINE__, __FILE__)
        call assertEqual( ref_parout_unit, parout_unit, "parout_unit", __LINE__, __FILE__)
   end subroutine test_ops_conc_rek3

   subroutine test_ops_conc_rek2()
   use m_ops_conc_rek
   use no_pfunit_ops_lt
   use m_ops_varin, only: Tvarin_unc
   use m_ops_tdo_proc, only: tdo_proc
       real, parameter :: tol = 1e-5
        type(Tvarin_unc) :: varin_unc
        type(tdo_proc) :: do_proc
        real, parameter :: ueff =   8.04345226E+00
        real, parameter :: qbpri =   1.00000000E+00
        logical, parameter :: isec = .false.
        real, parameter :: rc_sec_trj =  -4.01769275E-35
        real, parameter :: routsec =   0.00000000E+00
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: c0_undepl_total   =   6.04695715E-02
        real, parameter :: c0_undepl_mix     =   onder*c0_undepl_total
        real, parameter :: c_zrcp_undepl_mix =   c0_undepl_mix 
        real, parameter :: amol1 =   2.00000000E+01
        real, parameter :: amol2 =   2.00000000E+01
        real, parameter :: sigz =   1.80795395E+02
        real, parameter :: utr =   5.99296141E+00
        real, parameter :: rc_sec_rcp =   2.60407935E-24
        real, parameter :: ra_rcp_4 =   1.75061207E+01
        real, parameter :: ra_rcp_zra =   3.69904633E+01
        real, parameter :: rb_rcp =   1.64628677E+01
        real, parameter :: amol21 =   1.00000000E+00
        real, parameter :: ugmoldep =   2.78000012E-10
        real, parameter :: cch =   1.00000000E+00
        real, parameter :: cgt =   2.93203354E-01
        real, parameter :: cgt_z =   2.93203354E-01
        real, parameter :: grof =   0.00000000E+00
        real, parameter :: percvk =   9.77511168E-04
        real, parameter :: regenk =   0.00000000E+00
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: ri =   1.35000002E+00
        real, parameter :: vw10 =   4.77636433E+00
        real, parameter :: hbron =   5.00000000E+00
        real, parameter :: pcoef =   2.89999992E-01
        real, parameter :: rkc =   1.00000000E+00
        real, parameter :: disxx =   1.73223848E+04
        real, parameter :: vnatpri =   0.00000000E+00
        real, parameter :: vchem =   0.00000000E+00
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: xl =   4.81590851E+02
        real, parameter :: xloc =   4.65999969E+02
        real, parameter :: htot =   5.00000000E+00
        real, parameter :: twt =   2.83734411E-01
        real, parameter :: xvghbr =   9.91338372E-01
        real, parameter :: xvglbr =   1.21307540E+00
        real, parameter :: grad =   7.06796587E-01
        real, parameter :: frac =   1.00000000E+00
        real, parameter :: ra_trj_zra =   2.81038532E+01
        real, parameter :: rb_trj =   1.50981655E+01
        real, parameter :: rc_eff_rcp_4 =   1.30000000E+01
        real, parameter :: vd_coarse_part =   4.59121429E-41
        real, parameter :: buildingFact =   1.00000000E+00
        integer, parameter :: nparout = 4
        logical, parameter :: parout_write = .false.
        real, parameter :: in_cdn =   1.00000000E+00
        real, parameter :: in_cq2 =   5.38133085E-01
        double precision, parameter :: in_sdrypri =   4.49448635E+03
        double precision, parameter :: in_sdrysec =   0.00000000E+00
        double precision, parameter :: in_snatsec =   0.00000000E+00
        double precision, parameter :: in_somvnsec =   0.00000000E+00
        double precision, parameter :: in_telvnsec =   0.00000000E+00
        double precision, parameter :: in_vvchem =   0.00000000E+00
        double precision, parameter :: in_vtel =   2.84157433E+02
        double precision, parameter :: in_snatpri =   1.05654933E+03
        double precision, parameter :: in_somvnpri =   1.05654933E+03
        double precision, parameter :: in_telvnpri =   2.84157433E+02
        double precision, parameter :: in_ddepri =   1.04819664E-11
        double precision, parameter :: in_wdepri =   1.20268029E-12
        double precision, parameter :: in_drydep =   1.04819664E-11
        double precision, parameter :: in_wetdep =   1.20268029E-12
        real, parameter :: in_parout_val(4) = (/   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00/)
        real :: cdn
        real, parameter :: ref_cdn =   1.00000000E+00
        real :: cq2
        real, parameter :: ref_cq2 =   5.38133085E-01
        real :: c_zrcp
        real, parameter :: ref_c =   2.29996406E-02
        double precision :: sdrypri
        double precision, parameter :: ref_sdrypri =   4.49448808E+03
        double precision :: sdrysec
        double precision, parameter :: ref_sdrysec =   0.00000000E+00
        double precision :: snatsec
        double precision, parameter :: ref_snatsec =   0.00000000E+00
        double precision :: somvnsec
        double precision, parameter :: ref_somvnsec =   0.00000000E+00
        double precision :: telvnsec
        double precision, parameter :: ref_telvnsec =   0.00000000E+00
        double precision :: vvchem
        double precision, parameter :: ref_vvchem =   0.00000000E+00
        double precision :: vtel
        double precision, parameter :: ref_vtel =   2.84157505E+02
        double precision :: snatpri
        double precision, parameter :: ref_snatpri =   1.05654933E+03
        double precision :: somvnpri
        double precision, parameter :: ref_somvnpri =   1.05654933E+03
        double precision :: telvnpri
        double precision, parameter :: ref_telvnpri =   2.84157505E+02
        double precision :: ddepri
        double precision, parameter :: ref_ddepri =   1.09610143E-11
        double precision :: wdepri
        double precision, parameter :: ref_wdepri =   1.20268029E-12
        double precision :: drydep
        double precision, parameter :: ref_drydep =   1.09610143E-11
        double precision :: wetdep
        double precision, parameter :: ref_wetdep =   1.20268029E-12
        real :: parout_val(4)
        real, parameter :: ref_parout_val(4) = (/   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00/)
        real :: qsec
        real, parameter :: ref_qsec =   1.43492963E-42
        real :: consec
        real, parameter :: ref_consec =   7.00649232E-45
        real :: pr
        real, parameter :: ref_pr =   6.66849464E-02
        real :: vd_eff_trj_zra
        real, parameter :: ref_vd_eff_trj_zra =   2.15841960E-02
        character(len=100), parameter :: in_parout_name(4) = (/ &
        "                                                                                                    ", &
        "                                                                                                    ", &
        "                                                                                                    ", &
        "                                                                                                    " /)
        character(len=40), parameter :: in_parout_unit(4) = (/ &
        "                                        ", &
        "                                        ", &
        "                                        ", &
        "                                        " /)
        character(len=128) :: parout_name(4)
        character(len=128), parameter :: ref_parout_name(4) = (/ &
        "                                                                                                                                ", &
        "                                                                                                                                ", &
        "                                                                                                                                ", &
        "                                                                                                                                " /)
        character(len=40) :: parout_unit(4)
        character(len=40), parameter :: ref_parout_unit(4) = (/ &
        "                                        ", &
        "                                        ", &
        "                                        ", &
        "                                        " /)
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        cdn = in_cdn
        cq2 = in_cq2
        sdrypri = in_sdrypri
        sdrysec = in_sdrysec
        snatsec = in_snatsec
        somvnsec = in_somvnsec
        telvnsec = in_telvnsec
        vvchem = in_vvchem
        vtel = in_vtel
        snatpri = in_snatpri
        somvnpri = in_somvnpri
        telvnpri = in_telvnpri
        ddepri = in_ddepri
        wdepri = in_wdepri
        drydep = in_drydep
        wetdep = in_wetdep
        parout_val = in_parout_val
        parout_name = in_parout_name
        parout_unit = in_parout_unit
        vd_eff_trj_zra = ref_vd_eff_trj_zra 
        qsec = ref_qsec
        consec = ref_consec
        pr = ref_pr
        call ops_conc_rek(varin_unc, do_proc, ueff, qbpri, isec, rc_sec_trj, routsec, c0_undepl_total, c0_undepl_mix, c_zrcp_undepl_mix, &
                     &  amol1, amol2, sigz, utr, rc_sec_rcp, ra_rcp_4, ra_rcp_zra, &
                     &  rb_rcp, amol21, ugmoldep, cch, cgt, cgt_z, grof, percvk, onder, regenk, virty, ri, vw10, hbron, pcoef, &
                     &  rkc, disxx, vnatpri, vchem, radius, xl, xloc, htot, twt, xvghbr, xvglbr, grad, frac, &
                     &  cdn, cq2, c_zrcp, sdrypri, sdrysec, snatsec, somvnsec, telvnsec, vvchem, vtel, snatpri, somvnpri, &
                     &  telvnpri, ddepri, wdepri, drydep, wetdep, qsec, consec, pr, vd_eff_trj_zra, ra_trj_zra, rb_trj, rc_eff_rcp_4, vd_coarse_part, &
                     &  buildingFact, nparout, parout_val, parout_name, parout_unit, parout_write)
        call assertEqual( ref_cdn, cdn, tol, "cdn", __LINE__, __FILE__)
        call assertEqual( ref_cq2, cq2, tol, "cq2", __LINE__, __FILE__)
        call assertEqual( ref_c, c_zrcp, tol, "c_zrcp", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_sdrypri, sdrypri, tol, "sdrypri", __LINE__, __FILE__)
        call assertEqual( ref_sdrysec, sdrysec, tol, "sdrysec", __LINE__, __FILE__)
        call assertEqual( ref_snatsec, snatsec, tol, "snatsec", __LINE__, __FILE__)
        call assertEqual( ref_somvnsec, somvnsec, tol, "somvnsec", __LINE__, __FILE__)
        call assertEqual( ref_telvnsec, telvnsec, tol, "telvnsec", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_vvchem, vvchem, tol, "vvchem", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_vtel, vtel, tol, "vtel", __LINE__, __FILE__)
        call assertEqual( ref_snatpri, snatpri, tol, "snatpri", __LINE__, __FILE__)
        call assertEqual( ref_somvnpri, somvnpri, tol, "somvnpri", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_telvnpri, telvnpri, tol, "telvnpri", __LINE__, __FILE__)
        call assertEqual( ref_ddepri, ddepri, tol, "ddepri", __LINE__, __FILE__)
        call assertEqual( ref_wdepri, wdepri, tol, "wdepri", __LINE__, __FILE__)
        call assertEqual( ref_drydep, drydep, tol, "drydep", __LINE__, __FILE__)
        call assertEqual( ref_wetdep, wetdep, tol, "wetdep", __LINE__, __FILE__)
        call assertEqual( ref_parout_val, parout_val, tol, "parout_val", __LINE__, __FILE__)
        call assertEqual( ref_qsec, qsec, tol, "qsec", __LINE__, __FILE__)
        call assertEqual( ref_consec, consec, tol, "consec", __LINE__, __FILE__)
        call assertEqual( ref_pr, pr, tol, "pr", __LINE__, __FILE__)
        call assertEqual( ref_vd_eff_trj_zra, vd_eff_trj_zra, tol, "vd_eff_trj_zra", __LINE__, __FILE__)
        call assertEqual( ref_parout_name, parout_name, "parout_name", __LINE__, __FILE__)
        call assertEqual( ref_parout_unit, parout_unit, "parout_unit", __LINE__, __FILE__)
   end subroutine test_ops_conc_rek2

   subroutine test_ops_conc_rek()
   use m_ops_conc_rek
   use no_pfunit_ops_lt
   use m_ops_varin, only: Tvarin_unc
   use m_ops_tdo_proc, only: tdo_proc
       real, parameter :: tol = 1e-5
        type(Tvarin_unc) :: varin_unc
        type(tdo_proc) :: do_proc
        real, parameter :: ueff =   4.44325399E+00
        real, parameter :: qbpri =   1.00000000E+00
        logical, parameter :: isec = .false.
        real, parameter :: rc_sec_trj =   7.78406443E-39
        real, parameter :: routsec =   0.00000000E+00
        real, parameter :: onder =   1.00000000E+00
        real, parameter :: c0_undepl_total   =   6.86089471E-02
        real, parameter :: c0_undepl_mix     =   onder*c0_undepl_total
        real, parameter :: c_zrcp_undepl_mix =   c0_undepl_mix 
        real, parameter :: amol1 =   2.00000000E+01
        real, parameter :: amol2 =   2.00000000E+01
        real, parameter :: sigz =   7.55815918E+02
        real, parameter :: utr =   4.13313913E+00
        real, parameter :: rc_sec_rcp =   0.00000000E+00
        real, parameter :: ra_rcp_4 =   2.43975487E+01
        real, parameter :: ra_rcp_zra =   3.31806717E+01
        real, parameter :: rb_rcp =   3.01502495E+01
        real, parameter :: amol21 =   1.00000000E+00
        real, parameter :: ugmoldep =   2.78000012E-10
        real, parameter :: cch =   9.53811109E-01
        real, parameter :: cgt =   1.15066350E-01
        real, parameter :: cgt_z =   1.15066350E-01
        real, parameter :: grof =   0.00000000E+00
        real, parameter :: percvk =   6.67061238E-03
        real, parameter :: regenk =   1.70000009E-02
        real, parameter :: virty =   0.00000000E+00
        real, parameter :: ri =   1.77999997E+00
        real, parameter :: vw10 =   2.01143265E+00
        real, parameter :: hbron =   5.00000000E+00
        real, parameter :: pcoef =   2.94000000E-01
        real, parameter :: rkc =   1.00000000E+00
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: vnatpri =   3.32806897E+00
        real, parameter :: vchem =   0.00000000E+00
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: xl =   2.96321716E+02
        real, parameter :: xloc =   2.08999985E+02
        real, parameter :: htot =   5.00000000E+00
        real, parameter :: twt =   4.50051814E-01
        real, parameter :: xvghbr =   1.02960062E+00
        real, parameter :: xvglbr =   1.07823014E+00
        real, parameter :: grad =   8.84933650E-01
        real, parameter :: frac =   1.00000000E+00
        real, parameter :: ra_trj_zra =   3.47806282E+01
        real, parameter :: rb_trj =   2.85675850E+01
        real, parameter :: rc_eff_rcp_4 =   1.30000000E+01
        real, parameter :: vd_coarse_part =   1.26118263E-40
        real, parameter :: buildingFact =   1.00000000E+00
        integer, parameter :: nparout = 4
        logical, parameter :: parout_write = .false.
        real, parameter :: in_cdn =   8.35698187E-01
        real, parameter :: in_cq2 =   7.62434900E-01
        double precision, parameter :: in_sdrypri =   0.00000000E+00
        double precision, parameter :: in_sdrysec =   0.00000000E+00
        double precision, parameter :: in_snatsec =   0.00000000E+00
        double precision, parameter :: in_somvnsec =   0.00000000E+00
        double precision, parameter :: in_telvnsec =   0.00000000E+00
        double precision, parameter :: in_vvchem =   0.00000000E+00
        double precision, parameter :: in_vtel =   0.00000000E+00
        double precision, parameter :: in_snatpri =   0.00000000E+00
        double precision, parameter :: in_somvnpri =   0.00000000E+00
        double precision, parameter :: in_telvnpri =   0.00000000E+00
        double precision, parameter :: in_ddepri =   0.00000000E+00
        double precision, parameter :: in_wdepri =   0.00000000E+00
        double precision, parameter :: in_drydep =   0.00000000E+00
        double precision, parameter :: in_wetdep =   0.00000000E+00
        real, parameter :: in_parout_val(4) = (/   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00/)
        real :: cdn
        real, parameter :: ref_cdn =   8.35698187E-01
        real :: cq2
        real, parameter :: ref_cq2 =   7.62434900E-01
        real :: c_zrcp
        real, parameter :: ref_c =   3.68982740E-02
        double precision :: sdrypri
        double precision, parameter :: ref_sdrypri =   1.31178619E-02
        double precision :: sdrysec
        double precision, parameter :: ref_sdrysec =   0.00000000E+00
        double precision :: snatsec
        double precision, parameter :: ref_snatsec =   0.00000000E+00
        double precision :: somvnsec
        double precision, parameter :: ref_somvnsec =   0.00000000E+00
        double precision :: telvnsec
        double precision, parameter :: ref_telvnsec =   0.00000000E+00
        double precision :: vvchem
        double precision, parameter :: ref_vvchem =   0.00000000E+00
        double precision :: vtel
        double precision, parameter :: ref_vtel =   8.24004237E-04
        double precision :: snatpri
        double precision, parameter :: ref_snatpri =   2.74234288E-03
        double precision :: somvnpri
        double precision, parameter :: ref_somvnpri =   2.74234288E-03
        double precision :: telvnpri
        double precision, parameter :: ref_telvnpri =   8.24004237E-04
        double precision :: ddepri
        double precision, parameter :: ref_ddepri =   3.64676562E-12
        double precision :: wdepri
        double precision, parameter :: ref_wdepri =   7.62371340E-13
        double precision :: drydep
        double precision, parameter :: ref_drydep =   3.64676562E-12
        double precision :: wetdep
        double precision, parameter :: ref_wetdep =   7.62371340E-13
        real :: parout_val(4)
        real, parameter :: ref_parout_val(4) = (/   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00/)
        real :: qsec
        real, parameter :: ref_qsec =   1.43492963E-42
        real :: consec
        real, parameter :: ref_consec =   5.05561425E-30
        real :: pr
        real, parameter :: ref_pr =   9.26025271E-01
        real :: vd_eff_trj_zra
        real, parameter :: ref_vd_eff_trj_zra =   1.41225327E-02
        character(len=100), parameter :: in_parout_name(4) = (/ &
        "                                                                                                    ", &
        "                                                                                                    ", &
        "                                                                                                    ", &
        "                                                                                                    " /)
        character(len=40), parameter :: in_parout_unit(4) = (/ &
        "                                        ", &
        "                                        ", &
        "                                        ", &
        "                                        " /)
        character(len=128) :: parout_name(4)
        character(len=128), parameter :: ref_parout_name(4) = (/ &
        "                                                                                                                                ", &
        "                                                                                                                                ", &
        "                                                                                                                                ", &
        "                                                                                                                                " /)
        character(len=40) :: parout_unit(4)
        character(len=40), parameter :: ref_parout_unit(4) = (/ &
        "                                        ", &
        "                                        ", &
        "                                        ", &
        "                                        " /)
        do_proc%chem = .true.
        do_proc%depl_drydep = .true.
        do_proc%depl_wetdep = .true.
        do_proc%grad_drydep = .true.
        cdn = in_cdn
        cq2 = in_cq2
        sdrypri = in_sdrypri
        sdrysec = in_sdrysec
        snatsec = in_snatsec
        somvnsec = in_somvnsec
        telvnsec = in_telvnsec
        vvchem = in_vvchem
        vtel = in_vtel
        snatpri = in_snatpri
        somvnpri = in_somvnpri
        telvnpri = in_telvnpri
        ddepri = in_ddepri
        wdepri = in_wdepri
        drydep = in_drydep
        wetdep = in_wetdep
        parout_val = in_parout_val
        parout_name = in_parout_name
        parout_unit = in_parout_unit
        vd_eff_trj_zra = ref_vd_eff_trj_zra 
        qsec = ref_qsec
        consec = ref_consec
        pr = ref_pr
        call ops_conc_rek(varin_unc, do_proc, ueff, qbpri, isec, rc_sec_trj, routsec, c0_undepl_total, c0_undepl_mix, c_zrcp_undepl_mix, &
                     &  amol1, amol2, sigz, utr, rc_sec_rcp, ra_rcp_4, ra_rcp_zra, &
                     &  rb_rcp, amol21, ugmoldep, cch, cgt, cgt_z, grof, percvk, onder, regenk, virty, ri, vw10, hbron, pcoef, &
                     &  rkc, disxx, vnatpri, vchem, radius, xl, xloc, htot, twt, xvghbr, xvglbr, grad, frac, &
                     &  cdn, cq2, c_zrcp, sdrypri, sdrysec, snatsec, somvnsec, telvnsec, vvchem, vtel, snatpri, somvnpri, &
                     &  telvnpri, ddepri, wdepri, drydep, wetdep, qsec, consec, pr, vd_eff_trj_zra, ra_trj_zra, rb_trj, rc_eff_rcp_4, vd_coarse_part, &
                     &  buildingFact, nparout, parout_val, parout_name, parout_unit, parout_write)
        call assertEqual( ref_cdn, cdn, tol, "cdn", __LINE__, __FILE__)
        call assertEqual( ref_cq2, cq2, tol, "cq2", __LINE__, __FILE__)
        call assertEqual( ref_c, c_zrcp, tol, "c_zrcp", __LINE__, __FILE__)
        call assertEqual( ref_sdrypri, sdrypri, tol, "sdrypri", __LINE__, __FILE__)
        call assertEqual( ref_sdrysec, sdrysec, tol, "sdrysec", __LINE__, __FILE__)
        call assertEqual( ref_snatsec, snatsec, tol, "snatsec", __LINE__, __FILE__)
        call assertEqual( ref_somvnsec, somvnsec, tol, "somvnsec", __LINE__, __FILE__)
        call assertEqual( ref_telvnsec, telvnsec, tol, "telvnsec", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_vvchem, vvchem, tol, "vvchem", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_vtel, vtel, tol, "vtel", __LINE__, __FILE__)
        call assertEqual( ref_snatpri, snatpri, tol, "snatpri", __LINE__, __FILE__)
        call assertEqual( ref_somvnpri, somvnpri, tol, "somvnpri", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_telvnpri, telvnpri, tol, "telvnpri", __LINE__, __FILE__)
        call assertEqual( ref_ddepri, ddepri, tol, "ddepri", __LINE__, __FILE__)
        call assertEqual( ref_wdepri, wdepri, tol, "wdepri", __LINE__, __FILE__)
        call assertEqual( ref_drydep, drydep, tol, "drydep", __LINE__, __FILE__)
        call assertEqual( ref_wetdep, wetdep, tol, "wetdep", __LINE__, __FILE__)
        call assertEqual( ref_parout_val, parout_val, tol, "parout_val", __LINE__, __FILE__)
        call assertEqual( ref_qsec, qsec, tol, "qsec", __LINE__, __FILE__)
        call assertEqual( ref_consec, consec, tol, "consec", __LINE__, __FILE__)
        call assertEqual( ref_pr, pr, tol, "pr", __LINE__, __FILE__)
        call assertEqual( ref_vd_eff_trj_zra, vd_eff_trj_zra, tol, "vd_eff_trj_zra", __LINE__, __FILE__)
        call assertEqual( ref_parout_name, parout_name, "parout_name", __LINE__, __FILE__)
        call assertEqual( ref_parout_unit, parout_unit, "parout_unit", __LINE__, __FILE__)
   end subroutine test_ops_conc_rek
end module m_test_ops_conc_rek
 
program p_test_ops_conc_rek
use m_test_ops_conc_rek
use no_pfunit
implicit none
   call test_ops_conc_rek4()
   call test_ops_conc_rek3()
   call test_ops_conc_rek2()
   call test_ops_conc_rek()
   call conclusion()
end program p_test_ops_conc_rek
