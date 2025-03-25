module m_test_interp_sek
implicit none
contains
    subroutine test_interp_sek()
    use m_astat
    use m_ops_statparexp
    use m_commonfile
    use m_ops_varin
    use no_pfunit
        type(Tvarin_unc) :: varin_unc
        real, parameter :: tol = 1e-5
        integer, parameter :: istab = 1
        integer, parameter :: isec_in = 1
        integer, parameter :: itrx = 1
        integer, parameter :: isec2 = 2
        real, parameter :: s_wind =   0.00000000E+00
        integer, parameter :: isec1 = 1
        real, parameter :: stt(27) = (/   0.00000000E+00,   2.59347412E+02,   2.10111094E+00,   2.60090885E+01, &
             4.50201988E+01,   5.60333252E+01,   1.00027275E+00,   1.00009096E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   2.72066683E-01,   9.99999905E+00,   5.00605917E+00,  -4.11121063E+01, &
             3.41896935E+01,   8.20010071E+01,   2.95051483E+02,   3.39969673E+01,   4.49361542E+02/)
        real :: xl
        real, parameter :: ref_xl =   2.59347412E+02
        real :: vw10
        real, parameter :: ref_vw10 =   2.10111094E+00
        real :: rb_ms
        real, parameter :: ref_rb_ms =   2.60090885E+01
        real :: ra_ms_4
        real, parameter :: ref_ra_ms_4 =   1.90111103E+01
        real :: ra_ms_zra
        real, parameter :: ref_ra_ms_zra =   3.00242367E+01
        real :: xvglbr
        real, parameter :: ref_xvglbr =   1.00027275E+00
        real :: xvghbr
        real, parameter :: ref_xvghbr =   1.00009096E+00
        real :: uster_metreg_rcp
        real, parameter :: ref_uster_metreg_rcp =   2.72066683E-01
        real :: temp_C
        real, parameter :: ref_temp_C =   9.99999905E+00
        real :: ol_metreg_rcp
        real, parameter :: ref_ol_metreg_rcp =  -4.11121063E+01
        real :: h0
        real, parameter :: ref_h0 =   3.41896935E+01
        real :: xloc
        real, parameter :: ref_xloc =   259.347412109
        real :: xl100
        real, parameter :: ref_xl100 =   4375.0380859
        real :: rad
        real, parameter :: ref_rad =   149.766464233
        real :: rc_so2_ms
        real, parameter :: ref_rc_so2_ms =   92.1013717651
        real :: hum
        real, parameter :: ref_hum =   77.4114074707
        real :: pcoef
        real, parameter :: ref_pcoef =   0.409133136272
        real :: rc_nh3_ms
        real, parameter :: ref_rc_nh3_ms =   170.326873779
        real :: rc_no2_ms
        real, parameter :: ref_rc_no2_ms =   727.562683105
        real :: rc_aer_ms
        real, parameter :: ref_rc_aer_ms =   3540.77270508
        real :: buil
        real, parameter :: ref_buil =   1.66570317745
        real :: rint
        real, parameter :: ref_rint =   1.47962081432
        real :: shear
        real, parameter :: ref_shear =   5.00605917E+00
        real :: dscor(4)
        real, parameter :: ref_dscor(4) = (/   0.00000000E+00,   104.898338318,   204.418319702,   1408.58679199/)
        real :: coef_space_heating
        real, parameter :: ref_coef_space_heating =   4.25570964813
        real :: regenk
        real, parameter :: ref_regenk =   0.155417039990
        real, allocatable :: astat(:, :, :, :) ! statistical meteo parameters
        call example_astat(astat)

        call interp_sek( istab, isec_in, itrx, isec2, s_wind, isec1, stt, astat, varin_unc, xl, vw10, rb_ms, ra_ms_4, ra_ms_zra, &
            xvglbr, xvghbr, uster_metreg_rcp, temp_C, ol_metreg_rcp, h0, xloc, xl100, rad, rc_so2_ms, &
            hum, pcoef, rc_nh3_ms, rc_no2_ms, rc_aer_ms, buil, rint, shear, dscor, coef_space_heating, &
            regenk)

        call assertEqual( ref_xl, xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_vw10, vw10, tol, "vw10", __LINE__, __FILE__)
        call assertEqual( ref_rb_ms, rb_ms, tol, "rb_ms", __LINE__, __FILE__)
        call assertEqual( ref_ra_ms_4, ra_ms_4, tol, "ra_ms_4", __LINE__, __FILE__)
        call assertEqual( ref_ra_ms_zra, ra_ms_zra, tol, "ra_ms_zra", __LINE__, __FILE__)
        call assertEqual( ref_xvglbr, xvglbr, tol, "xvglbr", __LINE__, __FILE__)
        call assertEqual( ref_xvghbr, xvghbr, tol, "xvghbr", __LINE__, __FILE__)
        call assertEqual( ref_uster_metreg_rcp, uster_metreg_rcp, tol, "uster_metreg_rcp", __LINE__, __FILE__)
        call assertEqual( ref_temp_C, temp_C, tol, "temp_C", __LINE__, __FILE__)
        call assertEqual( ref_ol_metreg_rcp, ol_metreg_rcp, tol, "ol_metreg_rcp", __LINE__, __FILE__)
        call assertEqual( ref_h0, h0, tol, "h0", __LINE__, __FILE__)
        call assertEqual( ref_xloc, xloc, tol, "xloc", __LINE__, __FILE__)
        call assertEqual( ref_xl100, xl100, tol, "xl100", __LINE__, __FILE__)
        call assertEqual( ref_rad, rad, tol, "rad", __LINE__, __FILE__)
        call assertEqual( ref_rc_so2_ms, rc_so2_ms, tol, "rc_so2_ms", __LINE__, __FILE__)
        call assertEqual( ref_hum, hum, tol, "hum", __LINE__, __FILE__)
        call assertEqual( ref_pcoef, pcoef, tol, "pcoef", __LINE__, __FILE__)
        call assertEqual( ref_rc_nh3_ms, rc_nh3_ms, tol, "rc_nh3_ms", __LINE__, __FILE__)
        call assertEqual( ref_rc_no2_ms, rc_no2_ms, tol, "rc_no2_ms", __LINE__, __FILE__)
        call assertEqual( ref_rc_aer_ms, rc_aer_ms, tol, "rc_aer_ms", __LINE__, __FILE__)
        call assertEqual( ref_buil, buil, tol, "buil", __LINE__, __FILE__)
        call assertEqual( ref_rint, rint, tol, "rint", __LINE__, __FILE__)
        call assertEqual( ref_shear, shear, tol, "shear", __LINE__, __FILE__)
        call assertEqual( ref_dscor, dscor, tol, "dscor", __LINE__, __FILE__)
        call assertEqual( ref_coef_space_heating, coef_space_heating, tol, "coef_space_heating", __LINE__, __FILE__)
        call assertEqual( ref_regenk, regenk, tol, "regenk", __LINE__, __FILE__)
   end subroutine test_interp_sek
end module m_test_interp_sek

program tst_interp_sek
use no_pfunit
use m_test_interp_sek
implicit none
    call test_interp_sek()
    call conclusion()
end program tst_interp_sek
