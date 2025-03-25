module m_test_ops_statparexp
implicit none
contains
   subroutine test_ops_statparexp2()
   use m_commonfile
   use m_ops_statparexp
   use m_astat
   use m_commonconst_lib, only: NTRAJ, NCOMP, NSTAB, NSEK
   use no_pfunit
   use m_error
   use m_ops_varin, only: Tvarin_meteo, Tvarin_unc
   
        TYPE(Tvarin_meteo) :: varin_meteo ! input variables for meteo
        type(Tvarin_unc) :: varin_unc
        real, parameter :: tol = 1e-5
        integer, parameter :: istab = 1
        real, parameter :: hbron =   5.00000000E+00
        real, parameter :: qww =   0.00000000E+00
        real, parameter :: D_stack =  -9.99000000E+02
        real, parameter :: V_stack =  -9.99000000E+02
        real, parameter :: Ts_stack =  -9.99000000E+02
        logical, parameter :: emis_horizontal = .false.
        integer, parameter :: iwd = 135
        real, parameter :: radius =   1.00000000E+03
        real, parameter :: uurtot =   8.75600000E+03
        real, parameter :: trafst(4) = (/   0.00000000E+00,   1.00000000E+05,   3.00000000E+05,   1.00000000E+06/)
        real, parameter :: disx =   2.11424922E+04
        integer, parameter :: isec_prelim = 6
        real :: disxx
        real, parameter :: ref_disxx =   2.11424922E+04
        integer :: isec1
        integer, parameter :: ref_isec1 = 5
        real :: vw10
        real, parameter :: ref_vw10 =   2.01143265E+00
        real :: h0
        real, parameter :: ref_h0 =   4.34898224E+01
        real :: hum
        real, parameter :: ref_hum =   7.58666534E+01
        real :: ol_metreg_rcp
        real, parameter :: ref_ol_metreg_rcp =  -4.30853157E+01
        real :: shear
        real, parameter :: ref_shear =   1.41542683E+01
        real :: rc_aer_ms
        real, parameter :: ref_rc_aer_ms =   4.41672119E+02
        real :: rc_nh3_ms
        real, parameter :: ref_rc_nh3_ms =   3.34727898E+01
        real :: rc_no2_ms
        real, parameter :: ref_rc_no2_ms =   3.47779175E+02
        real :: temp_C
        real, parameter :: ref_temp_C =   1.28663206E+01
        real :: uster_metreg_rcp
        real, parameter :: ref_uster_metreg_rcp =   2.77255356E-01
        real :: pcoef
        real, parameter :: ref_pcoef =   2.94000000E-01
        real :: htot
        real, parameter :: ref_htot =   1.00000000E+01
        real :: htt
        real, parameter :: ref_htt =   5.00000000E+00
        integer :: itra
        integer, parameter :: ref_itra = 1
        real :: aant
        real, parameter :: ref_aant =   5.84078827E+01
        real :: xl
        real, parameter :: ref_xl =   2.79640625E+02
        real :: rb_ms
        real, parameter :: ref_rb_ms =   2.58219471E+01
        real :: ra_ms_4
        real, parameter :: ref_ra_ms_4 =   1.94376163E+01
        real :: ra_ms_zra
        real, parameter :: ref_ra_ms_zra =   3.22204666E+01
        real :: xvglbr
        real, parameter :: ref_xvglbr =   1.07823014E+00
        real :: xvghbr
        real, parameter :: ref_xvghbr =   1.02960062E+00
        real :: xloc
        real, parameter :: ref_xloc =   2.08999985E+02
        real :: xl100
        real, parameter :: ref_xl100 =   6.21999939E+02
        real :: rad
        real, parameter :: ref_rad =   9.11993179E+01
        real :: rc_so2_ms
        real, parameter :: ref_rc_so2_ms =   6.56554642E+01
        real :: coef_space_heating
        real, parameter :: ref_coef_space_heating =   5.10666656E+00
        real :: regenk
        real, parameter :: ref_regenk =   2.30019055E-02
        real :: buil
        real, parameter :: ref_buil =   2.27999997E+00
        real :: rint
        real, parameter :: ref_rint =   1.77999997E+00
        real :: percvk
        real, parameter :: ref_percvk =   7.88627565E-03
        integer :: isec_in
        integer, parameter :: ref_isec_in = 5
        type(TError) :: error
        real, allocatable :: astat(:,:,:,:)
        character(len=500) :: filename
        filename  = './level_1/resources/astat_after_readstexp.data'
        call read_astat(astat, filename)
        call ops_statparexp( varin_meteo, varin_unc, istab, hbron, qww, D_stack, V_stack, Ts_stack, emis_horizontal, iwd, radius, &
            uurtot, astat, trafst, disx, isec_prelim, disxx, isec1, vw10, h0, hum, ol_metreg_rcp, shear, rc_aer_ms, &
            rc_nh3_ms, rc_no2_ms, temp_C, uster_metreg_rcp, pcoef, htot, htt, itra, aant, xl, rb_ms, ra_ms_4, &
            ra_ms_zra, xvglbr, xvghbr, xloc, xl100, rad, rc_so2_ms, coef_space_heating, regenk, buil, &
            rint, percvk, isec_in, error)
        call assertEqual( ref_disxx, disxx, tol, "disxx", __LINE__, __FILE__)
        call assertEqual( ref_isec1, isec1, "isec1", __LINE__, __FILE__)
        call assertEqual( ref_vw10, vw10, tol, "vw10", __LINE__, __FILE__)
        call assertEqual( ref_h0, h0, tol, "h0", __LINE__, __FILE__)
        call assertEqual( ref_hum, hum, tol, "hum", __LINE__, __FILE__)
        call assertEqual( ref_ol_metreg_rcp, ol_metreg_rcp, tol, "ol_metreg_rcp", __LINE__, __FILE__)
        call assertEqual( ref_shear, shear, tol, "shear", __LINE__, __FILE__)
        call assertEqual( ref_rc_aer_ms, rc_aer_ms, tol, "rc_aer_ms", __LINE__, __FILE__)
        call assertEqual( ref_rc_nh3_ms, rc_nh3_ms, tol, "rc_nh3_ms", __LINE__, __FILE__)
        call assertEqual( ref_rc_no2_ms, rc_no2_ms, 10*tol, "rc_no2_ms", __LINE__, __FILE__)
        call assertEqual( ref_temp_C, temp_C, tol, "temp_C", __LINE__, __FILE__)
        call assertEqual( ref_uster_metreg_rcp, uster_metreg_rcp, tol, "uster_metreg_rcp", __LINE__, __FILE__)
        call assertEqual( ref_pcoef, pcoef, tol, "pcoef", __LINE__, __FILE__)
        call assertEqual( ref_htot, htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_htt, htt, tol, "htt", __LINE__, __FILE__)
        call assertEqual( ref_itra, itra, "itra", __LINE__, __FILE__)
        call assertEqual( ref_aant, aant, tol, "aant", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_xl, xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_rb_ms, rb_ms, tol, "rb_ms", __LINE__, __FILE__)
        call assertEqual( ref_ra_ms_4, ra_ms_4, tol, "ra_ms_4", __LINE__, __FILE__)
        call assertEqual( ref_ra_ms_zra, ra_ms_zra, tol, "ra_ms_zra", __LINE__, __FILE__)
        call assertEqual( ref_xvglbr, xvglbr, tol, "xvglbr", __LINE__, __FILE__)
        call assertEqual( ref_xvghbr, xvghbr, tol, "xvghbr", __LINE__, __FILE__)
        call assertEqual( ref_xloc, xloc, tol, "xloc", __LINE__, __FILE__)
        call assertEqual( ref_xl100, xl100, tol, "xl100", __LINE__, __FILE__)
        call assertEqual( ref_rad, rad, tol, "rad", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_rc_so2_ms, rc_so2_ms, tol, "rc_so2_ms", __LINE__, __FILE__)
        call assertEqual( ref_coef_space_heating, coef_space_heating, tol, "coef_space_heating", __LINE__, __FILE__)
        call assertEqual( ref_regenk, regenk, tol, "regenk", __LINE__, __FILE__)
        call assertEqual( ref_buil, buil, tol, "buil", __LINE__, __FILE__)
        call assertEqual( ref_rint, rint, tol, "rint", __LINE__, __FILE__)
        call assertEqual( ref_percvk, percvk, tol, "percvk", __LINE__, __FILE__)
        call assertEqual( ref_isec_in, isec_in, "isec_in", __LINE__, __FILE__)
   end subroutine test_ops_statparexp2

   subroutine test_ops_statparexp()
   use m_commonfile
   use m_ops_statparexp
   use m_astat
   use m_commonconst_lib, only: NTRAJ, NCOMP, NSTAB, NSEK
   use m_error
   use no_pfunit
   use m_ops_varin, only: Tvarin_meteo, Tvarin_unc
   
        TYPE(Tvarin_meteo) :: varin_meteo ! input variables for meteo 
        type(Tvarin_unc) :: varin_unc
        real, parameter :: tol = 1e-5
        integer, parameter :: istab = 1
        real, parameter :: hbron =   1.00000000E+01
        real, parameter :: qww =   0.00000000E+00
        real, parameter :: D_stack =  -9.99000000E+02
        real, parameter :: V_stack =  -9.99000000E+02
        real, parameter :: Ts_stack =  -9.99000000E+02
        logical, parameter :: emis_horizontal = .false.
        integer, parameter :: iwd = 0
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: uurtot =   8.75600000E+03
        real, parameter :: trafst(4) = (/   0.00000000E+00,   1.00000000E+05,   3.00000000E+05,   1.00000000E+06/)
        real, parameter :: disx =   1.00000000E+02
        integer, parameter :: isec_prelim = 1
        real :: disxx
        real, parameter :: ref_disxx =   1.00000000E+02
        integer :: isec1
        integer, parameter :: ref_isec1 = 1
        real :: vw10
        real, parameter :: ref_vw10 =   2.10111094E+00
        real :: h0
        real, parameter :: ref_h0 =   3.41896935E+01
        real :: hum
        real, parameter :: ref_hum =   8.19999924E+01
        real :: ol_metreg_rcp
        real, parameter :: ref_ol_metreg_rcp =  -4.11121063E+01
        real :: shear
        real, parameter :: ref_shear =   5.00605917E+00
        real :: rc_aer_ms
        real, parameter :: ref_rc_aer_ms =   4.48999969E+02
        real :: rc_nh3_ms
        real, parameter :: ref_rc_nh3_ms =   3.39999962E+01
        real :: rc_no2_ms
        real, parameter :: ref_rc_no2_ms =   2.94999969E+02
        real :: temp_C
        real, parameter :: ref_temp_C =   9.99999905E+00
        real :: uster_metreg_rcp
        real, parameter :: ref_uster_metreg_rcp =   2.72066683E-01
        real :: pcoef
        real, parameter :: ref_pcoef =   2.39999995E-01
        real :: htot
        real, parameter :: ref_htot =   1.00000000E+01
        real :: htt
        real, parameter :: ref_htt =   1.00000000E+01
        integer :: itra
        integer, parameter :: ref_itra = 1
        real :: aant
        real, parameter :: ref_aant =   6.19757576E+01
        real :: xl
        real, parameter :: ref_xl =   2.59347412E+02
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
        real :: xloc
        real, parameter :: ref_xloc =   2.58999969E+02
        real :: xl100
        real, parameter :: ref_xl100 =   6.02999939E+02
        real :: rad
        real, parameter :: ref_rad =   7.19999924E+01
        real :: rc_so2_ms
        real, parameter :: ref_rc_so2_ms =   4.79999962E+01
        real :: coef_space_heating
        real, parameter :: ref_coef_space_heating =   8.59999943E+00
        real :: regenk
        real, parameter :: ref_regenk =   5.20000011E-02
        real :: buil
        real, parameter :: ref_buil =   1.43999994E+00
        real :: rint
        real, parameter :: ref_rint =   1.24000001E+00
        real :: percvk
        real, parameter :: ref_percvk =   7.07809022E-03
        integer :: isec_in
        integer, parameter :: ref_isec_in = 1
        type(TError) :: error
        logical, parameter :: ref_error_haserror = .false.
        character(len=*), parameter :: ref_error_message = ""
        real, allocatable :: astat(:,:,:,:)
        character(len=500) :: filename
        filename  = './level_1/resources/astat_after_readstexp.data'
        call read_astat(astat, filename)
        
        call ops_statparexp( varin_meteo, varin_unc, istab, hbron, qww, D_stack, V_stack, Ts_stack, emis_horizontal, iwd, radius, &
            uurtot, astat, trafst, disx, isec_prelim, disxx, isec1, vw10, h0, hum, ol_metreg_rcp, shear, rc_aer_ms, &
            rc_nh3_ms, rc_no2_ms, temp_C, uster_metreg_rcp, pcoef, htot, htt, itra, aant, xl, rb_ms, ra_ms_4, &
            ra_ms_zra, xvglbr, xvghbr, xloc, xl100, rad, rc_so2_ms, coef_space_heating, regenk, buil, &
            rint, percvk, isec_in, error)
        if (error%haserror) call write_error(IOB_STDOUT, error)
        call assertFalse( error%haserror, "expected error from ops_statparexp", __LINE__, __FILE__)
        call assertEqual( ref_disxx, disxx, tol, "disxx", __LINE__, __FILE__)
        call assertEqual( ref_isec1, isec1, "isec1", __LINE__, __FILE__)
        call assertEqual( ref_vw10, vw10, tol, "vw10", __LINE__, __FILE__)
        call assertEqual( ref_h0, h0, tol, "h0", __LINE__, __FILE__)
        call assertEqual( ref_hum, hum, tol, "hum", __LINE__, __FILE__)
        call assertEqual( ref_ol_metreg_rcp, ol_metreg_rcp, tol, "ol_metreg_rcp", __LINE__, __FILE__)
        call assertEqual( ref_shear, shear, tol, "shear", __LINE__, __FILE__)
        call assertEqual( ref_rc_aer_ms, rc_aer_ms, tol, "rc_aer_ms", __LINE__, __FILE__)
        call assertEqual( ref_rc_nh3_ms, rc_nh3_ms, tol, "rc_nh3_ms", __LINE__, __FILE__)
        call assertEqual( ref_rc_no2_ms, rc_no2_ms, tol, "rc_no2_ms", __LINE__, __FILE__)
        call assertEqual( ref_temp_C, temp_C, tol, "temp_C", __LINE__, __FILE__)
        call assertEqual( ref_uster_metreg_rcp, uster_metreg_rcp, tol, "uster_metreg_rcp", __LINE__, __FILE__)
        call assertEqual( ref_pcoef, pcoef, tol, "pcoef", __LINE__, __FILE__)
        call assertEqual( ref_htot, htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_htt, htt, tol, "htt", __LINE__, __FILE__)
        call assertEqual( ref_itra, itra, "itra", __LINE__, __FILE__)
        call assertEqual( ref_aant, aant, tol, "aant", __LINE__, __FILE__)
        call assertEqual( ref_xl, xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_rb_ms, rb_ms, tol, "rb_ms", __LINE__, __FILE__)
        call assertEqual( ref_ra_ms_4, ra_ms_4, tol, "ra_ms_4", __LINE__, __FILE__)
        call assertEqual( ref_ra_ms_zra, ra_ms_zra, tol, "ra_ms_zra", __LINE__, __FILE__)
        call assertEqual( ref_xvglbr, xvglbr, tol, "xvglbr", __LINE__, __FILE__)
        call assertEqual( ref_xvghbr, xvghbr, tol, "xvghbr", __LINE__, __FILE__)
        call assertEqual( ref_xloc, xloc, tol, "xloc", __LINE__, __FILE__)
        call assertEqual( ref_xl100, xl100, tol, "xl100", __LINE__, __FILE__)
        call assertEqual( ref_rad, rad, tol, "rad", __LINE__, __FILE__)
        call assertEqual( ref_rc_so2_ms, rc_so2_ms, tol, "rc_so2_ms", __LINE__, __FILE__)
        call assertEqual( ref_coef_space_heating, coef_space_heating, tol, "coef_space_heating", __LINE__, __FILE__)
        call assertEqual( ref_regenk, regenk, tol, "regenk", __LINE__, __FILE__)
        call assertEqual( ref_buil, buil, tol, "buil", __LINE__, __FILE__)
        call assertEqual( ref_rint, rint, tol, "rint", __LINE__, __FILE__)
        call assertEqual( ref_percvk, percvk, tol, "percvk", __LINE__, __FILE__)
        call assertEqual( ref_isec_in, isec_in, "isec_in", __LINE__, __FILE__)
   end subroutine test_ops_statparexp
end module m_test_ops_statparexp
 
program p_test_ops_statparexp
use m_test_ops_statparexp
use no_pfunit
implicit none
   call test_ops_statparexp()
   call test_ops_statparexp2()
   call conclusion()
end program p_test_ops_statparexp

