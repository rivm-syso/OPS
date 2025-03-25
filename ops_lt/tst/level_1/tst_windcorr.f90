module m_test_windcorr
implicit none
contains
    subroutine test_windcorr()
    use m_ops_statparexp
    use m_astat
    use m_commonfile
    use no_pfunit
        real, parameter :: tol = 1e-5
        integer, parameter :: itra = 1
        integer, parameter :: istab = 1
        real, parameter :: radius =   1.00000000E+00
        real, parameter :: disx =   1.00000000E+02
        integer, parameter :: isec1 = 1
        integer, parameter :: iwdd = 1
        integer, parameter :: isec2 = 2
        integer, parameter :: in_isec_in = 1
        integer :: isec_in
        integer, parameter :: ref_isec_in = 1
        integer :: ispecial
        integer, parameter :: ref_ispecial = 1
        real :: s_wind
        real, parameter :: in_s_wind = 12.0
        real, parameter :: ref_s_wind = 0.0
        real, allocatable :: astat(:, :, :, :) ! statistical meteo parameters
        call example_astat(astat)

        isec_in = in_isec_in
        s_wind = in_s_wind
        astat(itra,1,istab,isec_in) = 3
        call windcorr( itra, istab, radius, disx, isec1, iwdd, isec2, astat, isec_in, ispecial, s_wind)
        call assertEqual( 0, ispecial, "ispecial", __LINE__, __FILE__)

        astat(itra,1,istab,isec_in) = 0
        s_wind = in_s_wind
        isec_in = in_isec_in
        call windcorr( itra, istab, radius, disx, isec1, iwdd, isec2, astat, isec_in, ispecial, s_wind)
        call assertEqual( ref_ispecial, ispecial, "ispecial", __LINE__, __FILE__)
        call assertEqual( ref_isec_in, isec_in, "isec_in", __LINE__, __FILE__)
        call assertEqual( ref_s_wind, s_wind, tol, "s_wind", __LINE__, __FILE__)
    end subroutine test_windcorr


    subroutine test_windcorr2()
    use m_ops_statparexp
    use m_astat
    use m_commonfile
    use no_pfunit
        real, parameter :: tol = 1e-5
        integer, parameter :: itra = 1
        integer, parameter :: istab = 3
        real, parameter :: radius =   1.00000000E+02
        real, parameter :: disx =   1.00000000E+02
        integer, parameter :: isec1 = 8
        integer, parameter :: iwdd = 160
        integer, parameter :: isec2 = 9
        integer, parameter :: in_isec_in = 8
        integer :: isec_in
        integer, parameter :: ref_isec_in = 9
        integer :: ispecial
        integer, parameter :: ref_ispecial = 1
        real :: s_wind
        real, parameter :: in_s_wind = 12.0
        real, parameter :: ref_s_wind =   1.0
        real, allocatable :: astat(:, :, :, :) ! statistical meteo parameters
        call example_astat(astat)
        isec_in = in_isec_in
        s_wind = in_s_wind
        astat(itra,1,istab,isec_in) = 3
        call windcorr( itra, istab, radius, disx, isec1, iwdd, isec2, astat, isec_in, ispecial, s_wind)
        call assertEqual( 0, ispecial, "ispecial", __LINE__, __FILE__)
        call assertEqual( in_isec_in, isec_in, "isec_in", __LINE__, __FILE__)
        call assertEqual( in_s_wind, s_wind, tol, "s_wind", __LINE__, __FILE__)

        isec_in = in_isec_in
        s_wind = in_s_wind
        astat(itra,1,istab,isec_in) = 0
        call windcorr( itra, istab, radius, disx, isec1, iwdd, isec2, astat, isec_in, ispecial, s_wind)
        call assertEqual( in_isec_in, isec_in, "isec_in", __LINE__, __FILE__)
        call assertEqual( 0, ispecial, "ispecial", __LINE__, __FILE__)
        call assertEqual( in_s_wind, s_wind, tol, "s_wind", __LINE__, __FILE__)

        isec_in = in_isec_in
        s_wind = in_s_wind
        astat(itra,1,istab,isec2) = 0
        call windcorr( itra, istab, radius, disx, isec1, iwdd, isec2, astat, isec_in, ispecial, s_wind)
        call assertEqual( ref_isec_in, isec_in, "isec_in", __LINE__, __FILE__)
        call assertEqual( ref_ispecial, ispecial, "ispecial", __LINE__, __FILE__)
        call assertEqual( ref_s_wind, s_wind, tol, "s_wind", __LINE__, __FILE__)
    end subroutine test_windcorr2
end module m_test_windcorr

program tst_windcorr
use m_test_windcorr
use no_pfunit
implicit none
   call test_windcorr()
   call test_windcorr2()
   call conclusion()
end program tst_windcorr


