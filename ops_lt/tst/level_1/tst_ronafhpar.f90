module m_test_ronafhpar
implicit none
contains
    subroutine test_ronafhpar()
    use m_ops_statparexp
    use m_commonfile
    use m_astat
    use no_pfunit
        real, parameter :: tol = 1e-5
        real, parameter :: radius =   1.00000000E+03
        real, parameter :: disxx =   2.11424922E+04
        integer, parameter :: istab = 1
        real, parameter :: s_wind =   4.66666698E-01
        integer, parameter :: isec1 = 5
        real, parameter :: s_dist(4) = (/   0.00000000E+00,   2.11432815E-01,   0.00000000E+00,   0.00000000E+00/)
        integer, parameter :: ids = 2
        real :: sa
        real, parameter :: ref_sa =   0.00000000E+00
        real :: so
        real, parameter :: ref_so =   -1179.54736328
        real :: stta(27)
        real, parameter :: ref_stta(27) = (/   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00/)
        real :: sttr(27)
        real, parameter :: ref_sttr(27) = (/   0.00000000E+00,  -6.11372438E+05,   0.00000000E+00,   1.17836775E+06, &
             1.17836775E+06,   1.17836775E+06,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             2.48988547E+05,   0.00000000E+00,   0.00000000E+00,   1.60425922E+05,   0.00000000E+00,   1.75200000E+05, &
             0.00000000E+00,   0.00000000E+00,   1.17836775E+06,   1.17836775E+06,   0.00000000E+00,   1.17836775E+06, &
             1.17836775E+06,   0.00000000E+00,  -4.23285188E+05,   1.36491062E+05,  -5.16795050E+06/)
        real, allocatable :: astat(:, :, :, :) ! statistical meteo parameters
        call example_astat(astat)
        call ronafhpar( radius, disxx, istab, s_wind, isec1, astat, s_dist, ids, sa, so, stta, sttr)
        call assertEqual( ref_sa, sa, tol, "sa", __LINE__, __FILE__)
        call assertEqual( ref_so, so, tol=1e-3, message="so", lineno=__LINE__, filename=__FILE__)
        call assertEqual( ref_stta, stta, tol, "stta", __LINE__, __FILE__)
        call assertEqual( ref_sttr, sttr, tol=0.75, message="sttr", lineno=__LINE__, filename=__FILE__)
    end subroutine test_ronafhpar

    subroutine test_ronafhpar2()
    use m_ops_statparexp
    use m_commonfile
    use m_astat
    use m_log_call
    use no_pfunit
        real, parameter :: tol = 1e-5
        real, parameter :: radius =   2.30000000E+04
        real, parameter :: disxx =   2.11424922E+04
        integer, parameter :: istab = 1
        real, parameter :: s_wind =   4.66666698E-01
        integer, parameter :: isec1 = 5
        real, parameter :: s_dist(4) = (/   0.00000000E+00,   2.11432815E-01,   0.00000000E+00,   0.00000000E+00/)
        integer, parameter :: ids = 2
        real :: sa
        real, parameter :: ref_sa =   -5994.00000000
        real :: so
        real, parameter :: ref_so =   -4862.83251953
        real :: stta(27)
        real, parameter :: ref_stta(27) = (/   0.00000000E+00,   3.01495361E+02,   0.00000000E+00,  -9.99000000E+02, &
            -9.99000000E+02,  -9.99000000E+02,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             1.48646191E-01,   0.00000000E+00,   0.00000000E+00,   9.91250839E+01,   0.00000000E+00,   8.32318420E+01, &
             0.00000000E+00,   0.00000000E+00,  -9.99000000E+02,  -9.99000000E+02,   0.00000000E+00,  -9.99000000E+02, &
            -9.99000000E+02,   0.00000000E+00,   5.61070496E+02,   1.48510788E+02,   5.46861182E+03/)
        real :: sttr(27)
        real, parameter :: ref_sttr(27) = (/   0.00000000E+00,  -2.48309406E+05,   0.00000000E+00,   4.85796950E+06, &
             4.85796950E+06,   4.85796950E+06,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             1.02646712E+06,   0.00000000E+00,   0.00000000E+00,   6.65033125E+05,   0.00000000E+00,   6.76299250E+05, &
             0.00000000E+00,   0.00000000E+00,   4.85796950E+06,   4.85796950E+06,   0.00000000E+00,   4.85796950E+06, &
             4.85796950E+06,   0.00000000E+00,  -1.26557200E+06,   5.15293656E+05,  -2.23803960E+07/)

        real, allocatable :: astat(:, :, :, :) ! statistical meteo parameters
        call example_astat(astat)
        call ronafhpar( radius, disxx, istab, s_wind, isec1, astat, s_dist, ids, sa, so, stta, sttr)
        call assertEqual( ref_sa, sa, tol, "sa", __LINE__, __FILE__)
        call assertEqual( ref_so, so, tol, "so", __LINE__, __FILE__)
        call assertEqual( ref_stta, stta, tol, "stta", __LINE__, __FILE__)
        call assertEqual( ref_sttr, sttr, tol, "sttr", __LINE__, __FILE__)
        call print_array("ref_stta", stta,"")
    end subroutine test_ronafhpar2
end module m_test_ronafhpar

program tst_ronafhpar
use no_pfunit
use m_test_ronafhpar
implicit none
    call test_ronafhpar()
    call test_ronafhpar2()
    call conclusion()
end program tst_ronafhpar

