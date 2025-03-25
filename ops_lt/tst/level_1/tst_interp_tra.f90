



module m_test_interp_tra
implicit none
contains
    subroutine test_interp_tra()
    use m_astat
    use m_ops_statparexp
    use m_commonfile
    use no_pfunit
        integer :: itest
        integer, parameter :: ntest = 2
        real, parameter :: tol = 1e-5
        integer, parameter :: itra = 1
        real, parameter :: s_dist(4) = (/   0.00000000E+00,   1.01000071E-03,   0.00000000E+00,   0.00000000E+00/)
        integer :: ids
        integer, parameter :: istab = 1
        integer, parameter :: isec_in = 1
        real, parameter :: tal(4) = (/   6.19999962E+01,   3.79999962E+01,   2.99999981E+01,   1.29999990E+01/)
        integer :: itrx
        integer, parameter :: ref_itrx = 1
        real :: aant
        real, parameter :: ref_aant(ntest) =  (/ 6.19757576E+01,   37.9999961853  /)
        real :: stt(27)
        real, parameter :: ref_stt(27,2) = reshape( (/ &
             0.00000000E+00,   3.48399689E+02,   5.73864365E+00,  -9.99000000E+02, &
            -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,  -9.99000000E+02,  -9.99000000E+02,   7.22331238E+01,  -9.99000000E+02, &
            -9.99000000E+02,   7.63242340E+01,   7.25818848E+02,   1.69145859E+02,   3.53618750E+03, &
             0.00000000E+00,   4.37503809E+03,  -9.99000000E+02,  -9.99000000E+02, &
            -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,  -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02, &
            -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02,  -9.99000000E+02/), (/ 27,2 /) )
        real, allocatable :: astat(:, :, :, :) ! statistical meteo parameters
        call example_astat(astat)

        do itest = 1,ntest
           ids = itest + 1
           call interp_tra( itra, s_dist, ids, istab, isec_in, tal, astat, itrx, aant, stt)
           call assertEqual( ref_itrx, itrx, "itrx", __LINE__, __FILE__)
           call assertEqual( ref_aant(itest), aant, tol, "aant", __LINE__, __FILE__)
           call assertEqual( ref_stt(:,itest), stt, tol, "stt", __LINE__, __FILE__)
        end do
   end subroutine test_interp_tra
end module m_test_interp_tra

program tst_interp_tra
use no_pfunit
use m_test_interp_tra
implicit none
    call test_interp_tra()
    call conclusion()
end program tst_interp_tra
