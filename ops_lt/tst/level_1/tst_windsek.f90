module m_test_windsek
implicit none
contains
    subroutine test_windsek()
    use m_log_call
    use m_astat
    use m_commonfile
    use m_ops_statparexp, only: windsek
    use no_pfunit
        real, parameter :: tol = 1e-5
        integer :: itest
        integer, parameter :: ntests = 8
        integer, parameter :: istab(ntests) = (/ 1, 5, 6, 2, 3, 4, 4, 4 /)
        real, parameter :: htt(ntests) =    (/  10.,  20.,  10.,  10.,  10.,    5., 5., 5. /)
        real, parameter :: disx(ntests) =   (/ 100., 100., 800., 100., 100., 1670.34434, 1550.0, 200000. /)
        integer, parameter :: iwd(ntests) = (/ 0, 0, 0, 30, 30, 154, 270, 490 /)
        integer, parameter :: isec_prelim(ntests) = (/ 1, 1, 1, 2, 2, 6, 10, 10 /)
        integer :: isec1(ntests)                                                          
        real :: shear(ntests)                                                               
        real :: htot(ntests)                                                                         
        integer :: iwdd(ntests)                                                              
        integer :: isec_in(ntests)
        integer :: isec2(ntests)                                                                 
        real :: s_wind(ntests)                                                                     
        integer, parameter :: ref_isec1(8) = (/ 12, 12, 12, 1, 2, 5, 9, 5/)
        integer, parameter :: ref_iwdd(8) = (/ 354, 351, 357, 27, 30, 146, 266, 130/)
        real, parameter :: ref_htot(8) = (/   1.00000000E+01,   2.00000000E+01,   1.00000000E+01,   1.00000000E+01, &
             1.00000000E+01,   1.00000000E+01,   1.00000000E+01,   1.00000000E+01/)
        real, parameter :: ref_shear(8) = (/   7.33161621E+01,   5.26285400E+01,   3.03414307E+01,   3.33838348E+01, &
            -3.87367249E+00,   8.96240234E+01,   4.50895844E+01,   4.50895844E+01/)
        integer, parameter :: ref_isec_in(8) = (/ 1, 1, 1, 2, 2, 6, 10, 5/)
        integer, parameter :: ref_isec2(8) = (/ 1, 1, 1, 2, 3, 6, 10, 6/)
        real, parameter :: ref_s_wind(8) = (/   8.00000191E-01,   6.99999809E-01,   8.99999619E-01,   8.99999976E-01, &
             0.00000000E+00,   8.66666794E-01,   8.66666794E-01,   3.33333492E-01/)
        real, allocatable :: astat(:, :, :, :) ! statistical meteo parameters
        call example_astat(astat)

        do itest = 1, ntests
           print *,'test ',itest
           call windsek( istab(itest), htt(itest), disx(itest), iwd(itest), astat, isec_prelim(itest), &
                         isec1(itest), shear(itest), htot(itest), iwdd(itest), isec_in(itest), isec2(itest), s_wind(itest))

           call assertEqual( ref_isec1(itest), isec1(itest), "isec1", __LINE__, __FILE__)
           call assertEqual( ref_shear(itest), shear(itest), tol, "shear", __LINE__, __FILE__)
           call assertEqual( ref_htot(itest), htot(itest), tol, "htot", __LINE__, __FILE__)
           call assertEqual( ref_iwdd(itest), iwdd(itest), "iwdd", __LINE__, __FILE__)
           call assertEqual( ref_isec_in(itest), isec_in(itest), "isec_in", __LINE__, __FILE__)
           call assertEqual( ref_isec2(itest), isec2(itest), "isec2", __LINE__, __FILE__)
           call assertEqual( ref_s_wind(itest), s_wind(itest), tol, "s_wind", __LINE__, __FILE__)
        end do
        call print_array("ref_shear", shear,"")
        call print_array("ref_isec_in", isec_in,"")
        call print_array("ref_isec2", isec2,"")
        call print_array("ref_s_wind", s_wind,"")
        call print_array("ref_isec1", isec1,"")
        call print_array("ref_iwdd", iwdd,"")
        call print_array("ref_htot", htot,"")


    end subroutine test_windsek

end module m_test_windsek

program tst_windsek
use no_pfunit
use m_test_windsek
implicit none
    call test_windsek()
    call conclusion()
end program tst_windsek
