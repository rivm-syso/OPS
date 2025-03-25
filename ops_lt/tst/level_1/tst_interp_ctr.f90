module m_test_interp_ctr
implicit none
contains
    subroutine test_interp_ctr()
    use m_commonfile
    use m_ops_statparexp
    use no_pfunit
        real, parameter :: tol = 1e-5
        real, parameter :: disx =   1.00000000E+02
        real, parameter :: trafst(4) = (/   0.00000000E+00,   1.00000000E+05,   3.00000000E+05,   1.00000000E+06/)
        integer :: itra
        integer, parameter :: ref_itra = 1
        real :: s_dist(4)
        real, parameter :: ref_s_dist(4) = (/   0.00000000E+00,   1.01000071E-03,   0.00000000E+00,   0.00000000E+00/)
        integer :: ids
        integer, parameter :: ref_ids = 2
        call interp_ctr( disx, trafst, itra, s_dist, ids)
        call assertEqual( ref_itra, itra, "itra", __LINE__, __FILE__)
        call assertEqual( ref_s_dist, s_dist, tol, "s_dist", __LINE__, __FILE__)
        call assertEqual( ref_ids, ids, "ids", __LINE__, __FILE__)
    end subroutine test_interp_ctr

    subroutine test_interp_ctr2()
    use m_ops_statparexp
    use m_commonfile
    use no_pfunit
        real, parameter :: tol = 1e-5
        real, parameter :: disx =   1.60930109E+05
        real, parameter :: trafst(4) = (/   0.00000000E+00,   1.00000000E+05,   3.00000000E+05,   1.00000000E+06/)
        integer :: itra
        integer, parameter :: ref_itra = 3
        real :: s_dist(4)
        real, parameter :: ref_s_dist(4) = (/   0.00000000E+00,   0.00000000E+00,   3.04650545E-01,   0.00000000E+00/)
        integer :: ids
        integer, parameter :: ref_ids = 3
        call interp_ctr( disx, trafst, itra, s_dist, ids)
        call assertEqual( ref_itra, itra, "itra", __LINE__, __FILE__)
        call assertEqual( ref_s_dist, s_dist, tol, "s_dist", __LINE__, __FILE__)
        call assertEqual( ref_ids, ids, "ids", __LINE__, __FILE__)
    end subroutine test_interp_ctr2

    subroutine test_interp_ctr3()
    use m_ops_statparexp
    use m_commonfile
    use m_error
    use no_pfunit
        real, parameter :: tol = 1e-5
        real, parameter :: disx =   1.61692609E+05
        real, parameter :: trafst(4) = (/   0.00000000E+00,   1.00000000E+05,   3.00000000E+05,   1.00000000E+06/)
        integer :: itra
        integer, parameter :: ref_itra = 3
        real :: s_dist(4)
        real, parameter :: ref_s_dist(4) = (/   0.00000000E+00,   0.00000000E+00,   3.08463037E-01,   0.00000000E+00/)
        integer :: ids
        integer, parameter :: ref_ids = 3
        call interp_ctr( disx, trafst, itra, s_dist, ids)
        call assertEqual( ref_itra, itra, "itra", __LINE__, __FILE__)
        call assertEqual( ref_s_dist, s_dist, tol, "s_dist", __LINE__, __FILE__)
        call assertEqual( ref_ids, ids, "ids", __LINE__, __FILE__)
    end subroutine test_interp_ctr3

end module m_test_interp_ctr

program tst_interp_ctr
use no_pfunit
use m_test_interp_ctr
implicit none
    call test_interp_ctr()
    call test_interp_ctr2()
    call test_interp_ctr3()
    call conclusion()
end program tst_interp_ctr

