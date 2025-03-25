module m_test_bepafst
implicit none
contains
    subroutine test_bepafst()
    use m_ops_statparexp
    use m_commonfile
    use no_pfunit
    use m_error, only: TError
        real, parameter :: tol = 1e-5
        integer, parameter :: itra = 1
        real, parameter :: s_dist(4) = (/   0.00000000E+00,   1.01000071E-03,   0.00000000E+00,   0.00000000E+00/)
        real, parameter :: trafst(4) = (/   0.00000000E+00,   1.00000000E+05,   3.00000000E+05,   1.00000000E+06/)
        real, parameter :: disx =   1.00000000E+02
        integer, parameter :: istab = 1
        type(TError) :: error
        real, parameter :: in_dscor(4) = (/   0.00000000E+00,   1.04999992E+02,   3.04999969E+02,   1.12099988E+03/)
        real, parameter :: in_xl =   2.59347412E+02
        real :: dscor(4)
        real, parameter :: ref_dscor(4) = (/   0.00000000E+00,   1.04999992E+02,   3.04999969E+02,   1.12099988E+03/)
        real :: xl
        real, parameter :: ref_xl =   2.59347412E+02
        real :: disxx
        real, parameter :: ref_disxx =   1.00000000E+02
        error%haserror = .false.
        error%message = ""
        dscor = in_dscor
        xl = in_xl
        call bepafst( itra, s_dist, trafst, disx, istab, error, dscor, xl, disxx)
        call assertEqual( ref_dscor, dscor, tol, "dscor", __LINE__, __FILE__)
        call assertEqual( ref_xl, xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_disxx, disxx, tol, "disxx", __LINE__, __FILE__)
    end subroutine test_bepafst

    subroutine test_bepafst2()
    use m_ops_statparexp
    use m_commonfile
    use no_pfunit
    use m_error, only: TError
        real, parameter :: tol = 1e-5
        integer, parameter :: itra = 3
        real, parameter :: s_dist(4) = (/   0.00000000E+00,   0.00000000E+00,   3.04650545E-01,   0.00000000E+00/)
        real, parameter :: trafst(4) = (/   0.00000000E+00,   1.00000000E+05,   3.00000000E+05,   1.00000000E+06/)
        real, parameter :: disx =   1.60930109E+05
        integer, parameter :: istab = 1
        type(TError) :: error
        real, parameter :: in_dscor(4) = (/   0.00000000E+00,   1.10966660E+02,   3.99466614E+02,   1.44229980E+03/)
        real, parameter :: in_xl =   4.83222778E+02
        real :: dscor(4)
        real, parameter :: ref_dscor(4) = (/   0.00000000E+00,   1.10966660E+02,   3.99466614E+02,   1.44229980E+03/)
        real :: xl
        real, parameter :: ref_xl =   4.83222778E+02
        real :: disxx
        real, parameter :: ref_disxx =   1.89457406E+05
        dscor = in_dscor
        xl = in_xl
        call bepafst( itra, s_dist, trafst, disx, istab, error, dscor, xl, disxx)
        call assertEqual( ref_dscor, dscor, tol, "dscor", __LINE__, __FILE__)
        call assertEqual( ref_xl, xl, tol, "xl", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_disxx, disxx, tol, "disxx", __LINE__, __FILE__)
    end subroutine test_bepafst2

    subroutine test_bepafst3()
    use m_ops_statparexp
    use m_commonfile
    use no_pfunit
    use m_error, only: TError
        real, parameter :: tol = 1e-5
        integer, parameter :: itra = 3
        real, parameter :: s_dist(4) = (/   0.00000000E+00,   0.00000000E+00,   3.04650545E-01,   0.00000000E+00/)
        real, parameter :: trafst(4) = (/   0.00000000E+00,   1.00000000E+05,   3.00000000E+05,   1.00000000E+06/)
        real, parameter :: disx =   1.60930109E+05
        integer, parameter :: istab = 1
        type(TError) :: error
        real, parameter :: in_dscor(4) = (/   0.00000000E+00,   1.10966660E+02,   0.0,   1.44229980E+03/)
        real, parameter :: in_xl =   4.83222778E+02
        real :: dscor(4)
        real, parameter :: ref_dscor(4) = (/   0.00000000E+00,   1.10966660E+02,   300.0,   1.44229980E+03/)
        real :: xl
        real, parameter :: ref_xl =   4.83222778E+02
        real :: disxx
        real, parameter :: ref_disxx =   173202.078125
        dscor = in_dscor
        xl = in_xl
        call bepafst( itra, s_dist, trafst, disx, istab, error, dscor, xl, disxx)
        call assertEqual( ref_dscor, dscor, tol, "dscor", __LINE__, __FILE__)
        call assertEqual( ref_xl, xl, tol, "xl", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_disxx, disxx, tol, "disxx", __LINE__, __FILE__)
    end subroutine test_bepafst3

    subroutine test_bepafst4()
    use m_ops_statparexp
    use m_commonfile
    use no_pfunit
    use m_error, only: TError
        real, parameter :: tol = 1e-5
        integer, parameter :: itra = 3
        real, parameter :: s_dist(4) = (/   0.00000000E+00,   0.00000000E+00,   3.04650545E-01,   0.00000000E+00/)
        real, parameter :: trafst(4) = (/   0.00000000E+00,   1.00000000E+05,   3.00000000E+05,   1.00000000E+06/)
        real, parameter :: disx =   1.60930109E+06
        integer, parameter :: istab = 1
        type(TError) :: error
        real, parameter :: in_dscor(4) = (/   0.00000000E+00,   1.10966660E+02,   0.0,   1.44229980E+03/)
        real, parameter :: in_xl =   4.83222778E+02
        real :: dscor(4)
        real, parameter :: ref_dscor(4) = (/   0.00000000E+00,   1.10966660E+02,   300.0,   1.44229980E+03/)
        real :: xl
        
        real, parameter :: ref_xl =   5.715512276044684e+02
        real :: disxx
        real, parameter :: ref_disxx =   2754791.00000
        dscor = in_dscor
        xl = in_xl
        call bepafst( itra, s_dist, trafst, disx, istab, error, dscor, xl, disxx)
        call assertEqual( ref_dscor, dscor, tol, "dscor", __LINE__, __FILE__)
        call assertEqual( ref_xl, xl, tol, "xl", __LINE__, __FILE__)
        call assertEqual( ref_disxx, disxx, tol, "disxx", __LINE__, __FILE__)
    end subroutine test_bepafst4
end module m_test_bepafst

program tst_bepafst
use no_pfunit
use m_test_bepafst
implicit none
    call test_bepafst()
    call test_bepafst2()
    call test_bepafst3()
    call test_bepafst4()
    call conclusion()
end program tst_bepafst

