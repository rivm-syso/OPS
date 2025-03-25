module m_test_m_gauss_cwi
implicit none
contains
   subroutine test_gauss_cwi()
   use m_gauss_cwi
   use no_pfunit_ops_lt

        real, parameter :: tol = 1e-5
        
        ! Input for gauss_cwi
        real  :: q                  ! source strength of emission [g/s]
        real  :: hemis2             ! emission height (including plume rise and plume descent due to sedimentation) [m]
        real  :: x                  ! source-receptor distance [m]
        real  :: z                  ! receptor height [m]
        real  :: u                  ! wind speed [m/s]
        real  :: sigz               ! vertical dispersion length [m]
        real  :: zi                 ! mixing height [m]
        real  :: vs                 ! settling velocity [m/s]
        real  :: sedfac             ! sedimentation factor
                                    ! sedfac is based on the ratio of sigma_z and d_descent = distance the plume has descended
                                    ! and describes the relative importance of sedimentation vs. vertical dispersion.
        ! Output:
        real :: c                  ! crosswind integrated concentration at receptor distance x and height z, for a source with Q g/s source strength [g/m2]

        real, parameter :: ref_c(4) =   (/ 7.589709013700E-03, 4.999999888241E-03, 5.787073634565E-03, 0.0 /)

        ! Default case
        q = 1.0; hemis2 = 5.0; x = 100; z = 4.0; u = 5.0; sigz = 20.0; zi = 400; vs = 0.0; sedfac = 0.0;
        call gauss_cwi(q,hemis2,x,z,u,sigz,zi,vs,sedfac,c)
        call assertRelativelyEqual( ref_c(1), c, tol, "concentration", __LINE__, __FILE__)

        ! sigz > 1.6*zi
        q = 1.0; hemis2 = 5.0; x = 100; z = 4.0; u = 5.0; sigz = 80.0; zi = 40; vs = 0.0; sedfac = 0.0;
        call gauss_cwi(q,hemis2,x,z,u,sigz,zi,vs,sedfac,c)
        call assertRelativelyEqual( ref_c(2), c, tol, "concentration", __LINE__, __FILE__)

        ! With settling velocity:
        q = 1.0; hemis2 = 5.0; x = 100; z = 4.0; u = 5.0; sigz = 20.0; zi = 400; vs = 0.1; sedfac = 0.5;
        call gauss_cwi(q,hemis2,x,z,u,sigz,zi,vs,sedfac,c)
        call assertRelativelyEqual( ref_c(3), c, tol, "concentration", __LINE__, __FILE__)

        ! z > zi:
        q = 1.0; hemis2 = 5.0; x = 100; z = 45.0; u = 5.0; sigz = 20.0; zi = 40; vs = 0.0; sedfac = 0.0;
        call gauss_cwi(q,hemis2,x,z,u,sigz,zi,vs,sedfac,c)
        call assertRelativelyEqual( ref_c(4), c, tol, "concentration", __LINE__, __FILE__)

   end subroutine test_gauss_cwi

end module m_test_m_gauss_cwi
 
program p_test_m_gauss_cwi
use m_test_m_gauss_cwi
use no_pfunit
implicit none
   call test_gauss_cwi()
   call conclusion()
end program p_test_m_gauss_cwi
