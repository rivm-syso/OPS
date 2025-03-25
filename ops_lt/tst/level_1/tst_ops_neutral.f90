module m_test_ops_neutral
implicit none
contains
   subroutine test_ops_neutral5()
   use m_ops_neutral
   use m_commonfile
   use no_pfunit
   use m_ops_varin
   
        TYPE(Tvarin_meteo) :: varin_meteo ! input variables for meteo
        real, parameter :: tol = 1e-5
        real, parameter :: z0 =   2.00000003E-01
        real, parameter :: zi =   1.97453857E+02
        real, parameter :: ol =  -3.73346863E+01
        real, parameter :: uster =   2.42146090E-01
        real, parameter :: h =   1.00000000E+02
        real, parameter :: x =   2.27705957E+03
        real :: uh
        real, parameter :: ref_uh =   2.77236533E+00
        real :: zu
        real, parameter :: ref_zu =   9.87269287E+01
        real :: szn
        real, parameter :: ref_szn =   1.45724991E+02
        call ops_neutral(varin_meteo, z0, zi, ol, uster, h, x, uh, zu, szn)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_szn, szn, tol, "szn", __LINE__, __FILE__)
   end subroutine test_ops_neutral5

   subroutine test_ops_neutral4()
   use m_ops_neutral
   use m_commonfile
   use no_pfunit
   use m_ops_varin
   
        TYPE(Tvarin_meteo) :: varin_meteo ! input variables for meteo
        real, parameter :: tol = 1e-5
        real, parameter :: z0 =   2.00000003E-01
        real, parameter :: zi =   1.80572968E+02
        real, parameter :: ol =  -2.45657368E+01
        real, parameter :: uster =   2.07582891E-01
        real, parameter :: h =   1.00000000E+02
        real, parameter :: x =   1.55724121E+03
        real :: uh
        real, parameter :: ref_uh =   2.23841524E+00
        real :: zu
        real, parameter :: ref_zu =   9.72647934E+01
        real :: szn
        real, parameter :: ref_szn =   1.21679726E+02
        call ops_neutral(varin_meteo, z0, zi, ol, uster, h, x, uh, zu, szn)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_szn, szn, tol, "szn", __LINE__, __FILE__)
   end subroutine test_ops_neutral4

   subroutine test_ops_neutral3()
   use m_ops_neutral
   use m_commonfile
   use no_pfunit
   use m_ops_varin
   
        TYPE(Tvarin_meteo) :: varin_meteo ! input variables for meteo
        real, parameter :: tol = 1e-5
        real, parameter :: z0 =   2.00000003E-01
        real, parameter :: zi =   5.10781250E+01
        real, parameter :: ol =   9.73651981E+00
        real, parameter :: uster =   5.99999987E-02
        real, parameter :: h =   5.00000000E+00
        real, parameter :: x =   1.10700039E+04
        real :: uh
        real, parameter :: ref_uh =   2.04774666E+00
        real :: zu
        real, parameter :: ref_zu =   2.49923553E+01
        real :: szn
        real, parameter :: ref_szn =   2.24454231E+01
        call ops_neutral( varin_meteo, z0, zi, ol, uster, h, x, uh, zu, szn)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_szn, szn, tol, "szn", __LINE__, __FILE__)
   end subroutine test_ops_neutral3

   subroutine test_ops_neutral2()
   use m_ops_neutral
   use m_commonfile
   use no_pfunit
   use m_ops_varin
   
        TYPE(Tvarin_meteo) :: varin_meteo ! input variables for meteo
        real, parameter :: tol = 1e-5
        real, parameter :: z0 =   1.99999995E-04
        real, parameter :: zi =   8.18747711E+01
        real, parameter :: ol =  -7.99372435E+00
        real, parameter :: uster =   5.99999987E-02
        real, parameter :: h =   5.00000000E+00
        real, parameter :: x =   1.21240125E+05
        real :: uh
        real, parameter :: ref_uh =   1.52816868E+00
        real :: zu
        real, parameter :: ref_zu =   4.09373856E+01
        real :: szn
        real, parameter :: ref_szn =   8.92614136E+02
        call ops_neutral( varin_meteo, z0, zi, ol, uster, h, x, uh, zu, szn)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_szn, szn, tol, "szn", __LINE__, __FILE__)
   end subroutine test_ops_neutral2

   subroutine test_ops_neutral()
   use m_ops_neutral
   use m_commonfile
   use no_pfunit
   use m_ops_varin
 
        TYPE(Tvarin_meteo) :: varin_meteo ! input variables for meteo
        real, parameter :: tol = 1e-5
        real, parameter :: z0 =   2.00000003E-01
        real, parameter :: zi =   5.02630997E+01
        real, parameter :: ol =   1.00844126E+01
        real, parameter :: uster =   5.99999987E-02
        real, parameter :: h =   5.00000000E+00
        real, parameter :: x =   2.11424922E+04
        real :: uh
        real, parameter :: ref_uh =   2.02255630E+00
        real :: zu
        real, parameter :: ref_zu =   2.51315498E+01
        real :: szn
        real, parameter :: ref_szn =   3.16808681E+01
        call ops_neutral( varin_meteo, z0, zi, ol, uster, h, x, uh, zu, szn)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_szn, szn, tol, "szn", __LINE__, __FILE__)
   end subroutine test_ops_neutral
end module m_test_ops_neutral
 
program p_test_ops_neutral
use m_test_ops_neutral
use no_pfunit
use m_ops_varin

implicit none
   call test_ops_neutral5()
   call test_ops_neutral4()
   call test_ops_neutral3()
   call test_ops_neutral2()
   call test_ops_neutral()
   call conclusion()
end program p_test_ops_neutral

