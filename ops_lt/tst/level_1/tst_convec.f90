module m_test_ops_convec
implicit none
contains
   subroutine test_ops_convec2()
   use m_ops_convec
   use m_commonfile
   use no_pfunit
   use m_ops_varin
        
        real, parameter :: tol = 1e-5
        real, parameter :: z0 =   2.00000003E-01
        real, parameter :: zi =   8.42167419E+02
        real, parameter :: ol =  -3.06986713E+01
        real, parameter :: uster =   3.40021461E-01
        real, parameter :: h =   1.00000000E+02
        real, parameter :: x =   2.78657495E+03
        real :: uh
        real, parameter :: ref_uh =   4.11513376E+00
        real :: zu
        real, parameter :: ref_zu =   3.15502197E+02
        real :: szc
        real, parameter :: ref_szc =   4.21681458E+02
        TYPE(Tvarin_meteo)                               :: varin_meteo                ! input variables for meteo
        
        call ops_convec( varin_meteo, z0, zi, ol, uster, h, x, uh, zu, szc)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_szc, szc, tol, "szc", __LINE__, __FILE__)
   end subroutine test_ops_convec2

   subroutine test_ops_convec()
   use m_ops_convec
   use m_commonfile
   use no_pfunit
   use m_ops_varin
        real, parameter :: tol = 1e-5
        real, parameter :: z0 =   2.00000003E-01
        real, parameter :: zi =   2.86306641E+02
        real, parameter :: ol =  -2.62438622E+01
        real, parameter :: uster =   2.40789771E-01
        real, parameter :: h =   1.00000000E+02
        real, parameter :: x =   1.87175059E+04
        real :: uh
        real, parameter :: ref_uh =   2.70279980E+00
        real :: zu
        real, parameter :: ref_zu =   1.43153320E+02
        real :: szc
        real, parameter :: ref_szc =   2.45805908E+03
        TYPE(Tvarin_meteo)                               :: varin_meteo                ! input variables for meteo

        call ops_convec( varin_meteo, z0, zi, ol, uster, h, x, uh, zu, szc)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_szc, szc, tol, "szc", __LINE__, __FILE__)
   end subroutine test_ops_convec
end module m_test_ops_convec
 
program p_test_ops_convec
use m_test_ops_convec
use no_pfunit
implicit none
   call test_ops_convec2()
   call test_ops_convec()
   call conclusion()
end program p_test_ops_convec

