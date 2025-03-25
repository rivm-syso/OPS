module m_test_ops_surface
implicit none
contains
   subroutine test_ops_surface7()
   use m_ops_surface
   use m_commonfile
   use no_pfunit
   use m_ops_varin
       
        TYPE(Tvarin_meteo) :: varin_meteo ! input variables for meteo
        real, parameter :: tol = 1e-5
        real, parameter :: z0 =   2.00000003E-01
        real, parameter :: zi =   8.88187988E+02
        real, parameter :: ol =  -8.95448608E+01
        real, parameter :: uster =   3.93561244E-01
        real, parameter :: h =   5.00000000E+00
        real, parameter :: x =   1.17441475E+04
        real :: uh
        real, parameter :: ref_uh =   5.59375668E+00
        real :: zu
        real, parameter :: ref_zu =   4.44093994E+02
        real :: szs
        real, parameter :: ref_szs =   8.69073853E+02
        call ops_surface( varin_meteo, z0, zi, ol, uster, h, x, uh, zu, szs)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_szs, szs, tol, "szs", __LINE__, __FILE__)
   end subroutine test_ops_surface7

   subroutine test_ops_surface6()
   use m_ops_surface
   use m_commonfile
   use no_pfunit
   use m_ops_varin
       
        TYPE(Tvarin_meteo) :: varin_meteo ! input variables for meteo
        real, parameter :: tol = 1e-5
        real, parameter :: z0 =   2.00000003E-01
        real, parameter :: zi =   3.80191612E+01
        real, parameter :: ol =   1.18723192E+01
        real, parameter :: uster =   5.99999987E-02
        real, parameter :: h =   5.00000000E+00
        real, parameter :: x =   2.35702286E+01
        real :: uh
        real, parameter :: ref_uh =   7.63575673E-01
        real :: zu
        real, parameter :: ref_zu =   5.00000000E+00
        real :: szs
        real, parameter :: ref_szs =   2.17035604E+00
        call ops_surface( varin_meteo, z0, zi, ol, uster, h, x, uh, zu, szs)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_szs, szs, tol, "szs", __LINE__, __FILE__)
   end subroutine test_ops_surface6


  subroutine test_ops_surface5()
   use m_ops_surface
   use m_commonfile
   use no_pfunit
   use m_ops_varin
       
        TYPE(Tvarin_meteo) :: varin_meteo ! input variables for meteo
        real, parameter :: tol = 1e-5
        real, parameter :: z0 =   2.00000003E-01
        real, parameter :: zi =   4.11510048E+01
        real, parameter :: ol =   9.63784313E+00
        real, parameter :: uster =   5.99999987E-02
        real, parameter :: h =   5.00000000E+00
        real, parameter :: x =   2.54950974E+02
        real :: uh
        real, parameter :: ref_uh =   8.23710799E-01
        real :: zu
        real, parameter :: ref_zu =   5.00000000E+00
        real :: szs
        real, parameter :: ref_szs =   6.30967665E+00
        call ops_surface( varin_meteo, z0, zi, ol, uster, h, x, uh, zu, szs)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_szs, szs, tol, "szs", __LINE__, __FILE__)
   end subroutine test_ops_surface5


   subroutine test_ops_surface4()
   use m_ops_surface
   use m_commonfile
   use no_pfunit
   use m_ops_varin
       
        TYPE(Tvarin_meteo) :: varin_meteo ! input variables for meteo
        real, parameter :: tol = 1e-5
        real, parameter :: z0 =   2.00000003E-01
        real, parameter :: zi =   4.78277473E+01
        real, parameter :: ol =   9.79801559E+00
        real, parameter :: uster =   5.99999987E-02
        real, parameter :: h =   5.00000000E+00
        real, parameter :: x =   1.47120693E+04
        real :: uh
        real, parameter :: ref_uh =   1.99607861E+00
        real :: zu
        real, parameter :: ref_zu =   2.39138737E+01
        real :: szs
        real, parameter :: ref_szs =   3.81437836E+01
        call ops_surface( varin_meteo, z0, zi, ol, uster, h, x, uh, zu, szs)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_szs, szs, tol, "szs", __LINE__, __FILE__)
   end subroutine test_ops_surface4

   subroutine test_ops_surface3()
   use m_ops_surface
   use m_commonfile
   use no_pfunit
   use m_ops_varin
       
        TYPE(Tvarin_meteo) :: varin_meteo ! input variables for meteo
        real, parameter :: tol = 1e-5
        real, parameter :: z0 =   2.00000003E-01
        real, parameter :: zi =   2.86960632E+02
        real, parameter :: ol =   1.50119598E+02
        real, parameter :: uster =   3.24872583E-01
        real, parameter :: h =   5.00000000E+00
        real, parameter :: x =   9.82344116E+02
        real :: uh
        real, parameter :: ref_uh =   4.25615120E+00
        real :: zu
        real, parameter :: ref_zu =   1.99694805E+01
        real :: szs
        real, parameter :: ref_szs =   3.03530846E+01
        call ops_surface( varin_meteo, z0, zi, ol, uster, h, x, uh, zu, szs)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertEqual( ref_szs, szs, tol, "szs", __LINE__, __FILE__)
   end subroutine test_ops_surface3

   subroutine test_ops_surface2()
   use m_ops_surface
   use m_commonfile
   use no_pfunit
   use m_ops_varin
       
        TYPE(Tvarin_meteo) :: varin_meteo ! input variables for meteo
        real, parameter :: tol = 1e-5
        real, parameter :: z0 =   2.00000003E-01
        real, parameter :: zi =   2.85942810E+02
        real, parameter :: ol =   1.39790878E+02
        real, parameter :: uster =   3.21115643E-01
        real, parameter :: h =   5.00000000E+00
        real, parameter :: x =   2.11424922E+04
        real :: uh
        real, parameter :: ref_uh =   7.16747808E+00
        real :: zu
        real, parameter :: ref_zu =   8.82900620E+01
        real :: szs
        real, parameter :: ref_szs =   1.37633621E+02
        call ops_surface( varin_meteo, z0, zi, ol, uster, h, x, uh, zu, szs)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_szs, szs, tol, "szs", __LINE__, __FILE__)
   end subroutine test_ops_surface2
   subroutine test_ops_surface()
   use m_ops_surface
   use m_commonfile
   use no_pfunit
   use m_ops_varin
       
        TYPE(Tvarin_meteo) :: varin_meteo ! input variables for meteo
        real, parameter :: tol = 1e-5
        real, parameter :: z0 =   2.00000003E-01
        real, parameter :: zi =   2.96321716E+02
        real, parameter :: ol =  -3.29010468E+01
        real, parameter :: uster =   2.52821952E-01
        real, parameter :: h =   5.00000000E+00
        real, parameter :: x =   2.11424922E+04
        real :: uh
        real, parameter :: ref_uh =   2.93962026E+00
        real :: zu
        real, parameter :: ref_zu =   1.48160858E+02
        real :: szs
        real, parameter :: ref_szs =   7.27447571E+02
        call ops_surface( varin_meteo, z0, zi, ol, uster, h, x, uh, zu, szs)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_szs, szs, tol, "szs", __LINE__, __FILE__)
   end subroutine test_ops_surface
end module m_test_ops_surface
 
program p_test_ops_surface
use m_test_ops_surface
use no_pfunit
implicit none
   call test_ops_surface7()
   call test_ops_surface6()
   call test_ops_surface5()
   call test_ops_surface4()
   call test_ops_surface3()
   call test_ops_surface2()
   call test_ops_surface()
   call conclusion()
end program p_test_ops_surface

