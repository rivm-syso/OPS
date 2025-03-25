module m_test_ops_vertdisp
implicit none
contains
   subroutine test_ops_vertdisp4()
   use m_ops_vertdisp
   use m_commonfile
   use m_error
   use no_pfunit
   use m_ops_varin, only: Tvarin
   
        TYPE(Tvarin) :: varin ! input variables for meteo
        real, parameter :: tol = 1e-5
        real, parameter :: z0 =   2.00000003E-01
        real, parameter :: zi =   9.79355347E+02
        real, parameter :: ol =  -3.91123466E+01
        real, parameter :: uster =   3.63831908E-01
        real, parameter :: hh =   1.00000000E+02
        real, parameter :: x =   2.11424922E+04
        integer, parameter :: ircp = -999
        integer, parameter :: istab = -999
        character(len=*), parameter :: debugnam = "at_rcp_with_meteo_rcp"
        logical, parameter :: in_error_haserror = .false.
        character(len=*), parameter :: in_error_message = ""
        real :: uh
        real, parameter :: ref_uh =   4.67325544E+00
        real :: zu
        real, parameter :: ref_zu =   4.89677673E+02
        real :: sz
        real, parameter :: ref_sz =   2.30889160E+03
        type(TError) :: error
        logical, parameter :: ref_error_haserror = .false.
        character(len=*), parameter :: ref_error_message = ""
        error%haserror = in_error_haserror
        error%message = in_error_message
        call ops_vertdisp( varin, z0, zi, ol, uster, hh, x, ircp, istab, debugnam, uh, zu, sz, error)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_sz, sz, tol, "sz", __LINE__, __FILE__)
        if (error%haserror) call write_error(IOB_STDOUT,error)
        call assertFalse( error%haserror, "ops_vertdisp", __LINE__, __FILE__)
   end subroutine test_ops_vertdisp4

   subroutine test_ops_vertdisp3()
   use m_ops_vertdisp
   use m_commonfile
   use m_error
   use no_pfunit
   use m_ops_varin, only: Tvarin
   
        TYPE(Tvarin) :: varin ! input variables for meteo
        real, parameter :: tol = 1e-5
        real, parameter :: z0 =   2.00000003E-01
        real, parameter :: zi =   5.02630997E+01
        real, parameter :: ol =   1.00844126E+01
        real, parameter :: uster =   5.99999987E-02
        real, parameter :: hh =   1.00000000E+02
        real, parameter :: x =   2.11424922E+04
        integer, parameter :: ircp = -999
        integer, parameter :: istab = -999
        character(len=*), parameter :: debugnam = "at_rcp_with_meteo_rcp"
        logical, parameter :: in_error_haserror = .false.
        character(len=*), parameter :: in_error_message = ""
        real :: uh
        real, parameter :: ref_uh =   2.65423751E+00
        real :: zu
        real, parameter :: ref_zu =   4.52367897E+01
        real :: sz
        real, parameter :: ref_sz =   5.28767824E+00
        type(TError) :: error
        logical, parameter :: ref_error_haserror = .false.
        character(len=*), parameter :: ref_error_message = ""
        error%haserror = in_error_haserror
        error%message = in_error_message
        call ops_vertdisp( varin, z0, zi, ol, uster, hh, x, ircp, istab, debugnam, uh, zu, sz, error)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_sz, sz, tol, "sz", __LINE__, __FILE__)
        if (error%haserror) call write_error(IOB_STDOUT,error)
        call assertFalse( error%haserror, "ops_vertdisp", __LINE__, __FILE__)
   end subroutine test_ops_vertdisp3

   subroutine test_ops_vertdisp2()
   use m_ops_vertdisp
   use m_commonfile
   use m_error
   use no_pfunit
   use m_ops_varin, only: Tvarin
   
        TYPE(Tvarin) :: varin
        real, parameter :: tol = 1e-5
        real, parameter :: z0 =   2.00000003E-01
        real, parameter :: zi =   2.96321716E+02
        real, parameter :: ol =  -3.29010468E+01
        real, parameter :: uster =   2.52821952E-01
        real, parameter :: hh =   1.00000000E+02
        real, parameter :: x =   2.11424922E+04
        integer, parameter :: ircp = -999
        integer, parameter :: istab = -999
        character(len=*), parameter :: debugnam = "at_rcp_with_meteo_rcp"
        logical, parameter :: in_error_haserror = .false.
        character(len=*), parameter :: in_error_message = ""
        real :: uh
        real, parameter :: ref_uh =   2.93962026E+00
        real :: zu
        real, parameter :: ref_zu =   1.48160858E+02
        real :: sz
        real, parameter :: ref_sz =   1.54802197E+03
        type(TError) :: error
        logical, parameter :: ref_error_haserror = .false.
        character(len=*), parameter :: ref_error_message = ""
        error%haserror = in_error_haserror
        error%message = in_error_message
        call ops_vertdisp( varin, z0, zi, ol, uster, hh, x, ircp, istab, debugnam, uh, zu, sz, error)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_sz, sz, tol, "sz", __LINE__, __FILE__)
        if (error%haserror) call write_error(IOB_STDOUT,error)
        call assertFalse( error%haserror, "ops_vertdisp", __LINE__, __FILE__)
   end subroutine test_ops_vertdisp2

   subroutine test_ops_vertdisp()
   use m_ops_vertdisp
   use m_commonfile
   use m_error
   use no_pfunit
   use m_ops_varin, only: Tvarin
   
        TYPE(Tvarin) :: varin
        real, parameter :: tol = 1e-5
        real, parameter :: z0 =   2.00000003E-01
        real, parameter :: zi =   2.96321716E+02
        real, parameter :: ol =  -3.29010468E+01
        real, parameter :: uster =   2.52821952E-01
        real, parameter :: hh =   5.00000000E+00
        real, parameter :: x =   2.11424922E+04
        integer, parameter :: ircp = -999
        integer, parameter :: istab = -999
        character(len=*), parameter :: debugnam = "at_rcp_with_meteo_rcp"
        logical, parameter :: in_error_haserror = .false.
        character(len=*), parameter :: in_error_message = ""
        real :: uh
        real, parameter :: ref_uh =   2.93962026E+00
        real :: zu
        real, parameter :: ref_zu =   1.48160858E+02
        real :: sz
        real, parameter :: ref_sz =   7.27447571E+02
        type(TError) :: error
        logical, parameter :: ref_error_haserror = .false.
        character(len=*), parameter :: ref_error_message = ""
        error%haserror = in_error_haserror
        error%message = in_error_message
        call ops_vertdisp( varin, z0, zi, ol, uster, hh, x, ircp, istab, debugnam, uh, zu, sz, error)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_sz, sz, tol, "sz", __LINE__, __FILE__)
        if (error%haserror) call write_error(IOB_STDOUT,error)
        call assertFalse( error%haserror, "ops_vertdisp", __LINE__, __FILE__)
   end subroutine test_ops_vertdisp
end module m_test_ops_vertdisp
 
program p_test_ops_vertdisp
use m_test_ops_vertdisp
use no_pfunit

implicit none
   call test_ops_vertdisp()
   call test_ops_vertdisp2()
   call test_ops_vertdisp3()
   call test_ops_vertdisp4()
   call conclusion()
end program p_test_ops_vertdisp

