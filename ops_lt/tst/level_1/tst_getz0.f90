module m_test_ops_getz0
implicit none
contains
   subroutine test_ops_getz0()
   use m_aps
   use m_ops_getz0
   use m_commonfile
   use m_error
   use no_pfunit
       real, parameter :: tol = 1e-5
        real, parameter :: xr =   2.29500000E+05
        real, parameter :: yr =   6.19500000E+05
        real, parameter :: xr_far = 35e5, yr_far = 35e5
        
        ! real, parameter :: xr_very_far = 55e6, yr_very_far = 55e6
        type(tApsGridInt) :: z0nlgrid
        type(tApsGridInt) :: z0eurgrid
        real :: z0
        real, parameter :: ref_z0 =   1.00000005E-03, ref_z0_very_far =  0.150000005960
        real, parameter :: ref_z0_far = 0.003  ! very low value

        type(TError) :: error
        logical, parameter :: ref_error_haserror = .false.
        character(len=*), parameter :: ref_error_message = ""
        call read_aps_integer("./level_1/resources/apsgrid_139022_101.int", "z0nlgrid", z0nlgrid, error)
        call assertFalse(error%haserror, "reading apsgrid_139022_101.int")
        call read_aps_integer("./level_1/resources/apsgrid_139022_102.int", "z0eurgrid", z0eurgrid, error)
        call assertFalse(error%haserror, "reading apsgrid_139022_102.int")
        z0eurgrid%value = z0eurgrid%value * 2

        z0 = -1.0
        call ops_getz0( xr, yr, z0nlgrid, z0eurgrid, z0, error)
        call assertEqual( ref_z0, z0, tol, "z0", __LINE__, __FILE__)
        if (error%haserror) call write_error(IOB_STDOUT,error)
        call assertFalse( error%haserror, "ops_getz0", __LINE__, __FILE__)

        z0 = -1.0
        call ops_getz0( xr_far, yr_far, z0nlgrid, z0eurgrid, z0, error)
        call assertEqual( 2*ref_z0, z0, tol, "z0", __LINE__, __FILE__)
        if (error%haserror) call write_error(IOB_STDOUT,error)
        call assertFalse( error%haserror, "ops_getz0", __LINE__, __FILE__)

        
        !z0 = -1.0
        !call ops_getz0( xr_very_far, yr_very_far, z0nlgrid, z0eurgrid, z0, error)
        !call assertEqual( ref_z0_very_far, z0, tol, "z0", __LINE__, __FILE__)
        !if (error%haserror) call write_error(IOB_STDOUT,error)
        !call assertFalse( error%haserror, "ops_getz0", __LINE__, __FILE__)

        z0eurgrid%value = 0
        z0nlgrid%value = 0

        z0 = -1.0
        call ops_getz0( xr_far, yr_far, z0nlgrid, z0eurgrid, z0, error)
        call assertEqual( ref_z0_far, z0, tol, "z0", __LINE__, __FILE__)
        if (error%haserror) call write_error(IOB_STDOUT,error)
        call assertFalse( error%haserror, "ops_getz0", __LINE__, __FILE__)
   end subroutine test_ops_getz0
end module m_test_ops_getz0
 
program p_test_ops_getz0
use m_test_ops_getz0
use no_pfunit
implicit none
   call test_ops_getz0()
   call conclusion()
end program p_test_ops_getz0
