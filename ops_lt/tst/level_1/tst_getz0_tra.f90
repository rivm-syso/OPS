module m_test_ops_getz0_tra
implicit none
contains
   subroutine test_ops_getz0_tra()
   use m_ops_getz0_tra
   use m_commonfile
   use m_error
   use m_aps
   use no_pfunit
       real, parameter :: tol = 1e-5
        real, parameter :: xr =   2.18500000E+05
        real, parameter :: yr =   6.20500000E+05
        real, parameter :: xb =   1.55000000E+05
        real, parameter :: yb =   4.63000000E+05
        type(tApsGridInt) :: z0nlgrid
        type(tApsGridInt) :: z0eurgrid
        real :: z0_tra
        real, parameter :: ref_z0_tra =   7.38451555E-02
        type(TError) :: error
        logical, parameter :: ref_error_haserror = .false.
        character(len=*), parameter :: ref_error_message = ""
        call read_aps_integer("./level_1/resources/apsgrid_160565_17.int", "z0nlgrid", z0nlgrid, error)
        call assertFalse(error%haserror, "reading apsgrid_160565_17.int")
        call read_aps_integer("./level_1/resources/apsgrid_160565_18.int", "z0eurgrid", z0eurgrid, error)
        call assertFalse(error%haserror, "reading apsgrid_160565_18.int")
        call ops_getz0_tra( xr, yr, xb, yb, z0nlgrid, z0eurgrid, z0_tra, error)
        call assertEqual( ref_z0_tra, z0_tra, tol, "z0_tra", __LINE__, __FILE__)
        if (error%haserror) call write_error(IOB_STDOUT,error)
        call assertFalse( error%haserror, "ops_getz0_tra", __LINE__, __FILE__)
   end subroutine test_ops_getz0_tra
end module m_test_ops_getz0_tra
 
program p_test_ops_getz0_tra
use m_test_ops_getz0_tra
use no_pfunit
implicit none
   call test_ops_getz0_tra()
   call conclusion()
end program p_test_ops_getz0_tra
