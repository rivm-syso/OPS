module m_test_ops_getlu_tra
implicit none
contains
   subroutine test_ops_getlu_tra()
   use m_log_call
   use m_ops_getlu_tra
   use m_commonfile
   use m_error
   use m_aps
   use no_pfunit
       real, parameter :: tol = 1e-5
        real, parameter :: xr =   2.37500000E+05
        real, parameter :: yr =   6.21500000E+05
        real, parameter :: xb =   1.55000000E+05
        real, parameter :: yb =   4.63000000E+05
        type(tApsGridInt) :: lugrid
        logical, parameter :: domlu = .false.
        real :: lu_tra_per(9)
        real, parameter :: ref_lu_tra_per(9) = (/   3.43333321E+01,   1.96190472E+01,   2.85714269E+00, &
             4.28571403E-01,   6.42857122E+00,   2.47619038E+01,   8.52380943E+00,   3.04761887E+00,   0.00000000E+00/)

        real, parameter :: lu_tra_per_domlu(9) = (/ 4.76190472, 0., 0., 0., 0., 0., 0., 0., 0./)

        type(TError) :: error
        logical, parameter :: ref_error_haserror = .false.
        character(len=*), parameter :: ref_error_message = ""
        call read_aps_integer("./level_1/resources/apsgrid_62317_2.int", "lugrid", lugrid, error)
        call assertFalse(error%haserror, "reading apsgrid_62317_2.int")

        call ops_getlu_tra( xr, yr, xb, yb, lugrid, domlu, lu_tra_per, error)
        call assertEqual( ref_lu_tra_per, lu_tra_per, tol, "lu_tra_per", __LINE__, __FILE__)
        if (error%haserror) call write_error(IOB_STDOUT,error)
        call assertFalse( error%haserror, "ops_getlu_tra", __LINE__, __FILE__)

        call ops_getlu_tra( xr, yr, xb, yb, lugrid, .not.domlu, lu_tra_per, error)
        call print_array("lu_tra_per", lu_tra_per,"")
        call assertEqual( lu_tra_per_domlu, lu_tra_per, tol, "lu_tra_per", __LINE__, __FILE__)
        if (error%haserror) call write_error(IOB_STDOUT,error)
        call assertFalse( error%haserror, "ops_getlu_tra", __LINE__, __FILE__)

   end subroutine test_ops_getlu_tra
end module m_test_ops_getlu_tra
 
program p_test_ops_getlu_tra
use m_test_ops_getlu_tra
use no_pfunit
implicit none
   call test_ops_getlu_tra()
   call conclusion()
end program p_test_ops_getlu_tra
