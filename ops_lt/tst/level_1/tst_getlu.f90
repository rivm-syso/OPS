module m_test_getlu
implicit none
contains
   subroutine test_getlu()
   use m_aps
   use m_log_call
   use m_ops_getlu
   use m_commonconst_lt
   use m_commonfile
   use m_error
   use no_pfunit
       real, parameter :: tol = 1e-5
        real, parameter :: xr =   2.32425000E+05
        real, parameter :: yr =   6.13575000E+05
        real, parameter :: xr_far = 35e5, yr_far = 35e5
        logical, parameter :: in_trajectory = .true.
        type(tApsGridInt) :: lugrid
        INTEGER   :: landuse(NLU+1)
        INTEGER,   parameter :: ref_landuse(NLU+1) =  (/ 6, 0, 0, 0, 0, 0, 100, 0, 0, 0/) 
        INTEGER,   parameter :: ref_landuse_far(NLU+1) = (/ 1, 100, 0, 0, 0, 0, 0, 0, 0, 0/)
        type(TError) :: error
        logical, parameter :: ref_error_haserror = .false.
        character(len=*), parameter :: ref_error_message = ""
        call read_aps_integer("./level_1/resources/apsgrid_33009_2.int", "lugrid", lugrid, error)
        call assertFalse(error%haserror, "reading apsgrid_33009_2.int")

        call ops_getlu( xr, yr, in_trajectory, lugrid, landuse, error)
        call assertEqual( ref_landuse, landuse, "landuse", __LINE__, __FILE__)
        if (error%haserror) call write_error(IOB_STDOUT,error)
        call assertFalse( error%haserror, "getlu", __LINE__, __FILE__)

        call ops_getlu( xr_far, yr_far, in_trajectory, lugrid, landuse, error)
        call assertEqual( ref_landuse_far, landuse, "landuse", __LINE__, __FILE__)
        if (error%haserror) call write_error(IOB_STDOUT,error)
        call assertFalse( error%haserror, "getlu", __LINE__, __FILE__)

        lugrid%value = 0
        call ops_getlu( xr, yr, in_trajectory, lugrid, landuse, error)
        call print_array("landuse",landuse,"")
        call assertEqual( ref_landuse_far, landuse, "landuse", __LINE__, __FILE__)
        if (error%haserror) call write_error(IOB_STDOUT,error)
        call assertFalse( error%haserror, "getlu", __LINE__, __FILE__)

   end subroutine test_getlu
end module m_test_getlu
 
program p_test_getlu
use m_test_getlu
use no_pfunit
implicit none
   call test_getlu()
   call conclusion()
end program p_test_getlu

