
module m_test_test_log_call
implicit none
contains
   subroutine test_test_log_call()
   use no_pfunit_ops_lib
   use m_aps, only: TApsGridReal
   use m_aps, only: TApsGridInt
   use m_error, only: TError
       integer         :: log_call_stat, i1, i2, i3, i4, i5, fid
       real, parameter :: tol = 1e-5
        type(TApsGridReal) :: input_apsgrid_real
        type(TApsGridReal) :: output_apsgrid_real
        type(TApsGridReal) :: ref_output_apsgrid_real
        type(TApsGridInt) :: input_apsgrid_real
        type(TApsGridInt) :: output_apsgrid_real
        type(TApsGridInt) :: ref_output_apsgrid_real
        type(TError) :: input_error_real
        type(TError) :: output_error_real
        type(TError) :: ref_output_error_real
        call InitAps("apsgrid_pid_1.real", "InitApsReal", input_apsgrid_real, __LINE__, __FILE__)
        call InitAps("apsgrid_pid_2.real", "InitApsReal", ref_output_apsgrid_real, __LINE__, __FILE__)
        call InitAps("apsgrid_pid_3.real", "InitApsReal", inout_apsgrid_real, __LINE__, __FILE__)
        call InitAps("apsgrid_pid_1.int", "InitApsInt", input_apsgrid_real, __LINE__, __FILE__)
        call InitAps("apsgrid_pid_2.int", "InitApsInt", ref_output_apsgrid_real, __LINE__, __FILE__)
        call InitAps("apsgrid_pid_3.int", "InitApsInt", inout_apsgrid_real, __LINE__, __FILE__)
        input_error_real%haserror = .true.
        input_error_real%message = "need positive number of rows and columns in APS header"
        ref_output_error_real%haserror = .true.
        ref_output_error_real%message = "need positive number of rows and columns in APS header"
        inout_error_real%haserror = .true.
        inout_error_real%message = "need positive number of rows and columns in APS header"
        call test_log_call( input_apsgrid_real, output_apsgrid_real, input_apsgrid_real, output_apsgrid_real, &
            input_error_real, output_error_real,)
        call assertEqual(output_apsgrid_real, ref_output_apsgrid_real, tol, "output_apsgrid_real", __LINE__, __FILE__)
        call assertEqual(output_apsgrid_real, ref_output_apsgrid_real, tol, "output_apsgrid_real", __LINE__, __FILE__)
        call assertEqual(output_error_real, ref_output_error_real, "test_log_call", __LINE__, __FILE__)
   end subroutine test_test_log_call
end module m_test_test_log_call

program p_test_test_log_call
use m_test_test_log_call
use no_pfunit
implicit none
   call test_test_log_call()
   call conclusion()
end program p_test_test_log_call
