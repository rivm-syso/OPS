
module m_test_test_log_call
implicit none
contains
   subroutine test_test_log_call()
   use no_pfunit
       integer         :: log_call_stat, i1, i2, i3, i4, i5, fid
       real, parameter :: tol = 1e-5
        integer, parameter :: input_int0 = 6
        integer :: input_int1(10) = (/ &
           7, 7, 7, 7, 7, 7, 7, 7, 7, 7/)
        integer :: input_int2(2,3) = reshape( (/ &
           8, 8, 8, 8, 8, 8/), &
          (/2,3/) )
        integer :: output_int0
        integer, parameter :: ref_output_int0 = 6
        integer :: output_int1(10)
        integer :: ref_output_int1(10) = (/ &
           7, 7, 7, 7, 7, 7, 7, 7, 7, 7/)
        integer :: output_int2(2,3)
        integer :: ref_output_int2(2,3) = reshape( (/ &
           8, 8, 8, 8, 8, 8/), &
          (/2,3/) )
        integer, parameter :: in_inout_int0 = 6
        integer :: in_inout_int1(10) = (/ &
           7, 7, 7, 7, 7, 7, 7, 7, 7, 7/)
        integer :: in_inout_int2(2,3) = reshape( (/ &
           8, 8, 8, 8, 8, 8/), &
          (/2,3/) )
        real, parameter :: in_inout_real0 =   2.00000000E+00
        real :: in_inout_real1(8) = (/ &
             1.00000000E+00,   1.00000000E+00,   1.00000000E+00,   1.00000000E+00,   1.00000000E+00,   1.00000000E+00, &
             1.00000000E+00,   1.00000000E+00/)
        real :: in_inout_real2(3,2) = reshape( (/ &
             2.00000000E+00,   2.00000000E+00,   2.00000000E+00,   2.00000000E+00,   2.00000000E+00,   2.00000000E+00/), &
          (/3,2/) )
        real :: in_inout_real3(2,2,3) = reshape( (/ &
             2.62500000E+00,   2.62500000E+00,   2.62500000E+00,   2.62500000E+00,   2.62500000E+00,   2.62500000E+00, &
             2.62500000E+00,   2.62500000E+00,   2.62500000E+00,   2.62500000E+00,   2.62500000E+00,   2.62500000E+00/), &
          (/2,2,3/) )
        real :: in_inout_real4(1,2,1,1) = reshape( (/ &
             1.25000000E+00,   1.25000000E+00/), &
          (/1,2,1,1/) )
        real :: in_inout_real5(1,2,1,2,1) = reshape( (/ &
             2.50000000E+00,   2.50000000E+00,   2.50000000E+00,   2.50000000E+00/), &
          (/1,2,1,2,2/) )
        real :: output_real0
        real, parameter :: ref_output_real0 =   2.00000000E+00
        real :: output_real1(8)
        real :: ref_output_real1(8) = (/ &
             1.00000000E+00,   1.00000000E+00,   1.00000000E+00,   1.00000000E+00,   1.00000000E+00,   1.00000000E+00, &
             1.00000000E+00,   1.00000000E+00/)
        real :: output_real2(3,2)
        real :: ref_output_real2(3,2) = reshape( (/ &
             2.00000000E+00,   2.00000000E+00,   2.00000000E+00,   2.00000000E+00,   2.00000000E+00,   2.00000000E+00/), &
          (/3,2/) )
        real :: output_real3(2,2,3)
        real :: ref_output_real3(2,2,3) = reshape( (/ &
             2.62500000E+00,   2.62500000E+00,   2.62500000E+00,   2.62500000E+00,   2.62500000E+00,   2.62500000E+00, &
             2.62500000E+00,   2.62500000E+00,   2.62500000E+00,   2.62500000E+00,   2.62500000E+00,   2.62500000E+00/), &
          (/2,2,3/) )
        real :: output_real4(1,2,1,1)
        real :: ref_output_real4(1,2,1,1) = reshape( (/ &
             1.25000000E+00,   1.25000000E+00/), &
          (/1,2,1,1/) )
        real :: output_real5(1,2,1,2,1)
        real :: ref_output_real5(1,2,1,2,1) = reshape( (/ &
             2.50000000E+00,   2.50000000E+00,   2.50000000E+00,   2.50000000E+00/), &
          (/1,2,1,2,2/) )
        real, parameter :: input_real0 =   2.00000000E+00
        real :: input_real1(8) = (/ &
             1.00000000E+00,   1.00000000E+00,   1.00000000E+00,   1.00000000E+00,   1.00000000E+00,   1.00000000E+00, &
             1.00000000E+00,   1.00000000E+00/)
        real :: input_real2(3,2) = reshape( (/ &
             2.00000000E+00,   2.00000000E+00,   2.00000000E+00,   2.00000000E+00,   2.00000000E+00,   2.00000000E+00/), &
          (/3,2/) )
        real :: input_real3(2,2,3) = reshape( (/ &
             2.62500000E+00,   2.62500000E+00,   2.62500000E+00,   2.62500000E+00,   2.62500000E+00,   2.62500000E+00, &
             2.62500000E+00,   2.62500000E+00,   2.62500000E+00,   2.62500000E+00,   2.62500000E+00,   2.62500000E+00/), &
          (/2,2,3/) )
        real :: input_real4(1,2,1,1) = reshape( (/ &
             1.25000000E+00,   1.25000000E+00/), &
          (/1,2,1,1/) )
        real :: input_real5(1,2,1,2,1) = reshape( (/ &
             2.50000000E+00,   2.50000000E+00,   2.50000000E+00,   2.50000000E+00/), &
          (/1,2,1,2,2/) )
        logical :: out_bool0
        logical, parameter :: ref_out_bool0 = .false.
        logical :: out_bool1(3)
        real :: ref_out_bool1(3) = (/ &
           .true., .true., .true./)
        logical, parameter :: in_bool0 = .false.
        real :: in_bool1(3) = (/ &
           .true., .true., .true./)
        logical, parameter :: in_inout_bool0 = .false.
        real :: in_inout_bool1(3) = (/ &
           .true., .true., .true./)
        character(len=200) :: out_string0
        character(len=*), parameter :: ref_out_string0 = "OPS-lib"
        character(len=6) :: out_string1(3)
        character(len=6) :: ref_out_string1(3) = (/ &
        "OPS-lt", &
        "OPS-lt", &
        "OPS-lt" /)
        character(len=*), parameter :: in_string0 = "OPS-lib"
        character(len=6) :: in_string1(3) = (/ &
        "OPS-lt", &
        "OPS-lt", &
        "OPS-lt" /)
        character(len=*), parameter :: in_inout_string0 = "OPS-lib"
        character(len=6) :: in_inout_string1(3) = (/ &
        "OPS-lt", &
        "OPS-lt", &
        "OPS-lt" /)
        double precision :: out_double0
        double precision, parameter :: ref_out_double0 =   5.25000000E+00
        double precision, parameter :: in_double0 =   5.25000000E+00
        double precision, parameter :: in_inout_double0 =   5.25000000E+00
        integer*2 :: out_short0(2)
        integer :: ref_out_short0(2) = (/ &
           5, 5/)
        integer :: in_short0(2) = (/ &
           5, 5/)
        integer :: in_inout_short0(2) = (/ &
           5, 5/)
        inout_int0 = in_inout_int0
        inout_int1 = in_inout_int1
        inout_int2 = in_inout_int2
        inout_real0 = in_inout_real0
        inout_real1 = in_inout_real1
        inout_real2 = in_inout_real2
        inout_real3 = in_inout_real3
        inout_real4 = in_inout_real4
        inout_real5 = in_inout_real5
        inout_bool0 = in_inout_bool0
        inout_bool1 = in_inout_bool1
        inout_string0 = in_inout_string0
        inout_string1 = in_inout_string1
        inout_double0 = in_inout_double0
        inout_short0 = in_inout_short0
        call test_log_call( input_int0, input_int1, input_int2, output_int0, output_int1, output_int2, &
            output_real0, output_real1, output_real2, output_real3, output_real4, output_real5, input_real0, &
            input_real1, input_real2, input_real3, input_real4, input_real5, out_bool0, out_bool1, in_bool0, &
            in_bool1, out_string0, out_string1, in_string0, in_string1, out_double0, in_double0, out_short0, &
            in_short0,)
        call assertEqual( ref_output_int0, output_int0, "output_int0", __LINE__, __FILE__)
        call assertEqual( ref_output_int1, output_int1, "output_int1", __LINE__, __FILE__)
        call assertEqual( ref_output_int2, output_int2, "output_int2", __LINE__, __FILE__)
        call assertEqual( ref_output_real0, output_real0, tol, "output_real0", __LINE__, __FILE__)
        call assertEqual( ref_output_real1, output_real1, tol, "output_real1", __LINE__, __FILE__)
        call assertEqual( ref_output_real2, output_real2, tol, "output_real2", __LINE__, __FILE__)
        call assertEqual( ref_output_real3, output_real3, tol, "output_real3", __LINE__, __FILE__)
        call assertEqual( ref_output_real4, output_real4, tol, "output_real4", __LINE__, __FILE__)
        call assertEqual( ref_output_real5, output_real5, tol, "output_real5", __LINE__, __FILE__)
        call assertEqual( ref_out_bool0, out_bool0, "out_bool0", __LINE__, __FILE__)
        call assertEqual( ref_out_bool1, out_bool1, "out_bool1", __LINE__, __FILE__)
        call assertEqual( ref_out_string0, out_string0, tol, "out_string0", __LINE__, __FILE__)
        call assertEqual( ref_out_string1, out_string1, "out_string1", __LINE__, __FILE__)
        call assertEqual( ref_out_double0, out_double0, tol, "out_double0", __LINE__, __FILE__)
        call assertEqual( ref_out_short0, out_short0, tol, "out_short0", __LINE__, __FILE__)
   end subroutine test_test_log_call
end module m_test_test_log_call

program p_test_test_log_call
use m_test_test_log_call
use no_pfunit
implicit none
   call test_test_log_call()
   call conclusion()
end program p_test_test_log_call
