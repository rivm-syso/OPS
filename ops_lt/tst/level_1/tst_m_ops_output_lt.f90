module m_test_m_ops_output_lt
implicit none
contains
   subroutine test_ops_parout_fill()
   use m_ops_output_lt
   use no_pfunit_ops_lt
       real, parameter :: tol = 1e-5
        integer, parameter :: nparout = 4
        real, parameter :: ra_rcp_4 =   2.43975487E+01
        real, parameter :: rb_rcp =   3.01502495E+01
        real, parameter :: rc_eff_rcp_4 =   1.30000000E+01
        real, parameter :: percvk =   6.67061238E-03
        real, parameter :: in_parout_val(4) = (/   1.00000000E+00,   2.00000000E+00,   3.00000000E+00, &
             4.00000000E+00/)
        real :: parout_val(4)
        real, parameter :: ref_parout_val(4) = (/   1.00027347E+00,   2.00022125E+00,   3.00051308E+00, &
             4.02001190E+00/)
        character(len=13) :: parout_name(4)
        character(len=13), parameter :: ref_parout_name(4) = (/ &
        "1/Ra,z=4m    ", &
        "1/Rb         ", &
        "1/Rc_eff,z=4m", &
        "check = 3    " /)
        character(len=3) :: parout_unit(4)
        character(len=3), parameter :: ref_parout_unit(4) = (/ &
        "m/s", &
        "m/s", &
        "m/s", &
        "-  " /)
        parout_val = in_parout_val
        call ops_parout_fill( nparout, ra_rcp_4, rb_rcp, rc_eff_rcp_4, percvk, parout_val, parout_name, &
            parout_unit)
        call assertEqual( ref_parout_val, parout_val, tol, "parout_val", __LINE__, __FILE__)
        call assertEqual( ref_parout_name, parout_name, "parout_name", __LINE__, __FILE__)
        call assertEqual( ref_parout_unit, parout_unit, "parout_unit", __LINE__, __FILE__)
   end subroutine test_ops_parout_fill
end module m_test_m_ops_output_lt
 
program p_test_ops_parout_fill
use m_test_m_ops_output_lt
use no_pfunit
implicit none
   call test_ops_parout_fill()
   call conclusion()
end program p_test_ops_parout_fill

