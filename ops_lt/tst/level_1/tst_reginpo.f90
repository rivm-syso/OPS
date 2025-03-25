module m_test_reginpo
implicit none
contains
   subroutine test_reginpo()
   use m_ops_rcp_char_1
   use no_pfunit_ops_lt
   use m_error, only: TError
       integer         :: log_call_stat, i1, i2, i3, i4, i5, fid
       real, parameter :: tol = 1e-5
        real, parameter :: x =   5.38750982E+00, x_far = 5e4
        real, parameter :: y =   5.14554710E+01, y_far = 5e4
        real, allocatable :: cs(:,:,:,:,:)
        real :: z0_metreg(6) = (/ &
             1.09999999E-01,   2.39999995E-01,   2.09999993E-01,   1.00000001E-01,   2.89999992E-01,   2.89999992E-01/)
        real :: xreg(6) = (/ &
             5.61000013E+00,   4.44000006E+00,   6.77999973E+00,   3.83999991E+00,   5.30000019E+00,   6.40000010E+00/)
        real :: yreg(6) = (/ &
             5.31100006E+01,   5.22200012E+01,   5.24000015E+01,   5.15400009E+01,   5.19500008E+01,   5.12200012E+01/)
        real :: z0_metreg_xy
        real, parameter :: ref_z0_metreg_xy =   2.82533765E-01
        real :: uurtot
        real, parameter :: ref_uurtot =   8.75660156E+03
        real, allocatable :: astat(:,:,:,:)
        real, allocatable :: ref_astat(:,:,:,:)
        type(TError) :: error
        type(TError) :: ref_error
        allocate(cs(4,27,6,12,6), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'cs'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/real5d_86759_7.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'real5d_86759_7.real' for reading variable cs", __LINE__, __FILE__)
        do i5 = 1,6
           do i4 = 1,12
              do i3 = 1,6
                 do i2 = 1,27
                    do i1 = 1,4
                       read(fid,*) cs(i1, i2, i3, i4, i5)
                    end do
                 end do
              end do
           end do
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'real5d_86759_7.real' for reading variable cs", __LINE__, __FILE__)
        allocate(ref_astat(4,27,6,12), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'ref_astat'", __LINE__, __FILE__)
        allocate(astat(4,27,6,12), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'astat'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/real4d_86759_5.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'real4d_86759_5.real' for reading variable astat", __LINE__, __FILE__)
        do i4 = 1,12
           do i3 = 1,6
              do i2 = 1,27
                 do i1 = 1,4
                    read(fid,*) ref_astat(i1, i2, i3, i4)
                 end do
              end do
           end do
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'real4d_86759_5.real' for reading variable astat", __LINE__, __FILE__)
        ref_error%haserror = .false.
        ref_error%message = ""
        call reginpo( x, y, cs, z0_metreg, xreg, yreg, z0_metreg_xy, uurtot, astat, error)
        call assertEqual( ref_z0_metreg_xy, z0_metreg_xy, tol, "z0_metreg_xy", __LINE__, __FILE__)
        call assertEqual( ref_uurtot, uurtot, tol, "uurtot", __LINE__, __FILE__)
        call assertEqual( ref_astat, astat, 10*tol, "astat", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "reginpo", __LINE__, __FILE__)

   end subroutine test_reginpo
end module m_test_reginpo

program p_test_reginpo
use m_test_reginpo
use no_pfunit
implicit none
   call test_reginpo()
   call conclusion()
end program p_test_reginpo

