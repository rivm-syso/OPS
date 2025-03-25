module m_test_ops_statfil
implicit none
contains
   subroutine test_ops_statfil()
   use m_commonfile, only: kname
   use m_ops_read_meteo
   use no_pfunit_ops_lt
   use m_error, only: TError
   use m_ops_varin, only: Tvarin_unc
       integer         :: log_call_stat, i1, i2, i3, i4, i5, fid
       real, parameter :: tol = 1e-5
       type(Tvarin_unc) :: varin_unc
        integer :: jb
        integer, parameter :: ref_jb = 5
        integer :: mb
        integer, parameter :: ref_mb = 1
        integer :: idb
        integer, parameter :: ref_idb = 1
        integer :: jt
        integer, parameter :: ref_jt = 6
        integer :: mt
        integer, parameter :: ref_mt = 1
        integer :: idt
        integer, parameter :: ref_idt = 1
        real :: uurtot
        real, parameter :: ref_uurtot =   8.75600000E+03
        integer :: iseiz
        integer, parameter :: ref_iseiz = 1
        integer :: iline
        real :: zf
        real, parameter :: ref_zf =   5.00000000E-01
        real, allocatable :: astat(:,:,:,:)
        real, allocatable :: ref_astat(:,:,:,:)
        real :: trafst(4)
        real :: ref_trafst(4) = (/ &
             0.00000000E+00,   1.00000000E+05,   3.00000000E+05,   1.00000000E+06/)
        real, allocatable :: cs(:,:,:,:,:)
        real, allocatable :: ref_cs(:,:,:,:,:)
        real :: rainreg(6)
        real :: ref_rainreg(6) = (/ &
             9.41454768E-02,   8.75713378E-02,   8.82915929E-02,   9.66036692E-02,   8.84507746E-02,   7.63911754E-02/)
        real :: z0_metreg(6)
        real :: ref_z0_metreg(6) = (/ &
             1.09999999E-01,   2.39999995E-01,   2.09999993E-01,   1.00000001E-01,   2.89999992E-01,   2.89999992E-01/)
        real :: xreg(6)
        real :: ref_xreg(6) = (/ &
             5.61000013E+00,   4.44000006E+00,   6.77999973E+00,   3.83999991E+00,   5.30000019E+00,   6.40000010E+00/)
        real :: yreg(6)
        real :: ref_yreg(6) = (/ &
             5.31100006E+01,   5.22200012E+01,   5.24000015E+01,   5.15400009E+01,   5.19500008E+01,   5.12200012E+01/)
        real :: hourreg(6)
        real :: ref_hourreg(6) = (/ &
             8.76000000E+03,   8.75800000E+03,   8.75100000E+03,   8.75900000E+03,   8.75600000E+03,   8.75700000E+03/)
        type(TError) :: error
        type(TError) :: ref_error
        allocate(ref_astat(4,27,6,12), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'ref_astat'", __LINE__, __FILE__)
        allocate(astat(4,27,6,12), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'astat'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/real4d_29934_1.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'real4d_29934_1.real' for reading variable astat", __LINE__, __FILE__)
        iline = 0
        do i4 = 1,12
           do i3 = 1,6
              do i2 = 1,27
                 do i1 = 1,4
                    iline  = iline + 1
                    read(fid,*) ref_astat(i1, i2, i3, i4)
                    if (i1==1 .and. i2==27 .and. i3==5 .and. i4==5) then
                        print *,'just read from line ',iline,': ref_astat = ',ref_astat(1,27,5,5)
                    end if
                 end do
              end do
           end do
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'real4d_29934_1.real' for reading variable astat", __LINE__, __FILE__)
        allocate(ref_cs(4,27,6,12,6), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'ref_cs'", __LINE__, __FILE__)
        allocate(cs(4,27,6,12,6), stat=log_call_stat)
        call AssertEqual(log_call_stat,0,"allocation of 'cs'", __LINE__, __FILE__)
        open(newunit=fid, file="./level_1/resources/real5d_29934_1.real", status="old", action="read", iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"opening 'real5d_29934_1.real' for reading variable cs", __LINE__, __FILE__)
        do i5 = 1,6
           do i4 = 1,12
              do i3 = 1,6
                 do i2 = 1,27
                    do i1 = 1,4
                       read(fid,*) ref_cs(i1, i2, i3, i4, i5)
                    end do
                 end do
              end do
           end do
        end do
        close(unit=fid, iostat=log_call_stat)
        call AssertEqual(log_call_stat,0,"closing 'real5d_29934_1.real' for reading variable cs", __LINE__, __FILE__)
        ref_error%haserror = .false.
        ref_error%message = ""
        kname = '../data/GCN2022/Meteo/a005105c.005'
        cs = 0
        astat = 0
        call ops_statfil(varin_unc, jb, mb, idb, jt, mt, idt, uurtot, iseiz, zf, astat, trafst, cs, rainreg, z0_metreg, &
            xreg, yreg, hourreg, error)
        ! call assertEqual(error, ref_error, "ops_statfil", __LINE__, __FILE__)
        call assertEqual( ref_jb, jb, "jb", __LINE__, __FILE__)
        call assertEqual( ref_mb, mb, "mb", __LINE__, __FILE__)
        call assertEqual( ref_idb, idb, "idb", __LINE__, __FILE__)
        call assertEqual( ref_jt, jt, "jt", __LINE__, __FILE__)
        call assertEqual( ref_mt, mt, "mt", __LINE__, __FILE__)
        call assertEqual( ref_idt, idt, "idt", __LINE__, __FILE__)
        call assertEqual( ref_uurtot, uurtot, tol, "uurtot", __LINE__, __FILE__)
        call assertEqual( ref_iseiz, iseiz, "iseiz", __LINE__, __FILE__)
        call assertEqual( ref_zf, zf, tol, "zf", __LINE__, __FILE__)
        call assertEqual_array_4d( ref_astat, astat, 1000*tol, "astat", __LINE__, __FILE__)
        call assertEqual( ref_trafst, trafst, tol, "trafst", __LINE__, __FILE__)
        call assertEqual_array_5d( ref_cs, cs, 100*tol, "cs", __LINE__, __FILE__)
        call assertEqual( ref_rainreg, rainreg, tol, "rainreg", __LINE__, __FILE__)
        call assertEqual( ref_z0_metreg, z0_metreg, tol, "z0_metreg", __LINE__, __FILE__)
        call assertEqual( ref_xreg, xreg, tol, "xreg", __LINE__, __FILE__)
        call assertEqual( ref_yreg, yreg, tol, "yreg", __LINE__, __FILE__)
        call assertEqual( ref_hourreg, hourreg, tol, "hourreg", __LINE__, __FILE__)
   end subroutine test_ops_statfil
end module m_test_ops_statfil

program p_test_ops_statfil
use m_test_ops_statfil
use no_pfunit
implicit none
   call test_ops_statfil()
   call conclusion()
end program p_test_ops_statfil
