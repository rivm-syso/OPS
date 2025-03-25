module m_test_ops_readstexp
implicit none
contains
   subroutine test_ops_readstexp()
   use m_commonconst_lib, only: NTRAJ, NCOMP, NSTAB, NSEK
   use m_ops_read_meteo
   use m_astat
   use m_commonfile
   use m_error
   use m_ops_varin, only: Tvarin_unc
   use no_pfunit

       real, parameter :: tol = 1e-5
        character(len=*), parameter :: nfile = "../data/GCN2022/Meteo/a005105c.005"
        type(Tvarin_unc) :: varin_unc
        integer :: jb
        integer, parameter :: ref_jb = 5
        integer :: mb
        integer, parameter :: ref_mb = 1
        integer :: idb
        integer, parameter :: ref_idb = 1
        real :: gemre
        real, parameter :: ref_gemre =   8.84507746E-02
        integer :: iyr
        integer, parameter :: ref_iyr = 17
        integer :: imon
        integer, parameter :: ref_imon = 1
        integer :: iday
        integer, parameter :: ref_iday = 1
        real :: xpos
        real, parameter :: ref_xpos =   5.30000019E+00
        real :: ypos
        real, parameter :: ref_ypos =   5.19500008E+01
        real :: z0_metreg1
        real, parameter :: ref_z0_metreg1 =   2.89999992E-01
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
        real :: zf
        real, parameter :: ref_zf =   5.00000000E-01
        real, allocatable :: ref_astat(:,:,:,:), astat(:,:,:,:)
        real :: trafst(4)
        real, parameter :: ref_trafst(4) = (/   0.00000000E+00,   1.00000000E+05,   3.00000000E+05,   1.00000000E+06/)
        type(TError) :: error
        logical, parameter :: ref_error_haserror = .false.
        character(len=*), parameter :: ref_error_message = ""
        character(len=500) :: filename
        integer :: isek, istab
        filename  = './level_1/resources/astat_after_readstexp.data'
        call read_astat(ref_astat, filename)
        allocate(astat(NTRAJ, NCOMP, NSTAB, NSEK))
        call ops_readstexp( nfile, varin_unc, jb, mb, idb, gemre, iyr, imon, iday, xpos, ypos, z0_metreg1, jt, mt, &
            idt, uurtot, iseiz, zf, astat, trafst, error)
        if (.not. error%haserror) then
           call assertEqual( ref_jb, jb, "jb", __LINE__, __FILE__)
           call assertEqual( ref_mb, mb, "mb", __LINE__, __FILE__)
           call assertEqual( ref_idb, idb, "idb", __LINE__, __FILE__)
           call assertEqual( ref_gemre, gemre, tol, "gemre", __LINE__, __FILE__)
           call assertEqual( ref_iyr, iyr, "iyr", __LINE__, __FILE__)
           call assertEqual( ref_imon, imon, "imon", __LINE__, __FILE__)
           call assertEqual( ref_iday, iday, "iday", __LINE__, __FILE__)
           call assertEqual( ref_xpos, xpos, tol, "xpos", __LINE__, __FILE__)
           call assertEqual( ref_ypos, ypos, tol, "ypos", __LINE__, __FILE__)
           call assertEqual( ref_z0_metreg1, z0_metreg1, tol, "z0_metreg1", __LINE__, __FILE__)
           call assertEqual( ref_jt, jt, "jt", __LINE__, __FILE__)
           call assertEqual( ref_mt, mt, "mt", __LINE__, __FILE__)
           call assertEqual( ref_idt, idt, "idt", __LINE__, __FILE__)
           call assertEqual( ref_uurtot, uurtot, tol, "uurtot", __LINE__, __FILE__)
           call assertEqual( ref_iseiz, iseiz, "iseiz", __LINE__, __FILE__)
           call assertEqual( ref_zf, zf, tol, "zf", __LINE__, __FILE__)
           call assertEqual( ref_trafst, trafst, tol, "trafst", __LINE__, __FILE__)
           do isek = 1,NSEK
              do istab = 1,NSTAB
                 call assertEqual( ref_astat(:,:,istab,isek), astat(:,:,istab,isek), &
                                   100*tol, "astat", __LINE__, __FILE__)
              end do
           end do
        else
           call write_error(IOB_STDOUT,error)
        endif
        call assertFalse( error%haserror, "ops_readstexp", __LINE__, __FILE__)
   end subroutine test_ops_readstexp
end module m_test_ops_readstexp

program p_test_ops_readstexp
use m_test_ops_readstexp
use no_pfunit
implicit none
   call test_ops_readstexp()
   call conclusion()
end program p_test_ops_readstexp

