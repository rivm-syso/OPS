module m_test_ops_readdata
implicit none
contains
   subroutine test_ops_readdata()
   use m_commonconst_lib, only: NTRAJ, NCOMP, NSTAB, NSEK
   use m_ops_read_meteo
   use m_astat
   use m_commonfile
   use m_error
   use no_pfunit
       real, parameter :: tol = 1e-5
        character(len=*), parameter :: nfile = "../data/GCN2022/Meteo/a005105c.005"
        integer*2 :: ishort(72)
        integer, parameter :: ref_ishort(72) = (/ 5, 1, 1, 6, 1, 1, 8, 756, 0, 4, 6, 12, 0, 100, 300, &
           1000, 4, 27, 17, 1, 1, 51, 95, 5, 30, 30, 290, 200, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/)
        real, allocatable :: ref_astat(:,:,:,:), astat(:,:,:,:)
        type(TError) :: error
        logical, parameter :: ref_error_haserror = .false.
        character(len=*), parameter :: ref_error_message = ""
        integer :: isek, istab
        character(len=500) :: filename
        filename  = './level_1/resources/astat.data'
        call read_astat(ref_astat, filename)
        allocate(astat(NTRAJ, NCOMP, NSTAB, NSEK))
        call ops_readdata( nfile, ishort, astat, error)
        if (error%haserror) call write_error(IOB_STDOUT, error)
        call assertFalse(error%haserror,'error from ops_readdata')
        call assertEqual( int(ref_ishort), int(ishort), "ishort", __LINE__, __FILE__)
        do isek = 1,NSEK
           do istab = 1,NSTAB
              call assertEqual( ref_astat(:,:,istab,isek), astat(:,:,istab,isek), &
                                tol, "astat", __LINE__, __FILE__)
           end do
        end do
   end subroutine test_ops_readdata
end module m_test_ops_readdata
 
program p_test_ops_readdata
use m_test_ops_readdata
use no_pfunit
implicit none
   call test_ops_readdata()
   call conclusion()
end program p_test_ops_readdata

