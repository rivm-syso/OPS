module m_astat
implicit none
contains

   subroutine example_astat(astat)
   Use iso_fortran_env, only : int64
   use m_random
   use m_commonconst_lib, only: NTRAJ, NCOMP, NSTAB, NSEK
   use m_commonconst_lt,only: MISVALNUM
      real, allocatable, intent(out)    :: astat(:, :, :, :)     ! statistical meteo parameters
      integer :: istab, isek

      call set_seed(int(16111970,int64))
      allocate(astat(NTRAJ, NCOMP, NSTAB, NSEK))
      astat(:, :, :, :) = -999. ! first fill with "missing data"

      ! now fill with some 'plausible data', only for NTRAJ=1 and NCOMP=3,15
      do istab = 1,nstab
         do isek = 1,nsek
             ! local mixing height (near source)
             astat(1, 2,  istab, isek) = (7e2- 3e1) * r32random() + 3e1   

             ! mixing height at 100 km  
             astat(2, 2,  istab, isek) = (7e3- 6e1) * r32random() + 6e1   


             ! wind speed [m/s] @ 10 m
             astat(1,  3, istab, isek) = (7.0-0.5) * r32random() + 0.5

             !   dscor : effective travel distance for each distance class
             !           for current stability class and current wind direction 
             astat(2, 9, istab, isek) = (120.-100.) * r32random() + 100.
             astat(3, 9, istab, isek) = (300.-430.) * r32random() + 300.
             astat(4, 9, istab, isek) = (1600.-1000.) * r32random() + 1000.

             ! space heating coefficient (degree-day values in combination with a wind speed correction) [C m^1/2 / s^1/2] 
             astat(1, 10, istab, isek) = (26.6-2.4) * r32random() + 2.4

             ! rain probability [-]
             astat(1, 11, istab, isek) = (0.23-0) * r32random() + 0

             ! buil (length of rainfall period) 
             astat(1, 12, istab, isek) = (3.5-0) * r32random() + 0

             ! rain intensity [mm/h]
             astat(1, 13, istab, isek) = (1.5-0) * r32random() + 0

             ! rad
             astat(1, 14, istab, isek) = (180.-1.00) * r32random() + 1.00

             ! wind speed power law coefficient
             astat(1, 15, istab, isek) = (0.45-0.13) * r32random() + 0.13 ! OPS manual

             ! Rc(SO2) from meteo statistics [s/m]  
             astat(1, 16, istab, isek) = (150.-15.) * r32random() + 15.

             astat(1, 21, istab, isek) = (100.0- (-100.0)) * r32random() + (-100.0)

             ! relative humidity
             astat(1, 24, istab, isek) = (92.-54.) * r32random() + 54.

             ! Rc(NO2) from meteo statistics [s/m]
             astat(1, 25, istab, isek) = (881.-167.) * r32random() + 167.

             ! Rc(NH3) from meteo statistics [s/m]
             astat(1, 26, istab, isek) = (190.-100.) * r32random() + 100.

             ! Rc(SO4-aerosol) from meteo statistics [s/m] 
             astat(1, 27, istab, isek) = (9503.-1275.) * r32random() + 1275.
         end do
      end do

      do istab = 1,nstab
         do isek = 1,nsek
             if (r32random() < 0.35) astat(2, 2,  istab, isek) = real(MISVALNUM)
         end do
      end do
   end subroutine example_astat

   subroutine read_astat(astat, filename)
   use m_commonconst_lib, only: NTRAJ, NCOMP, NSTAB, NSEK
   use m_fileutils
      real, allocatable, intent(out) :: astat(:,:,:,:)
      character(len=*),  intent(inout) :: filename
      integer :: unit, iostat
      integer :: itraj, icomp, istab, isek
      allocate(astat(NTRAJ, NCOMP, NSTAB, NSEK))
      call fix_slashes(filename)
      open(newunit=unit, file=filename, iostat=iostat)
      if (iostat/=0) then
         print *,'Error opening file "'//trim(filename)//'"'
         stop 1
      end if
      do isek = 1,NSEK
         do istab=1,NSTAB
            do icomp=1,NCOMP
               do itraj=1,NTRAJ
                  read(unit,*,iostat=iostat) astat(itraj, icomp, istab, isek)
                  if (iostat/=0) then
                     print *,'Error reading from file "'//trim(filename)//'"'
                     stop 1
                  end if
               end do
            end do
         end do
      end do
   end subroutine read_astat
end module m_astat

