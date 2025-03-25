module m_random
Use iso_fortran_env, only : int64, real32
implicit none

    ! MMIX by Donald Knuth
    integer(kind=int64), parameter :: a=1664525, c=1013904223, m=int(2,int64)**32
    integer(kind=int64), save      :: seed = 0

contains

    function i64random()
       integer(kind=int64) :: i64random
       seed = mod(m+mod(a*seed + c, m),m)
       i64random = seed
    end function i64random

    function r32random()
       real(kind=real32) :: r32random
       r32random = i64random() / real(m,kind=real32)
    end function r32random

    subroutine set_seed(s)
      integer(int64), intent(in) :: s
      seed = s
    end subroutine set_seed

    function get_seed()
      integer(int64) :: get_seed
      get_seed = seed
    end function get_seed
     
end module m_random



module m_tst_m_plumerize
implicit none
contains

   subroutine example_astat(astat)
   Use iso_fortran_env, only : int64
   use m_random
   use m_commonconst_lib, only: NTRAJ, NCOMP, NSTAB, NSEK
      real, allocatable, intent(out)    :: astat(:, :, :, :) ! statistical meteo parameters
      integer :: istab, isek

      call set_seed(int(16111970,int64))
      allocate(astat(NTRAJ, NCOMP, NSTAB, NSEK))
      astat(:, :, :, :) = -999. ! first fill with "missing data"

      ! now fill with some 'plausible data', only for NTRAJ=1 and NCOMP=3,15
      do istab = 1,nstab
         do isek = 1,nsek
             ! wind speed [m/s] @ 10 m
             astat(1,  3, istab, isek) = (7.0-0.5) * r32random() + 0.5
             ! wind speed power law coefficient
             astat(1, 15, istab, isek) = (0.45-0.13) * r32random() + 0.13 ! OPS manual
         end do
      end do
   end subroutine example_astat

   !---------------------------------------------------------
   ! test subroutines in module m_ops_plumerise
   
   !---------------------------------------------------------
   subroutine tst_ops_plumerise
   use no_pfunit
   use m_ops_plumerise
   use m_error
   use m_commonfile, only: IOB_STDOUT
   use m_ops_varin

      ! Local:
      integer :: itest             ! index of test run
      integer, parameter :: ntests = 5
      real,    parameter  :: tol = 1.0e-5      ! tolerance in equality checks
      character(len=*), parameter :: str1 = 'Error when computing effluent gas temperature Ts_stack from given heat content. ' // &
                'The heat content specified cannot be reached within the given volume flux specified by velocity and stack diameter.'
      
      ! Input for test
      real, parameter :: z0       =    0.03  ! roughness length [m]
      real, parameter :: hemis0   =    5.0   ! initial emission height = stack height [m]
      real, parameter :: uster    =    0.2   ! friction velocity [m/s]
      real, parameter :: ol       =   10.0   ! Monin-Obukhov length [m]
      real, parameter :: D_stack  =    2.0   ! diameter of the stack [m]
      real, parameter :: V_stack  =    1.0   ! exit velocity of plume at stack tip [m/s]
      real, parameter :: Ts_stack = -999.    ! temperature of effluent from stack [K]
      real, parameter :: temp_C   =   12.0   ! ambient temperature at height zmet_T [C] 
      real, parameter :: zmix     = 1000.0   ! mixing height [m] 
      real, parameter :: zmix_loc = 1000.0   ! mixing height, local scale [m]
      integer, parameter :: isec_prelim = 1

      real, parameter    :: qw(ntests)              = (/     0.1,     0.5,    0.0,    0.5,   1e10  /)  ! heat content [MW]
      logical, parameter :: emis_horizontal(ntests) = (/ .false., .false., .true., .true., .false. /)  ! horizontal outflow of emission
      real, allocatable  :: astat(:, :, :, :) ! statistical meteo parameters
      
      ! Output from test                    
      real,    parameter :: ref_hemis1(ntests)   = (/ 27.75813, 41.84061, 5.0, 41.84061,    0.0 /)  ! emission height, including plume rise [m]
      real,    parameter :: hemis_prelim(ntests) = (/ 31.97794, 48.91345, 5.0, 21.65816,    0.0 /)  ! emission height, including plume rise [m]
      real,    parameter :: ref_onder(ntests)  = (/  1.0,        1.0,   1.0,   1.0,       1.0 /)  ! part of plume below mixing height 
      integer, parameter :: istab(ntests)      = (/   5,          5,     1,     1,         1  /)  ! index of stability class and preliminary wind sector
      real :: hemis1, onder
      type (TError):: error              ! error handling record 
      type (Tvarin_meteo):: varin_meteo  !varin structure for meteo variables
      type (Tvarin_unc)  :: varin_unc

      call example_astat(astat)
      
      do itest = 1,size(qw,1)
         call ops_plumerise(z0, hemis0, uster, ol, qw(itest), D_stack, V_stack, Ts_stack, emis_horizontal(itest), temp_C, zmix, zmix_loc, varin_meteo, varin_unc, hemis1, onder, error)
         if (itest==5) then
            call assertEqual(str1,error%message)
            call assertTrue(error%haserror,'error in input has not been trapped by routine ops_plumerise')
         else
            if (error%haserror) call write_error(IOB_STDOUT, error)
            call assertFalse(error%haserror,'error in input has not been trapped by routine ops_plumerise')
            call assertEqual(hemis1, ref_hemis1(itest), tol, 'hemis1')
            call assertEqual(onder, ref_onder(itest), tol, 'onder')
         end if
         error%haserror=.false. ! reset haserror
         call ops_plumerise_prelim(istab(itest), isec_prelim, astat, hemis0, qw(itest), D_stack, V_stack, Ts_stack, emis_horizontal(itest), varin_meteo, varin_unc, hemis1, error)
         if (itest==5) then
            call assertEqual(str1,error%message)
            call assertTrue(error%haserror,'error in input has not been trapped by routine ops_plumerise_prelim')
         else
            if (error%haserror) call write_error(IOB_STDOUT, error)
            call assertFalse(error%haserror,'error in input has not been trapped by routine ops_plumerise_prelim')
            call assertEqual(hemis1, hemis_prelim(itest), tol, 'hemis1')
          end if
      end do 
   end subroutine tst_ops_plumerise
   

   subroutine tst_ops_plumerise_qw_Ts
   use no_pfunit
   use m_ops_plumerise
   use m_error
   use m_commonfile, only: IOB_STDOUT
   use Binas, only : pi, T0
   
      ! Local 
      integer :: itest             ! index of test run
      integer, parameter :: ntests = 5
      real,    parameter :: tol = 1.0e-5      ! tolerance in equality checks
      character(len=*), parameter :: str1 = 'Error when computing effluent gas temperature Ts_stack from given heat content. ' // &
                'The heat content specified cannot be reached within the given volume flux specified by velocity and stack diameter.'

      ! Input for test
      real, parameter :: V0       = 1.25  ! normal flux (m0^3/s)
      real, parameter :: D_stack  = 2.0   ! diameter of the stack [m]
      real, parameter :: Ts_stack = 373.  ! temperature of effluent from stack [K]
      real, parameter :: Ta_stack = 285  ! ambient temperature at stack height (K)  
      real, parameter :: V_stack  = (V0*Ts_stack)/(T0*pi*(0.5*D_stack)**2)  ! exit velocity of plume at stack tip [m/s] 
      real, parameter :: C1       = rho0*Cp0*(pi*(0.5*D_stack)**2)*V_stack*T0*(1.0e-6)               ! help variable nEeded for Ts_stack2 
      
      real, parameter    :: qw(ntests)              = (/ -999., 0.142941177,  1.0,  -999.,  C1+0.1 /)  ! heat content [MW]
      logical, parameter :: emis_horizontal(ntests) = (/ .false., .false., .true., .true., .false. /)  ! horizontal outflow of emission
      real, parameter    :: Ts_stacks(ntests)       = (/ Ts_stack,  -999.,  -999., Ts_stack, -999. /)  ! effluent temperature [K]
      
      ! Output:
      real, parameter    :: ref_Ts_stack2(ntests)   = (/ Ts_stack, Ts_stack,  -999., Ts_stack,   -999. /)  ! effluent temperature [K]
      real, parameter    :: ref_qw2(ntests)         = (/ 0.1429412, 0.1429412,  1.0, 0.1429412, C1+0.1 /)  ! heat content [MW]

      real :: Ts_stack2            ! effluent temperature at stack height, but missing value replaced by computation from Qw [K]
      real :: qw2                  ! heat content emission, but missing value replaced by computation from Ts [MW]
      type (TError):: error        ! error handling record 
      
      do itest = 1,size(qw,1)
         call ops_plumerise_qw_Ts(qw(itest), D_stack, V_stack, Ts_stacks(itest), emis_horizontal(itest), Ta_stack, qw2, Ts_stack2, error)
         if (itest==5) then
            call assertEqual(str1,error%message)
            call assertTrue(error%haserror,'error in input has not been trapped by routine ops_plumerise')
         else
            if (error%haserror) call write_error(IOB_STDOUT, error)
            call assertFalse(error%haserror,'error in input has not been trapped by routine ops_plumerise')
            call assertEqual(Ts_stack2, ref_Ts_stack2(itest), tol, 'Ts_stack2')
            call assertEqual(qw2, ref_qw2(itest), tol, 'qw2')
         end if
      end do 
   end subroutine tst_ops_plumerise_qw_Ts
   
   subroutine tst_ops_plumerise_buoyancy
   use no_pfunit
   use m_ops_plumerise
   use m_error
   use m_commonfile, only: IOB_STDOUT
   use m_ops_varin, only: Tvarin_meteo
      
      integer :: itest
      integer, parameter :: ntests = 4
      real,    parameter :: tol = 0.1      ! tolerance in equality checks, based on parameter 'epsa' from the subroutine ops_plumerise_buoyancy
      ! Input
      real,    parameter :: z0              =   0.03  ! roughness length [m]
      real,    parameter :: hemis0          =  50.0   ! initial emission height = stack height [m]
      real,    parameter :: uster           =   0.2   ! friction velocity [m/s]
      real,    parameter :: ol              =  10.0   ! Monin-Obukhov length [m]
      real,    parameter :: Ta_stack        = 285     ! ambient temperature @ stack height [K]
      real,    parameter :: dthetadz_stable =   0.006 ! In the OPS manual (just below Eq. 4.6) it is stated that an average value of 0.006 K/m is taken as representative
      real,    parameter :: u_stack         =   5.0   ! wind speed @ stack height [m/s]
      logical, parameter :: prelim          = .false. ! preliminary plume rise, based on stability class and preliminary wind sector (ol, uster, ... still unknown)
      integer, parameter :: isec_prelim     =   1     ! preliminary wind sector

      logical, parameter :: non_stable(ntests) = (/ .false., .false., .true., .true. /)      ! non-stable (unstable/neutral) conditions
      real,    parameter :: qw(ntests)         = (/   0.0,     0.1,    0.1,     6.5  /)      ! heat content (MW)
      integer, parameter :: istab(ntests)      = (/    5,       5,      1,       1   /)      ! index of stability class and preliminary wind sector
      type (Tvarin_meteo):: varin_meteo                                                      ! varin structure for meteo variables
      
      ! Output                           
      real, parameter :: ref_dh_buoyancy(ntests) = (/ 0.0, 19.04226, 1.883308,  39.11432 /)       ! plume rise due to buoyancy [m]
      real :: dh_buoyancy      ! plume rise due to buoyancy [m]
      
      do itest = 1,ntests
         call ops_plumerise_buoyancy(z0, ol, uster, varin_meteo, non_stable(itest), qw(itest), Ta_stack, dthetadz_stable, &
                                     u_stack, hemis0, dh_buoyancy, prelim, istab(itest), isec_prelim)
         call assertEqual(ref_dh_buoyancy(itest),dh_buoyancy,tol,'dh_buoyancy')
      end do
   end subroutine tst_ops_plumerise_buoyancy
   
   subroutine tst_ops_plumerise_momentum()
   use no_pfunit
   use m_ops_plumerise
   use m_ops_utils, only: is_missing

      integer :: itest
      integer, parameter :: ntests = 3
      real,    parameter :: tol = 1.0e-5      ! tolerance in equality checks

      ! Input:
      real, parameter :: u_stack   = 5.0              ! wind speed @ stack height [m/s]
      real, parameter :: D_stack   = 2.0              ! diameter of the stack [m]
      real, parameter :: Ts_stack  = -999.            ! temperature of effluent from stack [K]
      real, parameter :: Ta_stack  = 285.0            ! ambient temperature at stack height [K]
      real, parameter :: dthetadz_stable = 0.006      ! In the OPS manual (just below Eq. 4.6) it is stated that an average value of 0.006 K/m is taken as representative
      real,    parameter :: V_stack(ntests)    = (/    1.0,     1.0,     0.0 /)   ! exit velocity of plume at stack tip [m/s]
      logical, parameter :: non_stable(ntests) = (/ .true., .false., .false. /)   ! non-stable (unstable/neutral) conditions
                                           
      ! Output:
      real, parameter :: ref_dh_momentum(ntests)  = (/ 1.2, -2.375771, 0.0 /)
      real :: dh_momentum          ! plume rise due to momentum [m]
      
      do itest = 1,ntests
         call ops_plumerise_momentum(u_stack, D_stack, V_stack(itest), Ts_stack, Ta_stack, &
                                     dthetadz_stable, non_stable(itest), dh_momentum)
         call assertEqual(ref_dh_momentum(itest),dh_momentum,tol,'dh_momentum')
      end do
   end subroutine tst_ops_plumerise_momentum 
   
   subroutine tst_ops_plume_penetration()
   use no_pfunit
   use m_ops_plumerise
   use m_ops_varin, only: Tvarin_meteo, Tvarin_unc

      ! Input                          
      real :: hemis0     ! initial emission height = stack height [m]
      real :: zmix       ! mixing height [m] 
      real :: zmix_loc   ! mixing height, local scale [m]
      real :: ol         ! Monin-Obukhov length [m]
      real :: dh         ! plume rise due to either buoyancy or momentum [m]
      real :: z0               ! roughness length [m] 
      real :: uster            ! friction velocity [m/s] 
      logical :: non_stable       ! non-stable (unstable/neutral) conditions
      real    :: qw               ! heat content (MW)
      real    :: Ta_stack         ! ambient temperature at stack height [K]
      real    :: D_stack              ! stack internal diameter [m]
      real    :: V_stack              ! exit velocity of plume at stack tip [m/s]
      real    :: Ts_stack             ! temperature of effluent from stack [K] 
      real    :: dthetadz_stable  ! fixed potential temperature gradient dtheta/dz [K/m] for stable conditions, used for dh_buoyancy and dh_momentum
      real    :: u_stack          ! wind speed at stack height [m/s]
      type (Tvarin_meteo):: varin_meteo  ! varin structure for meteo variables
      
      ! Input, optional:
      logical :: prelim      ! preliminary plume rise, based on stability class and preliminary wind sector (ol, uster, ... still unknown)
      integer :: istab       ! index of stability class and preliminary wind sector
      integer :: isec_prelim ! index of preliminary source-receptor wind sector (wind shear not yet taken into account)
      !real    :: astat(NTRAJ, NCOMP, NSTAB, NSEK) ! statistical meteo parameters
      
      ! Input/Output                   
      real :: hemis1     ! emission height, including plume rise [m]
      
      ! Output                         
      real :: onder      ! part of plume below mixing height 
      real :: tol = 1.0e-5      ! tolerance in equality checks
      
      z0              =  0.03            ! roughness length [m]
      uster           =  0.2             ! friction velocity [m/s]
      qw              = 1.0              ! heat content [MW]
      non_stable      = .FALSE.          ! unstable conditions?
      Ta_stack        = 285              ! ambient temperature @ stack height [K]
      dthetadz_stable = 0.006            ! In the OPS manual (just below Eq. 4.6) it is stated that an average value of 0.006 K/m is taken as representative
      u_stack         = 5.0              ! wind speed @ stack height [m/s]
      prelim          = .FALSE.          ! preliminary plume rise?
      istab           = 5                ! U1/U2, N1/N2, S1/S2
      isec_prelim     = 1                ! preliminary wind sector
      hemis0    =  80.0             ! initial emission height = stack height [m]
      zmix      = 100.0           ! mixing height [m] 
      zmix_loc  = 100.0           ! mixing height, local scale [m]
      ol        = 10.0             ! Monin-Obukhov length [m]
      D_stack   = 2.0              ! diameter of the stack [m]
      V_stack   = 2.0              ! exit velocity of plume at stack tip [m/s]
      Ts_stack  = 300.            ! temperature of effluent from stack [K]
      
      !----------------------------------------------------------
      ! Test 1: plumerise due to buoyancy, everyting below
      !----------------------------------------------------------
      zmix = 1000.0; zmix_loc = 1000.0
      call ops_plumerise_buoyancy(z0, ol, uster, varin_meteo, non_stable, qw, Ta_stack, dthetadz_stable, u_stack, hemis0, dh, prelim, istab, isec_prelim)
      hemis1 = hemis0 + dh
      call assertLessThan(hemis0, hemis1, 'Test 1: hemis0<hemis1')
      call ops_plume_penetration(hemis0,zmix,zmix_loc,ol,dh,hemis1,onder)
      call assertEqual(1.0,onder,tol,'Test 1: onder')
      
      !----------------------------------------------------------
      ! Test 2: plumerise due to buoyancy, onder = 0.3567484 (2021-06-22)
      !----------------------------------------------------------
      zmix = 100.0; zmix_loc = 100.0
      call ops_plumerise_buoyancy(z0, ol, uster, varin_meteo, non_stable, qw, Ta_stack, dthetadz_stable, u_stack, hemis0, dh, prelim, istab, isec_prelim)
      hemis1 = hemis0 + dh
      call assertLessThan(hemis0, hemis1, 'Test 2: hemis0<hemis1')
      call ops_plume_penetration(hemis0,zmix,zmix_loc,ol,dh,hemis1,onder)
      call assertEqual(0.3567484,onder,tol,'Test 2: onder')
      
      !----------------------------------------------------------
      ! Test 3: ol > 100, onder = 0.1114180 (2021-06-22)
      !----------------------------------------------------------
      ol = 101.0; zmix = 110.0; zmix_loc = 110.0
      call ops_plumerise_buoyancy(z0, ol, uster, varin_meteo, non_stable, qw, Ta_stack, dthetadz_stable, u_stack, hemis0, dh, prelim, istab, isec_prelim)
      hemis1 = hemis0 + dh
      call assertLessThan(hemis0, hemis1, 'Test 3: hemis0<hemis1')
      call ops_plume_penetration(hemis0,zmix,zmix_loc,ol,dh,hemis1,onder)
      call assertEqual(0.1114180,onder,tol,'Test 3: onder')
      
      !----------------------------------------------------------
      ! Test 4: hemis1>zmix & onder>0, onder = 0.7071428 & hemis1=zmix (2021-06-22)
      !----------------------------------------------------------
      ol = 10.0; zmix = 110.0; zmix_loc = 110.0
      hemis1 = 115.0
      dh = hemis1 - hemis0
      call ops_plume_penetration(hemis0,zmix,zmix_loc,ol,dh,hemis1,onder)
      call assertEqual(0.7071428,onder,tol,'Test 4: onder')
      call assertEqual(zmix,hemis1,tol,'Test 4: hemis1')
   
   end subroutine tst_ops_plume_penetration
   
end module m_tst_m_plumerize

program tst_m_plumerise
use m_tst_m_plumerize
use no_pfunit
implicit none
    call tst_ops_plumerise()
    call tst_ops_plumerise_qw_Ts()
    call tst_ops_plumerise_buoyancy()
    call tst_ops_plumerise_momentum()
    call tst_ops_plume_penetration()
    call conclusion()
end program tst_m_plumerise

