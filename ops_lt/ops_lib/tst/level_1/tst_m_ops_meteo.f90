!-------------------------------------------------------------
! Test routines in module m_ops_meteo
!-------------------------------------------------------------
module m_tst_ops_wv_log_profile

implicit none

contains

subroutine tst_ops_wv_log_profile

use no_pfunit
use m_error
use m_ops_meteo
use m_ops_varin
use Binas, only: pi

implicit none

! SUBROUTINE ARGUMENTS - INPUT
REAL                              :: z0                         ! roughness length (m)
REAL                              :: zu                         ! height at which the wind velocity has to be compute
REAL                              :: uster                      ! friction velocity (m)
REAL                              :: ol                         ! Monin-Obukhov length  (m)
TYPE(Tvarin)                      :: varin

! SUBROUTINE ARGUMENTS - OUTPUT
REAL                              :: uz                         ! wind velocity (m/s)

! SUBROUTINE ARGUMENTS - LOCAL
TYPE(TError)                        :: error                      ! error handling record
real :: hi, oli ! loop indices
character(len=255) :: message ! assertion message

real, dimension(9, 7) :: data_holtslag ! 9 stability classes a-i, 7 heights 10-200m
real, dimension(7) :: h_holtslag ! heights
real, dimension(9) :: ol_holtslag ! average Obukhov lengths, L_m in the paper
real, dimension(9) :: klasse_tol ! tolerance per Obukhov lengths
real :: psim, x ! helper variables
real :: z0_holtslag = 0.2 ! average z0, approximation

real                              :: tol = 1.0e-5               ! tolerance for equality of reals

! Taken from Table IV Holtslag, see below
data h_holtslag / 10., 20., 40., 80., 120., 160., 200. /
! ol from paper Holtslag, from fig 2-4, not same data as table
! data ol_holtslag / -28., -95., -365., 10000., 350., 130., 60., 20., 5. /
! data klasse_tol / 0.1, 0.06, 0.03, 0.1, 0.05, 0.04, 0.16, 0.15, 0.62 /
! ol from paper Van Ulden/Holtslag 1985, Table 2
data ol_holtslag / -30., -100., -370., 10000., 350., 130., 60., 20., 9. /
data klasse_tol / 0.11, 0.06, 0.03, 0.1, 0.05, 0.04, 0.16, 0.15, 0.62 /
data data_holtslag / 2.1, 3.9, 5.4, 6.3, 5.2, 3.9, 3.3, 2.6, 1.9, &
                     2.4, 4.5, 6.2, 6.8, 6.1, 4.8, 4.2, 3.6, 2.9, &
                     2.4, 4.8, 6.9, 7.7, 7.2, 6.1, 5.6, 5.0, 4.2, &
                     2.6, 5.1, 7.6, 8.9, 9.0, 8.3, 7.8, 6.8, 5.6, &
                     2.7, 5.4, 8.1, 9.7, 10.6, 9.4, 9.3, 7.8, 6.1, &
                     2.7, 5.6, 8.4, 10.3, 11.7, 11.0, 10.0, 8.1, 6.3, &
                     2.7, 5.8, 8.8, 10.9, 12.7, 11.6, 10.4, 8.3, 6.5 /

call ops_read_varin( './level_1/resources/ops_varin1.txt', varin, error) 

! Define inputs, call routine and check;
! reference values are from a run at 2020-09-30: 
z0 = 0.03; zu = 4.0; uster = 0.5; ol = -1000; 
call ops_wv_log_profile(z0, zu, uster, ol, varin%varin_meteo, uz)
call assertEqual(6.097657,uz,tol,'error in computed wind speed',__LINE__,__FILE__)

z0 = 0.03; zu = 4.0; uster = 0.5; ol = 1000; 
call ops_wv_log_profile(z0, zu, uster, ol, varin%varin_meteo, uz)
call assertEqual(6.140516,uz,tol,'error in computed wind speed',__LINE__,__FILE__)

z0 = 0.03; zu = 4.0; uster = 0.5; ol = -1; 
call ops_wv_log_profile(z0, zu, uster, ol, varin%varin_meteo, uz)
call assertEqual(3.765900,uz,tol,'error in computed wind speed',__LINE__,__FILE__)

z0 = 0.03; zu = 4.0; uster = 0.5; ol = 1; 
call ops_wv_log_profile(z0, zu, uster, ol, varin%varin_meteo, uz)
call assertEqual(20.52041,uz,tol,'error in computed wind speed',__LINE__,__FILE__)

z0 = 0.03; zu = 4.0; uster = 0.1; ol = -1000; 
call ops_wv_log_profile(z0, zu, uster, ol, varin%varin_meteo, uz)
call assertEqual(1.219531,uz,tol,'error in computed wind speed',__LINE__,__FILE__)

! incorrect inputs:
! z0 = -0.03; zu = 4.0; uster = 0.1; ol = -1000; 
! call ops_wv_log_profile(z0, zu, uster, ol, varin%varin_meteo, uz)


z0 = 0.03; zu = -4.0; uster = 0.1; ol = -1000; 
! call ops_wv_log_profile(z0, zu, uster, ol, varin%varin_meteo, uz)



z0 = 0.03; zu = 4.0; uster = -0.1; ol = -1000; 
call ops_wv_log_profile(z0, zu, uster, ol, varin%varin_meteo, uz)
call assertEqual(0.75,uz,tol,'wind speed not set at lower limit value; ',__LINE__,__FILE__)  

! Comparison to Table IV of:
! Holtslag A.A.M. (1984) Estimates of diabatic wind speed profiles from near surface weather observations. Boundary-Layer Meteorol. 29, 225-250

! Loop over combinations of Obukhov lengths oli and reference heights hi
! For CSV: write(*,*) 'z , L , uster , uz , type'
do oli = 1, size(ol_holtslag,1)
    do hi = 1, size(h_holtslag,1)
        ! Calculate windspeed for hi
        z0 = z0_holtslag; zu = h_holtslag(hi); ol = ol_holtslag(oli)
        IF (ol .GT. 0.0) THEN  
           !---------------------------
           ! L > 0, stable atmosphere, Van Ulden & Holtslag (1985), eq. 54
           !---------------------------
           psim = -17.*(1. - EXP( -0.29*h_holtslag(1)/ol)) + 17.*(1. - EXP( -0.29*z0/ol))
        ELSE
           !--------------------------------------
           ! L <= 0, unstable/neutral atmosphere, eq. 8a
           !--------------------------------------
           x    = (1. - 16.*h_holtslag(1) / ol)**0.25
           psim = 2.*LOG((1. + x)/2.) + LOG((1. + x*x)/2.) - 2.*ATAN(x) + pi/2.
        ENDIF

        ! Equation 9 Holtslag paper @ reference height of 10m = h_holtslag(1)
        uster = 0.4*data_holtslag(oli, 1)/(log(h_holtslag(1)/z0)-psim)
        call ops_wv_log_profile(z0, zu, uster, ol, varin%varin_meteo, uz)

        ! Format assertion error message, compare
        ! for CSV: write(*, *) h_holtslag(hi), ',', ol_holtslag(oli), ',', uster, ',', data_holtslag(oli, hi), ', Holtslag'
        ! for CSV: write(*, *) h_holtslag(hi), ',', ol_holtslag(oli), ',', uster, ',', uz, ', OPS'
        write(message, *) "Wind speed mismatch: h = ", h_holtslag(hi), ', L = ', ol_holtslag(oli), ', u* = ', uster, ', Holtslag = ', data_holtslag(oli, hi), ', OPS = ', uz, ', diff = ', (uz-data_holtslag(oli, hi))/uz
        call assertRelativelyEqual(uz,data_holtslag(oli, hi),klasse_tol(oli),message,__LINE__,__FILE__)
    end do
end do

end subroutine tst_ops_wv_log_profile

!-------------------------------------------------------------------------
subroutine tst_ops_wv_powerlaw_metstat

use no_pfunit
use m_ops_meteo
USE m_commonconst_lib, only: NTRAJ, NCOMP, NSTAB, NSEK

implicit none

! Input:
integer  :: istab    ! index of stability class
integer  :: isek     ! index of wind sector
real     :: astat(NTRAJ, NCOMP, NSTAB, NSEK) ! statistical meteo parameters
real     :: z        ! heigth where to compute wind velocity [m]

! Output:
real     :: uz       ! wind velocity at height z [m/s]
real     :: vw10     ! wind velocity at reference height 10 m [m/s]
real     :: pcoef    ! coefficient in wind speed power law


real     :: tol = 1.0e-5  ! tolerance for equality of reals

! Fill astat array:
astat(:, :, :, :) = -999. ! first fill with "missing data"
! now fill with some 'plausible data', only for NTRAJ=1 and NCOMP=3,15
! wind speed [m/s] @ 10 m
call random_number(astat(1, 3, :, :))
astat(1, 3, :, :) = (7.0-0.5) * astat(1, 3, :, :) + 0.5

! wind speed power law coefficient
call random_number(astat(1, 15, :, :))
astat(1, 15, :, :) = (0.45-0.13) * astat(1, 15, :, :) + 0.13 ! OPS manual

! Define inputs, call routine and check;
! reference values are from a run at 2020-09-30:
istab = 1; isek = 1; z = 4.0;
call ops_wv_powerlaw_metstat(istab,isek,astat,z,uz,vw10,pcoef)
call assertEqual(astat(1, 3, istab, isek),vw10,tol,'tst_ops_wv_powerlaw_metstat 1',__LINE__,__FILE__)
call assertEqual(astat(1, 15, istab, isek),pcoef,tol,'tst_ops_wv_powerlaw_metstat 1',__LINE__,__FILE__)
call assertEqual(vw10,uz,tol,'tst_ops_wv_powerlaw_metstat 1',__LINE__,__FILE__)

istab = 1; isek = 1; z = 14.0;
call ops_wv_powerlaw_metstat(istab,isek,astat,z,uz,vw10,pcoef)
call assertEqual(astat(1, 3, istab, isek),vw10,tol,'tst_ops_wv_powerlaw_metstat 2',__LINE__,__FILE__)
call assertEqual(astat(1, 15, istab, isek),pcoef,tol,'tst_ops_wv_powerlaw_metstat 2',__LINE__,__FILE__)
call assertEqual(vw10*(z/10.)**pcoef,uz,tol,'tst_ops_wv_powerlaw_metstat 2',__LINE__,__FILE__)

istab = 5; isek = 6; z = 4.0;
call ops_wv_powerlaw_metstat(istab,isek,astat,z,uz,vw10,pcoef)
call assertEqual(astat(1, 3, istab, isek),vw10,tol,'tst_ops_wv_powerlaw_metstat 3',__LINE__,__FILE__)
call assertEqual(astat(1, 15, istab, isek),pcoef,tol,'tst_ops_wv_powerlaw_metstat 3',__LINE__,__FILE__)
call assertEqual(vw10,uz,tol,'tst_ops_wv_powerlaw_metstat 3',__LINE__,__FILE__)

! use VWREP
istab = 1; isek = 1; z = 4.0
astat(1, 3, istab, isek) = 0.
call ops_wv_powerlaw_metstat(istab,isek,astat,z,uz,vw10,pcoef)
call assertEqual(2.6,vw10,tol,'tst_ops_wv_powerlaw_metstat 4',__LINE__,__FILE__)
call assertEqual(astat(1, 15, istab, isek),pcoef,tol,'tst_ops_wv_powerlaw_metstat 4',__LINE__,__FILE__)
call assertEqual(vw10,uz,tol,'tst_ops_wv_powerlaw_metstat 4',__LINE__,__FILE__)

! Incorrect inputs:
! istab = 50 -> Fortran error subscript out of bounds
! all values below zmet_u = 10 m give the same value as for 10 m, also for z = -4 
istab = 5; isek = 6; z = -4.0;
call ops_wv_powerlaw_metstat(istab,isek,astat,z,uz,vw10,pcoef)
call assertEqual(astat(1, 3, istab, isek),vw10,tol,'tst_ops_wv_powerlaw_metstat 5',__LINE__,__FILE__)
call assertEqual(astat(1, 15, istab, isek),pcoef,tol,'tst_ops_wv_powerlaw_metstat 5',__LINE__,__FILE__)
call assertEqual(vw10,uz,tol,'tst_ops_wv_powerlaw_metstat 5',__LINE__,__FILE__)

end subroutine tst_ops_wv_powerlaw_metstat

!-------------------------------------------------------------------------
subroutine tst_ops_meteo_cutoff

use no_pfunit
use m_ops_meteo

implicit none

! Input:
!real                :: var_cutoff                ! cutoff value for variable var

! Input/output:
real                :: var                       ! variable

real                :: tol = 1.0e-5               ! tolerance for equality of reals

! subroutine ops_meto_cutoff(var_cutoff,var)
var =  0.1
call ops_meteo_cutoff(5.0,var)
call assertEqual(5.0,var,tol,'ops_meteo_cutoff 1',__LINE__,__FILE__)

var = -0.1
call ops_meteo_cutoff(5.0,var)
call assertEqual(5.0,var,tol,'ops_meteo_cutoff 2',__LINE__,__FILE__)

end subroutine tst_ops_meteo_cutoff

!-------------------------------------------------------------------------
subroutine tst_ops_meteo_cutoff2

use no_pfunit
use m_ops_meteo

implicit none

! Input:
!real                :: var_cutoff1              ! cutoff value for negative values of variable var
!real                :: var_cutoff2              ! cutoff value for positive values of variable var

! Input/output:
real                :: var                       ! variable

real                :: tol = 1.0e-5               ! tolerance for equality of reals

! subroutine ops_meteo_cutoff2(var_cutoff1,var_cutoff2,var)
var = 0.1
call ops_meteo_cutoff2(-5.0,7.0,var)
call assertEqual(7.0,var,tol,'ops_meteo_cutoff2 a',__LINE__,__FILE__)

var = -0.1
call ops_meteo_cutoff2(-5.0,7.0,var)
call assertEqual(-5.0,var,tol,'ops_meteo_cutoff2 b',__LINE__,__FILE__)


!var = 0.0/0.0
!call ops_meteo_cutoff2(-5.0,7.0,var)
!call assertEqual(-5.0,var,tol,'ops_meteo_cutoff2 c',__LINE__,__FILE__)

end subroutine tst_ops_meteo_cutoff2

!----------------------------------------------------------------
subroutine tst_ops_meteo_cutoff_obukhov

! Cut-off for small values of Obukhov length

use no_pfunit
use m_ops_meteo

implicit none

! Input:
integer    :: ol_cutoff_iopt             ! option for cutoff of Obukhov length; see ops_meteo_cutoff_obukhov 
                                                                ! 1 -> fixed values for ol_min_stable and ol_max_unstable
                                                                ! 2 -> from NNM: L < 100*z0 For stable/unstable??
real       :: ol_add_stable              ! extra offset for Obukhov length in stable situations (may be zero) [m]
real       :: ol_max_unstable            ! cutoff value for unstable situations (ol < 0) [m]
real       :: ol_min_stable              ! cutoff value for stable situations (ol > 0) [m]
real       :: ol_z0_ratio_cutoff1        ! minimal value of ol/z0 ratio (from NNM)
real       :: ol_z0_ratio_cutoff2        ! minimal value of ol/z0 ratio (from NNM)
real       :: z0                         ! roughness length [m]

! Input/output:
real       :: ol                         ! Obukhov length [m]

! Local
real       :: tol = 1.0e-5               ! tolerance for equality of reals

! Test fixed cutoff values
ol_cutoff_iopt  = 1
ol_add_stable   = 0.0
ol_max_unstable = -7.0
ol_min_stable   = 5.0
ol_z0_ratio_cutoff1 = 100.0
ol_z0_ratio_cutoff2 = 50.0
z0 = 0.03
ol = 0.1
call ops_meteo_cutoff_obukhov(ol_cutoff_iopt, ol_add_stable, ol_max_unstable, ol_min_stable, &
                              ol_z0_ratio_cutoff1, ol_z0_ratio_cutoff2, z0, ol)
call assertEqual(5.0,ol,tol,'ops_meteo_cutoff_obukhov 1',__LINE__,__FILE__)

! Test 100*z0 for stable, -5 m for unstable:
ol_cutoff_iopt  = 2
ol = 0.1
call ops_meteo_cutoff_obukhov(ol_cutoff_iopt, ol_add_stable, ol_max_unstable, ol_min_stable, &
                              ol_z0_ratio_cutoff1, ol_z0_ratio_cutoff2, z0, ol)
call assertEqual(3.0,ol,tol,'ops_meteo_cutoff_obukhov 2a',__LINE__,__FILE__)
ol = -0.1; 
ol_max_unstable = -5;
call ops_meteo_cutoff_obukhov(ol_cutoff_iopt, ol_add_stable, ol_max_unstable, ol_min_stable, &
                              ol_z0_ratio_cutoff1, ol_z0_ratio_cutoff2, z0, ol)
call assertEqual(-5.0,ol,tol,'ops_meteo_cutoff_obukhov 2b',__LINE__,__FILE__)

! Test 100*z0 for stable, -50*z0 for unstable:
ol_cutoff_iopt  = 3
ol = 0.1
call ops_meteo_cutoff_obukhov(ol_cutoff_iopt, ol_add_stable, ol_max_unstable, ol_min_stable, &
                              ol_z0_ratio_cutoff1, ol_z0_ratio_cutoff2, z0, ol)
call assertEqual(3.0,ol,tol,'ops_meteo_cutoff_obukhov 3a',__LINE__,__FILE__)
ol = -0.1; 
call ops_meteo_cutoff_obukhov(ol_cutoff_iopt, ol_add_stable, ol_max_unstable, ol_min_stable, &
                              ol_z0_ratio_cutoff1, ol_z0_ratio_cutoff2, z0, ol)
call assertEqual(-1.5,ol,tol,'ops_meteo_cutoff_obukhov 3b',__LINE__,__FILE__)

! Test fixed cutoff values + add_stable
ol_cutoff_iopt = 1
ol_add_stable  = 5.0
ol = 0.1
call ops_meteo_cutoff_obukhov(ol_cutoff_iopt, ol_add_stable, ol_max_unstable, ol_min_stable, &
                              ol_z0_ratio_cutoff1, ol_z0_ratio_cutoff2, z0, ol)
call assertEqual(5.1,ol,tol,'ops_meteo_cutoff_obukhov 4',__LINE__,__FILE__)

! The same, but lower value of add_stable:
ol_add_stable  = 4.0
ol = 0.1
call ops_meteo_cutoff_obukhov(ol_cutoff_iopt, ol_add_stable, ol_max_unstable, ol_min_stable, &
                              ol_z0_ratio_cutoff1, ol_z0_ratio_cutoff2, z0, ol)
call assertEqual(5.0,ol,tol,'ops_meteo_cutoff_obukhov 5',__LINE__,__FILE__)

end subroutine tst_ops_meteo_cutoff_obukhov

!----------------------------------------------------------------
subroutine tst_ops_meteo_cutoff_uster

! Cut-off for small values of friction velocity

use no_pfunit
use m_ops_meteo

implicit none

! Input:
integer    :: uster_cutoff_iopt          ! option for cutoff of uster 
                                         ! 1 -> fixed values for uster_min
                                         ! 2 -> correction according to the altered Obukhov length (uster2 = uster1*(ol2/ol1)**0.33) 
real       :: uster_min                  ! fixed cutoff value for uster [m/s]
real       :: ol_old                     ! Obukhov length before setting it to the cutoff value in ops_meteo_cutoff_obukhov
real       :: ol_new                     ! Obukhov length after setting it to the cutoff value in ops_meteo_cutoff_obukhov

! Input/output:
real       :: uster                      ! friction velocity [m/s]

! Local
real       :: tol = 1.0e-5               ! tolerance for equality of reals

! Test fixed cutoff values
uster_cutoff_iopt = 1
uster_min         = 0.04
uster             = 0.01
ol_old            = 0.0
ol_new            = 0.0
call ops_meteo_cutoff_uster(uster_cutoff_iopt, uster_min, ol_old, ol_new, uster)
call assertEqual(0.04,uster,tol,'ops_meteo_cutoff_uster 1',__LINE__,__FILE__)

! Test according to change in Obukhov length:
! uster = 0.01*(5/4)^(1/3) = 0.010772
uster_cutoff_iopt = 2
uster  = 0.01
ol_old = 4.0
ol_new = 5.0 
call ops_meteo_cutoff_uster(uster_cutoff_iopt, uster_min, ol_old, ol_new, uster)
call assertEqual(0.010772,uster,tol,'ops_meteo_cutoff_uster 2',__LINE__,__FILE__)

! According to change in Obukhov length:
! uster = 0.01*(10/5)^(0.33) = 0.01257  
uster  = 0.01
ol_old = 5.0
ol_new = 10.0 
call ops_meteo_cutoff_uster(uster_cutoff_iopt, uster_min, ol_old, ol_new, uster)
call assertEqual(0.01257,uster,tol,'ops_meteo_cutoff_uster 3',__LINE__,__FILE__)



end subroutine tst_ops_meteo_cutoff_uster

!----------------------------------------------------------------
subroutine tst_ops_meteo_cutoff_obust

! Cut-off for Obukhov length and friction velocity

use no_pfunit
use m_ops_meteo
use m_ops_varin

implicit none

! Input:
type(Tvarin_meteo)             :: varin_meteo    ! input model paramaters for meteo processes
real                           :: z0             ! roughness length, regional scale (> 50 km) [m]
                                                 
! Input/output:                                  
real                           :: ol             ! Obukhov length, regional scale (> 50 km) [m]
real                           :: uster          ! friction velocity, regional scale (> 50 km) [m/s]

! Local:
real       :: tol = 1.0e-5               ! tolerance for equality of reals

varin_meteo%ol_cutoff_iopt1     = 1
varin_meteo%ol_add_stable0      = 0.0
varin_meteo%ol_max_unstable1    = -7.0
varin_meteo%ol_min_stable1      = 5.0
varin_meteo%ol_z0_ratio_cutoff1 = 100.0
varin_meteo%ol_z0_ratio_cutoff2 = 0.0
varin_meteo%uster_cutoff_iopt1  = 1
varin_meteo%uster_min1          = 0.04
z0 = 0.03
ol = 0.1
uster = 0.001
call ops_meteo_cutoff_obust(varin_meteo,z0,ol,uster)
call assertEqual(5.0,ol,   tol,'ops_meteo_cutoff_obust 1 ol',__LINE__,__FILE__)
call assertEqual(0.04,uster,tol,'ops_meteo_cutoff_obust 1 uster',__LINE__,__FILE__)

! More tests -> tst_ops_meteo_cutoff_obukhov and tst_ops_meteo_cutoff_uster

end subroutine tst_ops_meteo_cutoff_obust

end module m_tst_ops_wv_log_profile

program p_tst_ops_wv_log_profile
use no_pfunit
use m_tst_ops_wv_log_profile

implicit none

call tst_ops_wv_log_profile
call tst_ops_wv_powerlaw_metstat
call tst_ops_meteo_cutoff
call tst_ops_meteo_cutoff2
call tst_ops_meteo_cutoff_obukhov
call tst_ops_meteo_cutoff_uster
call tst_ops_meteo_cutoff_obust
call conclusion

end program p_tst_ops_wv_log_profile
