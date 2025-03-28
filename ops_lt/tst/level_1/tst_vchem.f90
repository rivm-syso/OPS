module m_test_vchem
implicit none
contains
    subroutine test_ops_vchem_add_nox_no2()
    use m_commonconst_lt, only: NSEK
    use m_ops_vchem
    use m_commonfile
    use m_error
    use no_pfunit
        real, parameter :: tol = 1e-5
        logical, parameter :: lroad_corr = .false.
        integer, parameter :: iter = 1
        integer, parameter :: nrrcp = 90000
        integer, parameter :: isec_prelim = 6
        real, parameter :: c =   8.42251033E-02
        real, parameter :: r_no2_nox =   5.87130308E-01
        real, parameter :: percvk =   7.73159508E-03
        logical :: lroad_corr_present
        logical, parameter :: ref_lroad_corr_present = .false.
        integer :: nstab_present
        integer, parameter :: in_nstab_present = 0
        logical, parameter :: in_lroad_corr_present = .false.
        integer, parameter :: ref_nstab_present = 0
        real, allocatable :: cno2, ref_cno2, percvk_sec(:), cnox_sec(:)
        integer, allocatable :: nsrc_sec(:)
        allocate(percvk_sec(NSEK), nsrc_sec(NSEK), cnox_sec(NSEK))
        nstab_present = in_nstab_present 
        lroad_corr_present  = in_lroad_corr_present 
        cno2=  3.37710371E-04
        ref_cno2=  7.20046344E-04
        call ops_vchem_add_nox_no2( lroad_corr, iter, isec_prelim, c, r_no2_nox, percvk, &
            lroad_corr_present, cnox_sec, cno2, percvk_sec, nsrc_sec, nstab_present)
        call assertEqual( cno2, ref_cno2, tol, "lroad_corr_present", __LINE__, __FILE__)
        call assertEqual( ref_lroad_corr_present, lroad_corr_present, "lroad_corr_present", __LINE__, __FILE__)
        call assertEqual( ref_nstab_present, nstab_present, "nstab_present", __LINE__, __FILE__)
   end subroutine test_ops_vchem_add_nox_no2
   
!===============================================================================================================
! tst_ops_vchem: test different options for computung chemical conversion rate.
!                All if-statements in ops_vchem are checked.
!                Reference values are determined independently (simple retrieve actions or simple multiplications by hand).
!                Subroutine ops_vchem_ratio_no2_nox is tested implicitly by testing ops_vchem.
! tst_ops_vchem_ratio_no2_nox_vdhout: test vd Hout formula for NO2/NOx ratio.
!===============================================================================================================

subroutine tst_ops_vchem

use no_pfunit
USE m_commonconst_lt, only: NSEK
use m_ops_varin, only: Tvarin_unc
use m_ops_vchem

implicit none

! SUBROUTINE ARGUMENTS - INPUT
INTEGER                              :: icm                        ! component number
LOGICAL                              :: isec                       ! TRUE if component=[SO2, NOx, NH3]
INTEGER                              :: iopt_vchem                 ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
REAL                                 :: vchemc                     ! chemical conversion rate [%/h]
REAL                                 :: vchemv                     ! light dependent part of chemical conversion rate
REAL                                 :: vchemnh3                   ! chemical conversion rate for NH3 -> NH4 [%/h]
REAL                                 :: rad                        ! global radiation [J/cm2/h]
REAL                                 :: rad_W_m2                   ! global radiation [W/m2]
REAL                                 :: regenk                     ! rain probability [-]
INTEGER                              :: iseiz                      ! 0 = long term, 1 = year, 2 = winter, 3 = summer, 4 = month in winter, 5 = month in summer
INTEGER                              :: istab                      ! index of stability class
INTEGER                              :: itra                       ! index of trajectory class
REAL                                 :: ar                         ! proportionality constant [ppb J-1 cm2 h] in relation [OH] = ar Qr
REAL                                 :: koh                        ! reaction constant [ppb-1 h-1] for NO2 + OH -> HNO3
INTEGER                              :: isec_prelim                ! index of preliminary source-receptor wind sector (wind shear not yet taken into account)
REAL                                 :: disxx                      ! effective travel distance between source and receptor [m]
REAL                                 :: r_no2_nox_sec(NSEK)        ! sector averaged NO2/NOx ratio according to vdHout parameterisation [-]
REAL                                 :: r_no2_nox_year_bg          ! component of NO2/NOx ratio which is based on yearly averaged background concentrations 
REAL                                 :: r_no2_nox_season           ! component of NO2/NOx ratio which is season dependent  
INTEGER                              :: ibroncat                   ! emission category number
INTEGER                              :: nemcat_road                ! number of road emission categories (for vdHout NO2/NOx ratio)
logical								 :: road_chem
INTEGER                              :: emcat_road(3)              ! list of road emission categories (for vdHout NO2/NOx ratio)
type(Tvarin_unc)                     :: varin_unc


! SUBROUTINE ARGUMENTS - OUTPUT
REAL                                 :: r_no2_nox                  ! NO2/NOx ratio [-]
LOGICAL                              :: lroad_corr                 ! road correction needed for NO2/NOx ratio
REAL                                 :: vchem                      ! chemical conversion rate [%/h]

! Local
REAL                                 :: tola = 1.0e-5              ! absolute tolerance for equality of two reals
REAL                                 :: r_no2_nox_ref(NSEK)        ! reference data for sector averaged NO2/NOx ratio

!-----------------------------------------------------------------------------
! Unit tests for vchem
!-----------------------------------------------------------------------------
! Inputs for NO2/NOx ratio (are changed in later tests):
disxx = -999.0
iseiz = -999
istab = -999
r_no2_nox_year_bg = -999.0
r_no2_nox_season  = -999.0 
emcat_road = (/ 3100, 3200, 3300 /)
nemcat_road = 3
road_chem = .TRUE.
ibroncat = -999

! Non-acidifying species; vchem = vchemc + vchemv*rad_W_m2
isec       = .false.          ! TRUE if component=[SO2, NOx, NH3]
vchemc     = 5                ! chemical conversion rate [%/h]
vchemv     = 0.01             ! light dependent part of chemical conversion rate
rad        = 100.0            ! global radiation [J/cm2/h]
rad_W_m2   = rad*2.78         ! global radiation [W/m2]
call ops_vchem(icm, isec, iopt_vchem, vchemc, vchemv, vchemnh3, rad, rad_W_m2, regenk, iseiz, istab, itra, ar, koh, &
               isec_prelim, disxx, r_no2_nox_sec, r_no2_nox_year_bg, r_no2_nox_season, ibroncat, nemcat_road, road_chem, emcat_road, varin_unc, r_no2_nox, lroad_corr, vchem)
call assertEqual(7.78,vchem, tola, 'ops_vchem/assert 1',__LINE__,__FILE__)
                       
! EMEP conversion rate; 
isec         = .true.         ! TRUE if component=[SO2, NOx, NH3]
iopt_vchem   = 1              ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
vchem        = 0.12345        ! structure for chemical conversion rates, vchem = chemical conversion rate
call ops_vchem(icm, isec, iopt_vchem, vchemc, vchemv, vchemnh3, rad, rad_W_m2, regenk, iseiz, istab, itra, ar, koh, &
               isec_prelim, disxx, r_no2_nox_sec, r_no2_nox_year_bg, r_no2_nox_season, ibroncat, nemcat_road, road_chem, emcat_road, varin_unc, r_no2_nox, lroad_corr, vchem)
call assertEqual(0.12345,vchem, tola, 'ops_vchem/assert 2',__LINE__,__FILE__)

! old OPS conversion rate NH3; vchem = vchemnh3
iopt_vchem = 0                ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
icm        = 3                ! component number; 3 = NH3
vchemnh3   = 6.54321          ! chemical conversion rate for NH3 -> NH4 [%/h]
call ops_vchem(icm, isec, iopt_vchem, vchemc, vchemv, vchemnh3, rad, rad_W_m2, regenk, iseiz, istab, itra, ar, koh, &
               isec_prelim, disxx, r_no2_nox_sec, r_no2_nox_year_bg, r_no2_nox_season, ibroncat, nemcat_road, road_chem, emcat_road, varin_unc, r_no2_nox, lroad_corr, vchem)
call assertEqual(6.54321,vchem, tola, 'ops_vchem/assert 3',__LINE__,__FILE__)

! old OPS conversion rate SO2; vchem = 1.2*((rad*.016) + .5 + (regenk*12.))
iopt_vchem = 0                ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
icm        = 1                ! component number; 1 = SO2
regenk     = 0.1              ! rain probability [-]
call ops_vchem(icm, isec, iopt_vchem, vchemc, vchemv, vchemnh3, rad, rad_W_m2, regenk, iseiz, istab, itra, ar, koh, &
               isec_prelim, disxx, r_no2_nox_sec, r_no2_nox_year_bg, r_no2_nox_season, ibroncat, nemcat_road, road_chem, emcat_road, varin_unc, r_no2_nox, lroad_corr, vchem)

! vchem = 1.2*((rad*.016) + .5 + (regenk*12.)) = vchem = 1.2*(1.6 + .5 + 1.2) = 3.96
call assertEqual(3.96,vchem, tola, 'ops_vchem/assert 4',__LINE__,__FILE__)

!-----------------------------------------------------------------------------
! Unit tests for r_no2_nox, based on background concentrations and vchem(NOx);
! this is also a test of ops_vchem_ratio_no2_nox.
!-----------------------------------------------------------------------------
! old OPS conversion rate NOx; vchem = frac_night_hours*2. + 100*rad*ar*koh*r_no2_nox
icm         = 2                ! component number; 2 = NOx
isec        = .true.           ! TRUE if component=[SO2, NOx, NH3]
iopt_vchem  = 0                ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
iseiz       = 1                ! 0 = long term, 1 = year, 2 = winter, 3 = summer, 4 = month in winter, 5 = month in summer
istab       = 3                ! index of stability class
itra        = 1                ! index of trajectory class
ar          = 61.4e-8          ! proportionality constant [ppb J-1 cm2 h] in relation [OH] = ar Qr
koh         = 1020.*0.9        ! reaction constant [ppb-1 h-1] for NO2 + OH -> HNO3
disxx       = 10000            ! source-receptor distance (must be larger than dist_road_corr in order to get old parametrisation);
isec_prelim = -999             ! wind sector (preliminary) - only needed for vd Hout
r_no2_nox_sec     = -999       ! average NO2/NOx ratio per wind sector - only needed for vd Hout
r_no2_nox_season  = .65        ! yearly average NO2/NOx ratio (iseiz = 1)
r_no2_nox_year_bg = 0.8        ! NO2/NOx ratio from background grid; chosen arbitrarily
call ops_vchem(icm, isec, iopt_vchem, vchemc, vchemv, vchemnh3, rad, rad_W_m2, regenk, iseiz, istab, itra, ar, koh, &
               isec_prelim, disxx, r_no2_nox_sec, r_no2_nox_year_bg, r_no2_nox_season, ibroncat, nemcat_road, road_chem, emcat_road, varin_unc, r_no2_nox, lroad_corr, vchem)
! r_no2_nox = r_no2_nox_year_bg*r_no2_nox_season*scno2nox = 0.65*0.8*1 = 0.52
call assertEqual(0.52,r_no2_nox, tola, 'ops_vchem/assert 5a',__LINE__,__FILE__)

! frac_night_hours = FLOAT(NACHTWINTER(istab, itra) + NACHTZOMER(istab, itra))/200. = (61 + 66)/200 = 0.635 for (istab,itra) = (3,1)
! old OPS conversion rate NOx; vchem = frac_night_hours*2. + 100*rad*ar*koh*r_no2_nox = 0.635*2 + 100*100*61.4e-8*1020*0.9*0.52 = 4.20099
call assertEqual(4.20099,vchem, tola, 'ops_vchem/assert 5b',__LINE__,__FILE__)
call assertEqual(.false.,lroad_corr, 'tst_ops_vchem/assert 5c',__LINE__,__FILE__)

!-----------------------------------------------------------------------------
! Unit tests for r_no2_nox, based on background concentrations and vchem(NOx);
! this is also a test of ops_vchem_ratio_no2_nox. 
! Now choose a distance < dist_road_corr, but exclude vdHout because of non-road category; 
! this should give the same result as the previous test
!-----------------------------------------------------------------------------
disxx    = 500
ibroncat = 1000
lroad_corr = .false.
call ops_vchem(icm, isec, iopt_vchem, vchemc, vchemv, vchemnh3, rad, rad_W_m2, regenk, iseiz, istab, itra, ar, koh, &
               isec_prelim, disxx, r_no2_nox_sec, r_no2_nox_year_bg, r_no2_nox_season, ibroncat, nemcat_road, road_chem, emcat_road, varin_unc, r_no2_nox, lroad_corr, vchem)
! r_no2_nox = r_no2_nox_year_bg*r_no2_nox_season*scno2nox = 0.65*0.8*1 = 0.52
call assertEqual(0.52,r_no2_nox, tola, 'ops_vchem/assert 6a',__LINE__,__FILE__)

!-----------------------------------------------------------------------------
! Unit test for r_no2_nox, vd Hout formula;
! this is a test of ops_vchem_ratio_no2_nox.
!-----------------------------------------------------------------------------
! Distance < dist_road_corr and ibroncat present in emcat_road -> use vdHout formula, lroad_corr is set to true:
disxx = 200 
ibroncat = 3300
lroad_corr = .true.

! Reference data; for vdHout this routine is only a retrieve action:
r_no2_nox_sec = (/ 0.2102958  , 0.4581026  , 0.1788752  , 0.4611298  , 0.2371719  , 0.1577749  , 0.8061988  , 0.4250576  , &
                   0.3503025  , 0.3375960  , 0.2985530  , 0.4647051   /)
r_no2_nox_ref = r_no2_nox_sec

do isec_prelim = 1, NSEK
   call ops_vchem(icm, isec, iopt_vchem, vchemc, vchemv, vchemnh3, rad, rad_W_m2, regenk, iseiz, istab, itra, ar, koh, &
                  isec_prelim, disxx, r_no2_nox_sec, r_no2_nox_year_bg, r_no2_nox_season, ibroncat, nemcat_road, road_chem, emcat_road, varin_unc, r_no2_nox, lroad_corr, vchem)
    !write(*,*) 'isec_prelim, r_no2_nox: ',isec_prelim,r_no2_nox
    call assertEqual(r_no2_nox_ref(isec_prelim),r_no2_nox, tola, 'tst_ops_vchem/assert 7a',__LINE__,__FILE__)
    call assertEqual(.true.,lroad_corr, 'tst_ops_vchem/assert 7b',__LINE__,__FILE__)
enddo 

END SUBROUTINE tst_ops_vchem

!---------------------------------------------------------------------
subroutine tst_ops_vchem_ratio_no2_nox_vdhout

use no_pfunit
use m_ops_vchem
USE m_commonconst_lt, only: NSEK

implicit none

! Compute r_no2_nox = [NO2]/[NOx] ratio according to vd Hout parameterisation for roads.

! SUBROUTINE ARGUMENTS - INPUT
INTEGER                                        :: iter                       ! iteration counter
INTEGER, PARAMETER                             :: nrrcp = 1                  ! number of receptors - for TEST: nrrcp = 1
REAL                                           :: o3bg_rcp(NSEK,nrrcp)       ! O3 background concentration at receptor for all wind sectors [ug/m3] 
REAL                                           :: o3bgtra(NSEK)              ! O3 background concentration average over trajectory [ug/m3] 
INTEGER                                        :: nsrc_sec(NSEK,nrrcp)       ! number of sources present in wind sector (roads only) [-]

! SUBROUTINE ARGUMENTS - INPUT/OUTPUT
REAL                                           :: cnox_sec(NSEK,nrrcp)       ! wind sector averaged NOx concentration (roads only) [ug/m3]
REAL                                           :: cno2(nrrcp)                ! NO2 concentration (derived from NOx and parameterised ratio NO2/NOx) [ug/m3]
REAL                                           :: percvk_sec(NSEK,nrrcp)     ! frequency of occurrence of wind sector (roads only) [-]

! SUBROUTINE ARGUMENTS - OUTPUT
REAL                                           :: r_no2_nox_sec(NSEK,nrrcp)  ! sector averaged NO2/NOx ratio according to vdHout parameterisation [-]
REAL                                           :: cnox(nrrcp)                ! NOx concentration, per receptor [ug/m3]

! LOCAL
REAL                                           :: r_no2_nox_ref(NSEK,nrrcp)  ! reference data for r_no2_nox_sec
REAL                                           :: cno2_ref(nrrcp)            ! NO2 reference concentration 
REAL                                           :: tola = 1.0e-5              ! absolute tolerance for equality of two reals
REAL                                           :: tolr = 1.0e-3              ! relative tolerance for equality of two reals


! Set inputs

! iteration step:
iter = 1 ! only of interest if nsrc_sec .ne. 1

! Initial NO2:
cno2 = 0.0 

! Number of sources per wind sector and frequency of occurrence percvk do not play a role in this test:
nsrc_sec(:,1)   = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /)
percvk_sec(:,1) = (/  1.0  ,  1.0  ,  1.0  ,  1.0  ,  1.0  ,  1.0  ,  1.0  ,  1.0  ,  1.0  ,  1.0  ,  1.0  ,  1.0   /)
! percvk_sec      = percvk_sec/12;

! Data from spreadsheet SRM2ChemieConcept.xlsx Sjoerd van Ratingen 2020-09-22
cnox_sec(:,1)   = (/ 14.823, 67.307, 77.985, 29.769, 16.014, 12.524, 6.302 , 79.369, 75.583, 2.262, 103.703, 8.018  /)
o3bg_rcp(:,1)   = (/ 13.890, 63.892, 14.915, 50.876, 17.438,  7.143, 82.917, 61.917, 46.730, 26.937, 42.652, 43.447 /) 

! Reference data for f_dir_no2 = 0.15
r_no2_nox_ref(:,1) = (/ 0.254852329, 0.495445572, 0.226242077, 0.49511887 , 0.280461808, 0.204873581, 0.818959   , 0.464275299, & 
                        0.391835489, 0.374644901, 0.342690426, 0.495736825 /)
! Reference data for f_dir_no2 = 0.10
! r_no2_nox_ref(:,1) = (/ 0.2102958  , 0.4581026  , 0.1788752  , 0.4611298  , 0.2371719  , 0.1577749  , 0.8061988  , 0.4250576  , &
!                         0.3503025  , 0.3375960  , 0.2985530  , 0.4647051 /)

cno2_ref = sum(r_no2_nox_ref*cnox_sec) ! Only if percvk = 1                        

o3bgtra  = o3bg_rcp(:,1)

call ops_vchem_ratio_no2_nox_vdhout(iter,nrrcp,o3bg_rcp,o3bgtra,nsrc_sec,cnox_sec,cno2,percvk_sec,r_no2_nox_sec,cnox)

! Assert r_no2_nox_sec:
! write(*,*) 'r_no2_nox_sec: ',r_no2_nox_sec
call assertEqual(r_no2_nox_ref,r_no2_nox_sec, tola, 'tst_ops_vchem_ratio_no2_nox_vdhout/assert 1, rno2_nox_sec',__LINE__,__FILE__)

! Assert cno2:
!write(*,*) 'cno2: ',cno2
!write(*,*) 'cno2_ref: ',cno2_ref

call assertEqual(cno2_ref,cno2, 10*tola, 'tst_ops_vchem_ratio_no2_nox_vdhout/assert 2, cno2',__LINE__,__FILE__)



END SUBROUTINE tst_ops_vchem_ratio_no2_nox_vdhout

end module m_test_vchem

program tst_vchem
use m_test_vchem 
use no_pfunit
implicit none
   call test_ops_vchem_add_nox_no2()
   call tst_ops_vchem
   call tst_ops_vchem_ratio_no2_nox_vdhout
   call conclusion()
end program tst_vchem

