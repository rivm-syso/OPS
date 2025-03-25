!-------------------------------------------------------------------------------------------------------------------------------
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
!

module m_ops_resist_rek

!-------------------------------------------------------------------------------------------------------------------------------
! USAGE              : %M%
! DESCRIPTION        : Compute resistances for dry deposition, in-cloud scavenging ratio and chemical conversion rate.
!
! contains 
! ops_resist_rek                : computes all resistances needed for dry deposition + chemical conversion rate + in-cloud scavenging ratio; calls all routines below.
! ops_resist_ra_all             : compute aerodynamic resistances Ra at different locations and different heights
! ops_resist_ra                 : compute aerodynamic resistances Ra at a specific location and height
! ops_resist_rb_all             : compute laminar boundary layer resistances Rb at different locations
! ops_resist_rb                 : compute laminar boundary layer resistance Rb at a specific location
! ops_resist_rc_all             : compute canopy resistances Rc at different locations
! ops_resist_rc                 : compute canopy resistance Rc at a specific location [s/m] for acidifying components (SO2, NOx, NH3), calling DEPAC.
! ops_resist_rain_out_scav_ratio: compute rain out scavenging ratio
! ops_resist_rc_sec_trj         : compute aerodynamic resistance for secondary component for trajectory
! ops_resist_rcaer              : compute aerodynamic resistance for aerosols at the receptor
!-------------------------------------------------------------------------------------------------------------------------------

implicit none

contains

SUBROUTINE ops_resist_rek(varin_meteo, varin_unc, disxx, vchemc, iopt_vchem, vchemv, rad, isec, icm, rc_so2_ms, regenk, rc_aer_ms, iseiz, istab, itra, isec_prelim, ar, &
                          r_no2_nox_season, ibroncat, nemcat_road, road_chem, emcat_road, vchemnh3, hum, uster_rcp, ol_rcp, uster_tra, &
                          z0_rcp, z0_metreg_rcp, kdeel, mb, vw10, temp_C, zm, koh, &
                          rations, rhno3_trj, rc_no, rhno2, rc_hno3, croutpri, r_no2_nox_sec, r_no2_nox_year_bg_tra, rhno3_rcp, &
                          rb_ms, ra_ms_4, ra_ms_zra, rc_user, routpri, vchem, rc_sec_trj, uh, rc_sec_rcp, rc_eff_rcp_4_pos, rb_rcp, &
                          ra_rcp_4, ra_rcp_zra, ra_rcp_zrcp, z0_src, ol_src, uster_src, z0_tra, rc_eff_trj_4_pos, rc_eff_src_4_pos, ra_src_4, &
                          rb_src, ra_trj_4, ra_trj_zra, rb_trj, rc_eff_rcp_4, nh3bg_rcp, nh3bgtra, &
                          so2bg_rcp, so2bgtra, gw_rcp, gwtra, gym, depudone, gasv, lu_rcp_per, lu_tra_per, r_no2_nox, lroad_corr)

use m_ops_varin, only: Tvarin_meteo, Tvarin_unc
use m_commonconst_lt, only: EPS_DELTA, NSEK, icm_NH3, icm_SO2, icm_NOx
use m_ops_vchem
use m_ops_meteo
use m_commonconst_lib, only: NLU

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_resist_rek')

! SUBROUTINE ARGUMENTS - INPUT
TYPE(Tvarin_meteo), INTENT(IN)                   :: varin_meteo                ! input variables for meteo
TYPE(Tvarin_unc), INTENT(IN)                     :: varin_unc                  ! NOTE:
REAL,      INTENT(IN)                            :: disxx                      ! effective travel distance between source and receptor [m]
REAL,      INTENT(IN)                            :: vchemc                     ! chemical conversion rate [%/h]
INTEGER,   INTENT(IN)                            :: iopt_vchem                 ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
REAL,      INTENT(IN)                            :: vchemv                     ! light dependent part of chemical conversion rate
REAL,      INTENT(IN)                            :: rad                        ! global radiation [J/cm2/h]
LOGICAL,   INTENT(IN)                            :: isec                       ! TRUE if component=[SO2, NOx, NH3]
INTEGER,   INTENT(IN)                            :: icm                        ! component number
REAL,      INTENT(IN)                            :: rc_so2_ms                  ! Rc(SO2) from meteo statistics [s/m] 
REAL,      INTENT(IN)                            :: regenk                     ! rain probability [-]
REAL,      INTENT(IN)                            :: rc_aer_ms                  ! Rc(SO4-aerosol) from meteo statistics [s/m]
INTEGER,   INTENT(IN)                            :: iseiz                      ! 0 = long term, 1 = year, 2 = winter, 3 = summer, 4 = month in winter, 5 = month in summer
INTEGER,   INTENT(IN)                            :: istab                      ! index of stability class
INTEGER,   INTENT(IN)                            :: itra                       ! index of trajectory class
INTEGER,   INTENT(IN)                            :: isec_prelim                ! index of preliminary source-receptor wind sector (wind shear not yet taken into account)
REAL,      INTENT(IN)                            :: ar                         ! proportionality constant [ppb J-1 cm2 h] in relation [OH] = ar Qr
REAL,      INTENT(IN)                            :: r_no2_nox_sec(NSEK)        ! sector averaged NO2/NOx ratio according to vdHout parameterisation [-]
REAL,      INTENT(IN)                            :: r_no2_nox_season           ! component of NO2/NOx ratio which is season dependent [-]
INTEGER,   INTENT(IN)                            :: ibroncat                   ! emission category number
INTEGER,   INTENT(IN)                            :: nemcat_road                ! number of road emission categories (for vdHout NO2/NOx ratio)
INTEGER,   INTENT(IN)                            :: emcat_road(:)              ! list of road emission categories (for vdHout NO2/NOx ratio)
LOGICAL,   INTENT(IN)							 :: road_chem					 !switch for road chemistry GTHO
REAL,      INTENT(IN)                            :: vchemnh3                   ! chemical conversion rate for NH3 -> NH4 [%/h]
REAL,      INTENT(IN)                            :: hum                        ! relative humidity [%]
REAL,      INTENT(IN)                            :: uster_rcp                  ! friction velocity at receptor; for z0 at receptor [m/s]
REAL,      INTENT(IN)                            :: ol_rcp                     ! Monin-Obukhov length at receptor; for z0 at receptor [m/s]
REAL,      INTENT(IN)                            :: uster_tra                  ! friction velocity representative for trajectory [m/s]
REAL,      INTENT(IN)                            :: z0_rcp                     ! roughness length at receptor; from z0-map [m]
REAL,      INTENT(IN)                            :: z0_metreg_rcp              ! roughness length at receptor; interpolated from meteo regions [m]
INTEGER,   INTENT(IN)                            :: kdeel                      ! index of particle size class
INTEGER,   INTENT(IN)                            :: mb                         ! month number (only used for iseiz = 4,5)
REAL,      INTENT(IN)                            :: vw10                       ! wind speed at 10 m height [m/s]
REAL,      INTENT(IN)                            :: temp_C                     ! temperature at height zmet_T [C]
REAL,      INTENT(IN)                            :: zm                         ! z-coordinate of receptor point (m)
REAL,      INTENT(IN)                            :: koh                        ! reaction constant [ppb-1 h-1] for NO2 + OH -> HNO3
REAL,      INTENT(IN)                            :: rations                    ! NH3/SO2 ratio over trajectory 
REAL,      INTENT(IN)                            :: rhno3_trj                  ! ratio [HNO3]/[NO3_total] for trajectory [-]
REAL,      INTENT(IN)                            :: rc_no                      ! canopy resistance for NO [s/m]
REAL,      INTENT(IN)                            :: rhno2                      ! ratio HNO2/NOx [-]
REAL,      INTENT(IN)                            :: rc_hno3                    ! canopy resistance for HNO3 [s/m]
REAL,      INTENT(IN)                            :: croutpri                   ! constant (initial) in-cloud scavenging ratio [-] for primary component                   
REAL,      INTENT(IN)                            :: r_no2_nox_year_bg_tra      ! component of NO2/NOx ratio which is based on yearly averaged background concentrations over a trajectory
REAL,      INTENT(IN)                            :: rhno3_rcp                  ! ratio [HNO3]/[NO3_totaal] at the receptor [-]
REAL,      INTENT(IN)                            :: z0_src                     ! roughness length at source; from z0-map [m]
REAL,      INTENT(IN)                            :: ol_src                     ! Monin-Obukhov length at source [m]
REAL,      INTENT(IN)                            :: uster_src                  ! friction velocity at source [m/s]
REAL,      INTENT(IN)                            :: z0_tra                     ! roughness length representative for trajectory [m]
REAL,      INTENT(IN)                            :: nh3bg_rcp                  ! background NH3-concentration at receptor [ug/m3]
REAL,      INTENT(IN)                            :: nh3bgtra                   ! background NH3-concentration over trajectory [ug/m3]
REAL,      INTENT(IN)                            :: so2bg_rcp                  ! background SO2-concentration at receptor [ug/m3]
REAL,      INTENT(IN)                            :: so2bgtra                   ! background SO2-concentration over trajectory [ug/m3]
REAL,      INTENT(IN)                            :: gw_rcp                     ! gamma water value at receptor
REAL,      INTENT(IN)                            :: gwtra                      ! gamma water value over trajectory
REAL,      INTENT(IN)                            :: gym                        ! y-coordinate of receptor (lon-lat) [degrees]
LOGICAL,   INTENT(IN)                            :: gasv                       ! TRUE for gasuous component
REAL,      INTENT(IN)                            :: lu_rcp_per(NLU)            ! land use percentages for all land use classes of receptor
REAL,      INTENT(IN)                            :: lu_tra_per(NLU)            ! land use percentages for all land use classes over trajectory
REAL,      INTENT(IN)                            :: rc_user                    ! canopy resistance specified by user in control file [s/m]
REAL,      INTENT(IN)                            :: ra_ms_4                    ! aerodynamic resistance at 4 m from meteo statistics [s/m]
REAL,      INTENT(IN)                            :: ra_ms_zra                  ! aerodynamic resistance at height zra from meteo statistics [s/m] 

! SUBROUTINE ARGUMENTS - I/O
REAL,      INTENT(INOUT)                         :: rb_ms                      ! (in) boundary layer resistance SO2 from meteo statistics [s/m]
                                                                               ! (out) boundary layer resistance SO2 or NH3 from meteo statistics [s/m]
LOGICAL,   INTENT(INOUT)                         :: depudone                   ! Ra, Rb have been computed (no need to repeat this for all particle size classes)
                                                                               
! SUBROUTINE ARGUMENTS - I/O aerodynamid (Ra) and boundary layer (Rb) resistances
! Ra, Rb values for particles (.not. gasv) are only computed for the first particle size class (intent(OUT) and for the 
! next classes they use the values from the call of the first class (intent(IN))
REAL,      INTENT(INOUT)                         :: ra_src_4                   ! aerodynamic resistance at source, 4 m height [s/m] 
REAL,      INTENT(INOUT)                         :: rb_src                     ! boundary layer resistance at source [s/m] 

REAL,      INTENT(INOUT)                         :: ra_trj_4                   ! aerodynamic resistance for trajectory, 4 m height [s/m] 
REAL,      INTENT(INOUT)                         :: ra_trj_zra                 ! aerodynamic resistance for trajectory, height zra [s/m];
                                                                               ! zra is height where concentration profile is undisturbed by deposition = 50 m 
REAL,      INTENT(INOUT)                         :: rb_trj                     ! boundary layer resistance for trajectory [s/m]

REAL,      INTENT(INOUT)                         :: ra_rcp_zrcp                ! aerodynamic resistance at receptor, height zrcp [s/m]
REAL,      INTENT(INOUT)                         :: ra_rcp_4                   ! aerodynamic resistance at receptor, 4 m height [s/m] 
REAL,      INTENT(INOUT)                         :: ra_rcp_zra                 ! aerodynamic resistance at receptor, height zra [s/m];
                                                                               ! zra is height where concentration profile is undisturbed by deposition = 50 m
REAL,      INTENT(INOUT)                         :: rb_rcp                     ! boundary layer resistance at receptor [s/m] 


! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(INOUT)                         :: routpri                    ! in-cloud scavenging ratio [-] for primary component (rout << rain-out = in-cloud) 
                                                                               ! NB: INOUT, because routpri keeps its original value when gasv .and. .not. isec
REAL,      INTENT(INOUT)                         :: vchem                      ! chemical conversion rate [%/h]
REAL,      INTENT(OUT)                           :: uh                         ! wind speed used in parametrisation of vd for aerosols [m/s]
REAL,      INTENT(INOUT)                         :: r_no2_nox                  ! NO2/NOx ratio
                                                                               ! NB: INOUT, because r_no2_nox keeps its original 
                                                                               !     value when icm/=icm_NOx
LOGICAL,   INTENT(INOUT)                         :: lroad_corr                 ! road correction needed for NO2/NOx ratio
                                                                               ! NB: INOUT, because lroad_corr keeps its original 
                                                                               !     value when icm /= icm_NOx

! Canopy resistances Rc
REAL,      INTENT(OUT)                           :: rc_eff_src_4_pos           ! effective canopy resistance at source, 4 m height, no re-emission (Rc is positive) [s/m]; 
                                                                               !   used for the computation of cq1 = source depletion ratio for all removal processes for phase 1 (area source) 
REAL,      INTENT(OUT)                           :: rc_eff_trj_4_pos           ! effective canopy resistance for trajectory, 4 m height, no re-emission (Rc is positive) [s/m]
                                                                               !   used for source depletion (loss over trajectory)
REAL,      INTENT(OUT)                           :: rc_eff_rcp_4_pos           ! effective canopy resistance at receptor, 4 m height, no re-emission (Rc is positive) [s/m];
                                                                               !   used for deposition gradient at receptor
REAL,      INTENT(OUT)                           :: rc_eff_rcp_4               ! effective canopy resistance at receptor, 4 m height, re-emission allowed [s/m];  
                                                                               !   used for the computation of drypri, the deposition at the receptor
! Rc for secondary component:
REAL,      INTENT(INOUT)                         :: rc_sec_trj                 ! canopy resistance secondary aerosol (SO4, NO3, NH4) for trajectory [s/m]
                                                                               ! NB: INOUT, because rc_sec_trj keeps its original 
                                                                               !     value when .not.isec
REAL,      INTENT(INOUT)                         :: rc_sec_rcp                 ! canopy resistance secondary aerosol (SO4, NO3, NH4) at the receptor [s/m]
                                                                               ! NB: INOUT, because rc_sec_rcp keeps its original 
                                                                               !     value when .not.isec
                                                                               
! LOCAL VARIABLES
INTEGER                                          :: icmpsec                    ! component number secondary component used in ops_resist_rek_rcaer
INTEGER                                          :: nwet                       ! wetness indicator depending on percipitation probability and humidity
REAL                                             :: rad_W_m2                   ! global radiation [W/m2]
REAL,   PARAMETER                                :: zra = 50.0                 ! zra is height where concentration profile is undisturbed by deposition = 50 m
!-------------------------------------------------------------------------------------------------------------------------------

! Wind speed for rough surfaces, such as forests (z0 > 0.5 m) -> from logarithmic wind profile at 8*z0(receptor);
! for smoother surfaces (z0 <= 0.5) -> set equal to wind speed at 10 m height.
IF (z0_rcp .GT. 0.5 + EPS_DELTA) THEN
   CALL ops_wv_log_profile(z0_rcp,8*z0_rcp,uster_rcp,ol_rcp, varin_meteo, uh)
ELSE
   uh = vw10
ENDIF

! Convert global radiation from J/cm2/h to J/m2/s = W/m2; conversion factor = 1.0e4/3600 = 2.78
rad_W_m2 = rad*2.78

!  Compute nwet = wetness indicator depending on percipitation probability and humidity;
!                 dry(=0),wet(=1) or snow(=9); 6.25 OPS report
nwet = NINT((regenk * 0.4 + hum/59 - 0.4)**5*.3)                              ! 990324
IF(nwet.gt.1) nwet=1

! Compute NO2/NOx ratio and chemical conversion rate:
call ops_vchem(icm, isec, iopt_vchem, vchemc, vchemv, vchemnh3, rad, rad_W_m2, regenk, iseiz, istab, itra, ar, koh, &
               isec_prelim, disxx, r_no2_nox_sec, r_no2_nox_year_bg_tra, r_no2_nox_season, ibroncat, nemcat_road, road_chem, emcat_road, varin_unc, r_no2_nox, lroad_corr, vchem)
               
! Continue for a gas and in case of particles if depudone is false (to prevent repeating this for each particle size class):
if ( gasv .OR. (.not.gasv .AND. .not.depudone)) THEN  ! this is equivalent with (gasv .OR. (.not.depudone))

   ! Compute aerodynamic resistance at different locations (src, trj, rcp) and different heights (4 m, zm=zrcp, zra):
   call ops_resist_ra_all(zm,zra,z0_src,z0_tra,z0_rcp,z0_metreg_rcp,ol_src,ol_rcp,uster_src,uster_rcp,ra_ms_4,ra_ms_zra, &
                             ra_src_4,ra_trj_4,ra_trj_zra,ra_rcp_zrcp,ra_rcp_4,ra_rcp_zra)

   ! Compute boundary layer resistance at different locations (src, trj, rcp):
   call ops_resist_rb_all(icm,uster_src,uster_tra,uster_rcp,rb_src,rb_trj,rb_rcp)
   
   ! Deposition parameters have been computed
   depudone = .TRUE.
ENDIF ! (gasv .OR. (.not.depudone))

! Compute rb_ms for NH3  



IF (icm == icm_NH3) THEN
   !                                                   Rb(NH3)     M(NH3)  1/3   17  1/3
   ! rb_ms  : boundary layer resistance rb_ms [s/m];  -------- = (--------)  = (----)  = 0.6431
   !          (4.5 OPS report)                         Rb(SO2)     M(SO2)        64
   
   rb_ms = rb_ms*0.64
ENDIF

! Compute routpri = in-cloud scavenging ratio for primary component; (rout << rain-out = in-cloud) [-])
call ops_resist_rain_out_scav_ratio(icm,gasv,isec,kdeel,croutpri,rations,r_no2_nox,routpri, varin_unc)   

! Compute canopy resistances at different locations and different heights (effective Rc may depend on height via Ra):
call ops_resist_rc_all(icm,isec,gasv,kdeel,iseiz,mb,gym,uster_tra,uster_rcp, &
                       nwet,hum,temp_C,rad_W_m2,lu_tra_per,lu_rcp_per,so2bgtra,nh3bgtra,so2bg_rcp,nh3bg_rcp, gwtra, gw_rcp, &
                       ra_ms_4,ra_trj_4,ra_rcp_4,rb_trj,rb_ms,rb_rcp,rc_user,rc_so2_ms,rc_no,r_no2_nox,rhno2, &
                       rc_eff_src_4_pos,rc_eff_trj_4_pos,rc_eff_rcp_4,rc_eff_rcp_4_pos)

IF (isec) THEN
  
   ! Compute rc_sec_trj = canopy resistance secondary aerosol representative for trajectory:
   call ops_resist_rc_sec_trj(icm,ra_ms_4,rb_ms,rc_aer_ms,rc_hno3,rhno3_trj,rc_sec_trj)   
                  
   !---------------------------------------------------------------------------------------------------
   !  Compute rc_sec_rcp = canopy resistance secondary aerosol (SO4, NO3, NH4) at the receptor [s/m]
   !---------------------------------------------------------------------------------------------------
   
   !  Set component number for secondary component of SO2, NO2, NH3:
   !  10 + icm_SO2: sulphate
   !  10 + icm_NOx: nitrate
   !  10 + icm_NH3: ammonium
   !  and set a default secondary component = 10 + icm_SO2 (note that in this IF-branch, isec = .true., so icm = icm_SO2,icm_NOx,icm_NH3)
   !
   IF (ANY(icm == (/icm_SO2,icm_NOx,icm_NH3/))) THEN
      icmpsec = 10 + icm
   ELSE
      icmpsec = 10 + icm_SO2
   ENDIF

   ! Compute rc_sec_rcp = canopy resistance secondary aerosol (SO4, NO3, NH4) at the receptor [s/m]:
   CALL ops_resist_rcaer (icmpsec, z0_rcp, uster_rcp, ol_rcp, hum, nwet, uh, ra_ms_4, rb_ms, rc_hno3, rhno3_rcp, rc_sec_rcp)   

ENDIF

RETURN

END SUBROUTINE ops_resist_rek

!-----------------------------------------------------------------------------------------------------
SUBROUTINE ops_resist_ra_all(zm,zra,z0_src,z0_tra,z0_rcp,z0_metreg_rcp,ol_src,ol_rcp,uster_src,uster_rcp,ra_ms_4,ra_ms_zra, &
                             ra_src_4,ra_trj_4,ra_trj_zra,ra_rcp_zrcp,ra_rcp_4,ra_rcp_zra)

! Compute aerodynamic resistances Ra at different locations (src, trj, rcp)

! SUBROUTINE ARGUMENTS - INPUT
REAL,      INTENT(IN)                            :: zm                         ! z-coordinate of receptor point (m)
REAL,      INTENT(IN)                            :: zra                        ! zra is height where concentration profile is undisturbed by deposition = 50 m
REAL,      INTENT(IN)                            :: z0_src                     ! roughness length at source; from z0-map [m]
REAL,      INTENT(IN)                            :: z0_tra                     ! roughness length representative for trajectory [m]
REAL,      INTENT(IN)                            :: z0_rcp                     ! roughness length at receptor; from z0-map [m]
REAL,      INTENT(IN)                            :: z0_metreg_rcp              ! roughness length at receptor; interpolated from meteo regions [m]
REAL,      INTENT(IN)                            :: ol_src                     ! Monin-Obukhov length at source [m]
REAL,      INTENT(IN)                            :: ol_rcp                     ! Monin-Obukhov length at receptor; for z0 at receptor [m/s]
REAL,      INTENT(IN)                            :: uster_src                  ! friction velocity at source [m/s]
REAL,      INTENT(IN)                            :: uster_rcp                  ! friction velocity at receptor; for z0 at receptor [m/s]
REAL,      INTENT(IN)                            :: ra_ms_4                    ! aerodynamic resistance at 4 m from meteo statistics [s/m]
REAL,      INTENT(IN)                            :: ra_ms_zra                  ! aerodynamic resistance at height zra from meteo statistics [s/m] 
                                                                               
! SUBROUTINE ARGUMENTS - OUTPUT aerodynamic resistances Ra
REAL,      INTENT(OUT)                           :: ra_src_4                   ! aerodynamic resistance at source, 4 m height [s/m] 
REAL,      INTENT(OUT)                           :: ra_trj_4                   ! aerodynamic resistance for trajectory, 4 m height [s/m] 
REAL,      INTENT(OUT)                           :: ra_trj_zra                 ! aerodynamic resistance for trajectory, height zra [s/m];
                                                                               ! zra is height where concentration profile is undisturbed by deposition = 50 m 
REAL,      INTENT(OUT)                           :: ra_rcp_zrcp                ! aerodynamic resistance at receptor, height zrcp [s/m]
REAL,      INTENT(OUT)                           :: ra_rcp_4                   ! aerodynamic resistance at receptor, 4 m height [s/m] 
REAL,      INTENT(OUT)                           :: ra_rcp_zra                 ! aerodynamic resistance at receptor, height zra [s/m];
                                                                               ! zra is height where concentration profile is undisturbed by deposition = 50 m
! LOCAL VARIABLES
REAL                                             :: d                          ! displacement height [m]

! displacement height (usually 0.7 * vegetation height) (m)

d = 0.

! Calculate aerodynamic resistance Ra at receptor/receptor height, source/4 m height, 
! receptor/4 m height and receptor/zra;
! zra is height where concentration profile is undisturbed by deposition = 50 m
CALL ops_resist_ra(z0_src,  4., d, ol_src, uster_src, ra_src_4)
CALL ops_resist_ra(z0_rcp,  zm, d, ol_rcp, uster_rcp, ra_rcp_zrcp)
CALL ops_resist_ra(z0_rcp,  4., d, ol_rcp, uster_rcp, ra_rcp_4)
CALL ops_resist_ra(z0_rcp, zra, d, ol_rcp, uster_rcp, ra_rcp_zra)

! Compute Ra averaged over trajectory at 4 m height and zra = 50 m height; 
! ra_ms_4 = Ra(4 m) from meteo statistics, based on z0_metreg_rcp, which is interpolated from meteo regions.
! 4.3 OPS report, neglecting the stability corrections psi_h and assuming that u* remains the same.
!
!  Ra(z=4,trajectory)       z=4              z=4
! ------------------- = ln(-------) / ln(-------------)
!    Ra(z=4)               z0_tra        z0_metreg_rcp
   
ra_trj_4   = ra_ms_4   * alog( 4./z0_tra)/alog( 4./z0_metreg_rcp)
ra_trj_zra = ra_ms_zra * alog(zra/z0_tra)/alog(zra/z0_metreg_rcp)

END SUBROUTINE ops_resist_ra_all

!-----------------------------------------------------------------------------------------
SUBROUTINE ops_resist_ra(z0, z_dep, d, ol, uster, ra)

! AUTHOR             : J.W. mrt 1990/ Franka Loeve (Cap Volmac)
! DESCRIPTION        : This routine calculates aerodynamic resistance Ra [s/m]
!                      the aerodynamic resistance over a layer with height z is the integral from z0 to z of
!                      1/K(z)dz with K(z)=k.u*.z/phi(h) and phi(h)=(1-16z/l)**-0.5 for l<0 and 1.+5.(z/l) for l>0. (Dyer and
!                      Hicks (1977) relations)
!                      By integration we get ra=1./(k.u*).(ln(z0/z)+ghi(z0)-ghi(z)) with ghi(z), the stability correction for
!                      heat (Beljaars et al., 1989).
!
!                      References:
!                      1. Businger J.A. (1973) in: workshop on micrometeorology
!                         Atm. Meteor. Soc.
!                      2. Wesely M.L. and Hicks B.B. (1977)
!                         Some factors that effect the deposition rates on so2 and simular gases on vegetation.
!                         J. Air Pollut. Contr. Org. 27 no.11, pp 1110-1116.
!                      3. Hicks B.B., Baldocchi D.D., Meyers T.P., Hosker Jr. R.P., Matt D.R. (1987)
!                         A preliminary multiple resistance routine for deriving dry deposition velocities from measured
!                         quantities.
!                         J. Water, Air and Soil Pollut.,36,311-330.
!                      4. Erisman J.W., Versluis A.H., Verplanke T.A.J.W., de Haan D., Anink D., van Elzakker B.G. and van Aalst
!                         R.M. (1990)
!                         Monitoring the dry deposition of SO2 in the Netherlands.
!                         Report nr. 228601002, RIVM, Bilthoven, the Netherlands.
!                      5. Beljaars A.C.M., Holtslag A.A.M. and Westrhenen R.M. van (1989)
!                         Description oa a software library for the calculation of surface fluxes.
!                         Technical report TR-112, KNMI, De Bilt.

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_resist_ra')

! CONSTANTS
REAL                                             :: VONK                       ! Von Karman constant
PARAMETER   (VONK = 0.4  )

! SUBROUTINE ARGUMENTS - INPUT
REAL,      INTENT(IN)                            :: z0                         ! surface roughness length in meters.
REAL,      INTENT(IN)                            :: z_dep                      ! height for which deposition velocity is calculated (m)
REAL,      INTENT(IN)                            :: d                          ! displacement height (usually 0.7 * vegetation height) (m)
REAL,      INTENT(IN)                            :: ol                         ! monin-obukhov length (m)
REAL,      INTENT(IN)                            :: uster                      ! friction velocity u* (m/s)

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: ra                         ! aerodynamic resistance at z_dep (s/m)

! LOCAL VARIABLES
REAL                                             :: zru                        ! height, corrected for displacement height

! Correction for the displacement height:
zru = z_dep - d

! Compute aerodynamic resistance Ra:
ra = (1./(VONK*uster))*(alog((zru)/z0) - fpsih(zru/ol) + fpsih(z0/ol))

END SUBROUTINE ops_resist_ra

!-----------------------------------------------------------------------------------------------------
SUBROUTINE ops_resist_rb_all(icm,uster_src,uster_tra,uster_rcp,rb_src,rb_trj,rb_rcp)

use m_commonconst_lt, only: icm_NH3

! Compute laminar boundary layer resistances Rb at different locations (src, trj, rcp)

! SUBROUTINE ARGUMENTS - INPUT
INTEGER, INTENT(IN)                              :: icm                        ! component number
REAL,    INTENT(IN)                              :: uster_src                  ! friction velocity at source [m/s]
REAL,    INTENT(IN)                              :: uster_tra                  ! friction velocity representative for trajectory [m/s]
REAL,    INTENT(IN)                              :: uster_rcp                  ! friction velocity at receptor; for z0 at receptor [m/s]
                                   
! SUBROUTINE ARGUMENTS - OUTPUT    
REAL,    INTENT(OUT)                             :: rb_src                     ! boundary layer resistance at source [s/m] 
REAL,    INTENT(OUT)                             :: rb_trj                     ! boundary layer resistance for trajectory [s/m]
REAL,    INTENT(OUT)                             :: rb_rcp                     ! boundary layer resistance at receptor [s/m] 

! LOCAL VARIABLES
INTEGER                                          :: icnr                       ! component number for calculation of rb

! 
! Component number icnr for calculation of Rb;
! Rb currently only for SO2 or NH3; for NO2 Rc values are high and Rb not so important.
IF (icm == icm_NH3) THEN
   icnr = 9
ELSE
   icnr = 1
ENDIF

! Calculate boundary layer resistance Rb at source and receptor:
CALL ops_resist_rb(icnr, uster_src, rb_src)
CALL ops_resist_rb(icnr, uster_rcp, rb_rcp)

! Compute Rb for trajectory 4.4 OPS report:
rb_trj = rb_rcp*uster_rcp/uster_tra

END SUBROUTINE ops_resist_rb_all

!-----------------------------------------------------------------------------------------
SUBROUTINE ops_resist_rb(icnr, uster, rb)

! AUTHOR             : J.W. mrt 1990/ Franka Loeve (Cap Volmac)
! DESCRIPTION        : This routine calculates boundary layer resistance Rb [s/m].
!                      The laminair boundary layer resistance : rb=2(ku*)**-1*(Sc/Pr)**p with Sc=Schmidt number, Pr=Prandtl
!                      number, p=2/3 by experiment.
!
!                      References:
!                      1. Businger J.A. (1973) in: workshop on micrometeorology
!                         Atm. Meteor. Soc.
!                      2. Wesely M.L. and Hicks B.B. (1977)
!                         Some factors that effect the deposition rates on so2 and simular gases on vegetation.
!                         J. Air Pollut. Contr. Org. 27 no.11, pp 1110-1116.
!                      3. Hicks B.B., Baldocchi D.D., Meyers T.P., Hosker Jr. R.P., Matt D.R. (1987)
!                         A preliminary multiple resistance routine for deriving dry deposition velocities from measured
!                         quantities.
!                         J. Water, Air and Soil Pollut.,36,311-330.
!                      4. Erisman J.W., Versluis A.H., Verplanke T.A.J.W., de Haan D., Anink D., van Elzakker B.G. and van Aalst
!                         R.M. (1990)
!                         Monitoring the dry deposition of SO2 in the Netherlands.
!                         Report nr. 228601002, RIVM, Bilthoven, the Netherlands.
!                      5. Beljaars A.C.M., Holtslag A.A.M. and Westrhenen R.M. van (1989)
!                         Description oa a software library for the calculation of surface fluxes.
!                         Technical report TR-112, KNMI, De Bilt.
!

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_resist_rb')

! CONSTANTS
REAL                                             :: P                          ! exponent (experimenteel bepaald)
REAL                                             :: PR                         ! Prandtl number
REAL                                             :: VONK                       ! Von Karman constante

PARAMETER   (VONK = 0.4  )
PARAMETER   (P    = 2./3.)
PARAMETER   (PR   = 0.72 )

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: icnr                       ! component number for calculation of rb
REAL,      INTENT(IN)                            :: uster                      ! friction velocity u* (m/s)

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: rb                         ! laminar layer resistance for component incr (s/m)

! LOCAL VARIABLES
REAL                                             :: sc                         ! Schmidt number


! Schmidt number:
IF (icnr .EQ. 1 .OR. icnr .LE. 0) THEN                                         ! SO2 or user-defined substance
   sc = 1.25
ELSE IF (icnr .EQ. 5) THEN                                                     ! O3
   sc = 0.88
ELSE IF (icnr .EQ. 6) THEN                                                     ! NO
   sc = 1.07
ELSE IF (icnr .EQ. 7) THEN                                                     ! NO2
   sc = 1.07
ELSE IF (icnr .EQ. 8) THEN                                                     ! HNO3
   sc = 1.25
ELSE                                                                           ! NH3  (op basis van molgewicht)
   sc = 0.69
ENDIF

! Compute laminar boundary layer resistance Rb:
rb = (2./(VONK*uster))*((sc/PR)**P)

END SUBROUTINE ops_resist_rb

!-------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_resist_rc_all(icm,isec,gasv,kdeel,iseiz,mb,gym,uster_tra,uster_rcp, &
                             nwet,hum,temp_C,rad_W_m2,lu_tra_per,lu_rcp_per,so2bgtra,nh3bgtra,so2bg_rcp,nh3bg_rcp, gwtra, gw_rcp, &
                             ra_ms_4,ra_trj_4,ra_rcp_4,rb_trj,rb_ms,rb_rcp,rc_user,rc_so2_ms,rc_no,r_no2_nox,rhno2, &
                             rc_eff_src_4_pos,rc_eff_trj_4_pos,rc_eff_rcp_4,rc_eff_rcp_4_pos)

use m_commonconst_lt, only: EPS_DELTA, NPARTCLASS, icm_NOx, icm_SO2, icm_NH3
use m_commonconst_lib, only: NLU

LOGICAL,   INTENT(IN)                            :: isec                       ! TRUE if component=[SO2, NOx, NH3]
INTEGER,   INTENT(IN)                            :: icm                        ! component number
REAL,      INTENT(IN)                            :: rc_so2_ms                  ! Rc(SO2) from meteo statistics [s/m] 
INTEGER,   INTENT(IN)                            :: iseiz                      ! 0 = long term, 1 = year, 2 = winter, 3 = summer, 4 = month in winter, 5 = month in summer
INTEGER,   INTENT(IN)                            :: nwet                       ! wetness indicator depending on percipitation probability and humidity
REAL,      INTENT(IN)                            :: hum                        ! relative humidity [%]
REAL,      INTENT(IN)                            :: uster_rcp                  ! friction velocity at receptor; for z0 at receptor [m/s]
REAL,      INTENT(IN)                            :: uster_tra                  ! friction velocity representative for trajectory [m/s]
INTEGER,   INTENT(IN)                            :: kdeel                      ! index of particle size class
INTEGER,   INTENT(IN)                            :: mb                         ! month number (only used for iseiz = 4,5)
REAL,      INTENT(IN)                            :: temp_C                     ! temperature at height zmet_T [C]
REAL,      INTENT(IN)                            :: rad_W_m2                   ! global radiation [W/m2]
REAL,      INTENT(IN)                            :: rc_no                      ! canopy resistance for NO [s/m]
REAL,      INTENT(IN)                            :: rhno2                      ! ratio HNO2/NOx [-]
REAL,      INTENT(IN)                            :: nh3bg_rcp                  ! background NH3-concentration at receptor [ug/m3]
REAL,      INTENT(IN)                            :: nh3bgtra                   ! background NH3-concentration over trajectory [ug/m3]
REAL,      INTENT(IN)                            :: so2bg_rcp                  ! background SO2-concentration at receptor [ug/m3]
REAL,      INTENT(IN)                            :: so2bgtra                   ! background SO2-concentration over trajectory [ug/m3]
REAL,      INTENT(IN)                            :: gw_rcp                     ! gamma water value at receptor
REAL,      INTENT(IN)                            :: gwtra                      ! gamma water value over trajectory
REAL,      INTENT(IN)                            :: gym                        ! y-coordinate of receptor (lon-lat) [degrees]
LOGICAL,   INTENT(IN)                            :: gasv                       ! TRUE for gasuous component
REAL,      INTENT(IN)                            :: lu_rcp_per(NLU)            ! land use percentages for all land use classes of receptor
REAL,      INTENT(IN)                            :: lu_tra_per(NLU)            ! land use percentages for all land use classes over trajectory
REAL,      INTENT(IN)                            :: rc_user                    ! canopy resistance specified by user in control file [s/m]
REAL,      INTENT(IN)                            :: r_no2_nox                  ! NO2/NOx ratio
REAL,      INTENT(IN)                            :: ra_ms_4                    ! aerodynamic resistance at 4 m from meteo statistics [s/m]
REAL,      INTENT(IN)                            :: ra_trj_4                   ! aerodynamic resistance for trajectory, 4 m height [s/m] 
REAL,      INTENT(IN)                            :: ra_rcp_4                   ! aerodynamic resistance at receptor, 4 m height [s/m] 
REAL,      INTENT(IN)                            :: rb_ms                      ! boundary layer resistance from meteo statistics [s/m]
REAL,      INTENT(IN)                            :: rb_trj                     ! boundary layer resistance for trajectory [s/m]
REAL,      INTENT(IN)                            :: rb_rcp                     ! boundary layer resistance at receptor [s/m] 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: rc_eff_src_4_pos           ! effective canopy resistance at source, 4 m height, no re-emission (Rc is positive) [s/m]; 
                                                                               !   used for the computation of cq1 = source depletion ratio for all removal processes for phase 1 (area source) 
REAL,      INTENT(INOUT)                           :: rc_eff_trj_4_pos           ! effective canopy resistance for trajectory, 4 m height, no re-emission (Rc is positive) [s/m]
                                                                               !   used for source depletion (loss over trajectory)
REAL,      INTENT(OUT)                           :: rc_eff_rcp_4_pos           ! effective canopy resistance at receptor, 4 m height, no re-emission (Rc is positive) [s/m];
                                                                               !   used for deposition gradient at receptor
REAL,      INTENT(OUT)                           :: rc_eff_rcp_4               ! effective canopy resistance at receptor, 4 m height, re-emission allowed [s/m];  
                                                                               !   used for the computation of drypri, the deposition at the receptor

! LOCAL VARIABLES
INTEGER                                          :: iratns                     ! N/S ratio indicator (nitrogen/sulphur) (1 = low, 2 = high, 3 = very low ratio; see DEPAC)
REAL                                             :: rc_dummy                   ! Rc value not used (dummy output) [s/m]
REAL                                             :: catm_trj, catm_rcp
REAL                                             :: c_ave_prev_nh3_trj, c_ave_prev_nh3_rcp 
REAL                                             :: c_ave_prev_so2_trj, c_ave_prev_so2_rcp 

! CONSTANTS
! RCDEEL: canopy resistance Rc per per particle class computed from Ra and Rb values per particle class and 
! averaged dry deposition velocities vd (weighed over stability classes).
! See new OPS report, Table 5.2
! See also definition of VDDEEL in ops_depoparexp 
REAL,   parameter :: RCDEEL(NPARTCLASS) = &    ! canopy resistance for particle size classes [s/m]
               (/3200., 700., 150., 50., 2., -17./)


! Compute canopy resistances Rc at different locations (src, trj, rcp) and different heights;
! note that the effective canopy resistance may depend on height via Ra
!
! rc_eff_src_4_pos: canopy resistance at the source, no re-emission allowed [s/m]; is used for the computation of 
!                   cq1 = source depletion ratio for all removal processes for phase 1 (area source) in ops_brondepl
! rc_eff_trj_4_pos: effective canopy resistance for trajectory, 4 m height, no re-emission allowed (Rc is positive) [s/m];
!                   is used for source depletion (loss over trajectory)
! rc_eff_rcp_4_pos: effective canopy resistance at receptor, 4 m height, no re-emission (Rc is positive) [s/m] 
!                   is used for deposition gradient at receptor
! rc_eff_rcp_4    : effective canopy resistance at receptor, 4 m height, re-emission allowed [s/m]; 
!                   is used for the computation of drypri, the deposition at the receptor


IF (isec) THEN

   ! Acidifying components
   
   ! N/S ratio indicator (nitrogen/sulphur) (1 = low, 2 = high, 3 = very low ratio; see DEPAC)
   iratns = 2

   !-----------------------------------------------------------------------         
   ! 1. Compute average canopy resistance Rc over trajectory (DEPAC).
   !-----------------------------------------------------------------------
   ! In case of NH3 a resistance scheme with compensationpoints is used for which two extra inputs are needed.
   ! c_ave_prev   air concentration averaged over a previous period (e.g. previous year or month) (ug/m3);
   !              we use here the NH3 background concentration along the trajectory
   ! catm         actual atmospheric concentration (ug/m3); we use here the NH3 background concentration along the trajectory;
   ! the output Rc is returned in rc_eff_trj_4_pos = effective Rc over the trajectory (positive value, no net re-emission allowed)
   ! 
   ! (17/24) = conversion factor ppb -> ug/m3.
   catm_trj           = nh3bgtra*17/24
   c_ave_prev_nh3_trj = nh3bgtra*17/24
   IF (ANY(icm == (/icm_SO2,icm_NH3/))) c_ave_prev_so2_trj = so2bgtra*64/24
   
   !-----------------------------------------------------------------------         
   ! 2. Compute canopy resistance Rc near the receptor (DEPAC).
   !-----------------------------------------------------------------------         
   ! The same as above, but now we use the NH3 background concentration at the receptor as inputs;
   ! the output Rc is returned in rc_eff_rcp_4_pos = effective Rc near the receptor (always positive). 
   ! rc_eff_rcp_4 = effective Rc near the receptor (might become a negative value -> re-emission)
   ! Note: catm and c_ave_prev are only used for NH3.
   ! Conversion from ppb -> ug/m3 for nh3bg_rcp already done in ops_rcp_char.
   ! Note that DEPAC returns Rc(NO2) for icm = icm_NOx (NOx).
   catm_rcp           = nh3bg_rcp
   c_ave_prev_nh3_rcp = nh3bg_rcp
   c_ave_prev_so2_rcp = so2bg_rcp

  ! Compute 1. and 2. (these have been combined to save an expensive extra call to RC_GSTOM_EMB)
   call ops_resist_rc_trj_rcp(icm, iseiz, mb, gym, temp_C, rad_W_m2, hum, nwet, iratns, &  ! Generic inputs
                            & catm_trj, c_ave_prev_nh3_trj, c_ave_prev_so2_trj, gwtra, &  
                            & uster_tra, lu_tra_per, ra_trj_4, rb_trj, rc_eff_trj_4_pos, rc_dummy, &                             ! Tra inputs
                            & catm_rcp, c_ave_prev_nh3_rcp, c_ave_prev_so2_rcp, gw_rcp, &  
                            & uster_rcp, lu_rcp_per, ra_rcp_4, rb_rcp, rc_eff_rcp_4_pos, rc_eff_rcp_4)                           ! Rcp inputs
   rc_eff_src_4_pos = rc_eff_trj_4_pos !

   ! Compute canopy resistances for NOx mixture:
   IF (icm == icm_NOx) THEN     
      call ops_resist_rc_nox(ra_ms_4, rb_ms, rc_so2_ms, r_no2_nox, rc_no, rhno2, &
                             rc_eff_rcp_4_pos, rc_eff_trj_4_pos, rc_eff_rcp_4, rc_eff_src_4_pos)
   ENDIF
ELSE
   
   ! (.not. isec): non-acidifying components
   
   IF (.NOT.gasv) THEN
      ! Set canopy resistances Rc for particles at different locations (receptor, source, average over trajectory and rc_eff_rcp_4)
      ! (Rc's all the same) and set routpri = in-cloud scavenging ratio [-] for the current particle class:
      rc_eff_rcp_4_pos = RCDEEL(kdeel)
      rc_eff_src_4_pos = RCDEEL(kdeel)
      rc_eff_trj_4_pos = RCDEEL(kdeel)
      rc_eff_rcp_4     = RCDEEL(kdeel)
   ELSE

      ! Other non-acidifying components. Set all Rc-values to user specified value read from control-file:
      rc_eff_rcp_4_pos = rc_user
      rc_eff_trj_4_pos = rc_user
      rc_eff_src_4_pos = rc_user
      rc_eff_rcp_4     = rc_user
   ENDIF
ENDIF 
   
END SUBROUTINE ops_resist_rc_all

!-------------------------------------------------------------------
! ops_resist_rc_trj_rcp: compute canopy resistance for 2 inputs (trj, rcp) at the same time
!
! Compute canopy resistance Rc [s/m] for acidifying components (SO2, NOx, NH3), calling DEPAC;
! DEPAC returns Rc(NO2) for icm = icm_NOx (NOx).
! ops_resist_rc_trj_rcp uses one or more representative months and computes the average over different land use classes 
! (via averaging of deposition velocities).
!-------------------------------------------------------------------

SUBROUTINE ops_resist_rc_trj_rcp(icm, iseiz, mb, gym ,temp_C, rad_W_m2, hum, nwet, iratns, &
                      & catm1, c_ave_prev_nh31, c_ave_prev_so21, gamma_soil_water_fac1, &
                      & uster1, lu_per1, ra1, rb1, rc_eff_pos1, rc_eff1, &
                      & catm2, c_ave_prev_nh32, c_ave_prev_so22, gamma_soil_water_fac2, &
                      & uster2, lu_per2, ra2, rb2, rc_eff_pos2, rc_eff2)

use m_commonconst_lt, only: icm_NOx, icm_SO2, icm_NH3
use m_commonconst_lib, only: NLU, i_HNO3, i_NO, i_NO2, i_O3, i_SO2, i_NH3
use m_depac

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_resist_rc')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: icm                        ! component number (icm_SO2 = SO2, icm_NOx = NOx, icm_NH3 = NH3)
INTEGER,   INTENT(IN)                            :: iseiz                      ! 0 = long term, 1 = year, 2 = winter, 3 = summer, 4 = month in winter, 5 = month in summer
INTEGER,   INTENT(IN)                            :: mb                         ! month number (only used for iseiz = 4,5)
INTEGER,   INTENT(IN)                            :: nwet                       ! wetness indicator depending on percipitation probability and humidity
REAL,      INTENT(IN)                            :: hum                        ! relative humidity [%] 
REAL,      INTENT(IN)                            :: temp_C                     ! temperature at height zmet_T [C]
REAL,      INTENT(IN)                            :: gym                        ! y-coordinate of receptor (lon-lat) [degrees]
REAL,      INTENT(IN)                            :: rad_W_m2                   ! global radiation [W/m2]
INTEGER,   INTENT(IN)                            :: iratns                     ! N/S ratio indicator (nitrogen/sulphur) (1 = low, 2 = high, 3 = very low ratio; see DEPAC)
REAL,      INTENT(IN)                            :: catm1, catm2                      ! actual atmospheric concentration used in compensation point [ug/m3]
REAL,      INTENT(IN)                            :: c_ave_prev_nh31, c_ave_prev_nh32             ! NH3 air concentration averaged over a previous period (e.g. previous year or month) [ug/m3]
REAL,      INTENT(IN)                            :: c_ave_prev_so21, c_ave_prev_so22             ! SO2 air concentration averaged over a previous period (e.g. previous year or month) [ug/m3]
REAL,      INTENT(IN)                            :: uster1                      ! friction velocity [m/s]
REAL,      INTENT(IN)                            :: uster2                      ! friction velocity [m/s]
REAL,      INTENT(IN)                            :: ra1                         ! aerodynamic resistance [s/m]
REAL,      INTENT(IN)                            :: ra2                         ! aerodynamic resistance [s/m]
REAL,      INTENT(IN)                            :: rb1                         ! boundary layer resistance [s/m]
REAL,      INTENT(IN)                            :: rb2                         ! boundary layer resistance [s/m]
REAL,      INTENT(IN)                            :: lu_per1(NLU)                ! land use percentages for all land use classes [%]
REAL,      INTENT(IN)                            :: lu_per2(NLU)                ! land use percentages for all land use classes [%]
REAL,      INTENT(IN)                            :: gamma_soil_water_fac1       ! factor in linear relation between gamma_soil and NH3
REAL,      INTENT(IN)                            :: gamma_soil_water_fac2       ! If positive or zero, gamma_soil = gamma_soil_water_fac
                                                                                ! If negative, gamma_soil = abs(gamma_soil_water_fac) * c_ave_prev_nh3
                                                                                ! This replaces the fix by
                                                                                ! in runs with a single source. 
                                                                                ! In OPS LT, gamma_soil = gamma_soil_water_fac = 430 as before.



! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: rc_eff_pos1                 ! canopy resistance, no re-emission [s/m]  
REAL,      INTENT(OUT)                           :: rc_eff1                     ! canopy resistance, re-emission allowed [s/m];  
REAL,      INTENT(OUT)                           :: rc_eff_pos2                 ! canopy resistance, no re-emission [s/m]  
REAL,      INTENT(OUT)                           :: rc_eff2                     ! canopy resistance, re-emission allowed [s/m];  

! LOCAL VARIABLES
INTEGER                                          :: comp_id                    ! component id for DEPAC call
INTEGER                                          :: day_of_year                ! 
INTEGER                                          :: mnt                        ! 
INTEGER,   DIMENSION(2)                          :: mnt_select                 ! 
INTEGER                                          :: luclass                    ! 
REAL                                             :: som_vd_month1               ! summed vd over representative months
REAL                                             :: som_vd_eff_ave1              ! summed vd over land use classes (vd = 1/Ra + Rb + Rc_eff)
REAL                                             :: som_vd_eff_ave_pos1          ! summed vd over land use classes (vd = 1/Ra + Rb + Rc_eff_pos)
REAL                                             :: som_vd_month2               ! summed vd over representative months
REAL                                             :: som_vd_eff_ave2              ! summed vd over land use classes (vd = 1/Ra + Rb + Rc_eff)
REAL                                             :: som_vd_eff_ave_pos2          ! summed vd over land use classes (vd = 1/Ra + Rb + Rc_eff_pos)
REAL                                             :: telmaand 
REAL                                             :: rc_eff_ave1                  ! canopy resistance, re-emission allowed, averaged over representative months
REAL                                             :: rc_eff_ave_pos1              ! canopy resistance, no re-emission, averaged over representative months
REAL                                             :: rc_eff_ave2                  ! canopy resistance, re-emission allowed, averaged over representative months
REAL                                             :: rc_eff_ave_pos2              ! canopy resistance, no re-emission, averaged over representative months
REAL                                             :: sinphi
REAL,   PARAMETER                                :: catm_min = 0.1E-05
REAL                                             :: rc_eff_depac1                ! canopy resistance from depac, re-emission allowed [s/m];  
REAL                                             :: rc_eff_depac2                ! canopy resistance from depac, re-emission allowed [s/m];  
REAL                                             :: lai          ! one-sided leaf area index (-)
REAL                                             :: sai          ! surface area index (-) (lai + branches and stems)
LOGICAL                                          :: ready        ! Rc has been set
REAL                                             :: rsoil_wet    ! soil resistance for wet soil
REAL                                             :: rsoil_frozen ! soil resistance for frozen soil
REAL,   DIMENSION(nlu)                           :: rsoil        ! soil resistance
REAL                                             :: gw           ! external leaf conductance (m/s) 
REAL                                             :: gstom        ! stomatal conductance (m/s)
REAL                                             :: rc_tot
REAL                                             :: ccomp_tot
LOGICAL                                          :: run1
LOGICAL                                          :: run2
!-------------------------------------------------------------------------------------------------------------------------------

! Get DEPAC component id from component name
select case(icm)
case(icm_NOx)
   comp_id = i_NO2
case(icm_SO2)
   comp_id = i_SO2
case(icm_NH3)
   comp_id = i_NH3
end select

! Initialise sums:
som_vd_month1       = 0.0
som_vd_eff_ave1     = 0.0
som_vd_eff_ave_pos1 = 0.0
som_vd_month2       = 0.0
som_vd_eff_ave2     = 0.0
som_vd_eff_ave_pos2 = 0.0
   
! loop over land use classes:
DO luclass = 1,NLU

  ! determine if input1 (trj) and/or input2 (rcp) need to be run
  IF (lu_per1(luclass) /= 0.0) THEN
    run1 = .TRUE. 
  ELSE
    run1 = .FALSE.
  ENDIF
  IF (lu_per2(luclass) /= 0.0) THEN
    run2 = .TRUE. 
  ELSE
    run2 = .FALSE.
  ENDIF
  IF (run1 .or. run2) THEN

    !  Select representative month(s)
    !
    ! iseiz: 0 = long term, 1 = year, 2 = winter, 3 = summer, 4 = month in winter, 5 = month in summer
    !
    ! Arable land and long term or year run; average over April and July;
    ! April and July are representative months in order to be able to compute deposition over a whole year;
    ! this has been tested against results for 12 separate months
    !
    ! No arable land; choose one representative month;
    ! long term , year run -> May
    ! winter               -> November
    ! summer               -> June
    ! 1 month              -> use mb, which has been read in ops_read_meteo
    telmaand = 0.0
    som_vd_month1 = 0.0
    som_vd_month2 = 0.0
    SELECT CASE(iseiz)
    CASE(0, 1)
      IF (luclass == 2) THEN
        mnt_select(1)=4
        mnt_select(2)=7
      ELSE
        mnt_select=5
      ENDIF
    CASE(2)
       mnt_select=11
    CASE(3)
      mnt_select=6
    CASE DEFAULT
      mnt_select=mb
    END SELECT
    
    ! Compute Rc only for mnt_select(1) and if necessary mnt_select(2):
    DO mnt=1,12
      IF (mnt .EQ. mnt_select(1) .OR. mnt .EQ. mnt_select(2) ) THEN

        ! Set approximate day of year:
        day_of_year = mnt*30-15   

        ! Set sin of solar elevation angle; 
        ! fit of sinphi is based on hourly data of global radiation (cloudy hours are filtered out)
        sinphi = 0.00237*rad_W_m2 - .00000186*rad_W_m2*rad_W_m2  
        
        ! Update month counter:
        telmaand = telmaand+1

        ! Compute canopy resistance Rc with DEPAC for icm = icm_SO2,icm_NOx,icm_NH3 ('SO2', 'NO2', 'NH3').
        ! DEPAC has 3 outputs:
        ! rc_tot      : total canopy resistance Rc (is not used here)
        ! ccomp_tot   : total compensation point (is not used here)
        ! rc_eff_depac: effective Rc (includes effect of compensation point); rc_eff_depac depends on the value of Ra and Rb.
        !
        ! DEPAC consists of two calls, depac_prepare() and depac_finish();
        ! if depac_prepare() sets ready to .TRUE., depac_finish() does not need to be run.
        ! The depac_prepare() call is slow, but only needs to be run once for a trj/rcp combination.

        CALL depac_prepare(comp_id, day_of_year, gym ,temp_C, rad_W_m2, sinphi, hum, nwet, luclass, iratns,   & 
        & lai, sai, ready, rsoil_wet, rsoil_frozen, rsoil, gw, gstom, rc_tot, ccomp_tot)
        if (.NOT. ready) then
          if (run1) then
            CALL depac_finish(comp_id, day_of_year, temp_C, uster1, nwet, luclass, lai, sai, rsoil_wet, rsoil_frozen, rsoil, gw, gstom, & 
                    & c_ave_prev_nh31, c_ave_prev_so21, max(catm1,catm_min), gamma_soil_water_fac1, ra1, rb1, rc_tot, ccomp_tot, rc_eff_depac1)
            ! Detect missing values and set default values
            IF (rc_eff_depac1  .EQ. -9999) rc_eff_depac1 = 10000
            som_vd_month1 = som_vd_month1 + 1/(rc_eff_depac1 + ra1 + rb1)
          endif
          if (run2) then
            CALL depac_finish(comp_id, day_of_year, temp_C, uster2, nwet, luclass, lai, sai, rsoil_wet, rsoil_frozen, rsoil, gw, gstom, & 
                    & c_ave_prev_nh32, c_ave_prev_so22, max(catm2,catm_min), gamma_soil_water_fac2, ra2, rb2, rc_tot, ccomp_tot, rc_eff_depac2)
            ! Detect missing values and set default values
            IF (rc_eff_depac2  .EQ. -9999) rc_eff_depac2 = 10000
            som_vd_month2 = som_vd_month2 + 1/(rc_eff_depac2 + ra2 + rb2)
          endif
        endif
      ENDIF
    ENDDO

    if (run1) then
      ! Compute average over selected months:
      rc_eff_ave1  = telmaand / som_vd_month1  - (ra1 + rb1)
      ! Negative values for effective Rc (re-emission) is not allowed in _pos variables; reset Rc = 1000:
      IF (rc_eff_ave1 .GT. 0 ) THEN
        rc_eff_ave_pos1 = rc_eff_ave1
      ELSE
        rc_eff_ave_pos1 = 1000  
      ENDIF
      som_vd_eff_ave_pos1 = som_vd_eff_ave_pos1 + lu_per1(luclass)/sum(lu_per1(1:NLU)) * 1/(rc_eff_ave_pos1 + (ra1 + rb1))
      som_vd_eff_ave1     = som_vd_eff_ave1     + lu_per1(luclass)/sum(lu_per1(1:NLU)) * 1/(rc_eff_ave1     + (ra1 + rb1))
    endif
    if (run2) then
      ! Compute average over selected months:
      rc_eff_ave2  = telmaand / som_vd_month2  - (ra2 + rb2)
      ! Negative values for effective Rc (re-emission) is not allowed in _pos variables; reset Rc = 1000:
      IF (rc_eff_ave2 .GT. 0 ) THEN
        rc_eff_ave_pos2 = rc_eff_ave2
      ELSE
        rc_eff_ave_pos2 = 1000  
      ENDIF
      som_vd_eff_ave_pos2 = som_vd_eff_ave_pos2 + lu_per2(luclass)/sum(lu_per2(1:NLU)) * 1/(rc_eff_ave_pos2 + (ra2 + rb2))
      som_vd_eff_ave2     = som_vd_eff_ave2     + lu_per2(luclass)/sum(lu_per2(1:NLU)) * 1/(rc_eff_ave2     + (ra2 + rb2))
    endif
  ENDIF
ENDDO  ! loop over land use classes

! Compute Rc without (_pos) and with re-emission: 
rc_eff_pos1 = 1/som_vd_eff_ave_pos1 - (ra1 + rb1)
rc_eff_pos2 = 1/som_vd_eff_ave_pos2 - (ra2 + rb2)
rc_eff1     = 1/som_vd_eff_ave1     - (ra1 + rb1)
rc_eff2     = 1/som_vd_eff_ave2     - (ra2 + rb2)

END SUBROUTINE ops_resist_rc_trj_rcp

!-----------------------------------------------------------------------
SUBROUTINE ops_resist_rc_nox(ra_ms_4, rb_ms, rc_so2_ms, r_no2_nox, rc_no, rhno2, &
                             rc_eff_rcp_4_pos, rc_eff_trj_4_pos, rc_eff_rcp_4, rc_eff_src_4_pos)

! Compute canopy resistance for NOx mixture.                            
! The primary substance is calculated as NO2 (because emissions are specified as such) but contains in reality a mixture of
! NO, NO2 and HNO2. The whole is (finally) mentioned NOx and specified in ppb. Therefore dry deposition velocities have to
! be calculated as representative for the NO-NO2-HNO2 mixture
! Rc for NOx is, uptil now, Rc for NO2 (from DEPAC); now we compute the effective Rc for the NOx mixture as a weighed mean
! of the Rc-values for NO, NO2, HNO2.

! Input arguments:
REAL,      INTENT(IN)                            :: ra_ms_4                    ! aerodynamic resistance at 4 m from meteo statistics [s/m]
REAL,      INTENT(IN)                            :: rb_ms                      ! boundary layer resistance from meteo statistics [s/m]
REAL,      INTENT(IN)                            :: rc_so2_ms                  ! Rc(SO2) from meteo statistics [s/m] 
REAL,      INTENT(IN)                            :: r_no2_nox                  ! NO2/NOx ratio
REAL,      INTENT(IN)                            :: rc_no                      ! canopy resistance for NO [s/m]
REAL,      INTENT(IN)                            :: rhno2                      ! ratio HNO2/NOx [-]

! Input/output:
REAL,      INTENT(INOUT)                         :: rc_eff_rcp_4_pos           ! effective canopy resistance at receptor, 4 m height, no re-emission (Rc is positive) [s/m]
                                                                               ! (i) for NO2, (o): for NOx
REAL,      INTENT(INOUT)                         :: rc_eff_trj_4_pos           ! effective canopy resistance for trajectory, 4 m height, no re-emission (Rc is positive) [s/m]
                                                                               ! (i) for NO2, (o): for NOx
! Output:
REAL,      INTENT(OUT)                           :: rc_eff_rcp_4               ! effective canopy resistance at receptor, 4 m height, re-emission allowed [s/m]
REAL,      INTENT(OUT)                           :: rc_eff_src_4_pos           ! effective canopy resistance at source, 4 m height, no re-emission (Rc is positive) [s/m]

! Local:
REAL                                             :: r                          ! help variable = Ra + Rb [s/m]
REAL                                             :: rc_hno2                    ! canopy resistance HNO2 [s/m]

! Compute canopy resistance for mixture NOx, based on weighed average of deposition velocities of NO2, NO and HNO2
!
!        1                [NO2]/[NOx]         (1-[NO2]/[NOx])        [HNO2]/[NOx]
! ------------------- = ------------------ + -------------------- + --------------------
!  Rc(NOx) + Rb + Ra     Rc(NO2)+ Rb + Ra      Rc(NO) + Rb + Ra      Rc(HNO2) + Rb + Ra
!
r                = rb_ms + ra_ms_4
rc_hno2          = rc_so2_ms         ! Rc(HNO2) = Rc(SO2); here Rc(SO2) from meteo statistics is used, so not Rc(SO2) of DEPAC
rc_eff_rcp_4_pos = 1./(r_no2_nox/(rc_eff_rcp_4_pos+r)  + (1.-r_no2_nox)/(rc_no+r) + rhno2/(rc_hno2+r)) - r   
rc_eff_rcp_4     = rc_eff_rcp_4_pos  ! for NOx no re-emission, only for NH3
rc_eff_trj_4_pos = 1./(r_no2_nox/(rc_eff_trj_4_pos+r) + (1.-r_no2_nox)/(rc_no+r) + rhno2/(rc_hno2+r)) - r
rc_eff_src_4_pos = rc_eff_trj_4_pos  

END SUBROUTINE ops_resist_rc_nox

!-------------------------------------------------------------------------------------------------------------------------------
REAL FUNCTION fpsih(eta)

! FUNCTION           : fpsih
! DESCRIPTION        : Stability correction function in the surface layer temperature profile. The present model is an empirical
!                      fit by Holtslag and De Bruin(1987) of data by Hicks (1976, Quart. J. R. Meteor. Soc., 102, 535-551).
!                      See also Holtslag (1984, BLM, 29, 225-250)
! AUTHOR             : ANTON BELJAARS (KNMI 25-5-87) / Franka Loeve (Cap Volmac)

use m_commonconst_lt                                                              ! EPS_DELTA only

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'fpsih')

! SUBROUTINE ARGUMENTS - INPUT
REAL,      INTENT(IN)                            :: eta                        ! stabiliteitsparameter z/l

! OUTPUT
!     Return value
!
! LOCAL VARIABLES
REAL                                             :: y                          ! hulpvariabele bij de berekening

!-------------------------------------------------------------------------------------------------------------------------------
IF (eta .LT. (0. - EPS_DELTA)) THEN
   y         = SQRT(1. - 16.*eta)
   fpsih = 2.*ALOG((1. + y)/2.)
ELSE
   fpsih = -(0.7*eta) - (0.75*eta - 10.72)* EXP( -0.35*eta) - 10.72
ENDIF

END FUNCTION fpsih

!----------------------------------------------------------------------------------------
SUBROUTINE ops_resist_rain_out_scav_ratio(icm,gasv,isec,kdeel,croutpri,rations,r_no2_nox,routpri,varin_unc)   

! Compute in-cloud scavenging ratio (rout << rain-out = in-cloud) [-])

USE m_commonconst_lt, only: NPARTCLASS, icm_NOx, icm_NH3, icm_SO2
USE m_ops_varin, only: Tvarin_unc

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: icm                        ! component number
LOGICAL,   INTENT(IN)                            :: gasv                       ! TRUE for gasuous component
LOGICAL,   INTENT(IN)                            :: isec                       ! TRUE if component=[SO2, NOx, NH3] (acidifying components)
INTEGER,   INTENT(IN)                            :: kdeel                      ! index of particle size class
REAL,      INTENT(IN)                            :: croutpri                   ! constant (initial) in-cloud scavenging ratio [-] for primary component (rout << rain-out = in-cloud) 
REAL,      INTENT(IN)                            :: rations                    ! NH3/SO2 ratio over trajectory
REAL,      INTENT(IN)                            :: r_no2_nox                  ! NO2/NOx ratio
TYPE(Tvarin_unc), INTENT(IN)                     :: varin_unc

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(INOUT)                         :: routpri                    ! in-cloud scavenging ratio for primary component
                                                                               ! NB: INOUT, because routpri keeps its original 
                                                                               !     value when gasv .and. .not. isec

! CONSTANTS
REAL                                             :: RORATIO(NPARTCLASS)        ! (geschatte) waarden scavenging ratio per deeltjesklasse

! roratio: scavenging ratio per particle class 
! Estimated from measurements of rainwater concentration and air concentration for different substances.

! See nok/luk reports "Luchtverontreniging t.g.v. de uitworp van kolengestookte installaties"
! Potma C.J., Onderdelinden D. and Slanina J. (1986): bijdrage van een kolengestookte elecktriciteitscentrale
!       aan de lokale luchtconcentratie- en depositiveniveaus. PEO report NOK-LUK 3, no. 20.70-017.10, 
!       RIVM report 22822 02 004, Bilthoven.
! van Jaarsveld en Onderdelinden (1986): Modelmatige beschrijving van concentratie en depositie van kolen relevante 
!       componenten in NL veroorzaakt door emissies in Europa, PEO report NOK-LUK 3, RIVM report 2282 02 002, Bilthoven.
!       See Table III measurements in Lelystad and Bilthoven .
! DATA RORATIO / 120000.,1000000.,5000000.,2*9000000./ aangepast (zie EUTREND) 931120
!
DATA RORATIO/240000., 1000000., 1000000., 5000000., 2*9000000./

IF (isec) THEN

   ! Acidifying componentsL
   IF (icm == icm_SO2) THEN
      
      ! icm = icm_SO2: SO2
      routpri = croutpri*rations ! depends on N/S ratio
   
   ELSE IF (icm == icm_NOx) THEN
   
      ! icm = icm_NOx: NOx
      routpri = croutpri*r_no2_nox  ! depends on NO2/NOx ratio
   
   ELSE IF (icm == icm_NH3) THEN
   
      !   icm = icm_NH3: NH3  
      routpri = croutpri
   
   ELSE
      write(*,*) 'internal programming error in ops_resist_rek; isec and icm < icm_SO2 or icm > icm_NH3'
      write(*,*) icm
      stop 1
   ENDIF ! IF icm = icm_SO2,icm_NOx or icm_NH3
ELSE
   ! (.not. isec): non-acidifying components
   
   IF (.NOT.gasv) THEN
      ! Particles:
      routpri = RORATIO(kdeel)
   ELSE
      ! Gases, but not acidifying -> routpri retains the value set in ops_init
      CONTINUE   
   ENDIF
ENDIF

!Edit
! Adjustments relevant for sensitivity analyses. Multiplication factors are 1.0 by default.
! Note: hier aanpassen, of direct onder bepaling van routpri (4x) hierboven. 
routpri = varin_unc%unc_sourcedepl%rainout_pri_fact * routpri
!End Edit

END SUBROUTINE ops_resist_rain_out_scav_ratio

!----------------------------------------------------------------------------------------
SUBROUTINE ops_resist_rc_sec_trj(icm,ra_ms_4,rb_ms,rc_aer_ms,rc_hno3,rhno3_trj,rc_sec_trj)
use m_commonconst_lt, only: icm_SO2, icm_NOx, icm_NH3
! Compute rc_sec_trj: canopy resistance secondary component representative for trajectory

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: icm                        ! component number
REAL,      INTENT(IN)                            :: ra_ms_4                    ! aerodynamic resistance at 4 m from meteo statistics [s/m]
REAL,      INTENT(IN)                            :: rb_ms                      ! boundary layer resistance from meteo statistics [s/m]
REAL,      INTENT(IN)                            :: rc_aer_ms                  ! Rc(SO4-aerosol) from meteo statistics [s/m]
REAL,      INTENT(IN)                            :: rc_hno3                    ! canopy resistance for HNO3 [s/m]
REAL,      INTENT(IN)                            :: rhno3_trj                      ! ratio [HNO3]/[NO3_totaal] [-]


! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: rc_sec_trj                 !  canopy resistance secondary aerosol (SO4, NO3, NH4) for trajectory [s/m]

! LOCAL VARIABLES
REAL                                             :: r                          ! help variable = Ra + Rb

IF (icm == icm_SO2) THEN
   
   ! icm = icm_SO2: SO2, secondary component = SO4
   ! rc_sec_trj: canopy resistance secondary aerosol representative for trajectory; taken as 0.8*Rc(SO4_aerosol)
   rc_sec_trj = rc_aer_ms*0.8

ELSE IF (icm == icm_NOx) THEN

   ! icm = icm_NOx: NOx, secondary component = NO3
   !
   !            1             [HNO3]/[NO3]_totaal     (1-[HNO3]/[NO3]_totaal)
   ! ------------------- = ----------------------- + ---------------------------- 
   !  Rc(NO3) + Rb + Ra       Rc(HNO3)+ Rb + Ra        Rc(NO3_aerosol) + Rb + Ra      
         
   r          = rb_ms + ra_ms_4                                                 
   rc_sec_trj = 1./(rhno3_trj/(r+rc_hno3) + (1.-rhno3_trj)/(r+rc_aer_ms)) - r   
   
ELSE IF (icm .EQ. icm_NH3) THEN

   ! icm = icm_NH3: NH3, secondary component = NH4 
   ! rc_sec_trj = 0.8*Rc(SO4_aerosol)
   rc_sec_trj = rc_aer_ms*0.8
ELSE
   write(*,*) 'internal programming error in ops_resist_rc_sec_trj; isec and icm < icm_SO2 or icm > icm_NH3'
   write(*,*) icm
   stop 1
ENDIF 

END SUBROUTINE ops_resist_rc_sec_trj

!----------------------------------------------------------------------------------------
SUBROUTINE ops_resist_rcaer (icmpsec, znul, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, rc_aer)  

use m_commonconst_lt, only: icm_SO2, icm_NH3, icm_NOx

! SUBROUTINE         : ops_resist_rcaer
! DESCRIPTION        : calculation of canopy resistance for aerosols SO4, NO3, NH4 according
!                      to Wesely et al. (1985) for low vegetation and other areas with
!                      a roughness length (znul) < 0.5m and to Erisman et al (1994) and
!                      Ruijgrok et al. (1994) for forest and other areas with a roughness
!                      length above 0.5m.
! AUTHOR             : Hans van Jaarsveld

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_resist_rcaer')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: icmpsec                    ! component number (11 = SO4, 12 = NO3, 13 = NH4)
REAL,      INTENT(IN)                            :: znul                       ! roughness length [m]
REAL,      INTENT(IN)                            :: ust                        ! friction velocity [m/s]
REAL,      INTENT(IN)                            :: ol                         ! Monin-Obukhov length [m][m]
REAL,      INTENT(IN)                            :: hum                        ! relative humidity [%]
INTEGER,   INTENT(IN)                            :: nwet                       ! wetness indicator depending on percipitation probability and humidity
REAL,      INTENT(IN)                            :: Uh                         ! wind speed used in parametrisation of vd for aerosols [m/s]
REAL,      INTENT(IN)                            :: ra_ms_4                    ! aerodynamic resistance at 4 m from meteo statistics [s/m]
REAL,      INTENT(IN)                            :: rb_ms                      ! boundary layer resistance SO2 from meteo statistics [s/m]
REAL,      INTENT(IN)                            :: rc_hno3                    ! canopy resistance for HNO3 [s/m]
REAL,      INTENT(IN)                            :: rhno3                      ! ratio [HNO3]/[NO3_total] [-]

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: rc_aer                     ! canopy resistance aerosols SO4, NO3, NH4 [s/m]

! LOCAL VARIABLES
REAL                                             :: E                          ! collecting efficiency
REAL                                             :: r                          ! help variable = Ra + Rb [s/m]

! Calculation of canopy resistance Rc.
! For low vegetation and other areas with a roughness length
! below 0.5m by Wesely et al (1985) (for stable/neutral conditions
! (ol<0) and stable conditions (ol>0)), and for forest and other
! areas with a roughness length above 0.5m by Erisman et al (1994), AI.12, AI.13, AI.14 OPS report
!
! Note: 2020-07-06 - we don't use vd anymore, only rc_aer
!
! Sulphate, nitrate and ammonium
! Note: Rb is neglected for aerosols
!
IF (znul.lt.0.5) THEN
   IF (ol.lt.0) THEN
     rc_aer = 1.0/((ust/500.)*(1.+((300./(-1.*ol))**(2./3.))))  ! 960515
   ELSE
     rc_aer = 1.0/(ust/500)
   ENDIF
ELSE
   !-------------------------------------------------------------------------------------------------------------------------------
   ! components:
   ! 11 = SO4
   ! 12 = NO3
   ! 13 = NH4
   !
   ! Calculation of the collecting efficiency E for different components
   ! for different relative humidity and surface wetness. AI.15 OPS report
   !
   ! Sulphate:
   !
   IF (icmpsec== 10 + icm_SO2) THEN
     IF (hum.le.80) THEN
       IF (nwet.ge.1) THEN
         E = 0.08*ust**0.45
       ELSE
         E = 0.05*ust**0.28
       ENDIF
     ELSE
       IF (nwet.ge.1) THEN
         E = 0.08*ust**0.45*(1+0.37*EXP((hum-80)/20))
       ELSE
         E = 0.05*ust**0.28*(1+0.18*EXP((hum-80)/20))
       ENDIF
     ENDIF
   !
   ! Nitrate:
   !
   ELSE IF (icmpsec==10 + icm_NOx) THEN
     IF (hum.le.80) THEN
       IF (nwet.ge.1) THEN
         E = 0.10*ust**0.43
       ELSE
         E = 0.063*ust**0.25
       ENDIF
     ELSE
       IF (nwet.ge.1) THEN
         E = 0.10*ust**0.43*(1+0.37*EXP((hum-80)/20))
       ELSE
         E = 0.063*ust**0.25*(1+0.18*EXP((hum-80)/20))
       ENDIF
     ENDIF
   !
   ! Ammonium:
   !
   ELSE IF (icmpsec==10+icm_NH3) THEN
     IF (hum.le.80) THEN
       IF (nwet.ge.1) THEN
         E = 0.066*ust**0.41
       ELSE
         E = 0.05*ust**0.23
       ENDIF
     ELSE
       IF (nwet.ge.1) THEN
         E = 0.066*ust**0.41*(1+0.37*EXP((hum-80)/20))
       ELSE
         E = 0.05*ust**0.23*(1+0.18*EXP((hum-80)/20))
       ENDIF
     ENDIF
   ENDIF
   rc_aer = 1.0/(E*ust**2/Uh) 
ENDIF

IF (icmpsec == 10 + icm_NOx) THEN     

   !  rc_aer is valid for NO3 aerosol. Calculate now a weighted value for the NO3+HNO3 mixture 
   !
   !          1               [HNO3]/[NO3]_totaal     (1-[HNO3]/[NO3]_totaal)
   ! ------------------- = ----------------------- + ---------------------------- 
   !  Rc(NO3) + Rb + Ra       Rc(HNO3)+ Rb + Ra        Rc(NO3_aerosol) + Rb + Ra      

   r      = rb_ms + ra_ms_4  
   rc_aer = 1./(rhno3/(r+rc_hno3) + (1.-rhno3)/(r+rc_aer)) - r

ENDIF

RETURN

END SUBROUTINE ops_resist_rcaer

end module m_ops_resist_rek
