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
MODULE m_ops_varin

use m_commonconst_lib, only: MISVALNUM

IMPLICIT NONE

!-------------------------------------------------------------------------------------------------------------------------------
! Definition of structures (derives types) with input variables; using these input variables makes it possible to vary
! variables which, up till now, were hard coded in the code. A xml-file is available with default values 
! for these variables. An expert user may change values in this file in order to test the effect on model results. 
!
! Default values may be coded in the type definition below; values read from the varin-file overrule 
! the deafult values provided here.
!
! TYPE Tvarin
!! encompasses all varin structures
!    type(Tvarin_general) :: varin_general            ! input variables, general
!    type(Tvarin_meteo  ) :: varin_meteo              ! input variables for meteo
!    type(Tvarin_disp   ) :: varin_disp               ! input variables for dispersion
!    type(Tvarin_emis   ) :: varin_emis               ! input variables for emission
!    type(Tvarin_drydep ) :: varin_drydep             ! input variables for dry deposition
!    type(Tvarin_wetdep ) :: varin_wetdep             ! input variables for wet deposition
!    type(Tvarin_chem   ) :: varin_chem               ! input variables for chemistry
!    type(Tvarin_unc    ) :: varin_unc                ! noise parameters for uncertainty analysis
! END TYPE Tvarin
!
! Currently only varin_meteo partially implemented.
!-------------------------------------------------------------------------------------------------------------------------------

!TYPE Tvarin_general
!   real         :: air_density                       !   density of air [kg/m3] (variable, depends on temperature) 
!   real         :: air_heat_capacity                 !   specific heat capacity of air [J/(kg K)] (variable, depends on temperature) 
!   real         :: earth_radius                      !   R = earth radius [m]. R1 = equatorial radius: 6378137 m, R2 = distance centre - pole: 6356752 m Note: CONV = pi/180. In source code: R*CONV = 111000 m, so R = 180*111000/pi = 6359831.5 m
!END TYPE Tvarin_general
!
TYPE Tvarin_meteo
   ! real         :: HUMAX - m_commonconst_lib?                            !   maximal height for which power law for windspeed can be applied; above HUMAX: vwind(H) = vwind(HUMAX) [m] 
   ! real         :: inv_space_heating_coeff           !   1/mean(space_heating_coefficient), longterm average, is used to normalise the space_heating_coefficent. 
   ! real         :: mixing_height_growth_fac          !   growth factor for mixing height (used for distances > 1000 km) 
   ! real         :: NACHTWINTER(NSTAB, NTRAJ) - m_commonconst_lib?        !   relative occurrences of nighttime hours in winter (for each stability class and distance class) ("NACHT" = night) [%] 
   ! real         :: NACHTZOMER(NSTAB, NTRAJ)  - m_commonconst_lib?        !   relative occurrences of nighttime hours in summer (for each stability class and distance class) ("NACHT" = night, "ZOMER" = summer) [%] 
   
   ! Variables to prevent (very) small values of uz (wind speed), L (Obukhov length), u* (friction velocity).
   ! More variables (...1, ...2, ...) are needed because of different settings in different parts of OPS-ST and -LT.
   ! In the future, harmonisation will lead to a reduction in the number of variables needed here.
   ! Note: a cutoff value (max or min) of 0 means 'no cutoff'.
   integer      :: ol_cutoff_iopt1 = 1               ! option for cutoff of Obukhov length in part 1 of OPS-ST/LT
   integer      :: ol_cutoff_iopt2 = 2               ! option for cutoff of Obukhov length in part 2 of OPS-ST/LT
   integer      :: ol_cutoff_iopt3 = 3               ! option for cutoff of Obukhov length in part 3 of OPS-ST/LT
                                                     ! 1 -> fixed values for ol_min_stable and ol_max_unstable
                                                     ! 2 -> from NNM   : L > ol_z0_ratio_cutoff_stable*z0 for stable, 
                                                     !                   L < ol_max_unstable for unstable
                                                     !                   NNM: ol_z0_ratio_cutoff_stable = 100, ol_max_unstable = -5 m
                                                     ! 3 -> from OPS-ST: L > ol_z0_ratio_cutoff_stable*z0 for stable, 
                                                     !                   L < -ol_z0_ratio_cutoff_unstable*z0 for unstable
                                                     !                   OPS-ST: ol_z0_ratio_cutoff_stable = ol_z0_ratio_cutoff_unstable*z0 = 100
   real         :: ol_add_stable0     =   0.0        ! additive value for Monin_Obukhov length for stable situations [m]; 0.0 -> no offset
                                                     ! From OPS-LT/ops_stab_rek 1 and 2:  L > 0 -> L = max(10, L+5)
   real         :: ol_add_stable1     =   5.0        ! additive value for Monin_Obukhov length for stable situations [m] 
                                                     ! From OPS-LT/ops_stab_rek 1 and 2:  L > 0 -> L = max(10, L+5)
   real         :: ol_max_unstable0   =   0.0        ! no cutoff for Monin_Obukhov length for unstable situations [m] 
   real         :: ol_max_unstable1   =  -5.0        ! maximal value of Monin_Obukhov length for unstable situations [m] 
                                                     ! From OPS-LT/ops_z0corr: L < 0 -> L = min(L,-5)   
   real         :: ol_max_unstable2   =  -7.0        ! maximal value of Monin_Obukhov length for unstable situations [m] 
                                                     ! From OPS-LT/ops_stab_rek 2 only: L < 0 -> L = min(L, -7)
   real         :: ol_min_stable1     =   5.0        ! minimal value of Monin_Obukhov length for stable situations [m] 
                                                     ! From OPS-LT/ops_z0corr: L > 0 -> L = max(L,5)   
   real         :: ol_min_stable2     =  10.0        ! minimal value of Monin_Obukhov length for stable situations [m] 
                                                     ! From OPS-LT/ops_stab_rek 1:  L > 0 -> L = max(10, L+5)
   real         :: ol_min_stable3     =  20.0        ! minimal value of Monin_Obukhov length for stable situations [m] 
                                                     ! From OPS-ST/?:  L > 0 -> L = max(20, L)
   real         :: ol_z0_ratio_cutoff1 = 100.0       ! minimal value of ol/z0 ratio (from NNM)
                                                     ! From OPS-ST/ops_st_meteo_boundary_layer (icmp .ne 2/4)
   real         :: ol_z0_ratio_cutoff2 =   0.0       ! minimal value of ol/z0 ratio (from NNM)
                                                     
                                                     
                                                     !       ol_min = 5 (used as minimum 5 for stable, and maximum -5 for unstable), 
                                                     !       ol_min = 100*z0l (minimum for stable; maximum (-ol_min = -100*z0l) for unstable
                                                     !       ol2 = 10 (ops_st_meteo_stable), and oldisp = 10 (ops_st_plume_par); only used for stable conditions
                                                     !       oltrj = amax1(oltrj,olsrc), if oltrj.gt0.and.oltrj.lt.ol_min: oltrj = ol_min (only for stable conditions)
                                                     !       ol2 = -5 (unstable conditions), ol2 = 5 (stable conditions) (in z0corr, in the end not used as olrcp is overwritten by ollok)
                                                     
                                                     !       o = 20 (stable), o = -5 (unstable)   
 ! real         :: phi_inside_area_source            !   source-receptor direction for receptor inside area source (for non-occurring meteo class) [degrees] 
 ! real         :: stab_momentum_coef1               !   coefficient f1 in stability correction for momentum phim = f1*(1. - exp( - f2*h/ol)) 
 ! real         :: stab_momentum_coef2               !   coefficient f2 in stability correction for momentum phim = f1*(1. - exp( - f2*h/ol)) 
   integer      :: uster_cutoff_iopt1 = 1            ! option 1 for cutoff of friction velocity
   integer      :: uster_cutoff_iopt2 = 2            ! option 2 for cutoff of friction velocity
                                                     ! 1 -> fixed values for uster_min
                                                     ! 2 -> correction according to the altered Obukhov length (uster2 = uster1*(ol2/ol1)**0.33)
   real         :: uster_min1 = 0.04                 !   minimal allowed value for u* [m/s], option 1
   real         :: uster_min2 = 0.06                 !   minimal allowed value for u* [m/s], option 2
   real         :: uster_min3 = 0.07                 !   minimal allowed value for u* [m/s], option 3
                                                     
                                                     
   real         :: uz_cutoff1 = 0.5                  !   cutoff value for low wind velocities [m/s], option 1 (readmb and ops_st_meteo_boundary_layer in OPS-ST)
   real         :: uz_cutoff2 = 0.75                 !   cutoff value for low wind velocities [m/s], option 2 (ops_wv_log_profile in OPS-lib)
                                                     
 ! real         :: wind_shear_coef                   !   c in alpha = c*shear*(1.-EXP(-1.0*htot/h_tower_cabauw)) 
 ! logical      :: file_exist                        ! input file with varin-parameter values exists (if not -> default values are used)
END TYPE Tvarin_meteo
!
!TYPE Tvarin_disp
!   real         :: coefa_stab_init                   !   coefficient a in initial value for z = a*x**b, for stable situations 
!   real         :: coefa_unstab_init                 !   coefficient a in initial value for z = a*x**b, for unstable situations 
!   real         :: coefb_stab_init                   !   coefficient b in initial value for z = a*x**b, for stable situations 
!   real         :: coefb_unstab_init                 !   coefficient b in initial value for z = a*x**b, for unstable situations 
!   real         :: corr_fac_area_coefa               !   coefficient a in correction for concentration inside area source, c = c*((f - 1)*(f1**a)*f2 + 1) 
!   real         :: dispg(NSTAB)                             !   dispersion coefficients for vertical dispersion for each stability class; sigma_z = dispg*x**disph. dispg is initially set in ops_main, but re-computed (for distance > 1 m) as function of sigma_z and disph [-] 
!   real         :: disph(NSTAB)                             !   dispersion coefficients for vertical dispersion for each stability class; sigma_z = dispg*x**disph. disph is fixed[-] 
!   real         :: fac_disp_height_mix_height        !   tuning factor to determine dispersion height as fraction of mixing height, if plume extends above mixing height [-] 
!   real         :: fac_height_over_sigma_z           !   representative height for which to compute u and Kz, as fraction of sigma_z (in convective layer) ?!?!? what about the stack height ?? [-]
!   real         :: kz_sstable_cal_fac                !   calibration factor for Kz, for strongly stable situations (0 < L < 30) 
!   real         :: kz_stable_cal_fac                 !   calibration factor for Kz, for stable and neutral situations (L >= 30) 
!   real         :: ns                                !   number of sub sectors in calculating representative quantity over a line between source and receptor. 
!   real         :: plume_descent_area_coef1          !   coefficient f1 in correction for plume descent of heavy particles in area source: pld = (sigz*zwcor(istab)) + (pld/f1) [-] 
!   real         :: dimless_height_surf               ! 
!   real         :: dimless_height_mix                ! 
!   real         :: dimless_stab_convective           ! 
!   real         :: dimless_stab_neutral              ! 
!   real         :: sigma_wc_over_wstar               !   sigma_wc/w*, sigma_wc = standard deviation of vertical velocity due to convective activity, w* = convective velocity scale [-]
!   real         :: sigma_wm_over_ustar               !   sigma_wm/u*, sigma_wm = standard deviation of vertical velocity due to wind shear, u* = friction velocity [-]
!   real         :: sigma_z_cal_fac                   !   calibration factor for sigma_z in convective layer, based on on Gryning and Holtslag data [-]
!   real         :: sigma_z_min                       !   minimal value for sigma_z [m] 
!   real         :: sigmaz_area_cal_fac               !   calibration factor for sigma_z at edge of area source 
!   real         :: sigmaz_buoyant_cal_fac            !   calibration factor in extra sigma_z for buoyant plumes (=1/1.5) [-] 
!   real         :: sigmaz_def                        !   default value for sigma_z of area source [m] 
!   real         :: sigmaz_sec_area_cal_fac           !   calibration factor f1 in sigma_z for secondary component inside area source sigzsec = f1*sigz[-] 
!   real         :: sigmaz_zi_split                   !   split for ratio sigma_z/zi, if sigma_z/zi > sigmaz_zi_split -> well mixed [-]
!   real         :: time_scale_Lagrange_coef1         !   coefficient f1 in equation for Lagrangian time scale tau_l (Gryning et al., 1987): tau_l = f1 - (f2/L) 
!   real         :: time_scale_Lagrange_coef2         !   coefficient f2 in equation for Lagrangian time scale tau_l (Gryning et al., 1987): tau_l = f1 - (f2/L) 
!   real         :: time_scale_Lagrange_max           !   maximal value for Lagrangian time scale tau_l [s] 
!   real         :: time_scale_Lagrange_min           !   minimal value for Lagrangian time scale tau_l [s] 
!   real         :: tl_high_source                    !   characteristic time for high source (> 50 m) [s] 
!   real         :: tl_low_source                     !   characteristic time for low source (<= 50 m) [s] 
!   real         :: z_over_L_convec                   !   value for z/L for which we have a convective mixing layer [-] 
!   real         :: zwcor(NSTAB)                             !   correction term for plume descent of heavy particles in area source, for each stability class [-] 
!END TYPE Tvarin_disp
!
!TYPE Tvarin_emis
!   real         :: ibtg                              !   diurnal emission variation code ibtg < 0 user specified emission variation 0: uniform in time 1: according to the (average) industrial activity over a working day 2: according to the (average) heating activity for space heating 3: according to the (average) traffic intensity 4: NH3 emission from animal housing systems 5: NH3 emission from application, fertiliser and other 
!   real         :: nh3_ah_temp_coef                  !   coefficient f in temperature correction for emissions from animal housing tcor=max((tem+f-10)/f, tcor_min) 
!   real         :: nh3_ah_temp_corr_min              !   minimal temperature correction for emissions from animal housing 
!   real         :: nh3_app_coef1                     !   coefficient f1 in emission factor for NH3 application, emf=f1*((100./(ra4+rd))**f2*(tem+f3)**f4)**f5 
!   real         :: nh3_app_coef2                     !   coefficient f2 in emission factor for NH3 application, emf=f1*((100./(ra4+rd))**f2*(tem+f3)**f4)**f5 
!   real         :: nh3_app_coef3                     !   coefficient f3 in emission factor for NH3 application, emf=f1*((100./(ra4+rd))**f2*(tem+f3)**f4)**f5 
!   real         :: nh3_app_coef4                     !   coefficient f4 in emission factor for NH3 application, emf=f1*((100./(ra4+rd))**f2*(tem+f3)**f4)**f5 
!   real         :: nh3_app_coef5                     !   coefficient f5 in emission factor for NH3 application, emf=f1*((100./(ra4+rd))**f2*(tem+f3)**f4)**f5 
!   real         :: nh3_app_rain_coef1                !   coefficient f1 in rain correction for emission of NH3 (application), rcor=(f1-regenk)**f2 
!   real         :: nh3_app_rain_coef2                !   coefficient f2 in rain correction for emission of NH3 (application), rcor=(f1-regenk)**f2 
!   real         :: nh3_app_rain_corr_max             !   maximal value of rain correction for emission of NH3 (application) 
!   real         :: nh3_app_rain_corr_min             !   minimal value of rain correction for emission of NH3 (application) 
!   real         :: plrise_ci_neutral             !   ci = empirical constant representing the trapping effect for neutral situations 
!   real         :: plrise_nonstab_Fbhigh_coef1    !   f1 in plume rise relation for non-stable situations, high Qw: delh = f1*(qww**f2)/utop 
!   real         :: plrise_nonstab_Fbhigh_coef2    !   f2 in plume rise relation for non-stable situations, high Qw: delh = f1*(qww**f2)/utop 
!   real         :: plrise_nonstab_Fblow_coef1     !   f1 in plume rise relation for non-stable situations, low Qw: delh = f1*(qww**f2)/utop 
!   real         :: plrise_nonstab_Fblow_coef2     !   f2 in plume rise relation for non-stable situations, low Qw: delh = f1*(qww**f2)/utop 
!   real         :: plrise_nonstab_Fbsplit        !   value of split in Qw for non-stable situations [MW] 
!   real         :: plrise_ci_add_stab_unstab     !   additive empirical constant for onder (fraction of plume below mixing height), representing temperature inversion effects in stable and unstable situations 
!   real         :: plrise_stab_coef1             !   f1 in plume rise relation for stable situations: delh = f1*(qww/utop)**f2 
!   real         :: plrise_stab_coef2             !   f2 in plume rise relation for stable situations: delh = f1*(qww/utop)**f2 
!   real         :: plrise_stab_dtheta_dz
!   real         :: STOKES(NPARTCLASS)            !   Sedimentation velocity (m/s) needed for plume descent in case of heavy particles, for each particle class. Sedimentation velocity depends on particle size according to Stokes law.
!   real         :: sigz0                         !   initial vertical dispersion length [m]
!END TYPE Tvarin_emis
!
!TYPE Tvarin_drydep
!   real         :: a1_cal_fac                        !   calibration factor in plume thickness for dry deposition (area source) a1 = f1*sigma_z 
!   real         :: b                                 !   empirical constant b in in-canopy resistance, for each landuse class: rinc = b h SAI/u*, u* > 0 
!   real         :: collecting_eff_nh4_coefs          !   6 coefficients in collecting efficiency NH4, see table 5.4 
!   real         :: collecting_eff_no3_coefs          !   6 coefficients in collecting efficiency NO3, see table 5.4 
!   real         :: collecting_eff_so4_coefs          !   6 coefficients in collecting efficiency SO4, see table 5.4 
!   real         :: d                                 !   displacement height [m] 
!   real         :: ddeppar                           !   kdeppar = 1: ddeppar = deposition velocity vd [m/s] kdeppar = 2: ddeppar = surface resistance Rc [s/m] 
!   real         :: dxeff_cal_fac1                    !   calibration factor f1 in definition of effective travel distance, for plume above mixing height: dxeff = diameter/f1 
!   real         :: dxeff_cal_fac2                    !   calibration factor f2 in definition of effective deposition distance, for area source: dxeff = diameter/f1*EXP( -a/f2) 
!   real         :: gamma_soil_c_fac                  !   factor in linear relation between gamma_soil and NH3 air concentration; gamma_soil = [NH4+]/[H+] ratio in soil, for each landuse class
!   real         :: gamma_stom_c_fac                  !   factor in linear relation between gamma_stom and NH3 air concentration; gamma_stom = [NH4+]/[H+] ratio in apoplast, for each landuse class
!   real         :: gamma_stom_coef1                  !   coefficient f1 in gamma_stom = gamma_stom_c_fac(lu)*c_ave_prev*f1*exp(-f2*t) 
!   real         :: gamma_stom_coef2                  !   coefficient f2 in gamma_stom = gamma_stom_c_fac(lu)*c_ave_prev*f1*exp(-f2*t) 
!   real         :: gamma_tfac_coef1                  !   coefficient f1 in temperature factor for gamma: tfac = (f1/tk)*exp(-f2/tk) 
!   real         :: gamma_tfac_coef2                  !   coefficient f2 in temperature factor for gamma: tfac = (f1/tk)*exp(-f2/tk) 
!   real         :: gamma_w_coef1                     !   coefficient f1 in gamma_w = f1 + f2*catm*exp(-f3*t) 
!   real         :: gamma_w_coef2                     !   coefficient f2 in gamma_w = f1 + f2*catm*exp(-f3*t) 
!   real         :: gamma_w_coef3                     !   coefficient f3 in gamma_w = f1 + f2*catm*exp(-f3*t) 
!   real         :: h                                 !   vegetation height h in in-canopy resistance, for each landuse class: rinc = b h SAI/u*, u* > 0 [m]
!   real         :: h_u_vd_sec_aer_rough              !   height for which to compute wind speed u needed for deposition velocity of secondary aerosols, for rough surfaces [m] 
!   real         :: h_u_vd_sec_aer_smooth_coef1       !   coefficient f1 in height = f1*z0 for which to compute wind speed u needed for deposition velocity of secondary aerosols, for smooth surfaces [-] 
!   real         :: hf_area_coef1                     !   coefficient f1 in plume height for area source: hf = (sigzr/f1 + htot + f2)/f3 
!   real         :: hf_area_coef2                     !   coefficient f2 in plume height for area source: hf = (sigzr/f1 + htot + f2)/f3 
!   real         :: hf_area_coef3                     !   coefficient f3 in plume height for area source: hf = (sigzr/f1 + htot + f2)/f3 
!   real         :: mnt_select                        !   representative month for DEPAC NH3 parameters 
!   real         :: nwet_coef1                        !   coefficient f1 in wetness indicator nwet = NINT((regenk * f1 + hum/f2 - f3)**f4*f5) 
!   real         :: nwet_coef2                        !   coefficient f2 in wetness indicator nwet = NINT((regenk * f1 + hum/f2 - f3)**f4*f5) 
!   real         :: nwet_coef3                        !   coefficient f3 in wetness indicator nwet = NINT((regenk * f1 + hum/f2 - f3)**f4*f5) 
!   real         :: nwet_coef4                        !   coefficient f4 in wetness indicator nwet = NINT((regenk * f1 + hum/f2 - f3)**f4*f5) 
!   real         :: nwet_coef5                        !   coefficient f5 in wetness indicator nwet = NINT((regenk * f1 + hum/f2 - f3)**f4*f5) 
!   real         :: nwet_switch                       !   switch value for nwet in determining wet or dry conditions 
!   real         :: rc_missing                        !   default Rc value in case that DEPAC computes a missing value [s/m] 
!   real         :: rc_negative                       !   default Rc value in case that DEPAC computes a negative value (re-emission) [s/m] 
!   real         :: rc_so4_cal_fac                    !   calibration? factor f1 in Rc(SO4) = Rc(NH4) = f1*Rc(NO3) 
!   real         :: rc_tot_HNO3                       !   canopy resistance HNO3, for winter situations and other [s/m] 
!   real         :: rc_tot_NO_wws                     !   canopy resistance NO, for water and wet situations [s/m] 
!   real         :: rcdeel                            !   surface resistance Rc per particle class [s/m] 
!   real         :: rchno3                            !   surface resistance Rc for HNO3 [s/m] 
!   real         :: rcno                              !   surface resistance Rc for NO [s/m] 
!   real         :: resistance_part_coef1             !   coefficient f1 in definition of canopy resistance for particles, 1/Rpart = (u*/f1)*(1 + (f2/(-L))**f3) 
!   real         :: resistance_part_coef2             !   coefficient f2 in definition of canopy resistance for particles, 1/Rpart = (u*/f1)*(1 + (f2/(-L))**f3) 
!   real         :: resistance_part_coef3             !   coefficient f3 in definition of canopy resistance for particles, 1/Rpart = (u*/f1)*(1 + (f2/(-L))**f3) 
!   real         :: rinc_ustar_neg                    !   in-canopy resistance, u* <= 0 [s/m]
!   real         :: rsoil_NH3                         !   soil resistance NH3, per landuse class + wet, frozen, snow [s/m] 
!   real         :: rsoil_NO                          !   soil resistance NO, per landuse class + wet, frozen, snow [s/m] 
!   real         :: rsoil_NO2                         !   soil resistance NO2, per landuse class + wet, frozen, snow [s/m] 
!   real         :: rsoil_O3                          !   soil resistance O3, per landuse class + wet, frozen, snow [s/m] 
!   real         :: rsoil_SO2                         !   soil resistance SO2, per landuse class + wet, frozen, snow [s/m] 
!   real         :: rw_NH3_coef1                      !   coefficient f1 external leaf resistance NH3: rw = f1 * exp((100.0 - rh)/f2) [s/m] 
!   real         :: rw_NH3_coef2                      !   coefficient f2 external leaf resistance NH3: rw = f1 * exp((100.0 - rh)/f2) [-] 
!   real         :: rw_NH3_frozen                     !   external leaf resistance NH3, frozen situations [s/m] 
!   real         :: rw_NO                             !   external leaf resistance NO [s/m] 
!   real         :: rw_NO2                            !   external leaf resistance NO2 [s/m] 
!   real         :: rw_O3                             !   external leaf resistance O3 [s/m] 
!   real         :: rw_SO2_dry_coef1                  !   coefficient f1 in external leaf resistance SO2: rw = f1*exp(f2*rh), for dry situations [s/m]
!   real         :: rw_SO2_dry_coef2                  !   coefficient f2 in external leaf resistance SO2: rw = f1*exp(f2*rh), for dry situations [-] 
!   real         :: rw_SO2_dry_frozen                 !   external leaf resistance SO2, for dry/frozen (-5 C < T < -1 C) situations [s/m] 
!   real         :: rw_SO2_dry_frozen_extra           !   external leaf resistance SO2, for dry/extra frozen (T < -5 C) situations [s/m] 
!   real         :: rw_SO2_wet                        !   external leaf resistance SO2, for wet situations [s/m] 
!   real         :: sinphi_coef1                      !   coefficient f1 in relation for sine of solar elevation: sinphi = f1*glrad + f2*glrad*glrad 
!   real         :: sinphi_coef2                      !   coefficient f2 in relation for sine of solar elevation: sinphi = f1*glrad + f2*glrad*glrad 
!   real         :: temp_water_coef1                  !   coefficient tk0 in water temperature: tk = tk0 + tk_amp*sin(day_of_year - tk_day_shift)
!   real         :: temp_water_coef2                  !   coefficient tk_amp in water temperature: tk = tk0 + tk_amp*sin(day_of_year - tk_day_shift)
!   real         :: temp_water_coef3                  !   coefficient tk_day_shift in water temperature: tk = tk0 + tk_amp*sin(day_of_year - tk_day_shift)
!   real         :: travel_time_cal_fac               !   calibration factor in travel time (area source) t = f1*(radius/u) 
!   real         :: vg50trans_cal_fac                 !   calibration factor f1 in definition of deposition velocity for trajectory, for very high sources: vg50trans = xvghbr*vg50tra*f1 
!   real         :: vgdeel                            !   deposition velocity per particle class [m/s] 
!   real         :: vgmax                             !   maximal deposition velocity [m/s] (only for kdeppar = 1) 
!END TYPE Tvarin_drydep
!
!TYPE Tvarin_wetdep
!   real         :: cmnd                              !   Monthly correction shower duration and rain intensity [-] 
!   real         :: croutpri                          !   (wash-out + rain-out) ratio primary component, default value without correction for background concentration, season, stability class [-] icm = 1 (SO2): croutpri = wash out ratio at an N/S ratio of 1 icm = 2 (NOx): croutpri = wash out ratio at an NO2/NOx ratio of 1 icm = 3 (NH3): croutpri = wash out ratio (no correction)
!   real         :: ddrup_coef1                       !   coefficient f1 in definition ddrup = f1*(ri**f2) 
!   real         :: ddrup_coef2                       !   coefficient f2 in definition ddrup = f1*(ri**f2) 
!   real         :: distr_below_in_cloud_area_coefa   !   coefficient a in distribution factor below - in cloud scavenging inside area source: pr = EXP(-(hl+f1)**2/(2*sigz*sigz*a)) 
!   real         :: distr_below_in_cloud_coef1        !   coefficient f1 in distribution factor below - in cloud scavenging: pr = EXP(-(hl+f1)**2/(2*sigz*sigz*a)) 
!   real         :: epsilon                           !   collision efficiency for particles [-] 
!   real         :: lambda_alpha1                     !   coefficient alpha1 in below cloud scavenging rate: LAMBDA = alpha1*(D_g**alpha2)*(Ri**alpha3); 
!   real         :: lambda_alpha2                     !   coefficient alpha2 in below cloud scavenging rate: LAMBDA = alpha1*(D_g**alpha2)*(Ri**alpha3); 
!   real         :: lambda_alpha3                     !   coefficient alpha3 in below cloud scavenging rate: LAMBDA = alpha1*(D_g**alpha2)*(Ri**alpha3); 
!   real         :: lambda_part_beta                  !   coefficient beta in below cloud scavenging rate for particles: LAMBDA = lambda0*epsilon*(Ri**(1-beta)) 
!   real         :: lambda_part_lambd0                !   coefficient lambda0 in below cloud scavenging rate for particles: LAMBDA = lambda0*epsilon*(Ri**(1-beta)) 
!   real         :: precip_dsource                    !   source distance used in computing the precipitation amount [m] 
!   real         :: precip_hsource                    !   source height used in computing the precipitation amount[m] 
!   real         :: roratio                           !   scavenging ratio per particle class [-] 
!   real         :: routsec                           !   in-cloud (rain-out) scavenging ratio for secondary component, resp. for SO4, NO3, NH4 [-] 
!   real         :: twet_cal_fac                      !   calibration factor f1 in definition of twet = tau_w,Euler = f1*buil 
!   real         :: twt_cal_fac                       !   calibration factor f1 in definition of twt = tau_w,Lagrange = 1.0*twet*(1. - EXP( - f1*treis/twet)) 
!   real         :: twt_cutoff                        !   cut-off value for twt = tau_w,Lagrange [h] 
!   real         :: vdrup_coef1                       !   coefficient f1 in definition of rain drop vertical velocity: vdrup = f1*ddrup**f2 
!   real         :: vdrup_coef2                       !   coefficient f2 in definition of rain drop vertical velocity: vdrup = f1*ddrup**f2 
!   real         :: wdeppar                           !   knatdeppar = 1: wdeppar = scavenging coefficient [%/h] knatdeppar = 2: wdeppar = scavenging ratio, i.e. average ratio of rainwater concentrations and air concentration [-] 
!   real         :: x_wet_shift                       !   extra shift in distance over which there is wet deposition: x = disx + virty + x_wet_shift + virnat 
!END TYPE Tvarin_wetdep
!
!TYPE Tvarin_chem
!   real         :: ar                                !   proportionality constant in relation [OH] = ar Qr, with Qr = global radiation in W/m2 [(cm3 m2)/(molec W2)], see Egmond en Kesseboom (1983) 
!   real         :: ar0                               !   fixed value of proportionality constant in relation [OH] = ar Qr, with Qr = global radiation in W/m2 [(cm3 m2)/(molec W2)], see Egmond en Kesseboom (1983) 
!   real         :: conc_cf                           !   NOx concentration correction factor = 1/1.08 (to account for and HNO2 and PAN contributions to NO2); = 1 for all other species. Is only used for output purposes. [-] 
!   real         :: idgr                              !   particle size distribution code ("dgr" << deeltjesgrootte = particle size)
!   real         :: k_no3_night                       !   conversion rate NO2+O3 -> NO3 (nighttime) [%/h] 
!   real         :: k_so4_aq                          !   reaction rate SO2 -> sulphate (Aqueous phase) [%/h] 
!   real         :: k_so4_he                          !   reaction rate SO2 -> sulphate (Particle phase) [%/h] 
!   real         :: koh                               !   second order reaction rate constant of reaction NO2 + OH -> HNO3 [cm3/(molec s)] 
!   real         :: no2sek                            !   coefficient in correction factor for trajectory averaged background NO2 concentration for each wind direction sector; derived from 15 regional LML stations over 2004 
!   real         :: ratns                             !   indicator for NH3/SO2 ratio (1=low; 2=high) 
!   real         :: rhno2                             !   ratio [HNO2]/[NOx] 
!   real         :: rhno3_coef1                       !   C1 in relation rhno3 = C1*(nh3bgtra_corr/1000)**(C2) 
!   real         :: rhno3_coef2                       !   C2 in relation rhno3 = C1*(nh3bgtra_corr/1000)**(C2) 
!   real         :: rhno3_max                         !   maximal value allowed for rhno3 
!   real         :: rno2nox                           !   season dependent component of [NO2]/[NOx] ratio [-] 
!   real         :: rno2nox_ave_nl                    !   average [NO2]/[NOx] ratio in NL 
!   real         :: rrno2nox_coef1                    !   coefficient f1 in relation rrno2nox=no2bgtra_corr/(exp((no2bgtra_corr+f1)/f2))/rno2nox_ave_nl 
!   real         :: rrno2nox_coef2                    !   coefficient f2 in relation rrno2nox=no2bgtra_corr/(exp((no2bgtra_corr+f1)/f2))/rno2nox_ave_nl 
!   real         :: SCWINTER                          !   variation in NO2/NOx ratio (relative to stability class S2) for each stability class (only in winter); see OPS-doc/chem, bookmark table_no2_nox. 
!   real         :: so2sek                            !   coefficient in correction factor for for trajectory averaged background SO2 concentration for each wind direction sector; derived from 24 regional LML stations over 2003 
!   real         :: vchem_nh3_coef0                   !   k0 in relation kNH3 = k0 + k1 C1 + k2 C2 + k3 (C2)**4 + k4 (C2)**6, C1 = [NO2]/[NH3], C2 = [SO2]/[NH3] 
!   real         :: vchem_nh3_coef1                   !   k1 in relation kNH3 = k0 + k1 C1 + k2 C2 + k3 (C2)**4 + k4 (C2)**6, C1 = [NO2]/[NH3], C2 = [SO2]/[NH3] 
!   real         :: vchem_nh3_coef10                  !   f10 in calibration kNH3 = max(kNH3_min,f10 + f11 kNH3) 
!   real         :: vchem_nh3_coef11                  !   f11 in calibration kNH3 = max(kNH3_min,f10 + f11 kNH3) 
!   real         :: vchem_nh3_coef2                   !   k2 in relation kNH3 = k0 + k1 C1 + k2 C2 + k3 (C2)**4 + k4 (C2)**6, C1 = [NO2]/[NH3], C2 = [SO2]/[NH3] 
!   real         :: vchem_nh3_coef3                   !   k3 in relation kNH3 = k0 + k1 C1 + k2 C2 + k3 (C2)**4 + k4 (C2)**6, C1 = [NO2]/[NH3], C2 = [SO2]/[NH3] 
!   real         :: vchem_nh3_coef4                   !   k4 in relation kNH3 = k0 + k1 C1 + k2 C2 + k3 (C2)**4 + k4 (C2)**6, C1 = [NO2]/[NH3], C2 = [SO2]/[NH3] 
!   real         :: vchem_nh3_min                     !   kNH3_min in calibration kNH3 = max(kNH3_min,f10 + f11 kNH3) 
!   real         :: vchemc                            !   chemical conversion rate, independent of light [%/h] 
!   real         :: vchemv                            !   chemical conversion rate, dependent on light [%/h] 
!END TYPE Tvarin_chem

TYPE Tunc_sourcedepl
   real :: vchem_fact = 1.0
   real :: washout_pri_fact = 1.0
   real :: washout_sec_fact = 1.0
   real :: rainout_pri_fact = 1.0
   real :: rainout_sec_fact = 1.0
   real :: vd_drydep_pri_fact = 1.0
   real :: vd_drydep_sec_fact = 1.0
END TYPE Tunc_sourcedepl

TYPE Tunc_meteo
   real :: xl_fact = 1.0  ! Mixing layer height factor.
END TYPE Tunc_meteo

TYPE Tvarin_unc
   real :: plrise_fact = 1.0                ! Plumerise noise factor; assumed to be distributed log-normally.
   real :: sigz_fact = 1.0
   type(Tunc_sourcedepl) :: unc_sourcedepl  ! Noise factors for source depletion in deposition; assumed to be log-normal.
   type(Tunc_meteo) :: unc_meteo            ! Noise factors for meteorological values; assumed to be log-normal.
   real :: diurn_scale_index = 0.5  ! Scaling index used in `scale_dverl`.
END TYPE Tvarin_unc

TYPE Tvarin
    TYPE(Tvarin_meteo) :: varin_meteo
    TYPE(Tvarin_unc) :: varin_unc
    CHARACTER(len=512) :: file  ! Filename of varin file.
END TYPE Tvarin

contains

!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_read_varin( namvarin, varin, error)

! Open file with varin variables (input model variables), read variable values and close file.
! All variables must be present in the same order as defined in the type definition.
! For a more flexible implementation -> ops_varin_read2

USE m_getkey
USE m_error
USE m_commonfile, only: fu_varin
USE m_fileutils

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - OUTPUT 
character(len=*), intent(in) :: namvarin ! name of varin file

TYPE(Tvarin), INTENT(OUT) :: varin
!TYPE(Tvarin_gen),       INTENT(OUT) :: varin_gen                  ! general_variables
!TYPE(Tvarin_meteo),     INTENT(OUT) :: varin_meteo                ! meteo_variables
!TYPE(Tvarin_disp),      INTENT(OUT) :: varin_disp                 ! dispersion_variables
!TYPE(Tvarin_emis),      INTENT(OUT) :: varin_emis                 ! emission_variables
!TYPE(Tvarin_drydep),    INTENT(OUT) :: varin_drydep               ! dry_deposition_variables
!TYPE(Tvarin_wetdep),    INTENT(OUT) :: varin_wetdep               ! wet_deposition_variables
!TYPE(Tvarin_chem),      INTENT(OUT) :: varin_chem                 ! chemistry_variables
!TYPE(Tvarin_unc),       INTENT(OUT) :: varin_unc
TYPE(TError),           INTENT(INOUT) :: error                      ! error handling record

! LOCAL VARIABLES
CHARACTER*255, PARAMETER            :: ROUTINENAAM = 'ops_read_varin'

! Open file with varin parameters:
IF (.NOT. sysopen(fu_varin, namvarin, 'r', 'varin file', error)) GOTO 1001

! Read general_variables: 
!IF (.NOT. GetKeyValue(fu_varin,'air_density', varin_gen%air_density, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'air_heat_capacity', varin_gen%air_heat_capacity, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'earth_radius', varin_gen%earth_radius, error)) GOTO 1000
!write(*,*) 'varin_gen:'
!write(*,*) varin_gen

! Read meteo_variables: 
!IF (.NOT. GetKeyValue(fu_varin,'HUMAX', varin_meteo%HUMAX, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'inv_space_heating_coeff', varin_meteo%inv_space_heating_coeff, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'mixing_height_growth_fac', varin_meteo%mixing_height_growth_fac, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'NACHTWINTER', varin_meteo%NACHTWINTER, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'NACHTZOMER', varin_meteo%NACHTZOMER, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'ol_cutoff_iopt1'    , varin%varin_meteo%ol_cutoff_iopt1, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'ol_cutoff_iopt2'    , varin%varin_meteo%ol_cutoff_iopt2, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'ol_cutoff_iopt3'    , varin%varin_meteo%ol_cutoff_iopt3, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'ol_add_stable0'     , varin%varin_meteo%ol_add_stable0, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'ol_add_stable1'     , varin%varin_meteo%ol_add_stable1, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'ol_max_unstable0'   , varin%varin_meteo%ol_max_unstable0, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'ol_max_unstable1'   , varin%varin_meteo%ol_max_unstable1, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'ol_max_unstable2'   , varin%varin_meteo%ol_max_unstable2, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'ol_min_stable1'     , varin%varin_meteo%ol_min_stable1, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'ol_min_stable2'     , varin%varin_meteo%ol_min_stable2, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'ol_min_stable3'     , varin%varin_meteo%ol_min_stable3, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'ol_z0_ratio_cutoff1', varin%varin_meteo%ol_z0_ratio_cutoff1, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'ol_z0_ratio_cutoff2', varin%varin_meteo%ol_z0_ratio_cutoff2, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'phi_inside_area_source', varin%meteo%phi_inside_area_source, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'stab_momentum_coef1', varin_meteo%stab_momentum_coef1, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'stab_momentum_coef2', varin_meteo%stab_momentum_coef2, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'uster_cutoff_iopt1', varin%varin_meteo%uster_cutoff_iopt1, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'uster_cutoff_iopt2', varin%varin_meteo%uster_cutoff_iopt2, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'uster_min1'        , varin%varin_meteo%uster_min1, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'uster_min2'        , varin%varin_meteo%uster_min2, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'uster_min3'        , varin%varin_meteo%uster_min3, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'uz_cutoff1'        , varin%varin_meteo%uz_cutoff1, error)) GOTO 1000
IF (.NOT. GetKeyValue(fu_varin,'uz_cutoff2'        , varin%varin_meteo%uz_cutoff2, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'wind_shear_coef7', varin_meteo%wind_shear_coef7, error)) GOTO 1000
!write(*,*) 'varin_meteo:'
!write(*,*) varin_meteo

!! Read dispersion_variables: 
!IF (.NOT. GetKeyValue(fu_varin,'coefa_stab_init', varin_disp%coefa_stab_init, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'coefa_unstab_init', varin_disp%coefa_unstab_init, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'coefb_stab_init', varin_disp%coefb_stab_init, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'coefb_unstab_init', varin_disp%coefb_unstab_init, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'corr_fac_area_coefa', varin_disp%corr_fac_area_coefa, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'dispg', varin_disp%dispg, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'disph', varin_disp%disph, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'fac_disp_height_mix_height', varin_disp%fac_disp_height_mix_height, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'fac_height_over_sigma_z', varin_disp%fac_height_over_sigma_z, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'kz_sstable_cal_fac', varin_disp%kz_sstable_cal_fac, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'kz_stable_cal_fac', varin_disp%kz_stable_cal_fac, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'ns', varin_disp%ns, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'plume_descent_area_coef1', varin_disp%plume_descent_area_coef1, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'dimless_height_surf', varin_disp%dimless_height_surf, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'dimless_height_mix', varin_disp%dimless_height_mix, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'dimless_stab_convective', varin_disp%dimless_stab_convective, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'dimless_stab_neutral', varin_disp%dimless_stab_neutral, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'sigma_wc_over_wstar', varin_disp%sigma_wc_over_wstar, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'sigma_wm_over_ustar', varin_disp%sigma_wm_over_ustar, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'sigma_z_cal_fac', varin_disp%sigma_z_cal_fac, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'sigma_z_min', varin_disp%sigma_z_min, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'sigmaz_area_cal_fac', varin_disp%sigmaz_area_cal_fac, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'sigmaz_buoyant_cal_fac', varin_disp%sigmaz_buoyant_cal_fac, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'sigmaz_def', varin_disp%sigmaz_def, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'sigmaz_sec_area_cal_fac', varin_disp%sigmaz_sec_area_cal_fac, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'sigmaz_zi_split', varin_disp%sigmaz_zi_split, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'time_scale_Lagrange_coef1', varin_disp%time_scale_Lagrange_coef1, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'time_scale_Lagrange_coef2', varin_disp%time_scale_Lagrange_coef2, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'time_scale_Lagrange_max', varin_disp%time_scale_Lagrange_max, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'time_scale_Lagrange_min', varin_disp%time_scale_Lagrange_min, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'tl_high_source', varin_disp%tl_high_source, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'tl_low_source', varin_disp%tl_low_source, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'z_over_L_convec', varin_disp%z_over_L_convec, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'zwcor', varin_disp%zwcor, error)) GOTO 1000
!write(*,*) 'varin_disp:'
!write(*,*) varin_disp
!
!! Read emission_variables: 
!IF (.NOT. GetKeyValue(fu_varin,'ibtg', varin_emis%ibtg, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'nh3_ah_temp_coef', varin_emis%nh3_ah_temp_coef, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'nh3_ah_temp_corr_min', varin_emis%nh3_ah_temp_corr_min, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'nh3_app_coef1', varin_emis%nh3_app_coef1, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'nh3_app_coef2', varin_emis%nh3_app_coef2, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'nh3_app_coef3', varin_emis%nh3_app_coef3, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'nh3_app_coef4', varin_emis%nh3_app_coef4, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'nh3_app_coef5', varin_emis%nh3_app_coef5, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'nh3_app_rain_coef1', varin_emis%nh3_app_rain_coef1, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'nh3_app_rain_coef2', varin_emis%nh3_app_rain_coef2, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'nh3_app_rain_corr_max', varin_emis%nh3_app_rain_corr_max, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'nh3_app_rain_corr_min', varin_emis%nh3_app_rain_corr_min, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'plrise_ci_neutral', varin_emis%plrise_ci_neutral, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'plrise_nonstab_Fbhigh_coef1', varin_emis%plrise_nonstab_Fbhigh_coef1, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'plrise_nonstab_Fbhigh_coef2', varin_emis%plrise_nonstab_Fbhigh_coef2, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'plrise_nonstab_Fblow_coef1', varin_emis%plrise_nonstab_Fblow_coef1, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'plrise_nonstab_Fblow_coef2', varin_emis%plrise_nonstab_Fblow_coef2, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'plrise_nonstab_Fbsplit', varin_emis%plrise_nonstab_Fbsplit, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'plrise_ci_add_stab_unstab', varin_emis%plrise_ci_add_stab_unstab, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'plrise_stab_coef1', varin_emis%plrise_stab_coef1, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'plrise_stab_coef2', varin_emis%plrise_stab_coef2, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'plrise_stab_dtheta_dz', varin_emis%plrise_stab_dtheta_dz, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'STOKES', varin_emis%STOKES, error)) GOTO 1000
!IF (.NOT. GetKeyValue(fu_varin,'sigz0', varin_emis%szopp, error)) GOTO 1000
!write(*,*) 'varin_emis:'
!write(*,*) varin_emis


!$$$$ ! Read dry_deposition_variables: 
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'a1_cal_fac', varin_drydep%a1_cal_fac, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'b', varin_drydep%b, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'collecting_eff_nh4_coefs', varin_drydep%collecting_eff_nh4_coefs, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'collecting_eff_no3_coefs', varin_drydep%collecting_eff_no3_coefs, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'collecting_eff_so4_coefs', varin_drydep%collecting_eff_so4_coefs, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'d', varin_drydep%d, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'ddeppar', varin_drydep%ddeppar, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'dxeff_cal_fac1', varin_drydep%dxeff_cal_fac1, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'dxeff_cal_fac2', varin_drydep%dxeff_cal_fac2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'gamma_soil_c_fac', varin_drydep%gamma_soil_c_fac, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'gamma_stom_c_fac', varin_drydep%gamma_stom_c_fac, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'gamma_stom_coef1', varin_drydep%gamma_stom_coef1, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'gamma_stom_coef2', varin_drydep%gamma_stom_coef2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'gamma_tfac_coef1', varin_drydep%gamma_tfac_coef1, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'gamma_tfac_coef2', varin_drydep%gamma_tfac_coef2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'gamma_w_coef1', varin_drydep%gamma_w_coef1, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'gamma_w_coef2', varin_drydep%gamma_w_coef2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'gamma_w_coef3', varin_drydep%gamma_w_coef3, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'h', varin_drydep%h, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'h_u_vd_sec_aer_rough', varin_drydep%h_u_vd_sec_aer_rough, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'h_u_vd_sec_aer_smooth_coef1', varin_drydep%h_u_vd_sec_aer_smooth_coef1, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'hf_area_coef1', varin_drydep%hf_area_coef1, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'hf_area_coef2', varin_drydep%hf_area_coef2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'hf_area_coef3', varin_drydep%hf_area_coef3, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'mnt_select', varin_drydep%mnt_select, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'nwet_coef1', varin_drydep%nwet_coef1, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'nwet_coef2', varin_drydep%nwet_coef2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'nwet_coef3', varin_drydep%nwet_coef3, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'nwet_coef4', varin_drydep%nwet_coef4, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'nwet_coef5', varin_drydep%nwet_coef5, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'nwet_switch', varin_drydep%nwet_switch, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rc_missing', varin_drydep%rc_missing, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rc_negative', varin_drydep%rc_negative, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rc_so4_cal_fac', varin_drydep%rc_so4_cal_fac, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rc_tot_HNO3', varin_drydep%rc_tot_HNO3, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rc_tot_NO_wws', varin_drydep%rc_tot_NO_wws, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rcdeel', varin_drydep%rcdeel, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rchno3', varin_drydep%rchno3, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rcno', varin_drydep%rcno, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'resistance_part_coef1', varin_drydep%resistance_part_coef1, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'resistance_part_coef2', varin_drydep%resistance_part_coef2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'resistance_part_coef3', varin_drydep%resistance_part_coef3, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rinc_ustar_neg', varin_drydep%rinc_ustar_neg, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rsoil_NH3', varin_drydep%rsoil_NH3, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rsoil_NO', varin_drydep%rsoil_NO, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rsoil_NO2', varin_drydep%rsoil_NO2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rsoil_O3', varin_drydep%rsoil_O3, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rsoil_SO2', varin_drydep%rsoil_SO2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rw_NH3_coef1', varin_drydep%rw_NH3_coef1, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rw_NH3_coef2', varin_drydep%rw_NH3_coef2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rw_NH3_frozen', varin_drydep%rw_NH3_frozen, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rw_NO', varin_drydep%rw_NO, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rw_NO2', varin_drydep%rw_NO2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rw_O3', varin_drydep%rw_O3, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rw_SO2_dry_coef1', varin_drydep%rw_SO2_dry_coef1, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rw_SO2_dry_coef2', varin_drydep%rw_SO2_dry_coef2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rw_SO2_dry_frozen', varin_drydep%rw_SO2_dry_frozen, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rw_SO2_dry_frozen_extra', varin_drydep%rw_SO2_dry_frozen_extra, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rw_SO2_wet', varin_drydep%rw_SO2_wet, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'sinphi_coef1', varin_drydep%sinphi_coef1, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'sinphi_coef2', varin_drydep%sinphi_coef2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'temp_water_coef1', varin_drydep%temp_water_coef1, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'temp_water_coef2', varin_drydep%temp_water_coef2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'temp_water_coef3', varin_drydep%temp_water_coef3, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'travel_time_cal_fac', varin_drydep%travel_time_cal_fac, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'vg50trans_cal_fac', varin_drydep%vg50trans_cal_fac, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'vgdeel', varin_drydep%vgdeel, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'vgmax', varin_drydep%vgmax, error)) GOTO 1000
!$$$$ 
!$$$$ ! Read wet_deposition_variables: 
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'cmnd', varin_wetdep%cmnd, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'croutpri', varin_wetdep%croutpri, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'ddrup_coef1', varin_wetdep%ddrup_coef1, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'ddrup_coef2', varin_wetdep%ddrup_coef2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'distr_below_in_cloud_area_coefa', varin_wetdep%distr_below_in_cloud_area_coefa, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'distr_below_in_cloud_coef1', varin_wetdep%distr_below_in_cloud_coef1, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'epsilon', varin_wetdep%epsilon, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'lambda_alpha1', varin_wetdep%lambda_alpha1, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'lambda_alpha2', varin_wetdep%lambda_alpha2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'lambda_alpha3', varin_wetdep%lambda_alpha3, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'lambda_part_beta', varin_wetdep%lambda_part_beta, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'lambda_part_lambd0', varin_wetdep%lambda_part_lambd0, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'precip_dsource', varin_wetdep%precip_dsource, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'precip_hsource', varin_wetdep%precip_hsource, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'roratio', varin_wetdep%roratio, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'routsec', varin_wetdep%routsec, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'twet_cal_fac', varin_wetdep%twet_cal_fac, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'twt_cal_fac', varin_wetdep%twt_cal_fac, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'twt_cutoff', varin_wetdep%twt_cutoff, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'vdrup_coef1', varin_wetdep%vdrup_coef1, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'vdrup_coef2', varin_wetdep%vdrup_coef2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'wdeppar', varin_wetdep%wdeppar, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'x_wet_shift', varin_wetdep%x_wet_shift, error)) GOTO 1000
!$$$$ 
!$$$$ ! Read chemistry_variables: 
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'ar', varin_chem%ar, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'ar0', varin_chem%ar0, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'conc_cf', varin_chem%conc_cf, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'idgr', varin_chem%idgr, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'k_no3_night', varin_chem%k_no3_night, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'k_so4_aq', varin_chem%k_so4_aq, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'k_so4_he', varin_chem%k_so4_he, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'koh', varin_chem%koh, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'no2sek', varin_chem%no2sek, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'ratns', varin_chem%ratns, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rhno2', varin_chem%rhno2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rhno3_coef1', varin_chem%rhno3_coef1, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rhno3_coef2', varin_chem%rhno3_coef2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rhno3_max', varin_chem%rhno3_max, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rno2nox', varin_chem%rno2nox, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rno2nox_ave_nl', varin_chem%rno2nox_ave_nl, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rrno2nox_coef1', varin_chem%rrno2nox_coef1, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'rrno2nox_coef2', varin_chem%rrno2nox_coef2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'SCWINTER', varin_chem%SCWINTER, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'so2sek', varin_chem%so2sek, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'vchem_nh3_coef0', varin_chem%vchem_nh3_coef0, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'vchem_nh3_coef1', varin_chem%vchem_nh3_coef1, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'vchem_nh3_coef10', varin_chem%vchem_nh3_coef10, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'vchem_nh3_coef11', varin_chem%vchem_nh3_coef11, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'vchem_nh3_coef2', varin_chem%vchem_nh3_coef2, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'vchem_nh3_coef3', varin_chem%vchem_nh3_coef3, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'vchem_nh3_coef4', varin_chem%vchem_nh3_coef4, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'vchem_nh3_min', varin_chem%vchem_nh3_min, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'vchemc', varin_chem%vchemc, error)) GOTO 1000
!$$$$ IF (.NOT. GetKeyValue(fu_varin,'vchemv', varin_chem%vchemv, error)) GOTO 1000

! Read uncertainty variables:
IF (.NOT. GetKeyValue(fu_varin, 'plrise_fact', varin%varin_unc%plrise_fact, error)) GOTO 1000

! Close varin file (in case that file has been opened, but a read error occurred or if all went ok):
1000 CALL sysclose(fu_varin, namvarin, error)

! Label if file was not successfully opened:
1001 continue

! Error message:
IF (error%haserror) THEN
   CALL ErrorParam('input variables file', namvarin, error)
   CALL ErrorCall(ROUTINENAAM, error)   
ENDIF

END SUBROUTINE ops_read_varin

!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_varin_read2( namvarin, varin, error) 

! Open file with varin parameters (input model parameters), read parameter values and close file.
! This version differs from ops_read_varin, in that the order in the varin-file is irrelevant.
! ops_varin_read2 offers more possibilities for error handling, ops_varin_read3 is very limited in this aspect;
! it only tells you that there is an error in the namelist read, not in which line etc. 
! The variables in the file are defined in blocks, e.g. 
! VARIN varin_meteo
!    par1 val1
!    par2 val2
! END VARIN
!
! This will define 
!    varin_meteo%par1 = val1
!    varin_meteo%par2 = val2

USE m_getkey
USE m_error
USE m_commonfile, only: fu_varin, fu_scratch
USE m_fileutils

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - OUTPUT 
character(len=*), intent(in) :: namvarin ! name of varin file

TYPE(Tvarin),           INTENT(INOUT) :: varin
!TYPE(Tvarin_gen),       INTENT(INOUT) :: varin_gen                  ! general_variables
!TYPE(Tvarin_meteo),     INTENT(INOUT) :: varin_meteo                ! meteo_variables
!TYPE(Tvarin_disp),      INTENT(INOUT) :: varin_disp                 ! dispersion_variables
!TYPE(Tvarin_emis),      INTENT(INOUT) :: varin_emis                 ! emission_variables
!TYPE(Tvarin_drydep),    INTENT(INOUT) :: varin_drydep               ! dry_deposition_variables
!TYPE(Tvarin_wetdep),    INTENT(INOUT) :: varin_wetdep               ! wet_deposition_variables
!TYPE(Tvarin_chem),      INTENT(INOUT) :: varin_chem                 ! chemistry_variables
!TYPE(Tvarin_unc),       INTENT(INOUT) :: varin_unc
TYPE(TError),           INTENT(INOUT) :: error                      ! error handling record

! LOCAL VARIABLES
integer, parameter                  :: mvarin = 100        ! maximal number of varin variables in a structure
real                                :: range_perm(2)       ! permitted range for varin variable (low, high)
                                                           ! note that range is also used for integer variables (is no problem)
logical                             :: eof                 ! end of file has been reached
logical                             :: eob                 ! end of block with varin-variables has been reached
character(len = 100)                :: var_name            ! variable name
real                                :: var_value           ! variable value 
character(len = 100)                :: varin_block_name    ! name of block of varin variables, e.g. varin_meteo

CHARACTER*255, PARAMETER            :: ROUTINENAAM = 'ops_varin_read2'


!!-------------------------------------------------------------------------------------------
!! put field names of derived type varn_meteo into an array (part of code now obsolete)
!!-------------------------------------------------------------------------------------------
! namelist / meteo / varin_meteo
!
!! Write field names to scratch file:
!open(unit = fu_scratch, status = 'scratch')  
!write(unit = fu_scratch, nml = meteo, iostat = io_status)  
!! Check for errors iostat ....
!
!! Read field names from scratch file and put them in array varin_names:
!rewind(fu_scratch)
!eof = .false.
!ifield = 0
!do while (.not. eof) 
!   read(fu_scratch, '(a)',iostat = io_status) line
!   eof = (io_status .lt. 0) 
!   if (.not. eof) then
!      ifield = ifield + 1
!      
!      ! Field name is part before "=" (lower case):
!      varin_names(ifield) = trim(adjustl(LoCase(line(1:index(line,'=')-1))))
!   endif
!enddo
!close(fu_scratch)
!nfield = ifield
!do ifield = 1,nfield
!   write(*,*) 'aa ',trim(varin_names(ifield))
!enddo
!-------------------------------------------------------------------------------------------

! Open file with varin parameters:
IF (.NOT. sysopen(fu_varin, namvarin, 'r', 'varin file', error)) GOTO 1001

! Read header for first block with varin-variables:
if (.not. GetKeyValue(fu_varin, 'VARIN', varin_block_name, error)) goto 1000 

! Loop over file:
eof = .false.
do while (.not. eof) 

   ! Read next key value pair:
   IF (.not. GetKeyValue(fu_varin, '', var_value, error, var_name)) goto 1000
   if (error%haserror) write(*,*) 'Error ::',trim(error%message),'::'
   
   ! Check for end of file:
   eof = (var_name .eq. '@@end_of_file@@') 
   
   ! Check for end of block:
   IF (.not. eof) THEN
      eob = (var_name .eq. 'END_VARIN')
      IF (eob) THEN
         ! Read new block:
         IF (.not. GetKeyValue(fu_varin, 'VARIN', varin_block_name, error)) THEN
            ! It is ok to reach end-of-file while reading a new block:
            eof = (error%message .eq. 'Unexpected end-of-file') 
            IF (eof) THEN
               error%haserror = .false.
            ELSE
               goto 1000 ! other error
            ENDIF
         ENDIF  
      ENDIF   ! header for new block has been read
   ENDIF

   if (.not. eof) then
      ! Default settings for permitted range:
      range_perm = 0.0
      
      ! Fill value into structure varin_meteo:
      select case (varin_block_name)
      case ('varin_meteo')
         ! call ops_varin_get_index('varin_meteo',var_name,varin_names,ifield,found) 
         select case (var_name) ! there is no smart way to loop over fields in a derived type ....
            case ('ol_cutoff_iopt1'    ); varin%varin_meteo%ol_cutoff_iopt1     = var_value; range_perm = (/    1,   3 /);
            case ('ol_cutoff_iopt2'    ); varin%varin_meteo%ol_cutoff_iopt2     = var_value; range_perm = (/    1,   3 /);
            case ('ol_cutoff_iopt3'    ); varin%varin_meteo%ol_cutoff_iopt3     = var_value; range_perm = (/    1,   3 /);
            case ('ol_add_stable0'     ); varin%varin_meteo%ol_add_stable0      = var_value; range_perm = (/    0,  20 /);
            case ('ol_add_stable1'     ); varin%varin_meteo%ol_add_stable1      = var_value; range_perm = (/    0,  20 /);
            case ('ol_max_unstable0'   ); varin%varin_meteo%ol_max_unstable0    = var_value; range_perm = (/  -50,   0 /);
            case ('ol_max_unstable1'   ); varin%varin_meteo%ol_max_unstable1    = var_value; range_perm = (/ -100,   0 /);
            case ('ol_max_unstable2'   ); varin%varin_meteo%ol_max_unstable2    = var_value; range_perm = (/ -100,   0 /);
            case ('ol_min_stable1'     ); varin%varin_meteo%ol_min_stable1      = var_value; range_perm = (/    0, 100 /);
            case ('ol_min_stable2'     ); varin%varin_meteo%ol_min_stable2      = var_value; range_perm = (/    0, 100 /);
            case ('ol_min_stable3'     ); varin%varin_meteo%ol_min_stable3      = var_value; range_perm = (/    0, 100 /);
            case ('ol_z0_ratio_cutoff1'); varin%varin_meteo%ol_z0_ratio_cutoff1 = var_value; range_perm = (/    0, 500 /);
            case ('ol_z0_ratio_cutoff2'); varin%varin_meteo%ol_z0_ratio_cutoff2 = var_value; range_perm = (/    0, 500 /);
            case ('uster_cutoff_iopt1' ); varin%varin_meteo%uster_cutoff_iopt1  = var_value; range_perm = (/    1,   2 /);
            case ('uster_cutoff_iopt2' ); varin%varin_meteo%uster_cutoff_iopt2  = var_value; range_perm = (/    1,   2 /);
            case ('uster_min1'         ); varin%varin_meteo%uster_min1          = var_value; range_perm = (/    0,   5 /);
            case ('uster_min2'         ); varin%varin_meteo%uster_min2          = var_value; range_perm = (/    0,   5 /);
            case ('uster_min3'         ); varin%varin_meteo%uster_min3          = var_value; range_perm = (/    0,   5 /);
            case ('uz_cutoff1'         ); varin%varin_meteo%uz_cutoff1          = var_value; range_perm = (/    0,   5 /);
            case ('uz_cutoff2'         ); varin%varin_meteo%uz_cutoff2          = var_value; range_perm = (/    0,   5 /);
            case default
               call SetError('unknown variable for input variables block',error)
               call ErrorParam('input variables block',varin_block_name,error)
               call ErrorParam('input variable',var_name,error)
               goto 1000
         end select
      case ('varin_depos') 
          write(*,*) 'varin_depos not yet implemented' 
      case ("varin_unc")
         select case (var_name)
            case ('plrise_fact'); varin%varin_unc%plrise_fact = var_value; range_perm = (/ 0.0, huge(1.0) /);
            case ('sigz_fact'); varin%varin_unc%sigz_fact = var_value; range_perm = (/ 0.0, huge(1.0) /);

            ! TODO:
            case ('unc_sourcedepl')
               ! Use same value across all fields if only one value is given.
               varin%varin_unc%unc_sourcedepl%washout_pri_fact = var_value
               varin%varin_unc%unc_sourcedepl%washout_sec_fact = var_value
               varin%varin_unc%unc_sourcedepl%rainout_pri_fact = var_value
               varin%varin_unc%unc_sourcedepl%rainout_sec_fact = var_value
               varin%varin_unc%unc_sourcedepl%vd_drydep_pri_fact = var_value
               varin%varin_unc%unc_sourcedepl%vd_drydep_sec_fact = var_value
               varin%varin_unc%unc_sourcedepl%vchem_fact = var_value
               range_perm = (/ 0.0, huge(1.0) /)
            case ('unc_sourcedepl%washout_pri_fact'); varin%varin_unc%unc_sourcedepl%washout_pri_fact = var_value; range_perm = (/ 0.0, huge(1.0) /);
            case ('unc_sourcedepl%washout_sec_fact'); varin%varin_unc%unc_sourcedepl%washout_sec_fact = var_value; range_perm = (/ 0.0, huge(1.0) /);
            case ('unc_sourcedepl%rainout_pri_fact'); varin%varin_unc%unc_sourcedepl%rainout_pri_fact = var_value; range_perm = (/ 0.0, huge(1.0) /);
            case ('unc_sourcedepl%rainout_sec_fact'); varin%varin_unc%unc_sourcedepl%rainout_sec_fact = var_value; range_perm = (/ 0.0, huge(1.0) /);
            case ('unc_sourcedepl%vd_drydep_pri_fact'); varin%varin_unc%unc_sourcedepl%vd_drydep_pri_fact = var_value; range_perm = (/ 0.0, huge(1.0) /);
            case ('unc_sourcedepl%vd_drydep_sec_fact'); varin%varin_unc%unc_sourcedepl%vd_drydep_sec_fact = var_value; range_perm = (/ 0.0, huge(1.0) /);
            case ('unc_sourcedepl%vchem_fact'); varin%varin_unc%unc_sourcedepl%vchem_fact = var_value; range_perm = (/ 0.0, huge(1.0) /);
            case('unc_meteo')
               varin%varin_unc%unc_meteo%xl_fact = var_value; range_perm = (/ 0.0, huge(1.0) /)
               range_perm = (/ 0.0, huge(1.0) /)
            case ('unc_meteo%xl_fact'); varin%varin_unc%unc_meteo%xl_fact = var_value
            case('diurn_scale_index'); varin%varin_unc%diurn_scale_index = var_value; range_perm = (/ 0.0, 1.0 /)
         end select
      case default
         call SetError('unknown name for input variables block',error)
         call ErrorParam('name read from file',varin_block_name,error)
         call ErrorParam('allowed names','varin_meteo',error)
         goto 1000
      end select
      
      ! Check range:   var_value is real!
      ! See getkey check range
      if (range_perm(1) < range_perm(2) .and. (var_value < range_perm(1) .or. var_value  > range_perm(2))) then
         call SetError('variable read out of range',error)
         call ErrorParam('input variables block',varin_block_name,error)
         call ErrorParam('input variable',var_name,error)
         call ErrorParam('input value',var_value,error)
         call ErrorParam('allowed range',range_perm,error)
         goto 1000
      endif
   endif
enddo

! Close varin file (in case that file has been opened, but a read error occurred or if all went ok):
1000 CALL sysclose(fu_varin, namvarin, error)

! Label if file was not successfully opened:
1001 continue

! Error message:
IF (error%haserror) THEN
   CALL ErrorParam('input variables file', namvarin, error)
   CALL ErrorCall(ROUTINENAAM, error)   
ENDIF

END SUBROUTINE ops_varin_read2

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE ops_varin_read3( varin_gen, varin_meteo, varin_disp, varin_emis, varin_drydep, varin_wetdep, varin_chem, error) 
SUBROUTINE ops_varin_read3( namvarin, varin_meteo, error) 

! Open file with varin parameters (input model parameters), read parameter values (usig NAMELIST) and close file.
! This version differs from ops_read_varin, in that the order in the varin-file is irrelevant.
! Field names that are missing in the namelist-file, retain their initial value, defined in the type definition.
! ops_varin_read3 is very limited in error handling; it only tells you that there is an error in the namelist read,
! not in which line or for which variable. Better error handling (and range checking) is achieved by ops_varin_read2.

! ?? === WIP  === ??
! Alternative: do not use the derived types varin_meteo, but use data type 
! TYPE name_value
!    character(len=200) :: name     ! name of variable
!    character(len=1)   :: type_val ! 'r' (real) or 'i' (integer)
!    integer            :: ival     ! integer value
!    real               :: rval     ! real value
! END TYPE name_value
!
! type(name_value), allocatable :: varin_meteo(:)
! ...
!
! allocate ...
!
! i = 1;   varin_meteo(i)%name = 'ol_cutoff_iopt1'; varin_meteo(i)%type_val = 'i';  varin_meteo(i)%ival = 1
! i = i+1; varin_meteo(i)%name = 'ol_cutoff_iopt2'; varin_meteo(i)%type_val = 'i';  varin_meteo(i)%ival = 2
! i = i+1; varin_meteo(i)%name = 'ol_cutoff_iopt3'; varin_meteo(i)%type_val = 'i';  varin_meteo(i)%ival = 3
! i = i+1; varin_meteo(i)%name = 'ol_add_stable0' ; varin_meteo(i)%type_val = 'r';  varin_meteo(i)%rval = 0.0
! i = i+1; varin_meteo(i)%name = 'ol_add_stable1' ; varin_meteo(i)%type_val = 'r';  varin_meteo(i)%rval = 5.0
! ...            
! Using this data type makes it much easier to loop over all variable names. 
! WIP; deal with different varin types varin_meteo, varin_gen, ...
! ?? === WIP  === ?? 
!
! Example namelist-file:
! &meteo
!    varin_meteo%uz_cutoff1 = 0.5  ! cutoff value for low wind velocities [m/s], option 1 (readmb and ops_st_meteo_boundary_layer in OPS-ST)
!    varin_meteo%uz_cutoff2 = 0.75 ! cutoff value for low wind velocities [m/s], option 2 (ops_wv_log_profile in OPS-lib)
! /
!

USE m_error
USE m_commonfile, only: fu_varin, IOB_STDOUT
USE m_fileutils

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - OUTPUT 
character(len=*), intent(in) :: namvarin ! name of varin file

!TYPE(Tvarin_gen),       INTENT(OUT) :: varin_gen                  ! general_variables
TYPE(Tvarin_meteo),     INTENT(OUT) :: varin_meteo                ! meteo_variables
!TYPE(Tvarin_disp),      INTENT(OUT) :: varin_disp                 ! dispersion_variables
!TYPE(Tvarin_emis),      INTENT(OUT) :: varin_emis                 ! emission_variables
!TYPE(Tvarin_drydep),    INTENT(OUT) :: varin_drydep               ! dry_deposition_variables
!TYPE(Tvarin_wetdep),    INTENT(OUT) :: varin_wetdep               ! wet_deposition_variables
!TYPE(Tvarin_chem),      INTENT(OUT) :: varin_chem                 ! chemistry_variables
TYPE(TError),           INTENT(INOUT) :: error                    ! error handling record

! Namelist definition:
namelist / meteo / varin_meteo

! LOCAL VARIABLES
integer                             :: io_status                  ! I/O status

CHARACTER*255, PARAMETER            :: ROUTINENAAM = 'ops_varin_read3'

! Open namelist file with varin parameters:
IF (sysopen(fu_varin, namvarin, 'r', 'varin file', error)) THEN

   ! Read namelist file:
   read (unit = fu_varin, nml=meteo, iostat=io_status)
   if (io_status .ne. 0) then
      call SetError('Invalid format reading Namelist file',error)
      call ErrorParam('I/O status', io_status, error)
   endif

   ! Close namelist file:
   CALL sysclose(fu_varin, namvarin, error)
ENDIF

! Add file name to error message:
IF (error%haserror) THEN
   CALL ErrorParam('input variables file', namvarin, error)
   CALL ErrorCall(ROUTINENAAM, error)   
ENDIF

END SUBROUTINE ops_varin_read3

!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_varin_get_index(type_name,var_name,varin_names,indx,found)

use m_error

! Input:
character(len=*), intent(in)       :: type_name      ! name of derived type
character(len=*), intent(in)       :: var_name       ! name of variable
character(len=*), intent(in)       :: varin_names(*) ! list of names of variable

! Output
integer,          intent(out)      :: indx         ! index of name of variable in list
logical,          intent(out)      :: found        ! variable name has been found

! Local
integer                            :: i            ! index in list

CHARACTER*255, PARAMETER            :: ROUTINENAAM = 'ops_varin_get_index'

! Loop over list and check if var_name has been found:
found = .false.
i = 0
do while (i < len(varin_names) .and. .not. found)
   i = i + 1
!write(*,*) '1/',trim(type_name // '%' // var_name),'/1' 
!write(*,*) '2/',trim(varin_names(i)),'/2' 
   found = (type_name // '%' // var_name .eq. varin_names(i))
   if (found) indx = i
enddo
     
END SUBROUTINE ops_varin_get_index

!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_varin_write( fu_prt, varin) 

! Write varin parameters (input model parameters) to PRNFILE.

USE m_error

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - OUTPUT 
integer, intent(in)                 :: fu_prt                     ! unit number of print file

TYPE(Tvarin),            INTENT(IN) :: varin
!TYPE(Tvarin_gen),       INTENT(IN) :: varin_gen                  ! general_variables
!TYPE(Tvarin_meteo),      INTENT(IN) :: varin_meteo                ! meteo_variables
!TYPE(Tvarin_disp),      INTENT(IN) :: varin_disp                 ! dispersion_variables
!TYPE(Tvarin_emis),      INTENT(IN) :: varin_emis                 ! emission_variables
!TYPE(Tvarin_drydep),    INTENT(IN) :: varin_drydep               ! dry_deposition_variables
!TYPE(Tvarin_wetdep),    INTENT(IN) :: varin_wetdep               ! wet_deposition_variables
!TYPE(Tvarin_chem),      INTENT(IN) :: varin_chem                 ! chemistry_variables
!TYPE(Tvarin_unc),        INTENT(IN) :: varin_unc

if (varin%file .ne. '') then
    write(fu_prt,*) '---------------------------------------------------------------------'
    write(fu_prt,*) 'VARIN_METEO'
    write(fu_prt,*) '---------------------------------------------------------------------'
    write(fu_prt,*) 'ol_cutoff_iopt1     [-]   = ',varin%varin_meteo%ol_cutoff_iopt1
    write(fu_prt,*) 'ol_cutoff_iopt2     [-]   = ',varin%varin_meteo%ol_cutoff_iopt2
    write(fu_prt,*) 'ol_cutoff_iopt3     [-]   = ',varin%varin_meteo%ol_cutoff_iopt3
    write(fu_prt,*) 'ol_add_stable0      [m]   = ',varin%varin_meteo%ol_add_stable0
    write(fu_prt,*) 'ol_add_stable1      [m]   = ',varin%varin_meteo%ol_add_stable1
    write(fu_prt,*) 'ol_max_unstable0    [m]   = ',varin%varin_meteo%ol_max_unstable0
    write(fu_prt,*) 'ol_max_unstable1    [m]   = ',varin%varin_meteo%ol_max_unstable1
    write(fu_prt,*) 'ol_max_unstable2    [m]   = ',varin%varin_meteo%ol_max_unstable2
    write(fu_prt,*) 'ol_min_stable1      [m]   = ',varin%varin_meteo%ol_min_stable1
    write(fu_prt,*) 'ol_min_stable2      [m]   = ',varin%varin_meteo%ol_min_stable2
    write(fu_prt,*) 'ol_min_stable3      [m]   = ',varin%varin_meteo%ol_min_stable3
    write(fu_prt,*) 'ol_z0_ratio_cutoff1 [-]   = ',varin%varin_meteo%ol_z0_ratio_cutoff1
    write(fu_prt,*) 'ol_z0_ratio_cutoff2 [-]   = ',varin%varin_meteo%ol_z0_ratio_cutoff2
    write(fu_prt,*) 'uster_cutoff_iopt1  [-]   = ',varin%varin_meteo%uster_cutoff_iopt1
    write(fu_prt,*) 'uster_cutoff_iopt2  [-]   = ',varin%varin_meteo%uster_cutoff_iopt2
    write(fu_prt,*) 'uster_min1          [m/s] = ',varin%varin_meteo%uster_min1
    write(fu_prt,*) 'uster_min2          [m/s] = ',varin%varin_meteo%uster_min2
    write(fu_prt,*) 'uster_min3          [m/s] = ',varin%varin_meteo%uster_min3
    write(fu_prt,*) 'uz_cutoff1          [m/s] = ',varin%varin_meteo%uz_cutoff1
    write(fu_prt,*) 'uz_cutoff2          [m/s] = ',varin%varin_meteo%uz_cutoff2
    write(fu_prt,*) ''

    write(fu_prt,*) '---------------------------------------------------------------------'
    write(fu_prt,*) 'VARIN_UNC'
    write(fu_prt,*) '---------------------------------------------------------------------'
    write(fu_prt,*) 'plrise_fact                        [-]   = ',varin%varin_unc%plrise_fact
    write(fu_prt,*) 'unc_sourcedepl%washout_pri_fact    [-]   = ',varin%varin_unc%unc_sourcedepl%washout_pri_fact
    write(fu_prt,*) 'unc_sourcedepl%washout_sec_fact    [-]   = ',varin%varin_unc%unc_sourcedepl%washout_sec_fact
    write(fu_prt,*) 'unc_sourcedepl%rainout_pri_fact    [-]   = ',varin%varin_unc%unc_sourcedepl%rainout_pri_fact
    write(fu_prt,*) 'unc_sourcedepl%rainout_sec_fact    [-]   = ',varin%varin_unc%unc_sourcedepl%rainout_sec_fact
    write(fu_prt,*) 'unc_sourcedepl%vd_drydep_pri_fact  [-]   = ',varin%varin_unc%unc_sourcedepl%vd_drydep_pri_fact
    write(fu_prt,*) 'unc_sourcedepl%vd_drydep_sec_fact  [-]   = ',varin%varin_unc%unc_sourcedepl%vd_drydep_sec_fact
    write(fu_prt,*) 'unc_sourcedepl%vchem_fact          [-]   = ',varin%varin_unc%unc_sourcedepl%vchem_fact
    write(fu_prt,*) 'unc_meteo%xl_fact                  [-]   = ',varin%varin_unc%unc_meteo%xl_fact
    ! TODO:
    write(fu_prt,*) ''
endif

END SUBROUTINE ops_varin_write

END MODULE m_ops_varin
