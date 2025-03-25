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

!-------------------------------------------------------------------------------------------------------------------------------
! DESCRIPTION        : Module for chemical conversion rates.
!                      Contains three subroutines:
!                      ops_vchem:                      compute chemical conversion rate for acidifying cmponents (from EMEP or parameterised)
!                                                      or for non-acidifying components based on input parameters.
!                      ops_vchem_ratio_no2_nox:        compute r_no2_nox = [NO2]/[NOx] ratio for current source - receptor, 
!                                                      either based on the vdHout formula (near roads) or based on background concentrations.
!                      ops_vchem_ratio_no2_nox_vdhout: compute sector averaged [NO2]/[NOx] ratio, according to vdHout parameterisation for roads, 
!                                                      for all wind sectors, all receptors and add road contributions to NO2 concentration.
!                      ops_vchem_add_nox_no2:          Add NOx (in case of road correction) or NO2 contribution from current source
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_vchem

use m_aps

implicit none

type Tvchem
  
   TYPE (TApsGridReal) :: mass_prec_grid               ! APS grid with column averaged mass of precursor pre chemistry step (from chemistry model, e.g. EMEP) [ug/m2]
   TYPE (TApsGridReal) :: mass_conv_dtfac_grid         ! APS grid with (100/dt) * column averaged mass, converted during chemistry step (from chemistry model, e.g. EMEP) [(ug/m2) (%/h)]

end type Tvchem

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_vchem(icm, isec, iopt_vchem, vchemc, vchemv, vchemnh3, rad, rad_W_m2, regenk, iseiz, istab, itra, ar, koh, &
                     isec_prelim, disxx, r_no2_nox_sec, r_no2_nox_year_bg_tra, r_no2_nox_season, ibroncat, nemcat_road, road_chem, emcat_road, & 
                     varin_unc, r_no2_nox, lroad_corr, vchem)

! Compute NO2/NOx ratio and chemical conversion rate for acidifying cmponents (from EMEP or parameterised) or 
! for non-acidifying components based on input parameters. Chemical conversion rate here depends on meteo class,
! see also ops_par_chem for chemical data and EMEP conversion rates that do not depend on meteo class.

USE m_commonconst_lt
USE m_ops_varin, ONLY: Tvarin_unc

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_vchem')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: icm                        ! component number
LOGICAL,   INTENT(IN)                            :: isec                       ! TRUE if component=[SO2, NOx, NH3]
INTEGER,   INTENT(IN)                            :: iopt_vchem                 ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
REAL,      INTENT(IN)                            :: vchemc                     ! chemical conversion rate [%/h]
REAL,      INTENT(IN)                            :: vchemv                     ! light dependent part of chemical conversion rate
REAL,      INTENT(IN)                            :: vchemnh3                   ! chemical conversion rate for NH3 -> NH4 [%/h]
REAL,      INTENT(IN)                            :: rad                        ! global radiation [J/cm2/h]
REAL,      INTENT(IN)                            :: rad_W_m2                   ! global radiation [W/m2]
REAL,      INTENT(IN)                            :: regenk                     ! rain probability [-]
INTEGER,   INTENT(IN)                            :: iseiz                      ! 0 = long term, 1 = year, 2 = winter, 3 = summer, 4 = month in winter, 5 = month in summer
INTEGER,   INTENT(IN)                            :: istab                      ! index of stability class
INTEGER,   INTENT(IN)                            :: itra                       ! index of trajectory class
REAL,      INTENT(IN)                            :: ar                         ! proportionality constant [ppb J-1 cm2 h] in relation [OH] = ar Qr
REAL,      INTENT(IN)                            :: koh                        ! reaction constant [ppb-1 h-1] for NO2 + OH -> HNO3
INTEGER,   INTENT(IN)                            :: isec_prelim                ! index of preliminary source-receptor wind sector (wind shear not yet taken into account)
REAL,      INTENT(IN)                            :: disxx                      ! effective travel distance between source and receptor [m]
REAL,      INTENT(IN)                            :: r_no2_nox_sec(NSEK)        ! sector averaged NO2/NOx ratio according to vdHout parameterisation [-]
REAL,      INTENT(IN)                            :: r_no2_nox_year_bg_tra      ! component of NO2/NOx ratio which is based on yearly averaged background concentrations over a trajectory
REAL,      INTENT(IN)                            :: r_no2_nox_season           ! component of NO2/NOx ratio which is season dependent  
INTEGER,   INTENT(IN)                            :: ibroncat                   ! emission category number
INTEGER,   INTENT(IN)                            :: nemcat_road                ! number of road emission categories (for vdHout NO2/NOx ratio)
INTEGER,   INTENT(IN)                            :: emcat_road(*)              ! list of road emission categories (for vdHout NO2/NOx ratio)
LOGICAL,   INTENT(IN)							 :: road_chem					 !switch for road chemistry GTHO
TYPE(Tvarin_unc), INTENT(IN)                     :: varin_unc


! SUBROUTINE ARGUMENTS - INPUT/OUTPUT
REAL,      INTENT(INOUT)                         :: r_no2_nox                  ! NO2/NOx ratio [-]
                                                                               ! NB: INOUT, because r_no2_nox keeps its original value when icm /= icm_NOx
LOGICAL,   INTENT(INOUT)                         :: lroad_corr                 ! road correction needed for NO2/NOx ratio
                                                                               ! NB: INOUT, because lroad_corr keeps its original value when icm /= icm_NOx
REAL,      INTENT(INOUT)                         :: vchem                      ! chemical conversion rate [%/h]. If option EMEP this is already set in ops_par_chem.
                                                                              
! LOCAL VARIABLES
REAL                                             :: frac_night_hours           ! fraction of nighttime hours [-]
REAL                                             :: chemn                      ! chemical conversion rate for NO2+O3 -> NO3 (nigthttime) [%/h]
REAL                                             :: chemr                      ! chemical conversion rate for NO2 + OH -> HNO3 (daytime) [%/h]

!-------------------------------------------------------------------------------------------------------------------------------

! Compute r_no2_nox = [NO2]/[NOx] ratio:
if (icm == icm_NOx) call ops_vchem_ratio_no2_nox(iseiz, istab, isec_prelim, disxx, r_no2_nox_sec, r_no2_nox_year_bg_tra, r_no2_nox_season, & 
                                             ibroncat, nemcat_road, road_chem, emcat_road, r_no2_nox, lroad_corr)

IF (isec) THEN


   !---------------------------------------------------------------------------------------------------------
   ! Compute chemical conversion rate (%/h) for acidifying components (icm = icm_SO2,icm_NOx,icm_NH3)
   !---------------------------------------------------------------------------------------------------------

   IF (iopt_vchem .eq. 1) THEN
   
      !--------------------------------------------------------------------------------
      ! Chemical conversion rate from EMEP maps; vchem is already set in ops_par_chem
      !--------------------------------------------------------------------------------
      continue

   ELSE
   
      !------------------------------------------------------
      ! Old parameterisation of chemical conversion rates
      !------------------------------------------------------
      IF (icm == icm_SO2) THEN
         
         !-----------------------------------
         ! icm = icm_SO2: SO2
         !-----------------------------------
         ! Compute vchem    : chemical conversion rate for SO2 -> SO4 (%/h)
         !
         ! ar = proportionality constant [ppb J-1 cm2 h] in relation [OH] = ar Qr, with 
         ! [OH] = OH radical concentration [ppb] , Qr = global radiation in J/cm2/h, see 
         ! Van Egmond N.D. and Kesseboom H. (1985) A numerical mesoscale model for long-term average NOx and NO2-concentration. 
         ! Atmospheric Environment 19, 587-595.
         ! Table 6.1 OPS-report: 
         ! ar(summer) = 7345 molec cm-3 W-1 m2
         ! ar(winter) = 3540 molec cm-3 W-1 m2.
         ! Cnversion factors seconds_per_hour = 3600; cm2_per_m2 = 10^4.
         !    at T = 20 C, p = 1013 mb:
         !       ppbfac = conversion factor ppb -> molec/cm3 = 2.5029e10 molec/cm3/ppb
         !       ar(summer) = 7345*cm2_per_m2/(ppbfac*seconds_per_hour) = 81.5e-8 ppb J-1 cm2 h.
         !       ar(winter) = 3540*cm2_per_m2/(ppbfac*seconds_per_hour) = 39.3e-8 ppb J-1 cm2 h.
         !       ar(year)   = average of ar_winter) and ar_wummer)      = 60.4e-8 ppb J-1 cm2 h.
         !    at T = 25 C, p = 1013 mb:
         !       ppbfac     =  2.4610e+010 molec/cm3/ppb
         !       ar(summer) =  82.9e-8 ppb J-1 cm2 h.
         !       ar(winter) =  40.0e-8 ppb J-1 cm2 h.
         !       ar(year)   =  61.4e-8 ppb J-1 cm2 h.
         !
         ! For a specific month, a cos-function over the year is used, such that ar(average) = 62e-8, ar(Feb) = 40e-8, ar(Aug) = 83e-8.
         !    Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec
         !    32.1 40.1 54.0 70.0 83.9 91.9 91.9 83.9 70.0 54.0 40.1 32.1 *1e-8
         !
         ! k_ho	SO2 + OH -> sulphate (gas phase)      3.44 x 10-12 cm3 molec-1 s-1 
         !        at T = 20 C, p = 1013 mb: ppbfac = conversion factor ppb -> molec/cm3 = 2.5029e10 molec/cm3/ppb ->
         !        k_ho = 3.44e-12*3600*2.5029e10 = 309.96 ppb-1 h-1       
         !        k_ho*[OH] = k_ho*ar*Q = 100*309.96*62e-8*Q %/h = 0.0192*Q %/h  
         ! k_he	SO2 -> sulphate (Particle phase) 1.7 x 10-6 s-1 = 0.612 %/h
         ! k_aq	SO2 -> sulphate (Aqueous phase)  4.0 x 10-5 s-1 = 14.4  %/h
         !
         ! Note: in source code 1.2*[0.016 0.5 12] = [0.0192 0.6 14.4] = [ar khe kaq]
      
         vchem = 1.2*((rad*.016) + .5 + (regenk*12.))
         ! write(*,*) 'ops_vchem, vchem: ',vchem
      
      ELSE IF (icm == icm_NOx) THEN
      
         !-----------------------------------
         ! icm = icm_NOx: NOx
         !-----------------------------------
         ! Compute frac_night_hours = fraction of nighttime hours, depending on season;
         ! NACHTZOMER and NACHTWINTER are relative occurrences (%) of nighttime hours in summer and winter,
         ! for each stability class and distance class. ("NACHT" = night, "ZOMER" = summer)
         IF ((iseiz .EQ. 3) .OR. (iseiz .EQ. 5)) THEN
            frac_night_hours = FLOAT(NACHTZOMER(istab, itra))/100.
         ELSE IF ((iseiz .EQ. 2) .OR. (iseiz .EQ. 4)) THEN
            frac_night_hours = FLOAT(NACHTWINTER(istab, itra))/100.
         ELSE
            frac_night_hours = FLOAT(NACHTWINTER(istab, itra) + NACHTZOMER(istab, itra))/200.
         ENDIF
      
         ! Compute chemn = chemical conversion rate for NO2+O3 -> NO3 (nigthttime), assuming a 2%/h conversion rate
         ! Van Egmond N.D. and Kesseboom H. (1983) Mesoscale air pollution dispersion models-II. Lagrangian PUFF model,
         ! and comparison with Eulerian GRID model. Atmospheric Environment, 17, 265-274.   
         chemn = frac_night_hours*2.
      
         ! chemr : chemical conversion rate for NO2 + OH -> HNO3 (daytime); [%/h] (factor 100 is to make percentage instead of fractions)
         !         rad : global radiation [J/cm2/h]
         !         ar  : proportionality constant [ppb J-1 cm2 h] in relation [OH] = ar Qr, with 
         !               [OH] = OH radical concentration [ppb] , Qr = global radiation [J/cm2/h] 
         !         koh : reaction constant [ppb-1 h-1] (Van Aalst en Bergsma, 1981)
         ! vchem  : total chemical conversion rate, split into daytime and nighttime part
         chemr    = 100*rad*ar*koh*r_no2_nox
      
         ! vchem  : total chemical conversion rate, split into daytime and nighttime part
         vchem = chemr + chemn
         
      ELSE IF (icm == icm_NH3) THEN
      
         !-----------------------------------
         !   icm = icm_NH3: NH3  
         !-----------------------------------
         ! Compute vchem = chemical conversion rate for NH3 -> NH4 conversion [%/h]
         vchem = vchemnh3
      ELSE
         write(*,*) 'internal programming error in ops_vchem; isec and icm < icm_SO2 or icm > icm_NH3'
         write(*,*) isec,icm
         stop 1
      ENDIF ! IF icm = icm_SO2,icm_NOx or icm_NH3

      ! Adjustment relevant for sensitivity analyses. Multiplication factor is 1.0 by default.
      vchem = varin_unc%unc_sourcedepl%vchem_fact * vchem
   ENDIF ! iopt_vchem
   
ELSE
      !-------------------------------------------------------------------------------
      ! Compute chemical conversion rate for non-acidifying components (.not. isec)
      !--------------------------------------------------------------------------------
      
      ! Chemical conversion rate is partly constant and partly radiation dependent (OPS report Eq. 7.2):
      vchem = vchemc + vchemv*rad_W_m2

      ! Adjustment relevant for sensitivity analyses. Multiplication factor is 1.0 by default.
      vchem = varin_unc%unc_sourcedepl%vchem_fact * vchem
ENDIF

END SUBROUTINE ops_vchem

!---------------------------------------------------------------------
SUBROUTINE ops_vchem_ratio_no2_nox(iseiz, istab, isec_prelim, disxx, r_no2_nox_sec, r_no2_nox_year_bg_tra, r_no2_nox_season, & 
                                   ibroncat, nemcat_road, road_chem, emcat_road, r_no2_nox, lroad_corr)

! Compute r_no2_nox = [NO2]/[NOx] ratio for current source - receptor, 
! either based on the vdHout formula (near roads) or based on background concentrations.

USE m_commonconst_lt, only: NSEK, SCWINTER

! SUBROUTINE ARGUMENTS - INPUT
INTEGER, INTENT(IN)                            :: iseiz                      ! 0 = long term, 1 = year, 2 = winter, 3 = summer, 4 = month in winter, 5 = month in summer
INTEGER, INTENT(IN)                            :: istab                      ! index of stability class
INTEGER, INTENT(IN)                            :: isec_prelim                ! index of preliminary source-receptor wind sector (wind shear not yet taken into account)
REAL,    INTENT(IN)                            :: disxx                      ! effective travel distance between source and receptor [m]
REAL,    INTENT(IN)                            :: r_no2_nox_sec(NSEK)        ! sector averaged NO2/NOx ratio according to vdHout parameterisation [-]
REAL,    INTENT(IN)                            :: r_no2_nox_year_bg_tra      ! component of NO2/NOx ratio which is based on yearly averaged background concentrations over a trajectory
REAL,    INTENT(IN)                            :: r_no2_nox_season           ! component of NO2/NOx ratio which is season dependent  
INTEGER, INTENT(IN)                            :: ibroncat                   ! emission category number
INTEGER, INTENT(IN)                            :: nemcat_road                ! number of road emission categories (for vdHout NO2/NOx ratio)
LOGICAL, INTENT(IN)							   :: road_chem					 !switch for road chemistry GTHO
INTEGER, INTENT(IN)                            :: emcat_road(*)              ! list of road emission categories (for vdHout NO2/NOx ratio)

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,    INTENT(OUT)                           :: r_no2_nox                  ! NO2/NOx ratio
LOGICAL, INTENT(OUT)                           :: lroad_corr                 ! road correction needed for NO2/NOx ratio

! LOCAL VARIABLES
REAL                                           :: scno2nox                   ! stability class dependent component in NO2/NOx-ratio (only in winter)
real,    parameter                             :: dist_road_corr = 5000.0    ! distance beyond which there is no road correction [m]

!------------------------------------------------------------------------------------

! Check distance to road and whether r_nox_nox_vdhout has been filled already (in the first iteration step, r_no2_nox_sec < 0 -> is not filled):
IF (lroad_corr .and. r_no2_nox_sec(1) .ge. 0.0) THEN

   ! Get NO2/NOx ratio near a road (sector average) from previous iteration:
   r_no2_nox = r_no2_nox_sec(isec_prelim)
ELSE
   ! NO2/NOx ratio based on background concentrations:
   ! r_no2_nox consists of a space varying component (r_no2_nox_year_bg_tra, computed in ops_par_chem), a season dependent component (r_no2_nox_season, set in ops_init)
   ! and a stability class dependent component (scno2nox, only in winter)
   IF ((iseiz .EQ. 2) .OR. (iseiz .EQ. 4)) THEN
      scno2nox = SCWINTER(istab)
   ELSE
      scno2nox = 1.
   ENDIF
   r_no2_nox = r_no2_nox_year_bg_tra*r_no2_nox_season*scno2nox
ENDIF

END SUBROUTINE ops_vchem_ratio_no2_nox

!---------------------------------------------------------------------
SUBROUTINE ops_vchem_ratio_no2_nox_vdhout(iter,nrrcp,o3bg_rcp,o3bgtra,nsrc_sec,cnox_sec,cno2,percvk_sec,r_no2_nox_sec,cnox)

! Compute r_no2_nox_sec = sector averaged [NO2]/[NOx] ratio, according to vdHout parameterisation for roads, 
! for all wind sectors, all receptors and add road contributions to NO2 concentration.

USE m_commonconst_lt, only: NSEK

! SUBROUTINE ARGUMENTS - INPUT
INTEGER                                        :: iter                       ! iteration counter
INTEGER                                        :: nrrcp                      ! number of receptors
REAL,    INTENT(IN)                            :: o3bg_rcp(NSEK,nrrcp)       ! O3 background concentration at receptor for all wind sectors [ug/m3] 
REAL,    INTENT(IN)                            :: o3bgtra(NSEK)              ! O3 background concentration average over trajectory [ug/m3] 
INTEGER, INTENT(IN)                            :: nsrc_sec(NSEK,nrrcp)       ! number of sources present in wind sector (roads only) [-]

! SUBROUTINE ARGUMENTS - INPUT/OUTPUT
REAL,    INTENT(INOUT)                         :: cnox_sec(NSEK,nrrcp)       ! wind sector averaged NOx concentration (roads only) [ug/m3]
REAL,    INTENT(INOUT)                         :: cno2(nrrcp)                ! NO2 concentration (derived from NOx and parameterised ratio NO2/NOx) [ug/m3]
REAL,    INTENT(INOUT)                         :: percvk_sec(NSEK,nrrcp)     ! frequency of occurrence of wind sector (roads only) [-]

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,    INTENT(OUT)                           :: r_no2_nox_sec(NSEK,nrrcp)  ! sector averaged NO2/NOx ratio according to vdHout parameterisation [-]
REAL,    INTENT(OUT)                           :: cnox(nrrcp)                ! NOx concentration, per receptor [ug/m3]

! LOCAL VARIABLES
real,    parameter                             :: f_dir_no2      = 0.15      ! fraction directly emitted NO2 [-]
real,    parameter                             :: K_road_corr    = 100       ! empirical parameter in vdHout formula for NO2 from roads = 100 μg/m3




! Compute NO2/NOx ratio using formula of vd Hout (1988) near roads.
! Hout, K.D. van den, and Baars, H.P., 1988: 
! Development of two models for the dispersion of pollution from traffic: 
! the TNO Traffic Model and the CAR Model. TNO report 88/192 (in Dutch).
!
! NO2/NOx = fNO2 + O3*(1-fNO2)/((1-fNO2)*NOx + K)
! 
! fNO2 : f_dir_no2 = fraction directly emitted NO2 [-] 
! K    : K_road_corr = empirical parameter = 100 μg/m3
! O3   : ozone concentration [μg/m3]


! Average wind sector frequency over all sources in that wind sector
! (note: percvk_sec does not change during iteration):
if (iter == 1) then
   where(nsrc_sec > 0)
      percvk_sec = percvk_sec/nsrc_sec
   elsewhere
      percvk_sec = 0.0
   endwhere
endif

! Compute wind sector averaged NOx concentration and NO2/NOx ratio (vd Hout): 
where(nsrc_sec > 0)
   cnox_sec      = cnox_sec/percvk_sec ! vdHout is applied to a situation where we have a frequency of occurrence = 100% in that sector
   r_no2_nox_sec = f_dir_no2 + o3bg_rcp*(1-f_dir_no2)/((1-f_dir_no2)*cnox_sec + K_road_corr)
elsewhere
   cnox_sec      = 0.0
   r_no2_nox_sec = 0.0
endwhere

! Add road contributions of NO2 concentration as a weighed sum over wind sectors (dimension 1): 
cno2 = cno2 + sum(percvk_sec*r_no2_nox_sec*cnox_sec,1) 

! Compute NOx concentration as a weighed sum over wind sectors (dimension 1): 
cnox = sum(percvk_sec*cnox_sec,1) 

! ! Debug output:
! write(*,'(a30,a6,99(1x,i12))')      'vdhout, ircp,isector;       ','ircp',(isek, isek = 1,12)
! do ircp = 1,nrrcp
!    write(*,'(a30,i6,99(1x,e12.5))') 'vdhout, ircp,percvk_sec;    ', ircp,percvk_sec(:,ircp)
!    write(*,'(a30,i6,99(1x,i12))')   'vdhout, ircp,nsrc_sec;      ', ircp,nsrc_sec(:,ircp)
!    write(*,'(a30,i6,99(1x,e12.5))') 'vdhout, ircp,cnox_sec;      ', ircp,cnox_sec(:,ircp)
!    write(*,'(a30,i6,99(1x,e12.5))') 'vdhout, ircp,o3bg_sec;      ', ircp,o3bg_rcp(:,ircp)
!    write(*,'(a30,i6,99(1x,e12.5))') 'vdhout, ircp,r_no2_nox_sec; ', ircp,r_no2_nox_sec(:,ircp)
!    write(*,'(a30,i6,99(1x,e12.5))') 'vdhout, ircp,cno2_sec;      ', ircp,r_no2_nox_sec(:,ircp)*cnox_sec(:,ircp)
! enddo

END SUBROUTINE ops_vchem_ratio_no2_nox_vdhout

!---------------------------------------------------------------------
SUBROUTINE ops_vchem_add_nox_no2(lroad_corr, iter, isec_prelim, c, r_no2_nox, percvk, &
                                 lroad_corr_present, cnox_sec, cno2, percvk_sec, nsrc_sec, nstab_present)

use m_commonconst_lt, only: NSEK

! Add NOx (in case of road correction) or NO2 contribution from current source;
! in case of road correction, NO2 is computed after all contributions have been added.

! Input variables:
LOGICAL,   INTENT(IN)                            :: lroad_corr                 ! road correction needed for NO2/NOx ratio
INTEGER,   INTENT(IN)                            :: iter                       ! iteration index for road correction
INTEGER,   INTENT(IN)                            :: isec_prelim                ! index of preliminary source-receptor wind sector (wind shear not yet taken into account)
REAL,      INTENT(IN)                            :: c                          ! concentration at receptor height zm [ug/m3]
REAL,      INTENT(IN)                            :: r_no2_nox                  ! NO2/NOx ratio [-]
REAL,      INTENT(IN)                            :: percvk                     ! fraction of occurrence of {distance/stability/wind-direction} class

! Input/output variables:
LOGICAL,   INTENT(INOUT)                         :: lroad_corr_present         ! at least one road with vdHout correction is present
REAL,      INTENT(INOUT)                         :: cnox_sec(:)                ! wind sector averaged NOx concentration (roads only) [ug/m3]
REAL,      INTENT(INOUT)                         :: cno2                       ! NO2 concentration (derived from NOx and parameterised ratio NO2/NOx) [ug/m3]
REAL,      INTENT(INOUT)                         :: percvk_sec(:)     ! frequency of occurrence of wind sector (roads only) [-]
INTEGER,   INTENT(INOUT)                         :: nsrc_sec(:)       ! number of sources present in wind sector (roads only) [-]
INTEGER,   INTENT(INOUT)                         :: nstab_present              ! number of contributing stability classes present up till now (percvk > 0, disxx > 0, q > 0)

! Check for road correction according to vdHout:
IF (lroad_corr) THEN 
   
   ! At least one road with vdHout correction is present:
   lroad_corr_present = .true. 
   
   ! Add NOx contribution for this wind sector to cnox_sec:
   ! we may use isec_prelim, because we are still close to the road
   cnox_sec(isec_prelim) = cnox_sec(isec_prelim) + c*percvk
   
   ! Add current frequency to frequency of wind sector and update number of sources in this wind sector;
   ! note that percvk_sec and nsrc_sec do not change during NOx-NO2 iteration:
   IF (iter .eq. 1) THEN 
      percvk_sec(isec_prelim) = percvk_sec(isec_prelim) + percvk
      
      ! Count source only for first contributing stability class present:
      nstab_present = nstab_present + 1
      if (nstab_present .eq. 1) nsrc_sec(isec_prelim) = nsrc_sec(isec_prelim) + 1
   ENDIF  
ELSE
   ! Not near road, add NO2 contribution and use ratio NO2/NOx based on background concentrations:
   cno2 = cno2 + r_no2_nox*c*percvk
ENDIF

END SUBROUTINE ops_vchem_add_nox_no2

end module m_ops_vchem
