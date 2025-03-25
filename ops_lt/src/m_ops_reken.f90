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
! DESCRIPTION          : compute contribution of current emission source to concentration and deposition 
!                        at current receptor ("reken"= compute).
!
!========================
! Structure of ops_reken
!========================
! ops_gen_precip: Generate precipitation field for this receptor
! Determine number of sub-receptor points (if needed)
! Loop over sub receptors (in x- and y-direction)
! | Compute number of sub area sources for the current sub receptor (if needed)
! ! Loop over sub-area sources
! ! | wind_rek               : Get emission data for current source and compute wind sector for source - receptor direction
! | | ops_building_get_factor: Compute building effect 
! | | ops_par_chem           : Compute chemical parameters (conversion rates, concentration ratios) in case of secondary components
! | | Loop over stability classes
! | | | ops_statparexp       : Get relevant parameters from meteo statistics
! | | | IF combination of stability, distance and wind direction class occurs 
! | | | | IF distance source - receptor > 0
! | | | | | ops_stab_rek       : Compute parameters which depend on stability class (friction velocity, Monin-Obukhov length, plume rise,
! | | | | |                      vertical dispersion coefficient). Adjust yearly averaged emission for the current {stability, distance} class,
! | | | | |                      compute plume rise and onder = fraction inside the mixing layer.
! | | | | | IF source strength > 0
! | | | | | | Loop over particle classes (gas -> only 1 class)
! | | | | | | | Compute source strength of primary component, for the current particle class
! | | | | | | | IF source strength of this particle class > 0
! | | | | | | | | ops_conc_ini  : Compute initial concentrations due to transport and dispersion; no removal processes yet
! | | | | | | | |                 -> c0_undepl_total, c0_undepl_mix, c_zrcp_undepl_mix (undepleted concentrations at z = 0 m and at receptor height)
! | | | | | | | | ops_resist_rek: Compute resistances for dry deposition, in-cloud scavenging ratio and chemical conversion rate
! | | | | | | | | ops_depoparexp: Compute parameters needed for dry deposition, wet deposition and chemical conversion 
! | | | | | | | |                 (source depletion factors, vertical gradient factor)
! | | | | | | | | ops_conc_rek  : Compute concentration, taking into account source depletion and vertical gradient
! | | | | | | | |                 -> c_zrcp (depleted concentrations at receptor height)
! | | | | | | | - end condition source strength of particle class > 0
! | | | | | | -  end loop over particle classes
! | | | | | -  end condition source strength > 0
! | | | | -  end condition source - receptor distance > 0
! | | | -  end condition percvk > 0 (fraction occurrence of meteo class)
! | | - end loop over stability classes
! | - end loops over sub-areas
! - end loops over sub-receptors
! error handling
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_reken

implicit none

contains

SUBROUTINE ops_reken(varin, do_proc, iter, niter, idep, isec, icm, gasv, vchemc, iopt_vchem, vchemv, dv, amol1, amol2, amol21, ar, & 
                  &  r_no2_nox_sec, r_no2_nox_season, ecvl, iseiz, zf, &
                  &  trafst, knatdeppar, mb, ugmoldep, dg, irev, scavcoef, koh, croutpri, rc_no, rhno2, rc_hno3, &
                  &  ircp, gxm, gym, xm, ym, zm, frac, nh3bg_rcp, gw_rcp, o3bg_rcp, so2bg_rcp, rhno3_rcp, &   ! o3bg_rcp here only used for debug write
                  &  bqrv, bqtr, bx, by, bdiam, bsterkte, bwarmte, bhoogte, &
                  &  bsigmaz, bD_stack, bV_stack, bTs_stack, bemis_horizontal, bbuilding, buildingEffect, & 
                  &  btgedr, bdegr, bcatnr, nemcat_road, road_chem, road_disp, emcat_road, &
                  &  z0_src, z0_tra, z0_rcp, z0_metreg_rcp, lu_tra_per, lu_rcp_per, &
                  &  so2sek, no2sek, so2bgtra, no2bgtra, nh3bgtra, gwtra, o3bgtra, &
                  &  mass_prec_tra, mass_conv_dtfac_tra, &
                  &  maxidx, pmd, uspmd, spgrid, grid, subbron, uurtot, routsec, &
                  &  rc_user, lroad_corr_present, somvnsec, telvnsec, vvchem, vtel, somvnpri, &
                  &  telvnpri, ddepri, wdepri, sdrypri, snatpri, sdrysec, snatsec, cpri, cpri_class, percvk_class, nsrc_class, &
                  &  class_output, csec, drydep, wetdep, astat, cnox_sec, cno2, &                   
                  &  percvk_sec, nsrc_sec, precip, routpri_in, dispg_in, & 
                  &  nparout, parout_val, parout_name, parout_unit, parout_write, error)

use m_ops_varin
use m_commonconst_lt
use m_commonfile
use m_error
use m_aps
use m_geoutils
use m_ops_building
use m_ops_utils, only: is_missing
use m_ops_vchem
use m_ops_resist_rek
use m_commonconst_lib, only: NLU
use m_ops_gen_precip
use m_ops_par_chem
use m_ops_statparexp
use m_ops_stab_rek
use m_ops_conc_ini
use m_ops_depoparexp
use m_ops_conc_rek
use m_ops_conc_sum
use m_ops_tdo_proc, only: Tdo_proc

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_reken')

! SUBROUTINE ARGUMENTS - INPUT
TYPE(Tvarin), INTENT(IN)                         :: varin                ! input variables for meteo
INTEGER,   INTENT(IN)                            :: iter                       ! iteration index for road correction
INTEGER,   INTENT(IN)                            :: niter                      ! number of iterations for road correction
TYPE(Tdo_proc), INTENT(IN)                       :: do_proc                    ! options to switch on/off specific processes
LOGICAL,   INTENT(IN)                            :: idep                       ! TRUE if deposition is modelled
LOGICAL,   INTENT(IN)                            :: isec                       ! TRUE if component is either SO2, NOx or NH3 (and a secondary component is present)
INTEGER,   INTENT(IN)                            :: icm                        ! component number used in OPS 
                                                                               ! 1 SO2 
                                                                               ! 2 NO2 
                                                                               ! 3 NH3 
                                                                               ! 24 PM
LOGICAL,   INTENT(IN)                            :: gasv                       ! TRUE if component is a gas
REAL,      INTENT(IN)                            :: vchemc                     ! chemical conversion rate, independent of light [%/h]
INTEGER,   INTENT(IN)                            :: iopt_vchem                 ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)                                                                                                                                           
REAL,      INTENT(IN)                            :: vchemv                     ! chemical conversion rate, dependent on light [%/h]
INTEGER,   INTENT(IN)                            :: dv                         ! maximum code diurnal emission variation dverl
REAL,      INTENT(IN)                            :: amol1                      ! molar mass primary component [g/mol]
REAL,      INTENT(IN)                            :: amol2                      ! molar mass secondary component [g/mol]
REAL,      INTENT(IN)                            :: amol21                     ! (molar mass secondary component)/(molar mass primary component) [-] 
REAL,      INTENT(IN)                            :: ar                         ! proportionality constant in relation [OH] = ar Qr, with Qr = global radiation in W/m2 [(cm3 m2)/(molec W2)], see Egmond en Kesseboom (1983)
REAL,      INTENT(IN)                            :: r_no2_nox_sec(:)           ! [NSEK] sector averaged NO2/NOx ratio according to vdHout parameterisation [-]
REAL,      INTENT(IN)                            :: r_no2_nox_season           ! component of NO2/NOx ratio which is season dependent  
REAL,      INTENT(IN)                            :: ecvl(:, :, :)              ! ecvl(NSTAB, NTRAJ, *) average diurnal emission variation for each stability/distance class
INTEGER,   INTENT(IN)                            :: iseiz                      ! season index (0=long term; 1=year; 2=winter; 3=summer; 4=month in winter; 5=month in summer)
REAL,      INTENT(IN)                            :: zf                         ! interpolation factor between summer and winter (zf << "zomer fractie" = summer fraction)
REAL,      INTENT(IN)                            :: trafst(:)                  ! trafst[NTRAJ] travel distances for each distance class [m]
INTEGER,   INTENT(IN)                            :: knatdeppar                 ! choice for parameterisation wet deposition
                                                                               ! knatdeppar = 1: read wdeppar = scavenging coefficient [%/h]
                                                                               ! knatdeppar = 2: read wdeppar =  scavenging ratio, i.e. average ratio if rainwater concentrations and air concentration [-]
                                                                               ! knatdeppar = 3: if secondary components are present [SO2, NO2, NH3]. Wash-out and rain-out parameters are fixed in the OPS code (ops_init) [-]
INTEGER,   INTENT(IN)                            :: mb                         ! start month of meteo statistics period ("b" << begin = start) 
REAL,      INTENT(IN)                            :: ugmoldep                   ! conversion factor from ug/m2/h to each of the deposition units in DEPUNITS 
REAL,      INTENT(IN)                            :: dg                         ! diffusion coefficient in air [cm^2/s] 
LOGICAL,   INTENT(IN)                            :: irev                       ! TRUE for reversible wash-out 
REAL,      INTENT(IN)                            :: scavcoef                   ! scavenging rate [%/h] 
REAL,      INTENT(IN)                            :: koh                        ! second order reaction rate constant of reaction NO2 + OH -> HNO3 [cm3/(molec s)]
REAL,      INTENT(IN)                            :: croutpri                   ! (wash-out + rain-out) ratio primary component, default value without correction for background concentration, season, stability class [-]
                                                                               ! icm = icm_SO2 (SO2): croutpri = wash out ratio at an N/S ratio of 1
                                                                               ! icm = icm_NOx (NOx): croutpri = wash out ratio at an NO2/NOx ratio of 1
                                                                               ! icm = icm_NH3 (NH3): croutpri = wash out ratio (no correction)
REAL,      INTENT(IN)                            :: rc_no                      ! canopy resistance Rc for NO [s/m]
REAL,      INTENT(IN)                            :: rhno2                      ! ratio [HNO2]/[NOx] 
REAL,      INTENT(IN)                            :: rc_hno3                    ! canopy resistance Rc for HNO3 [s/m] 
INTEGER,   INTENT(IN)                            :: ircp                       ! index of receptorpoint
REAL,      INTENT(IN)                            :: gxm                        ! x-coordinate of receptor (lon-lat) [degrees]
REAL,      INTENT(IN)                            :: gym                        ! y-coordinate of receptor (lon-lat) [degrees]
REAL,      INTENT(IN)                            :: xm                         ! x-coordinate of receptor (m RDM)
REAL,      INTENT(IN)                            :: ym                         ! y-coordinate of receptor (m RDM)
REAL,      INTENT(IN)                            :: zm                         ! z-coordinate of receptor points (m)
REAL,      INTENT(IN)                            :: frac                       ! fraction of grid cell inside NL 
REAL,      INTENT(IN)                            :: nh3bg_rcp                  ! NH3 background concentration at receptor (used in DEPAC) [ug/m3]  
REAL,      INTENT(IN)                            :: gw_rcp                  ! gamma water at receptor (used in DEPAC) 
REAL,      INTENT(IN)                            :: o3bg_rcp(:)                ! [NSEK] O3 background concentration at receptor for all wind sectors [ug/m3]  
REAL,      INTENT(IN)                            :: so2bg_rcp                  ! SO2 background concentration at receptor(used in DEPAC) [ug/m3]  																																	   
REAL,      INTENT(IN)                            :: rhno3_rcp                  ! ratio [HNO3]/[NO3]_total at receptor points, [NO3]_total = [HNO3] + [NO3_aerosol] 
REAL,      INTENT(IN)                            :: bqrv                       ! source strength of space heating source (rv << "ruimteverwarming" = space heating) [g/s]
REAL,      INTENT(IN)                            :: bqtr                       ! source strength of traffic source [g/s]
INTEGER,   INTENT(IN)                            :: bx                         ! x-coordinate of source 
INTEGER,   INTENT(IN)                            :: by                         ! y-coordinate of source 
REAL,      INTENT(IN)                            :: bdiam                      ! source diameter [m]; if bdiam < 0 -> circular source, bdiam > 0 -> square sourc 
REAL,      INTENT(IN)                            :: bsterkte                   ! source strength [g/s] 
REAL,      INTENT(IN)                            :: bwarmte                    ! heat content of source [MW] 
REAL,      INTENT(IN)                            :: bhoogte                    ! source height [m] 
REAL,      INTENT(IN)                            :: bsigmaz                    ! spread in source height to represent different sources in a area source; 
                                                                               ! also used for initial sigma_z (vertical dispersion) of emission (e.g. traffic, building influence) [m] 
REAL,      INTENT(IN)                            :: bD_stack                   ! diameter of the stack [m]
REAL,      INTENT(IN)                            :: bV_stack                   ! exit velocity of plume at stack tip [m/s]
REAL,      INTENT(IN)                            :: bTs_stack                  ! temperature of effluent from stack [K]            
LOGICAL,   INTENT(IN)                            :: bemis_horizontal           ! horizontal outflow of emission
type(Tbuilding), INTENT(IN)                      :: bbuilding                  ! structure with building parameters
type(TbuildingEffect), INTENT(IN)                :: buildingEffect             ! structure containing building effect tables
INTEGER,   INTENT(IN)                            :: btgedr                     ! temporal behaviour of sources (tgedr << "tijdsgedrag"== temporal behaviour)[-] 
INTEGER,   INTENT(IN)                            :: bdegr                      ! option for particle size distribution
                                                                               ! bdegr >= 0 -> standard particle size distribution pmd
                                                                               ! bdegr  < 0 -> user-defined particle size distribution uspmd 
INTEGER,   INTENT(IN)                            :: bcatnr                     ! emission category number
INTEGER,   INTENT(IN)                            :: nemcat_road                ! number of road emission categories (for vdHout NO2/NOx ratio)
INTEGER,   INTENT(IN)                            :: emcat_road(:)              ! list of road emission categories (for vdHout NO2/NOx ratio)
LOGICAL,   INTENT(IN)							 :: road_chem					 !switch for road chemistry GTHO
LOGICAL,   INTENT(IN)                            :: road_disp                  ! TRUE if user wants OPS to interpret sigz0 as SRM2 initial spread
REAL,      INTENT(IN)                            :: z0_src                     ! roughness length at source; from z0-map [m]
REAL,      INTENT(IN)                            :: z0_tra                     ! roughness length representative for trajectory [m]
REAL,      INTENT(IN)                            :: z0_rcp                     ! roughness length at receptor; from z0-map [m]
REAL,      INTENT(IN)                            :: z0_metreg_rcp              ! roughness length at receptor; interpolated from meteo regions [m]
REAL,      INTENT(IN)                            :: lu_tra_per(:)              ! [NLU] landuse (percentages) for all classes over trajectory
REAL,      INTENT(IN)                            :: lu_rcp_per(:)              ! [NLU] landuse (percentages) for all classes for receptor
REAL,      INTENT(IN)                            :: so2sek(:)                  ! [NSEK] coefficient in correction factor for SO2 background concentration for each wind direction sector; derived from 24 regional LML stations over 2003
REAL,      INTENT(IN)                            :: no2sek(:)                  ! [NSEK] coefficient in correction factor for NO2 background concentration for each wind direction sector; derived from 15 regional LML stations over 2004
REAL,      INTENT(IN)                            :: so2bgtra                   ! SO2 background concentration, trajectory averaged [ppb]
REAL,      INTENT(IN)                            :: no2bgtra                   ! NO2 background concentration, trajectory averaged [ppb]
REAL,      INTENT(IN)                            :: nh3bgtra                   ! NH3 background concentration, trajectory averaged [ppb]
REAL,      INTENT(IN)                            :: gwtra                      ! gamma water, trajectory averaged [ppb]
REAL,      INTENT(IN)                            :: o3bgtra(:)                 ! [NSEK] O3 background concentration, trajectory averaged [ppb]
real,      intent(in)                            :: mass_prec_tra, mass_conv_dtfac_tra
INTEGER,   INTENT(IN)                            :: maxidx                     ! max. number of particle classes (= 1 for gas)
REAL,      INTENT(IN)                            :: pmd(:,:)                   ! [NPARTCLASS,MAXDISTR] standard particle size distributions 
REAL,      INTENT(IN)                            :: uspmd(:,:)                 ! [NPARTCLASS,MAXDISTR] user-defined particle size distributions 
INTEGER,   INTENT(IN)                            :: spgrid                     ! indicator for type of receptor points 
                                                                               ! spgrid = 0: regular grid of receptors, NL
                                                                               ! spgrid = 1: rectangular regular grid of receptors, user defined 
                                                                               ! spgrid = 2: receptors at specific locations, read from file
                                                                               ! spgrid = 3: receptors at user specific regular grid, not necessarily rectangular, read from file
REAL,      INTENT(IN)                            :: grid                       ! grid resolution [m] 
LOGICAL,   INTENT(IN)                            :: subbron                    ! whether to create "subbrons" (sub-sources inside a area source) and "subareas" (sub receptors inside a grid cell) or not  
REAL,      INTENT(IN)                            :: uurtot                     ! total number of hours from meteo statistics
REAL,      INTENT(IN)                            :: routsec                    ! in-cloud (rain-out) scavenging ratio for secondary component
REAL,      INTENT(IN)                            :: rc_user                    ! canopy resistance specified by user in control file [s/m]
INTEGER,   INTENT(IN)                            :: nparout                    ! number of extra output parameters (besides concentration, deposition)
LOGICAL,   INTENT(IN)                            :: parout_write               ! write parout parameters to output
LOGICAL,   INTENT(IN)                            :: class_output               ! indicator whether results for receptors will be stored per wind sector/distance/particle/stability class

! SUBROUTINE ARGUMENTS - I/O       (INOUT)
LOGICAL,   INTENT(INOUT)                         :: lroad_corr_present         ! at least one road with vdHout correction is present

! DOUBLE PRECISION,      INTENT(INOUT)                         :: somvnsec(NPARTCLASS)       ! summed wet deposition flux secondary component [ug/m2/h] 
! DOUBLE PRECISION,      INTENT(INOUT)                         :: telvnsec(NPARTCLASS)       ! summed deposited mass per area for wet deposition of secondary component [ug/m2]
! DOUBLE PRECISION,      INTENT(INOUT)                         :: vvchem(NPARTCLASS)         ! summed chemical conversion rate [%/h] 
! DOUBLE PRECISION,      INTENT(INOUT)                         :: vtel(NPARTCLASS)           ! weighing factors for averaging vvchem (i.e. deposited mass)
! DOUBLE PRECISION,      INTENT(INOUT)                         :: somvnpri(NPARTCLASS)       ! summed wet deposition flux primary component [ug/m2/h] 
! DOUBLE PRECISION,      INTENT(INOUT)                         :: telvnpri(NPARTCLASS)       ! summed deposited mass per area for wet deposition of primary component [ug/m2]
! DOUBLE PRECISION,    INTENT(INOUT)               :: ddepri(NPARTCLASS)   ! dry deposition of primary component at receptor points [mol/ha/y] 
! DOUBLE PRECISION,    INTENT(INOUT)               :: wdepri(NPARTCLASS)   ! wet deposition of primary component at receptor points [mol/ha/y] 
! DOUBLE PRECISION,      INTENT(INOUT)                         :: sdrypri(NPARTCLASS)        ! summed dry deposition of primary component [ug/m2/h]
! DOUBLE PRECISION,      INTENT(INOUT)                         :: snatpri(NPARTCLASS)        ! summed wet deposition of primary component [ug/m2/h]  (<< "nat" = wet)
! DOUBLE PRECISION,      INTENT(INOUT)                         :: sdrysec(NPARTCLASS)        ! summed dry deposition of secondary component [ug/m2/h]
! DOUBLE PRECISION,      INTENT(INOUT)                         :: snatsec(NPARTCLASS)        ! summed wet deposition of secondary component [ug/m2/h]  (<< "nat" = wet)
! DOUBLE PRECISION,    INTENT(INOUT)               :: cpri(NPARTCLASS)     ! concentration of primary component at receptor points and height zm [ug/m3] 
! DOUBLE PRECISION,    INTENT(INOUT)               :: cpri_class(NPARTCLASS,NSTAB,NSEK,NTRAJ) ! concentration of primary component at receptor points and height zm, per class [ug/m3]
! DOUBLE PRECISION,    INTENT(INOUT)               :: percvk_class(NSTAB,NSEK,NTRAJ) ! percvk of primary component at receptor points and height zm, per class [factor of occurrence] 
! INTEGER, INTENT(INOUT)                           :: nsrc_class(NSTAB,NSEK,NTRAJ)   ! number of sources present in wind/distance sector (-classoutput only) [-]
! DOUBLE PRECISION,    INTENT(INOUT)               :: csec(NPARTCLASS)     ! concentration of secondary component ar receptor points [ug/m3] 
! DOUBLE PRECISION,    INTENT(INOUT)               :: drydep(NPARTCLASS)   ! dry deposition at receptor points [mol/ha/y] 
! DOUBLE PRECISION,    INTENT(INOUT)               :: wetdep(NPARTCLASS)   ! wet deposition at receptor points ["depeh"] 
! REAL,      INTENT(INOUT)                         :: astat(NTRAJ,NCOMP,NSTAB,NSEK) ! meteo statistics for each distance class, stability/mixing height class, wind direction sector
DOUBLE PRECISION,      INTENT(INOUT)                         :: somvnsec(:)                ! (NPARTCLASS)       ! summed wet deposition flux secondary component [ug/m2/h] 
DOUBLE PRECISION,      INTENT(INOUT)                         :: telvnsec(:)                ! (NPARTCLASS)       ! summed deposited mass per area for wet deposition of secondary component [ug/m2]
DOUBLE PRECISION,      INTENT(INOUT)                         :: vvchem(:)                  ! (NPARTCLASS)       ! summed chemical conversion rate [%/h] 
DOUBLE PRECISION,      INTENT(INOUT)                         :: vtel(:)                    ! (NPARTCLASS)       ! weighing factors for averaging vvchem (i.e. deposited mass)
DOUBLE PRECISION,      INTENT(INOUT)                         :: somvnpri(:)                ! (NPARTCLASS)       ! summed wet deposition flux primary component [ug/m2/h] 
DOUBLE PRECISION,      INTENT(INOUT)                         :: telvnpri(:)                ! (NPARTCLASS)       ! summed deposited mass per area for wet deposition of primary component [ug/m2]
DOUBLE PRECISION,    INTENT(INOUT)               :: ddepri(:)                  ! (NPARTCLASS) ! dry deposition of primary component at receptor points [mol/ha/y] 
DOUBLE PRECISION,    INTENT(INOUT)               :: wdepri(:)                  ! (NPARTCLASS) ! wet deposition of primary component at receptor points [mol/ha/y] 
DOUBLE PRECISION,      INTENT(INOUT)                         :: sdrypri(:)                 ! (NPARTCLASS)       ! summed dry deposition of primary component [ug/m2/h]
DOUBLE PRECISION,      INTENT(INOUT)                         :: snatpri(:)                 ! (NPARTCLASS)       ! summed wet deposition of primary component [ug/m2/h]  (<< "nat" = wet)
DOUBLE PRECISION,      INTENT(INOUT)                         :: sdrysec(:)                 ! (NPARTCLASS)       ! summed dry deposition of secondary component [ug/m2/h]
DOUBLE PRECISION,      INTENT(INOUT)                         :: snatsec(:)                 ! (NPARTCLASS)       ! summed wet deposition of secondary component [ug/m2/h]  (<< "nat" = wet)
DOUBLE PRECISION,    INTENT(INOUT)               :: cpri(:)                    ! (NPARTCLASS) ! concentration of primary component at receptor points and height zm [ug/m3] 
DOUBLE PRECISION,    INTENT(INOUT)               :: cpri_class(:,:,:,:)        ! (NPARTCLASS,NSTAB,NSEK,NTRAJ) ! concentration of primary component at receptor points and height zm, per class [ug/m3]
DOUBLE PRECISION,    INTENT(INOUT)               :: percvk_class(:,:,:)        ! (NSTAB,NSEK,NTRAJ) ! percvk of primary component at receptor points and height zm, per class [factor of occurrence] 
INTEGER, INTENT(INOUT)                           :: nsrc_class(:,:,:)          ! (NSTAB,NSEK,NTRAJ) ! number of sources present in wind/distance sector (-classoutput only) [-]
DOUBLE PRECISION,    INTENT(INOUT)               :: csec(:)                    ! (NPARTCLASS)       ! concentration of secondary component ar receptor points [ug/m3] 
DOUBLE PRECISION,    INTENT(INOUT)               :: drydep(:)                  ! (NPARTCLASS)       ! dry deposition at receptor points [mol/ha/y] 
DOUBLE PRECISION,    INTENT(INOUT)               :: wetdep(:)                  ! (NPARTCLASS)       ! wet deposition at receptor points ["depeh"] 
REAL,      INTENT(INOUT)                         :: astat(:,:,:,:)             ! (NTRAJ,NCOMP,NSTAB,NSEK) ! meteo statistics for each distance class, stability/mixing height class, wind direction sector
                                                                               !        1. number of hours for which a certain combination of classes has occurred [hours]
                                                                               !        2. maximal mixing height over transport distance [m]
                                                                               !        3. wind speed (at 10 m height) [m/s]
                                                                               !        4. boundary layer resistance Rb for SO2 [s/m]
                                                                               !        5. aerodynamic resistance 4 m + boundary layer resistance [s/m]
                                                                               !        6. aerodynamic resistance 50 m + boundary layer resistance [s/m]
                                                                               !        7. ratio effective dry deposition velocity over transport distance and 
                                                                               !           average dry deposition velocity over transport distance for low sources [-]
                                                                               !           See OPS-doc/meteo; bookmark correction_factor_deposition_velocity
                                                                               !        8. ratio effective dry deposition velocity over transport distance and 
                                                                               !           average dry deposition velocity over transport distance for high sources [-]
                                                                               !           See OPS-doc/meteo; bookmark correction_factor_deposition_velocity
                                                                               !        9. effective travel distance [km]
                                                                               !        10. degree day or domestic heating coefficient (= 19-T for T < 12 degree C)  [degree C] 
                                                                               !        11. precipitation probability [-]
                                                                               !        12. length of rainfall period [hours]
                                                                               !        13. rain intensity [mm/h]
                                                                               !        14. global radiation [J/cm2/h]
                                                                               !        15. wind speed power law coefficient [-]
                                                                               !        16. surface resistance Rc for SO2 [s/m] 
                                                                               !        17. percentage of total hours that certain stability/mixing height class occurs per 2 hour block, source oriented [%]
                                                                               !        18. percentage of total hours that certain stability/mixing height class occurs per 2 hour block, receptor oriented [%]
                                                                               !        19. friction velocity u* [m/s]
                                                                               !        20. temperature T [degree C]
                                                                               !        21. turning angle for wind shear (at reference height) [degree]
                                                                               !        22. Monin-Obukhov length L [m]
                                                                               !        23. sensible heat flux H0 [W/m2]
                                                                               !        24. relative humidity [%]
                                                                               !        25. surface resistance Rc of NO2 [s/m]
                                                                               !        26. surface resistance Rc of NH3 [s/m]
                                                                               !        27. surface resistance Rc of SO4 aerosol [s/m]
REAL,      INTENT(INOUT)                         :: cnox_sec(:)                ! (NSEK)   ! wind sector averaged NOx concentration (roads only) [ug/m3]
REAL,      INTENT(INOUT)                         :: cno2                       !          ! NO2 concentration (derived from NOx and parameterised ratio NO2/NOx) [ug/m3]
REAL,      INTENT(INOUT)                         :: percvk_sec(:)              ! (NSEK)   ! frequency of occurrence of wind sector (roads only) [-]
INTEGER,   INTENT(INOUT)                         :: nsrc_sec(:)                ! (NSEK)   ! number of sources present in wind sector (roads only) [-]
REAL,      INTENT(INOUT)                         :: parout_val(:)              ! (nparout)! values for extra output parameters, for current receptor
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record 
       
! SUBROUTINE ARGUMENTS - OUTPUT       (OUT)
REAL,      INTENT(OUT)                           :: precip                     ! total precipitation per year [mm/year]
REAL,      INTENT(IN)                            :: routpri_in                 ! in-cloud (rain-out) scavenging ratio for primary component [-]  
REAL,      INTENT(IN)                            :: dispg_in(NSTAB)            ! dispersion coefficients for vertical dispersion; sigma_z = dispg*x^disph [-]
                                                                               !   dispg remains unchanged if bdiam==0 and disxx==0
CHARACTER(len=*), INTENT(OUT)                    :: parout_name(nparout)       ! names of extra output parameters                      
CHARACTER(len=*), INTENT(OUT)                    :: parout_unit(nparout)       ! units of extra output parameters                      

! LOCAL VARIABLES
REAL                                             :: dispg(NSTAB)               ! coefficient for vertical dispersion coefficient sigma_z; sigma_z = dispg*x^disp [-] for various stability classes
INTEGER                                          :: istab                      ! teller over stabiliteitsklassen
INTEGER                                          :: kdeel                      ! teller over deeltjesklassen
INTEGER                                          :: itra                       ! distance class with travel distance ~nearest to the current source-receptor distance
INTEGER                                          :: idgr                       ! 
INTEGER                                          :: ibroncat                   ! emission category number
INTEGER                                          :: rond                       ! 
INTEGER                                          :: iwd                        ! wind direction if wind is from source to receptor (degrees) 
INTEGER                                          :: ibtg                       ! 
INTEGER                                          :: isec_prelim                ! index of preliminary source-receptor wind sector (wind shear not yet taken into account)
INTEGER                                          :: isec_in                    ! index of source-receptor wind sector (wind shear taken into account)
INTEGER                                          :: isec1                      ! index of first of two interpolating wind sectors
INTEGER                                          :: nk                         ! number of sub receptors (needed for grid averaged concentrations)
INTEGER                                          :: nr                         ! 
INTEGER                                          :: mrcp                       ! 
INTEGER                                          :: nrcp                       ! 
INTEGER                                          :: kk                         ! number of sub area sources
INTEGER                                          :: nb                         ! 
INTEGER                                          :: karea                      ! 
INTEGER                                          :: larea                      ! 
REAL                                             :: htot                       ! plume height at receptor, including plume descent due to heavy particles [m]
REAL                                             :: c0_undepl_total            ! undepleted concentration at z = 0 (including part of plume above mixing layer); is needed for secondary species [ug/m3]
REAL                                             :: c0_undepl_mix              ! undepleted concentration at z = 0 m (only due to part of plume inside the mixing layer) [ug/m3]
REAL                                             :: c_zrcp_undepl_mix          ! undepleted concentration at z = zrcp (only due to part of plume inside the mixing layer) [ug/m3]
REAL                                             :: c_zrcp                     ! concentration at z = zrcp [ug/m3]
REAL                                             :: ueff                       ! wind speed at effective transport height heff; 
                                                                               ! for short distances heff = plume height;
                                                                               ! for large distances heff = 1/2 mixing height;
                                                                               ! heff is interpolated for intermediate distances.
REAL                                             :: rations                    ! NH3/SO2 ratio over trajectory 
REAL                                             :: qbron                      ! total source strength qbron (g/s) (qobb + qrvv + qvk)
REAL                                             :: qtr                        ! source strength for traffic (total source strength for traffic / nr of sub area sources / nur sub receptors)
REAL                                             :: qruim                      ! space heating emission as function of the temperature before diurnal variation correction 
REAL                                             :: grad                       ! 
REAL                                             :: qob                        ! 
REAL                                             :: qww                        ! 
REAL                                             :: hbron                      ! emission height at source (stack height), without plume rise [m]
REAL                                             :: percvk                     ! fraction of occurrence of {distance/stability/wind-direction} class
REAL                                             :: grof                       ! = 1 -> coarse particles
REAL                                             :: cgt                        ! gradient factor at 4 m height [-] (In ops_deporarexp 1-grad, in ops_brondepl corrected for distance (1 - grad) (1 - exp[-t/tau]); grad = vd(zra)/vd(4))
REAL                                             :: cgt_z                      ! gradient factor at receptor height zm [-]
REAL                                             :: x                          ! 
REAL                                             :: y                          ! 
REAL                                             :: diam                       ! 
REAL                                             :: diameter                   ! 
REAL                                             :: sigz0                      ! 
REAL                                             :: D_stack                    ! diameter of the stack [m]
REAL                                             :: V_stack                    ! exit velocity of plume at stack tip [m/s]
REAL                                             :: Ts_stack                   ! temperature of effluent from stack [K]
LOGICAL                                          :: emis_horizontal            ! horizontal outflow of emission  
type(Tbuilding)                                  :: building                   ! structure with building paramaters
REAL                                             :: buildingFact               ! The interpolated building effect from the buildingTable
REAL                                             :: qrv                        ! source strength for space heating (total source strength for space heating / nr of sub area sources / nr sub receptors)
REAL                                             :: virty                      ! distance virtual point source - centre area source [m]
REAL                                             :: consec                     ! 
REAL                                             :: angle_SR_xaxis             ! angle between source-receptor vector and x-axis (needed for building effect) [degrees]
REAL                                             :: disx                       ! linear distance between source and receptor [m]
REAL                                             :: disxx                      ! effective travel distance between source and receptor [m]
REAL                                             :: radius                     ! radius of area source [m]
REAL                                             :: uster_metreg_rcp           ! friction velocity u* [m/s]
REAL                                             :: temp_C                     ! temperature at height zmet_T [C]
REAL                                             :: shear                      ! turning angle for wind shear (at reference height) [degrees]
REAL                                             :: ol_metreg_rcp              ! Monin-Obukhov length L [m]
REAL                                             :: h0                         ! sensible heat flux H0 [W/m2]
REAL                                             :: hum                        ! relative humidity [%]
REAL                                             :: rc_no2_ms                  ! Rc(NO2) from meteo statistics [s/m] (NOT USED)
REAL                                             :: rc_nh3_ms                  ! Rc(NH3) from meteo statistics [s/m] (NOT USED)
REAL                                             :: rc_aer_ms                  ! Rc(SO4-aerosol) from meteo statistics [s/m]
REAL                                             :: vw10                       ! wind speed at 10 m height [m/s]
REAL                                             :: pcoef                      ! coefficient in wind speed power law
REAL                                             :: htt                        ! plume height at source, including plume rise [m]
REAL                                             :: aant                       ! number of hours of occurrence of interpolation of distance classes/wind sectors for the current stability class
REAL                                             :: xl                         ! maximal mixing height over transport distance [m] (extrapolated when x > 1000km, largest distance category in meteo statistics; xl = 2.*htt when xl < htt)
REAL                                             :: rb_ms                      ! boundary layer resistance SO2 from meteo statistics [s/m]
REAL                                             :: rbm                        ! 
REAL                                             :: ra_ms_4                    ! aerodynamic resistance at 4 m from meteo statistics [s/m] 
REAL                                             :: ra4m                       ! 
REAL                                             :: ra_ms_zra                  ! aerodynamic resistance at height zra from meteo statistics [s/m] 
REAL                                             :: ra_zram                    ! 
REAL                                             :: xvglbr                     ! ratio effective dry deposition velocity over transport distance and average dry deposition velocity over transport distance for low sources [-]
REAL                                             :: xvghbr                     ! ratio effective dry deposition velocity over transport distance and average dry deposition velocity over transport distance for high sources [-]
REAL                                             :: xloc                       ! local mixing height (near source) [m]
REAL                                             :: xl100                      ! mixing height at 100 km [m] 
REAL                                             :: rad                        ! global radiation [J/cm2/h]
REAL                                             :: rc_so2_ms                  ! Rc(SO2) from meteo statistics [s/m]
REAL                                             :: coef_space_heating         ! space heating coefficient (degree-day values in combination with a wind speed correction) [C m^1/2 / s^1/2] 
REAL                                             :: regenk                     ! rain probability [-] 
REAL                                             :: buil                       ! length of rain event [0.01 hours]
REAL                                             :: rint                       ! rain intensity [mm/h] 
REAL                                             :: uster_rcp                  ! friction velocity at receptor; for z0 at receptor [m/s]
REAL                                             :: ol_rcp                     ! Monin-Obukhov length at receptor; for z0 at receptor [m/s] 
REAL                                             :: uster_src                  ! friction velocity u* at source [m/s]
REAL                                             :: ol_src                     ! Monin-Obukhov length at source [m]
REAL                                             :: uster_tra                  ! friction velocity u*, trajectory averaged [m/s]
REAL                                             :: ol_tra                     ! Monin-Obukhov length, trajectory averaged  [m]
REAL                                             :: uh                         ! windspeed (m/s) at receptor at height zu using z0, u* and L of source site (ops_stab_rek); wind speed used in parametrisation of vd for aerosols [m/s] (ops_resist_rek)
REAL                                             :: zu                         ! representative plume height (m) at receptor using z0, u* and L of source site, taking reflection into account
REAL                                             :: onder                      ! fraction of emission below mixing height [-]
REAL                                             :: xlm                        ! 
REAL                                             :: onderm                     ! 
REAL                                             :: qbpri                      ! source strength current source (for current particle class) [g/s]
REAL                                             :: qsec                       ! 
REAL                                             :: sigz                       ! vertical dispersion length [m]
REAL                                             :: rc_sec_trj                 ! canopy resistance secondary aerosol (SO4, NO3, NH4) for trajectory [s/m]
REAL                                             :: rc_sec_rcp                 ! canopy resistance secondary aerosol (SO4, NO3, NH4) at the receptor [s/m]
REAL                                             :: rb_rcp                     ! boundary layer resistance at receptor [s/m] 
REAL                                             :: ra_rcp_zra                 ! aerodynamic resistance at receptor, height zra [s/m];
                                                                               ! zra is height where concentration profile is undisturbed by deposition = 50 m
REAL                                             :: ra_rcp_zrcp                ! aerodynamic resistance at receptor, height zrcp [s/m] 
REAL                                             :: rc_eff_rcp_4_pos           ! effective canopy resistance at receptor, 4 m height, no re-emission (Rc is positive) [s/m] 
REAL                                             :: ra_rcp_4                   ! aerodynamic resistance at receptor, 4 m height [s/m] 
REAL                                             :: pr                         ! Distribution factor between washout (below cloud) and rainout (in cloud). pr = 0 -> washout, pr = 1 -> rainout. 
REAL                                             :: utr                        ! average wind speed over the trajectory (m/s)
REAL                                             :: vchem                      ! 
REAL                                             :: vd_eff_trj_zra             ! effective deposition velocity over trajectory, taking into account amount of time that plume is above mixing
                                                                               ! height and no deposition takes place [m/s] 
REAL                                             :: vd_coarse_part             ! deposition velocity coarse particles [m/s] 
REAL                                             :: rkc                        ! obsolete factor 
REAL                                             :: ri                         ! 
REAL                                             :: twt                        ! average duration of a rainfall period, dependent on source - receptor distance [h] 
REAL                                             :: vnatpri                    ! wet deposition loss rate for primary components [%/h]
REAL                                             :: cq2                        ! source depletion ratio for dry deposition for phase 2 (plume not yet mixed over mixing layer) 
REAL                                             :: cdn                        ! source depletion ratio for dry deposition for phase 3 (plume fully mixed over mixing layer)
REAL                                             :: cch                        ! source depletion factor for wet deposition/chemical conversion
REAL                                             :: cratio                     ! ratio surface concentration (= c0_undepl_mix) and concentration at full mixing in mixing layer (needed for reversible wet deposition).
REAL                                             :: rhno3_trj                  ! HNO3/NO3-total ratio for trajectory [-]     
REAL                                             :: r_no2_nox_year_bg_tra      ! component of NO2/NOx ratio which is based on yearly averaged background concentrations over a trajectory
REAL                                             :: vchemnh3                   ! chemical conversion rate for NH3 (old parameterisation)
REAL                                             :: dx                         ! 
REAL                                             :: dy                         ! 
REAL                                             :: dxsub                      ! 
REAL                                             :: dysub                      ! 
REAL                                             :: rc_eff_trj_4_pos           ! effective canopy resistance for trajectory, 4 m height, no re-emission (Rc is positive) [s/m]
REAL                                             :: rc_eff_rcp_4               ! effective canopy resistance at receptor, 4 m height, re-emission allowed [s/m]
REAL                                             :: rc_eff_src_4_pos           ! effective canopy resistance at source, 4 m height, no re-emission (Rc is positive) [s/m]
REAL                                             :: ra_src_4                   ! aerodynamic resistance at source, 4 m height [s/m]
REAL                                             :: rb_src                     ! boundary layer resistance at source [s/m] 
REAL                                             :: ra_trj_4                   ! aerodynamic resistance for trajectory, 4 m height [s/m]
REAL                                             :: ra_trj_zra                 ! aerodynamic resistance for trajectory, height zra [s/m];
                                                                               ! zra is height where concentration profile is undisturbed by deposition = 50 m  
REAL                                             :: rb_trj                     ! boundary layer resistance for trajectory [s/m]
REAL                                             :: r_no2_nox                  ! NO2/NOx ratio [-]

LOGICAL                                          :: inc_rcp                    ! increase receptorpoints
LOGICAL                                          :: depudone                   ! Ra, Rb have been computed (no need to repeat this for all particle size classes)
LOGICAL                                          :: lroad_corr                 ! road correction needed for NO2/NOx ratio
INTEGER                                          :: nstab_present              ! number of contributing stability classes present up till now (percvk > 0, disxx > 0, q > 0)
REAL                                             :: routpri                    ! in-cloud scavenging ratio for primary component [-] (rout << rain-out = in-cloud)

!-------------------------------------------------------------------------------------------------------------------------------
    ! Set local variables to input arguments:
    routpri = routpri_in
    dispg   = dispg_in

    ! Generate precipitation field for this receptor. 
    ! Can be used to derive concentration in rainwater from deposition flux,
    ! in order to compare this rainwater concentration with measurements.
    IF (precip.eq.0) CALL ops_gen_precip(varin%varin_meteo, varin%varin_unc, uurtot, astat, trafst, precip, error)
    !
    ! Simulate a receptor grid cell by a (large) number of receptor points.
    ! Determine nk = number of sub-receptor points.
    !
    ! Get source diameter and decide whether we have a circular source (rond = 1) or a square one (rond = 0)
    ! ("rond" = round)
    rond = 0
    diam = abs(bdiam)
    IF (bdiam < 0) rond = 1

    ! Simulate a receptor grid cell by a (large) number of receptor points.
    ! Determine nk = number of sub-receptor points.
    ! Decide whether we need sub-receptors; they are not needed in case of user-specified receptor locations (spgrid = 2), 
    ! but only for gridded receptors (in order to obtain an averaged grid-value).
    ! If the current source is an area source with the same diameter as the receptor grid resolution (diam = grid), 
    ! concentration gradients of the source are relatively small and no sub receptors are needed. Note that in this case,
    ! we also subdivide the area source into several sub-area sources.
    ! Also check the command line argument subbron.      
    inc_rcp = spgrid /= 2 .AND. diam /= grid 
    IF (inc_rcp .AND. subbron) THEN

       ! Compute x- and y-distance between receptor and source
       dx = xm - bx
       dy = ym - by 
       
       IF (abs(dx) <= grid*0.5 .AND. abs(dy) <= grid*0.5) THEN
          ! If source and receptor are close, we have strong gradients;
          ! low source -> high concentration gradients -> many sub receptors 
          ! small diameter -> point source -> high concentration gradients -> many sub receptors 
          nk = grid/(bhoogte*20 + diam)
       
       ELSE
          ! If source and receptor are far from each other, we have less strong gradients;
          ! small distance between source and receptor -> high concentration gradients -> many sub receptors 
          ! low source -> high concentration gradients -> many sub receptors 
          ! small diameter -> point source -> high concentration gradients -> many sub receptors 
          ! maximal 4 in 1/2 the grid cell -> 8*8 = 64
          ! Note: for sources having geographical coordinates (IGEO = 1), the distance for receptors in NL is so large that nk = 0
          nk = grid/(sqrt(dx**2+dy**2) + diam + bhoogte*10)*2
          nk = min0(nk,4)                                                        ! This (4) is an arbitrary limit
       ENDIF
      
       ! Compute nr = number of sub receptors within a grid cell;
       ! maximal (128*2+1)**2 = 66049 sub receptors.
       nk = min0(nk,128)                                                        ! This (128) is an arbitrary limit
       nr = (nk*2 + 1)**2
    ELSE

       ! (no grid .OR. diam = grid .OR. .NOT. subbron) -> no sub receptors
       nk = 0
       nr = 1
    ENDIF
    
    !++++++++++++++++++++++++++++++++++++++++++++++++++++   
    ! Loop over sub receptors (in x- and y-direction) 
    !++++++++++++++++++++++++++++++++++++++++++++++++++++   
    DO mrcp = -nk,nk
      DO nrcp = -nk,nk

         
        ! Compute kk = number of sub area sources for the current sub receptor
        ! if diam = 0, no sub area sources are needed
        ! if rond /= 0 (circular area source) -> no sub sources (sub sources are not supported when using circular sources)
        ! if -nosub is given at the command line -> no sub sources (test only)
        ! if number of sub receptors >= 1000 -> no sub sources; is needed to limit computational resources.
        IF (diam > 0 .AND. rond == 0 .AND. subbron .AND. nr .LT. 1000) THEN    ! This (1000) is an arbitrary limit

          ! Compute x- and y-distance between sub receptor and source
          dxsub = xm + mrcp*grid/(nk*2 + 1) - bx
          dysub = ym + nrcp*grid/(nk*2 + 1) - by

          IF (abs(dxsub) <= diam*0.5 .AND. abs(dysub) <= diam*0.5) THEN
            ! A. sub receptor inside area source
            ! low diameter -> few sub sources
            ! low source -> many sub sources
            ! Note that in many cases, where diam < 250*bhoogte -> kk = 6, nb = 169
            kk = int(diam/bhoogte/500) + 6

          ELSE
            ! B. sub receptor outside area source
            ! low diameter -> few sub sources
            ! relatively far -> few sub sources
            kk = int(diam/(sqrt(dxsub**2+dysub**2) + 0.1)*2)
          ENDIF
     
          ! Compute nb = number of sub area sources;
          ! maximal (32*2+1)**2 = 4225 sub receptors.
          kk = min0(kk,32)      ! This (32) is an arbitrary limit
          nb = (kk*2 + 1)**2
        ELSE
          ! No sub area sources are needed:
          kk = 0
          nb = 1
        ENDIF
        
        !++++++++++++++++++++++++++++++++++++++++++++++++++++   
        ! Loop over sub-area sources 
        !++++++++++++++++++++++++++++++++++++++++++++++++++++   

        ! Simulate an area source by a (large) number of small area sources. 
        ! Note: the next loop is carried out only once in case of a point source (kk=0)
        DO karea = -kk,kk
          DO larea = -kk,kk

            ! Get emission data for current source and compute wind sector for source - receptor direction
            
            ! Note: in ops_statparexp, iwd and isec_prelim are corrected for the turning of the wind at higher altitudes.
            CALL wind_rek(bx, by, bdiam, bsterkte, bwarmte, bhoogte, bsigmaz, bD_stack, bV_stack, bTs_stack, bemis_horizontal, & 
                        & bbuilding, btgedr, bdegr, bcatnr, bqrv, bqtr, gxm, gym, xm, ym, grid, nk, nr, mrcp, nrcp, kk, nb, & 
                        & karea, larea, angle_SR_xaxis, disx, x, y, qob, qww, hbron, sigz0, D_stack, V_stack, Ts_stack, & 
                        & emis_horizontal, building, ibtg, idgr, ibroncat, qrv, qtr, rond, diameter, iwd, isec_prelim)

            ! Compute building effect (depends on source-receptor distance; we can use linear distance, because the building effect is only present near the source):
            call ops_building_get_factor(building%type, angle_SR_xaxis, disx, buildingEffect%buildingFactAngleSRxaxis, buildingEffect%buildingFactDistances, building%buildingFactFunction, buildingFact)
            ! write(*,*) 'ops_reken/disx;buildingFact', angle_SR_xaxis, disx, buildingFact
            if (error%debug) write(*,'(3a,1(1x,i6,";"),1(1x,e12.5,";"),2(1x,i6,";"),99(1x,e12.5,";"))') trim(ROUTINENAAM),',A; ','ircp; disx; iwd; isec_prelim; angle_SR_xaxis; buildingFact; ', &
                                                                                                                                  ircp, disx, iwd, isec_prelim, angle_SR_xaxis, buildingFact

            ! Compute chemical parameters (conversion rates, concentration ratios) in case of secondary components, independent of meteo class:
            CALL ops_par_chem(icm, isec, iopt_vchem, isec_prelim, so2sek, no2sek, so2bgtra, no2bgtra, nh3bgtra, o3bgtra, &
                           &  mass_prec_tra, mass_conv_dtfac_tra, disx, diameter, varin%varin_unc, vchem, vchemnh3, rhno3_trj,  &
                           &  r_no2_nox_year_bg_tra, rations)
            if (error%debug) write(*,'(3a,1x,i6,";",99(1x,e12.5,";"))') trim(ROUTINENAAM),',B; ','ircp; disx; vchemnh3; rhno3_trj; r_no2_nox_year_bg_tra; rations; ', &
                                                                                                  ircp, disx, vchemnh3, rhno3_trj, r_no2_nox_year_bg_tra, rations    

            !++++++++++++++++++++++++++++++++++++++++++++++++++++   
            ! Loop over stability classes
            !++++++++++++++++++++++++++++++++++++++++++++++++++++   
            nstab_present = 0
            DO istab = 1, NSTAB  
        
              ! Compute source radius
              radius = diameter/2.
              ! Get relevant parameters from meteo statistics (dependent on wind direction and source-receptor distance), for the current
              ! stability class. Note that output values htt and htot are preliminary values, which are overwritten later on.
              CALL ops_statparexp(varin%varin_meteo, varin%varin_unc, istab, hbron, qww, D_stack, V_stack, Ts_stack, emis_horizontal, iwd, radius, uurtot, astat, &
                               &  trafst, disx, isec_prelim, disxx, isec1, vw10, &
                               &  h0, hum, ol_metreg_rcp, shear, rc_aer_ms, rc_nh3_ms, rc_no2_ms, temp_C, uster_metreg_rcp, pcoef,  &
                               &  htot, htt, itra, aant, xl, rb_ms, ra_ms_4, ra_ms_zra, xvglbr, xvghbr, xloc, xl100, rad, rc_so2_ms,  &
                               &  coef_space_heating, regenk, buil, rint, percvk, isec_in, error)
              !write(*,*) 'percvk1 = ',percvk

              if (error%debug) then
                 write(*,'(3a,2(1x,i6,";"),2(1x,e12.5,";"),3(1x,i6,";"),99(1x,e12.5,";"))') trim(ROUTINENAAM),',C1; ','ircp; istab; disx; disxx; isec_prelim; isec1; itra; percvk; ', &
                                                                                                                       ircp, istab, disx, disxx, isec_prelim, isec1, itra, percvk
                 write(*,'(3a,2(1x,i6,";"),99(1x,e12.5,";"))') trim(ROUTINENAAM),',C2; ','ircp; istab; disx; disxx; vw10; h0; hum; ol_metreg_rcp; shear; rc_aer_ms; rc_nh3_ms; rc_no2_ms; temp_C; uster_metreg_rcp; pcoef; ', &
                                                                                          ircp, istab, disx, disxx, vw10, h0, hum, ol_metreg_rcp, shear, rc_aer_ms, rc_nh3_ms, rc_no2_ms, temp_C, uster_metreg_rcp, pcoef
                 write(*,'(3a,2(1x,i6,";"),99(1x,e12.5,";"))') trim(ROUTINENAAM),',C3; ','ircp; istab; disx; disxx; htot; htt; aant; xl; rb_ms; ra_ms_4; ra_ms_zra; xvglbr; xvghbr; xloc; xl100; rad; rc_so2_ms; ', &
                                                                                          ircp, istab, disx, disxx, htot, htt, aant, xl, rb_ms, ra_ms_4, ra_ms_zra, xvglbr, xvghbr, xloc, xl100, rad, rc_so2_ms
                 write(*,'(3a,2(1x,i6,";"),99(1x,e12.5,";"))') trim(ROUTINENAAM),',C4; ','ircp; istab; disx; disxx; coef_space_heating; regenk; buil; rint; percvk; ', &
                                                                                          ircp, istab, disx, disxx, coef_space_heating, regenk, buil, rint, percvk
              endif

              IF (error%haserror) GOTO 9999

              ! Zero sensible heat flux H0 [W/m2] not allowed (to avoid division by zero)
              IF (ABS(h0) <= EPS_DELTA) THEN
                h0 = 1.
              ENDIF

              ! If combination of stability, distance and wind direction class occurs 
              IF (percvk > EPS_DELTA) THEN
                ! Skip if point source and receptor coincide
                IF (ABS(disxx) > EPS_DELTA .OR. ABS(diameter) > EPS_DELTA) THEN

                  ! Compute parameters which depend on stability class (friction velocity, Monin-Obukhov length, plume rise,
                  ! vertical dispersion coefficient). Adjust yearly averaged emission for the current {stability, distance} class,
                  ! compute plume rise and onder = fraction inside the mixing layer.
                  CALL ops_stab_rek(varin, icm, rb_ms, temp_C, h0, z0_metreg_rcp, disxx, disx, z0_rcp, xl, diameter/2., qtr, qrv, dv, &
                                 &  ecvl, coef_space_heating, ibtg,    &
                                 &  uster_metreg_rcp, hbron, qww, D_stack, V_stack, Ts_stack, emis_horizontal, ircp, istab, itra, qob, xloc, regenk, ra_ms_4, z0_tra, z0_src,  &
                                 &  ol_metreg_rcp, error, uster_rcp, ol_rcp, uster_src, ol_src, uster_tra, ol_tra,                                  &
                                 &  htot, htt, onder, uh, zu, qruim, qbron, dispg(istab))
                  if (error%debug) write(*,'(3a,2(1x,i6,";"),99(1x,e12.5,";"))') &
                     trim(ROUTINENAAM),',D; ',' ircp; istab; disx; disxx; uster_rcp; ol_rcp; uster_src; ol_src; uster_tra; ol_tra; htot; htt; onder; uh; zu; qruim; qbron; dispg; ', &
                                                ircp, istab, disx, disxx, uster_rcp, ol_rcp, uster_src, ol_src, uster_tra, ol_tra, htot, htt, onder, uh, zu, qruim, qbron, dispg(istab)
                  IF (error%haserror) GOTO 9999
                  
                  ! Continue if source strength > 0
                  IF (qbron > (0. + EPS_DELTA)) THEN
                    
                    ! Store parameters xl, onder, rb_ms, ra_ms_zra and ra_ms_4 for further use
                    xlm     = xl
                    onderm  = onder
                    rbm     = rb_ms                                                        ! 960215
                    ra_zram = ra_ms_zra                                                    ! 960215
                    ra4m    = ra_ms_4                                                      ! 960215

                    !++++++++++++++++++++++++++++++++++++++++++++++++++++   
                    ! Loop over particle classes
                    !++++++++++++++++++++++++++++++++++++++++++++++++++++   
                    ! For a gaseous component, there is only one such class.
                    depudone = .FALSE.
                    DO kdeel = 1, maxidx

                      ! Get stored parameters xl, onder, rb_ms, ra_ms_zra and ra_ms_4 and total source height htt
                      xl        = xlm
                      htot      = htt
                      onder     = onderm
                      ra_ms_zra = ra_zram
                      ra_ms_4   = ra4m
                      rb_ms     = rbm                                                      ! 960215

                      ! Compute source strength of primary component, for the current particle class;
                      ! multiply emissions of particles with the mass fraction in the current particle class;
                      ! idgr >= 0 -> standard particle size distribution pmd
                      ! idgr  < 0 -> user-defined particle size distribution uspmd
                      IF (.NOT.gasv) THEN
                        IF (idgr .GE. 0) THEN 
                          qbpri = qbron*pmd(kdeel, idgr)
                        ELSE
                          qbpri = qbron*uspmd(kdeel, ABS(idgr))
                        ENDIF
                        qsec  = 0.
                      ELSE
                        qbpri = qbron
                      ENDIF

                      ! Continue if source strength of this particle class > 0:
                      IF (ABS(qbpri) .GT. EPS_DELTA) THEN
						
						
						! We use road correction according to vd Hout, if the emission category is a road category and if the receptor is close to a road:
						if (road_disp .or. road_chem ) then
						   ! Special case emcat_road = [0] -> all categories use road correction
						   lroad_corr = (nemcat_road==1 .and. emcat_road(1) == 0) .or. &
										any(ibroncat==emcat_road(1:nemcat_road))
						   lroad_corr = lroad_corr .and. (disxx < 5000.0)
						else
						   lroad_corr = .false. 
						endif
						
						
						
                        ! Compute initial concentrations due to transport and dispersion; no removal processes yet
                        CALL ops_conc_ini(varin%varin_meteo, varin%varin_unc, gasv, vw10, htt, pcoef, disx, disxx, zm, kdeel, qbpri, z0_src, sigz0, road_disp, lroad_corr, rond, uster_src, ol_src,  &
                                       &  ircp, istab, iwd, qww, hbron, dispg(istab), radius, xl, onder,                                     &
                                       &  htot, grof, c0_undepl_total, c0_undepl_mix, c_zrcp_undepl_mix, sigz, ueff, virty, error)
                        if (error%debug) write(*,'(3a,2(1x,i6,";"),99(1x,e12.5,";"))') trim(ROUTINENAAM),',E; ','ircp; istab; disx; disxx; radius; xl; onder; htot; grof; c0_undepl_total; c0_undepl_mix; c_zrcp_undepl_mix; sigz; ueff; virty; ', &
                                                                                                                 ircp, istab, disx, disxx, radius, xl, onder, htot, grof, c0_undepl_total, c0_undepl_mix, c_zrcp_undepl_mix, sigz, ueff, virty

                        ! Compute deposition velocities for dry and wet deposition and the concentration decrease as a result
                        ! of deposition and (chemical) conversion. Only if idep = TRUE.
                        IF (idep) THEN
                          ! Compute resistances for dry deposition, in-cloud scavenging ratio, chemical conversion rate and NO2/NOx ratio:
                          CALL ops_resist_rek(varin%varin_meteo, varin%varin_unc, disxx,vchemc, iopt_vchem, vchemv, rad, isec, icm, rc_so2_ms, regenk, rc_aer_ms, iseiz, istab, itra, isec_prelim, ar, &
                                           &  r_no2_nox_season, ibroncat, nemcat_road, road_chem, emcat_road, vchemnh3, hum, uster_rcp, ol_rcp, uster_tra, &
                                           &  z0_rcp, z0_metreg_rcp, kdeel, mb, vw10, temp_C, zm, koh, &
                                           &  rations, rhno3_trj, rc_no, rhno2, rc_hno3, croutpri, r_no2_nox_sec, r_no2_nox_year_bg_tra, rhno3_rcp, &
                                           &  rb_ms, ra_ms_4, ra_ms_zra, rc_user, routpri, vchem, rc_sec_trj, uh, rc_sec_rcp, rc_eff_rcp_4_pos, rb_rcp, &
                                           &  ra_rcp_4, ra_rcp_zra, ra_rcp_zrcp, z0_src, ol_src, uster_src, z0_tra, rc_eff_trj_4_pos, rc_eff_src_4_pos, &
                                           &  ra_src_4, rb_src, ra_trj_4, ra_trj_zra, rb_trj, rc_eff_rcp_4, nh3bg_rcp, nh3bgtra, &
                                           &  so2bg_rcp, so2bgtra, gw_rcp, gwtra, gym, depudone, gasv, lu_rcp_per, lu_tra_per, r_no2_nox, lroad_corr)
                          if (error%debug) write(*,'(2a,2(1x,i6,";"),2(1x,e12.5,";"),1(1x,i6,";"),99(1x,e12.5,";"))') 'ops_reken,F1; ircp; istab; disx; disxx; isec_prelim; o3bg_rcp(isec_prelim); ', &
                                                                                                                      'o3bg_rcp(1); o3bg_rcp(2); o3bg_rcp(3); o3bg_rcp(4); o3bg_rcp(5); o3bg_rcp(6); o3bg_rcp(7); o3bg_rcp(8); o3bg_rcp(9); o3bg_rcp(10); o3bg_rcp(11); o3bg_rcp(12);', &
                                                                                                                                     ircp, istab, disx, disxx, isec_prelim, o3bg_rcp(isec_prelim), o3bg_rcp(:)
  
                          if (error%debug) write(*,'(4a,2(1x,i6,";"),15(1x,e12.5,";"),99(1x,e12.5,";"))') trim(ROUTINENAAM),',F2; ',&
                                                                                                                       'ircp; istab; disx; disxx; ra_src_4; rb_src; ra_trj_4; ra_trj_zra; rb_trj; ra_rcp_zrcp; ra_rcp_4; ra_rcp_zra; rb_rcp; routpri; vchem; uh; r_no2_nox;  ', &
                                                                                                                       'rc_eff_src_4_pos; rc_eff_trj_4_pos; rc_eff_rcp_4_pos; rc_eff_rcp_4; rc_sec_trj; rc_sec_rcp; ', &
                                                                                                                        ircp, istab, disx, disxx, ra_src_4, rb_src, ra_trj_4, ra_trj_zra, rb_trj, ra_rcp_zrcp, ra_rcp_4, ra_rcp_zra, rb_rcp, routpri, vchem, uh, r_no2_nox, &
                                                                                                                        rc_eff_src_4_pos, rc_eff_trj_4_pos, rc_eff_rcp_4_pos, rc_eff_rcp_4, rc_sec_trj, rc_sec_rcp

                          ! Compute parameters needed for dry deposition, wet deposition and chemical conversion;
                          ! not only deposition velocities, but also source depletion factors and vertical gradient factor:
                          cratio = 1.
                          CALL ops_depoparexp(varin, do_proc, kdeel, c0_undepl_mix, qbpri, ra_rcp_4, ra_rcp_zra, ra_rcp_zrcp, rb_rcp, sigz, ueff, &
                                           &  virty, gasv, ircp, istab, grof, xvghbr, xvglbr, &
                                           &  regenk, rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, xl, onder, dg, &
                                           &  knatdeppar, scavcoef, irev, htt, xloc, xl100, vw10, pcoef, vchem, dispg(istab), htot, &
                                           &  error, pr, twt, cratio, rc_eff_rcp_4_pos, grad, utr, routpri, &
                                           &  vd_eff_trj_zra, rkc, ri, vnatpri, cgt, cgt_z, cq2, cdn, cch, z0_src, ol_src, uster_src, & 
                                           &  rc_eff_trj_4_pos, rc_eff_src_4_pos, ra_src_4, rb_src, ra_trj_zra, rb_trj, vd_coarse_part, &
                                           &  xm, ym, zm, bx, by)
                          if (error%debug) then
                             write(*,'(3a,2(1x,i6,";"),99(1x,e12.5,";"))') trim(ROUTINENAAM),',G1; ','ircp; istab; disx; disxx; pr; twt; cratio; ri; vnatpri; ', &
                                                                                                      ircp, istab, disx, disxx, pr, twt, cratio, ri, vnatpri
                             write(*,'(3a,2(1x,i6,";"),99(1x,e12.5,";"))') trim(ROUTINENAAM),',G2; ','ircp; istab; disx; disxx; rc_eff_rcp_4_pos; rc_eff_trj_4_pos; rc_eff_src_4_pos; ra_src_4; rb_src; ra_trj_zra; rb_trj; ', &
                                                                                                      ircp, istab, disx, disxx, rc_eff_rcp_4_pos, rc_eff_trj_4_pos, rc_eff_src_4_pos, ra_src_4, rb_src, ra_trj_zra, rb_trj                             
                             write(*,'(3a,2(1x,i6,";"),99(1x,e12.5,";"))') trim(ROUTINENAAM),',G3; ','ircp; istab; disx; disxx; grad; utr; routpri; vd_eff_trj_zra; rkc; cgt; cgt_z; cq2; cdn; cch; z0_src; ol_src; uster_src; vd_coarse_part; ', &
                                                                                                      ircp, istab, disx, disxx, grad, utr, routpri, vd_eff_trj_zra, rkc, cgt, cgt_z, cq2, cdn, cch, z0_src, ol_src, uster_src, vd_coarse_part
                          endif
                                           
                          ! Compute concentration, taking into account source depletion factors for dry deposition,
                          ! wet deposition and chemical conversion and the vertical gradient:
                          CALL ops_conc_rek(varin%varin_unc, do_proc, ueff, qbpri, isec, rc_sec_trj, routsec, c0_undepl_total, c0_undepl_mix, c_zrcp_undepl_mix,&
                                         &  amol1, amol2, sigz, utr, rc_sec_rcp, &
                                         &  ra_rcp_4, ra_rcp_zra, rb_rcp, amol21, ugmoldep, cch, cgt, cgt_z, grof, percvk, onder, &
                                         &  regenk, virty, ri, vw10, hbron, pcoef, rkc, disxx, vnatpri, vchem, radius, xl, xloc, &
                                         &  htot, twt, xvghbr, xvglbr, grad, frac, cdn, cq2, c_zrcp, sdrypri(kdeel), &
                                         &  sdrysec(kdeel), snatsec(kdeel), somvnsec(kdeel), telvnsec(kdeel), vvchem(kdeel), &
                                         &  vtel(kdeel), snatpri(kdeel), somvnpri(kdeel), telvnpri(kdeel), ddepri(kdeel), &
                                         &  wdepri(kdeel), drydep(kdeel),  wetdep(kdeel), qsec, consec, pr, & 
                                         &  vd_eff_trj_zra, ra_trj_zra, rb_trj, rc_eff_rcp_4, vd_coarse_part, buildingFact, &
                                         &  nparout, parout_val, parout_name, parout_unit, parout_write)
                        ELSE
                           ! Building effect for idep = 0 (no depletion):
                           c_zrcp = c_zrcp_undepl_mix*buildingFact
                        ENDIF ! end condition idep (compute deposition)

                        ! update concentration arrays
                        CALL ops_conc_sum(c_zrcp, consec, percvk, iter, niter, class_output, idep, isec, &
                                          cpri(kdeel), csec(kdeel), cpri_class(kdeel,istab,isec_in,itra), &
                                          percvk_class(istab,isec_in,itra), nsrc_class(istab,isec_in,itra))

                        ! Add NOx contribution to cnox_sec or NO2 contribution to cno2:
                        IF (do_proc%chem .and. icm == icm_NOx) THEN 
                           call ops_vchem_add_nox_no2(lroad_corr, iter, isec_prelim, c_zrcp, r_no2_nox, percvk, &
                                   lroad_corr_present, cnox_sec, cno2, percvk_sec, nsrc_sec, nstab_present)
                           if (error%debug) &
                              write(*,'(3a,4(1x,i6,";"),99(1x,e12.5,";"))') trim(ROUTINENAAM),',H; ',' ircp; istab; iwd; isec_prelim; disx; disxx; percvk; r_no2_nox; r_no2_nox_sec(isec_prelim); cnox_sec(isec_prelim); ', & 
                                                                                                       ircp, istab, iwd, isec_prelim, disx, disxx, percvk, r_no2_nox, r_no2_nox_sec(isec_prelim), cnox_sec(isec_prelim)
                        ENDIF
                                 
                        ! Check for negative concentrations:
                        IF (c_zrcp < 0. - EPS_DELTA) GOTO 1000
                      ENDIF                                          ! end condition source strength of particle class > 0
                    ENDDO                                            ! end loop over particle classes
                  ENDIF                                              ! end condition source strength > 0
                ENDIF                                                ! end condition source - receptor distance > 0
              ENDIF                                                  ! end condition percvk > 0 (fraction occurrence of meteo class)              
            ENDDO                                                    ! end loop over stability classes
            
            if (error%debug .and. lroad_corr_present) & 
            write(*,'(3a,4(1x,i6,";"),99(1x,e12.5,";"))') trim(ROUTINENAAM),',I; ',' ircp; istab; iwd; isec_prelim; disx; disxx; percvk; cnox_sec(isec_prelim); ', & 
                                                                                     ircp, istab, iwd, isec_prelim, disx, disxx, percvk, cnox_sec(isec_prelim)
          ENDDO                                                      ! end loop over sub-areas (y-direction)
        ENDDO                                                        ! end loop over sub-areas (x-direction)
      ENDDO                                                          ! end loop over sub receptors (y-direction)
    ENDDO                                                            ! end loop over sub receptors (x-direction)

    ! Computation for this source completed, deallocate buildingFactFunction for this source:
    if (building%type > 0) deallocate(building%buildingFactFunction)
RETURN

! Negative concentration. Create error message and close the progress file:
1000 CALL SetError('Negative concentration encountered', error)
CALL ErrorParam('concentration', c_zrcp, error)
CALL ErrorParam('cch', cch, error)
CALL ErrorParam('cgt', cgt, error)
CALL ErrorParam('cgt_z', cgt_z, error)
CALL ErrorParam('z0_metreg_rcp', z0_metreg_rcp, error)
CALL ErrorParam('z0_rcp', z0_rcp, error)
CALL ErrorParam('uster_rcp', uster_rcp, error)
CALL ErrorParam('ol_rcp', ol_rcp, error)
CALL ErrorParam('istab', istab, error)
CALL ErrorParam('onder', onder, error)
CALL ErrorParam('zu', zu, error)
CALL ErrorParam('uh', uh, error)
CALL ErrorParam('ueff', ueff, error)
CALL ErrorParam('sigz', sigz, error)
CALL ErrorParam('ueff', ueff, error)
CALL ErrorParam('xl', xl, error)
CALL ErrorParam('sigz', sigz, error)
CALL ErrorParam('cq2', cq2, error)
CALL ErrorParam('cdn', cdn, error)
CALL ErrorParam('grad', grad, error)
CALL ErrorParam('ol_metreg_rcp', ol_metreg_rcp, error)
CALL ErrorParam('vd_eff_trj_zra', vd_eff_trj_zra, error)
CALL ErrorParam('c0_undepl_total', c0_undepl_total, error)
CALL ErrorParam('c0_undepl_mix', c0_undepl_mix, error)

9999 CLOSE (fu_progress)
CALL ErrorCall(ROUTINENAAM, error)

RETURN

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : wind_rek, contained in ops_reken
! DESCRIPTION        : Compute preliminary wind sector, not including wind shear.
!                      Also get all source data for the current source
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE wind_rek(bx, by, bdiam, bsterkte, bwarmte, bhoogte, bsigmaz, bD_stack, bV_stack, bTs_stack, bemis_horizontal, &
                  & bbuilding, btgedr, bdegr, bcatnr, bqrv, bqtr, gxm, gym, xm, ym, grid, nk, nr, mrcp, nrcp, kk, nb, & 
                  & karea, larea, angle_SR_xaxis, disx, x, y, qob, qww, hbron, sigz0, D_stack, V_stack, Ts_stack, & 
                  & emis_horizontal, building, ibtg, idgr, ibroncat, qrv, qtr, rond, diameter, iwd, isec_prelim)

use Binas, only: deg2rad, rad2deg
                 
! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'wind_rek')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: bx                         ! 
INTEGER,   INTENT(IN)                            :: by                         ! 
REAL,      INTENT(IN)                            :: bdiam                      ! 
REAL,      INTENT(IN)                            :: bsterkte                   ! 
REAL,      INTENT(IN)                            :: bwarmte                    ! 
REAL,      INTENT(IN)                            :: bhoogte                    ! 
REAL,      INTENT(IN)                            :: bsigmaz                    ! 
REAL,      INTENT(IN)                            :: bD_stack                   ! diameter of the stack [m]
REAL,      INTENT(IN)                            :: bV_stack                   ! exit velocity of plume at stack tip [m/s]
REAL,      INTENT(IN)                            :: bTs_stack                  ! temperature of effluent from stack [K]            
LOGICAL,   INTENT(IN)                            :: bemis_horizontal           ! horizontal outflow of emission
type(Tbuilding), INTENT(IN)                      :: bbuilding                  ! structure with building parameters
INTEGER,   INTENT(IN)                            :: btgedr                     ! 
INTEGER,   INTENT(IN)                            :: bdegr                      ! 
INTEGER,   INTENT(IN)                            :: bcatnr                     ! emission category number
REAL,      INTENT(IN)                            :: bqrv                       ! source strength of space heating source (rv << "ruimteverwarming" = space heating) [g/s]
REAL,      INTENT(IN)                            :: bqtr                       ! 
REAL,      INTENT(IN)                            :: gxm                        ! 
REAL,      INTENT(IN)                            :: gym                        ! 
REAL,      INTENT(IN)                            :: xm                         ! 
REAL,      INTENT(IN)                            :: ym                         ! 
REAL,      INTENT(IN)                            :: grid                       ! 
INTEGER,   INTENT(IN)                            :: nk                         ! 
INTEGER,   INTENT(IN)                            :: nr                         ! number of sub receptors within a grid cell
INTEGER,   INTENT(IN)                            :: mrcp                       ! 
INTEGER,   INTENT(IN)                            :: nrcp                       ! 
INTEGER,   INTENT(IN)                            :: kk                         ! 
INTEGER,   INTENT(IN)                            :: nb                         ! number of sub area sources
INTEGER,   INTENT(IN)                            :: karea                      ! 
INTEGER,   INTENT(IN)                            :: larea                      ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: angle_SR_xaxis             ! angle between source-receptor vector and x-axis (needed for building effect) [degrees]
REAL,      INTENT(OUT)                           :: disx                       ! linear distance between source and receptor [m] 
REAL,      INTENT(OUT)                           :: x                          ! 
REAL,      INTENT(OUT)                           :: y                          ! 
REAL,      INTENT(OUT)                           :: qob                        ! 
REAL,      INTENT(OUT)                           :: qww                        ! 
REAL,      INTENT(OUT)                           :: hbron                      ! emission height at source (stack height), without plume rise [m]
REAL,      INTENT(OUT)                           :: sigz0                      ! 
REAL,      INTENT(OUT)                           :: D_stack                    ! diameter of the stack [m]
REAL,      INTENT(OUT)                           :: V_stack                    ! exit velocity of plume at stack tip [m/s]
REAL,      INTENT(OUT)                           :: Ts_stack                   ! temperature of effluent from stack [K]            
LOGICAL,   INTENT(OUT)                           :: emis_horizontal            ! horizontal outflow of emission
type(Tbuilding), INTENT(OUT)                     :: building                   ! strucure with building parameters
INTEGER,   INTENT(OUT)                           :: ibtg                       ! 
INTEGER,   INTENT(OUT)                           :: idgr                       ! 
INTEGER,   INTENT(OUT)                           :: ibroncat                   ! emission category number
REAL,      INTENT(OUT)                           :: qrv                        ! source strength for space heating (total source strength for space heating / nr of sub area sources / nr sub receptors)
REAL,      INTENT(OUT)                           :: qtr                        ! source strength for traffic (total source strength for traffic / nr of sub area sources / nur sub receptors)
INTEGER,   INTENT(OUT)                           :: rond                       ! 
REAL,      INTENT(OUT)                           :: diameter                   ! 
INTEGER,   INTENT(OUT)                           :: iwd                        ! wind direction if wind is from source to receptor (degrees)
INTEGER,   INTENT(OUT)                           :: isec_prelim                ! index of preliminary source-receptor wind sector (wind shear not yet taken into account)

! LOCAL VARIABLES
REAL                                             :: dx                         ! 
REAL                                             :: dy                         ! 

!-------------------------------------------------------------------------------------------------------------------------------
!
! Get source parameters from source buffer arrays
!
! Location of centre of sub area source:
x        = bx + karea*diam/(kk*2 + 1)
y        = by + larea*diam/(kk*2 + 1)

! source strength (divide by the number of sub area sources and the number of sub receptors):
qob      = (bsterkte/nb)/nr

! Other source parameters:
qww      = bwarmte                    ! heat content[MW]
hbron    = bhoogte                    ! emission height [m]
sigz0    = bsigmaz                    ! spread in emission height [m]
D_stack  = bD_stack                   ! diameter of the stack [m]
V_stack  = bV_stack                   ! exit velocity of plume at stack tip [m/s]
Ts_stack = bTs_stack                  ! temperature of effluent from stack [K]            
emis_horizontal = bemis_horizontal    ! horizontal outflow of emission
building = bbuilding                  ! building parameters 
ibtg     = btgedr                     ! diurnal variation code
idgr     = bdegr                      ! particle size distribution code
ibroncat = bcatnr                     ! emission category number

! Source strengths for space heating and traffic:
qrv      = (bqrv/nb)/nr
qtr      = (bqtr/nb)/nr

! Determine whether this a circular (rond = 1) or square source (rond = 0);
! In the latter case, divide the diameter by the number of sub area sources in x- (or y-) direction.
! Note that for cicular sources, sub area sources are not supported.
IF (bdiam .LT. (0. - EPS_DELTA)) THEN
  rond     = 1
  diameter = ABS(bdiam)
ELSE
  rond     = 0
  diameter = bdiam/(kk*2 + 1)
ENDIF
!
! Determine source - (sub) receptor distance (in meters), given source = (x1,y1) and receptor = (x2,y2) in degrees.
! deg2rad = pi/180. R = earth radius (m).
! distance in y-direction = R*(y2 - y1)*deg2rad, y latitude
! distance in x-direction = R*cos(y*deg2rad)*(x2 - x1)*deg2rad, x longitude
! distance between 1 and 2: R*sqrt([cos(y*deg2rad)*(x2-x1)*deg2rad]^2 + [(y2-y1)*deg2rad]^2) = R*deg2rad*([cos(y*deg2rad)*(x2-x1)]^2 + (y2-y1)^2)
! Note: R1 = equatorial radius: 6378.137 km, R2 = distance centre - pole: 6356.752 km   
!       R1*deg2rad = 111319.5 m, R2*deg2rad = 110946.3 m (average = 111132.9 m). Here rounded to 111000 m.
! 
IF (IGEO .EQ. 1) THEN
  ! Geographical coordinates (degrees)
  dy    = gym - y
  dx    = (gxm - x)*COS((y + dy/2.)*deg2rad)
  disx  = 111000.*SQRT(dx*dx + dy*dy)
ELSE
  ! RDM coordinates [m]
  dx    = xm + mrcp*grid/(nk*2 + 1) - x
  dy    = ym + nrcp*grid/(nk*2 + 1) - y
  disx  = SQRT((dx*dx) + (dy*dy))
ENDIF
                                   
!   North     receptor                          
!    |       /                        
!    |      /                         
!    |     /                          
!  dy|    /                           
!    |   /                            
!    |  /                             
!    | /alpha                               
!    |--------
!   source    dx
!
! Determine iwd = preliminary wind direction if wind is from source to receptor (degrees); 
! some correction will be applied later for a height dependent wind shear, 
! but plume rise is not known at this point. Outcome: 0 <= iwd <= 360.
! Note that in the OPS model, the wind direction is characterised by its angle with the North; 
! Angle with North = pi/2 - alpha = pi/2 - atan2(dy,dx) = atan2(dx,dy).
! The addition of 180 degrees is because we need the wind direction coming from the source.
!
IF (disx .GT. 2.) THEN
  iwd = NINT(ATAN2(dx, dy)*rad2deg + 180)
  IF (iwd.EQ.360) iwd = 0
ELSE
!
! winddirection for zero source-receptor distance is undetermined, an average value of 60 degr. is given (based on sensitivity tests).
! This is especially important in the centre of area sources.
!
  iwd = 60
ENDIF
!
! Determine wind direction index isec_prelim for the source-receptor wind sector, knowing 0 <= iwd < 360.
!
isec_prelim = (iwd + 180/NSEK)*NSEK/360 + 1
IF (isec_prelim .GT. NSEK) THEN
  isec_prelim = 1
ENDIF

! Determine angle between source-receptor vector and x-axis, betweeen 0 and 360 degrees (needed for building effect):
angle_SR_xaxis = modulo(atan2(dy,dx)*rad2deg, 360.0)  ! degrees

RETURN
END SUBROUTINE wind_rek

END

end module m_ops_reken
