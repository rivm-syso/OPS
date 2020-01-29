!-------------------------------------------------------------------------------------------------------------------------------
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
!                       Copyright (C) 2002 by
!   National Institute of Public Health and Environment
!           Laboratory for Air Research (RIVM/LLO)
!                      The Netherlands
!
! SUBROUTINE
! NAME                 : %M%
! SCCS(SOURCE)         : %P%
! RELEASE - LEVEL      : %R% - %L%
! BRANCH -SEQUENCE     : %B% - %S%
! DATE - TIME          : %E% - %U%
! WHAT                 : %W%:%E%
! AUTHOR               :
! FIRM/INSTITUTE       : RIVM/LLO
! LANGUAGE             : FORTRAN-77/90
! USAGE                :
! DESCRIPTION          : Compute concentrations and depositions ("reken"= compute).
! EXIT CODES           :
! FILES AND OTHER      :
!      I/O DEVICES
! SYSTEM DEPENDENCIES  : HP-Fortran
! CALLED FUNCTIONS     :
! UPDATE HISTORY       :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_reken(idep, isec, icm, gasv, intpol, vchemc, vchemv, dv, amol1, amol2, amol21, ar, rno2nox, ecvl, iseiz, zf,    &
                  &  trafst, knatdeppar, mb, ugmoldep, dg, irev, scavcoef, koh, croutpri, rcno, rhno2, rchno3,                 &
                  &  nrrcp, ircp, gxm, gym, xm, ym, zm, frac, nh3bg_rcp, rhno3_rcp,                                            & 
                  &  bqrv, bqtr, bx, by, bdiam, bsterkte, bwarmte, bhoogte,                                                    &
                  &  bsigmaz, bD_stack, bV_stack, bTs_stack, bemis_horizontal, bbuilding, buildingEffect,                      & 
                  &  btgedr, bdegr,                                                                                            &
                  &  z0_src, z0_tra, z0_rcp, z0_metreg_rcp, lu_tra_per, lu_rcp_per,                                            &
                  &  so2sek, no2sek, so2bgtra, no2bgtra, nh3bgtra, maxidx, pmd, uspmd, spgrid, grid, subbron, uurtot, routsec, &
                  &  rc, somvnsec, telvnsec, vvchem, vtel, somvnpri,                                                           &
                  &  telvnpri, ddepri, sdrypri, snatpri, sdrysec, snatsec, cpri, csec, drydep, wetdep, astat, rno2_nox_sum,    &
                  &  precip, routpri, dispg, error) 


!DEC$ ATTRIBUTES DLLEXPORT:: ops_reken

USE m_commonconst
USE m_commonfile
USE m_error
USE m_aps
USE m_geoutils
USE m_ops_building
 use m_ops_utils, only: is_missing

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_reken')

! SUBROUTINE ARGUMENTS - INPUT
LOGICAL,   INTENT(IN)                            :: idep                       ! TRUE if deposition is modelled
LOGICAL,   INTENT(IN)                            :: isec                       ! TRUE if component is either SO2, NOx or NH3 (and a secondary component is present)
INTEGER*4, INTENT(IN)                            :: icm                        ! component number used in OPS 
                                                                               ! 1 SO2 
                                                                               ! 2 NO2 
                                                                               ! 3 NH3 
                                                                               ! 24 PM
LOGICAL,   INTENT(IN)                            :: gasv                       ! TRUE if component is a gas
INTEGER*4, INTENT(IN)                            :: intpol                     ! option for meteo interpolation 
                                                                               ! 0 interpolate between all meteo regions at specified location
                                                                               ! 1 use meteo parameters from user specified meteo region
                                                                               ! 2? use meteo parameters from user specified meteo file
REAL*4,    INTENT(IN)                            :: vchemc                     ! chemical conversion rate, independent of light [%/h]
REAL*4,    INTENT(IN)                            :: vchemv                     ! chemical conversion rate, dependent on light [%/h]
INTEGER*4, INTENT(IN)                            :: dv                         ! maximum code diurnal emission variation dverl
REAL*4,    INTENT(IN)                            :: amol1                      ! molar mass primary component [g/mol]
REAL*4,    INTENT(IN)                            :: amol2                      ! molar mass secondary component [g/mol]
REAL*4,    INTENT(IN)                            :: amol21                     ! (molar mass secondary component)/(molar mass primary component) [-] 
REAL*4,    INTENT(IN)                            :: ar                         ! proportionality constant in relation [OH] = ar Qr, with Qr = global radiation in W/m2 [(cm3 m2)/(molec W2)], see Egmond en Kesseboom (1983)
REAL*4,    INTENT(IN)                            :: rno2nox                    ! season dependent component of [NO2]/[NOx] ratio [-]
REAL*4,    INTENT(IN)                            :: ecvl(NSTAB, NTRAJ, *)      ! average diurnal emission variation for each stability/distance class
INTEGER*4, INTENT(IN)                            :: iseiz                      ! season index (0=long term; 1=year; 2=winter; 3=summer; 4=month in winter; 5=month in summer)
REAL*4,    INTENT(IN)                            :: zf                         ! interpolation factor between summer and winter (zf << "zomer fractie" = summer fraction)
REAL*4,    INTENT(IN)                            :: trafst(NTRAJ)              ! travel distances for each distance class [m]
INTEGER*4, INTENT(IN)                            :: knatdeppar                 ! choice for parameterisation wet deposition
                                                                               ! knatdeppar = 1: read wdeppar = scavenging coefficient [%/h]
                                                                               ! knatdeppar = 2: read wdeppar =  scavenging ratio, i.e. average ratio if rainwater concentrations and air concentration [-]
                                                                               ! knatdeppar = 3: if secondary components are present [SO2, NO2, NH3]. Wash-out and rain-out parameters are fixed in the OPS code (ops_init) [-]
INTEGER*4, INTENT(IN)                            :: mb                         ! start month of meteo statistics period ("b" << begin = start) 
REAL*4,    INTENT(IN)                            :: ugmoldep                   ! conversion factor from ug/m2/h to each of the deposition units in DEPUNITS 
REAL*4,    INTENT(IN)                            :: dg                         ! diffusion coefficient in air [cm^2/s] 
LOGICAL,   INTENT(IN)                            :: irev                       ! TRUE for reversible wash-out 
REAL*4,    INTENT(IN)                            :: scavcoef                   ! scavenging rate [%/h] 
REAL*4,    INTENT(IN)                            :: koh                        ! second order reaction rate constant of reaction NO2 + OH -> HNO3 [cm3/(molec s)]
REAL*4,    INTENT(IN)                            :: croutpri                   ! (wash-out + rain-out) ratio primary component, default value without correction for background concentration, season, stability class [-]
                                                                               ! icm = 1 (SO2): croutpri = wash out ratio at an N/S ratio of 1
                                                                               ! icm = 2 (NOx): croutpri = wash out ratio at an NO2/NOx ratio of 1
                                                                               ! icm = 3 (NH3): croutpri = wash out ratio (no correction)
REAL*4,    INTENT(IN)                            :: rcno                       ! surface resistance Rc for NO [s/m]
REAL*4,    INTENT(IN)                            :: rhno2                      ! ratio [HNO2]/[NOx] 
REAL*4,    INTENT(IN)                            :: rchno3                     ! surface resistance Rc for HNO3 [s/m] 
INTEGER*4, INTENT(IN)                            :: nrrcp                      ! number of receptor points  
INTEGER*4, INTENT(IN)                            :: ircp                       ! index of receptorpoint
REAL*4,    INTENT(IN)                            :: gxm                        ! x-coordinate of receptors (lon-lat) [degrees]
REAL*4,    INTENT(IN)                            :: gym                        ! y-coordinate of receptors (lon-lat) [degrees]
REAL*4,    INTENT(IN)                            :: xm                         ! x-coordinate of receptor points (RDM)
REAL*4,    INTENT(IN)                            :: ym                         ! y-coordinate of receptor points (RDM)
REAL*4,    INTENT(IN)                            :: zm                         ! z-coordinate of receptor points (RDM)
REAL*4,    INTENT(IN)                            :: frac                       ! fraction of grid cell inside NL 
REAL*4,    INTENT(IN)                            :: nh3bg_rcp                  ! NH3 background concentration (used in DEPAC) [ug/m3]  
REAL*4,    INTENT(IN)                            :: rhno3_rcp                  ! ratio [HNO3]/[NO3]_total at receptor points, [NO3]_total = [HNO3] + [NO3_aerosol] 
REAL*4,    INTENT(IN)                            :: bqrv                       ! source strength of space heating source (rv << "ruimteverwarming" = space heating) [g/s]
REAL*4,    INTENT(IN)                            :: bqtr                       ! source strength of traffic source [g/s]
INTEGER*4, INTENT(IN)                            :: bx                         ! x-coordinate of source 
INTEGER*4, INTENT(IN)                            :: by                         ! y-coordinate of source 
REAL*4,    INTENT(IN)                            :: bdiam                      ! source diameter [m]; if bdiam < 0 -> circular source, bdiam > 0 -> square sourc 
REAL*4,    INTENT(IN)                            :: bsterkte                   ! source strength [g/s] 
REAL*4,    INTENT(IN)                            :: bwarmte                    ! heat content of source [MW] 
REAL*4,    INTENT(IN)                            :: bhoogte                    ! source height [m] 
REAL*4,    INTENT(IN)                            :: bsigmaz                    ! spread in source height to represent different sources in a area source; 
                                                                               ! also used for initial sigma_z (vertical dispersion) of emission (e.g. traffic, building influence) [m] 
REAL*4,    INTENT(IN)                            :: bD_stack                   ! diameter of the stack [m]
REAL*4,    INTENT(IN)                            :: bV_stack                   ! exit velocity of plume at stack tip [m/s]
REAL*4,    INTENT(IN)                            :: bTs_stack                  ! temperature of effluent from stack [K]            
LOGICAL,   INTENT(IN)                            :: bemis_horizontal           ! horizontal outflow of emission
type(Tbuilding), INTENT(IN)                      :: bbuilding                  ! structure with building parameters
type(TbuildingEffect), INTENT(IN)                :: buildingEffect             ! structure containing building effect tables
INTEGER*4, INTENT(IN)                            :: btgedr                     ! temporal behaviour of sources (tgedr << "tijdsgedrag"== temporal behaviour)[-] 
INTEGER*4, INTENT(IN)                            :: bdegr                      ! option for particle size distribution
                                                                               ! bdegr >= 0 -> standard particle size distribution pmd
                                                                               ! bdegr  < 0 -> user-defined particle size distribution uspmd 
REAL*4,    INTENT(IN)                            :: z0_src                     ! roughness length at source; from z0-map [m]
REAL*4,    INTENT(IN)                            :: z0_tra                     ! roughness length representative for trajectory [m]
REAL*4,    INTENT(IN)                            :: z0_rcp                     ! roughness length at receptor; from z0-map [m]
REAL*4,    INTENT(IN)                            :: z0_metreg_rcp              ! roughness length at receptor; interpolated from meteo regions [m]
REAL*4,    INTENT(IN)                            :: lu_tra_per(NLU)            ! landuse (percentages) for all classes over trajectory
REAL*4,    INTENT(IN)                            :: lu_rcp_per(NLU)            ! landuse (percentages) for all classes for receptor
REAL*4,    INTENT(IN)                            :: so2sek(NSEK)               ! coefficient in correction factor for SO2 background concentration for each wind direction sector; derived from 24 regional LML stations over 2003
REAL*4,    INTENT(IN)                            :: no2sek(NSEK)               ! coefficient in correction factor for NO2 background concentration for each wind direction sector; derived from 15 regional LML stations over 2004
REAL*4,    INTENT(IN)                            :: so2bgtra                   ! SO2 background concentration, trajectory averaged [ppb]
REAL*4,    INTENT(IN)                            :: no2bgtra                   ! NO2 background concentration, trajectory averaged [ppb]
REAL*4,    INTENT(IN)                            :: nh3bgtra                   ! NH3 background concentration, trajectory averaged [ppb]
INTEGER*4, INTENT(IN)                            :: maxidx                     ! max. number of particle classes (= 1 for gas)
REAL*4,    INTENT(IN)                            :: pmd(NPARTCLASS,MAXDISTR)   ! standard particle size distributions 
REAL*4,    INTENT(IN)                            :: uspmd(NPARTCLASS,MAXDISTR) ! user-defined particle size distributions 
INTEGER*4, INTENT(IN)                            :: spgrid                     ! indicator for type of receptor points 
                                                                               ! spgrid = 0: regular grid of receptors, NL
                                                                               ! spgrid = 1: rectangular regular grid of receptors, user defined 
                                                                               ! spgrid = 2: receptors at specific locations, read from file
                                                                               ! spgrid = 3: receptors at user specific regular grid, not necessarily rectangular
REAL*4,    INTENT(IN)                            :: grid                       ! grid resolution [m] 
LOGICAL,   INTENT(IN)                            :: subbron                    ! whether to create "subbrons" (sub-sources inside a area source) and "subareas" (sub receptors inside a grid cell) or not  
REAL*4,    INTENT(IN)                            :: uurtot                     ! total number of hours in meteo statistics period ("uur"= hour) [hours]
REAL*4,    INTENT(IN)                            :: routsec                    ! in-cloud (rain-out) scavenging ratio for secondary component

! SUBROUTINE ARGUMENTS - I/O       (INOUT)
REAL*4,    INTENT(INOUT)                         :: rc                         ! surface resistance Rc [s/m] 
REAL*8,    INTENT(INOUT)                         :: somvnsec(NPARTCLASS)       ! summed wet deposition flux secondary component [ug/m2/h] 
REAL*8,    INTENT(INOUT)                         :: telvnsec(NPARTCLASS)       ! summed deposited mass per area for wet deposition of secondary component [ug/m2]
REAL*8,    INTENT(INOUT)                         :: vvchem(NPARTCLASS)         ! summed chemical conversion rate [%/h] 
REAL*8,    INTENT(INOUT)                         :: vtel(NPARTCLASS)           ! weighing factors for averaging vvchem (i.e. deposited mass)
REAL*8,    INTENT(INOUT)                         :: somvnpri(NPARTCLASS)       ! summed wet deposition flux primary component [ug/m2/h] 
REAL*8,    INTENT(INOUT)                         :: telvnpri(NPARTCLASS)       ! summed deposited mass per area for wet deposition of primary component [ug/m2]
DOUBLE PRECISION,    INTENT(INOUT)               :: ddepri(nrrcp,NPARTCLASS)   ! dry deposition of primary component at receptor points [mol/ha/y] 
REAL*8,    INTENT(INOUT)                         :: sdrypri(NPARTCLASS)        ! summed dry deposition of primary component [ug/m2/h]
REAL*8,    INTENT(INOUT)                         :: snatpri(NPARTCLASS)        ! summed wet deposition of primary component [ug/m2/h]  (<< "nat" = wet)
REAL*8,    INTENT(INOUT)                         :: sdrysec(NPARTCLASS)        ! summed dry deposition of secondary component [ug/m2/h]
REAL*8,    INTENT(INOUT)                         :: snatsec(NPARTCLASS)        ! summed wet deposition of secondary component [ug/m2/h]  (<< "nat" = wet)
DOUBLE PRECISION,    INTENT(INOUT)               :: cpri(nrrcp,NPARTCLASS)     ! concentration of primary component at receptor points [ug/m3] 
DOUBLE PRECISION,    INTENT(INOUT)               :: csec(nrrcp,NPARTCLASS)     ! concentration of secondary component ar receptor points [ug/m3] 
DOUBLE PRECISION,    INTENT(INOUT)               :: drydep(nrrcp,NPARTCLASS)   ! dry deposition at receptor points [mol/ha/y] 
DOUBLE PRECISION,    INTENT(INOUT)               :: wetdep(nrrcp,NPARTCLASS)   ! wet deposition at receptor points ["depeh"] 
REAL*4,    INTENT(INOUT)                         :: astat(NTRAJ,NCOMP,NSTAB,NSEK) ! meteo statistics for each distance class, stability/mixing height class, wind direction sector
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
                                                                               !        27. surface resistance Rc of NO3 aerosol [s/m]
REAL*4,    INTENT(INOUT)                         :: rno2_nox_sum(nrrcp)        ! NO2/NOx ratio, weighed sum over classes
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record 
       
! SUBROUTINE ARGUMENTS - OUTPUT       (OUT)
REAL*4,    INTENT(OUT)                           :: precip                     ! precipitation amount [mm]
REAL*4,    INTENT(OUT)                           :: routpri                    ! in-cloud (rain-out) scavenging ratio for primary component [-]  
REAL*4,    INTENT(OUT)                           :: dispg(NSTAB)               ! dispersion coefficients for vertical dispersion; sigma_z = dispg*x^disph [-]

! LOCAL VARIABLES
INTEGER*4                                        :: istab                      ! teller over stabiliteitsklassen
INTEGER*4                                        :: kdeel                      ! teller over deeltjesklassen
INTEGER*4                                        :: ndone                      ! 
INTEGER*4                                        :: itra                       ! 
INTEGER*4                                        :: idgr                       ! 
INTEGER*4                                        :: rond                       ! 
INTEGER*4                                        :: iwd                        ! 
INTEGER*4                                        :: ibtg                       ! 
INTEGER*4                                        :: isek                       ! 
INTEGER*4                                        :: isekt                      ! uitvoer stat_parexp
INTEGER*4                                        :: nk                         ! number of sub receptors (needed for grid averaged concentrations)
INTEGER*4                                        :: nr                         ! 
INTEGER*4                                        :: mrcp                       ! 
INTEGER*4                                        :: nrcp                       ! 
INTEGER*4                                        :: kk                         ! number of sub area sources
INTEGER*4                                        :: nb                         ! 
INTEGER*4                                        :: karea                      ! 
INTEGER*4                                        :: larea                      ! 
REAL*4                                           :: aind                       ! voortgangsindicator
REAL*4                                           :: htot                       ! 
REAL*4                                           :: c                          ! 
REAL*4                                           :: ueff                       ! wind speed at effective transport height heff; 
                                                                               ! for short distances heff = plume height;
                                                                               ! for large distances heff = 1/2 mixing height;
                                                                               ! heff is interpolated for intermediate distances.
REAL*4                                           :: rations                    ! trajectory verhouding N/S
REAL*4                                           :: qbron                      ! 
REAL*4                                           :: qtr                        ! 
REAL*4                                           :: qruim                      ! 
REAL*4                                           :: grad                       ! 
REAL*4                                           :: qob                        ! 
REAL*4                                           :: qww                        ! 
REAL*4                                           :: hbron                      ! 
REAL*4                                           :: percvk                     ! 
REAL*4                                           :: grof                       ! 
REAL*4                                           :: cgt                        ! 
REAL*4                                           :: cgt_z                      ! hoogte afhankelijkelijke cgt
REAL*4                                           :: x                          ! 
REAL*4                                           :: y                          ! 
REAL*4                                           :: diam                       ! 
REAL*4                                           :: diameter                   ! 
REAL*4                                           :: szopp                      ! 
REAL*4                                           :: D_stack                    ! diameter of the stack [m]
REAL*4                                           :: V_stack                    ! exit velocity of plume at stack tip [m/s]
REAL*4                                           :: Ts_stack                   ! temperature of effluent from stack [K]
LOGICAL                                          :: emis_horizontal            ! horizontal outflow of emission  
type(Tbuilding)                                  :: building                   ! structure with building paramaters
REAL*4                                           :: buildingFact               ! The interpolated building effect from the buildingTable
REAL*4                                           :: qrv                        ! 
REAL*4                                           :: virty                      ! 
REAL*4                                           :: consec                     ! 
REAL*4                                           :: angle_SR_xaxis             ! angle between source-receptor vector and x-axis (needed for building effect) [degrees]
REAL*4                                           :: disx                       ! linear distance between source and receptor [m]
REAL*4                                           :: disxx                      ! effective travel distance between source and receptor [m]
REAL*4                                           :: radius                     ! 
REAL*4                                           :: uster_metreg_rcp           ! 
REAL*4                                           :: temp_C                     ! temperature at height zmet_T [C]
REAL*4                                           :: shear                      ! 
REAL*4                                           :: ol_metreg_rcp              ! 
REAL*4                                           :: h0                         ! 
REAL*4                                           :: hum                        ! 
REAL*4                                           :: rcno2d                     ! 
REAL*4                                           :: rcnh3d                     ! 
REAL*4                                           :: rcaerd                     ! 
REAL*4                                           :: vw10                       ! 
REAL*4                                           :: pcoef                      ! 
REAL*4                                           :: htt                        ! 
REAL*4                                           :: aant                       ! 
REAL*4                                           :: xl                         ! 
REAL*4                                           :: rb                         ! 
REAL*4                                           :: rbm                        ! 
REAL*4                                           :: ra4                        ! 
REAL*4                                           :: ra4m                       ! 
REAL*4                                           :: ra50                       ! 
REAL*4                                           :: ra50m                      ! 
REAL*4                                           :: xvglbr                     ! 
REAL*4                                           :: xvghbr                     ! 
REAL*4                                           :: xloc                       ! 
REAL*4                                           :: xl100                      ! 
REAL*4                                           :: rad                        ! 
REAL*4                                           :: rcso2                      ! 
REAL*4                                           :: coef_space_heating         ! space heating coefficient (degree-day values in combination with a wind speed correction) [C m^1/2 / s^1/2] 
REAL*4                                           :: regenk                     ! 
REAL*4                                           :: buil                       ! 
REAL*4                                           :: rint                       ! 
REAL*4                                           :: aksek(NSEK)                ! .... (dummy output van ops_statparexp)
REAL*4                                           :: uster_rcp                  ! friction velocity at receptor; for z0 at receptor [m/s]
REAL*4                                           :: ol_rcp                     ! Monin-Obukhov length at receptor; for z0 at receptor [m/s] 
REAL*4                                           :: uster_src                  ! 
REAL*4                                           :: ol_src                     ! 
REAL*4                                           :: uster_tra                  ! 
REAL*4                                           :: ol_tra                     ! 
REAL*4                                           :: uh                         ! 
REAL*4                                           :: zu                         ! 
REAL*4                                           :: onder                      ! 
REAL*4                                           :: xlm                        ! 
REAL*4                                           :: onderm                     ! 
REAL*4                                           :: qbpri                      ! 
REAL*4                                           :: qsec                       ! 
REAL*4                                           :: sigz                       ! 
REAL*4                                           :: ccc                        ! undepleted concentration including part above mixing layer; 
                                                                               ! is needed for e.g. wet deposition.
REAL*4                                           :: rcsec                      ! 
REAL*4                                           :: rc_sec_rcp                 ! 
REAL*4                                           :: rb_rcp                     ! 
REAL*4                                           :: ra50_rcp                   ! 
REAL*4                                           :: raz_rcp                    ! 
REAL*4                                           :: rc_rcp                     ! 
REAL*4                                           :: ra4_rcp                    ! 
REAL*4                                           :: vg50_rcp                   ! 
REAL*4                                           :: pr                         ! 
REAL*4                                           :: utr                        ! average wind speed over the trajectory (m/s)
REAL*4                                           :: vchem                      ! 
REAL*4                                           :: vg50trans                  ! 
REAL*4                                           :: vgpart                     ! 
REAL*4                                           :: rkc                        ! 
REAL*4                                           :: ri                         ! 
REAL*4                                           :: twt                        ! 
REAL*4                                           :: vnatpri                    ! 
REAL*4                                           :: cq2                        ! 
REAL*4                                           :: cdn                        ! 
REAL*4                                           :: cch                        ! 
REAL*4                                           :: dm                         ! 
REAL*4                                           :: cratio                     ! 
REAL*4                                           :: rhno3                      ! 
REAL*4                                           :: rrno2nox                   ! 
REAL*4                                           :: vchemnh3                   ! 
REAL*4                                           :: dx                         ! 
REAL*4                                           :: dy                         ! 
REAL*4                                           :: dxsub                      ! 
REAL*4                                           :: dysub                      ! 
REAL*4                                           :: gbx                        ! 
REAL*4                                           :: gby                        ! 
REAL*4                                           :: rctra_0
REAL*4                                           :: rctra_50
REAL*4                                           :: rclocal
REAL*4                                           :: rcsrc
REAL*4                                           :: ra4src
REAL*4                                           :: rb_src
REAL*4                                           :: ra50src
REAL*4                                           :: ra4tra
REAL*4                                           :: ra50tra
REAL*4                                           :: rb_tra
REAL*4                                           :: rnox                       ! NO2/NOx ratio
REAL*4                                           :: xg

LOGICAL                                          :: inc_rcp                    ! increase receptorpoints
LOGICAL                                          :: z0found                    ! Wel of geen z0 gevonden
LOGICAL                                          :: depudone                   ! Wel of niet ops_depu aangeroepen
! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
!
!   Generate precipitation field for this receptor. 
!   Can be used to derive concentration in rainwater from deposition flux,
!   in order to compare this rainwater concentration with measurements.
!
    IF (precip.eq.0) CALL ops_gen_precip(uurtot, astat, trafst, precip, error)
!
!   Simulate a receptor grid cell by a (large) number of receptor points.
!   Determine nk = number of sub-receptor points.
!
!   Get source diameter and decide whether we have a circular source (rond = 1) or a square one (rond = 0)
!   ("rond" = round)
!
    rond = 0
    diam = abs(bdiam)
    IF (bdiam < 0) rond = 1
!
!   Decide whether we need sub-receptors; they are not needed in case of user-specified receptor locations (spgrid = 2), 
!   but only for gridded receptors (in order to obtain an averaged grid-value).
!   If the current source is an area source with the same diameter as the receptor grid resolution (diam = grid), 
!   concentration gradients of the source are relatively small and no sub receptors are needed. Note that in this case,
!   we also subdivide the area source into several sub-area sources.
!   Also check the command line argument subbron.
!      
    inc_rcp = spgrid /= 2 .AND. diam /= grid 
    IF (inc_rcp .AND. subbron) THEN
!
!     Compute x- and y-distance between receptor and source
!
      dx = xm - bx
      dy = ym - by
!     
!    If source and receptor are close, we have strong gradients;
!    low source -> high concentration gradients -> many sub receptors 
!    small diameter -> point source -> high concentration gradients -> many sub receptors 
!
     IF (abs(dx) <= grid*0.5 .AND. abs(dy) <= grid*0.5) THEN
        nk = grid/(bhoogte*20 + diam)
!
!    If source and receptor are far from each other, we have less strong gradients;
!    small distance between source and receptor -> high concentration gradients -> many sub receptors 
!    low source -> high concentration gradients -> many sub receptors 
!    small diameter -> point source -> high concentration gradients -> many sub receptors 
!    maximal 4 in 1/2 the grid cell -> 8*8 = 64
!    Note: for sources having geographical coordinates (IGEO = 1), the distance for receptors in NL is so large that nk = 0
!
      ELSE
        nk = grid/(sqrt(dx**2+dy**2) + diam + bhoogte*10)*2
        nk = min0(nk,4)                                                        ! This (4) is an arbitrary limit
      ENDIF
!      
!     Compute nr = number of sub receptors within a grid cell;
!     maximal (128*2+1)**2 = 66049 sub receptors.
!
      nk = min0(nk,128)                                                        ! This (128) is an arbitrary limit
      nr = (nk*2 + 1)**2
    ELSE
!
!    (no grid .OR. diam = grid .OR. .NOT. subbron) -> no sub receptors
!
      nk = 0
      nr = 1
    ENDIF
!
!+++ Loop over sub receptors (in x- and y-direction) ++++++++++++++++++++++++
!
    DO mrcp = -nk,nk
      DO nrcp = -nk,nk

!
!       Compute kk = number of sub area sources for the current sub receptor
!       if diam = 0, no sub sources are needed
!       if rond /= 0 (circular area source) -> no sub sources (sub sources are not supported when using circular sources)
!       if -nosub is given at the command line -> no sub sources (test only)
!       if number of sub receptors >= 1000 -> no sub sources; is needed to limit computational resources.
!
        IF (diam > 0 .AND. rond == 0 .AND. subbron .AND. nr .LT. 1000) THEN    ! This (1000) is an arbitrary limit
!
!         Compute x- and y-distance between sub receptor and source
!
          dxsub = xm + mrcp*grid/(nk*2 + 1) - bx
          dysub = ym + nrcp*grid/(nk*2 + 1) - by
!
!         A. sub receptor inside area source
!         low diameter -> few sub sources
!         low source -> many sub sources
!         Note that in many cases, where diam < 250*bhoogte -> kk = 6, nb = 169
!
          IF (abs(dxsub) <= diam*0.5 .AND. abs(dysub) <= diam*0.5) THEN
            kk = int(diam/bhoogte/500) + 6
!
!         B. sub receptor outside area source
!         low diameter -> few sub sources
!         relatively far -> few sub sources
!
          ELSE
            kk = int(diam/(sqrt(dxsub**2+dysub**2) + 0.1)*2)
          ENDIF
!      
!         Compute nb = number of sub area sources;
!         maximal (32*2+1)**2 = 4225 sub receptors.
!
          kk = min0(kk,32)      ! This (32) is an arbitrary limit
          nb = (kk*2 + 1)**2
        ELSE
          kk = 0
          nb = 1
        ENDIF
!
!++++++ Loop over sub-area sources ++++++++++++++++++++++++
!
!       Simulate an area source by a (large) number of small area sources. 
!       Note: the next loop is carried out only once in case of a point source (kk=0)
!
        DO karea = -kk,kk
          DO larea = -kk,kk
!
!           Get emission data for current source and compute wind sector for source - receptor direction

!
            CALL wind_rek(bx, by, bdiam, bsterkte, bwarmte, bhoogte, bsigmaz, bD_stack, bV_stack, bTs_stack, bemis_horizontal, bbuilding, btgedr, bdegr, bqrv, bqtr, gxm, gym, xm,    &
                       &  ym, grid, nk, nr, mrcp, nrcp, kk, nb, karea, larea, angle_SR_xaxis, disx, x, y, qob, qww, hbron, szopp, D_stack, V_stack, Ts_stack, emis_horizontal, building, ibtg, idgr,  &
                       &  qrv, qtr, rond, diameter, iwd, isek)

            ! Compute building effect (depends on source-receptor distance; we can use linear distance, because the building effect is only present near the source):
            call ops_building_get_factor(building%type, angle_SR_xaxis, disx, buildingEffect%buildingFactAngleSRxaxis, buildingEffect%buildingFactDistances, building%buildingFactFunction, buildingFact)
            ! write(*,*) 'ops_reken/disx;buildingFact', angle_SR_xaxis, disx, buildingFact
            if (error%debug) write(*,'(a,a,a,3(1x,i6),3(1x,e12.5))') trim(ROUTINENAAM),' A ',' ircp,iwd,isek,angle_SR_xaxis,disx,buildingFact:',ircp,iwd,isek,angle_SR_xaxis,disx,buildingFact
!
!           Compute chemical parameters (conversion rates, concentration ratios) in case of secondary components
!
            IF (isec) THEN
              CALL ops_par_chem(icm, isek, so2sek, no2sek, so2bgtra, no2bgtra, nh3bgtra, disx, diameter, vchemnh3, rhno3,       &
                             &  rrno2nox, rations)
            ENDIF
            if (error%debug) write(*,'(3a,1x,i6,4(1x,e12.5))') trim(ROUTINENAAM),' B ',' ircp,vchemnh3, rhno3, rrno2nox, rations :',ircp,vchemnh3, rhno3, rrno2nox, rations    


!
!++++++++++ Loop over stability classes ++++++++++++++++++++++++
!
            DO istab = 1, NSTAB  
        
              ! Compute source radius
              radius = diameter/2.
!
!             Get relevant parameters from meteo statistics (dependent on wind direction and source-receptor distance), for the current
!             stability class and compute the effective source height.
!
              CALL ops_statparexp(istab, hbron, qww, D_stack, V_stack, Ts_stack, emis_horizontal, iwd, radius, uurtot, astat, trafst, disx, isek, disxx, isekt, vw10, aksek, &
                               &  h0, hum, ol_metreg_rcp, shear, rcaerd, rcnh3d, rcno2d, temp_C, uster_metreg_rcp, pcoef,       &
                               &  htot, htt, itra, aant, xl, rb, ra4, ra50, xvglbr, xvghbr, xloc, xl100, rad, rcso2,            &
                               &  coef_space_heating, regenk, buil, rint, percvk, error)
              if (error%debug) then
                 write(*,'(3a,99(1x,i6))')             trim(ROUTINENAAM),',C1,',' ircp,istab, isek, isekt, itra :', &
                                                                                  ircp,istab, isek, isekt, itra
                 write(*,'(3a,2(1x,i6),99(1x,e12.5))') trim(ROUTINENAAM),',C2,',' ircp,istab, disx, disxx, vw10 :', &
                                                                                  ircp,istab,disx, disxx,  vw10
                 write(*,'(3a,2(1x,i6),99(1x,e12.5))') trim(ROUTINENAAM),',C3,',' ircp,istab, h0, hum, ol_metreg_rcp, shear, rcaerd, rcnh3d, rcno2d, temp_C, uster_metreg_rcp, pcoef :', &
                                                                                  ircp,istab, h0, hum, ol_metreg_rcp, shear, rcaerd, rcnh3d, rcno2d, temp_C, uster_metreg_rcp, pcoef
                 write(*,'(3a,2(1x,i6),99(1x,e12.5))') trim(ROUTINENAAM),',C4,',' ircp,istab, htot, htt, aant, xl, rb, ra4, ra50, xvglbr, xvghbr, xloc, xl100, rad, rcso2 :', &
                                                                                  ircp,istab, htot, htt, aant, xl, rb, ra4, ra50, xvglbr, xvghbr, xloc, xl100, rad, rcso2
                 write(*,'(3a,2(1x,i6),99(1x,e12.5))') trim(ROUTINENAAM),',C5,',' ircp,istab, coef_space_heating, regenk, buil, rint, percvk :', &
                                                                                  ircp,istab, coef_space_heating, regenk, buil, rint, percvk
               endif

              IF (error%haserror) GOTO 9999
!
!             Negative sensible heat flux H0 [W/m2] not allowed.
!
              IF (ABS(h0) <= EPS_DELTA) THEN
                h0 = 1.
              ENDIF
!
!             If combination of stability, distance and wind direction class occurs 
!
              IF (percvk > EPS_DELTA) THEN
! 
!               Skip if point source and receptor coincide
!
                IF (.NOT. ((ABS(disxx) <= EPS_DELTA) .AND. (ABS(diameter) <= EPS_DELTA))) THEN
!
!                 Compute parameters which depend on stability class (friction velocity, Monin-Obukhov length, plume rise,
!                 vertical dispersion coefficient). Adjust yearly averaged emission for the current {stability, distance} class.
!
                  CALL ops_stab_rek(icm, rb, temp_C, h0, z0_metreg_rcp, disxx, z0_rcp, xl, radius, qtr, qrv, dv, ecvl, coef_space_heating, ibtg,    &
                                 &  uster_metreg_rcp, hbron, qww, D_stack, V_stack, Ts_stack, emis_horizontal, istab, itra, qob, xloc, regenk, ra4, z0_tra, z0_src,  &
                                 &  ol_metreg_rcp, error, uster_rcp, ol_rcp, uster_src, ol_src, uster_tra, ol_tra,                                  &
                                 &  htot, htt, onder, uh, zu, qruim, qbron, dispg)
                  if (error%debug) write(*,'(3a,2(1x,i6),99(1x,e12.5))') &
                     trim(ROUTINENAAM),',D,',' ircp,istab, uster_rcp,ol_rcp,uster_src,ol_src,uster_tra,ol_tra,htot,htt,onder,uh,zu,qruim,qbron,dispg :', &
                                               ircp,istab,uster_rcp,ol_rcp,uster_src,ol_src,uster_tra,ol_tra,htot,htt,onder,uh,zu,qruim,qbron,dispg(istab)
                  IF (error%haserror) GOTO 9999
!
!                 Continue if source strength > 0
!
                  IF (qbron > (0. + EPS_DELTA)) THEN
                    
!                   Store parameters xl, onder, rb, ra50 and ra4 for further use
                    xlm    = xl
                    onderm = onder
                    rbm    = rb                                                        ! 960215
                    ra50m  = ra50                                                      ! 960215
                    ra4m   = ra4                                                       ! 960215
         
!
!++++++++++++++++   Loop over particle classes ++++++++++++++++++++++++
!                   For a gaseous component, there is only one such class.
!
                    depudone = .FALSE.
                    DO kdeel = 1, maxidx

                      ! Get stored parameters xl, onder, rb, ra50 and ra4 and total source height htt
                      xl    = xlm
                      htot  = htt
                      onder = onderm
                      ra50  = ra50m
                      ra4   = ra4m
                      rb    = rbm                                                      ! 960215
!
!                     Compute source strength of primary component, for the current particle class;
!                     multiply emissions of particles with the mass fraction in the current particle class;
!                     idgr >= 0 -> standard particle size distribution pmd
!                     idgr  < 0 -> user-defined particle size distribution uspmd
!
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
!
!                     Continue if source strength of this particle class > 0
!                      
                      IF (ABS(qbpri) .GT. EPS_DELTA) THEN
!
!                       Compute initial concentrations due to transport and dispersion; no removal processes yet
!                        
                        CALL ops_conc_ini(gasv, vw10, htt, pcoef, disxx, kdeel, qbpri, z0_src, szopp, rond, uster_src, ol_src,  &
                                       &  istab, iwd, qww, hbron, dispg, radius, xl, onder,                                     &
                                       &  htot, grof, c, sigz, ueff, virty, ccc, error)
                        if (error%debug) write(*,'(3a,2(1x,i6),99(1x,e12.5))') trim(ROUTINENAAM),',E,',' ircp,istab,radius,xl,onder,htot,grof,c,sigz,ueff,virty,ccc:', &
                                                                                                         ircp,istab,radius,xl,onder,htot,grof,c,sigz,ueff,virty,ccc
!
!                       Compute deposition velocities for dry and wet deposition and the concentration decrease as a result
!                       of deposition and (chemical) conversion. Only if idep = TRUE.
!
                        IF (idep) THEN
                          CALL ops_resist_rek(vchemc, vchemv, rad, isec, icm, rcso2, regenk, rcaerd, iseiz, istab, itra, ar,    &
                                           &  rno2nox, rcnh3d, vchemnh3, hum, uster_rcp, ol_rcp, uster_tra, ol_tra,             &
                                           &  z0_rcp, z0_metreg_rcp, rcno2d, kdeel, mb, vw10, temp_C, disxx, zm, koh,           &
                                           &  rations, rhno3, rcno, rhno2, rchno3, croutpri, rrno2nox, rhno3_rcp,               &
                                           &  rb, ra4, ra50, rc, routpri, vchem, rcsec, uh, rc_sec_rcp, rc_rcp, rb_rcp,         &
                                           &  ra4_rcp, ra50_rcp, raz_rcp, z0_src, ol_src, uster_src, z0_tra, rctra_0, rcsrc,    &
                                           &  ra4src, rb_src, ra50src, ra4tra, ra50tra, rb_tra, rclocal, nh3bg_rcp, nh3bgtra,   &
                                           &  gym, depudone, gasv, lu_rcp_per, lu_tra_per, rnox)
        
                          cratio = 1.
                          CALL ops_depoparexp(kdeel, c, ol_src, qbpri, ra4_rcp, ra50_rcp, raz_rcp, rb_rcp, sigz, ueff,          &
                                           &  uster_src, z0_src, virty, gasv, itra, rb, ra4, istab, grof, ra50, xvghbr, xvglbr, &
                                           &  regenk, rint, buil, zf, isekt, iseiz, mb, disxx, radius, xl, onder, dg,           &
                                           &  knatdeppar, scavcoef, irev, htt, xloc, xl100, vw10, pcoef, vchem, dispg, htot,    &
                                           &  error, pr, twt, cratio, rc_rcp, grad, rc, utr, vg50_rcp, routpri,                 &
                                           &  vg50trans, rkc, ri, vnatpri, cgt, cgt_z, cq2, cdn, cch, z0_src, ol_src, uster_src,& 
                                           &  z0_tra, rctra_0, rcsrc, ra4src, rb_src, ra50src, ra4tra, ra50tra, rb_tra, vgpart, &
                                           &  xm, ym, zm, bx, by, xg)
        
                          IF (.NOT.gasv) rclocal = rc_rcp
                          CALL ops_conc_rek(ueff, qbpri, isec, rcsec, routsec, ccc, amol1, amol2, sigz, utr, rc_sec_rcp,         &
                                         &  ra4_rcp, ra50_rcp, rb_rcp, amol21, ugmoldep, cch, cgt, cgt_z, grof, percvk, onder,   &
                                         &  regenk, virty, ri, vw10, hbron, pcoef, rkc, disxx, vnatpri, vchem, radius, xl, xloc, &
                                         &  htot, twt, rb, ra50, xvghbr, xvglbr, grad, frac, cdn, cq2, c, sdrypri(kdeel),        &
                                         &  sdrysec(kdeel), snatsec(kdeel), somvnsec(kdeel), telvnsec(kdeel), vvchem(kdeel),     &
                                         &  vtel(kdeel), snatpri(kdeel), somvnpri(kdeel), telvnpri(kdeel), ddepri(ircp,kdeel),   &
                                         &  drydep(ircp,kdeel),  wetdep(ircp,kdeel), dm, qsec, consec, pr,                       & 
                                         &  vg50trans, ra50tra, rb_tra, rclocal, vgpart, xg, buildingFact)
                                                                                
!
!                         Update summed concentration for secondary concentration
!                                           
                          csec(ircp,kdeel) = csec(ircp,kdeel) + (consec*percvk)
                        ELSE
                           ! Building effect for idep = 0:
                           c = c*buildingFact
                        ENDIF                                        ! end condition idep (compute deposition)
!
!                       Update summed concentration for primary concentration
!                                            
                        cpri(ircp,kdeel) = cpri(ircp,kdeel) + (c*percvk)
                        IF (idep) THEN
                          rno2_nox_sum(ircp) = rno2_nox_sum(ircp) + (rnox*percvk)
                        ENDIF
!
!                       Check for negative concentrations
!
                        IF (c < 0. - EPS_DELTA) GOTO 1000
                      ENDIF                                          ! end condition source strength of particle class > 0
                    ENDDO                                            ! end loop over particle classes
                  ENDIF                                              ! end condition source strength > 0
                ENDIF                                                ! end condition source - receptor distance > 0
              ENDIF                                                  ! end condition percvk > 0 (fraction occurrence of meteo class)
            ENDDO                                                    ! end loop over stability classes
          ENDDO                                                      ! end loop over sub-areas (y-direction)
        ENDDO                                                        ! end loop over sub-areas (x-direction)
      ENDDO                                                          ! end loop over sub receptors (y-direction)
    ENDDO                                                            ! end loop over sub receptors (x-direction)

    ! Computation for this source completed, deallocate buildingFactFunction for this source:
    if (building%type > 0) deallocate(building%buildingFactFunction)
RETURN
!
! Negative concentration. Create error message and close the progress file.
!
1000 CALL SetError('Negative concentration encountered', error)
CALL ErrorParam('concentration', c, error)
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
CALL ErrorParam('vg50_rcp', vg50_rcp, error)
CALL ErrorParam('xl', xl, error)
CALL ErrorParam('sigz', sigz, error)
CALL ErrorParam('cq2', cq2, error)
CALL ErrorParam('cdn', cdn, error)
CALL ErrorParam('grad', grad, error)
CALL ErrorParam('ol_metreg_rcp', ol_metreg_rcp, error)
CALL ErrorParam('vg50trans', vg50trans, error)
CALL ErrorParam('ccc', ccc, error)

9999 CLOSE (fu_progress)
CALL ErrorCall(ROUTINENAAM, error)

RETURN

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : wind_rek, contained in ops_reken
! DESCRIPTION        : Compute preliminary wind sector, not including wind shear.
!                      Also get all source data for the current source
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE wind_rek(bx, by, bdiam, bsterkte, bwarmte, bhoogte, bsigmaz, bD_stack, bV_stack, bTs_stack, bemis_horizontal, bbuilding, btgedr, bdegr, bqrv, bqtr, gxm, gym, xm, ym,  &
                 &  grid, nk, nr, mrcp, nrcp, kk, nb, karea, larea, angle_SR_xaxis, disx, x, y, qob, qww, hbron, szopp, D_stack, V_stack, Ts_stack, emis_horizontal, building, ibtg, idgr, qrv,   &
                 &  qtr, rond, diameter, iwd, isek)

USE Binas, only: deg2rad, rad2deg
                 
! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'wind_rek')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: bx                         ! 
INTEGER*4, INTENT(IN)                            :: by                         ! 
REAL*4,    INTENT(IN)                            :: bdiam                      ! 
REAL*4,    INTENT(IN)                            :: bsterkte                   ! 
REAL*4,    INTENT(IN)                            :: bwarmte                    ! 
REAL*4,    INTENT(IN)                            :: bhoogte                    ! 
REAL*4,    INTENT(IN)                            :: bsigmaz                    ! 
REAL*4,    INTENT(IN)                            :: bD_stack                   ! diameter of the stack [m]
REAL*4,    INTENT(IN)                            :: bV_stack                   ! exit velocity of plume at stack tip [m/s]
REAL*4,    INTENT(IN)                            :: bTs_stack                  ! temperature of effluent from stack [K]            
LOGICAL,   INTENT(IN)                            :: bemis_horizontal           ! horizontal outflow of emission
type(Tbuilding), INTENT(IN)                      :: bbuilding                  ! structure with building parameters
INTEGER*4, INTENT(IN)                            :: btgedr                     ! 
INTEGER*4, INTENT(IN)                            :: bdegr                      ! 
REAL*4,    INTENT(IN)                            :: bqrv                       ! 
REAL*4,    INTENT(IN)                            :: bqtr                       ! 
REAL*4,    INTENT(IN)                            :: gxm                        ! 
REAL*4,    INTENT(IN)                            :: gym                        ! 
REAL*4,    INTENT(IN)                            :: xm                         ! 
REAL*4,    INTENT(IN)                            :: ym                         ! 
REAL*4,    INTENT(IN)                            :: grid                       ! 
INTEGER*4, INTENT(IN)                            :: nk                         ! 
INTEGER*4, INTENT(IN)                            :: nr                         ! 
INTEGER*4, INTENT(IN)                            :: mrcp                       ! 
INTEGER*4, INTENT(IN)                            :: nrcp                       ! 
INTEGER*4, INTENT(IN)                            :: kk                         ! 
INTEGER*4, INTENT(IN)                            :: nb                         ! 
INTEGER*4, INTENT(IN)                            :: karea                      ! 
INTEGER*4, INTENT(IN)                            :: larea                      ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: angle_SR_xaxis             ! angle between source-receptor vector and x-axis (needed for building effect) [degrees]
REAL*4,    INTENT(OUT)                           :: disx                       ! linear distance between source and receptor [m] 
REAL*4,    INTENT(OUT)                           :: x                          ! 
REAL*4,    INTENT(OUT)                           :: y                          ! 
REAL*4,    INTENT(OUT)                           :: qob                        ! 
REAL*4,    INTENT(OUT)                           :: qww                        ! 
REAL*4,    INTENT(OUT)                           :: hbron                      ! 
REAL*4,    INTENT(OUT)                           :: szopp                      ! 
REAL*4,    INTENT(OUT)                           :: D_stack                    ! diameter of the stack [m]
REAL*4,    INTENT(OUT)                           :: V_stack                    ! exit velocity of plume at stack tip [m/s]
REAL*4,    INTENT(OUT)                           :: Ts_stack                   ! temperature of effluent from stack [K]            
LOGICAL,   INTENT(OUT)                           :: emis_horizontal            ! horizontal outflow of emission
type(Tbuilding), INTENT(OUT)                     :: building                   ! strucure with building parameters
INTEGER*4, INTENT(OUT)                           :: ibtg                       ! 
INTEGER*4, INTENT(OUT)                           :: idgr                       ! 
REAL*4,    INTENT(OUT)                           :: qrv                        ! 
REAL*4,    INTENT(OUT)                           :: qtr                        ! 
INTEGER*4, INTENT(OUT)                           :: rond                       ! 
REAL*4,    INTENT(OUT)                           :: diameter                   ! 
INTEGER*4, INTENT(OUT)                           :: iwd                        ! 
INTEGER*4, INTENT(OUT)                           :: isek                       ! 

! LOCAL VARIABLES
REAL*4                                           :: dx                         ! 
REAL*4                                           :: dy                         ! 

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
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
szopp    = bsigmaz                    ! spread in emission height [m]
D_stack  = bD_stack                   ! diameter of the stack [m]
V_stack  = bV_stack                   ! exit velocity of plume at stack tip [m/s]
Ts_stack = bTs_stack                  ! temperature of effluent from stack [K]            
emis_horizontal = bemis_horizontal    ! horizontal outflow of emission
building = bbuilding                  ! building parameters 
ibtg     = btgedr                     ! diurnal variation code
idgr     = bdegr                      ! particle size distribution code

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
! Determine preliminary wind-sector (some correction will be applied later for a height dependent wind shear, 
! but plume rise is not known at this point). Outcome: 0 <= iwd <= 360.
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
! Determine wind direction index isek for the wind sector, knowing 0 <= iwd < 360.
!
isek = (iwd + 180/NSEK)*NSEK/360 + 1
IF (isek .GT. NSEK) THEN
  isek = 1
ENDIF

! Determine angle between source-receptor vector and x-axis, betweeen 0 and 360 degrees (needed for building effect):
angle_SR_xaxis = modulo(atan2(dy,dx)*rad2deg, 360.0)  ! degrees

RETURN
END SUBROUTINE wind_rek

END
