module m_ops_receptor_loop
contains
   subroutine ops_receptor_loop( varin, &
         iter, niter, &
         ircp, namrcp, nsbuf, gxm_rcp, gym_rcp, x_rcp, y_rcp, zm, &
         xreg, yreg, z0_rcp_all, cs, z0_metreg, &
         z0nlgrid, z0eurgrid, so2bggrid, no2bggrid, nh3bggrid, gwgrid, o3bggrid, &
         f_z0user, z0_user, z0_metreg_user, spgrid, &
         lugrid, domlu, perc, lu_rcp_per_user_all, lu_rcp_dom_all, &
         parout_write, mindist, maxdist, bnr, bx, by, parout_disx, &
         icm, iopt_vchem, vchem_emep, vchemc, vchemv, dv, amol1, amol2, amol21, ar, &
         r_no2_nox_sec, r_no2_nox_season, ecvl, iseiz, zf, trafst, knatdeppar, mb, &
         ugmoldep, dg, irev, scavcoef, koh, croutpri, routpri, rc_no, rhno2, rc_hno3, &
         isec, idep, gasv, intpol, do_proc, &
         frac, nh3bg_rcp, gw_rcp, o3bg_rcp, so2bg_rcp, rhno3_rcp, bqrv, bqtr, bdiam, &
         bsterkte, bwarmte, bhoogte, bsigmaz, bD_stack, bV_stack, bTs_stack, &
         bemis_horizontal, bbuilding, buildingEffect, btgedr, bdegr, bcatnr, &
         nemcat_road, road_chem, road_disp, emcat_road, so2sek, no2sek, &
         maxidx, pmd, uspmd, grid, subbron, routsec, rc_user, nparout, &
         class_output, &
         uurtot_in, astat_in, lroad_corr_present, somvnsec, telvnsec, vvchem, &
         vtel, somvnpri, telvnpri, sdrypri, snatpri, sdrysec, snatsec, nsrc_class, &
         cnox_sec, cno2, percvk_sec, nsrc_sec, parout_val, ddepri, wdepri, cpri, &
         cpri_class, percvk_class, csec, drydep, wetdep, &
         precip, parout_name, parout_unit, error)
   use m_aps, only: TApsGridInt,  TApsGridReal 
   use m_ops_vchem, only: Tvchem
   use m_ops_building, only: tBuilding, tBuildingEffect
   use m_error, only: TError
   use m_ops_tdo_proc, only: Tdo_proc
   use m_commonconst_lib, only: NLU, NSEK, NSTAB, NTRAJ, NCOMP

   use m_utils,           only: AllocError
   use m_ops_rcp_char_1,  only: ops_rcp_char_1 
   use m_ops_output_lt,   only: ops_parout_circle 
   use m_ops_check_reken, only: ops_check_reken
   use m_ops_src_char,    only: ops_src_char 
   use m_error,           only: ErrorParam, ErrorCall
   use m_ops_tra_char,    only: ops_tra_char
   use m_ops_reken,       only: ops_reken 
   use m_ops_varin,       only: Tvarin
   
   implicit none

      CHARACTER(len=*), parameter :: ROUTINENAAM = 'ops_receptor_loop'

      TYPE(Tvarin), INTENT(IN) :: varin               ! input variables for meteo
      INTEGER,   INTENT(IN) :: iter, niter            ! iteration number out of number of iterations
      INTEGER,   INTENT(IN) :: ircp                   ! index of receptorpoint
      CHARACTER(len=*), intent(in) :: namrcp          ! receptor name
      integer,   intent(in) :: nsbuf                  ! number of sources in buffer  
      REAL,      INTENT(IN) :: gxm_rcp, gym_rcp       ! (x,y)-coordinaten van receptorpunt ircp (lola)
      REAL,      INTENT(IN) :: x_rcp, y_rcp           ! (x,y)-coordinaten van receptorpunten (RDM)
      REAL,      INTENT(IN) :: zm                     ! z-coordinate of receptor point ircp (m)
      REAL,      INTENT(IN) :: xreg(:), yreg(:)       ! arrays met (x,y)-coordinaten van meteo-regio's
      REAL,      INTENT(IN) :: z0_rcp_all             ! roughness length for receptor ircp; from z0-map or receptor file [m]
      REAL,      INTENT(IN) :: cs(:,:,:,:,:)          ! 
      REAL,      INTENT(IN) :: z0_metreg(:)           ! roughness lengths of NMETREG meteo regions; scale < 50 km [m]
      TYPE (TApsGridInt),  INTENT(IN) :: z0nlgrid     ! map of roughness lengths in NL [m]
      TYPE (TApsGridInt),  INTENT(IN) :: z0eurgrid    ! map of roughness lengths in Europe [m]
      TYPE (TApsGridReal), INTENT(IN) :: so2bggrid    ! grid with SO2 background concentration [ppb] (read as ug/m3, converted to ppb)
      TYPE (TApsGridReal), INTENT(IN) :: no2bggrid    ! grid with NO2 background concentration [ppb] (read as ug/m3, converted to ppb)
      TYPE (TApsGridReal), INTENT(IN) :: nh3bggrid    ! grid with NH3 background concentration [ppb] (read as ug/m3, converted to ppb)
      TYPE (TApsGridReal), INTENT(IN) :: gwgrid       ! grid with gamma water values
      TYPE (TApsGridReal), INTENT(IN) :: o3bggrid     ! grids with O3 background concentration per wind sector [ug/m3]
      LOGICAL,   INTENT(IN) :: f_z0user               ! user overwrites z0 values from meteo input
      REAL,      INTENT(IN) :: z0_user                ! roughness length specified by the user [m]
      REAL,      INTENT(IN) :: z0_metreg_user         ! roughness length of user specified meteo region [m]
      INTEGER,   INTENT(IN) :: spgrid                 ! indicator for type of receptor points 
                                                      ! spgrid = 0: regular grid of receptors, NL
                                                      ! spgrid = 1: rectangular regular grid of receptors, user defined 
                                                      ! spgrid = 2: receptors at specific locations, read from file
                                                      ! spgrid = 3: receptors at user specific regular grid, not necessarily rectangular, read from file
      TYPE (TApsGridInt), INTENT(IN) :: lugrid        ! grid with land use class information (1: dominant land use, 2:NLU+1: percentages land use class)
      LOGICAL,   INTENT(IN) :: domlu                  ! use dominant land use instead of land use percentages
      LOGICAL,   INTENT(IN) :: perc                   ! 
      REAL,      INTENT(IN) :: lu_rcp_per_user_all(:) ! percentage of landuse for receptor ircp, user defined in receptor file
      INTEGER,   INTENT(IN) :: lu_rcp_dom_all         ! dominant land use class for each receptor point
      real,      intent(in) :: parout_disx            ! source receptor distance used when computing parout variables
      LOGICAL,   intent(in) :: parout_write           ! write parout parameters to output
      LOGICAL,   INTENT(IN) :: mindist, maxdist       ! indicator whether results for receptors starting from mindist/up to maxdist will be calculated
      INTEGER,   INTENT(IN) :: icm                    ! component number used in OPS 
                                                      ! 1 SO2 
                                                      ! 2 NO2 
                                                      ! 3 NH3 
                                                      ! 24 PM
      INTEGER,   INTENT(IN) :: iopt_vchem             ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
      TYPE(Tvchem), INTENT(IN) :: vchem_emep          ! grids with EMEP precursor mass and converted mass for computing chemical conversion rates                 
      REAL,      INTENT(IN) :: vchemc                 ! chemical conversion rate, independent of light [%/h]
      REAL,      INTENT(IN) :: vchemv                 ! chemical conversion rate, dependent on light [%/h]
      INTEGER,   INTENT(IN) :: dv                     ! maximum code diurnal emission variation dverl
      REAL,      INTENT(IN) :: amol1                  ! molar mass primary component [g/mol]
      REAL,      INTENT(IN) :: amol2                  ! molar mass secondary component [g/mol]
      REAL,      INTENT(IN) :: amol21                 ! (molar mass secondary component)/(molar mass primary component) [-] 
      REAL,      INTENT(IN) :: ar                     ! proportionality constant in relation [OH] = ar Qr, with Qr = global radiation in W/m2 [(cm3 m2)/(molec W2)], see Egmond en Kesseboom (1983)
      REAL,      INTENT(IN) :: r_no2_nox_sec(:)       ! sector averaged NO2/NOx ratio according to vdHout parameterisation [-]
      REAL,      INTENT(IN) :: r_no2_nox_season       ! component of NO2/NOx ratio which is season dependent  
      REAL,      INTENT(IN) :: ecvl(:, :, :)          ! average diurnal emission variation for each stability/distance class
      INTEGER,   INTENT(IN) :: iseiz                  ! season index (0=long term; 1=year; 2=winter; 3=summer; 4=month in winter; 5=month in summer)
      REAL,      INTENT(IN) :: zf                     ! interpolation factor between summer and winter (zf << "zomer fractie" = summer fraction)
      REAL,      INTENT(IN) :: trafst(:)              ! travel distances for each distance class [m]
      INTEGER,   INTENT(IN) :: knatdeppar             ! choice for parameterisation wet deposition
                                                      ! knatdeppar = 1: read wdeppar = scavenging coefficient [%/h]
                                                      ! knatdeppar = 2: read wdeppar =  scavenging ratio, i.e. average ratio if rainwater concentrations and air concentration [-]
                                                      ! knatdeppar = 3: if secondary components are present [SO2, NO2, NH3]. Wash-out and rain-out parameters are fixed in the OPS code (ops_init) [-]
      INTEGER,   INTENT(IN) :: mb                     ! start month of meteo statistics period ("b" << begin = start) 
      REAL,      INTENT(IN) :: ugmoldep               ! conversion factor from ug/m2/h to each of the deposition units in DEPUNITS 
      REAL,      INTENT(IN) :: dg                     ! diffusion coefficient in air [cm^2/s] 
      LOGICAL,   INTENT(IN) :: irev                   ! TRUE for reversible wash-out 
      REAL,      INTENT(IN) :: scavcoef               ! scavenging rate [%/h] 
      REAL,      INTENT(IN) :: koh                    ! second order reaction rate constant of reaction NO2 + OH -> HNO3 [cm3/(molec s)]
      REAL,      INTENT(IN) :: croutpri               ! (wash-out + rain-out) ratio primary component, default value without correction for background concentration, season, stability class [-]
                                                      ! icm = icm_SO2 (SO2): croutpri = wash out ratio at an N/S ratio of 1
                                                      ! icm = icm_NO2 (NOx): croutpri = wash out ratio at an NO2/NOx ratio of 1
                                                      ! icm = icm_NH3 (NH3): croutpri = wash out ratio (no correction)
      REAL,      INTENT(IN) :: rc_no                  ! canopy resistance Rc for NO [s/m]
      REAL,      INTENT(IN) :: rhno2                  ! ratio [HNO2]/[NOx] 
      REAL,      INTENT(IN) :: rc_hno3                ! canopy resistance Rc for HNO3 [s/m] 
      LOGICAL,   INTENT(IN) :: isec                   ! TRUE if component is either SO2, NOx or NH3 (and a secondary component is present)
      LOGICAL,   INTENT(IN) :: idep                   ! TRUE if deposition is modelled
      LOGICAL,   INTENT(IN) :: gasv                   ! TRUE if component is a gas
      INTEGER,   INTENT(IN) :: intpol                 ! 
      TYPE(Tdo_proc), INTENT(IN) :: do_proc           ! options to switch on/off specific processes

      REAL,      INTENT(IN) :: frac                   ! fraction of grid cell inside NL 
      REAL,      INTENT(IN) :: nh3bg_rcp              ! NH3 background concentration at receptor (used in DEPAC) [ug/m3]  
      REAL,      INTENT(IN) :: gw_rcp                 ! gamma water at receptor location (used in DEPAC)
      REAL,      INTENT(IN) :: o3bg_rcp(:)            ! O3 background concentration at receptor for all wind sectors [ug/m3]  
      REAL,      INTENT(IN) :: so2bg_rcp              ! SO2 background concentration at receptor(used in DEPAC) [ug/m3]  																																	   
      REAL,      INTENT(IN) :: rhno3_rcp              ! ratio [HNO3]/[NO3]_total at receptor points, [NO3]_total = [HNO3] + [NO3_aerosol] 
      REAL,      INTENT(IN) :: bqrv(:)                ! source strength of space heating source (rv << "ruimteverwarming" = space heating) [g/s]
      REAL,      INTENT(IN) :: bqtr(:)                ! source strength of traffic source [g/s]
      REAL,      INTENT(IN) :: bdiam(:)               ! source diameter [m]; if bdiam < 0 -> circular source, bdiam > 0 -> square sourc 
      REAL,      INTENT(IN) :: bsterkte(:)            ! source strength [g/s] 
      REAL,      INTENT(IN) :: bwarmte(:)             ! heat content of source [MW] 
      REAL,      INTENT(IN) :: bhoogte(:)             ! source height [m] 
      REAL,      INTENT(IN) :: bsigmaz(:)             ! spread in source height to represent different sources in a area source; 
                                                      ! also used for initial sigma_z (vertical dispersion) of emission (e.g. traffic, building influence) [m] 
      REAL,      INTENT(IN) :: bD_stack(:)            ! diameter of the stack [m]
      REAL,      INTENT(IN) :: bV_stack(:)            ! exit velocity of plume at stack tip [m/s]
      REAL,      INTENT(IN) :: bTs_stack(:)           ! temperature of effluent from stack [K]            
      LOGICAL,   INTENT(IN) :: bemis_horizontal(:)    ! horizontal outflow of emission
      type(Tbuilding), INTENT(IN) :: bbuilding(:)     ! structure with building parameters
      type(TbuildingEffect), INTENT(IN) :: buildingEffect ! structure containing building effect tables
      INTEGER,   INTENT(IN) :: btgedr(:)              ! temporal behaviour of sources (tgedr << "tijdsgedrag"== temporal behaviour)[-] 
      INTEGER,   INTENT(IN) :: bdegr(:)               ! option for particle size distribution
                                                      ! bdegr >= 0 -> standard particle size distribution pmd
                                                      ! bdegr  < 0 -> user-defined particle size distribution uspmd 
      INTEGER,   INTENT(IN) :: bcatnr(:)              ! emission category number
      INTEGER,   INTENT(IN) :: nemcat_road            ! number of road emission categories (for vdHout NO2/NOx ratio)
      INTEGER,   INTENT(IN) :: emcat_road(:)          ! list of road emission categories (for vdHout NO2/NOx ratio)
	  LOGICAL,   INTENT(IN)	:: road_chem					 !switch for road chemistry GTHO
	  LOGICAL,   INTENT(IN)	:: road_disp                  ! TRUE if user wants OPS to interpret sigz0 as SRM2 initial spread

      REAL,      INTENT(IN) :: so2sek(:)              ! coefficient in correction factor for SO2 background concentration for each wind direction sector; derived from 24 regional LML stations over 2003
      REAL,      INTENT(IN) :: no2sek(:)              ! coefficient in correction factor for NO2 background concentration for each wind direction sector; derived from 15 regional LML stations over 2004
      INTEGER,   INTENT(IN) :: maxidx                 ! max. number of particle classes (= 1 for gas)
      REAL,      INTENT(IN) :: pmd(:,:)               ! standard particle size distributions 
      REAL,      INTENT(IN) :: uspmd(:,:)             ! user-defined particle size distributions 
      REAL,      INTENT(IN) :: grid                   ! grid resolution [m] 
      LOGICAL,   INTENT(IN) :: subbron                ! whether to create "subbrons" (sub-sources inside a area source) and "subareas" (sub receptors inside a grid cell) or not  
      REAL,      INTENT(IN) :: routsec                ! in-cloud (rain-out) scavenging ratio for secondary component
      REAL,      INTENT(IN) :: rc_user                ! canopy resistance specified by user in control file [s/m]
      INTEGER,   INTENT(IN) :: nparout                ! number of extra output parameters (besides concentration, deposition)
      LOGICAL,   INTENT(IN) :: class_output           ! indicator whether results for receptors will be stored per wind sector/distance/particle/stability class
      REAL,      INTENT(IN)    :: routpri             ! in-cloud (rain-out) scavenging ratio for primary component [-]  





      ! ---------------------- (in)outputs which are only touched when parout_write=.true. -----------------------
      !                                -  nrrcp == 1 in such cases -
      integer,   intent(inout) :: bnr(:)              ! source number
      integer,   intent(inout) :: bx(:), by(:)        ! (x,y)-coordinates of source [m RDM]
      CHARACTER(len=*), INTENT(OUT) :: parout_name(:), parout_unit(:)  ! names and units of extra output parameters                      
      ! ---------------------- end of (in)outputs which are only touched when parout_write=.true. -----------------

      ! --------------  Only modified if intpol == 0, in which case it is completely written anew -----------------
      ! ----------------------            NB: we have no test with intpol==0.          ----------------------------
      REAL,   TARGET, INTENT(IN) :: astat_in(:,:,:,:)      
      REAL,      INTENT(IN) :: uurtot_in              ! total number of hours from meteo statistics
      ! -----------  end of: Only modified if intpol == 0, in which case it is completely written anew ------------


      ! Reduction with .or.
      LOGICAL,   INTENT(INOUT) :: lroad_corr_present  ! at least one road with vdHout correction is present

      ! Reduction with plus
      DOUBLE PRECISION,      INTENT(INOUT) :: somvnsec(:)         ! summed wet deposition flux secondary component [ug/m2/h] 
      DOUBLE PRECISION,      INTENT(INOUT) :: telvnsec(:)         ! summed deposited mass per area for wet deposition of secondary component [ug/m2]
      DOUBLE PRECISION,      INTENT(INOUT) :: vvchem(:)           ! summed chemical conversion rate [%/h] 
      DOUBLE PRECISION,      INTENT(INOUT) :: vtel(:)             ! weighing factors for averaging vvchem (i.e. deposited mass)
      DOUBLE PRECISION,      INTENT(INOUT) :: somvnpri(:)         ! summed wet deposition flux primary component [ug/m2/h] 
      DOUBLE PRECISION,      INTENT(INOUT) :: telvnpri(:)         ! summed deposited mass per area for wet deposition of primary component [ug/m2]
      DOUBLE PRECISION,      INTENT(INOUT) :: sdrypri(:)          ! summed dry deposition of primary component [ug/m2/h]
      DOUBLE PRECISION,      INTENT(INOUT) :: snatpri(:)          ! summed wet deposition of primary component [ug/m2/h]  (<< "nat" = wet)
      DOUBLE PRECISION,      INTENT(INOUT) :: sdrysec(:)          ! summed dry deposition of secondary component [ug/m2/h]
      DOUBLE PRECISION,      INTENT(INOUT) :: snatsec(:)          ! summed wet deposition of secondary component [ug/m2/h]  (<< "nat" = wet)


      ! The following results are per-receptor results:
      INTEGER,   INTENT(INOUT) :: nsrc_class(:,:,:)   ! number of sources present in wind/distance sector (-classoutput only) [-]
      REAL,      INTENT(INOUT) :: cnox_sec(:)         ! wind sector averaged NOx concentration (roads only) [ug/m3]
      REAL,      INTENT(INOUT) :: cno2                ! NO2 concentration (derived from NOx and parameterised ratio NO2/NOx) [ug/m3]
      REAL,      INTENT(INOUT) :: percvk_sec(:)       ! frequency of occurrence of wind sector (roads only) [-]
      INTEGER,   INTENT(INOUT) :: nsrc_sec(:)         ! number of sources present in wind sector (roads only) [-]
      REAL,      INTENT(INOUT) :: parout_val(:)       ! values for extra output parameters, for current receptor
      DOUBLE PRECISION, INTENT(INOUT) :: ddepri(:)    ! dry deposition of primary component at receptor point ircp [mol/ha/y] 
      DOUBLE PRECISION, INTENT(INOUT) :: wdepri(:)    ! wet deposition of primary component at receptor point ircp [mol/ha/y] 
      DOUBLE PRECISION, INTENT(INOUT) :: cpri(:)      ! concentration of primary component at receptor point ircp and height zm [ug/m3] 
      DOUBLE PRECISION, INTENT(INOUT) :: cpri_class(:,:,:,:) ! concentration of primary component at receptor points and height zm, per class [ug/m3]
      DOUBLE PRECISION, INTENT(INOUT) :: percvk_class(:,:,:) ! percvk of primary component at receptor points and height zm, per class [factor of occurrence] 
      DOUBLE PRECISION, INTENT(INOUT) :: csec(:)      ! concentration of secondary component ar receptor point ircp [ug/m3] 
      DOUBLE PRECISION, INTENT(INOUT) :: drydep(:)    ! dry deposition at receptor point ircp [mol/ha/y] 
      DOUBLE PRECISION, INTENT(INOUT) :: wetdep(:)    ! wet deposition at receptor point ircp ["depeh"] 
      REAL,      INTENT(OUT)   :: precip              ! total precipitation per year [mm/year]
      ! ---- end of per-receptor results

      TYPE (TError), INTENT(INOUT) :: error  

      INTEGER   :: ierr            ! error code for array allocation
      REAL      :: z0_metreg_rcp   ! roughness length at receptor; interpolated from meteo regions [m]
      REAL      :: lu_rcp_per(NLU) ! percentages of landuse classes at receptor points
      INTEGER   :: lu_rcp_dom      ! dominant landuse class for receptor
      REAL      :: z0_rcp          ! roughness length at receptor; from z0-map [m]
      INTEGER   :: mmm
      REAL      :: z0_src          ! roughness length at source; from z0-map [m]
      real      :: mass_prec_tra   ! column averaged mass of precursor pre chemistry step, average between source - receptor [ug/m2]
      real      :: mass_conv_dtfac_tra ! (100/dt) * column averaged mass, converted during chemistry step, average between source - receptor [(ug/m2) (%/h)]
      REAL      :: z0_tra          ! roughness length representative for trajectory [m]
      LOGICAL   :: reken
      REAL      :: so2bgtra        ! SO2 background concentration, trajectory averaged [ppb]
      REAL      :: no2bgtra        ! NO2 background concentration, trajectory averaged [ppb]
      REAL      :: nh3bgtra        ! NH3 background concentration, trajectory averaged [ppb]
      REAL      :: gwtra           ! gamma water, trajectory averaged
      REAL      :: o3bgtra(NSEK)   ! O3 background concentration, trajectory averaged [ppb]
      REAL      :: lu_tra_per(NLU) ! percentages of landuse classes over trajectorie
      REAL      :: uurtot          ! total number of hours from meteo statistics
      REAL,   save:: dispg(NSTAB) &! dispersion coefficients for vertical dispersion; sigma_z = dispg*x^disph [-]
              = (/ .28,.28,.20,.20,.12,.20 /)
      REAL,   POINTER :: astat(:,:,:,:)      
      if (intpol==0) then
         allocate(astat(NTRAJ, NCOMP, NSTAB, NSEK), stat=ierr)
         CALL AllocError(ierr, ROUTINENAAM, 12, 'thread-astat', error)
      else
         astat => astat_in
      end if
      uurtot = uurtot_in
      lu_tra_per = 0
      ! Retrieve z0 and landuse values for this receptorpoint:
      CALL ops_rcp_char_1 (isec, ircp, intpol, gxm_rcp, gym_rcp, cs, z0_metreg, xreg, yreg, astat, &
                        &  z0_metreg_user, spgrid, x_rcp, y_rcp, lugrid, domlu, perc, &
                        &  lu_rcp_per_user_all, lu_rcp_dom_all, f_z0user, z0_rcp_all, uurtot, &
                        &  z0_metreg_rcp, lu_rcp_per, lu_rcp_dom, z0_rcp, error)
      IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program
      
      ! Define a circle of NSEK sources around a receptor point, used for writing parout-variables:
      IF (parout_write) call ops_parout_circle(x_rcp, y_rcp, parout_disx, bnr, bx, by)

      ! Loop over nsbuf sources in the buffer ++++++++++++++++++++++++
      DO mmm = 1, nsbuf
      
         ! Check if we should calculate this source <-> receptor due to a minimum or maximum distance or not
         CALL ops_check_reken(reken, mindist, maxdist, x_rcp, y_rcp, bx(mmm), by(mmm))
      
         IF (reken) THEN
            !
            ! compute source characteristics
            !
            CALL ops_src_char (f_z0user, z0_user, bx(mmm), by(mmm), z0nlgrid, z0eurgrid, z0_src, error)
            IF (error%haserror) THEN
               CALL ErrorParam('source number',mmm,error)
               GOTO 9999 ! GOTO error handling at end of program
            ENDIF
            !
            ! compute trajectory characteristics
            !
            CALL ops_tra_char (icm, iopt_vchem, f_z0user, z0_user, x_rcp, y_rcp, bx(mmm), by(mmm), &
                            &  lugrid, z0nlgrid, z0eurgrid, so2bggrid, no2bggrid, nh3bggrid, gwgrid, o3bggrid, vchem_emep, domlu, &
                            &  mass_prec_tra, mass_conv_dtfac_tra, z0_tra, &
                            &  lu_tra_per, so2bgtra, no2bgtra, nh3bgtra, gwtra, o3bgtra, error)
            IF (error%haserror) THEN
               CALL ErrorParam('source number',mmm,error)
               CALL ErrorParam('receptor number',ircp,error)
               CALL ErrorParam('receptor name',trim(namrcp),error)
               GOTO 9999 ! GOTO error handling at end of program
            ENDIF
            ! Compute contribution of current emission source to concentration and deposition at current receptor:
            CALL ops_reken(varin, do_proc, iter, niter, idep, isec, icm, gasv, vchemc, iopt_vchem, vchemv, dv, amol1, amol2, amol21, ar, &
                        &  r_no2_nox_sec, r_no2_nox_season, ecvl(:,:,:max(3,dv+3,dv+btgedr(mmm))), iseiz, zf, &
                        &  trafst, knatdeppar, mb, ugmoldep, dg, irev, scavcoef, koh, croutpri, rc_no, rhno2, rc_hno3, &
                        &  ircp, gxm_rcp, gym_rcp, x_rcp, y_rcp, zm, &
                        &  frac, nh3bg_rcp, gw_rcp, o3bg_rcp, so2bg_rcp, rhno3_rcp, &
                        &  bqrv(mmm), bqtr(mmm), bx(mmm), by(mmm), bdiam(mmm), bsterkte(mmm), bwarmte(mmm), bhoogte(mmm), &
                        &  bsigmaz(mmm), bD_stack(mmm), bV_stack(mmm), bTs_stack(mmm), bemis_horizontal(mmm), bbuilding(mmm), &
                        &  buildingEffect,btgedr(mmm), bdegr(mmm), bcatnr(mmm), nemcat_road, road_chem, road_disp, emcat_road, &
                        &  z0_src, z0_tra, z0_rcp, z0_metreg_rcp, lu_tra_per, &
                        &  lu_rcp_per, so2sek, no2sek, so2bgtra, no2bgtra, nh3bgtra, gwtra, o3bgtra, &
                        &  mass_prec_tra, mass_conv_dtfac_tra, &
                        &  maxidx, pmd, uspmd, spgrid, grid, &
                        &  subbron, uurtot, routsec, rc_user, lroad_corr_present, somvnsec, telvnsec, vvchem, vtel, somvnpri, &
                        &  telvnpri, ddepri, wdepri, sdrypri, snatpri, sdrysec, snatsec, &
                        &  cpri, cpri_class, percvk_class, nsrc_class, class_output, &
                        &  csec, drydep, wetdep, &
                        &  astat, cnox_sec, cno2, &
                        &  percvk_sec, nsrc_sec, precip, routpri, dispg, &
                        &  nparout, parout_val, parout_name, parout_unit, parout_write, error)
      
            IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program
         ENDIF ! endif reken == .true.
      ENDDO   ! end loop over sources in buffer
      if (intpol==0) deallocate(astat, stat=ierr)
      RETURN
9999  continue
      CALL ErrorCall(ROUTINENAAM, error)
   end subroutine ops_receptor_loop
end module m_ops_receptor_loop
