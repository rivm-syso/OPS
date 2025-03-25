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
!
! DESCRIPTION
!
! Program OPS-LT: Long Term version of "Operationeel Prioritaire Stoffen" model
! (operationeel = operational, prioritaire stoffen = priority substances)
! 
! This program has been derived from the TREND model at April 28, 1987
! 
! English version: Januari 30, 1990
! 
! The OPS-model is a long-term Lagrangian transport and deposition model that describes
! relations between individual sources or source areas, and individual receptors. The model is statistical in
! the sense that concentration and deposition values are calculated for a number of typical situations 
! (distance/meteo classes) and the long-term value is obtained by summation of these values, weighed with 
! their relative frequencies. All relations governing the transport and deposition process are solved analytically,
! allowing the use of nongridded receptors and sources, and variable grid sizes.
! 
! Meteo data for the requested period are read from a meteo statistics file, which has been prepared before (meteo-preprocessor).
! In this meteo statistics file, there are four distance classes: 0-100, 100-300, 300-1000 and >1000 km, 6 stability classes 
! (neutral/stable/unstable, each with a relatively high or low mixing height) and 12 wind sectors of 30 degrees.
! 
! OPS computes long term (1 month to 10 year) concentration and deposition at receptor points, either in a regularly spaced grid,
! or at user specified locations, e.g. locations of measuring stations. 
!
! 2012-01-24, documentation added; also references to OPS-report. In close cooperation
! with Hans van Jaarsveld.
! DISCLAIMER: although care has been taken to make the documentation as clear as possible,
! it should be noted that documentation has been added some 20 years after the start of the model. 
! This means that not all references have been resolved and that in some cases, source code
! may have been misinterpreted. 
!
!=======================
! Structure of ops_main
!=======================
! 1. Initialisation
!    ops_init_all: initialisation (read control file, initialise file names, emissions, meteo, receptors, background 
!                  concentrations, allocate memory, etc)
!-------------------------------------------------------------------------------------------------------------------------
! 2. Computation of concentration/deposition at all receptors, contributed from all emission sources
!    start loop over source data blocks of length LSBUF (until end-of-file of scratch file with source data)
!    |
!    | ops_bron_rek         : read source characteristics from scratch file
!    | Loop over all receptor points
!    | | ops_rcp_char_1     : compute receptor characteristics for current receptor
!    | | Loop over nsbuf sources in the buffer
!    | | | ops_src_char     : compute source characteristics
!    | | | ops_tra_char     : compute trajectory characteristics
!    | | | ops_reken        : compute contribution of current emission source to concentration and deposition at current receptor
!    | | - end loop over sources in buffer
!    | -  end loop over receptors
!    -  end loop over source data blocks
!    dealloc                : Deallocate memory
!
!-------------------------------------------------------------------------------------------------------------------------  
! 3. Postprocessing and finalisation -> moved to m_ops_save_results
!    Allocate memory for variables that are used for different output purposes and initialise them to zero
!    Loop over particle size classes that are relevant for producing output fields:
!    | Get output fields
!    | ops_outp_prep           : Compute variables used in different outputs
!    | ops_calc_stats          : Compute (grid) statistics
!    | Open print output file (= PRNFILE in control file)
!    | ops_print_recep2        : print output for receptors
!    | ops_print_grid          : print output for gridded receptors
!    | Open plot file (= PLTFILE in control file)
!    | ops_plot_uitv           : write data to PLTFILE
!    - end loop over particle size classes for which to produce output fields
!    ops_print_info            : Write additional data to print file (= PRNFILE in control file)
!    deallocation and error handling
!-------------------------------------------------------------------------------------------------------------------------------
PROGRAM ops_main

#if (SKIP_OPENMP != true)
use omp_lib, only: omp_get_thread_num, omp_get_max_threads, omp_set_num_threads
#endif
use m_ops_receptor_loop, only: ops_receptor_loop
use m_ops_varin
use m_ops_building
use m_aps
use m_utils
use m_fileutils
use m_error
use m_commonconst_lt
use m_commonfile
use m_ops_vchem
use m_ops_logfile
use m_ops_gen_rcp
use m_ops_get_arg, only: TKwargs
use m_ops_read_ctr, only: TCtrLayers
use m_ops_output_lt
use m_ops_landuse
use m_ops_init
use m_ops_bron_rek
use m_ops_rcp_char_1
use m_ops_src_char
use m_ops_tra_char
use m_ops_reken
use m_ops_check_reken
use m_ops_write_progress
use m_ops_tdo_proc, only: Tdo_proc
use m_ops_save_results

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_main')

! LOCAL VARIABLES
INTEGER                                          :: numbron
INTEGER                                          :: landmax
INTEGER                                          :: nbron
INTEGER                                          :: nsbuf


type(TbuildingEffect)                            :: buildingEffect             ! structure with building effect tables
INTEGER                                          :: jb
INTEGER                                          :: mb
INTEGER                                          :: idb
INTEGER                                          :: jt
INTEGER                                          :: mt
INTEGER                                          :: idt
INTEGER                                          :: dv
INTEGER                                          :: iseiz
INTEGER                                          :: iter                       ! iteration counter
REAL,      PARAMETER                             :: iter_epsr = 0.01           ! convergence criterion; if relative difference between iterands < iter_epsr then convergance
INTEGER                                          :: niter                      ! maximal number of iterations
INTEGER                                          :: nrrcp                      ! number of receptors
INTEGER                                          :: ircp
INTEGER                                          :: ithread                    ! index of thread for parallel computing
INTEGER                                          :: nthreads                   ! number of threads for parallel computing
INTEGER                                          :: ndone                      ! keep track of number of finished calculations
INTEGER                                          :: ntotal                     ! the number of total calculations needed
CHARACTER(LEN = 512)                             :: dir_chem                   ! directory where to read chemistry files from
CHARACTER(LEN = 512)                             :: fnames_used_chem           ! string with names of files used for chemistry maps
character(len=512) :: dir_bg_actual
INTEGER                                          :: memdone
INTEGER                                          :: maxidx                     ! max. number of particle classes (= 1 for gas)
REAL                                             :: aind                       ! hourglass (rough indicator of % of computing time spent)
REAL                                             :: amol2
REAL                                             :: amol21
REAL                                             :: z0_metreg_user             ! roughness length of user specified meteo region [m]
REAL                                             :: rc_user                    ! canopy resistance specified by user in control file [s/m]
REAL                                             :: ugmoldep
REAL                                             :: gemre
REAL                                             :: gemcpri
REAL                                             :: gemcsec
REAL                                             :: totddep
REAL                                             :: gemddep
REAL                                             :: gemddpri
REAL                                             :: gemddsec
REAL                                             :: ddrpri
REAL                                             :: ddrsec
REAL                                             :: totwdep
REAL                                             :: gemwdep
REAL                                             :: gemwdpri
REAL                                             :: gemwdsec
REAL                                             :: wdrpri
REAL                                             :: wdrsec
REAL                                             :: tottdep
REAL                                             :: gemtdep
REAL                                             :: gemprec
REAL                                             :: ccr
REAL                                             :: xul_cell_centre           ! x-coordinate of centre of upper-left grid cell [m]
REAL                                             :: yul_cell_centre           ! y-coordinate of centre of upper-left grid cell [m],
REAL                                             :: conc_cf
REAL                                             :: ar
REAL                                             :: r_no2_nox_season          ! component of NO2/NOx ratio which is season dependent
REAL                                             :: uurtot                    ! total number of hours from meteo statistics
REAL                                             :: zf
REAL                                             :: scavcoef
REAL                                             :: routsec                    ! in-cloud scavenging ratio for secondary component
                                                                               ! (rout << rain-out = in-cloud) [-]
REAL                                             :: routpri                    ! in-cloud scavenging ratio for primary component
                                                                               ! (rout << rain-out = in-cloud) [-]
REAL                                             :: croutpri                   ! constant (initial) in-cloud scavenging ratio [-] for primary component
REAL                                             :: rc_no                      ! canopy resistance NO (set at 'high' value) [s/m]
REAL                                             :: rhno2
REAL                                             :: rc_hno3                    ! canopy resistance HNO3 [s/m]

INTEGER,   allocatable :: btgedr(:), bdegr(:), bcatnr(:), blandnr(:), bx(:), by(:), bnr(:)
REAL,   allocatable :: bdiam(:), bsterkte(:), bwarmte(:), bhoogte(:), bsigmaz(:), emis(:,:), &
                       astat(:,:,:,:), o3bgtra(:), trafst(:), bqrv(:), bqtr(:), &
                       cs(:, :, :, :, :), xreg(:), yreg(:), ecvl(:,:,:), &
                       pmd(:,:), uspmd(:,:)
LOGICAL, allocatable         :: bemis_horizontal(:)   ! horizontal outflow of emission
type(Tbuilding), allocatable :: bbuilding(:)          ! array with structures with building parameters
REAL,   allocatable          :: lu_rcp_per(:)         ! percentages of landuse classes at receptor points
REAL,   allocatable          :: bD_stack(:)           ! diameter of the stack [m]
REAL,   allocatable          :: bV_stack(:)           ! exit velocity of plume at stack tip [m/s]
REAL,   allocatable          :: bTs_stack(:)          ! temperature of effluent from stack [K]
REAL,   allocatable          :: z0_metreg(:)          ! roughness lengths of NMETREG meteo regions; scale < 50 km [m]

!
! Set coefficients in correction factors for SO2 background concentration for each wind direction sector
! derived from 24 regional LML stations over 2003 (eastern wind -> higher SO2)
!
REAL,   parameter :: so2sek(NSEK) = (/ 0.77, 0.73, 0.88, 1.09, 1.30, 1.34, 1.28, 1.14, 0.97, 0.94, 0.90, 0.77/)
!
! Set coefficients in correction factor for nO2 background concentration for each wind direction sector
! derived from 15 regional LML stations over 2004 (eastern wind -> higher NO2)
!
REAL,   parameter :: no2sek(NSEK) = (/0.81, 0.88, 1.08, 1.30, 1.33, 1.40, 1.25, 1.03, 0.83, 0.71, 0.70, 0.68/)

REAL                          :: koh
REAL,        POINTER          :: gem_subsec(:)              ! grid mean for concentration of sub-secondary species [ug/m3]
REAL                          :: scale_con
REAL                          :: scale_sec
REAL,        POINTER          :: scale_subsec(:)
REAL                          :: scale_dep
type(Tvchem)                  :: vchem_emep                 ! grids with EMEP precursor mass and converted mass for computing chemical conversion rates    
DOUBLE PRECISION,        POINTER          :: sdrypri_arr(:,:), snatpri_arr(:,:), somvnpri_arr(:,:), &
                                 telvnpri_arr(:,:), sdrysec_arr(:,:), snatsec_arr(:,:), &
                                 somvnsec_arr(:,:), telvnsec_arr(:,:), vvchem_arr(:,:), &
                                 vtel_arr(:,:)

CHARACTER*80                  :: namsec
CHARACTER*80, POINTER         :: nam_subsec(:)
CHARACTER*80                  :: nam_pri_sec                ! name of primary + secondary component (SOx, NOy, NHy)
CHARACTER*10                  :: coneh
CHARACTER*10                  :: depeh
INTEGER                       :: diag                       ! = 1,3 (argument -r) -> print version number and quit
LOGICAL                       :: verb
LOGICAL                       :: eof

TYPE(Tvarin)                  :: varin                      ! input variables for meteo
type(TKwargs) :: kwargs
type(TCtrLayers) :: ctr

LOGICAL,          allocatable                 :: lroad_corr_present(:)      ! at least one road with vdHout correction is present
INTEGER,          POINTER     :: lu_rcp_dom_all(:)          ! dominant land use class for each receptor point
INTEGER,          POINTER     :: jump(:)                    ! number of successive points that can be skipped for output purposes
REAL,             POINTER     :: xm(:)                      ! x-coordinates of receptors (m RDM)
REAL,             POINTER     :: ym(:)                      ! y-coordinates of receptors (m RDM)
REAL,             POINTER     :: zm(:)                      ! z-coordinates of receptors (m)
REAL,             POINTER     :: frac(:)                    ! fraction of output cell on land surface
REAL,             POINTER     :: lu_rcp_per_user_all(:,:)   ! percentage of landuse for all receptors, user defined in receptor file
REAL,             POINTER     :: gxm(:)                     ! x-coordinates of receptors (lon-lat) [degrees]
REAL,             POINTER     :: gym(:)                     ! y-coordinates of receptors (lon-lat) [degrees]
REAL,             POINTER     :: z0_rcp_all(:)              ! roughness lengths for all receptors; from z0-map or receptor file [m]
REAL,             POINTER     :: rhno3_rcp(:)
REAL,             ALLOCATABLE :: f_subsec_rcp(:,:)          ! fractions for sub-secondary species, HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total [-]
REAL,             POINTER     :: precip(:)                  ! total precipitation per year [mm/year]
DOUBLE PRECISION, POINTER     :: cpri_d(:,:)                ! concentration of primary component, double precision [ug/m3]
DOUBLE PRECISION, POINTER     :: cpri_class(:,:,:,:,:)      ! concentration of primary component per class, double precision [ug/m3
DOUBLE PRECISION, POINTER     :: percvk_class(:,:,:,:)      ! percvk of primary component at receptor points and height zm, per class [factor of occurrence]
INTEGER,          POINTER     :: nsrc_class(:,:,:,:)        ! number of sources present in wind/distance sector (-classoutput only) [-]
DOUBLE PRECISION, POINTER     :: csec_d(:,:)                ! concentration of secondary component, double precision [ug/m3]
DOUBLE PRECISION, POINTER     :: drydep_d(:,:)
DOUBLE PRECISION, POINTER     :: wetdep_d(:,:)
DOUBLE PRECISION, POINTER     :: ddepri_d(:,:)
DOUBLE PRECISION, POINTER     :: wdepri_d(:,:)
REAL,             POINTER     :: nh3bg_rcp(:)
REAL,             POINTER     :: gw_rcp(:)
REAL,             POINTER     :: o3bg_rcp(:,:)              ! O3 background concentration for all receptors and for each wind sector (nrrcp x NSEK) [ug/m3]
REAL,             POINTER     :: so2bg_rcp(:)
REAL,             POINTER     :: cno2(:)                    ! NO2 concentration (derived from NOx and parameterised ratio NO2/NOx)
REAL,             POINTER     :: cnox_sec(:,:)              ! wind sector averaged NOx concentration (roads only) [ug/m3]
REAL,             POINTER     :: percvk_sec(:,:)            ! frequency of occurrence of wind sector (roads only) [-]
INTEGER,          POINTER     :: nsrc_sec(:,:)              ! number of sources present in wind sector (roads only) [-]
REAL,             POINTER     :: cnox(:)                    ! NOx concentration, per receptor, for output, only for ROADS (vdHout)
REAL,             POINTER     :: r_no2_nox_sec(:,:)         ! sector averaged NO2/NOx ratio according to vdHout parameterisation [-]


! Type Tparout
!    INTEGER,   PARAMETER                             :: numval = 4          ! number of extra output parameters (besides concentration, deposition)
!    REAL,      DIMENSION(:,:), POINTER               :: val                 ! values for extra output parameters [nrrcp,nparout]
!    CHARACTER(len= 128)                              :: name(nparout)       ! names of extra output parameters
!    CHARACTER(len=  40)                              :: unit(nparout)       ! units of extra output parameters
!    LOGICAL                                          :: set = .true.        ! write parout parameters to output
!    REAL                                             :: disx = 2000.0       ! source receptor distance used for parout parameters
!    REAL                                             :: hbron = 5.0         ! emission height used for parout parameters
!    REAL                                             :: qw = 0.0            ! heat content used for parout parameters
!    
! End Type Tparout
!
INTEGER,   PARAMETER                             :: nparout = 4                ! number of extra output parameters (besides concentration, deposition)
REAL,      DIMENSION(:,:), POINTER               :: parout_val                 ! values for extra output parameters [nrrcp,nparout]
CHARACTER(len= 128)                              :: parout_name(nparout)       ! names of extra output parameters
CHARACTER(len=  40)                              :: parout_unit(nparout)       ! units of extra output parameters
LOGICAL, PARAMETER                               :: parout_write = .false.     ! write parout parameters to output
REAL,   parameter                                :: parout_disx = 50000.0      ! source receptor distance used for parout parameters [m]

INTEGER                                          :: ierr                       ! error code for array allocation

CHARACTER*12, DIMENSION(:), POINTER              :: namrcp                     ! receptor names

TYPE (TApsGridInt)                               :: z0nlgrid                   ! map of roughness lengths in NL [m]
TYPE (TApsGridInt)                               :: lugrid                     ! grid with land use class information (1: dominant land use, 2:NLU+1: percentages land use class)
TYPE (TApsGridInt)                               :: z0eurgrid                  ! map of roughness lengths in Europe [m]
TYPE (TApsGridReal)                              :: so2bggrid                  ! grid with background concentrations SO2 [ppb] (read as ug/m3, converted to ppb)
TYPE (TApsGridReal)                              :: no2bggrid                  ! grid with background concentrations NO2 [ppb] (read as ug/m3, converted to ppb)
TYPE (TApsGridReal)                              :: nh3bggrid                  ! grid with background concentrations NH3 [ppb] (read as ug/m3, converted to ppb)
TYPE (TApsGridReal)                              :: gwgrid                     ! grid with gamma water values 
TYPE (TApsGridReal)                              :: o3bggrid                   ! grids with background concentrations O3 per wind sector [ug/m3]
TYPE (TError)                                    :: error                      ! error structure
TYPE (TError), allocatable                       :: thread_error(:)            ! error structures for threads
integer                                          :: kdeel                      ! index of particle size class
integer, parameter                               :: i_reduced = 0              ! index of threads-array where to store reduced (aggregated over all threads) data 
logical                                          :: haserror                   ! global error (one of the threads has an error)

!-------------------------------------------------------------------------------------------------------------------------------
allocate( bemis_horizontal(LSBUF), bbuilding(LSBUF), btgedr(LSBUF), bdegr(LSBUF), bcatnr(LSBUF), &
          blandnr(LSBUF), bx(LSBUF), by(LSBUF), bnr(LSBUF), lu_rcp_per(NLU), bD_stack(LSBUF), &
          bV_stack(LSBUF), bTs_stack(LSBUF), bdiam(LSBUF), bsterkte(LSBUF), bwarmte(LSBUF), &
          bhoogte(LSBUF), bsigmaz(LSBUF), emis(6,NLANDMAX), astat(NTRAJ, NCOMP, NSTAB, NSEK), &
          o3bgtra(NSEK), trafst(NTRAJ), bqrv(LSBUF), bqtr(LSBUF), cs(NTRAJ, NCOMP, NSTAB, NSEK, NMETREG), &
          xreg(NMETREG), yreg(NMETREG), ecvl(NSTAB, NTRAJ,2*MAXDISTR), pmd(NPARTCLASS,MAXDISTR), &
          uspmd(NPARTCLASS,MAXDISTR), z0_metreg(NMETREG), stat=ierr)
CALL AllocError(ierr, ROUTINENAAM, 10, 'lots of arrays', error)

! Initialise (arguments alphabetically):
call ops_init_all(varin, kwargs, ctr, &
amol2, amol21, ar, astat, buildingEffect, cno2, cnox_sec, cnox, conc_cf, coneh, cpri_d, cpri_class, &
percvk_class, nsrc_class, croutpri, cs, csec_d, &
ddepri_d, depeh, dir_chem, drydep_d, dv, ecvl, emis, eof, error, fnames_used_chem, dir_bg_actual, frac, f_subsec_rcp, &
gemre, gem_subsec, gwgrid, gw_rcp, gxm, gym, idb, idt, &
iseiz, jb, jt, jump, koh, landmax, lugrid, lu_rcp_dom_all, lu_rcp_per_user_all, maxidx, &
mb, memdone, mt, nam_pri_sec, namrcp, namsec, nam_subsec, nbron,  nh3bggrid, nh3bg_rcp, niter, no2bggrid, &
nparout, nrrcp, nsrc_sec, numbron, o3bggrid, o3bg_rcp, parout_val, percvk_sec, pmd, precip, rc_hno3, rc_no, &
rc_user, rhno2, rhno3_rcp, r_no2_nox_season, r_no2_nox_sec, routpri, routsec, scale_subsec, scavcoef, sdrypri_arr, &
sdrysec_arr, snatpri_arr, snatsec_arr, so2bggrid, so2bg_rcp, somvnpri_arr, &
somvnsec_arr, telvnpri_arr, telvnsec_arr, trafst, ugmoldep, uspmd, &
uurtot, vchem_emep, verb, vtel_arr, vvchem_arr, wdepri_d, wetdep_d, xm, xreg, &
xul_cell_centre, ym, yreg, yul_cell_centre, z0eurgrid, z0_metreg, z0_metreg_user, z0nlgrid, z0_rcp_all, zf, zm)
IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program
IF (error%debug) write(*,*) 'debug output started'

! Check diag (-r option); if set -> skip rest of program:
IF (kwargs%diag == 1 .OR. kwargs%diag == 3) THEN
   GOTO 9999
ENDIF

#if (SKIP_OPENMP != true)
nthreads = omp_get_max_threads()
#else
nthreads=1
#endif
allocate(lroad_corr_present(i_reduced:nthreads), thread_error(nthreads))
lroad_corr_present = .false.
thread_error(:)%debug = error%debug

! Calculate the total number of calculations needed
ntotal = niter * numbron * nrrcp
! Keep track of number of finished calculations
ndone = 0

! Iteration is needed because we need total NOx-concentration at a receptor before we can compute the NO2/NOx ratio based on vdHout:
DO iter = 1,niter

  ! Skip iteration steps if there are no roads for vdHout correction present:
  ! if (iter > 1 .and. .not. lroad_corr_present) -> skip:
  if (iter == 1 .or. lroad_corr_present(i_reduced)) then
     ! write(*,*) 'iter = ',iter,', nemcat_road = ',nemcat_road, ', emcat_road = ',emcat_road(1:nemcat_road)

     ! Initialisation for this iteration step:
     if (niter .gt. 1) then
        call ops_init_iter(eof, nbron, cpri_d, csec_d, drydep_d, wetdep_d, ddepri_d, &
                           wdepri_d, cnox_sec, cno2, emis, parout_val, &
                           sdrypri_arr, snatpri_arr, somvnpri_arr, telvnpri_arr, sdrysec_arr, snatsec_arr, somvnsec_arr, &
                           telvnsec_arr, vvchem_arr, vtel_arr)
     endif

     ! Start loop over source data blocks of length LSBUF (until end-of-file of scratch file with selected emissions):
     DO WHILE (.NOT. eof)

        ! Read source characteristics from scratch file and fill into buffer arrays (source data are read in
        ! blocks of length LSBUF (LSBUF=4000)):
        CALL ops_bron_rek (ctr%emission%emtrend, buildingEffect, landmax, emis, nsbuf, bnr, bx, by, bdiam, bsterkte, bwarmte, bhoogte, bsigmaz, &
                           bD_stack, bV_stack, bTs_stack, bemis_horizontal, bbuilding, btgedr, &
                           bdegr, bqrv, bqtr, bcatnr, blandnr, eof, error)
        IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program
       
        ! Adjust number of processed sources
        nbron = nbron + nsbuf
        if (parout_write) then
           call ops_parout_prepare_circle(kwargs%subbron, nsbuf, nbron, numbron, bdiam, bsterkte, & 
                             bwarmte, bhoogte, bsigmaz, bD_stack, bV_stack, bTs_stack, & 
                             bemis_horizontal, bbuilding, btgedr, bdegr, bqrv, bqtr, bcatnr, blandnr)
        end if

        ! Loop over all receptor points  ++++++++++++++++++++++++
!$OMP   PARALLEL DO DEFAULT(SHARED) PRIVATE(ithread) FIRSTPRIVATE(ndone)
        DO ircp = 1, nrrcp
           if (error%debug) then
              ! Empty line, because otherwise the first writestatement is in
              ! the same line as the progress in the debug output.
              write(*,'(a)'), ''
              write(*,'(a,i6)') 'ops_main; ircp;', ircp
           endif

#if (SKIP_OPENMP != true)
           ithread = omp_get_thread_num() + 1
#else
           ithread = 1
#endif
           if (.not. thread_error(ithread)%haserror) then
              call ops_receptor_loop(varin, &
              iter, niter, &
              ircp, namrcp(ircp), nsbuf, gxm(ircp), gym(ircp), xm(ircp), ym(ircp), zm(ircp), &
              xreg, yreg, z0_rcp_all(ircp), cs, z0_metreg, &
              z0nlgrid, z0eurgrid, so2bggrid, no2bggrid, nh3bggrid, gwgrid, o3bggrid, &
              ctr%output%f_z0user, ctr%meteo_surface%z0_user, z0_metreg_user, ctr%receptor%spgrid, &
              lugrid, kwargs%domlu, kwargs%perc, lu_rcp_per_user_all(:,ircp), lu_rcp_dom_all(ircp), &
              parout_write, kwargs%mindist, kwargs%maxdist, bnr, bx, by, parout_disx, &
              ctr%substance%icm, ctr%substance%iopt_vchem, vchem_emep, ctr%substance%vchemc, &
              ctr%substance%vchemv, dv, ctr%substance%amol1, amol2, amol21, ar, &
              r_no2_nox_sec(:,ircp), r_no2_nox_season, ecvl, iseiz, zf, trafst, ctr%substance%knatdeppar, mb, &
              ugmoldep, ctr%substance%dg, ctr%substance%irrev, scavcoef, koh, &
              croutpri, routpri, rc_no, rhno2, rc_hno3, &
              ctr%output%isec, ctr%substance%idep, ctr%substance%gasv, ctr%meteo_surface%intpol, ctr%substance%do_proc, &
              frac(ircp), nh3bg_rcp(ircp), gw_rcp(ircp), o3bg_rcp(:,ircp), so2bg_rcp(ircp), rhno3_rcp(ircp), bqrv, bqtr, bdiam, &
              bsterkte, bwarmte, bhoogte, bsigmaz, bD_stack, bV_stack, bTs_stack, &
              bemis_horizontal, bbuilding, buildingEffect, btgedr, bdegr, bcatnr, &
              ctr%emission%nemcat_road, ctr%emission%road_chem, ctr%emission%road_disp, ctr%emission%emcat_road, so2sek, no2sek, &
              maxidx, pmd, uspmd, ctr%receptor%grid, kwargs%subbron, routsec, rc_user, nparout, &
              kwargs%class_output, &
              uurtot, astat, lroad_corr_present(ithread), &
              somvnsec_arr(:,ithread), telvnsec_arr(:,ithread), &
              vvchem_arr(:,ithread), &
              vtel_arr(:,ithread), somvnpri_arr(:,ithread), telvnpri_arr(:,ithread), sdrypri_arr(:,ithread), &
              snatpri_arr(:,ithread), sdrysec_arr(:,ithread), snatsec_arr(:,ithread), &
              nsrc_class(:,:,:,ircp), cnox_sec(:,ircp), cno2(ircp), percvk_sec(:,ircp), nsrc_sec(:,ircp), &
              parout_val(:,ircp), ddepri_d(:,ircp), wdepri_d(:,ircp), cpri_d(:,ircp), &
              cpri_class(:,:,:,:,ircp), percvk_class(:,:,:,ircp), csec_d(:,ircp), drydep_d(:,ircp), wetdep_d(:,ircp), &
              precip(ircp), parout_name, parout_unit, thread_error(ithread))
           end if
           ndone = ndone+nsbuf  ! always update counter even if reken was .false.

           if (ithread==1) then
              ! Write progress (update each 2%); ndone is private to each thread
              ! and the amount of calculations to be done for one thread = notal/nthreads.
              ! Progess indicator is ndone/(ntotal/nthreads) = ndone*nthreads/ntotal
              ! Note that if nrrcp < nthreads, not all threads are active, so we use min(nrrcp,nthreads)
              ! write(*,*) 'ndone,nthreads,ntotal = ',ndone,nthreads,ntotal
              aind = 100.*FLOAT(ndone*min(nrrcp,nthreads))/FLOAT(ntotal)
              CALL ops_write_progress(aind, '(F5.1)', 5, memdone)
           endif
        ENDDO  ! end loop over receptors
!$OMP   END PARALLEL DO
     ENDDO ! end loop over source data blocks of length LSBUF (until end-of-file of scratch file with source data)
 
     ! Process thread errors into global error
     haserror = .false.
     do ithread = 1,nthreads
        if (thread_error(ithread)%haserror) haserror = .true.
     end do
     if (haserror) then
        CALL SetError('Thread(s) issue error(s)', error)
        CALL ErrorCall(ROUTINENAAM, error)
        IF (sysopen(fu_err, errnam, 'w', 'error file', error) ) THEN
           do ithread = 1,nthreads
              if (.not. thread_error(ithread)%haserror) cycle
              CALL ErrorParam('thread', ithread, error)
              write(fu_err,'(100(a,i0))') 'THREAD ',ithread,' reports an error'
              CALL WriteError(fu_err, thread_error(ithread))
           end do
           CALL WriteError(fu_err, error)
           CALL sysclose(fu_err, errnam, error)
        ENDIF
        call exit(1)
     ENDIF

     ! Reductions of global sums
     lroad_corr_present(i_reduced) = any(lroad_corr_present(1:)) 
     DO kdeel = 1, maxidx
        do ithread = 1,nthreads
           somvnsec_arr(kdeel,i_reduced) = somvnsec_arr(kdeel,i_reduced) + somvnsec_arr(kdeel,ithread)
           telvnsec_arr(kdeel,i_reduced) = telvnsec_arr(kdeel,i_reduced) + telvnsec_arr(kdeel,ithread)
           vvchem_arr(kdeel,i_reduced)   = vvchem_arr(kdeel,i_reduced)   + vvchem_arr(kdeel,ithread)
           vtel_arr(kdeel,i_reduced)     = vtel_arr(kdeel,i_reduced)     + vtel_arr(kdeel,ithread)
           somvnpri_arr(kdeel,i_reduced) = somvnpri_arr(kdeel,i_reduced) + somvnpri_arr(kdeel,ithread)
           telvnpri_arr(kdeel,i_reduced) = telvnpri_arr(kdeel,i_reduced) + telvnpri_arr(kdeel,ithread)
           snatpri_arr(kdeel,i_reduced)  = snatpri_arr(kdeel,i_reduced)  + snatpri_arr(kdeel,ithread)
           sdrypri_arr(kdeel,i_reduced)  = sdrypri_arr(kdeel,i_reduced)  + sdrypri_arr(kdeel,ithread)
           sdrysec_arr(kdeel,i_reduced)  = sdrysec_arr(kdeel,i_reduced)  + sdrysec_arr(kdeel,ithread)
           snatsec_arr(kdeel,i_reduced)  = snatsec_arr(kdeel,i_reduced)  + snatsec_arr(kdeel,ithread)
        ENDDO
     ENDDO

     ! Conversion NOx -> NO2 based on vdHout:
     if (ctr%substance%do_proc%chem .and. ctr%substance%icm==icm_NOx .and. lroad_corr_present(i_reduced)) &
        call ops_vchem_ratio_no2_nox_vdhout(iter,nrrcp,o3bg_rcp,o3bgtra,nsrc_sec,cnox_sec,cno2,percvk_sec,r_no2_nox_sec,cnox)

  ENDIF ! Check whether there are roads with vdHout correction present
ENDDO ! end of iteration

! Deallocate some memory; this creates some space for output arrays:
CALL dealloc(lugrid)
CALL dealloc(z0eurgrid)
CALL dealloc(z0nlgrid)
CALL dealloc(nh3bggrid)
CALL dealloc(gwgrid)
CALL dealloc(o3bggrid)
CALL dealloc(no2bggrid)
CALL dealloc(so2bggrid)
CALL dealloc(nh3bg_rcp)
CALL dealloc(o3bg_rcp)
CALL dealloc(so2bg_rcp)					   
CALL dealloc(gxm)
CALL dealloc(gym)

! close scratch file for sources
!CALL sysclose(fu_scratch, 'sources scratch file', error)
!IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program

! store results in output files
CALL ops_save_results( &
   varin, nrrcp, ctr%output%nsubsec, jump, ctr%substance%gasv, ctr%substance%idep, ctr%substance%icm, ctr%substance%do_proc, ctr%emission%road_chem, ctr%emission%road_disp, ctr%emission%nemcat_road, kwargs%class_output, kwargs%allow_sigz0_point_source, &
   ctr%substance%namco, namsec, nam_pri_sec, &
   nam_subsec, namrcp, xm, ym, ctr%receptor%nrcol, ctr%receptor%nrrow, xul_cell_centre, yul_cell_centre, &
   precip, lu_rcp_dom_all, z0_rcp_all, ctr%meteo_surface%intpol, &
   cpri_d, cpri_class, percvk_class, nsrc_class, csec_d, drydep_d, wetdep_d, &
   ddepri_d, wdepri_d, sdrypri_arr(:,i_reduced), snatpri_arr(:,i_reduced), somvnpri_arr(:,i_reduced), &
   telvnpri_arr(:,i_reduced), sdrysec_arr(:,i_reduced), snatsec_arr(:,i_reduced), somvnsec_arr(:,i_reduced), telvnsec_arr(:,i_reduced), &
   vvchem_arr(:,i_reduced), vtel_arr(:,i_reduced), conc_cf, f_subsec_rcp, cnox, cno2, &
   scale_con, scale_sec, scale_subsec, scale_dep, frac, &
   ugmoldep, gemre, gemcpri, gemcsec, totddep, gemddep, &
   gemddpri, gemddsec, ddrpri, ddrsec, totwdep, gemwdep, &
   gemwdpri, gemwdsec, wdrpri, wdrsec, tottdep, gemtdep, gemprec, &
   ctr%receptor%grid, amol21, ctr%receptor%spgrid, ctr%identification%project, coneh, depeh, &
   nparout, parout_name, parout_val, parout_unit, parout_write, &
   ccr, gem_subsec, ctr%output%isec, ctr%output%igrid, verb, trafst, &
   nbron, bnr, bx, by, bsterkte, bqrv, bqtr, &
   bwarmte, bhoogte, bdiam, bsigmaz, btgedr, bdegr, bcatnr, blandnr, emis, ctr%emission%emtrend, &
   jb, mb, idb, jt, mt, idt, iseiz,    &
   ctr%output%f_z0user, landmax, kwargs%subbron, kwargs%domlu, kwargs%varz, kwargs%perc, kwargs%mindist, kwargs%maxdist, dir_chem, fnames_used_chem, dir_bg_actual, &
   error)

! Finally, make sure progress counter is set to 100%
CALL ops_write_progress(100.0, '(F5.1)', 5, memdone)

! 
! Close the progression file:
CALL sysclose(fu_progress, indnam, error)
IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program
!if (nthreads==1) call print_stats()

! Close log file if it is open (this is checked in ops_closelog):
CALL ops_closelog(error)

!------------------------------------------------------------------------------
! Error handling at end of program.
! Note: deallocation of memory is automatic at end of execution of program.
!------------------------------------------------------------------------------
9999 CONTINUE
IF (error%haserror) THEN

   CALL ErrorCall(ROUTINENAAM, error)

   ! Open error file and write error message to file.
   IF (sysopen(fu_err, errnam, 'w', 'error file', error) ) THEN
     CALL WriteError(fu_err, error)
     CALL sysclose(fu_err, errnam, error)
   ELSE
      write(*,*) 'error file not yet open; writing errors to stdout'
      CALL WriteError(IOB_STDOUT, error)
   ENDIF
   CALL EXIT(1)
ELSE
   CALL EXIT(0)
ENDIF

END PROGRAM ops_main
