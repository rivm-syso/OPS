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

module m_ops_init

! Module for initialisation
! Subroutines:
! ops_init_all : all initialisations (call a.o. ops_init)
! ops_init     : initialisation of variables
! ops_init_iter: initialise summed quantities at start of iteration step

implicit none

contains

!--------------------------------------------------------------------------
subroutine ops_init_all( varin, kwargs, ctr, &
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

! Perform all initialisations needed for OPS-LT.
#if (SKIP_OPENMP != true)
use omp_lib, only: omp_get_max_threads
#endif
use m_ops_varin
use m_ops_building
use m_error
use m_aps
use m_ops_vchem
use m_ops_get_arg, only: TKwargs, ops_get_arg
use m_ops_read_ctr
use m_ops_gen_fnames
use m_ops_read_emis
use m_ops_read_meteo
use m_ops_read_bg, only: ops_read_bg
use m_ops_get_dim
use m_ops_rcp_char_all
use m_ops_write_progress
use m_ops_tdo_proc, only: Tdo_proc

! ops_get_arg            : Read program arguments
! MakeMonitorNames       : Make file names for process monitoring
! alloc                  : Allocate memory for catsel, emcat_road and landsel
! ops_read_ctr           : Read variables from control file
! ops_gen_fnames         : Generate full file names
! ops_read_emis          : Read source (emission) file and copy selected sources to scratch
! ops_building_file_names: Set file names for building effect tables
! ops_read_meteo         : Read meteo statistics
! ReadAps                : Read roughness length (z0) grids for NL and Europe and land use values
! ops_read_bg            : Read background concentrations and other chemical maps
! ops_get_dim            : Determine grid dimensions nrcol and nrrow
! alloc, allocate        : Allocate memory for xm, ym, zm, frac, jump, namrcp, z0_rcp_all, lu_rcp_dom_all
! ops_gen_rcp            : Generate receptor grid
! ops_init               : Initialisation
! alloc                  : Allocate miscellaneous arrays for receptor points
! ops_rcp_char_all       : Fill arrays with roughness length, landuse and rhno3_rcp, nh3bg_rcp, o3bg_rcp, f_subsec_rcp, domlu for all receptor points
! alloc                  : Allocate other arrays for receptor points and help arrays for particles
! Zeroise summed parameters, set maxidx = number of particle classes

! Used variables:
use m_commonfile,      only: IOB_STDOUT, indnam, fu_progress
use m_commonconst_lib, only: NTRAJ, NCOMP, NSTAB, NSEK
use m_commonconst_lt,  only: icm_NOx, icm_SO2, icm_NH3
use m_commonconst_lt,  only: MODVERSIE, RELEASEDATE
use m_commonconst_lt,  only: NCATMAX, NLANDMAX, NPARTCLASS, NMETREG, MAXDISTR, NHRBLOCKS
use m_commonconst_lt,  only: MISVALNUM
use m_commonconst_lib, only: NLU

! Subroutines called:
use m_utils,           only: alloc, AllocError, get_version_utils
use m_commonfile,      only: get_version_core, MakeMonitorNames
use m_fileutils,       only: sysopen
use m_ops_landuse,     only: ops_read_z0_landuse
use m_ops_gen_rcp,     only: ops_gen_rcp

! Variables:
! - check whether variable is used only in initialisation part; if so -> local variable -> move declaration from ops_main to here;
!   if not -> output variable; copy declaration from ops_main and include intent(out)
!             except for variables in m_commonconst or other used files

! Constant:
CHARACTER*512                                    :: ROUTINENAAM                !
PARAMETER    (ROUTINENAAM = 'ops_init_all')

! Subroutine arguments (alphabetically ordered)
! Input:
INTEGER,   INTENT(IN)                            :: nparout                    ! number of extra output parameters (besides concentration, deposition)

! Output
type(TKwargs), intent(out) :: kwargs
type(TCtrLayers), intent(out) :: ctr
TYPE(Tvarin), INTENT(OUT)                        :: varin                      ! input variables
REAL,      INTENT(OUT)                           :: amol2
REAL,      INTENT(OUT)                           :: amol21
REAL,      INTENT(OUT)                           :: ar
REAL,      INTENT(OUT)                           :: astat(NTRAJ, NCOMP, NSTAB, NSEK)
type(TbuildingEffect), INTENT(OUT)               :: buildingEffect             ! structure with building effect tables
REAL,      DIMENSION(:), POINTER, INTENT(OUT)    :: cno2                       ! NO2 concentration (derived from NOx and parameterised ratio NO2/NOx)
REAL,      DIMENSION(:,:),POINTER, INTENT(OUT)   :: cnox_sec                   ! wind sector averaged NOx concentration (roads only) [ug/m3]
REAL,      DIMENSION(:),POINTER, INTENT(OUT)     :: cnox                       ! NOx concentration, per receptor, for output
REAL,      INTENT(OUT)                           :: conc_cf
CHARACTER*10, INTENT(OUT)                        :: coneh
DOUBLE PRECISION,    DIMENSION(:,:), POINTER, INTENT(OUT)  :: cpri_d           ! concentration of primary component, double precision [ug/m3]
DOUBLE PRECISION,    DIMENSION(:,:,:,:,:), POINTER, INTENT(OUT)  :: cpri_class ! concentration of primary component per class, double precision [ug/m3]
DOUBLE PRECISION,    DIMENSION(:,:,:,:), POINTER, INTENT(OUT)  :: percvk_class ! percvk of primary component at receptor points and height zm, per class [factor of occurrence]
INTEGER, DIMENSION(:,:,:,:), POINTER, INTENT(OUT)              :: nsrc_class                 ! number of sources present in wind/distance sector (-classoutput only) [-]
REAL,      INTENT(OUT)                           :: croutpri                   ! constant (initial) in-cloud scavenging ratio [-] for primary component
REAL,      INTENT(OUT)                           :: cs(NTRAJ, NCOMP, NSTAB, NSEK, NMETREG)
DOUBLE PRECISION,    DIMENSION(:,:), POINTER, INTENT(OUT)  :: csec_d           ! concentration of secondary component, double precision [ug/m3]
DOUBLE PRECISION,    DIMENSION(:,:), POINTER, INTENT(OUT)  :: ddepri_d                   
CHARACTER*10, INTENT(OUT)                        :: depeh                      
CHARACTER(*), INTENT(OUT)                        :: dir_chem                   ! directory where to read chemistry files from 
DOUBLE PRECISION,    DIMENSION(:,:), POINTER, INTENT(OUT)  :: drydep_d                    
INTEGER,   INTENT(OUT)                           :: dv     
REAL,      INTENT(OUT)                           :: ecvl(NSTAB, NTRAJ,2*MAXDISTR) 
REAL,      INTENT(OUT)                           :: emis(6,NLANDMAX) 
LOGICAL,   INTENT(OUT)                           :: eof                        
TYPE (TError), INTENT(INOUT)                     :: error                      
CHARACTER(*), INTENT(OUT)                        :: fnames_used_chem           ! string with names of files used for chemistry maps
character(*), intent(out) :: dir_bg_actual
REAL,      DIMENSION(:), POINTER, INTENT(OUT)    :: frac                       ! fraction of output cell on land surface
REAL,      DIMENSION(:,:), ALLOCATABLE, INTENT(OUT)  :: f_subsec_rcp           ! fractions for sub-secondary species, HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total [-]
REAL,      INTENT(OUT)                           :: gemre
REAL,      DIMENSION(:), POINTER, INTENT(OUT)    :: gem_subsec                 ! grid mean for concentration of sub-secondary species [ug/m3]
REAL,      DIMENSION(:), POINTER, INTENT(OUT)    :: gxm                        ! x-coordinates of receptors (lon-lat) [degrees]
REAL,      DIMENSION(:), POINTER, INTENT(OUT)    :: gym                        ! y-coordinates of receptors (lon-lat) [degrees]
TYPE (TApsGridReal), INTENT(OUT)                 :: gwgrid                  ! grid with gamma water values
REAL,      DIMENSION(:), POINTER, INTENT(OUT)    :: gw_rcp  
INTEGER,   INTENT(OUT)                           :: idb                         
INTEGER,   INTENT(OUT)                           :: idt                         
INTEGER,   INTENT(OUT)                           :: iseiz                       
INTEGER,   INTENT(OUT)                           :: jb                          
INTEGER,   INTENT(OUT)                           :: jt                          
INTEGER,   DIMENSION(:), POINTER, INTENT(OUT)    :: jump                       ! number of successive points that can be skipped for output purposes
REAL,      INTENT(OUT)                           :: koh
INTEGER,   INTENT(OUT)                           :: landmax
TYPE (TApsGridInt), INTENT(OUT)                  :: lugrid                     ! grid with land use class information (1: dominant land use, 2:NLU+1: percentages land use class)
INTEGER,   DIMENSION(:), POINTER, INTENT(OUT)    :: lu_rcp_dom_all             ! dominant land use class for each receptor point
REAL,      DIMENSION(:,:), POINTER, INTENT(OUT)  :: lu_rcp_per_user_all        ! percentage of landuse for all receptors, user defined in receptor file
INTEGER,   INTENT(OUT)                           :: maxidx                     !  max. number of particle classes (= 1 for gas)
INTEGER,   INTENT(OUT)                           :: mb
INTEGER,   INTENT(OUT)                           :: memdone
INTEGER,   INTENT(OUT)                           :: mt
CHARACTER*80,  INTENT(OUT)                       :: nam_pri_sec                ! name of primary + secondary component (SOx, NOy, NHy)
CHARACTER*12,  DIMENSION(:), POINTER, INTENT(OUT):: namrcp                     ! receptor names
CHARACTER*80,  INTENT(OUT)                       :: namsec
CHARACTER*80,  DIMENSION(:), POINTER, INTENT(OUT):: nam_subsec
INTEGER,   INTENT(OUT)                           :: nbron
TYPE (TApsGridReal), INTENT(OUT)                 :: nh3bggrid                  ! grid with background concentrations NH3 [ppb] (read as ug/m3, converted to ppb)
REAL,      DIMENSION(:), POINTER, INTENT(OUT)    :: nh3bg_rcp
INTEGER, intent(out)                             :: niter                      ! maximal number of iterations
TYPE (TApsGridReal), INTENT(OUT)                 :: no2bggrid                  ! grid with background concentrations NO2 [ppb] (read as ug/m3, converted to ppb)
INTEGER,   INTENT(OUT)                           :: nrrcp
INTEGER,   DIMENSION(:,:),POINTER, INTENT(OUT)   :: nsrc_sec                   ! number of sources present in wind sector (roads only) [-]
INTEGER,   INTENT(OUT)                           :: numbron
TYPE (TApsGridReal), INTENT(OUT)                 :: o3bggrid                   ! grids with background concentrations O3 per wind sector [ug/m3]
REAL,      DIMENSION(:,:), POINTER, INTENT(OUT)  :: o3bg_rcp                   ! O3 background concentration for all receptors and for each wind sector (nrrcp x NSEK) [ug/m3]
REAL,      DIMENSION(:,:), POINTER, INTENT(OUT)  :: parout_val                 ! values for extra output parameters [nparout,nrrcp]
REAL,      DIMENSION(:,:),POINTER, INTENT(OUT)   :: percvk_sec                 ! frequency of occurrence of wind sector (roads only) [-]
REAL,      INTENT(OUT)                           :: pmd(NPARTCLASS,MAXDISTR)
REAL,      DIMENSION(:), POINTER, INTENT(OUT)    :: precip                     ! total precipitation per year [mm/year]
REAL,      INTENT(OUT)                           :: rc_hno3                    ! canopy resistance HNO3 [s/m]
REAL,      INTENT(OUT)                           :: rc_no                      ! canopy resistance NO (set at 'high' value) [s/m]
REAL,      INTENT(OUT)                           :: rc_user                    ! canopy resistance specified by user in control file [s/m]
REAL,      INTENT(OUT)                           :: rhno2
REAL,      DIMENSION(:), POINTER, INTENT(OUT)    :: rhno3_rcp
REAL,      INTENT(OUT)                           :: r_no2_nox_season           ! component of NO2/NOx ratio which is season dependent
REAL,      DIMENSION(:,:),POINTER, INTENT(OUT)   :: r_no2_nox_sec              ! sector averaged NO2/NOx ratio according to vdHout parameterisation [-]
REAL,      INTENT(OUT)                           :: routpri                    ! in-cloud scavenging ratio for primary component [-]
REAL,      INTENT(OUT)                           :: routsec                    ! in-cloud scavenging ratio for secondary component
REAL,      DIMENSION(:), POINTER, INTENT(OUT)    :: scale_subsec
REAL,      INTENT(OUT)                           :: scavcoef
DOUBLE PRECISION,      DIMENSION(:,:), POINTER, INTENT(OUT)    :: sdrypri_arr
DOUBLE PRECISION,      DIMENSION(:,:), POINTER, INTENT(OUT)    :: sdrysec_arr
DOUBLE PRECISION,      DIMENSION(:,:), POINTER, INTENT(OUT)    :: snatpri_arr
DOUBLE PRECISION,      DIMENSION(:,:), POINTER, INTENT(OUT)    :: snatsec_arr
TYPE (TApsGridReal), INTENT(OUT)                 :: so2bggrid                  ! grid with background concentrations SO2 [ppb] (read as ug/m3, converted to ppb)
REAL,      DIMENSION(:), POINTER, INTENT(OUT)    :: so2bg_rcp
DOUBLE PRECISION,      DIMENSION(:,:), POINTER, INTENT(OUT)    :: somvnpri_arr
DOUBLE PRECISION,      DIMENSION(:,:), POINTER, INTENT(OUT)    :: somvnsec_arr
DOUBLE PRECISION,      DIMENSION(:,:), POINTER, INTENT(OUT)    :: telvnpri_arr
DOUBLE PRECISION,      DIMENSION(:,:), POINTER, INTENT(OUT)    :: telvnsec_arr
REAL,      INTENT(OUT)                           :: trafst(NTRAJ)
REAL,      INTENT(OUT)                           :: ugmoldep
REAL,      INTENT(OUT)                           :: uspmd(NPARTCLASS,MAXDISTR)
REAL,      INTENT(OUT)                           :: uurtot                    ! total number of hours from meteo statistics
type(Tvchem), INTENT(OUT)                        :: vchem_emep                ! grids with EMEP precursor mass and converted mass for computing chemical conversion rates
LOGICAL,   INTENT(OUT)                           :: verb
DOUBLE PRECISION,      DIMENSION(:,:), POINTER, INTENT(OUT)    :: vtel_arr
DOUBLE PRECISION,      DIMENSION(:,:), POINTER, INTENT(OUT)    :: vvchem_arr
DOUBLE PRECISION,    DIMENSION(:,:), POINTER, INTENT(OUT)  :: wdepri_d
DOUBLE PRECISION,    DIMENSION(:,:), POINTER, INTENT(OUT)  :: wetdep_d
REAL,      DIMENSION(:), POINTER, INTENT(OUT)    :: xm                        ! x-coordinates of receptors (m RDM)
REAL,      INTENT(OUT)                           :: xreg(NMETREG)
REAL,      INTENT(OUT)                           :: xul_cell_centre           ! x-coordinate of centre of upper-left grid cell [m]
REAL,      DIMENSION(:), POINTER, INTENT(OUT)    :: ym                        ! y-coordinates of receptors (m RDM)
REAL,      INTENT(OUT)                           :: yreg(NMETREG)
REAL,      INTENT(OUT)                           :: yul_cell_centre           ! y-coordinate of centre of upper-left grid cell [m]
TYPE (TApsGridInt), INTENT(OUT)                  :: z0eurgrid                 ! map of roughness lengths in Europe [m]
REAL,      INTENT(OUT)                           :: z0_metreg(NMETREG)        ! roughness lengths of NMETREG meteo regions; scale < 50 km [m]
REAL,      INTENT(OUT)                           :: z0_metreg_user            ! roughness length of user specified meteo region [m]
TYPE (TApsGridInt), INTENT(OUT)                  :: z0nlgrid                  ! map of roughness lengths in NL [m]
REAL,      DIMENSION(:), POINTER, INTENT(OUT)    :: z0_rcp_all                ! roughness lengths for all receptors; from z0-map or receptor file [m]
REAL,      INTENT(OUT)                           :: zf
REAL,      DIMENSION(:), POINTER, INTENT(OUT)    :: zm                        ! z-coordinates of receptors (m)

! Local variables:
LOGICAL                                          :: building_present1         ! at least one building is present in the source file
CHARACTER*80                                     :: dll_date
CHARACTER*80                                     :: dll_version
TYPE (TApsGridReal)                              :: f_subsec_grid             ! grids of fractions for sub-secondary species, HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total [-]
REAL                                             :: hourreg(NMETREG)
INTEGER                                          :: ierr                      ! error code for array allocation
TYPE (TApsGridReal)                              :: masker
REAL                                             :: rainreg(NMETREG)
INTEGER                                          :: usdv
LOGICAL, allocatable                             :: presentcode(:,:)
REAL,   allocatable                              :: dverl(:,:), usdverl(:,:)
character*512 :: dir_bg = ""  ! Directory in which to look for background maps.

!-----------------------------------------------------------------------------------------------------
allocate( presentcode(MAXDISTR,4), dverl(NHRBLOCKS,MAXDISTR), usdverl(NHRBLOCKS,MAXDISTR), &
          stat=ierr)
CALL AllocError(ierr, ROUTINENAAM, MAXDISTR, 'large local arrays', error)

! Initialise error structure and verbose option:
verb           = .FALSE.
error%haserror = .FALSE.   ! no error detected yet

! Read program arguments and determine the name of the control file, which may be derived from the current working directory.
! As a first parameter the diag flag is returned.
call ops_get_arg(kwargs, error)
IF (error%haserror) GOTO 9999 ! GOTO error handling

if (kwargs%varin_file .ne. '') CALL ops_varin_read2( kwargs%varin_file, varin, error)
if (error%haserror) goto 9999

IF (kwargs%diag == 1 .OR. kwargs%diag == 3) THEN
#ifndef UNIX
  WRITE(IOB_STDOUT,*) 'OPS-version: W-',MODVERSIE(1:LEN_TRIM(MODVERSIE)),' ; Release date: ', RELEASEDATE(1:11)
#else
  WRITE(IOB_STDOUT,*) 'OPS-version: L-',MODVERSIE(1:LEN_TRIM(MODVERSIE)),' ; Release date: ', RELEASEDATE(1:11)
#endif
  IF (kwargs%diag == 3) THEN
    WRITE(IOB_STDOUT,*) "dll's used by OPS:"
    CALL get_version_core(dll_version, dll_date)
    WRITE(IOB_STDOUT,*) 'ops_core  version: ',dll_version(1:LEN_TRIM(dll_version)),'; Release date: ', dll_date(1:11)
    CALL get_version_utils(dll_version, dll_date)
    WRITE(IOB_STDOUT,*) 'ops_utils version: ',dll_version(1:LEN_TRIM(dll_version)),'; Release date: ', dll_date(1:11)
  ENDIF
  GOTO 9999 ! GOTO error handling
ELSEIF (kwargs%diag == 2) THEN
  verb = .TRUE.
ELSE
  continue
ENDIF
IF (verb) WRITE (IOB_STDOUT,*) 'Verbose is: ', verb

#if (SKIP_OPENMP != true)
! Set number of threads for parallel processing:
call omp_set_num_threads(kwargs%nthreads)
#else
kwargs%nthreads = 1
#endif

! Read variables from control file:
call ops_read_ctr(ctr, error)
IF (error%haserror) GOTO 9999 ! GOTO error handling

! Generate full file names of those files that were not set explicitly in the control file;
! also check the existence of these files:
CALL ops_gen_fnames(ctr%substance%gasv, ctr%receptor%spgrid, ctr%meteo_surface%intpol, error)
IF (error%haserror) GOTO 9999 ! GOTO error handling

! Read source file and copy selected sources to scratch:
CALL ops_read_emis( &
   ctr%substance%icm, ctr%substance%gasv, &
   ctr%emission%ncatsel, ctr%emission%catsel, ctr%emission%nlandsel, ctr%emission%landsel, &
   kwargs%allow_sigz0_point_source, varin%varin_unc, &
   numbron, dverl, usdverl, pmd, uspmd, dv, usdv, presentcode, building_present1, error)
IF (error%haserror) GOTO 9999 ! GOTO error handling

! Set file names for building effect tables:
if (building_present1) then
   call ops_building_file_names(error)
   IF (error%haserror) GOTO 9999 ! GOTO error handling
endif

! Read meteo statistics:
CALL ops_read_meteo ( &
   ctr%meteo_surface%intpol, varin%varin_unc, jb, mb, idb, jt, mt, idt, uurtot, &
   iseiz, zf, astat, trafst, gemre, z0_metreg_user, cs, rainreg, &
   z0_metreg, xreg, yreg, hourreg, error)
IF (error%haserror) GOTO 9999 ! GOTO error handling

! Read roughness length (z0) grids for NL and Europe and land use information:
IF (.NOT. ctr%output%f_z0user) THEN
   CALL ops_read_z0_landuse(ctr%output%isec, z0nlgrid, z0eurgrid, lugrid, error)
   IF (error%haserror) GOTO 9999 ! GOTO error handling
ENDIF

! Allocate arrays for sub secondary species:
allocate(nam_subsec(ctr%output%nsubsec))
allocate(scale_subsec(ctr%output%nsubsec))
allocate(gem_subsec(ctr%output%nsubsec))

! Read background concentrations and other chemical maps:
if (len(trim(kwargs%dir_bg)) > 0) then
   ! Use cmdline args.
   dir_bg = trim(kwargs%dir_bg)
else if (len(trim(ctr%substance%dir_bg)) > 0) then
   ! Use ctr file."
   dir_bg = trim(ctr%substance%dir_bg)
endif	! if neither, then use default dir for background maps

IF (ctr%output%isec) THEN
  CALL ops_read_bg( &
     ctr%substance%icm, ctr%substance%iopt_vchem, ctr%output%nsubsec, ctr%identification%year, &
     ctr%output%chem_meteo_prognosis, ctr%emission%nemcat_road, ctr%emission%road_chem, dir_bg, &
     so2bggrid, no2bggrid, nh3bggrid, o3bggrid, gwgrid, f_subsec_grid, &
     vchem_emep, error, dir_chem, fnames_used_chem, dir_bg_actual)
  IF (error%haserror) GOTO 9999 ! GOTO error handling


ENDIF

! Determine grid dimensions nrcol and nrrow:
CALL ops_get_dim( &
   ctr%receptor%spgrid, ctr%receptor%igrens, ctr%receptor%xc, ctr%receptor%yc, &
   ctr%receptor%grid, ctr%receptor%nrcol, ctr%receptor%nrrow, &
   nrrcp, xul_cell_centre, yul_cell_centre, masker, error &
)
IF (error%haserror) GOTO 9999 !  GOTO error handling

! Allocate memory for xm, ym, zm, frac and jump; jump requires one extra element:
CALL alloc(nrrcp, xm, error)
CALL alloc(nrrcp, ym, error)
CALL alloc(nrrcp, zm, error)
CALL alloc(nrrcp, frac, error)
CALL alloc(NLU, nrrcp, lu_rcp_per_user_all, error)
CALL alloc(nrrcp+1, 1, jump, error)  ! fill jump with value 1
IF (error%haserror) GOTO 9999 ! GOTO error handling

! Allocation of namrcp. Because generic allocation does not work for strings with deferred length under UNIX, the full
! allocation code is written here (instead of calling alloc).
ALLOCATE(namrcp(nrrcp), stat=ierr)
CALL AllocError(ierr, ROUTINENAAM, nrrcp, 'string', error)

CALL alloc(nrrcp, z0_rcp_all, error)
CALL alloc(nrrcp, lu_rcp_dom_all, error)
IF (error%haserror) GOTO 9999 ! GOTO error handling

! Generate receptor grid:
namrcp = ' '
! NOTE:
CALL ops_gen_rcp( &
   ctr%receptor%spgrid, ctr%receptor%igrens, masker, ctr%receptor%grid, &
   ctr%receptor%nrcol, ctr%receptor%nrrow, nrrcp, xul_cell_centre, &
   yul_cell_centre, jump, xm, ym, zm, frac, namrcp, lu_rcp_dom_all, z0_rcp_all, &
   lu_rcp_per_user_all, kwargs%varz, kwargs%perc, error)
IF (error%haserror) GOTO 9999 ! GOTO error handling

! Initialisation:
CALL ops_init ( &
   building_present1, iseiz, mb, astat, dverl, usdverl, dv, usdv, &
   ctr, varin, &
   amol2, rc_user, coneh, amol21, depeh, namsec, nam_pri_sec, ugmoldep, scavcoef, &
   rc_no, rhno2, rc_hno3, routsec, routpri, conc_cf, koh, croutpri, ar, r_no2_nox_season, &
   ecvl, nam_subsec, buildingEffect, niter, &
   error &
)

IF (error%haserror) GOTO 9999 ! GOTO error handling

! Allocate miscellaneous arrays for receptor points
CALL alloc(nrrcp, gxm, error)
CALL alloc(nrrcp, gym, error)

CALL alloc(nrrcp, nh3bg_rcp, error)
CALL alloc(nrrcp, gw_rcp, error)
CALL alloc(NSEK, nrrcp, o3bg_rcp,  error)
CALL alloc(nrrcp, so2bg_rcp, error)
CALL alloc(nrrcp, rhno3_rcp, error)
CALL alloc(nrrcp, ctr%output%nsubsec, f_subsec_rcp, error)

IF (error%haserror) GOTO 9999 ! GOTO error handling
!
! Fill arrays with roughness length, landuse and rhno3_rcp, nh3bg_rcp, gw_rcp, o3bg_rcp, f_subsec_rcp, domlu for all receptor points
!
CALL ops_rcp_char_all( &
   ctr%substance%icm, ctr%substance%iopt_vchem, ctr%output%isec, ctr%output%nsubsec, &
   xm, ym, ctr%output%f_z0user, ctr%meteo_surface%z0_user, z0nlgrid, z0eurgrid, &
   lugrid, ctr%emission%nemcat_road, ctr%emission%road_chem, so2bggrid, nh3bggrid, gwgrid, o3bggrid, f_subsec_grid, &
   nrrcp, namrcp, gxm, gym, lu_rcp_dom_all, z0_rcp_all, rhno3_rcp, nh3bg_rcp, &
   gw_rcp, o3bg_rcp, so2bg_rcp, f_subsec_rcp, kwargs%domlu, error)
IF (error%haserror) GOTO 9999 ! GOTO error handling

! Allocate other arrays for receptor points (and set to 0):
CALL alloc(nrrcp, precip, error)
IF (error%haserror) GOTO 9999 ! GOTO error handling

! Open the progress file and write 0.0 progression to screen.
! Numbs (= # characters to backspace for screen progress indicator) is 11 for this first progress call.
memdone = -2
IF (.NOT.sysopen(fu_progress, indnam, 'w', 'progress file', error)) GOTO 9999 ! GOTO error handling
CALL ops_write_progress(0.0, '('' OPS: '',F5.1,''% done'')', 11, memdone)

#if (SKIP_OPENMP != true)
! Allocate "helparrays" for particles:
kwargs%nthreads = omp_get_max_threads()
#else
kwargs%nthreads = 1
#endif

allocate( &
          somvnpri_arr(NPARTCLASS, 0:kwargs%nthreads), &
          telvnpri_arr(NPARTCLASS, 0:kwargs%nthreads), &
          somvnsec_arr(NPARTCLASS, 0:kwargs%nthreads), &
          telvnsec_arr(NPARTCLASS, 0:kwargs%nthreads), &
          vvchem_arr(NPARTCLASS, 0:kwargs%nthreads),   &
          vtel_arr(NPARTCLASS, 0:kwargs%nthreads),     &
          sdrypri_arr(NPARTCLASS, 0:kwargs%nthreads),  &
          sdrysec_arr(NPARTCLASS, 0:kwargs%nthreads),  &
          snatpri_arr(NPARTCLASS, 0:kwargs%nthreads),  &
          snatsec_arr(NPARTCLASS, 0:kwargs%nthreads),  stat = ierr)
CALL AllocError(ierr, ROUTINENAAM, NPARTCLASS, '2-dimensional arrays', error)

somvnpri_arr = 0
telvnpri_arr = 0
somvnsec_arr = 0
telvnsec_arr = 0
vvchem_arr   = 0
vtel_arr     = 0
sdrypri_arr  = 0
sdrysec_arr  = 0
snatpri_arr  = 0
snatsec_arr  = 0

CALL alloc(NPARTCLASS, nrrcp, cpri_d, error)
CALL alloc(NPARTCLASS, nrrcp, csec_d, error)
CALL alloc(NPARTCLASS, nrrcp, drydep_d, error)
CALL alloc(NPARTCLASS, nrrcp, wetdep_d, error)
CALL alloc(NPARTCLASS, nrrcp, ddepri_d, error)
CALL alloc(NPARTCLASS, nrrcp, wdepri_d, error)
CALL alloc(nparout,    nrrcp, parout_val,error)

! Allocate NO2 concentration and NOx concentration for 'road correction':
if (ctr%substance%icm == icm_NOx) then
   CALL alloc(nrrcp, 0.0, cno2, error)
   CALL alloc(nrrcp, 0.0, cnox, error)
   CALL alloc(NSEK, nrrcp, cnox_sec, error);      cnox_sec      =  0.0
   CALL alloc(NSEK, nrrcp, percvk_sec, error);    percvk_sec    =  0.0
   CALL alloc(NSEK, nrrcp, nsrc_sec, error);      nsrc_sec      =  0
   CALL alloc(NSEK, nrrcp, r_no2_nox_sec, error); r_no2_nox_sec = -1.0 ! -1 indicates 'not set'
else
   ! No NOx or no deposition -> ratio NO2/NOx is not set -> cno2 unknown
   CALL alloc(nrrcp, float(MISVALNUM), cno2, error) ! no deposition -> ratio NO2/NOx is not set -> cno2 unknown
   CALL alloc(nrrcp, float(MISVALNUM), cnox, error)
   CALL alloc(NSEK, nrrcp, r_no2_nox_sec, error); r_no2_nox_sec = -1.0 ! -1 indicates 'not set'

   CALL alloc(0, nrrcp, cnox_sec, error)
   CALL alloc(0, nrrcp, percvk_sec, error)
   CALL alloc(0, nrrcp, nsrc_sec, error)
   CALL alloc(0, nrrcp, r_no2_nox_sec, error)
endif

IF (kwargs%class_output) then
    IF (ctr%receptor%spgrid .NE. 2) THEN
      ! only supported for receptor output (spgrid == 2)
      CALL SetError('CLI option -classoutput is currently only supported for receptor output', error)
      CALL ErrorParam('spgrid', ctr%receptor%spgrid, error)
      GOTO 9999
    ENDIF
    ! concentration output per class is wanted, allocate and initialize the arrays
    CALL alloc(NPARTCLASS, NSTAB, NSEK, NTRAJ, nrrcp, cpri_class,   error)
    CALL alloc(            NSTAB, NSEK, NTRAJ, nrrcp, percvk_class, error)
    CALL alloc(            NSTAB, NSEK, NTRAJ, nrrcp, nsrc_class,   error)
else
    CALL alloc(0, 0, 0, 0, nrrcp, cpri_class,   error)
    CALL alloc(   0, 0, 0, nrrcp, percvk_class, error)
    CALL alloc(   0, 0, 0, nrrcp, nsrc_class,   error)
ENDIF

IF (error%haserror) GOTO 9999 ! GOTO error handling

! Zeroise summed parameters:
cpri_d   = 0.0
csec_d   = 0.0
drydep_d = 0.0
wetdep_d = 0.0
ddepri_d = 0.0
wdepri_d = 0.0
emis     = 0.0
landmax  = 0
nbron    = 0
parout_val = 0.0
eof      = .false.
!
! Set maxidx = number of particle classes.
!
IF (ctr%substance%gasv) THEN
  maxidx = 1
ELSE
  maxidx = NPARTCLASS
ENDIF

9999 CALL ErrorCall(ROUTINENAAM, error)

end subroutine ops_init_all

!-------------------------------------------------------------------------------------------------------------------------------
! DESCRIPTION           : Initialisation of variables based on data from the control file and on meteo statistics.
!-------------------------------------------------------------------------------------------------------------------------------
subroutine ops_init( &
      building_present1, iseiz, mb, astat, dverl, usdverl, dv, usdv, &
      ctr, varin, &
      amol2, rc_user, coneh, amol21, depeh, namsec, nam_pri_sec, ugmoldep, scavcoef, &
      rc_no, rhno2, rc_hno3, routsec, routpri, conc_cf, koh, croutpri, ar, r_no2_nox_season, &
      ecvl, nam_subsec, buildingEffect, niter, &
      error &
)

use m_commonconst_lt
use m_ops_building
use m_ops_read_ctr, only: TCtrLayers
use m_ops_tdo_proc, only: Tdo_proc

use m_error
use m_ops_varin

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                !
PARAMETER    (ROUTINENAAM = 'ops_init')

! SUBROUTINE ARGUMENTS - INPUT
LOGICAL,   INTENT(IN)                            :: building_present1           ! at least one building is present in the source file
INTEGER,   INTENT(IN)                            :: iseiz
INTEGER,   INTENT(IN)                            :: mb
REAL,      INTENT(IN)                            :: astat(NTRAJ, NCOMP, NSTAB, NSEK)
REAL,      INTENT(IN)                            :: dverl(NHRBLOCKS,MAXDISTR)
REAL,      INTENT(IN)                            :: usdverl(NHRBLOCKS,MAXDISTR)
INTEGER,   INTENT(IN)                            :: dv
INTEGER,   INTENT(IN)                            :: usdv

! SUBROUTINE ARGUMENTS - I/O
type(TCtrLayers), intent(inout) :: ctr
TYPE(Tvarin), INTENT(INOUT) :: varin  ! input variables for meteo
REAL,      INTENT(INOUT)                         :: amol2

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: rc_user                     ! canopy resistance specified by user in control file [s/m]
CHARACTER*(*), INTENT(OUT)                       :: coneh
REAL,      INTENT(OUT)                           :: amol21
CHARACTER*(*), INTENT(OUT)                       :: depeh
CHARACTER*(*), INTENT(OUT)                       :: namsec
CHARACTER*(*), INTENT(OUT)                       :: nam_pri_sec                ! name of primary + secondary component (SOx, NOy, NHy)
REAL,      INTENT(OUT)                           :: ugmoldep
REAL,      INTENT(OUT)                           :: scavcoef
REAL,      INTENT(OUT)                           :: rc_no                      ! canopy resistance Rc for NO [s/m]
REAL,      INTENT(OUT)                           :: rhno2
REAL,      INTENT(OUT)                           :: rc_hno3                    ! canopy resistance HNO3 [s/m]
REAL,      INTENT(OUT)                           :: routsec                    ! in-cloud scavenging ratio for secondary component
                                                                               ! (rout << rain-out = in-cloud) [-]
REAL,      INTENT(OUT)                           :: routpri                    ! in-cloud scavenging ratio for primary component [-] (rout << rain-out = in-cloud)
REAL,      INTENT(OUT)                           :: conc_cf
REAL,      INTENT(OUT)                           :: koh
REAL,      INTENT(OUT)                           :: croutpri                   ! constant (initial) in-cloud scavenging ratio [-] for primary component
REAL,      INTENT(OUT)                           :: ar
REAL,      INTENT(OUT)                           :: r_no2_nox_season           ! component of NO2/NOx ratio which is season dependent
REAL,      INTENT(OUT)                           :: ecvl(NSTAB, NTRAJ, *)
CHARACTER*(*), INTENT(OUT)                       :: nam_subsec(ctr%output%nsubsec)
type(TbuildingEffect), INTENT(OUT)               :: buildingEffect             ! structure with building effect tables
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER                                          :: niter                      ! maximal number of iterations
INTEGER                                          :: i
INTEGER                                          :: ndv
INTEGER                                          :: itraj
INTEGER                                          :: istab
INTEGER                                          :: iu
REAL                                             :: vdmax
REAL                                             :: som
!-------------------------------------------------------------------------------------------------------------------------------
!
! Limit emission trend correction factor; if emtrend = 0 -> emtrend = 1 
!
IF (ABS(ctr%emission%emtrend) .LE. EPS_DELTA) ctr%emission%emtrend = 1.
!
! Parameterisation substance properties
!

! Molecular weight [g/mol] for secondary component equals amol1, by default
amol2 = ctr%substance%amol1

! Default concentration correction factor
conc_cf = 1

! Set maximal deposition velocity [m/s] (only for kdeppar = 1)

vdmax=0.034

! Set default ratio amol2/amol1
amol21 = 1.

IF (ctr%substance%gasv) THEN ! if gas
  IF (ctr%substance%idep) THEN ! if deposition has to be computed

    IF (.NOT. ctr%output%isec) THEN

      ! Secondary components not present (so not SO2, NOx or NH3).

      ! Compute surface resistance Rc [s/m], scavenging rate (scavcoef [%/h]) or scavenging ratio W (routpri [-]),
      ! diffusion coefficient in air (dg [cm^2/s]) (according to Durham et al, 1981: Atm Env. 15, 1059-1068)
      ! and logical irev (reversible uptake of gas in droplets is possible)

      ! Depending on kdeppar, compute Rc from deposition velocity vd (kdeppar = 1) or directly from Rc = ddepar (kdeppar = 2)
      IF (ctr%substance%kdeppar .EQ. 1) rc_user = 1./ctr%substance%ddeppar - 1./SQRT(vdmax*ctr%substance%ddeppar)        ! if ddeppar = vdmax -> Rc = 0;
                                                                               ! ddeppar < vdmax is not possible (check in ops_read_ctr)
      IF (ctr%substance%kdeppar .EQ. 2) rc_user = ctr%substance%ddeppar

       ! Depending on knatdeppar, set scavenging rate (scavcoef [%/h]) or scavenging ratio (routpri [-]) (see ops_par_nat)
       ! note: amol2 = amol1
       IF (ctr%substance%knatdeppar .EQ. 1) THEN
        scavcoef = ctr%substance%wdeppar
        ctr%substance%dg       = SQRT(1./amol2)
        ctr%substance%irrev     = .FALSE.
      ELSEIF (ctr%substance%knatdeppar .EQ. 2) THEN
        routpri = ctr%substance%wdeppar
        ctr%substance%dg      = SQRT(1./amol2)
        ctr%substance%irrev    = .FALSE.
      ELSE
        routpri = ctr%substance%wdeppar
      ENDIF

      !Edit
      ! Adjustments relevant for sensitivity analyses. Multiplication factors are 1.0 by default.
      ! Note: hier aanpassen, of direct onder bepaling van scavcoef (1x) en routpri (2x) hierboven.
      ! Op de huidige manier staat de boel wel netjes bij elkaar en nog steeds in de buurt van scavcoef en routpri zodat het hopelijk niet over het hoofd wordt gezien.
      IF (ctr%substance%knatdeppar .EQ. 1) THEN
         scavcoef = varin%varin_unc%unc_sourcedepl%rainout_pri_fact * scavcoef 
      ELSE
         routpri = varin%varin_unc%unc_sourcedepl%rainout_pri_fact * routpri
      ENDIF
      !End Edit
    ELSE

      ! secondary components present [SO2, NO2, NH3] -> knatdeppar = 3.
      ! scavenging rate (scavcoef [%/h]) can be set to 0, because it is only used in case knatdeppar = 1.
      ! Set following parameters:
      ! amol2   :  molecular weight [g/mol] of secondary component
      ! croutpri: constant (initial) in-cloud scavenging ratio [-] for primary component
      ! routsec : in-cloud scavenging ratio for secondary component
      !           (rout << rain-out = in-cloud) [-])
      ! conc_cf : concentration correction factor for output.
      ! Section 6.3 OPS report FS

      ctr%substance%knatdeppar = 3
      scavcoef = 0
      !Edit
      ! Adjustments relevant for sensitivity analyses. Multiplication factors are 1.0 by default. 
      ! Not really needed since scavcoef = 0, but just in case changes in the code will be made in the future and scavcoef is no longer zero.  
      ! scavcoef = varin%varin_unc%unc_sourcedepl%rainout_pri_fact * scavcoef 
      !End Edit

      ! icm = icm_SO2: SO2 (secondary component SO4)
      IF (ctr%substance%icm == icm_SO2) THEN
         amol2 = 96.
         croutpri = 100000.
         routsec  = 2.0e6
         conc_cf  = 1.

      ! icm = icm_NOx: NOx (secondary component NO3)
      ELSE IF (ctr%substance%icm == icm_NOx) THEN

         amol2    = 62.
         croutpri = 20000.
         routsec  = 1.4e7

         ! Set parameters specific for NOx
         ! rhno2  : ratio [HNO2]/[NOx] based on measurements Speuld, Slanina et al 1990, but they report 4% (p. 66 OPS report) FS
         ! koh    : second order reaction rate constant of reaction NO2 + OH -> HNO3 [cm3/(molec s)]
         !          Baulch et al 1982 (OPS report Table 6.2 FS): kOH = 1.035e-11 cm3/(molec s) = 1000.9 ppb-1 h-1, at T = 0 C
         !                                                                                     =  932.6 ppb-1 h-1, at T = 20 C
         !                                                                                     =  917.0 ppb-1 h-1, at T = 25 C
         !          Baulch D.L., Cox, R.A. Crutzen P.J., Hampson R.F. Jr., Kerr, F.A. Troe, J. and Watson R.P. (1982)
         !          Evaluated kinetic and photochemical data for atmospheric chemistry: J. Phys. Chem. Ref. Data 11 (Suppl. 1), 327-496.
         ! conc_cf: correction factor (8%) for NOx (to account for HNO2 and PAN contributions to NO2)
         ! rc_no  : canopy resistance NO (set at 'high' value) [s/m]
         ! rc_hno3 : canopy resistance HNO3 (set at 'low' value) [s/m]
         
         rhno2   = 0.03
         koh     = 1020.*0.9 ! = 918 ppb-1 h-1
         rc_no    = 2000
         rc_hno3  = 10
         if (ctr%substance%do_proc%chem) then
            conc_cf = 1.0/1.08
         else
            ! If there is no chemistry, then NOx = NO + NO2 from emissions only, so no correction is needed:
            conc_cf = 1.0
         endif

      !
      ! icm = icm_NH3: NH3 (secondary component NH4)
      !
      ELSE IF (ctr%substance%icm == icm_NH3) THEN
         amol2    = 18.
         croutpri = 1.4e6
         routsec  = 1.4e7
         conc_cf  = 1.
      ENDIF
      !Edit
      ! Adjustments relevant for sensitivity analyses. Multiplication factors are 1.0 by default. 
      routsec = varin%varin_unc%unc_sourcedepl%rainout_sec_fact * routsec
      !End Edit

    ENDIF

    ! Set ratio of molecular weights of primary and secondary component
    amol21=amol2/ctr%substance%amol1
  ENDIF

!
! particles
!
ELSE
  ctr%substance%vchemc=0
  ctr%substance%vchemv=0
ENDIF
!
! Component names (see m_commonconst_lt for definition of CNAME)
!
IF (ctr%output%isec) THEN
  ctr%substance%namco       = CNAME(ctr%substance%icm,1)
  namsec      = CNAME(ctr%substance%icm,2)
  if (ctr%output%nsubsec .gt. 0) then
     nam_subsec = CNAME_SUBSEC(1:ctr%output%nsubsec)
  endif
  nam_pri_sec = CNAME(ctr%substance%icm,4)
ELSE
  namsec      = ctr%substance%namco
  nam_pri_sec = ctr%substance%namco
ENDIF
!
! Units for concentration and deposition and conversion factors
! (see m_commonconst_lt for definition of UNITS and DEPUNITS).
! Note:
!    1/(number of seconds in an hour) = 1/3600 = 0.278e-3
!    number of hours in a year = 8760
!    amol2 = molecular weight secondary component in g/mol
!
IF (ctr%substance%icm == icm_NOx) THEN                                                           ! NOx
  coneh = UNITS(2)                                                             ! ug/m3 NO2
ELSE
  coneh = UNITS(1)                                                             ! default ug/m3
ENDIF

depeh = DEPUNITS(ctr%output%ideh) ! used for file header
IF (ctr%substance%idep) THEN
  IF (ctr%output%ideh .EQ. 1) THEN       ! conversion ug/m2/h -> mmol/m2/s:
    ugmoldep = .278e-6/amol2
  ELSE IF (ctr%output%ideh .EQ. 2) THEN  ! conversion ug/m2/h -> g/m2/s:
    ugmoldep = .278e-9
  ELSE IF (ctr%output%ideh .EQ. 3) THEN  ! conversion ug/m2/h -> mol/ha/j:
    ugmoldep = 87.6/amol2
  ELSE IF (ctr%output%ideh .EQ. 4) THEN  ! conversion ug/m2/h -> kg/ha/j:
    ugmoldep = 87.6/1000.
  ELSE IF (ctr%output%ideh .EQ. 5) THEN  ! conversion ug/m2/h -> mmol/m2/j:
    ugmoldep = 8.76/amol2
  ELSE                        ! conversion ug/m2/h -> g/m2/j:
    ugmoldep = 8.76/1000.
  ENDIF
ELSE
   ugmoldep = 1.0
ENDIF

IF (ctr%substance%icm == icm_NOx) THEN
!
!  Set ar and r_no2_nox_season.
!
!  ar = proportionality constant [ppb J-1 cm2 h] in relation [OH] = ar Qr, with
!  [OH] = OH radical concentration [ppb] , Qr = global radiation in J/cm2/h, see
!  Van Egmond N.D. and Kesseboom H. (1985) A numerical mesoscale model for long-term average NOx and NO2-concentration.
!  Atmospheric Environment 19, 587-595.
!  Table 6.1 OPS-report:
!  ar(summer) = 7345 molec cm-3 W-1 m2
!  ar(winter) = 3540 molec cm-3 W-1 m2.
!  Cnversion factors seconds_per_hour = 3600; cm2_per_m2 = 10^4.
!     at T = 20 C, p = 1013 mb:
!        ppbfac = conversion factor ppb -> molec/cm3 = 2.5029e10 molec/cm3/ppb
!        ar(summer) = 7345*cm2_per_m2/(ppbfac*seconds_per_hour) = 81.5e-8 ppb J-1 cm2 h.
!        ar(winter) = 3540*cm2_per_m2/(ppbfac*seconds_per_hour) = 39.3e-8 ppb J-1 cm2 h.
!        ar(year)   = average of ar_winter) and ar_wummer)      = 60.4e-8 ppb J-1 cm2 h.
!     at T = 25 C, p = 1013 mb:
!        ppbfac     =  2.4610e+010 molec/cm3/ppb
!        ar(summer) =  82.9e-8 ppb J-1 cm2 h.
!        ar(winter) =  40.0e-8 ppb J-1 cm2 h.
!        ar(year)   =  61.4e-8 ppb J-1 cm2 h.
!
!  For a specific month, a cos-function over the year is used, such that ar(average) = 62e-8, ar(Feb) = 40e-8, ar(Aug) = 83e-8.
!     Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec
!     32.1 40.1 54.0 70.0 83.9 91.9 91.9 83.9 70.0 54.0 40.1 32.1 *1e-8
!
!  r_no2_nox_season = season dependent part of [NO2]/[NOx] ratio, see Table  6.3 OPS report for stability class S2:
!            r_no2_nox_season(summer) = 0.78, r_no2_nox_season(winter) = 0.58; r_no2_nox_season(year) = average of summer and winter value = 0.68.
!            For a specific month, 2 cos-functions are used, such that r_no2_nox_season(Feb) = 0.57, r_no2_nox_season(Aug) = 0.78:
!            Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec
!            0.53 0.57 0.62 0.70 0.78 0.82 0.82 0.78 0.70 0.62 0.57 0.53
!
!  iseiz: 0=long term; 1=year; 2=winter; 3=summer; 4=month in winter; 5=month in summer)
!
   IF (iseiz .LE. 1) THEN
     ar      = 62.e-8
     r_no2_nox_season = .65
   ELSE IF (iseiz .EQ. 2) THEN
     ar      = 40.e-8
     r_no2_nox_season = .58
   ELSE IF (iseiz .EQ. 3) THEN
     ar      = 83.e-8
     r_no2_nox_season = .78
   ELSE IF (iseiz .EQ. 4) THEN
     ar      = 62.e-8 - COS((FLOAT(mb) - .5)/NMONTH*2.*PI)*31.e-8
     r_no2_nox_season = .65 - COS((FLOAT(mb) - .5)/NMONTH*2.*PI)*.12
   ELSE
     ar      = 62.e-8 - COS((FLOAT(mb) - .5)/NMONTH*2.*PI)*31.e-8
     r_no2_nox_season = .65 - COS((FLOAT(mb) - .5)/NMONTH*2.*PI)*.18
   ENDIF
ENDIF
!
! Compute percentage of emission per stability class for traffic related sources and for low sources.
! User specified and standard diurnal emission variation distributions are averaged into one array (ecvl).
!
ndv = dv + usdv

! Loop over distance classes and stability classes:
DO itraj = 1, NTRAJ
   DO istab = 1, NSTAB
!
!     Initialise sum and ecvl.
!
      som = 0.
      ecvl(istab, itraj, :ndv) = 0.

      ! Loop over hour blocks and compute, for each stability/distance class, the average over hours:
      ! ecvl = weighed average of diurnal emission variation;
      ! weighing factors are astat(17): distribution of stability classes over day, source oriented [-].
      
      ! Factor 1.0e-2 is to convert from dverl as percentage to ecvl as fraction.
      ! Note: astat() has been filled with meteo parameters of most central region in NL (region 5); see ops_statfil

      DO iu = 1, NHRBLOCKS
         som = som + astat(itraj, 17, istab, iu)
!
!        Add contribution of standard diurnal emission variation.
!
         DO i = 1, dv
            ecvl(istab, itraj, i) = ecvl(istab, itraj, i) + dverl(iu, i) * astat(itraj, 17, istab, iu)*1.e-2
         ENDDO
!
!        Add contribution of user specified diurnal emission variation. If there is no user specified emission variation
!       (usdvnam has string length 0), then ndv = dv and we have an empty loop.
!
         DO i = dv+1, ndv
            ecvl(istab, itraj, i) = ecvl(istab, itraj, i) + usdverl(iu, (i - dv)) * astat(itraj, 17, istab, iu)*1.e-2
         ENDDO
      ENDDO

      ! Compute average diurnal emission variation:
      IF (som .GT. (0. + EPS_DELTA)) THEN
         ecvl(istab, itraj, :ndv) = ecvl(istab, itraj, :ndv)/som
      ENDIF
   ENDDO
ENDDO

! Read building effect tables:
if (building_present1) then
   call ops_building_read_tables(buildingEffect,error)
   !write(*,*) 'ops_init/classdefinitionArray: ',buildingEffect%classdefinitionArray
   !write(*,*) 'ops_init/buildingFactArray:',buildingEffect%buildingFactArray
else
   ! allocate the building arrays, as they're passed to subroutines whether there is a building or not
   call ops_building_effect_alloc_zero(buildingEffect,error)
endif
if (error%haserror) goto 9999

! Set maximal number of iterations for NOx;
! use 2 iterations if chemistry is enabled and if there are road emission categories present:
IF ( &
   ctr%substance%do_proc%chem &
   .and. ctr%substance%icm == icm_NOx &
   .and. ctr%emission%nemcat_road .gt. 0 &
   .and. ctr%emission%road_chem &
) THEN
   niter = 2
ELSE
   niter = 1
ENDIF

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE ops_init

!------------------------------------------------------------------
subroutine ops_init_iter(eof, nbron, cpri_d, csec_d, &
                         drydep_d, wetdep_d, ddepri_d, wdepri_d, cnox_sec, cno2, emis, parout_val, &
                         sdrypri_arr, snatpri_arr, somvnpri_arr, telvnpri_arr, sdrysec_arr, snatsec_arr, somvnsec_arr, &
                         telvnsec_arr, vvchem_arr, vtel_arr)

! Initialise summed quantities at start of iteration step

use m_commonconst_lt, only: NLANDMAX
use m_commonfile,     only: fu_scratch

! Output arguments:
LOGICAL                                          :: eof                        ! end of file has been reached (scratch file with selected emissions)
INTEGER                                          :: nbron                      ! number of sources
DOUBLE PRECISION,    DIMENSION(:,:), POINTER     :: cpri_d                     ! concentration of primary component, double precision [ug/m3]
DOUBLE PRECISION,    DIMENSION(:,:), POINTER     :: csec_d                     ! concentration of secondary component, double precision [ug/m3]
DOUBLE PRECISION,    DIMENSION(:,:), POINTER     :: drydep_d
DOUBLE PRECISION,    DIMENSION(:,:), POINTER     :: wetdep_d
DOUBLE PRECISION,    DIMENSION(:,:), POINTER     :: ddepri_d
DOUBLE PRECISION,    DIMENSION(:,:), POINTER     :: wdepri_d
REAL,                DIMENSION(:,:), POINTER     :: cnox_sec                   ! NOx concentration per receptor/wind sector for current iteration
REAL,                DIMENSION(:),   POINTER     :: cno2                       ! NO2 concentration (derived from NOx and parameterised ratio NO2/NOx) [ug/m3]
REAL                                             :: emis(6,NLANDMAX)
REAL,                DIMENSION(:,:), POINTER     :: parout_val                 ! values for extra output parameters [nparout,nrrcp]
DOUBLE PRECISION                                             :: sdrypri_arr(:,:)
DOUBLE PRECISION                                             :: snatpri_arr(:,:)
DOUBLE PRECISION                                             :: somvnpri_arr(:,:)
DOUBLE PRECISION                                             :: telvnpri_arr(:,:)
DOUBLE PRECISION                                             :: sdrysec_arr(:,:)
DOUBLE PRECISION                                             :: snatsec_arr(:,:)
DOUBLE PRECISION                                             :: somvnsec_arr(:,:)
DOUBLE PRECISION                                             :: telvnsec_arr(:,:)
DOUBLE PRECISION                                             :: vvchem_arr(:,:)
DOUBLE PRECISION                                             :: vtel_arr(:,:)

! Rewind scratch file:
rewind(fu_scratch)
eof    = .false.

! Zeroise summed quantities and other parameters:
nbron        = 0
cpri_d       = 0.0
csec_d       = 0.0
drydep_d     = 0.0
wetdep_d     = 0.0
ddepri_d     = 0.0
wdepri_d     = 0.0
cnox_sec     = 0.0
cno2         = 0.0
emis         = 0.0
parout_val   = 0.0
sdrypri_arr  = 0.0
snatpri_arr  = 0.0
somvnpri_arr = 0.0
telvnpri_arr = 0.0
sdrysec_arr  = 0.0
snatsec_arr  = 0.0
somvnsec_arr = 0.0
telvnsec_arr = 0.0
vvchem_arr   = 0.0
vtel_arr     = 0.0

end subroutine ops_init_iter

end module m_ops_init
