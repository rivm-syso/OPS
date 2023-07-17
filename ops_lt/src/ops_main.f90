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
!
!-------------------------------------------------------------------------------------------------------------------------
! 3. Postprocessing and finalisation
!    dealloc                 : Deallocate memory 
!    Allocate memory for variables that are used for different output purposes and initialise them to zero
!    Loop over particle size classes that are relevant for producing output fields:
!    | Get output fields
!    | ops_outp_prep           : Compute variables used in different outputs
!    | ops_calc_stats          : Compute (grid) statistics
!    | Open print output file (= PRNFILE in control file)
!    | ops_print_recep         : print output for receptors
!    | ops_print_grid          : print output for gridded receptors
!    | Open plot file (= PLTFILE in control file)
!    | ops_plot_uitv           : write data to PLTFILE
!    - end loop over particle size classes for which to produce output fields
!    ops_print_info            : Write additional data to print file (= PRNFILE in control file)
!    deallocation and error handling
!-------------------------------------------------------------------------------------------------------------------------------
PROGRAM ops_main

use m_ops_building
use m_aps
use m_utils
use m_fileutils
use m_error
use m_commonconst_lt
use m_commonfile
use IFPORT
use m_ops_vchem
use m_ops_logfile
use m_ops_gen_rcp
use m_ops_output_lt
use m_ops_landuse
use m_ops_init
use m_ops_bron_rek
use m_ops_rcp_char_1
use m_ops_src_char
use m_ops_tra_char
use m_ops_reken
use m_ops_write_progress
use m_ops_outp_prep
use m_ops_calc_stats
use m_ops_print_recep2
use m_ops_print_grid
use m_ops_plot_uitv
use m_ops_print_info
use m_ops_brondepl, only: Tdo_proc

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_main')

! LOCAL VARIABLES
INTEGER*4                                        :: numbron
INTEGER*4                                        :: spgrid 
INTEGER*4                                        :: landmax                     
INTEGER*4                                        :: nbron                       
INTEGER*4                                        :: nsbuf                       
INTEGER*4                                        :: btgedr(LSBUF)               
INTEGER*4                                        :: bdegr(LSBUF)                
INTEGER*4                                        :: bcatnr(LSBUF)               
INTEGER*4                                        :: blandnr(LSBUF)              
INTEGER*4                                        :: bx(LSBUF)                   
INTEGER*4                                        :: by(LSBUF)                   
INTEGER*4                                        :: bnr(LSBUF)      
REAL*4                                           :: disx                       ! linear distance between source and receptor [m]
type(TbuildingEffect)                            :: buildingEffect             ! structure with building effect tables
INTEGER*4                                        :: jb                          
INTEGER*4                                        :: mb                          
INTEGER*4                                        :: idb                         
INTEGER*4                                        :: jt                          
INTEGER*4                                        :: mt                          
INTEGER*4                                        :: idt                         
INTEGER*4                                        :: dv     
INTEGER*4                                        :: iseiz                       
INTEGER*4                                        :: icm                         
INTEGER*4                                        :: iter                       ! iteration counter
REAL*4,    PARAMETER                             :: iter_epsr = 0.01           ! convergence criterion; if relative difference between iterands < iter_epsr then convergance
INTEGER*4                                        :: niter                      ! maximal number of iterations
INTEGER*4                                        :: nsubsec                    ! number of sub-secondary species                       
INTEGER*4                                        :: nrrcp  
INTEGER*4                                        :: nrcol  
INTEGER*4                                        :: nrrow  
INTEGER*4                                        :: intpol                      
INTEGER*4                                        :: knatdeppar                  
INTEGER*4                                        :: ircp
INTEGER*4                                        :: mmm
INTEGER*4                                        :: ndone                      ! keep track of number of finished calculations
INTEGER*4                                        :: ntotal                     ! the number of total calculations needed
INTEGER*4                                        :: lu_rcp_dom                 ! dominant landuse class at receptor point
REAL*4                                           :: lu_tra_per(NLU)            ! percentages of landuse classes over trajectorie
REAL*4                                           :: lu_rcp_per(NLU)            ! percentages of landuse classes at receptor points
INTEGER*4                                        :: year                       ! year used for chemistry maps
CHARACTER(LEN = 512)                             :: dir_chem                   ! directory where to read chemistry files from 
CHARACTER(LEN = 512)                             :: fnames_used_chem           ! string with names of files used for chemistry maps
INTEGER*4                                        :: memdone                     
INTEGER*4                                        :: maxidx                     ! max. value of NPARTCLASS
INTEGER*4                                        :: todo
INTEGER*4                                        :: ntodo
REAL*4                                           :: aind                       ! hourglass (rough indicator of % of computing time spent)
REAL*4                                           :: amol2                       
REAL*4                                           :: amol21                      
REAL*4                                           :: z0_metreg_user             ! roughness length of user specified meteo region [m]      
REAL*4                                           :: z0_user                    ! roughness length specified by user [m]
REAL*4                                           :: z0_metreg_rcp              ! roughness length at receptor; interpolated from meteo regions [m]
REAL*4                                           :: z0_rcp                     ! roughness length at receptor; from z0-map [m]
REAL*4                                           :: z0_src                     ! roughness length at source; from z0-map [m]
REAL*4                                           :: z0_tra                     ! roughness length representative for trajectory [m]
REAL*4                                           :: vchemc                      
INTEGER*4                                        :: iopt_vchem                 ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)                                                                                                                                            
REAL*4                                           :: vchemv                      
REAL*4                                           :: rc_user                    ! canopy resistance specified by user in control file [s/m]
REAL*4                                           :: ugmoldep                    
REAL*4                                           :: gemre 
REAL*4                                           :: gemcpri
REAL*4                                           :: gemcsec                    
REAL*4                                           :: totddep                    
REAL*4                                           :: gemddep                    
REAL*4                                           :: gemddpri                   
REAL*4                                           :: gemddsec                   
REAL*4                                           :: ddrpri                     
REAL*4                                           :: ddrsec                     
REAL*4                                           :: totwdep                    
REAL*4                                           :: gemwdep                    
REAL*4                                           :: gemwdpri                   
REAL*4                                           :: gemwdsec                   
REAL*4                                           :: wdrpri                     
REAL*4                                           :: wdrsec                     
REAL*4                                           :: tottdep                    
REAL*4                                           :: gemtdep                    
REAL*4                                           :: gemprec                    
REAL*4                                           :: ccr                        
REAL*4                                           :: xul_cell_centre           ! x-coordinate of centre of upper-left grid cell [m]                    
REAL*4                                           :: yul_cell_centre           ! y-coordinate of centre of upper-left grid cell [m] 
REAL*4                                           :: bdiam(LSBUF)               
REAL*4                                           :: bsterkte(LSBUF)            
REAL*4                                           :: bwarmte(LSBUF)             
REAL*4                                           :: bhoogte(LSBUF)             
REAL*4                                           :: bsigmaz(LSBUF)    
REAL*4                                           :: bD_stack(LSBUF)           ! diameter of the stack [m]
REAL*4                                           :: bV_stack(LSBUF)           ! exit velocity of plume at stack tip [m/s]
REAL*4                                           :: bTs_stack(LSBUF)          ! temperature of effluent from stack [K]            
LOGICAL                                          :: bemis_horizontal(LSBUF)   ! horizontal outflow of emission
type(Tbuilding)                                  :: bbuilding(LSBUF)          ! array with structures with building parameters
REAL*4                                           :: emis(6,NLANDMAX) 
REAL*4                                           :: conc_cf
REAL*4                                           :: astat(NTRAJ, NCOMP, NSTAB, NSEK)  
REAL*4                                           :: ar
REAL*4                                           :: r_no2_nox_season          ! component of NO2/NOx ratio which is season dependent              
REAL*4                                           :: uurtot                    ! total number of hours from meteo statistics
REAL*4                                           :: zf                          
REAL*4                                           :: trafst(NTRAJ)               
REAL*4                                           :: bqrv(LSBUF)                 
REAL*4                                           :: bqtr(LSBUF)                 
REAL*4                                           :: cs(NTRAJ, NCOMP, NSTAB, NSEK, NMETREG)  
REAL*4                                           :: z0_metreg(NMETREG)    ! roughness lengths of NMETREG meteo regions; scale < 50 km [m]           
REAL*4                                           :: xreg(NMETREG)               
REAL*4                                           :: yreg(NMETREG)               
REAL*4                                           :: ecvl(NSTAB, NTRAJ,2*MAXDISTR) 
REAL*4                                           :: pmd(NPARTCLASS,MAXDISTR)    
REAL*4                                           :: uspmd(NPARTCLASS,MAXDISTR)  
REAL*4                                           :: amol1                      
REAL*4                                           :: emtrend                    
REAL*4                                           :: grid                       
REAL*4                                           :: scavcoef                   
REAL*4                                           :: routsec                    ! in-cloud scavenging ratio for secondary component
                                                                               ! (rout << rain-out = in-cloud) [-]
REAL*4                                           :: routpri                    ! in-cloud scavenging ratio for primary component
                                                                               ! (rout << rain-out = in-cloud) [-]
REAL*4                                           :: croutpri                   ! constant (initial) in-cloud scavenging ratio [-] for primary component                                   
REAL*4                                           :: rc_no                      ! canopy resistance NO (set at 'high' value) [s/m]   
REAL*4                                           :: rhno2                       
REAL*4                                           :: rc_hno3                    ! canopy resistance HNO3 [s/m]         
REAL*4                                           :: dg                         
REAL*4                                           :: dispg(NSTAB)               
REAL*4                                           :: koh                        
REAL*4                                           :: so2sek(NSEK)               
REAL*4                                           :: no2sek(NSEK)               
REAL*4, DIMENSION(:), POINTER                    :: gem_subsec                 ! grid mean for concentration of sub-secondary species [ug/m3]     
REAL*4                                           :: scale_con                  
REAL*4                                           :: scale_sec                  
REAL*4, DIMENSION(:), POINTER                    :: scale_subsec              
REAL*4                                           :: scale_dep                  
REAL*4                                           :: so2bgtra                   ! 
REAL*4                                           :: no2bgtra                   ! 
REAL*4                                           :: nh3bgtra                   ! 
REAL*4                                           :: o3bgtra(NSEK)              ! 
type(Tvchem)                                     :: vchem2                     
REAL*8, DIMENSION(:), POINTER                    :: sdrypri_arr                 
REAL*8                                           :: sdrypri                     
REAL*8, DIMENSION(:), POINTER                    :: snatpri_arr                 
REAL*8                                           :: snatpri                     
REAL*8, DIMENSION(:), POINTER                    :: somvnpri_arr                
REAL*8                                           :: somvnpri                    
REAL*8, DIMENSION(:), POINTER                    :: telvnpri_arr                
REAL*8                                           :: telvnpri                    
REAL*8, DIMENSION(:), POINTER                    :: sdrysec_arr                 
REAL*8                                           :: sdrysec                     
REAL*8, DIMENSION(:), POINTER                    :: snatsec_arr                 
REAL*8                                           :: snatsec                     
REAL*8, DIMENSION(:), POINTER                    :: somvnsec_arr                
REAL*8                                           :: somvnsec                    
REAL*8, DIMENSION(:), POINTER                    :: telvnsec_arr                
REAL*8                                           :: telvnsec                    
REAL*8, DIMENSION(:), POINTER                    :: vvchem_arr                  
REAL*8                                           :: vvchem                      
REAL*8, DIMENSION(:), POINTER                    :: vtel_arr                    
REAL*8                                           :: vtel                        

CHARACTER*512                                    :: namco                      
CHARACTER*80                                     :: project                    
CHARACTER*80                                     :: namsec                     
CHARACTER*80, DIMENSION(:), POINTER              :: nam_subsec                  
CHARACTER*80                                     :: nam_pri_sec                ! name of primary + secondary component (SOx, NOy, NHy)     
CHARACTER*10                                     :: coneh                      
CHARACTER*10                                     :: depeh                      
LOGICAL*4                                        :: f_z0user                   
INTEGER*4                                        :: diag                       ! = 1,3 (argument -r) -> print version number and quit
LOGICAL                                          :: verb                       
LOGICAL                                          :: isec                       
LOGICAL                                          :: igrid                      ! include receptor values in the Report output; is set with INCLUDE in ctr-file or -v option
LOGICAL                                          :: irev                       
LOGICAL                                          :: gasv                       
TYPE(Tdo_proc)                                   :: do_proc                    ! options to switch on/off specific processes
LOGICAL                                          :: idep                       
LOGICAL                                          :: eof                        
LOGICAL                                          :: subbron                    
LOGICAL                                          :: domlu                      ! use dominant land use instead of land use percentages
LOGICAL                                          :: varz                       ! indicator whether value for receptorheight is read from receptorfile                    
LOGICAL                                          :: perc                       ! indicator whether percentages for landuse are read from receptorfile
LOGICAL                                          :: mindist                    ! indicator whether results for receptors starting from mindist will be calculated
LOGICAL                                          :: maxdist                    ! indicator whether results for receptors upto maxdist will be calculated
LOGICAL                                          :: reken                      ! indicator whether results will be calculated for current source - receptor
LOGICAL                                          :: outputfile_opened                    

INTEGER*4, DIMENSION(:), POINTER                 :: catsel                     ! selection of categories (0: all categories)
INTEGER*4                                        :: nemcat_road                ! number of road emission categories (for vdHout NO2/NOx ratio)
INTEGER*4, DIMENSION(:), POINTER                 :: emcat_road                 ! list of road emission categories (for vdHout NO2/NOx ratio)
LOGICAL                                          :: lroad_corr_present = .false. ! at least one road with vdHout correction is present
INTEGER*4, DIMENSION(:), POINTER                 :: landsel                    ! selection of countries (0: all countries)
INTEGER*4, DIMENSION(:), POINTER                 :: lu_rcp_dom_all             ! dominant land use class for each receptor point
INTEGER*4, DIMENSION(:), POINTER                 :: jump                       ! number of successive points that can be skipped for output purposes

REAL*4,    DIMENSION(:), POINTER                 :: xm                         ! x-coordinates of receptors (m RDM)
REAL*4,    DIMENSION(:), POINTER                 :: ym                         ! y-coordinates of receptors (m RDM)
REAL*4,    DIMENSION(:), POINTER                 :: zm                         ! z-coordinates of receptors (m)
REAL*4,    DIMENSION(:), POINTER                 :: frac                       ! fraction of output cell on land surface
REAL*4,    DIMENSION(:,:), POINTER               :: lu_rcp_per_user_all        ! percentage of landuse for all receptors, user defined in receptor file
REAL*4,    DIMENSION(:), POINTER                 :: gxm                        ! x-coordinates of receptors (lon-lat) [degrees]
REAL*4,    DIMENSION(:), POINTER                 :: gym                        ! y-coordinates of receptors (lon-lat) [degrees]
REAL*4,    DIMENSION(:), POINTER                 :: z0_rcp_all                 ! roughness lengths for all receptors; from z0-map or receptor file [m]
REAL*4,    DIMENSION(:), POINTER                 :: rhno3_rcp                 
REAL*4,    DIMENSION(:,:), ALLOCATABLE           :: f_subsec_rcp               ! fractions for sub-secondary species, HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total [-]                                                                                                                                                                          
REAL*4,    DIMENSION(:), POINTER                 :: precip                     ! total precipitation per year [mm/year]
DOUBLE PRECISION,    DIMENSION(:,:), POINTER     :: cpri_d                     ! concentration of primary component, double precision [ug/m3]
REAL*4,    DIMENSION(:), POINTER                 :: cpri                       ! concentration of primary component [ug/m3]
DOUBLE PRECISION,    DIMENSION(:,:), POINTER     :: csec_d                     ! concentration of secondary component, double precision [ug/m3]
REAL*4,    DIMENSION(:), POINTER                 :: csec                       ! concentration of secondary component [ug/m3]
DOUBLE PRECISION,    DIMENSION(:,:), POINTER     :: drydep_d                    
REAL*4,    DIMENSION(:), POINTER                 :: drydep                      
DOUBLE PRECISION,    DIMENSION(:,:), POINTER     :: wetdep_d                    
REAL*4,    DIMENSION(:), POINTER                 :: wetdep                      
DOUBLE PRECISION,    DIMENSION(:,:), POINTER     :: ddepri_d                   
REAL*4,    DIMENSION(:), POINTER                 :: ddepri                      
DOUBLE PRECISION,    DIMENSION(:,:), POINTER     :: wdepri_d                   
REAL*4,    DIMENSION(:), POINTER                 :: wdepri                      
REAL*4,    DIMENSION(:), POINTER                 :: totdep                      
REAL*4,    DIMENSION(:,:), POINTER               :: csubsec                    ! concentration of sub-secondary species [ug/m3]                
REAL*4,    DIMENSION(:), POINTER                 :: nh3bg_rcp                  
REAL*4,    DIMENSION(:,:), POINTER               :: o3bg_rcp                   ! O3 background concentration for all receptors and for each wind sector (nrrcp x NSEK) [ug/m3]                  
REAL*4,    DIMENSION(:), POINTER                 :: so2bg_rcp                  
REAL*4,    DIMENSION(:), POINTER                 :: cno2                       ! NO2 concentration (derived from NOx and parameterised ratio NO2/NOx)
REAL*4,    DIMENSION(:,:),POINTER                :: cnox_sec                   ! wind sector averaged NOx concentration (roads only) [ug/m3]
REAL*4,    DIMENSION(:,:),POINTER                :: percvk_sec                 ! frequency of occurrence of wind sector (roads only) [-]
INTEGER*4, DIMENSION(:,:),POINTER                :: nsrc_sec                   ! number of sources present in wind sector (roads only) [-]
REAL*4,    DIMENSION(:),POINTER                  :: cnox                       ! NOx concentration, per receptor, for output, only for ROADS (vdHout)
REAL*4,    DIMENSION(:,:),POINTER                :: r_no2_nox_sec              ! sector averaged NO2/NOx ratio according to vdHout parameterisation [-]


! Type Tparout
!    INTEGER*4, PARAMETER                             :: numval = 4          ! number of extra output parameters (besides concentration, deposition)
!    REAL*4,    DIMENSION(:,:), POINTER               :: val                 ! values for extra output parameters [nrrcp,nparout]
!    CHARACTER(len= 128)                              :: name(nparout)       ! names of extra output parameters                      
!    CHARACTER(len=  40)                              :: unit(nparout)       ! units of extra output parameters                      
!    LOGICAL                                          :: set = .true.        ! write parout parameters to output
!    REAL*4                                           :: disx = 2000.0       ! source receptor distance used for parout parameters
!    REAL*4                                           :: hbron = 5.0         ! emission height used for parout parameters
!    REAL*4                                           :: qw = 0.0            ! heat content used for parout parameters
!    
! End Type Tparout
! 
INTEGER*4, PARAMETER                             :: nparout = 4                ! number of extra output parameters (besides concentration, deposition)
REAL*4,    DIMENSION(:,:), POINTER               :: parout_val                 ! values for extra output parameters [nrrcp,nparout]
CHARACTER(len= 128)                              :: parout_name(nparout)       ! names of extra output parameters                      
CHARACTER(len=  40)                              :: parout_unit(nparout)       ! units of extra output parameters                      
LOGICAL                                          :: parout_write = .false.      ! write parout parameters to output
REAL*4                                           :: parout_disx = 50000.0       ! source receptor distance used for parout parameters [m]


CHARACTER*12, DIMENSION(:), POINTER              :: namrcp                     ! receptor names

TYPE (TApsGridInt)                               :: z0nlgrid                   ! map of roughness lengths in NL [m]
TYPE (TApsGridInt)                               :: lugrid                     ! grid with land use class information (1: dominant land use, 2:NLU+1: percentages land use class)
TYPE (TApsGridInt)                               :: z0eurgrid                  ! map of roughness lengths in Europe [m]
TYPE (TApsGridReal)                              :: so2bggrid                  ! grid with background concentrations SO2 [ppb] (read as ug/m3, converted to ppb)
TYPE (TApsGridReal)                              :: no2bggrid                  ! grid with background concentrations NO2 [ppb] (read as ug/m3, converted to ppb)
TYPE (TApsGridReal)                              :: nh3bggrid                  ! grid with background concentrations NH3 [ppb] (read as ug/m3, converted to ppb)
TYPE (TApsGridReal)                              :: o3bggrid                   ! grids with background concentrations O3 per wind sector [ug/m3]
TYPE (TError)                                    :: error                      
!-------------------------------------------------------------------------------------------------------------------------------
! DATA:
DATA dispg /.28,.28,.20,.20,.12,.20/

!
! Set coefficients in correction factors for SO2 background concentration for each wind direction sector 
! derived from 24 regional LML stations over 2003 (eastern wind -> higher SO2)
!
DATA so2sek /0.77, 0.73, 0.88, 1.09, 1.30, 1.34, 1.28, 1.14, 0.97, 0.94, 0.90, 0.77/

!
! Set coefficients in correction factor for nO2 background concentration for each wind direction sector 
! derived from 15 regional LML stations over 2004 (eastern wind -> higher NO2)
!
DATA no2sek /0.81, 0.88, 1.08, 1.30, 1.33, 1.40, 1.25, 1.03, 0.83, 0.71, 0.70, 0.68/

! Initialise (arguments alphabetically):
CALL ops_init_all( &
   amol1, amol2, amol21, ar, astat, buildingEffect, catsel, cno2, cnox_sec, cnox, conc_cf, coneh, cpri_d, croutpri, cs, csec_d, &
   ddepri_d, depeh, dg, diag, dir_chem, domlu, drydep_d, dv, ecvl, emcat_road, emis, emtrend, eof, error, fnames_used_chem, frac, f_subsec_rcp, &
   f_z0user, gasv, gemre, gem_subsec, grid, gxm, gym, icm, idb, do_proc, idep, idt, igrid, intpol, iopt_vchem, irev, isec, &
   iseiz, jb, jt, jump, knatdeppar, koh, landmax, landsel, lugrid, lu_rcp_dom_all, lu_rcp_per_user_all, maxdist, maxidx, &
   mb, memdone, mindist, mt, namco, nam_pri_sec, namrcp, namsec, nam_subsec, nbron, nemcat_road, nh3bggrid, nh3bg_rcp, niter, no2bggrid, &
   nparout, nrcol, nrrcp, nrrow, nsrc_sec, nsubsec, numbron, o3bggrid, o3bg_rcp, parout_val, perc, percvk_sec, pmd, precip, project, rc_hno3, rc_no, &
   rc_user, reken, rhno2, rhno3_rcp, r_no2_nox_season, r_no2_nox_sec, routpri, routsec, scale_subsec, scavcoef, sdrypri, sdrypri_arr, sdrysec, &
   sdrysec_arr, snatpri, snatpri_arr, snatsec, snatsec_arr, so2bggrid, so2bg_rcp, somvnpri, somvnpri_arr, &
   somvnsec, somvnsec_arr, spgrid, subbron, telvnpri, telvnpri_arr, telvnsec, telvnsec_arr, trafst, ugmoldep, uspmd, &
   uurtot, vchem2, vchemc, vchemv, verb, vtel, vtel_arr, vvchem, vvchem_arr, wdepri_d, wetdep_d, xm, xreg, &
   xul_cell_centre, year, ym, yreg, yul_cell_centre, z0eurgrid, z0_metreg, z0_metreg_user, z0nlgrid, z0_rcp_all, z0_user, zf, zm)
IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program

! Check diag (-r option); if set -> skip rest of program:
IF (.not. (diag == 1 .OR. diag == 3)) THEN

   ! Calculate the total number of calculations needed
   ntotal = niter * numbron * nrrcp
   ! Keep track of number of finished calculations
   ndone = 0
 
   ! Iteration is needed because we need total NOx-concentration at a receptor before we can compute the NO2/NOx ratio based on vdHout:
   DO iter = 1,niter 
   
      ! Skip iteration steps if there are no roads for vdHout correction present:
      ! if (iter > 1 .and. .not. lroad_corr_present) -> skip:
      if (iter .eq. 1 .or. lroad_corr_present) then
         ! write(*,*) 'iter = ',iter,', nemcat_road = ',nemcat_road, ', emcat_road = ',emcat_road(1:nemcat_road)
         
         ! Initialisation for this iteration step:
         if (niter .gt. 1) then
            call ops_init_iter(eof, nbron, cpri_d, csec_d, drydep_d, wetdep_d, ddepri_d, wdepri_d, cnox_sec, cno2, emis, parout_val, & 
                               sdrypri_arr, snatpri_arr, somvnpri_arr, telvnpri_arr, sdrysec_arr, snatsec_arr, somvnsec_arr, & 
                               telvnsec_arr, vvchem_arr, vtel_arr)
         endif
         
         ! Start loop over source data blocks of length LSBUF (until end-of-file of scratch file with selected emissions):
         DO WHILE (.NOT. eof)
           
           ! Read source characteristics from scratch file and fill into buffer arrays (source data are read in
           ! blocks of length LSBUF (LSBUF=4000)):
           CALL ops_bron_rek (emtrend, buildingEffect, landmax, emis, nsbuf, bnr, bx, by, bdiam, bsterkte, bwarmte, bhoogte, bsigmaz, bD_stack, bV_stack, bTs_stack, bemis_horizontal, bbuilding, btgedr,        &
                           &  bdegr, bqrv, bqtr, bcatnr, blandnr, eof, error)
         
           IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program
         
           ! Adjust number of processed sources
           nbron = nbron + nsbuf
           
           ! Loop over all receptor points  ++++++++++++++++++++++++  
           DO ircp = 1, nrrcp
         
             ! Retrieve z0 and landuse values for this receptorpoint:
             CALL ops_rcp_char_1 (isec, ircp, nrrcp, intpol, gxm(ircp), gym(ircp), cs, z0_metreg, xreg, yreg, astat, z0_metreg_user,      &
                               &  spgrid, xm(ircp), ym(ircp), lugrid, domlu, perc, lu_rcp_per_user_all, lu_rcp_dom_all, f_z0user, z0_rcp_all, &
                               &  uurtot, z0_metreg_rcp, lu_rcp_per, lu_rcp_dom, z0_rcp, error)
             IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program
         
             ! Define a circle of NSEK sources around a receptor point, used for writing parout-variables:
             IF (parout_write) CALL ops_parout_circle(ircp, xm, ym, parout_disx, subbron, nsbuf, nbron, numbron, bnr, bx, by, bdiam, bsterkte, & 
                                      bwarmte, bhoogte, bsigmaz, bD_stack, bV_stack, bTs_stack, & 
                                      bemis_horizontal, bbuilding, btgedr, bdegr, bqrv, bqtr, bcatnr, blandnr)
            
             ! Loop over nsbuf sources in the buffer ++++++++++++++++++++++++
             DO mmm = 1, nsbuf
                   
                ! Check if we should calculate this source <-> receptor due to a minimum or maximum distance or not
                IF (mindist .or. maxdist) THEN
                  disx  = SQRT(((bx(mmm)-xm(ircp))*(bx(mmm)-xm(ircp))) + ((by(mmm)-ym(ircp))*(by(mmm)-ym(ircp))))
                  IF (mindist .and. maxdist) THEN
                    IF (disx .LE. DISTMIN .or. disx .GT. DISTMAX) reken = .FALSE. ! next source <-> receptor calculation.
                  ELSEIF (mindist) THEN
                    IF (disx .LE. DISTMIN) reken = .FALSE. ! next source <-> receptor calculation.
                  ELSE
   	             IF (disx .GT. DISTMAX) reken = .FALSE. ! next source <-> receptor calculation.
                  ENDIF
                ENDIF
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
                   CALL ops_tra_char (icm, iopt_vchem, f_z0user, z0_user, xm(ircp), ym(ircp), bx(mmm), by(mmm), &
                                   &  lugrid, z0nlgrid, z0eurgrid, so2bggrid, no2bggrid, nh3bggrid, o3bggrid, vchem2, domlu, & 
                                   &  z0_tra, lu_tra_per, so2bgtra, no2bgtra, nh3bgtra, o3bgtra, error)
                   IF (error%haserror) THEN
                      CALL ErrorParam('source number',mmm,error)
                      CALL ErrorParam('receptor number',ircp,error)
                      CALL ErrorParam('receptor name',trim(namrcp(ircp)),error)
                      GOTO 9999 ! GOTO error handling at end of program
                   ENDIF
                   
                   ! Compute contribution of current emission source to concentration and deposition at current receptor:
                   CALL ops_reken(do_proc, iter, idep, isec, icm, gasv, vchemc, iopt_vchem, vchemv, dv, amol1, amol2, amol21, ar, & 
                               &  r_no2_nox_sec(:,ircp), r_no2_nox_season, ecvl, iseiz, zf, &  
                               &  trafst, knatdeppar, mb, ugmoldep, dg, irev, scavcoef, koh, croutpri, rc_no, rhno2, rc_hno3, &
                               &  nrrcp, ircp, gxm(ircp), gym(ircp), xm(ircp), ym(ircp), zm(ircp), &
                               &  frac(ircp), nh3bg_rcp(ircp), o3bg_rcp(:,ircp), so2bg_rcp(ircp), rhno3_rcp(ircp), & 
                               &  bqrv(mmm), bqtr(mmm), bx(mmm), by(mmm), bdiam(mmm), bsterkte(mmm), bwarmte(mmm), bhoogte(mmm), &
                               &  bsigmaz(mmm), bD_stack(mmm), bV_stack(mmm), bTs_stack(mmm), bemis_horizontal(mmm), bbuilding(mmm), &
                               &  buildingEffect,btgedr(mmm), bdegr(mmm), bcatnr(mmm), nemcat_road, emcat_road, & 
                               &  z0_src, z0_tra, z0_rcp, z0_metreg_rcp, lu_tra_per, &
                               &  lu_rcp_per, so2sek, no2sek, so2bgtra, no2bgtra, nh3bgtra, o3bgtra, vchem2, maxidx, pmd, uspmd, spgrid, grid, &
                               &  subbron, uurtot, routsec, rc_user, lroad_corr_present, somvnsec_arr, telvnsec_arr, vvchem_arr, vtel_arr, somvnpri_arr, &
                               &  telvnpri_arr, ddepri_d, wdepri_d, sdrypri_arr, snatpri_arr, sdrysec_arr, snatsec_arr, & 
                               &  cpri_d, csec_d, drydep_d, wetdep_d, astat, cnox_sec, cno2, &
                               &  percvk_sec, nsrc_sec, precip(ircp), routpri, dispg, &
                               &  nparout, parout_val(:,ircp), parout_name, parout_unit, parout_write, error)
                               
                   IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program
                ENDIF 
                reken = .true.
                ndone = ndone+1  ! always update counter even if reken was .false.
             ENDDO   ! end loop over sources in buffer
             !
             ! Write progress (update each 2%)
             !             
             aind = 100.*FLOAT(ndone)/FLOAT(ntotal)
             CALL ops_write_progress(aind, '(F5.1)', 5, memdone)
           ENDDO  ! end loop over receptors
         ENDDO ! end loop over source data blocks of length LSBUF (until end-of-file of scratch file with source data)
         
         ! Conversion NOx -> NO2 based on vdHout: 
         if (do_proc%chem .and. icm .eq. 2 .and. lroad_corr_present) &
            call ops_vchem_ratio_no2_nox_vdhout(iter,nrrcp,o3bg_rcp,o3bgtra,nsrc_sec,cnox_sec,cno2,percvk_sec,r_no2_nox_sec,cnox)
            
      ENDIF ! Check whether there are roads with vdHout correction present
   ENDDO ! end of iteration
   
   ! Deallocate some memory; this creates some space for output arrays:
   CALL dealloc(lugrid)
   CALL dealloc(z0eurgrid)
   CALL dealloc(z0nlgrid)
   CALL dealloc(nh3bggrid)
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
   
   ! Allocate memory for variables that are used for different output purposes and initialise them to zero:
   CALL alloc(nrrcp, 0., cpri, error)
   CALL alloc(nrrcp, 0., csec, error)
   CALL alloc(nrrcp, 0., drydep, error)
   CALL alloc(nrrcp, 0., wetdep, error)
   CALL alloc(nrrcp, 0., ddepri, error)
   CALL alloc(nrrcp, 0., wdepri, error)
   CALL alloc(nrrcp, 0., totdep, error)
   CALL alloc(nrrcp, nsubsec, csubsec, error); if (nsubsec .gt. 0) csubsec = 0.0
   if (error%haserror) goto 9999
   
   ! ntodo: number of particle size classes that are relevant for producing output fields
   ! gas      : ntodo = 1
   ! particles: ntodo = 6 (all particle size classes)
   !            but produce only one summed field for concentration, dry deposition, wet deposition and total deposition.
   IF (gasv) then
      ntodo = 1
   ELSE
      ntodo = 6
   ENDIF
   
   ! For PM (code 24) only 4 particle size classes are relevant: PM1.0, PM2.5, PM4.0 and PM10.
   ! Only for the classes PM2.5 and PM10, OPS produces output of the concentration field into a file.
   IF (icm == 24) then
     ntodo = 4
     idep  = .FALSE.
   ENDIF
   !
   outputfile_opened = .FALSE.
   
   ! Loop over particle size classes that are relevant for producing output fields:
   DO todo = 1,ntodo
   
     ! Set name of component for PM-class:
     IF (icm == 24) THEN
       IF (todo == 1) namco = "PM1.0"
       IF (todo == 2) namco = "PM2.5"
       IF (todo == 3) namco = "PM4.0"
       IF (todo == 4) namco = "PM10."
       nam_pri_sec = namco
     ENDIF
   
     ! Get output fields:
     cpri     = cpri + SNGL(cpri_d(:,todo))
     csec     = csec + SNGL(csec_d(:,todo))
     drydep   = drydep + SNGL(drydep_d(:,todo))
     wetdep   = wetdep + SNGL(wetdep_d(:,todo))
     ddepri   = ddepri + SNGL(ddepri_d(:,todo))
     wdepri   = wdepri + SNGL(wdepri_d(:,todo))
     sdrypri  = sdrypri + sdrypri_arr(todo)
     sdrysec  = sdrysec + sdrysec_arr(todo)
     snatpri  = snatpri + snatpri_arr(todo)
     snatsec  = snatsec + snatsec_arr(todo)
     somvnpri = somvnpri + somvnpri_arr(todo)
     somvnsec = somvnsec + somvnsec_arr(todo)
     telvnpri = telvnpri + telvnpri_arr(todo)
     telvnsec = telvnsec + telvnsec_arr(todo)
     vvchem   = vvchem + vvchem_arr(todo)
     vtel     = vtel + vtel_arr(todo)
     
     ! Check for which value of loop index todo to produce output;
     ! gasv     -> todo = 1
     ! not gasv -> todo = 6
     ! PM       -> todo = 2 (PM2.5) or 4 (PM10)
     IF ((gasv .and. todo == 1) .or. (.not.gasv .and. todo == 6) .or. (icm == 24 .and. todo == 2) .or. (icm == 24 .and. todo == 4)) THEN
   
       ! Compute variables used in different output sources:
       CALL ops_outp_prep (nrrcp, icm, nsubsec, conc_cf, f_subsec_rcp, csec, drydep, wetdep, cpri, cnox, totdep, csubsec, scale_con, scale_sec,      &
           &  scale_subsec, scale_dep)
   
       ! Compute (grid) statistics:
       CALL ops_calc_stats (nrrcp, nsubsec, frac, cpri, csec, drydep ,wetdep, gemre, sdrypri, sdrysec, snatpri, snatsec, somvnpri, &
            &  somvnsec, vvchem, vtel, telvnpri, telvnsec, grid, amol21, ugmoldep, csubsec, gemcpri, gemcsec, totddep, &
            &  gemddep, gemddpri, gemddsec, ddrpri, ddrsec, totwdep, gemwdep, gemwdpri, gemwdsec, wdrpri, wdrsec, &
            &  gemprec, tottdep, gemtdep, ccr, gem_subsec)
   
       ! Open print output file (= PRNFILE in control file) and write data:
       IF (.not.outputfile_opened) THEN
         IF (.NOT.sysopen(fu_prt, prnnam, 'w', 'print file', error)) GOTO 9999 ! GOTO error handling at end of program
       ENDIF
       IF (spgrid .EQ. 2) THEN
         CALL ops_print_recep2(project, icm, gasv, idep, do_proc, isec, igrid, verb, namco, namsec, nam_pri_sec, coneh, depeh, conc_cf, amol21, &
                           &  ugmoldep, nrrcp, nsubsec, namrcp, xm, ym, precip, cpri, csec, drydep, ddepri, wetdep, wdepri, cno2, cnox, &  
                           &  lu_rcp_dom_all, z0_rcp_all, gemcpri, gemcsec, ccr, gemddep, gemddpri, gemddsec, ddrpri, ddrsec, gemwdep,  &
                           &  gemwdpri, gemwdsec, wdrpri, wdrsec, gemprec, gemtdep, csubsec, gem_subsec, nam_subsec, totdep, &
                           &  scale_con, scale_sec, scale_subsec, scale_dep, error)
       ELSE
         CALL ops_print_grid (nrrcp, nsubsec, jump, project, icm, gasv, idep, isec, igrid, verb, namco, namsec, nam_pri_sec, coneh, depeh, &
               &  conc_cf, amol21, ugmoldep, nrcol, nrrow, grid, xul_cell_centre, yul_cell_centre, precip, cpri, csec, drydep, wetdep, ddepri, &
               &  lu_rcp_dom_all, z0_rcp_all, gemcpri, gemcsec, ccr, gemddep, gemddpri, gemddsec, totddep, ddrpri, ddrsec, gemwdep, &
               &  gemwdpri, gemwdsec, totwdep, wdrpri, wdrsec, gemprec, gemtdep, tottdep, csubsec, gem_subsec, nam_subsec, totdep, &
               &  scale_con, scale_sec, scale_subsec, scale_dep, error)
       ENDIF
       IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program
   
       ! Open plot file (= PLTFILE in control file) and write data: 
       IF (.not.outputfile_opened) THEN
         IF (.NOT. sysopen(fu_plt, pltnam, 'w', 'plot file', error)) GOTO 9999 ! GOTO error handling at end of program
       ENDIF
       CALL ops_plot_uitv(spgrid, isec, coneh, nrrcp, nsubsec, jump, xul_cell_centre, yul_cell_centre, nrcol, nrrow, grid, idep, namco, nam_pri_sec, namsec, depeh,    &
                       & namrcp, xm, ym, cpri, csec, drydep, ddepri, wdepri, wetdep, cno2, cnox, icm, csubsec, nam_subsec,  &  
                       & nparout, parout_val, parout_name, parout_unit, parout_write, error)
       IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program
       outputfile_opened = .TRUE.
     ENDIF  !  if output is requested
   ENDDO ! End loop over particle size classes for which to produce output fields
   
   ! Write additional data to print file (= PRNFILE in control file):
   CALL ops_print_info (project, gasv, isec, intpol, spgrid, z0_rcp, namco, nbron, bnr, bx, by, bsterkte, bqrv, bqtr, bwarmte,     &
       &  bhoogte, bdiam, bsigmaz, btgedr, bdegr, bcatnr, blandnr, emis, emtrend, jb, mb, idb, jt, mt, idt, iseiz,                 &  
       &  f_z0user, landmax, subbron, domlu, varz, perc, mindist, maxdist, dir_chem, fnames_used_chem, error)
   IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program
   
   ! Close print file if it is open:
   IF (outputfile_opened) CALL sysclose(fu_prt, prnnam, error)
   
ENDIF ! Check -r option

! Finally, make sure progress counter is set to 100%
CALL ops_write_progress(100.0, '(F5.1)', 5, memdone)
! Close the progression file:
CALL sysclose(fu_progress, indnam, error)
IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program

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
   ENDIF
   CALL EXIT(1)
ELSE
   CALL EXIT(0)
ENDIF

END PROGRAM ops_main
