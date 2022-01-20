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
! DESCRIPTION         :
!
!    Program OPS-LT: Long Term version of "Operationeel Prioritaire Stoffen" model
!    (operationeel = operational, prioritaire stoffen = priority substances)
!
!    This program has been derived from the TREND model at April 28, 1987
!
!    English version: Januari 30, 1990
!
!    The OPS-model is a long-term Lagrangian transport and deposition model that describes
!    relations between individual sources or source areas, and individual receptors. The model is statistical in
!    the sense that concentration and deposition values are calculated for a number of typical situations 
!    (distance/meteo classes) and the long-term value is obtained by summation of these values, weighed with 
!    their relative frequencies. All relations governing the transport and deposition process are solved analytically,
!    allowing the use of nongridded receptors and sources, and variable grid sizes.
!
!    Meteo data for the requested period are read from a meteo statistics file, which has been prepared before (meteo-preprocessor).
!    In this meteo statistics file, there are four distance classes: 0-100, 100-300, 300-1000 and >1000 km, 6 stability classes 
!    (neutral/stable/unstable, each with a relatively high or low mixing height) and 12 wind sectors of 30 degrees.
!
!    OPS computes long term (1 month to 10 year) concentration and deposition at receptor points, either in a regularly spaced grid,
!    or at user specified locations, e.g. locations of measuring stations. 
!
!                        2012-01-24, Ferd Sauter: documentation added; also references to OPS-report. In close cooperation
!                        with Hans van Jaarsveld.
!                        DISCLAIMER: although care has been taken to make the documentation as clear as possible,
!                        it should be noted that documentation has been added some 20 years after the start of the model. 
!                        This means that not all references have been resolved and that in some cases, source code
!                        may have been misinterpreted. 
!
!=======================
! Structure of ops_main
!=======================
!-------------------------------------------------------------------------------------------------------------------------
! Initialisation
!-------------------------------------------------------------------------------------------------------------------------
! ops_get_arg            : Read program arguments
! MakeMonitorNames       : Make file names for process monitoring
! alloc                  : Allocate memory for catsel and landsel
! ops_read_ctr           : Read variables from control file
! ops_gen_fnames         : Generate full file names
! ops_read_emis          : Read source (emission) file and copy selected sources to scratch
! ops_building_file_names: Set file names for building effect tables
! ops_read_meteo         : Read meteo statistics
! ReadAps                : Read roughness length (z0) grids for NL and Europe and land use values
! ops_read_bg            : Read background concentrations for SO2, NH3, NO2
! ops_get_dim            : Determine grid dimensions nrcol and nrrow
! alloc, allocate        : Allocate memory for xm, ym, zm, frac, jump, namrcp, z0_rcp_all, lu_rcp_dom_all
! ops_gen_rcp            : Generate receptor grid
! ops_init               : Initialisation
! alloc                  : Allocate miscellaneous arrays for receptor points
! ops_rcp_char_all       : Fill arrays with roughness length, landuse and rhno3_rcp, nh3bg_rcp, f_subsec_rcp, domlu for all receptor points
! alloc                  : Allocate other arrays for receptor points and help arrays for particles
! Zeroise summed parameters, set maxidx = number of particle classes
!
!-------------------------------------------------------------------------------------------------------------------------
! Computation of concentration/deposition at all receptors, contributed from all emission sources
!-------------------------------------------------------------------------------------------------------------------------
! start loop over source data blocks of length LSBUF (until end-of-file of scratch file with source data)
! |
! | ops_bron_rek         : read source characteristics from scratch file
! | Loop over all receptor points
! | | ops_rcp_char_1     : Retrieve landuse values for this receptor
! | | Loop over nsbuf sources in the buffer
! | | | ops_src_char     : compute source characteristics
! | | | ops_tra_char     : compute trajectory characteristics
! | | | ops_reken        : compute contribution of current emission source to concentration and deposition at current receptor
! | | - end loop over sources in buffer
! | -  end loop over receptors
! -  end loop over source data blocks
!
!-------------------------------------------------------------------------------------------------------------------------
! Postprocessing and finalisation
!-------------------------------------------------------------------------------------------------------------------------
! dealloc                 : Deallocate memory 
! Allocate memory for variables that are used for different output purposes and initialise them to zero
! Loop over particle size classes that are relevant for producing output fields:
! | Get output fields
! | ops_outp_prep           : Compute variables used in different outputs
! | ops_calc_stats          : Compute (grid) statistics
! | Open print output file (= PRNFILE in control file)
! | ops_print_recep         : print output for receptors
! | ops_print_grid          : print output for gridded receptors
! | Open plot file (= PLTFILE in control file)
! | ops_plot_uitv           : write data to PLTFILE
! - end loop over particle size classes for which to produce output fields
! ops_print_info            : Write additional data to print file (= PRNFILE in control file)
! deallocation and error handling
!-------------------------------------------------------------------------------------------------------------------------------
PROGRAM ops_main

use m_ops_building
use m_aps
use m_depac318
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
use m_ops_bron_rek
use m_ops_calc_stats
use m_ops_gen_fnames
use m_ops_get_arg
use m_ops_get_dim
use m_ops_init
use m_ops_outp_prep
use m_ops_plot_uitv
use m_ops_print_grid
use m_ops_print_info
use m_ops_print_recep2
use m_ops_rcp_char_1
use m_ops_rcp_char_all
use m_ops_read_bg
use m_ops_read_ctr
use m_ops_read_emis
use m_ops_read_meteo
use m_ops_reken
use m_ops_src_char
use m_ops_tra_char
use m_ops_write_progress

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_main')

! LOCAL VARIABLES
INTEGER*4                                        :: diag
INTEGER*4                                        :: numbron
INTEGER*4                                        :: ncatsel                    ! number of categories selected
INTEGER*4                                        :: nlandsel                   ! number of countries selected
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
INTEGER*4                                        :: usdv   
INTEGER*4                                        :: iseiz                       
INTEGER*4                                        :: icm                         
INTEGER*4                                        :: nsubsec                    ! number of sub-secondary species                       
INTEGER*4                                        :: nrrcp  
INTEGER*4                                        :: nrcol  
INTEGER*4                                        :: nrrow  
INTEGER*4                                        :: intpol                      
INTEGER*4                                        :: kdeppar                     
INTEGER*4                                        :: knatdeppar                  
INTEGER*4                                        :: ideh                        
INTEGER*4                                        :: ircp
INTEGER*4                                        :: mmm
INTEGER*4                                        :: ndone
INTEGER*4                                        :: lu_rcp_dom                 ! dominant landuse class at receptor point
REAL*4                                           :: lu_tra_per(NLU)            ! percentages of landuse classes over trajectorie
REAL*4                                           :: lu_rcp_per(NLU)            ! percentages of landuse classes at receptor points
INTEGER*4                                        :: year                        
INTEGER*4                                        :: memdone                     
INTEGER*4                                        :: maxidx                     ! max. value of NPARTCLASS
INTEGER*4                                        :: ierr                       ! error code for array allocation
INTEGER*4                                        :: todo
INTEGER*4                                        :: ntodo
REAL*4                                           :: aind                       ! hourglass
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
REAL*4                                           :: xc                          
REAL*4                                           :: yc                          
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
LOGICAL                                          :: building_present1         ! at least one building is present in the source file   
REAL*4                                           :: emis(6,NLANDMAX) 
REAL*4                                           :: conc_cf
REAL*4                                           :: astat(NTRAJ, NCOMP, NSTAB, NSEK)  
REAL*4                                           :: ar                          
REAL*4                                           :: rno2nox                     
REAL*4                                           :: uurtot                    ! total number of hours from meteo statistics
REAL*4                                           :: zf                          
REAL*4                                           :: trafst(NTRAJ)               
REAL*4                                           :: bqrv(LSBUF)                 
REAL*4                                           :: bqtr(LSBUF)                 
REAL*4                                           :: cs(NTRAJ, NCOMP, NSTAB, NSEK, NMETREG)  
REAL*4                                           :: rainreg(NMETREG)            
REAL*4                                           :: z0_metreg(NMETREG)    ! roughness lengths of NMETREG meteo regions; scale < 50 km [m]           
REAL*4                                           :: xreg(NMETREG)               
REAL*4                                           :: yreg(NMETREG)               
REAL*4                                           :: hourreg(NMETREG)            
REAL*4                                           :: ecvl(NSTAB, NTRAJ,2*MAXDISTR) 
REAL*4                                           :: dverl(NHRBLOCKS,MAXDISTR)   
REAL*4                                           :: usdverl(NHRBLOCKS,MAXDISTR) 
REAL*4                                           :: pmd(NPARTCLASS,MAXDISTR)    
REAL*4                                           :: uspmd(NPARTCLASS,MAXDISTR)  
REAL*4                                           :: amol1                      
REAL*4                                           :: emtrend                    
REAL*4                                           :: grid                       
REAL*4                                           :: wdeppar                    
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
REAL*4                                           :: ddeppar                    
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
CHARACTER*80                                     :: runid                      
CHARACTER*80                                     :: namsec                     
CHARACTER*80, DIMENSION(:), POINTER              :: nam_subsec                  
CHARACTER*80                                     :: nam_pri_sec                ! name of primary + secondary component (SOx, NOy, NHy)     
CHARACTER*10                                     :: coneh                      
CHARACTER*10                                     :: depeh                      
CHARACTER*80                                     :: dll_version                     
CHARACTER*80                                     :: dll_date               

LOGICAL*4                                        :: f_z0user                   
LOGICAL                                          :: presentcode(MAXDISTR,4)     
LOGICAL                                          :: verb                       
LOGICAL                                          :: isec                       
LOGICAL                                          :: igrens                     
LOGICAL                                          :: igrid                      ! include receptor values in the Report output; is set with INCLUDE in ctr-file or -v option
LOGICAL                                          :: checked                    
LOGICAL                                          :: irev                       
LOGICAL                                          :: gasv                       
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
!LOGICAL                                          :: iscell                     ! whether point is inside masker grid

INTEGER*4, DIMENSION(:), POINTER                 :: catsel                     ! selection of categories (0: all categories)
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
REAL*4,    DIMENSION(:), POINTER                 :: so2bg_rcp                  
REAL*4,    DIMENSION(:), POINTER                 :: cno2                       ! NO2 concentration (derived from NOx and parameterised ratio NO2/NOx)


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
REAL*4,    DIMENSION(:,:), POINTER               :: parout_val                 ! values for extra output parameters [nparout,nrrcp]
CHARACTER(len= 128)                              :: parout_name(nparout)       ! names of extra output parameters                      
CHARACTER(len=  40)                              :: parout_unit(nparout)       ! units of extra output parameters                      
LOGICAL                                          :: parout_write = .false.      ! write parout parameters to output
REAL*4                                           :: parout_disx = 50000.0       ! source receptor distance used for parout parameters [m]


CHARACTER*12, DIMENSION(:), POINTER              :: namrcp                     ! receptor names

TYPE (TApsGridInt)                               :: z0nlgrid                   ! map of roughness lengths in NL [m]
TYPE (TApsGridInt)                               :: lugrid                     ! grid with land use class information (1: dominant land use, 2:NLU+1: percentages land use class)
TYPE (TApsGridInt)                               :: z0eurgrid                  ! map of roughness lengths in Europe [m]
TYPE (TApsGridReal)                              :: so2bggrid                  
TYPE (TApsGridReal)                              :: no2bggrid                  
TYPE (TApsGridReal)                              :: nh3bggrid                  
TYPE (TApsGridReal)                              :: f_subsec_grid              ! grids of fractions for sub-secondary species, HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total [-]                                                                                                                                                                                   
TYPE (TApsGridReal)                              :: masker                     
TYPE (TError)                                    :: error                      
!
!
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

! Initialise
error%debug    = .FALSE.   ! if true -> debug parameters are written to screen; only useful for a limited number of receptors and sources
verb           = .FALSE.
error%haserror = .FALSE.   ! no error detected yet
reken          = .TRUE.
!
! Read program arguments and determine the name of the control file, which may be derived from the current working directory.
! As a first parameter the diag flag is returned.
!
CALL ops_get_arg (diag, subbron, domlu, varz, perc, mindist, maxdist, error)
IF (error%haserror) GOTO 1000 ! GOTO error handling at end of program
IF (diag == 1 .OR. diag == 3) THEN
#ifndef UNIX
  WRITE(IOB_STDOUT,*) 'OPS-version: W-',MODVERSIE(1:LEN_TRIM(MODVERSIE)),' ; Release date: ', RELEASEDATE(1:11)
#else
  WRITE(IOB_STDOUT,*) 'OPS-version: L-',MODVERSIE(1:LEN_TRIM(MODVERSIE)),' ; Release date: ', RELEASEDATE(1:11)
#endif
  IF (diag == 3) THEN
    WRITE(IOB_STDOUT,*) "dll's used by OPS:"
    CALL get_version_core(dll_version, dll_date)
    WRITE(IOB_STDOUT,*) 'ops_core  version: ',dll_version(1:LEN_TRIM(dll_version)),'; Release date: ', dll_date(1:11) 
    CALL get_version_depac(dll_version, dll_date)
    WRITE(IOB_STDOUT,*) 'depac     version: ',dll_version(1:LEN_TRIM(dll_version)),'; Release date: ', dll_date(1:11) 
    CALL get_version_utils(dll_version, dll_date)
    WRITE(IOB_STDOUT,*) 'ops_utils version: ',dll_version(1:LEN_TRIM(dll_version)),'; Release date: ', dll_date(1:11) 
  ENDIF
  GOTO 1000 ! GOTO error handling at end of program
ELSEIF (diag == 2) THEN
  verb = .TRUE.
ELSE
  continue
ENDIF

WRITE (IOB_STDOUT,*) 'Verbose is: ', verb
!
! Make the file names for process monitoring (log, error and progress files)
!
CALL MakeMonitorNames(error)
IF (error%haserror) GOTO 1000 ! GOTO error handling at end of program
!
! Allocate memory for catsel and landsel
!
CALL alloc(NCATMAX,catsel,error)
CALL alloc(NLANDMAX,landsel,error)
IF (error%haserror) GOTO 2900 ! GOTO deallocate catsel, landsel and error handling at end of program
!
! Read variables from control file
!
CALL ops_read_ctr(project, runid, year, icm, namco, amol1, gasv, idep, kdeppar, ddeppar, knatdeppar, wdeppar, dg, irev,        &
               &  vchemc, iopt_vchem, vchemv, emtrend, ncatsel, catsel, nlandsel, landsel, spgrid, xc, yc, nrcol, nrrow, grid, igrens,     &
               &  z0_user, intpol, ideh, igrid, checked, f_z0user, isec, nsubsec, error)
IF (error%haserror) GOTO 1000 ! GOTO error handling at end of program
!
! Generate full file names of those files that were not set explicitly in the control file.
! Also check the existence of these files.
!
CALL ops_gen_fnames(gasv, spgrid, intpol, error)
IF (error%haserror) GOTO 1000 ! GOTO error handling at end of program
!
! Read source file and copy selected sources to scratch
!
CALL ops_read_emis(icm, gasv, ncatsel, catsel, nlandsel, landsel, numbron, dverl, usdverl, pmd, uspmd, dv,       &
                &  usdv, presentcode, building_present1, error)
IF (error%haserror) GOTO 1000 ! GOTO error handling at end of program

! Set file names for building effect tables:
if (building_present1) then
   call ops_building_file_names(error)
   IF (error%haserror) GOTO 1000 ! GOTO error handling at end of program
endif

! Read meteo statistics:
CALL ops_read_meteo (intpol, jb, mb, idb, jt, mt, idt, uurtot, iseiz, zf, astat, trafst, gemre, z0_metreg_user, cs, rainreg,   &
                  &  z0_metreg, xreg, yreg, hourreg, error)
IF (error%haserror) GOTO 1000 ! GOTO error handling at end of program
!
! Read roughness length (z0) grids for NL and Europe and land use information:
IF (.NOT. f_z0user) THEN
   CALL ops_read_z0_landuse(isec, z0nlgrid, z0eurgrid, lugrid, error)
   IF (error%haserror) GOTO 1000 ! GOTO error handling at end of program
ENDIF
!
! Read background concentrations for SO2, NH3, NO2
!
IF (isec) THEN
  allocate(nam_subsec(nsubsec))
  allocate(scale_subsec(nsubsec))
  allocate(gem_subsec(nsubsec))                               
  CALL ops_read_bg(icm, iopt_vchem, nsubsec, year, so2bggrid, no2bggrid, nh3bggrid, f_subsec_grid, vchem2, error)
  IF (error%haserror) GOTO 1000 ! GOTO error handling at end of program
else
  ! allocate to zero size to prevent runtime errors
  allocate(nam_subsec(0))
  allocate(scale_subsec(0))
  allocate(gem_subsec(0))
  IF (error%haserror) GOTO 1000 ! GOTO error handling at end of program
ENDIF
!
! Determine grid dimensions nrcol and nrrow.
!
CALL ops_get_dim(spgrid, igrens, xc, yc, grid, nrcol, nrrow, nrrcp, xul_cell_centre, yul_cell_centre, masker, error)
IF (error%haserror) GOTO 3000 !  deallocate some arrays and do error handling at end of program
!
! Allocate memory for xm, ym, zm, frac and jump. jump requires one extra element.
!
CALL alloc(nrrcp, xm, error)
CALL alloc(nrrcp, ym, error)
CALL alloc(nrrcp, zm, error)
CALL alloc(nrrcp, frac, error)
CALL alloc(nrrcp, NLU, lu_rcp_per_user_all, error)
CALL alloc(nrrcp+1, 1, jump, error)  ! fill jump with value 1
IF (error%haserror) GOTO 3300 ! GOTO deallocate all arrays and do error handling at end of program.
!
! Allocation of namrcp. Because generic allocation does not work for strings with deferred length under UNIX, the full
! allocation code is written here (instead of calling alloc).
!
ALLOCATE(namrcp(nrrcp), stat=ierr)
CALL AllocError(ierr, ROUTINENAAM, nrrcp, 'string', error)
CALL alloc(nrrcp, z0_rcp_all, error)
CALL alloc(nrrcp, lu_rcp_dom_all, error)
IF (error%haserror) GOTO 3300 ! GOTO deallocate all arrays and do error handling at end of program.
!
! Generate receptor grid.
!
namrcp = ' '
CALL ops_gen_rcp(spgrid, igrens, masker, grid, nrcol, nrrow, nrrcp, xul_cell_centre, yul_cell_centre, jump, xm, ym, zm, frac, namrcp,                &
              &  lu_rcp_dom_all, z0_rcp_all, lu_rcp_per_user_all, varz, perc, error)
IF (error%haserror) GOTO 3300 ! GOTO deallocate all arrays and do error handling at end of program.
!
! Initialisation
!
CALL ops_init   (gasv, idep, building_present1, kdeppar, knatdeppar, ddeppar, wdeppar, amol2, ideh, icm, isec, nsubsec, iseiz, mb, astat, dverl,           &
              &  usdverl, dv, usdv, namco, amol1, dg, irev, vchemc, vchemv, emtrend, rc_user, coneh, amol21, depeh, namsec, &
              &  nam_pri_sec, ugmoldep, scavcoef, rc_no, rhno2, rc_hno3, routsec, routpri, conc_cf, koh, croutpri, &
              &  ar, rno2nox, ecvl, nam_subsec, buildingEffect, error)

IF (error%haserror) GOTO 3300 ! GOTO deallocate all arrays and do error handling at end of program.

! Allocate miscellaneous arrays for receptor points
CALL alloc(nrrcp, gxm, error)
CALL alloc(nrrcp, gym, error)

CALL alloc(nrrcp, nh3bg_rcp, error)
CALL alloc(nrrcp, so2bg_rcp, error)								   
CALL alloc(nrrcp, rhno3_rcp, error)
CALL alloc(nrrcp, nsubsec, f_subsec_rcp, error)                                              

IF (error%haserror) GOTO 3300 ! GOTO deallocate all arrays and do error handling at end of program.
!
! Fill arrays with roughness length, landuse and rhno3_rcp, nh3bg_rcp, f_subsec_rcp, domlu for all receptor points
!
CALL ops_rcp_char_all(icm, iopt_vchem, isec, nsubsec, xm, ym, f_z0user, z0_user, z0nlgrid, z0eurgrid, lugrid, so2bggrid, nh3bggrid, f_subsec_grid, &
                    &  nrrcp, namrcp, gxm, gym, lu_rcp_dom_all, z0_rcp_all, rhno3_rcp, nh3bg_rcp, so2bg_rcp, f_subsec_rcp, domlu, error)
IF (error%haserror) GOTO 3300 ! GOTO deallocate all arrays and do error handling at end of program.

! Allocate other arrays for receptor points;
! directly after deallocating memory for different grids, some other receptor-vectors are allocated (see below).
!
CALL alloc(nrrcp, precip, error)
IF (error%haserror) GOTO 3300 ! GOTO deallocate all arrays and do error handling at end of program.

! Open the progress file and write 0.0 progression to screen.
! Numbs (= # characters to backspace for screen progress indicator) is 11 for this first progress call.
memdone = -2

IF (.NOT.sysopen(fu_progress, indnam, 'w', 'progress file', error)) GOTO 3300 ! GOTO deallocate all arrays and do error handling at end of program.
CALL ops_write_progress(0.0, '('' OPS: '',F5.1,''% done'')', 11, memdone)
!
! Allocate "helparrays" for particles
!
CALL alloc(NPARTCLASS, 0.0, somvnpri_arr, error)
CALL alloc(NPARTCLASS, 0.0, telvnpri_arr, error)
CALL alloc(NPARTCLASS, 0.0, somvnsec_arr, error)
CALL alloc(NPARTCLASS, 0.0, telvnsec_arr, error)
CALL alloc(NPARTCLASS, 0.0, vvchem_arr, error)
CALL alloc(NPARTCLASS, 0.0, vtel_arr, error)
CALL alloc(NPARTCLASS, 0.0, sdrypri_arr, error)
CALL alloc(NPARTCLASS, 0.0, sdrysec_arr, error)
CALL alloc(NPARTCLASS, 0.0, snatpri_arr, error)
CALL alloc(NPARTCLASS, 0.0, snatsec_arr, error)
CALL alloc(nrrcp, NPARTCLASS, cpri_d, error)
CALL alloc(nrrcp, NPARTCLASS, csec_d, error)
CALL alloc(nrrcp, NPARTCLASS, drydep_d, error)
CALL alloc(nrrcp, NPARTCLASS, wetdep_d, error)
CALL alloc(nrrcp, NPARTCLASS, ddepri_d, error)
CALL alloc(nrrcp, NPARTCLASS, wdepri_d, error)
CALL alloc(nparout,nrrcp,parout_val,error)
if (idep) then
   CALL alloc(nrrcp, 0., cno2, error)
else
   CALL alloc(nrrcp, float(MISVALNUM), cno2, error) ! no deposition -> ratio NO2/NOx is not set 
endif
IF (error%haserror) GOTO 3300

! Zeroise summed parameters:
somvnpri = 0.0
telvnpri = 0.0
somvnsec = 0.0
telvnsec = 0.0
vvchem   = 0.0
vtel     = 0.0
sdrypri  = 0.0
sdrysec  = 0.0
snatpri  = 0.0
snatsec  = 0.0
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
IF (gasv) THEN
  maxidx = 1
ELSE
  maxidx = NPARTCLASS
ENDIF
!
! start loop over source data blocks of length LSBUF (until end-of-file of scratch file with source data)
!
DO WHILE (.NOT. eof)
  !
  ! read source characteristics from scratch file and fill into buffer arrays (source data are read in
  ! blocks of length LSBUF (LSBUF=4000))
  !
  CALL ops_bron_rek (emtrend, buildingEffect, landmax, emis, nsbuf, bnr, bx, by, bdiam, bsterkte, bwarmte, bhoogte, bsigmaz, bD_stack, bV_stack, bTs_stack, bemis_horizontal, bbuilding, btgedr,        &
                  &  bdegr, bqrv, bqtr, bcatnr, blandnr, eof, error)

  IF (error%haserror) GOTO 3300 ! GOTO deallocate all arrays and do error handling at end of program.

  ! Adjust number of processed sources
  nbron = nbron + nsbuf
  
  ! Loop over all receptor points  ++++++++++++++++++++++++  
  ndone = 0
  DO ircp = 1, nrrcp

    ! Retrieve z0 and landuse values for this receptorpoint:
    CALL ops_rcp_char_1 (isec, ircp, nrrcp, intpol, gxm(ircp), gym(ircp), cs, z0_metreg, xreg, yreg, astat, z0_metreg_user,      &
                      &  spgrid, xm(ircp), ym(ircp), lugrid, domlu, perc, lu_rcp_per_user_all, lu_rcp_dom_all, f_z0user, z0_rcp_all, &
                      &  uurtot, z0_metreg_rcp, lu_rcp_per, lu_rcp_dom, z0_rcp, error)
    IF (error%haserror) GOTO 3300 ! GOTO deallocate all arrays and do error handling at end of program.

    ! Define a circle of NSEK sources arount a receptor point, used for writing parout-variables:
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
            GOTO 1000
         ENDIF
         !
         ! compute trajectory characteristics
         !
         CALL ops_tra_char (icm, iopt_vchem, f_z0user, z0_user, xm(ircp), ym(ircp), bx(mmm), by(mmm), &
                         &  lugrid, z0nlgrid, z0eurgrid, so2bggrid, no2bggrid, nh3bggrid, vchem2, domlu, & 
                         &  z0_tra, lu_tra_per, so2bgtra, no2bgtra, nh3bgtra, error)
         IF (error%haserror) THEN
            CALL ErrorParam('source number',mmm,error)
            CALL ErrorParam('receptor number',ircp,error)
            CALL ErrorParam('receptor name',trim(namrcp(ircp)),error)
            GOTO 1000
         ENDIF
         
         if (.not. building_present1) then
            ! allocate building arrays to zero size, they're passed even when there is no
            ! building.
            call ops_building_alloc_zero(bbuilding(mmm), error)
         endif
         
         ! Compute contribution of current emission source to concentration and deposition at current receptor:
         CALL ops_reken(idep, isec, icm, gasv, vchemc, iopt_vchem, vchemv, dv, amol1, amol2, amol21, ar, rno2nox, ecvl, iseiz, zf, &
                     &  trafst, knatdeppar, mb, ugmoldep, dg, irev, scavcoef, koh, croutpri, rc_no, rhno2, rc_hno3, &
                     &  nrrcp, ircp, gxm(ircp), gym(ircp), xm(ircp), ym(ircp), zm(ircp), &
                     &  frac(ircp), nh3bg_rcp(ircp), so2bg_rcp(ircp), rhno3_rcp(ircp), & 
                     &  bqrv(mmm), bqtr(mmm), bx(mmm), by(mmm), bdiam(mmm), bsterkte(mmm), bwarmte(mmm), bhoogte(mmm), &
                     &  bsigmaz(mmm), bD_stack(mmm), bV_stack(mmm), bTs_stack(mmm), bemis_horizontal(mmm), bbuilding(mmm), &
                     &  buildingEffect,btgedr(mmm), bdegr(mmm), & 
                     &  z0_src, z0_tra, z0_rcp, z0_metreg_rcp, lu_tra_per, &
                     &  lu_rcp_per, so2sek, no2sek, so2bgtra, no2bgtra, nh3bgtra,  vchem2, maxidx, pmd, uspmd, spgrid, grid, &
                     &  subbron, uurtot, routsec, rc_user, somvnsec_arr, telvnsec_arr, vvchem_arr, vtel_arr, somvnpri_arr, &
                     &  telvnpri_arr, ddepri_d, wdepri_d, sdrypri_arr, snatpri_arr, sdrysec_arr, snatsec_arr, & 
                     &  cpri_d, csec_d, drydep_d, wetdep_d, astat, cno2, precip(ircp), routpri, dispg, &
                     &  nparout, parout_val(:,ircp), parout_name, parout_unit, parout_write, error)
         IF (error%haserror) GOTO 3300 ! GOTO deallocate all arrays and do error handling at end of program.
      ENDIF
	  reken = .TRUE.

    ENDDO   ! end loop over sources in buffer
    !
    ! Write progress (update each 2%)
    !
    ndone = ndone+1
    aind= 100.*FLOAT(nbron-nsbuf)/FLOAT(numbron)+ (100.*FLOAT(nsbuf)/FLOAT(numbron))* (FLOAT(ndone)/FLOAT(nrrcp))
    CALL ops_write_progress(aind, '(F5.1)', 5, memdone)

  ENDDO  ! end loop over receptors
ENDDO ! end loop over source data blocks of length LSBUF (until end-of-file of scratch file with source data)
CLOSE (fu_progress)
!
! Deallocate memory not required anymore and close the progression file.
! Remark: If an error is detected these statements are skipped in the execution. That is not a problem, as deallocation of
! memory and closing of files are then automatically performed after execution is terminated.
!
CALL dealloc(lugrid)
CALL dealloc(z0eurgrid)
CALL dealloc(z0nlgrid)
CALL dealloc(nh3bggrid)
CALL dealloc(no2bggrid)
CALL dealloc(so2bggrid)
CALL dealloc(nh3bg_rcp)
CALL dealloc(so2bg_rcp)					   

CALL dealloc(gxm)
CALL dealloc(gym)
CALL sysclose(fu_progress, indnam, error)
IF (error%haserror) GOTO 3300 ! GOTO deallocate all arrays and do error handling at end of program.
!
! close scratch file for sources
!
!CALL sysclose(fu_scratch, 'sources scratch file', error)
!IF (error%haserror) GOTO 3300 ! GOTO deallocate all arrays and do error handling at end of program.
!
! Allocate memory for variables that are used for different output purposes and initialise them to zero:
!
CALL alloc(nrrcp, 0., cpri, error)
CALL alloc(nrrcp, 0., csec, error)
CALL alloc(nrrcp, 0., drydep, error)
CALL alloc(nrrcp, 0., wetdep, error)
CALL alloc(nrrcp, 0., ddepri, error)
CALL alloc(nrrcp, 0., wdepri, error)
CALL alloc(nrrcp, 0., totdep, error)
CALL alloc(nrrcp, nsubsec, csubsec, error); if (nsubsec .gt. 0) csubsec = 0.0

! ntodo: number of particle size classes that are relevant for producing output fields
! Default value for ntodo (for gas): 
ntodo = 1
!
! For non-gaseous components we use all 6 particle size classes,
! but produce only one summed field for concentration, dry deposition, wet deposition and total deposition.
!
IF (.not.gasv) then
  ntodo = 6
ENDIF
!
! For PM (code 24) only 4 particle size classes are relevant: PM1.0, PM2.5, PM4.0 and PM10.
! Only for the classes PM2.5 and PM10, OPS produces output of the concentration field into a file.
!
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
    CALL ops_outp_prep (nrrcp, icm, nsubsec, conc_cf, f_subsec_rcp, csec, drydep, wetdep, cpri, totdep, csubsec, scale_con, scale_sec,      &
        &  scale_subsec, scale_dep)

    ! Compute (grid) statistics:
    CALL ops_calc_stats (nrrcp, nsubsec, frac, cpri, csec, drydep ,wetdep, gemre, sdrypri, sdrysec, snatpri, snatsec, somvnpri, &
         &  somvnsec, vvchem, vtel, telvnpri, telvnsec, grid, amol21, ugmoldep, csubsec, gemcpri, gemcsec, totddep, &
         &  gemddep, gemddpri, gemddsec, ddrpri, ddrsec, totwdep, gemwdep, gemwdpri, gemwdsec, wdrpri, wdrsec, &
         &  gemprec, tottdep, gemtdep, ccr, gem_subsec)

    ! Open print output file (= PRNFILE in control file) and write data:
    IF (.not.outputfile_opened) THEN
      IF (.NOT.sysopen(fu_prt, prnnam, 'w', 'print file', error)) GOTO 3300 ! GOTO deallocate all arrays and do error handling at end of program.
    ENDIF
    IF (spgrid .EQ. 2) THEN
      CALL ops_print_recep2(project, icm, gasv, idep, isec, igrid, verb, namco, namsec, nam_pri_sec, coneh, depeh, conc_cf, amol21, &
                        &  ugmoldep, nrrcp, nsubsec, namrcp, xm, ym, precip, cpri, csec, drydep, ddepri, wetdep, wdepri, cno2, &
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
    IF (error%haserror) GOTO 4000

    ! Open plot file (= PLTFILE in control file) and write data: 
    IF (.not.outputfile_opened) THEN
      IF (.NOT. sysopen(fu_plt, pltnam, 'w', 'plot file', error)) GOTO 3300
    ENDIF
    CALL ops_plot_uitv(spgrid, isec, coneh, nrrcp, nsubsec, jump, xul_cell_centre, yul_cell_centre, nrcol, nrrow, grid, idep, namco, nam_pri_sec, namsec, depeh,    &
                    & namrcp, xm, ym, cpri, csec, drydep, ddepri, wdepri, wetdep, cno2, icm, csubsec, nam_subsec,  &
                    & nparout, parout_val, parout_name, parout_unit, parout_write, error)
    IF (error%haserror) GOTO 4000
    outputfile_opened = .TRUE.
  ENDIF  !  if output is requested
ENDDO ! End loop over particle size classes for which to produce output fields

! Write additional data to print file (= PRNFILE in control file):
CALL ops_print_info (project, gasv, isec, intpol, spgrid, z0_rcp, namco, nbron, bnr, bx, by, bsterkte, bqrv, bqtr, bwarmte,     &
    &  bhoogte, bdiam, bsigmaz, btgedr, bdegr, bcatnr, blandnr, emis, emtrend, jb, mb, idb, jt, mt, idt, iseiz,                 &  
    &  f_z0user, landmax, subbron, domlu, varz, perc, mindist, maxdist, error)

IF (error%haserror) GOTO 4000

! Close print file. This file is closed only if it was opened:
4000 CALL sysclose(fu_prt, prnnam,  error)
!
!------------------------------------------------------------------------------
! label 3300: deallocate all arrays and do error handling at end of program.
! De-allocation is in opposite order of allocation because of the error handling labels.
!------------------------------------------------------------------------------
3300 CALL dealloc(csec)
CALL dealloc(cpri)
CALL dealloc(drydep)
CALL dealloc(wetdep)
CALL dealloc(z0_rcp_all)
CALL dealloc(lu_rcp_dom_all)
CALL dealloc(precip)
CALL dealloc(rhno3_rcp)
CALL dealloc(cno2)

!------------------------------------------------------------------------------
! label 3000: deallocate some arrays and do error handling at end of program
!------------------------------------------------------------------------------
3000 CALL dealloc(xm)
CALL dealloc(ym)
CALL dealloc(frac)
CALL dealloc(namrcp)
CALL dealloc(jump)


!---------------------------------------------------------------------------------
! label 2900: deallocate catsel, landsel and do error handling at end of program
!---------------------------------------------------------------------------------
2900 CALL dealloc(catsel)
CALL dealloc(landsel)

!-----------------------------
! label 1000: error handling
!-----------------------------
!
! Close log file
!
1000 CALL ops_closelog(error)

IF (error%haserror) THEN
!
! Open error file and write error message to file.
!
  CALL ErrorCall(ROUTINENAAM, error)
  IF (sysopen(fu_err, errnam, 'w', 'error file', error) ) THEN
    CALL WriteError(fu_err, error)
    CALL sysclose(fu_err, errnam, error)
  ENDIF
  CALL EXIT(1)
ELSE
  CALL EXIT(0)
ENDIF

END PROGRAM ops_main
