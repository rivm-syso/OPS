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

module m_ops_save_results

! module to post-process output fields and save them to the filesystem
!-------------------------------------------------------------------------------------------------------------------------  
! Postprocessing and finalisation
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

implicit none

contains

!-----------------------------------------------------------------------------
subroutine ops_save_results(varin, nrrcp, nsubsec, jump, gasv, idep, icm, do_proc, road_chem, road_disp, nemcat_road, class_output, allow_sigz0_point_source, &
                            namco, namsec, nam_pri_sec, &
                            nam_subsec, namrcp, xm, ym, nrcol, nrrow, xul_cell_centre, yul_cell_centre, &
                            precip, lu_rcp_dom_all, z0_rcp_all, intpol, &
                            cpri_d, cpri_class, percvk_class, nsrc_class, csec_d, drydep_d, wetdep_d, &
                            ddepri_d, wdepri_d, sdrypri_arr, snatpri_arr, somvnpri_arr, &
                            telvnpri_arr, sdrysec_arr, snatsec_arr, somvnsec_arr, telvnsec_arr, &                            
                            vvchem_arr, vtel_arr, conc_cf, f_subsec_rcp, cnox, cno2, &
                            scale_con, scale_sec, scale_subsec, scale_dep, frac, &
                            ugmoldep, gemre, gemcpri, gemcsec, totddep, gemddep, &
                            gemddpri, gemddsec, ddrpri, ddrsec, totwdep, gemwdep, &
                            gemwdpri, gemwdsec, wdrpri, wdrsec, tottdep, gemtdep, gemprec, &
                            grid, amol21, spgrid, project, coneh, depeh, &
                            nparout, parout_name, parout_val, parout_unit, parout_write, &
                            ccr, gem_subsec, isec, igrid, verb, trafst, &
                            nbron, bnr, bx, by, bsterkte, bqrv, bqtr, &
                            bwarmte, bhoogte, bdiam, bsigmaz, btgedr, bdegr, bcatnr, blandnr, emis, emtrend, &
                            jb, mb, idb, jt, mt, idt, iseiz,    &
                            f_z0user, landmax, subbron, domlu, varz, perc, mindist, maxdist, dir_chem, fnames_used_chem, dir_bg, &
                            error)

use m_error
use m_commonfile
use m_commonconst_lt
use m_ops_outp_prep
use m_ops_calc_stats
use m_fileutils
use m_ops_print_recep2
use m_ops_print_grid
use m_ops_plot_uitv
use m_ops_plot_class_uitv
use m_ops_print_info
use m_ops_tdo_proc, only: Tdo_proc
use m_ops_varin

implicit none

! Input variables
TYPE(Tvarin), INTENT(IN)                   :: varin                ! input variables for meteo
INTEGER,   INTENT(IN)                            :: nrrcp                      ! number of receptors
INTEGER,   INTENT(IN)                            :: nsubsec                    ! number of sub-secondary species       
INTEGER,   INTENT(IN)                            :: jump(nrrcp+1)              ! number of successive points that can be skipped for output purposes                
LOGICAL, INTENT(IN)                              :: gasv
LOGICAL, INTENT(IN)                              :: class_output               ! indicator whether results for receptors will be stored per wind sector/distance/particle/stability class
LOGICAL,   INTENT(IN)                            :: allow_sigz0_point_source   ! allow initial sigma for point sources
INTEGER,   INTENT(IN)                            :: icm    
type(Tdo_proc),   INTENT(IN)                     :: do_proc                    ! options to switch on/off specific processes
DOUBLE PRECISION,    DIMENSION(:,:), POINTER, INTENT(IN)    :: cpri_d                     ! concentration of primary component, double precision [ug/m3]
DOUBLE PRECISION, DIMENSION(:,:,:,:,:), POINTER, INTENT(IN) :: cpri_class                 ! concentration of primary component per class, double precision [ug/m3]
DOUBLE PRECISION, DIMENSION(:,:,:,:), POINTER, INTENT(IN)   :: percvk_class               ! percvk of primary component at receptor points and height zm, per class [factor of occurrence]
INTEGER, DIMENSION(:,:,:,:), POINTER, INTENT(IN)            :: nsrc_class   ! number of sources present in wind/distance sector (-classoutput only) [-]
DOUBLE PRECISION,    DIMENSION(:,:), POINTER, INTENT(IN)    :: csec_d                     ! concentration of secondary component, double precision [ug/m3]
DOUBLE PRECISION,    DIMENSION(:,:), POINTER, INTENT(IN)    :: drydep_d                    
DOUBLE PRECISION,    DIMENSION(:,:), POINTER, INTENT(IN)    :: wetdep_d                    
DOUBLE PRECISION,    DIMENSION(:,:), POINTER, INTENT(IN)    :: ddepri_d                   
DOUBLE PRECISION,    DIMENSION(:,:), POINTER, INTENT(IN)    :: wdepri_d  
!WdVDOUBLE PRECISION,   DIMENSION(:), POINTER, INTENT(IN)        :: sdrypri_arr                                   
!WdVDOUBLE PRECISION,   DIMENSION(:), POINTER, INTENT(IN)        :: snatpri_arr                             
!WdVDOUBLE PRECISION,   DIMENSION(:), POINTER, INTENT(IN)        :: somvnpri_arr                             
!WdVDOUBLE PRECISION,   DIMENSION(:), POINTER, INTENT(IN)        :: telvnpri_arr                                
!WdVDOUBLE PRECISION,   DIMENSION(:), POINTER, INTENT(IN)        :: sdrysec_arr                                   
!WdVDOUBLE PRECISION,   DIMENSION(:), POINTER, INTENT(IN)        :: snatsec_arr                                
!WdVDOUBLE PRECISION,   DIMENSION(:), POINTER, INTENT(IN)        :: somvnsec_arr                                    
!WdVDOUBLE PRECISION,   DIMENSION(:), POINTER, INTENT(IN)        :: telvnsec_arr                              
!WdVDOUBLE PRECISION,   DIMENSION(:), POINTER, INTENT(IN)        :: vvchem_arr                                  
!WdVDOUBLE PRECISION,   DIMENSION(:), POINTER, INTENT(IN)        :: vtel_arr    
DOUBLE PRECISION,   DIMENSION(:), INTENT(IN)        :: sdrypri_arr                                   
DOUBLE PRECISION,   DIMENSION(:), INTENT(IN)        :: snatpri_arr                             
DOUBLE PRECISION,   DIMENSION(:), INTENT(IN)        :: somvnpri_arr                             
DOUBLE PRECISION,   DIMENSION(:), INTENT(IN)        :: telvnpri_arr                                
DOUBLE PRECISION,   DIMENSION(:), INTENT(IN)        :: sdrysec_arr                                   
DOUBLE PRECISION,   DIMENSION(:), INTENT(IN)        :: snatsec_arr                                
DOUBLE PRECISION,   DIMENSION(:), INTENT(IN)        :: somvnsec_arr                                    
DOUBLE PRECISION,   DIMENSION(:), INTENT(IN)        :: telvnsec_arr                              
DOUBLE PRECISION,   DIMENSION(:), INTENT(IN)        :: vvchem_arr                                  
DOUBLE PRECISION,   DIMENSION(:), INTENT(IN)        :: vtel_arr    
REAL,   INTENT(IN)                               :: conc_cf       
REAL,   DIMENSION(:,:), ALLOCATABLE, INTENT(IN)  :: f_subsec_rcp               ! fractions for sub-secondary species, HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total [-]                                                                                                                                                                          
REAL,      DIMENSION(:),POINTER, INTENT(IN)      :: cnox, cno2                 ! NOx, NO2 concentration, per receptor, for output, only for ROADS (vdHout)         
REAL,      DIMENSION(:), POINTER, INTENT(IN)     :: frac                       ! fraction of output cell on land surface         
REAL,   INTENT(IN)                               :: ugmoldep                       
REAL,   INTENT(IN)                               :: grid     
REAL,   INTENT(IN)                               :: amol21   
INTEGER,   INTENT(IN)                            :: spgrid
CHARACTER*80, INTENT(IN)                         :: project
CHARACTER*10, INTENT(IN)                         :: coneh
CHARACTER*10, INTENT(IN)                         :: depeh
CHARACTER*(*), INTENT(IN)                        :: namrcp(nrrcp)             ! 
REAL,      INTENT(IN)                            :: xm(nrrcp)                  ! x-coordinates of receptors (m RDM)
REAL,      INTENT(IN)                            :: ym(nrrcp)                  ! y-coordinates of receptors (m RDM)
REAL,      INTENT(IN)                            :: precip(nrrcp)              ! total precipitation per year [mm/year]
INTEGER,   INTENT(IN)                            :: lu_rcp_dom_all(nrrcp)      ! dominant land use class for each receptor point
REAL,      INTENT(IN)                            :: z0_rcp_all(nrrcp)          ! roughness lengths for all receptors; from z0-map or receptor file [m]
INTEGER,   INTENT(IN)                            :: nrcol                      ! number of grid cells in X-dir
INTEGER,   INTENT(IN)                            :: nrrow                      ! number of grid cells in Y-dir
REAL,      INTENT(IN)                            :: xul_cell_centre            ! x-coordinate of centre of upper-left grid cell [m] 
REAL,      INTENT(IN)                            :: yul_cell_centre            ! y-coordinate of centre of upper-left grid cell [m] 
INTEGER,   INTENT(IN)                            :: nparout         ! number of extra output parameters (besides concentration, deposition)
REAL,      DIMENSION(:,:), POINTER, INTENT(IN)   :: parout_val      ! values for extra output parameters [nrrcp,nparout]
CHARACTER(len= 128), DIMENSION(:), INTENT(IN)    :: parout_name     ! names of extra output parameters                      
CHARACTER(len=  40), DIMENSION(:), INTENT(IN)    :: parout_unit     ! units of extra output parameters                      
LOGICAL, INTENT(IN)                              :: parout_write    ! write parout parameters to output
REAL,   DIMENSION(:), POINTER, INTENT(IN)        :: gem_subsec                 ! grid mean for concentration of sub-secondary species [ug/m3]     
LOGICAL,   INTENT(IN)                            :: isec                       ! 
LOGICAL,   INTENT(IN)                            :: verb                       ! 
CHARACTER*(*), INTENT(IN)                        :: nam_subsec(nsubsec)        ! names of sub-secondary speciea
REAL,      INTENT(IN)                            :: trafst(NTRAJ)              ! travel distances for each class
INTEGER,   INTENT(IN)                            :: intpol                     ! 
INTEGER,   INTENT(IN)                            :: nbron                      ! number of emission sources (after selection)
INTEGER,   INTENT(IN)                            :: bnr(LSBUF)                 ! buffer with source numbers
INTEGER,   INTENT(IN)                            :: bx(LSBUF)                  ! buffer with x-coordinates
INTEGER,   INTENT(IN)                            :: by(LSBUF)                  ! buffer with y-coordinates
REAL,      INTENT(IN)                            :: bsterkte(LSBUF)            ! buffer with source strengths (industrial)
REAL,      INTENT(IN)                            :: bqrv(LSBUF)                ! buffer with source strengths (space heating)
REAL,      INTENT(IN)                            :: bqtr(LSBUF)                ! buffer with source strengths (traffic)
REAL,      INTENT(IN)                            :: bwarmte(LSBUF)             ! buffer with heat contents
REAL,      INTENT(IN)                            :: bhoogte(LSBUF)             ! buffer with source heights
REAL,      INTENT(IN)                            :: bdiam(LSBUF)               ! buffer with source diameters
REAL,      INTENT(IN)                            :: bsigmaz(LSBUF)             ! buffer with source heigth variances
INTEGER,   INTENT(IN)                            :: btgedr(LSBUF)              ! buffer with diurnal variation codes
INTEGER,   INTENT(IN)                            :: bdegr(LSBUF)               ! buffer with particle size distribution codes
INTEGER,   INTENT(IN)                            :: bcatnr(LSBUF)              ! buffer with category codes
INTEGER,   INTENT(IN)                            :: blandnr(LSBUF)             ! buffer with area codes
REAL,      INTENT(IN)                            :: emtrend                    ! emission correction factor
INTEGER,   INTENT(IN)                            :: jb                         ! starting year of meteo
INTEGER,   INTENT(IN)                            :: mb                         ! starting month of meteo
INTEGER,   INTENT(IN)                            :: idb                        ! starting day of meteo
INTEGER,   INTENT(IN)                            :: jt                         ! ending year of meteo
INTEGER,   INTENT(IN)                            :: mt                         ! ending month of meteo
INTEGER,   INTENT(IN)                            :: idt                        ! ending day of meteo
INTEGER,   INTENT(IN)                            :: iseiz                      ! 
LOGICAL,   INTENT(IN)                            :: f_z0user                   ! true if z0 is user specified
LOGICAL,   INTENT(IN)                            :: subbron                    
LOGICAL,   INTENT(IN)                            :: domlu                    
LOGICAL,   INTENT(IN)                            :: varz                    
LOGICAL,   INTENT(IN)                            :: perc                    
LOGICAL,   INTENT(IN)                            :: mindist                 
LOGICAL,   INTENT(IN)                            :: maxdist   
LOGICAL,   INTENT(IN)							 :: road_chem
LOGICAL,   INTENT(IN)							 :: road_disp  
INTEGER,		INTENT(IN)						 :: nemcat_road                
CHARACTER(LEN = *), INTENT(IN)                   :: dir_chem                   ! directory where to read chemistry files from 
CHARACTER(LEN = *), INTENT(IN)                   :: fnames_used_chem           ! string with names of files used for chemistry maps
character(len=*), intent(in) :: dir_bg

! Input / output variables
LOGICAL, INTENT(INOUT)                           :: idep
CHARACTER*(*), INTENT(INOUT)                     :: namco
CHARACTER*(*), INTENT(INOUT)                     :: namsec                     ! 
CHARACTER*80, INTENT(INOUT)                      :: nam_pri_sec                ! name of primary + secondary component (SOx, NOy, NHy)     
LOGICAL,   INTENT(INOUT)                         :: igrid                      ! include receptor values in the Report output; is set with INCLUDE in ctr-file or -v option
TYPE (TError), INTENT(INOUT)                     :: error
REAL,   INTENT(INOUT)                            :: scale_con                  
REAL,   INTENT(INOUT)                            :: scale_sec                  
REAL,   DIMENSION(:), POINTER, INTENT(INOUT)     :: scale_subsec              
REAL,   INTENT(INOUT)                            :: scale_dep  
REAL,   INTENT(INOUT)                            :: gemre 
REAL,   INTENT(INOUT)                            :: gemcpri
REAL,   INTENT(INOUT)                            :: gemcsec                    
REAL,   INTENT(INOUT)                            :: totddep                    
REAL,   INTENT(INOUT)                            :: gemddep                    
REAL,   INTENT(INOUT)                            :: gemddpri                   
REAL,   INTENT(INOUT)                            :: gemddsec                   
REAL,   INTENT(INOUT)                            :: ddrpri                     
REAL,   INTENT(INOUT)                            :: ddrsec                     
REAL,   INTENT(INOUT)                            :: totwdep                    
REAL,   INTENT(INOUT)                            :: gemwdep                    
REAL,   INTENT(INOUT)                            :: gemwdpri                   
REAL,   INTENT(INOUT)                            :: gemwdsec                   
REAL,   INTENT(INOUT)                            :: wdrpri                     
REAL,   INTENT(INOUT)                            :: wdrsec                     
REAL,   INTENT(INOUT)                            :: tottdep                    
REAL,   INTENT(INOUT)                            :: gemtdep                    
REAL,   INTENT(INOUT)                            :: gemprec
REAL,   INTENT(INOUT)                            :: ccr   
REAL,      INTENT(INOUT)                         :: emis(6,NLANDMAX)
INTEGER,   INTENT(INOUT)                         :: landmax                    ! number of countries in emission file


! Local variables
! Arrays used to store the final components, summed over all particle size classes
REAL,      DIMENSION(:), POINTER                 :: cpri                       ! concentration of primary component [ug/m3]
REAL,      DIMENSION(:,:,:,:), POINTER           :: cpri_class_sum             ! concentration of primary component per class, sum over part. classes [ug/m3]
REAL,      DIMENSION(:), POINTER                 :: csec                       ! concentration of secondary component [ug/m3]
REAL,      DIMENSION(:), POINTER                 :: drydep                      
REAL,      DIMENSION(:), POINTER                 :: wetdep                      
REAL,      DIMENSION(:), POINTER                 :: ddepri                      
REAL,      DIMENSION(:), POINTER                 :: wdepri                      
REAL,      DIMENSION(:), POINTER                 :: totdep                      
REAL,      DIMENSION(:,:), POINTER               :: csubsec                    ! concentration of sub-secondary species [ug/m3]
DOUBLE PRECISION                                             :: sdrypri  
DOUBLE PRECISION                                             :: snatpri        
DOUBLE PRECISION                                             :: somvnpri    
DOUBLE PRECISION                                             :: telvnpri      
DOUBLE PRECISION                                             :: sdrysec 
DOUBLE PRECISION                                             :: snatsec  
DOUBLE PRECISION                                             :: somvnsec    
DOUBLE PRECISION                                             :: telvnsec 
DOUBLE PRECISION                                             :: vvchem     
DOUBLE PRECISION                                             :: vtel 
INTEGER                                          :: todo
INTEGER                                          :: ntodo
LOGICAL                                          :: pltfile_opened
LOGICAL                                          :: prtfile_opened  

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_save_results')

IF (class_output) THEN
    ! divide percvk_class by number of sources
    where(nsrc_class > 0)
       percvk_class = percvk_class/real(nsrc_class)
    elsewhere
       percvk_class = 0.0
    endwhere
ENDIF

! Allocate memory for variables that are used for different output purposes and initialise them to zero:
CALL alloc(nrrcp, 0., cpri, error)

IF (class_output) THEN
  CALL alloc(NSTAB, NSEK, NTRAJ, nrrcp, cpri_class_sum, error)
ENDIF

CALL alloc(nrrcp, 0., csec, error)
CALL alloc(nrrcp, 0., drydep, error)
CALL alloc(nrrcp, 0., wetdep, error)
CALL alloc(nrrcp, 0., ddepri, error)
CALL alloc(nrrcp, 0., wdepri, error)
CALL alloc(nrrcp, 0., totdep, error)
CALL alloc(nrrcp, nsubsec, csubsec, error)
if (nsubsec .gt. 0) csubsec = 0.0
if (error%haserror) goto 9999

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

! ntodo: number of particle size classes that are relevant for producing output fields
! gas      : ntodo = 1
! particles: ntodo = 6 (all particle size classes)
!            but produce only one summed field for concentration, dry deposition, wet deposition and total deposition.
IF (gasv) then
  ntodo = 1
ELSE
  ! For PM (code 24) only 4 particle size classes are relevant: PM1.0, PM2.5, PM4.0 and PM10.
  ! Only for the classes PM2.5 and PM10, OPS produces output of the concentration field into a file.
  IF (icm == icm_PM) then
    ntodo = 4
    idep  = .FALSE.
  ELSE
    ntodo = 6
  ENDIF
ENDIF

!
prtfile_opened = .FALSE.
pltfile_opened = .FALSE.

! Loop over particle size classes that are relevant for producing output fields:
DO todo = 1,ntodo

 ! Set name of component for PM-class:
 IF (icm == icm_PM) THEN
   IF (todo == 1) namco = "PM1.0"
   IF (todo == 2) namco = "PM2.5"
   IF (todo == 3) namco = "PM4.0"
   IF (todo == 4) namco = "PM10."
   nam_pri_sec = namco
 ENDIF

 ! Get output fields:
 cpri     = cpri   + SNGL(cpri_d(todo,:))
 IF (class_output) THEN
   cpri_class_sum = cpri_class_sum + SNGL(cpri_class(todo,:,:,:,:))
 ENDIF
 csec     = csec   + SNGL(csec_d(todo,:))
 drydep   = drydep + SNGL(drydep_d(todo,:))
 wetdep   = wetdep + SNGL(wetdep_d(todo,:))
 ddepri   = ddepri + SNGL(ddepri_d(todo,:))
 wdepri   = wdepri + SNGL(wdepri_d(todo,:))
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
 IF ((gasv          .and. todo == 1) .or. (.not.gasv     .and. todo == 6) .or. &
     (icm == icm_PM .and. todo == 2) .or. (icm == icm_PM .and. todo == 4)) THEN

   ! Compute variables used in different output sources:
   CALL ops_outp_prep (nrrcp, icm, nsubsec, conc_cf, f_subsec_rcp, csec, drydep, wetdep, cpri, cpri_class_sum, cnox, totdep, csubsec, scale_con, scale_sec,      &
                       scale_subsec, scale_dep, class_output)

   ! Compute (grid) statistics:
   CALL ops_calc_stats (nrrcp, nsubsec, frac, cpri, csec, drydep ,wetdep, gemre, sdrypri, sdrysec, snatpri, snatsec, somvnpri, &
        &  somvnsec, vvchem, vtel, telvnpri, telvnsec, grid, amol21, ugmoldep, csubsec, gemcpri, gemcsec, totddep, &
        &  gemddep, gemddpri, gemddsec, ddrpri, ddrsec, totwdep, gemwdep, gemwdpri, gemwdsec, wdrpri, wdrsec, &
        &  gemprec, tottdep, gemtdep, ccr, gem_subsec)

   ! Open print output file (= PRNFILE in control file) and write data:
   IF (.not.prtfile_opened) THEN
     IF (.NOT.sysopen(fu_prt, prnnam, 'w', 'print file', error)) GOTO 9999 ! GOTO error handling at end of program
     prtfile_opened = .true.
   ENDIF
   IF (spgrid .EQ. 2) THEN
     CALL ops_print_recep2(project, icm, gasv, idep, do_proc, road_chem, road_disp, nemcat_road, isec, igrid, verb, namco, namsec, nam_pri_sec, coneh, depeh, conc_cf, amol21, &
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
   IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program

   ! Open plot file (= PLTFILE in control file) and write data: 
   IF (.not. pltfile_opened) THEN
     IF (.NOT. sysopen(fu_plt, pltnam, 'w', 'plot file', error)) GOTO 9999 ! GOTO error handling at end of program
     pltfile_opened = .true.
   ENDIF
   CALL ops_plot_uitv(spgrid, isec, coneh, nrrcp, nsubsec, jump, xul_cell_centre, yul_cell_centre, nrcol, nrrow, grid, idep, namco, nam_pri_sec, namsec, depeh,    &
                   & namrcp, xm, ym, cpri, csec, drydep, ddepri, wdepri, wetdep, cno2, cnox, icm, csubsec, nam_subsec,  &  
                   & nparout, parout_val, parout_name, parout_unit, parout_write, error)
   IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program
   
   IF (class_output) THEN
    CALL ops_plot_class_uitv_rcp(spgrid, nrrcp, namco, coneh, namrcp, xm, ym, cpri_class_sum, percvk_class, todo, trafst, error)
    IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program
   ENDIF
   
 ENDIF  !  if output is requested
ENDDO ! End loop over particle size classes for which to produce output fields

! Write additional data to print file (= PRNFILE in control file):
! Note: z0_rcp_all(1) is only used if z0 is constant for all receptors
CALL ops_print_info (varin, project, gasv, isec, intpol, spgrid, z0_rcp_all(1), namco, nbron, bnr, bx, by, bsterkte, bqrv, bqtr, bwarmte,     &
   &  bhoogte, bdiam, bsigmaz, btgedr, bdegr, bcatnr, blandnr, emis, emtrend, jb, mb, idb, jt, mt, idt, iseiz,                 &  
   &  f_z0user, landmax, subbron, domlu, varz, perc, mindist, maxdist, class_output, allow_sigz0_point_source, dir_chem, fnames_used_chem, dir_bg, error)
IF (error%haserror) GOTO 9999 ! GOTO error handling at end of program

! Close print file if it is open:
IF (prtfile_opened) then
   CALL sysclose(fu_prt, prnnam, error)
   if (error%haserror) goto 9999
endif
IF (pltfile_opened) then
   CALL sysclose(fu_plt, pltnam, error)
   if (error%haserror) goto 9999
endif
RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)
end subroutine ops_save_results

end module m_ops_save_results
