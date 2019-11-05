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
! NAME               : %M%
! SCCS(SOURCE)       : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH - SEQUENCE  : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : Martien de Haan (ARIS)
! FIRM/INSTITUTE     : RIVM/LLO
! LANGUAGE           : FORTRAN-77/90
! DESCRIPTION        : Read parameters for the OPS-model from the control file. 
! EXIT CODES         :
! FILES AND OTHER    :
!   I/O DEVICES
! SYSTEM DEPENDENCIES: HP-Fortran
! CALLED FUNCTIONS   :
! UPDATE HISTORY
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_read_ctr(project, runid, year, icm, namco, amol1, gasv, idep, kdeppar, ddeppar, knatdeppar, wdeppar, dg,        &
                     &  irev, vchemc, vchemv, emtrend, ncatsel, catsel, nlandsel, landsel, spgrid, xc, yc, nrcol, nrrow,       &
                     &  grid, igrens, z0_user, intpol, ideh, igrid, checked, f_z0user, isec, error)

USE m_getkey
USE m_fileutils
USE m_error
USE m_commonfile
USE m_commonconst

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_read_ctr')

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*(*), INTENT(OUT)                       :: project                    ! project name
CHARACTER*(*), INTENT(OUT)                       :: runid                      ! run identifier (is used in the output)
INTEGER*4, INTENT(OUT)                           :: year                       ! year under consideration
INTEGER*4, INTENT(OUT)                           :: icm                        
CHARACTER*(*), INTENT(OUT)                       :: namco                      
REAL*4,    INTENT(OUT)                           :: amol1                      
LOGICAL,   INTENT(OUT)                           :: gasv                       ! type of component (0: particle; 1: gas)
LOGICAL,   INTENT(OUT)                           :: idep                       
INTEGER*4, INTENT(OUT)                           :: kdeppar                    
REAL*4,    INTENT(OUT)                           :: ddeppar                    
INTEGER*4, INTENT(OUT)                           :: knatdeppar                 
REAL*4,    INTENT(OUT)                           :: wdeppar                    
REAL*4,    INTENT(OUT)                           :: dg                         
LOGICAL,   INTENT(OUT)                           :: irev                       
REAL*4,    INTENT(OUT)                           :: vchemc                     
REAL*4,    INTENT(OUT)                           :: vchemv                     
REAL*4,    INTENT(OUT)                           :: emtrend                    
INTEGER*4, INTENT(OUT)                           :: ncatsel                    
INTEGER*4, INTENT(OUT)                           :: catsel(*)                  
INTEGER*4, INTENT(OUT)                           :: nlandsel                   
INTEGER*4, INTENT(OUT)                           :: landsel(*)                 
INTEGER*4, INTENT(OUT)                           :: spgrid                     
REAL*4,    INTENT(OUT)                           :: xc                         ! x-coordinate grid centre of user specified grid (spgrid = 1)
REAL*4,    INTENT(OUT)                           :: yc                         ! y-coordinate grid centre of user specified grid (spgrid = 1)
INTEGER*4, INTENT(OUT)                           :: nrcol                      
INTEGER*4, INTENT(OUT)                           :: nrrow                      
REAL*4,    INTENT(OUT)                           :: grid                       ! grid resolution [m]
LOGICAL,   INTENT(OUT)                           :: igrens                     
REAL*4,    INTENT(OUT)                           :: z0_user                    ! roughness length specified by user [m]
INTEGER*4, INTENT(OUT)                           :: intpol                     
INTEGER*4, INTENT(OUT)                           :: ideh                       
LOGICAL,   INTENT(OUT)                           :: igrid                      
LOGICAL,   INTENT(OUT)                           :: checked                    
LOGICAL*4, INTENT(OUT)                           :: f_z0user                   
LOGICAL,   INTENT(OUT)                           :: isec                       
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
REAL*4                                           :: lower                      ! lower limit (is used for checking variables read) 
REAL*4                                           :: upper                      ! upper limit (is used for checking variables read) 

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'// char (0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Open the control file for reading
!
IF (.NOT. sysopen(fu_input, ctrnam, 'r', 'control file', error)) GOTO 2000
!
! Read name for data directory
!
IF (.NOT. GetKeyValue ('DATADIR', datadir, error)) GOTO 1000
!
! Read identification layer (project name, run id, year)
!
IF (.NOT. GetKeyValue ('PROJECT', project, error)) GOTO 1000
IF (.NOT. GetKeyValue ('RUNID', runid, error)) GOTO 1000
IF (.NOT. GetKeyValue ('YEAR', year, error)) GOTO 1000
!
! Read component code, name, mole weight, phase (gas/particle), whether or not to compute deposition ('LOSS')
!
IF (.NOT. GetCheckedKey('COMPCODE', 0, 27, .TRUE., icm, error)) GOTO 1000
!
! For acidifying components, isec is set to TRUE
!
isec = icm <= 3 .AND. icm > 0

IF (.NOT. GetKeyValue  ('COMPNAME', namco, error)) GOTO 1000
IF (.NOT. GetCheckedKey('MOLWEIGHT', 1., 1000., .TRUE., amol1, error)) GOTO 1000
IF (.NOT. GetKeyValue  ('PHASE', .TRUE., gasv, error)) GOTO 1000
IF (.NOT. GetKeyValue  ('LOSS', .TRUE., idep, error)) GOTO 1000
!
! Read choice of dry deposition parametrisation and dry deposition parameter ddeppar
! (either deposition velocity vd (kdeppar = 1) or surface resistance Rc (kdeppar = 2);
! check also if value read is between specified lower and upper boundaries.
! Note: upper = 0.034 should be the same as vgmax in ops_init.
!
IF (.NOT. GetCheckedKey('DDSPECTYPE', 1, 2, gasv .AND. idep .AND..NOT.isec, kdeppar, error)) GOTO 1000
IF (kdeppar .EQ. 1) THEN
  lower = .000001
  upper = .034
ELSE
  lower = 0.
  upper = 999999.
ENDIF
IF (.NOT. GetCheckedKey('DDPARVALUE', lower, upper, gasv .AND. idep .AND..NOT.isec, ddeppar, error)) GOTO 1000
!
! Read parameters
! knatdeppar: choice of wet deposition parametrisation
! wdeppar   : wet deposition parameter.
! wdeppar is either a scavenging rate [%/h] (knatdeppar = 1) or a scavenging ratio [-] (knatdeppar = 2);
! in case knatdeppar = 3 (acidifying components) a scavenging ratio is computed inside OPS; note that for acidifying
! components (isec = TRUE), knatdeppar and wdeppar are not read.
! Check also if value read is between specified lower and upper boundaries.
!
IF (.NOT. GetCheckedKey('WDSPECTYPE', 1, 3, gasv .AND. idep .AND..NOT.isec, knatdeppar, error)) GOTO 1000
IF (knatdeppar .EQ. 1) THEN
  lower = 0.
  upper = 9999.
ELSE
  lower = 0.
  upper = 9999999.
ENDIF
IF (.NOT. GetCheckedKey('WDPARVALUE', lower, upper, gasv .AND. idep .AND..NOT.isec, wdeppar, error)) GOTO 1000

! Read diffusion coefficient, logical for ireversible wash-out or not, chemical conversion rate, light dependent conversion rate
IF (.NOT. GetCheckedKey('DIFFCOEFF', 0., 1., gasv .AND. idep .AND. knatdeppar.EQ.3, dg, error)) GOTO 1000
IF (.NOT. GetKeyValue  ('WASHOUT', gasv .AND. idep .AND. knatdeppar.EQ.3, irev, error)) GOTO 1000
IF (.NOT. GetCheckedKey('CONVRATE', 0., 999., gasv .AND. idep .AND..NOT.isec, vchemc, error)) GOTO 1000
IF (.NOT. GetCheckedKey('LDCONVRATE', 0., 99.99, gasv .AND. idep .AND..NOT.isec, vchemv, error)) GOTO 1000

!
! Read emission layer (emission file, user defined diurnal variation file, 
! user defined particle size distribution file, emission trend factor, selected emission categories,
! selected emission countries)
!
IF (.NOT. GetCheckedKey('EMFILE',.TRUE.,.TRUE.,                             &  ! must be defined and be present
                     &  brnam, error)) GOTO 1000
IF (.NOT. GetCheckedKey('USDVEFILE',.FALSE.,.TRUE.,                         &  ! if defined it must be present
                     &  usdvnam, error)) GOTO 1000
IF (.NOT. GetCheckedKey('USPSDFILE',.FALSE.,.TRUE.,                         &  ! if defined it must be present
                     &  uspsdnam, error)) GOTO 1000
IF (.NOT. GetCheckedKey('EMCORFAC', 0.01, 9.99, .TRUE., emtrend, error)) GOTO 1000
IF (.NOT. GetCheckedKey('TARGETGROUP', 0, 9999, .TRUE., ncatsel, catsel, error)) GOTO 1000
IF (.NOT. GetCheckedKey('COUNTRY', 0, 9999, .TRUE., nlandsel, landsel, error)) GOTO 1000
!
! Read receptor layer (receptor type (regular grid/specified points), centre of grid,
! number of columns and rows of the grid)
!
IF (.NOT. GetCheckedKey('RECEPTYPE', 0, 3, .TRUE., spgrid, error)) GOTO 1000
IF (.NOT. GetKeyValue  ('XCENTER', xc, error)) GOTO 1000
IF (.NOT. GetKeyValue  ('YCENTER', yc, error)) GOTO 1000
IF (.NOT. GetCheckedKey('NCOLS', 0, MAXCOL, spgrid == 1, nrcol, error)) GOTO 1000
IF (.NOT. GetCheckedKey('NROWS', 0, MAXROW, spgrid == 1, nrrow, error)) GOTO 1000

!
! Read grid resolution, logical whether to use also points outside NL (spgrid = 0), 
! name of receptor file (spgrid = 2,3).
! For spgrid = 0,3, the grid resolution must be > 250 m;
! for other receptor types a 1 m resolution is the lower limit.
!
  IF (spgrid .EQ. 0 .OR. spgrid .EQ. 3) THEN
  lower=100
ELSEIF (spgrid .EQ. 1) THEN
  lower=1
ELSE
  CONTINUE
ENDIF
IF (.NOT. GetCheckedKey('RESO', lower, 999999., ANY(spgrid == (/0, 1, 3/)), grid, error)) GOTO 1000
IF (.NOT. GetKeyValue  ('OUTER', spgrid.EQ.0, igrens, error)) GOTO 1000
IF (.NOT. GetCheckedKey('RCPFILE', .TRUE., ANY(spgrid == (/2,3/)), namrecept, error)) GOTO 1000
!
! Read meteo & surface characteristics layer
! (z0 value (0 -> read from file), z0-file, land use file, option for meteo interpolation, meteo file;
! in case of reading the meteo file name, also check its existence if intpol /= 0)
!
IF (.NOT. GetCheckedKey('ROUGHNESS', 0., 3., .TRUE., z0_user, error)) GOTO 1000
!
! If z0_user  = 0 -> read z0 from file
! If z0_user /= 0 -> z0_user is the user defined fixed z0-value
! f_z0user is TRUE if roughness length (z0) is user defined  
!
f_z0user = NINT(10000. * z0_user) /= 0
IF (.NOT. GetCheckedKey('Z0FILE', .TRUE., .NOT.f_z0user, z0file, error)) GOTO 1000
!
! In het nieuwe z0file is de z0 waarde met als eenheid 0.1mm opgeslagen, voorheen was dit mm.
! Met welk bestand we te maken hebben bepalen we aan de hand de string "lgn3.ops" in de filenaam.
! Indien de string "lgn3.ops" in de filenaam voorkomt, dan is factor 1000 ipv 10000.
!
IF (INDEX(z0file,"lgn3.ops") .ne. 0) z0_FACT_NL=1000.

IF (.NOT. GetCheckedKey('LUFILE', .TRUE., .NOT.f_z0user .AND. isec, lufile, error)) GOTO 1000
IF (.NOT. GetCheckedKey('METEOTYPE',0, 2, .TRUE., intpol, error)) GOTO 1000
IF (.NOT. GetCheckedKey('MTFILE', .TRUE., intpol /= 0, kname, error)) GOTO 1000
!
! Read output layer (option for deposition unit, plot file, print file, option for printing grids, 
! logical whether control file has been made by the GUI)
!
IF (.NOT. GetCheckedKey('DEPUNIT', 1, NUNIT, .TRUE., ideh, error)) GOTO 1000
IF (.NOT. GetKeyValue('PLTFILE', pltnam, error)) GOTO 1000
IF (.NOT. GetKeyValue('PRNFILE', prnnam, error)) GOTO 1000
IF (.NOT. GetKeyValue('INCLUDE', .TRUE., igrid, error)) GOTO 1000
IF (.NOT. GetKeyValue('GUIMADE', .TRUE., checked, error)) GOTO 1000
!
! Label 1000: The control file will be closed after an invalid record is read.
!             If no error we are finished.
!
1000 CALL sysclose(fu_input, ctrnam, error)
IF (.NOT. error%haserror) RETURN
!
! Label 2000: An I/O error occurred. Tell error the name of the control file.
!
2000 CALL ErrorParam('control file', ctrnam, error)
CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE ops_read_ctr
