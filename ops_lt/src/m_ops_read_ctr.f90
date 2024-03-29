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
! DESCRIPTION        : Read parameters for the OPS-model from the control file. 
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_read_ctr

implicit none

contains

SUBROUTINE ops_read_ctr(project, runid, year, icm, namco, amol1, gasv, do_proc, idep, kdeppar, ddeppar, knatdeppar, wdeppar, dg, &
                     &  irev, vchemc, iopt_vchem, vchemv, emtrend, ncatsel, catsel, nemcat_road, emcat_road, nlandsel, landsel, & 
                     &  spgrid, xc, yc, nrcol, nrrow, grid, igrens, z0_user, intpol, ideh, igrid, checked, f_z0user, isec, nsubsec, chem_meteo_prognosis, error)

use m_getkey
use m_fileutils
use m_error
use m_commonfile
use m_commonconst_lt
use m_ops_brondepl, only: Tdo_proc

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_read_ctr')

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*(*), INTENT(OUT)                       :: project                    ! project name
CHARACTER*(*), INTENT(OUT)                       :: runid                      ! run identifier (is used in the output)
INTEGER*4, INTENT(OUT)                           :: year                       ! year for chemical maps
INTEGER*4, INTENT(OUT)                           :: icm                        
CHARACTER*(*), INTENT(OUT)                       :: namco                      
REAL*4,    INTENT(OUT)                           :: amol1                      
LOGICAL,   INTENT(OUT)                           :: gasv                       ! type of component (0: particle; 1: gas)
TYPE(Tdo_proc), INTENT(OUT)                      :: do_proc                    ! options to switch on/off specific processes
LOGICAL,   INTENT(OUT)                           :: idep                       
INTEGER*4, INTENT(OUT)                           :: kdeppar                    
REAL*4,    INTENT(OUT)                           :: ddeppar                    
INTEGER*4, INTENT(OUT)                           :: knatdeppar                 
REAL*4,    INTENT(OUT)                           :: wdeppar                    
REAL*4,    INTENT(OUT)                           :: dg                         
LOGICAL,   INTENT(OUT)                           :: irev                       
REAL*4,    INTENT(OUT)                           :: vchemc                     ! chemical conversion rate [%/h]
INTEGER*4, INTENT(OUT)                           :: iopt_vchem                 ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
REAL*4,    INTENT(OUT)                           :: vchemv                     
REAL*4,    INTENT(OUT)                           :: emtrend                    
INTEGER*4, INTENT(OUT)                           :: ncatsel                    ! number of selected emission categories
INTEGER*4, INTENT(OUT)                           :: catsel(*)                  ! list of selected emission categories
INTEGER*4, INTENT(OUT)                           :: nemcat_road                ! number of road emission categories (for vdHout NO2/NOx ratio)
INTEGER*4, INTENT(OUT)                           :: emcat_road(*)              ! list of road emission categories (for vdHout NO2/NOx ratio)
INTEGER*4, INTENT(OUT)                           :: nlandsel                   ! number of selected emission countries 
INTEGER*4, INTENT(OUT)                           :: landsel(*)                 ! list of selected emission countries 
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
LOGICAL,   INTENT(OUT)                           :: igrid                      ! include receptor values in the Report output; is set with INCLUDE in ctr-file or -v option 
LOGICAL,   INTENT(OUT)                           :: checked                    
LOGICAL*4, INTENT(OUT)                           :: f_z0user                   
LOGICAL,   INTENT(OUT)                           :: isec                       
INTEGER*4, INTENT(OUT)                           :: nsubsec                    ! number of sub-secondary species                       
LOGICAL,   INTENT(OUT)                           :: chem_meteo_prognosis       ! use meteo prognosis in chemistry maps
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
REAL*4                                           :: lower                      ! lower limit (is used for checking variables read) 
REAL*4                                           :: upper                      ! upper limit (is used for checking variables read) 

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
! Read identification layer (project name, run id)
!
IF (.NOT. GetKeyValue ('PROJECT', project, error)) GOTO 1000
IF (.NOT. GetKeyValue ('RUNID', runid, error)) GOTO 1000

! Read year for chemical maps:
call ops_read_ctr_year(year,chem_meteo_prognosis,error)
if (error%haserror) goto 1000

!
! Read component code, name, mole weight, phase (gas/particle)
!
IF (.NOT. GetCheckedKey('COMPCODE', 0, 27, .TRUE., icm, error)) GOTO 1000
!
! For acidifying components, isec is set to TRUE
!
isec = icm <= 3 .AND. icm > 0

IF (.NOT. GetKeyValue  ('COMPNAME', namco, error)) GOTO 1000
IF (.NOT. GetCheckedKey('MOLWEIGHT', 1., 1000., .TRUE., amol1, error)) GOTO 1000
IF (.NOT. GetKeyValue  ('PHASE', .TRUE., gasv, error)) GOTO 1000

! Read option(s) to switch on/of loss processes chemistry, dry and wet deposition ('LOSS')
call ops_read_ctr_loss(do_proc, idep, error)
if (error%haserror) goto 1000
!
! Read choice of dry deposition parametrisation and dry deposition parameter ddeppar
! (either deposition velocity vd (kdeppar = 1) or surface resistance Rc (kdeppar = 2);
! check also if value read is between specified lower and upper boundaries.
! Note: upper = 0.034 should be the same as vdmax in ops_init.
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
! wdeppar is either a scavenging rate [%/h] (knatdeppar = 1) or a scavenging ratio [-] (knatdeppar = 2).
! In case of acidifying components (COMPCODE = 1,2,3 <=> isec = TRUE), knatdeppar is set to 3 (see ops_init); 
! in this case a scavenging ratio is computed inside OPS.
! Note that for acidifying components, knatdeppar and wdeppar are not read.
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

! Read chemical conversion rate vchemc; this can be either the string EMEP (meaning that we use conversion rate maps from the EMEP-model -> 
! iopt_vchem = 1) or a fixed value of vchemc. 
! A value of vchemc is only required for non-acidifying components (isec = false), because for acidifying components, we use either
! the EMEP maps or (if EMEP is not specified) an old chemical conversion rate parameterisation (iopt_vchem = 0; see OPS-doc).
call ops_read_ctr_conv_rate(gasv,idep,isec,vchemc,iopt_vchem,error) 
if (error%haserror) GOTO 1000 

IF (.NOT. GetCheckedKey('LDCONVRATE', 0., 99.99, gasv .AND. idep .AND..NOT.isec, vchemv, error)) GOTO 1000

! Secondary species are SO4, NO3_total, NH4; 
! for NOx with EMEP chemical conversion, we have 3 sub-secondary species (HNO3, NO3_C (coarse in PM10-PM2.5), NO3_F (fine in PM2.5)):
if (icm .eq. 2) then
   if (iopt_vchem .eq. 0) then
      ! Old OPS parameterisation; no information on fine and coarse NO3:
      nsubsec = 2 
      CNAME_SUBSEC(1:nsubsec) = (/'NO3_AER', 'HNO3' /)          ! HNO3, NO3_aerosol (in PM10)
   else
      ! EMEP gives also a split between coarse and fine NO3:
      nsubsec = 4
      CNAME_SUBSEC(1:nsubsec) = (/'NO3_AER', 'HNO3', 'NO3_C', 'NO3_F' /)   ! NO3_aerosol (in PM10), HNO3, NO3_coarse (in PM10-PM2.5), NO3_fine (in PM2.5)  
   endif
else
   ! SO4 and NH4 all in fine PM-fraction; no sub-species:
   nsubsec = 0
endif
!if (icm .eq. 2) then
!   nsubsec = 3
!else
!   nsubsec = 0
!endif

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
IF (.NOT. GetCheckedKey('TARGETGROUP', 0, 9999, ncatsel, catsel, error)) GOTO 1000
IF (.NOT. GetCheckedKey('COUNTRY', 0, 9999, nlandsel, landsel, error)) GOTO 1000

! Read emission categories that are roads; they use a 'road correction' according to vdHout (close to the road) for NO2/NOx ratio.
! Record is optional; default no road emission categories (nemcat_road = 0:
call ops_read_ctr_roads(nemcat_road, emcat_road, error)
if (error%haserror) goto 1000

! Read receptor layer (receptor type (regular grid/specified points), centre of grid,
! number of columns and rows of the grid)
IF (.NOT. GetCheckedKey('RECEPTYPE', 0, 3, .TRUE., spgrid, error)) GOTO 1000

! Check invalid combination of ROADS and possible use of sub receptors: 
if ((spgrid .ne. 2) .and. (nemcat_road .gt. 0)) then
   call SetError('ROADS categories currently only allowed for RECEPTYPE 2 (no sub receptors).', &
                 'Combination of other RECEPTYPE and ROADS categories has not been tested yet.',error)
   call ErrorParam('RECEPTYPE',spgrid,error)
   goto 1000
endif

IF (.NOT. GetKeyValue  ('XCENTER', xc, error)) GOTO 1000  ! only needed for spgrid = 1; if value field is empty -> assign MISVALNUM
IF (.NOT. GetKeyValue  ('YCENTER', yc, error)) GOTO 1000  ! only needed for spgrid = 1; if value field is empty -> assign MISVALNUM
IF (.NOT. GetCheckedKey('NCOLS', 0, MAXCOL, spgrid == 1, nrcol, error)) GOTO 1000
IF (.NOT. GetCheckedKey('NROWS', 0, MAXROW, spgrid == 1, nrrow, error)) GOTO 1000

!
! Read RESO = grid resolution, OUTER = whether to use also points outside NL (spgrid = 0), 
! RCPFILE = name of receptor file (spgrid = 2,3).
! For spgrid = 0,3, the grid resolution must be > 100 m;
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

!------------------------------------------------------------------------------------------------
subroutine ops_read_ctr_conv_rate(gasv,idep,isec,vchemc,iopt_vchem,error)

use m_error
use m_commonconst_lt,only: MISVALNUM, EPS_DELTA
use m_commonfile
use m_getkey

implicit none

! Input:
logical, intent(in)  :: gasv            ! gasuous component
logical, intent(in)  :: idep            ! deposition/chemical conversion is switched on
logical, intent(in)  :: isec            ! acidifying component (SO2, NOx, NH3)

! Output
real,    intent(out) :: vchemc          ! chemical conversion rate [%/h]
integer, intent(out) :: iopt_vchem      ! = 0 -> use conversion rates from old OPS parameterisation
                                        ! = 1 -> use conversion rates from EMEP
type(Terror), intent(inout) :: error    ! error structure

! Local:
character(len=200)  :: str1

! Initialise:
vchemc = MISVALNUM
iopt_vchem = 0   ! old OPS parameterisation for chemical conversion rates

! Check for "CONVRATE EMEP":
IF (GetCheckedKey('CONVRATE', 'EMEP', 'EMEP', gasv .AND. idep .AND. isec, str1, error)) THEN
!   ! EMEP has been found or EMEP is not required; if it is required, set iopt_vchem to 1:
!   if (gasv .AND. idep .AND. isec) iopt_vchem = 1

   ! EMEP has been found or EMEP is not required; if it is required, set iopt_vchem to 1:
   if (gasv .AND. idep .AND. isec) then
      ! CONVRATE EMEP has been found and 'gasuous component' and 'chemical conversion is on' and 'acidifying component':
      iopt_vchem = 1   ! use EMEP conversion rates
   else
      ! CONVRATE EMEP is not needed; generate error if it is provided in input anyway:
      if (str1 .eq. 'EMEP') then
         if (.not. isec) then
            call SetError('CONVRATE EMEP can only be used for acidifying components SO2, NOx, NH3 ', error) 
         elseif (.not. idep) then
            call SetError('CONVRATE EMEP can only be used if deposition/chemical conversion switched on', error) 
         else
            call SetError('CONVRATE EMEP can only be used for gasuous components', error) 
         endif
         GOTO 1000
      endif
   endif      
ENDIF

If (iopt_vchem .eq. 0) THEN
    ! vchemc has not been set; read line again and extract vchemc (if required):
    backspace(fu_input); error%haserror = .false.
    IF (GetCheckedKey('CONVRATE', 0., 999., gasv .AND. idep .AND. .NOT.isec, vchemc, error)) then
    
       ! If 'CONVRATE value' is not required and a value is specified anyway, generate error:
       if (.not. (gasv .AND. idep .AND. .NOT.isec)) then
          if (error%haserror) then
             GOTO 1000
          elseif (vchemc .eq. MISVALNUM) then
             continue ! is ok; no error
          else
             if (isec) then
                call SetError('CONVRATE value cannot be specified for acidifying components SO2, NOx, NH3 ', error) 
             elseif (.not. idep) then
                call SetError('CONVRATE value can only be specified if deposition/chemical conversion switched on', error) 
             else
                call SetError('CONVRATE value can only be used for gasuous components', error) 
             endif
             GOTO 1000
          endif
       endif      
    else
       ! CONVRATE value is required but not found -> error 
       if (vchemc .eq. MISVALNUM) then
          error%haserror = .false.
          call SetError('CONVRATE must have a value ', error) 
       endif
    endif
ENDIF

RETURN
1000 CONTINUE  ! error handling in calling routine

end subroutine ops_read_ctr_conv_rate

!------------------------------------------------------------------------------------------------
subroutine ops_read_ctr_year(year,chem_meteo_prognosis,error)

! Read year for chemical maps (background concentrations, masses needed for conversion rates, 
! chemical distribution maps)

use m_error
use m_getkey
use m_commonconst_lt, only: FIRSTYEAR

implicit none

! Input

! Output
integer, intent(out)        :: year                   ! year for chemical maps 
logical, intent(out)        :: chem_meteo_prognosis   ! use meteo prognosis in chemistry maps
type(Terror), intent(inout) :: error                  ! error structure

! Local:
character(len=200)          :: str1                   ! string read in record "YEAR value string"

! Read YEAR value string:
IF (.NOT. GetKeyValue ('YEAR', year, str1, error)) GOTO 1000

! Check year:
if (year < FIRSTYEAR) then
   call SetError('invalid YEAR for chemical maps read from control file', error) 
   call ErrorParam('first year chemical maps available',FIRSTYEAR, error)
   call ErrorParam('YEAR read from control file',year, error)
   goto 1000
endif

! Check for option 'prognosis':
chem_meteo_prognosis = (str1 .eq. 'prognosis')

! Error if there is another string present (non empty and not comment):
if (.not. chem_meteo_prognosis .and. len_trim(str1) .gt. 0) then
   if (str1(1:1) .ne. '!') then
      call SetError('unexpected string beyond value for YEAR ', error) 
      call ErrorParam('year',year,error)
      call ErrorParam('string read',str1,error)
      call ErrorParam('strings allowed','"prognosis" or empty string or "! comment"',error)
      goto 1000
   endif
endif

RETURN
1000 CONTINUE  ! error handling in calling routine

end subroutine ops_read_ctr_year

!---------------------------------------------------------------
subroutine ops_read_ctr_loss(do_proc, idep, error)

! Read option(s) to switch on/off loss processes

use m_getkey
use m_error
use m_ops_brondepl, only: Tdo_proc
use m_commonfile, only: fu_input

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_read_ctr_loss')

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE(Tdo_proc), INTENT(OUT)                      :: do_proc                    ! options to switch on/off specific processes
LOGICAL,   INTENT(OUT)                           :: idep                       

! SUBROUTINE ARGUMENTS - INPUT/OUTPUT
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL
INTEGER*4                                        :: int5(5) = -999             ! array for five settings for idep and do_proc
INTEGER*4                                        :: ios                        ! IO-status
CHARACTER(LEN = 50)                              :: string                     ! string read after 'LOSS'

! Syntax 1
! LOSS idep
!      idep = 0 -> no chemistry, no dry deposition, no wet deposition (over trajectory and at receptor)
!      idep = 1 -> include chemistry, dry deposition, wet deposition (over trajectory and at receptor)
! 
! Supplementary values of LOSS are NOT for normal OPS users, only for testing purposes!!
! With these settings, one or more specific processes can be switched on (=1) or off (=0).
! 
! Syntax 2:
! LOSS idep do_proc%chem do_proc%depl_drydep do_proc%depl_wetdep do_proc%grad_drydep
!
! do_proc%chem         : do process chemistry
! do_proc%depl_drydep  : do process of depletion over trajectory due to dry deposition (deposition at receptor still possible)
! do_proc%depl_wetdep  : do process of depletion over trajectory due to wet deposition (deposition at receptor still possible)
! do_proc%grad_drydep  : do process of vertical gradient due to local deposition at receptor

! Read value for idep (syntax 1, 'normal' input):
IF (.NOT. GetKeyValue  ('LOSS', .TRUE., idep, error)) GOTO 9999

! Read line again and check whether there are five 0 and 1's available for idep and do_proc (syntax 2)
backspace(fu_input)
IF (.NOT. GetKeyValue  ('LOSS', string, error)) GOTO 9999  ! error will not occur (otherwise previous GetKeyValue has also an error)
read(string,*,iostat = ios) int5

! If reading was ok, get logicals from five integers:
if (ios .eq. 0) then

   ! Check for 0 / 1:
   if (any(int5 .lt. 0) .or. any(int5 .gt. 1)) then
      CALL SetError('Incorrect values for settings of do_proc in LOSS: only 0 or 1 allowed;','special OPS-users only', error)
      CALL ErrorParam('integer values read', int5, error)
      goto 9999
   endif

   ! idep               = (int5(1) .eq. 1) is already set
   do_proc%chem         = (int5(2) .eq. 1) 
   do_proc%depl_drydep  = (int5(3) .eq. 1) 
   do_proc%depl_wetdep  = (int5(4) .eq. 1) 
   do_proc%grad_drydep  = (int5(5) .eq. 1)
else
   ! If reading of five integers was unsuccessfull, use setting of idep:
   do_proc%chem         = idep 
   do_proc%depl_drydep  = idep 
   do_proc%depl_wetdep  = idep 
   do_proc%grad_drydep  = idep
endif

! If all loss processes are switched off by idep, you cannot switch a specific process on:
if (.not. idep .and. (do_proc%chem .or. do_proc%depl_drydep .or. do_proc%depl_wetdep .or. do_proc%grad_drydep)) then
      CALL SetError('If all loss processes are switched off by idep, you cannot switch a specific process on;','special OPS-users only', error)
      CALL ErrorParam('idep',                idep,                error)
      CALL ErrorParam('do_proc%chem',        do_proc%chem,        error) 
      CALL ErrorParam('do_proc%depl_drydep', do_proc%depl_drydep, error) 
      CALL ErrorParam('do_proc%depl_wetdep', do_proc%depl_wetdep, error) 
      CALL ErrorParam('do_proc%grad_drydep', do_proc%grad_drydep, error)
      goto 9999
endif

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)

end subroutine ops_read_ctr_loss

!---------------------------------------------------------------
subroutine ops_read_ctr_roads(nemcat_road, emcat_road, error)

! Read record with emission categories that are roads; they use a 'road correction' according to vdHout (close to the road) for NO2/NOx ratio.
! Record is optional; default no road emission categories.
! Examples:
! ROADS      3100 3200 3300  -> emission categories 3100, 3200 and 3300 are roads
! ROADS      0               -> all emission categories are roads
! ROADS                      -> no emission categories are roads
! -- no line with ROADS --   -> no emission categories are roads (allows the use of 'old' ctr-files)

use m_getkey
use m_commonfile, only: fu_input

! OUTPUT  
integer, intent(out)        :: nemcat_road                ! number of road emission categories (for vdHout NO2/NOx ratio)
integer, intent(out)        :: emcat_road(3)              ! list of road emission categories (for vdHout NO2/NOx ratio)

! INPUT/OUTPUT
type(Terror), intent(inout) :: error                      ! error handling record

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_read_ctr_roads')

! Read line with ROADS and array with values:
IF (.NOT. GetCheckedKey('ROADS', 0, 9999, nemcat_road, emcat_road, error)) THEN 
   ! GetCheckedKey is false due to an array value out of range -> error
   goto 9999
ELSE
   ! GetCheckedKey is true (no array value out of range), but we still have to check whether an error occurred
   if (error%haserror) then
      if (error%message .eq. 'Undeclared parameter') then
         ! Missing parameter name ROADS -> no road emission categories (is ok, so reset haserror and backspace input file):
         nemcat_road = 0
         error%haserror = .false. 
         backspace(fu_input)
      else
         ! Other error:
         goto 9999
      endif
   endif
ENDIF

!write(*,*) 'nemcat_road = ',nemcat_road
!write(*,*) 'emcat_road  = ',emcat_road

return

9999 CALL ErrorCall(ROUTINENAAM, error)

end subroutine ops_read_ctr_roads

end module m_ops_read_ctr
