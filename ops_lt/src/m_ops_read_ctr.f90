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

use m_ops_tdo_proc, only: Tdo_proc

implicit none

type TCtrLayerIdentification
   character*256 :: project
   character*256 :: runid
   integer :: year
end type

type TCtrLayerSubstance
   integer :: icm  ! Compcode
   character*256 :: namco  ! Compname
   real :: amol1  ! Molweight
   logical :: gasv  ! Phase
   type(Tdo_proc) :: do_proc  ! options to switch on/off specific processes
   logical :: idep
   integer :: kdeppar
   real :: ddeppar
   integer :: knatdeppar
   real :: wdeppar
   real :: dg
   logical :: irrev
   real :: vchemc
   integer :: iopt_vchem
   real :: vchemv
   character*256 :: dir_bg = ""
end type

type TCtrLayerEmission
   real :: emtrend
   integer :: ncatsel                    ! number of selected emission categories
   integer :: nemcat_road                ! number of road emission categories (for vdHout NO2/NOx ratio)
   integer :: nlandsel                   ! number of selected emission countries
   integer, allocatable :: catsel(:)  ! list of selected emission categories
   integer, allocatable :: emcat_road(:)              ! list of road emission categories (for vdHout NO2/NOx ratio)
   integer, allocatable :: landsel(:)                 ! list of selected emission countries
   LOGICAL                       :: road_chem					 !switch for road chemistry
   LOGICAL                       :: road_disp					 !switch for road dispersion
end type

type TCtrLayerReceptor
   integer :: spgrid
   real :: xc                         ! x-coordinate grid centre of user specified grid (spgrid = 1)
   real :: yc                         ! y-coordinate grid centre of user specified grid (spgrid = 1)
   integer :: nrcol
   integer :: nrrow
   real :: grid                       ! grid resolution [m]
   logical :: igrens
end type

type TCtrLayerMeteoSurface
   real :: z0_user                    ! roughness length specified by user [m]
   integer :: intpol
end type

type TCtrLayerOutput
   integer :: ideh
   logical :: igrid                      ! include receptor values in the Report output; is set with INCLUDE in ctr-file or -v option
   logical :: checked
   logical :: f_z0user
   logical :: isec
   integer :: nsubsec                    ! number of sub-secondary species
   logical :: chem_meteo_prognosis       ! use meteo prognosis in chemistry maps
end type

type TCtrLayers
   type(TCtrLayerIdentification) :: identification
   type(TCtrLayerSubstance) :: substance
   type(TCtrLayerEmission) :: emission
   type(TCtrLayerReceptor) :: receptor
   type(TCtrLayerMeteoSurface) :: meteo_surface
   type(TCtrLayerOutput) :: output
end type

contains

subroutine ops_read_ctr(ctr, error)

use m_getkey
use m_fileutils
use m_error
use m_commonfile
use m_commonconst_lt
use m_ops_tdo_proc, only: Tdo_proc
use m_utils, only: alloc

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                !
PARAMETER    (ROUTINENAAM = 'ops_read_ctr')

! SUBROUTINE ARGUMENTS - OUTPUT
type(TCtrLayers), intent(out) :: ctr
type (TError), intent(inout) :: error  ! error handling record

! LOCAL VARIABLES

! Temporary pointers to allocating array space.

REAL :: lower  ! lower limit (is used for checking variables read)
REAL :: upper  ! upper limit (is used for checking variables read)

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
IF (.NOT. GetKeyValue ('PROJECT', ctr%identification%project, error)) GOTO 1000
IF (.NOT. GetKeyValue ('RUNID', ctr%identification%runid, error)) GOTO 1000

! Read year for chemical maps:
call ops_read_ctr_year(ctr%identification%year, ctr%output%chem_meteo_prognosis,error)
if (error%haserror) goto 1000

!
! Read component code, name, mole weight, phase (gas/particle)
!
IF (.NOT. GetCheckedKey('COMPCODE', 0, 27, .TRUE., ctr%substance%icm, error)) GOTO 1000
!
! For acidifying components, isec is set to TRUE
!
ctr%output%isec = &
        ctr%substance%icm == icm_SO2 &
   .or. ctr%substance%icm == icm_NOx &
   .or. ctr%substance%icm == icm_NH3

IF (.NOT. GetKeyValue  ('COMPNAME', ctr%substance%namco, error)) GOTO 1000
IF (.NOT. GetCheckedKey('MOLWEIGHT', 1., 1000., .TRUE., ctr%substance%amol1, error)) GOTO 1000
IF (.NOT. GetKeyValue  ('PHASE', .TRUE., ctr%substance%gasv, error)) GOTO 1000

! Read option(s) to switch on/of loss processes chemistry, dry and wet deposition ('LOSS')
call ops_read_ctr_loss(ctr%substance%do_proc, ctr%substance%idep, error)
if (error%haserror) goto 1000
!
! Read choice of dry deposition parametrisation and dry deposition parameter ddeppar
! (either deposition velocity vd (kdeppar = 1) or surface resistance Rc (kdeppar = 2);
! check also if value read is between specified lower and upper boundaries.
! Note: upper = 0.034 should be the same as vdmax in ops_init.
!
IF (.NOT. GetCheckedKey( &
   'DDSPECTYPE', 1, 2, &
   ctr%substance%gasv .AND. ctr%substance%idep .AND..NOT. ctr%output%isec, &
   ctr%substance%kdeppar, error &
)) GOTO 1000

IF (ctr%substance%kdeppar .EQ. 1) THEN
  lower = .000001
  upper = .034
ELSE
  lower = 0.
  upper = 999999.
ENDIF

IF (.NOT. GetCheckedKey( &
   'DDPARVALUE', lower, upper, &
   ctr%substance%gasv .AND. ctr%substance%idep .AND..NOT. ctr%output%isec, &
   ctr%substance%ddeppar, error &
)) GOTO 1000
!
! Read parameters
! knatdeppar: choice of wet deposition parametrisation
! wdeppar   : wet deposition parameter.
! wdeppar is either a scavenging rate [%/h] (knatdeppar = 1) or a scavenging ratio [-] (knatdeppar = 2).
! In case of acidifying components (COMPCODE = icm_SO2,icm_NOx,icm_NH3 <=> isec = TRUE), knatdeppar is set to 3 (see ops_init);
! in this case a scavenging ratio is computed inside OPS.
! Note that for acidifying components, knatdeppar and wdeppar are not read.
! Check also if value read is between specified lower and upper boundaries.
!
IF (.NOT. GetCheckedKey( &
   'WDSPECTYPE', 1, 3, &
   ctr%substance%gasv .AND. ctr%substance%idep .AND..NOT. ctr%output%isec, &
   ctr%substance%knatdeppar, error &
)) GOTO 1000

IF (ctr%substance%knatdeppar .EQ. 1) THEN
  lower = 0.
  upper = 9999.
ELSE
  lower = 0.
  upper = 9999999.
ENDIF

IF (.NOT. GetCheckedKey( &
   'WDPARVALUE', lower, upper, &
   ctr%substance%gasv .AND. ctr%substance%idep .AND..NOT. ctr%output%isec, &
   ctr%substance%wdeppar, error &
)) GOTO 1000

! Read diffusion coefficient, logical for ireversible wash-out or not, chemical conversion rate, light dependent conversion rate
IF (.NOT. GetCheckedKey( &
   'DIFFCOEFF', 0., 1., &
   ctr%substance%gasv .AND. ctr%substance%idep .AND. ctr%substance%knatdeppar.EQ.3, &
   ctr%substance%dg, error &
)) GOTO 1000
IF (.NOT. GetKeyValue  ( &
   'WASHOUT', &
   ctr%substance%gasv .AND. ctr%substance%idep .AND. ctr%substance%knatdeppar.EQ.3, &
   ctr%substance%irrev, error &
)) GOTO 1000

! Read chemical conversion rate vchemc; this can be either the string EMEP (meaning that we use conversion rate maps from the EMEP-model ->
! iopt_vchem = 1) or a fixed value of vchemc.
! A value of vchemc is only required for non-acidifying components (isec = false), because for acidifying components, we use either
! the EMEP maps or (if EMEP is not specified) an old chemical conversion rate parameterisation (iopt_vchem = 0; see OPS-doc).
call ops_read_ctr_conv_rate( &
   ctr%substance%gasv, &
   ctr%substance%idep, &
   ctr%output%isec, &
   ctr%substance%vchemc, &
   ctr%substance%iopt_vchem, &
   error &
)
if (error%haserror) GOTO 1000

IF (.NOT. GetCheckedKey( &
   'LDCONVRATE', 0., 99.99, ctr%substance%gasv .AND. ctr%substance%idep .AND..NOT. ctr%output%isec, &
   ctr%substance%vchemv, error &
)) GOTO 1000

! Secondary species are SO4, NO3_total, NH4;
! for NOx with EMEP chemical conversion, we have 3 sub-secondary species (HNO3, NO3_C (coarse in PM10-PM2.5), NO3_F (fine in PM2.5)):
if (ctr%substance%icm .eq. icm_NOx) then
   if (ctr%substance%iopt_vchem .eq. 0) then
      ! Old OPS parameterisation; no information on fine and coarse NO3:
      ctr%output%nsubsec = 2
      CNAME_SUBSEC(1:ctr%output%nsubsec) = (/'NO3_AER', 'HNO3   ' /)          ! HNO3, NO3_aerosol (in PM10)
   else
      ! EMEP gives also a split between coarse and fine NO3:
      ctr%output%nsubsec = 4
      CNAME_SUBSEC(1:ctr%output%nsubsec) = (/'NO3_AER', 'HNO3   ', 'NO3_C  ', 'NO3_F  ' /)   ! NO3_aerosol (in PM10), HNO3, NO3_coarse (in PM10-PM2.5), NO3_fine (in PM2.5)
   endif
else
   ! SO4 and NH4 all in fine PM-fraction; no sub-species:
   ctr%output%nsubsec = 0
endif
!if (icm .eq. icm_NOx) then
!   nsubsec = 3
!else
!   nsubsec = 0
!endif

! Read the background directory map if it is specified.
if (.not. GetKeyValue("DIRBG", ctr%substance%dir_bg, error)) then
   if (error%message == "Undeclared parameter") then
      ! If the parameter is not specified, make it empty and move back in the file.
      backspace(fu_input)
      ctr%substance%dir_bg = ""
      error%haserror = .false.
   else
      ! Handle other errors in the standard manner.
      goto 1000
   endif
endif

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
IF (.NOT. GetCheckedKey('EMCORFAC', 0.01, 9.99, .TRUE., ctr%emission%emtrend, error)) GOTO 1000


! call alloc(NCATMAX, ctr%emission%catsel, error)
! call alloc(NCATMAX, ctr%emission%emcat_road, error)
! call alloc(NLANDMAX, ctr%emission%landsel, error)
allocate(ctr%emission%catsel(NCATMAX))
allocate(ctr%emission%emcat_road(NCATMAX))
allocate(ctr%emission%landsel(NLANDMAX))
if (error%haserror) goto 1000

if (.not. GetCheckedKey("TARGETGROUP", 0, 9999, ctr%emission%ncatsel, ctr%emission%catsel, error)) goto 1000
if (.not. GetCheckedKey("COUNTRY", 0, 9999, ctr%emission%nlandsel, ctr%emission%landsel, error)) goto 1000

! Read emission categories that are roads; they use a 'road correction' according to vdHout (close to the road) for NO2/NOx ratio.
! Record is optional; default no road emission categories (nemcat_road = 0:
call ops_read_ctr_roads(ctr%emission%nemcat_road, ctr%emission%road_chem, ctr%emission%road_disp, ctr%emission%emcat_road, error)
if (error%haserror) goto 1000

! Read receptor layer (receptor type (regular grid/specified points), centre of grid,
! number of columns and rows of the grid)
IF (.NOT. GetCheckedKey('RECEPTYPE', 0, 3, .TRUE., ctr%receptor%spgrid, error)) GOTO 1000

! Check invalid combination of ROADS and possible use of sub receptors:
if ((ctr%receptor%spgrid .ne. 2) .and. (ctr%emission%nemcat_road .gt. 0 .and. ctr%emission%road_chem)) then
   call SetError('ROADS categories currently only allowed for RECEPTYPE 2 (no sub receptors).', &
                 'Combination of other RECEPTYPE and ROADS categories has not been tested yet.',error)
   call ErrorParam('RECEPTYPE',ctr%receptor%spgrid,error)
   goto 1000
endif

IF (.NOT. GetKeyValue  ('XCENTER', ctr%receptor%xc, error)) GOTO 1000  ! only needed for spgrid = 1; if value field is empty -> assign MISVALNUM
IF (.NOT. GetKeyValue  ('YCENTER', ctr%receptor%yc, error)) GOTO 1000  ! only needed for spgrid = 1; if value field is empty -> assign MISVALNUM
IF (.NOT. GetCheckedKey('NCOLS', 0, MAXCOL, ctr%receptor%spgrid == 1, ctr%receptor%nrcol, error)) GOTO 1000
IF (.NOT. GetCheckedKey('NROWS', 0, MAXROW, ctr%receptor%spgrid == 1, ctr%receptor%nrrow, error)) GOTO 1000

!
! Read RESO = grid resolution, OUTER = whether to use also points outside NL (spgrid = 0),
! RCPFILE = name of receptor file (spgrid = 2,3).
! For spgrid = 0,3, the grid resolution must be > 100 m;
! for other receptor types a 1 m resolution is the lower limit.
!
IF (ctr%receptor%spgrid .EQ. 0 .OR. ctr%receptor%spgrid .EQ. 3) THEN
  lower=100
ELSEIF (ctr%receptor%spgrid .EQ. 1) THEN
  lower=1
ELSE
  CONTINUE
ENDIF
IF (.NOT. GetCheckedKey('RESO', lower, 999999., ANY(ctr%receptor%spgrid == (/0, 1, 3/)), ctr%receptor%grid, error)) GOTO 1000
IF (.NOT. GetKeyValue  ('OUTER', ctr%receptor%spgrid.EQ.0, ctr%receptor%igrens, error)) GOTO 1000
IF (.NOT. GetCheckedKey('RCPFILE', .TRUE., ANY(ctr%receptor%spgrid == (/2,3/)), namrecept, error)) GOTO 1000
!
! Read meteo & surface characteristics layer
! (z0 value (0 -> read from file), z0-file, land use file, option for meteo interpolation, meteo file;
! in case of reading the meteo file name, also check its existence if intpol /= 0)
!
IF (.NOT. GetCheckedKey('ROUGHNESS', 0., 3., .TRUE., ctr%meteo_surface%z0_user, error)) GOTO 1000
!
! If z0_user  = 0 -> read z0 from file
! If z0_user /= 0 -> z0_user is the user defined fixed z0-value
! f_z0user is TRUE if roughness length (z0) is user defined
!
ctr%output%f_z0user = NINT(10000. * ctr%meteo_surface%z0_user) /= 0
IF (.NOT. GetCheckedKey('Z0FILE', .TRUE., .NOT.ctr%output%f_z0user, z0file, error)) GOTO 1000
!
! In het nieuwe z0file is de z0 waarde met als eenheid 0.1mm opgeslagen, voorheen was dit mm.
! Met welk bestand we te maken hebben bepalen we aan de hand de string "lgn3.ops" in de filenaam.
! Indien de string "lgn3.ops" in de filenaam voorkomt, dan is factor 1000 ipv 10000.
!
IF (INDEX(z0file,"lgn3.ops") .ne. 0) z0_FACT_NL=1000.

IF (.NOT. GetCheckedKey('LUFILE', .TRUE., .NOT.ctr%output%f_z0user .AND. ctr%output%isec, lufile, error)) GOTO 1000
IF (.NOT. GetCheckedKey('METEOTYPE',0, 2, .TRUE., ctr%meteo_surface%intpol, error)) GOTO 1000
IF (.NOT. GetCheckedKey('MTFILE', .TRUE., ctr%meteo_surface%intpol /= 0, kname, error)) GOTO 1000
!
! Read output layer (option for deposition unit, plot file, print file, option for printing grids,
! logical whether control file has been made by the GUI)
!
IF (.NOT. GetCheckedKey('DEPUNIT', 1, NUNIT, .TRUE., ctr%output%ideh, error)) GOTO 1000
IF (.NOT. GetKeyValue('PLTFILE', pltnam, error)) GOTO 1000
IF (.NOT. GetKeyValue('PRNFILE', prnnam, error)) GOTO 1000
IF (.NOT. GetKeyValue('INCLUDE', .TRUE., ctr%output%igrid, error)) GOTO 1000
IF (.NOT. GetKeyValue('GUIMADE', .TRUE., ctr%output%checked, error)) GOTO 1000
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
logical :: keyfound
! Initialise:
vchemc = MISVALNUM
keyfound = GetKeyValue('CONVRATE', str1, error)

! Check for "CONVRATE EMEP":
if (gasv .AND. idep .AND. isec .and. keyfound .and. str1 == 'EMEP') then
   ! CONVRATE EMEP has been found and 'gasuous component' and 'chemical conversion is on' and 'acidifying component':
   iopt_vchem = 1   ! use EMEP conversion rates
else
   iopt_vchem = 0
   if (gasv .AND. idep .AND. isec .and. keyfound) then
      call SetError('CONVRATE must be EMEP', error)
   else if (.not. (gasv .and. idep .and. isec) .and. str1=='EMEP') then
      ! CONVRATE EMEP is not needed; generate error if it is provided in input anyway:
      if (.not. isec) then
         call SetError('CONVRATE EMEP can only be used for acidifying components SO2, NOx, NH3 ', error)
      elseif (.not. idep) then
         call SetError('CONVRATE EMEP can only be used if deposition/chemical conversion switched on', error)
      else
         call SetError('CONVRATE EMEP can only be used for gasuous components', error)
      endif
      GOTO 1000
   endif

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
      ! CONVRATE value is required but not found:
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
use m_ops_tdo_proc, only: Tdo_proc
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
INTEGER                                          :: int5(5) = -999             ! array for five settings for idep and do_proc
INTEGER                                          :: ios                        ! IO-status
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
subroutine ops_read_ctr_roads(nemcat_road, road_chem, road_disp, emcat_road, error)

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
integer, intent(out)        :: emcat_road(*)              ! list of road emission categories (for vdHout NO2/NOx ratio)
LOGICAL,	INTENT(OUT)		:: road_chem 					! switch for using chemistry for road emissions !ADDED by GTHO
 LOGICAL,	INTENT(OUT)		:: road_disp					! switch for using dispersion options for road emissions !ADDED by GTHO

! INPUT/OUTPUT
type(Terror), intent(inout) :: error                      ! error handling record

! LOCAL VARIABLES
logical                       :: gck                        ! output of GetCheckedKey
character(len = 50)           :: gkv                     ! string read after 'LOSS'  
INTEGER                       :: ios                   
INTEGER 					  :: road_opt(2)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                !
PARAMETER    (ROUTINENAAM = 'ops_read_ctr_roads')

! Read line with ROADS and array with values:
gck = GetCheckedKey('ROADS', 0, 9999, nemcat_road, emcat_road, error)

! Check error message:
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
else
   ! Just to be sure, but error%haserror = false and gck = false probably won't happen:
   if (.not. gck) goto 9999
endif

!read in ROADSopt from the contraol file, road_opt(1) = chemistry on or off, road_opt(2) = dispersion on or off
IF (.NOT. GetKeyValue  ('ROADSopt', gkv, error)) then
   if (error%message .eq. 'Undeclared parameter') then
		if (nemcat_road .eq. 0) THEN
			road_chem = .false.
			road_disp = .false.
		else
			write(*,*) "no variable given for ROADSopt, chem and disp assumed on"
			road_chem = .TRUE.
			road_disp = .TRUE.
		endif
    error%haserror = .false. 
    backspace(fu_input)
	ELSE
		GOTO 9999
	ENDIF
ELSE
	read(gkv,*,iostat = ios) road_opt
	if (road_opt(2) .lt. -9999) THEN
		road_chem = (road_opt(1) .eq. 1)
		road_disp = (road_opt(1) .eq. 1)
	ELSE
		road_chem = (road_opt(1) .eq. 1)
		road_disp = (road_opt(2) .eq. 1)
	ENDIF
ENDIF

read(gkv,*,iostat = ios) road_opt


return

9999 CALL ErrorCall(ROUTINENAAM, error)

end subroutine ops_read_ctr_roads

end module m_ops_read_ctr
