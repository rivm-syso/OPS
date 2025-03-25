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
!==============================================
module m_ops_emis_private
!==============================================

! This module contains all the helper variables/subroutines/functions
! that should be kept private. Only 'use' m_ops_emis, never this module
! directly, except in m_ops_emis itself and in its unit tests.
implicit none

PUBLIC

contains

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE NAME    : check_source
! DESCRIPTION        : check whether a source parameter lies within a specified range. If not, the paramater is fixed at either
!                      the lower or upper limit of the range. In this case, a warning is written to the log file;
!                      this warning includes the record number of the source. 
!                      Included for backward compatibility of old source files; better use check_source2.
! CALLED FUNCTIONS   :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE check_source(nr, varnaam, onder, boven, varwaarde, error)

USE m_error
USE m_commonfile, only: fu_log
USE m_commonconst_lib, only: EPS_DELTA
USE m_ops_logfile

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM               
PARAMETER    (ROUTINENAAM = 'check_source')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: nr                         ! record number of source file
CHARACTER*(*), INTENT(IN)                        :: varnaam                    ! variable to be checked
REAL,      INTENT(IN)                            :: onder                      ! lower limit
REAL,      INTENT(IN)                            :: boven                      ! upper limit

! SUBROUTINE ARGUMENTS - I/O
REAL,      INTENT(INOUT)                         :: varwaarde                  ! (adapted) value of variable
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER                                          :: mlen                       ! length of variable name
LOGICAL*1                                        :: switch                     ! indicates weather WARNING has already been printed

!-------------------------------------------------------------------------------------------------------------------------------
!
! Check and possibly open log file. From here there is always going to be something written to it, so the opening is allowed.
!
IF (error%haserror) GOTO 9999

switch = .FALSE.

!
! Check upper limit; if needed, write warning and fix variable at upper limit.
!
IF (varwaarde .GT. (boven + EPS_DELTA)) THEN                                   ! varwaarde too large

   IF (.NOT. switch) THEN
     IF (.NOT. ops_openlog(error)) GOTO 9000
     WRITE(fu_log,'("WARNING: OPS has detected a value outside", " its limits in routine ", A)')                               &
        &  ROUTINENAAM(:LEN_TRIM(ROUTINENAAM))
   ENDIF

   switch=.TRUE.

   mlen = LEN_TRIM(varnaam)
   WRITE(fu_log,'(''   Record number '',I6,'': Value of '', ''emission variable '', a, '' ('', G10.3,                          &
  &  '') is outside range '', ''('', G10.3, '' - '', G10.3, '')'')') nr, varnaam(:mlen), varwaarde, onder , boven
   WRITE(fu_log,'(25x,''and has been set to maximum value'')')

   varwaarde = boven

!
! Check lower limit; if needed, write warning and fix variable at lower limit.
!
ELSEIF (varwaarde .LT. (onder - EPS_DELTA)) THEN                               ! varwaarde too small

   IF (.NOT. switch) THEN
     IF (.NOT. ops_openlog(error)) GOTO 9000
     WRITE(fu_log,'("WARNING: OPS has detected a value outside", " its limits in routine ", A)')                               &
        &  ROUTINENAAM(:LEN_TRIM(ROUTINENAAM))
   ENDIF

   switch=.TRUE.

   mlen = LEN_TRIM(varnaam)
   WRITE(fu_log,'(''   Record number '',I6,'': Value of '', ''emission variable '', a, '' ('', G10.3,                          &
  &  '') is outside range '', ''('', G10.3, '' - '', G10.3, '')'')') nr, varnaam(:mlen), varwaarde, onder , boven

   IF (varnaam .EQ. '<sterkte>') THEN
     WRITE(fu_log,'(25x,''Record will be skipped'')')                          ! Zero emissions are meaningless
   ELSE
     WRITE(fu_log,'(25x,''and has been set to minimum value'')')
   ENDIF

   varwaarde = onder

ELSE
   CONTINUE
ENDIF
RETURN

9000 CALL ErrorCall(ROUTINENAAM, error)
9999 RETURN

END SUBROUTINE check_source

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE NAME    : check_isource
! DESCRIPTION        : check whether an integer source parameter lies within a specified range. If not, the paramater is fixed at either
!                      the lower or upper limit of the range. In this case, a warning is written to the log file;
!                      this warning includes the record number of the source. 
!                      Included for backward compatibility of old source files; better use check_isource2.
! CALLED FUNCTIONS   :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE check_isource(nr, varnaam, onder, boven, varwaarde, error)

USE m_error

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                
PARAMETER    (ROUTINENAAM = 'check_source')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: nr                         ! record number of source file
CHARACTER*(*), INTENT(IN)                        :: varnaam                    ! variable to be checked
INTEGER,   INTENT(IN)                            :: onder                      ! lower limit
INTEGER,   INTENT(IN)                            :: boven                      ! upper limit

! SUBROUTINE ARGUMENTS - I/O
INTEGER,   INTENT(INOUT)                         :: varwaarde                  ! (adapted) value of variable
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES
REAL                                             :: var                        ! help variable (= float(varwaarde)) 

var = FLOAT(varwaarde)
CALL check_source(nr, varnaam, FLOAT(onder), FLOAT(boven), var, error)
varwaarde = NINT(var)

END SUBROUTINE check_isource

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE NAME    : check_source2
! DESCRIPTION        : check whether a source parameter lies within a specified range. If not, an error message is generated
!                      and returned back to the calling routine. The error has to be handled in the calling routine.
!                      Note: check_source adjusts the value and generates a warning.
! CALLED FUNCTIONS   :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE check_source2(varnaam, onder, boven, varwaarde, error)

USE m_error
USE m_commonconst_lib, only: EPS_DELTA

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM               
PARAMETER    (ROUTINENAAM = 'check_source2')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: varnaam                    ! variable to be checked
REAL,      INTENT(IN)                            :: onder                      ! lower limit
REAL,      INTENT(IN)                            :: boven                      ! upper limit
REAL,      INTENT(IN)                            :: varwaarde                  ! value of variable

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES

!-------------------------------------------------------------------------------------------------------------------------------
!
! If an error has already occurred, this check is skipped
IF (.NOT. error%haserror) THEN

   ! Check range:
   IF (varwaarde .LT. (onder - EPS_DELTA) .OR. varwaarde .GT. (boven + EPS_DELTA)) THEN
       CALL SetError(trim(varnaam),' outside permitted range', error)
       CALL ErrorParam('lower limit', onder, error)
       CALL ErrorParam(trim(varnaam), varwaarde, error)
       CALL ErrorParam('upper limit', boven, error)
       CALL ErrorCall(ROUTINENAAM, error)
   ENDIF
ENDIF

RETURN

END SUBROUTINE check_source2

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE NAME    : check_source3
! DESCRIPTION        : check whether a source parameter lies within a specified range. If not, a warning message is generated
!                      and returned back to the calling routine. The warning has to be handled in the calling routine.
! CALLED FUNCTIONS   :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE check_source3(warning1, varnaam, onder, boven, varwaarde, error)

USE m_error
USE m_commonconst_lib, only: EPS_DELTA
USE m_commonfile, only: fu_log

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM               
PARAMETER    (ROUTINENAAM = 'check_source3')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: warning1                   ! first part of warning
CHARACTER*(*), INTENT(IN)                        :: varnaam                    ! variable to be checked
REAL,      INTENT(IN)                            :: onder                      ! lower limit
REAL,      INTENT(IN)                            :: boven                      ! upper limit
REAL,      INTENT(IN)                            :: varwaarde                  ! value of variable

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES

!-------------------------------------------------------------------------------------------------------------------------------
!

! Check range:
IF (varwaarde .LT. (onder - EPS_DELTA) .OR. varwaarde .GT. (boven + EPS_DELTA)) THEN
    CALL SetError(trim(warning1) // '; ' // trim(varnaam) ,' outside permitted range', error)
    CALL ErrorParam('lower limit', onder, error)
    CALL ErrorParam(trim(varnaam), varwaarde, error)
    CALL ErrorParam('upper limit', boven, error)
    CALL ErrorCall(ROUTINENAAM, error)
    
   ! Reset error message (only warning):
   error%haserror = .FALSE.
   
   ! Write warning to log file:
   CALL WriteError(fu_log, error)

ENDIF

RETURN

END SUBROUTINE check_source3

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE NAME    : check_isource2
! DESCRIPTION        : check whether an integer source parameter lies within a specified range. If not, an error message is generated
!                      and returned back to the calling routine. The error has to be handled in the calling routine.
!                      Note: check_isource adjusts the value and generates a warning.
! CALLED FUNCTIONS   :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE check_isource2(varnaam, onder, boven, varwaarde, error)

USE m_error

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                
PARAMETER    (ROUTINENAAM = 'check_source2')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: varnaam                    ! variable to be checked
INTEGER,   INTENT(IN)                            :: onder                      ! lower limit
INTEGER,   INTENT(IN)                            :: boven                      ! upper limit
INTEGER,   INTENT(IN)                            :: varwaarde                  ! value of variable

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES

CALL check_source2(varnaam, FLOAT(onder), FLOAT(boven), FLOAT(varwaarde), error)

END SUBROUTINE check_isource2

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE NAME    : check_verdeling
! DESCRIPTION        : Check whether distribution (=verdeling) has been read.
! CALLED FUNCTIONS   :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE check_verdeling(icode, presentcode, stdclass, usdclass, parname, error)

USE m_error
USE m_commonconst_lib, only: MAXDISTR

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                
PARAMETER    (ROUTINENAAM = 'check_verdeling')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: icode                      ! code that has to be checked; 
                                                                               ! if icode < 0 -> check whether a user defined distribution is present
                                                                               ! if icode > 0 -> check whether a standard distribution is present
                                                                               ! if icode = 0 -> do not check anything
LOGICAL,   INTENT(IN)                            :: presentcode(MAXDISTR,4)     
INTEGER,   INTENT(IN)                            :: stdclass                   ! index of standard distributions in 2nd dimension of presentcode
INTEGER,   INTENT(IN)                            :: usdclass                   ! index of user defined distributions in 2nd dimension of presentcode
CHARACTER*(*), INTENT(IN)                        :: parname                    ! parameter name in error messages

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER                                          :: klasse                     ! 2nd index into presentcode
CHARACTER*(100)                                  :: msg                        ! error message

!
! Check for user defined distributions, in case icode < 0,
! check for standard distributions, in case icode > 0
!
IF (.NOT.error%haserror .and. icode /= 0) THEN
  IF (icode < 0) THEN
    klasse = usdclass
  ELSE
    klasse = stdclass
  ENDIF
  IF (.NOT. presentcode(ABS(icode), klasse)) THEN
    write(msg,'(a,a,a)') 'No distribution available for this code of ',trim(parname),'.'
    CALL SetError(msg,'Note: use minus sign for user defined codes.',error)
    CALL ErrorParam(parname, icode, error)
    CALL ErrorCall(ROUTINENAAM, error)
  ENDIF
ELSE
  IF (icode == 0 .and. parname == "idgr") THEN
    CALL SetError('It is not permitted to use code 0 for', parname, error)
    CALL ErrorParam(parname, icode, error)
    CALL ErrorCall(ROUTINENAAM, error)
  ENDIF
ENDIF

END SUBROUTINE check_verdeling

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE NAME    : check_stack_param
! DESCRIPTION        : Check stack parameters qww, D_stack, V_stack, Ts_stack_C
! CALLED FUNCTIONS   :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE check_stack_param(qww, VsDs_opt, D_stack, D_stack_outer, V_stack, Ts_stack_C, error)

USE m_error
USE m_ops_utils, only: is_missing
USE m_commonconst_lib, only: EPS_DELTA

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                
PARAMETER    (ROUTINENAAM = 'check_stack_param')

! SUBROUTINE ARGUMENTS - INPUT
real   , intent(in)                              :: qww                        ! heat content[ MW]
logical, intent(in)                              :: VsDs_opt                   ! read stack parameters Ds/Vs/Ts from source file
real   , intent(in)                              :: D_stack                    ! diameter of the stack [m]
real   , intent(in)                              :: D_stack_outer              ! outer diameter of the stack [m]
real   , intent(in)                              :: V_stack                    ! exit velocity of plume at stack tip [m/s]
real   , intent(in)                              :: Ts_stack_C                 ! temperature of effluent from stack [C]

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES

! Check that either Qw or Ts_stack_C is defined (and not both):
if (VsDs_opt) then
   if ((is_missing(Ts_stack_C) .and. is_missing(qww)) .or. (.not. is_missing(Ts_stack_C) .and. .not. is_missing(qww))) then
      CALL SetError('One of heat content (Qw) or temperature effluent gas (Ts_stack_C) must be specified, other must be -999.', error)
      CALL ErrorParam('Ts_stack_C', Ts_stack_C, error)
      CALL ErrorParam('Qw', qww, error)
      CALL ErrorCall(ROUTINENAAM, error)
   endif
else
  if (is_missing(qww)) then
      CALL SetError('Heat content (Qw) must be specified', error)
      CALL ErrorParam('Qw', qww, error)
      CALL ErrorCall(ROUTINENAAM, error)  
   endif
endif

! If D_stack_outer is given, D_stack is required:
if (.not. is_missing(D_stack_outer) .and. is_missing(D_stack)) then
    CALL SetError('D_stack is required to use D_stack_outer','Specify D_stack', error)
    CALL ErrorParam('D_stack_outer', D_stack_outer, error)
    CALL ErrorParam('D_stack', D_stack, error)
    CALL ErrorCall(ROUTINENAAM, error)
endif

! Check ranges:
! (for the check on Ts_stack_C -> see also check in m_ops_plume_rise - ops_plumerise_qw_Ts)
if (.not. is_missing(D_stack))       CALL check_source2('<inner diameter stack [m]>'      , 0.01    ,   30.0 , D_stack, error)       ! Infomil NNM 2.1.2 - Modelinvoer
if (.not. is_missing(D_stack_outer)) CALL check_source2('<outer diameter stack [m]>'      , D_stack ,   30.0 , D_stack_outer, error) 
if (.not. is_missing(V_stack))       CALL check_source2('<exit velocity [m/s]>'           , 0.0     ,   50.0 , V_stack, error)       ! V_stack = 0 is ok; in this case Qw = 0. Upper limit V_stack? 
if (.not. is_missing(Ts_stack_C))    CALL check_source2('<temperature effluent gas [C]>'  , 0.0     , 2000.0 , Ts_stack_C, error)    ! temperature waste burning ~ 1300 C


! Check whether V_stack = 0 and Qw > 0 -> error
if (.not. is_missing(V_stack)) then
   if (V_stack .lt. EPS_DELTA .and. qww .gt. EPS_DELTA) then
      CALL SetError('If exit velocity (V_stack) is zero, then heat content (Qw) must be zero also.','Use V_stack = -999. if you only want to specify Qw.', error)
      CALL ErrorParam('V_stack', V_stack, error)
      CALL ErrorParam('Qw', qww, error)
      CALL ErrorCall(ROUTINENAAM, error)  
   endif
endif

END SUBROUTINE check_stack_param

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE NAME    : check_building_param
! DESCRIPTION        : Check building parameters 
! CALLED FUNCTIONS   :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE check_building_param(brn_version, building, hbron, qww, D_stack, D_stack_outer, V_stack, error)

USE m_error
USE m_ops_utils, only: is_missing
USE m_commonconst_lib, only: EPS_DELTA
use m_ops_building
USE m_commonfile, only: fu_log
USE m_ops_logfile

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                
PARAMETER    (ROUTINENAAM = 'check_building_param')

! Input:
integer, intent(in)  :: brn_version               ! version of the emission file
type(Tbuilding),  intent(in)  :: building         ! structure with building parameters
real,    intent(in)  :: hbron                     ! emission height at source (stack height), without plume rise [m]
real,    intent(in)  :: qww                       ! heat content [MW]
real,    intent(in)  :: D_stack                   ! stack diameter [m]
real   , intent(in)  :: D_stack_outer             ! outer diameter of the stack [m]
real,    intent(in)  :: V_stack                   ! exit velocity [m/s]

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! Local:
real :: wlratio ! ratio width/length building

! Check only needed if all building dimensions have been specified:
if (.not. (is_missing(building%length) .or. is_missing(building%width) .or. is_missing(building%height) .or. is_missing(building%orientation))) then

   ! Set width/length ratio:
   if (building%length > 0.0) then
      wlRatio = building%width/building%length  
   else
      ! if length = 0 -> buildingType = 0 (see below)
      wlRatio = HUGE(1.0)
   endif
   
   ! If values outside limits -> write warning to log-file   
   ! limits based on data for 2500 animal houses in 2018  
   ! Note that it is already checked that all building dimensions (length, width, height) have been specified

   ! Error if Qw must be specified (= 0) and cannot be missing:
   if (is_missing(qww) .and. brn_version .lt. 5) then
      CALL SetError('If building is present, then heat content (Qw) must be zero (cannot be missing).', error)
      CALL ErrorParam('Qw', qww, error)
      goto 9999  
   endif


    if (brn_version .eq. 5 .and. (is_missing(building%x) .or. is_missing(building%y))) then
      ! Check on max distance between source and building is handled within the building module
      CALL SetError('If building is present in BRN version 5, then building location must be specified', error)
      CALL ErrorParam('building location', building%x, error)
      CALL ErrorParam('building location', building%y, error)
      goto 9999  
    endif
    if (brn_version .eq. 5 .and. (is_missing(D_stack_outer))) then
      CALL SetError('If building is present in BRN version 5, then D_stack_outer must be specified', error)
      CALL ErrorParam('Outer diameter of stack', D_stack_outer, error)
      goto 9999  
    endif

   ! Open log file if not already open:
   IF (.NOT. ops_openlog(error)) GOTO 9999
     
   ! Warnings if value is outside table boundaries: 
                                             CALL check_source3('check table building effect ','<building height [m]>'             ,  0.0  ,  20.0  , building%height, error)
   if (.not. is_missing(hbron))              CALL check_source3('check table building effect ','<emission height [m]>'             ,  0.0  ,  20.0  , hbron, error)
   if (brn_version .lt. 5)                   CALL check_source3('check table building effect ','<heat content [MW]>'               ,  0.0  ,   0.0  , qww, error)   ! Table only for qww = 0
   if (.not. is_missing(V_stack))            CALL check_source3('check table building effect ','<exit velocity [m/s]>'             ,  0.0  ,   8.4  , V_stack, error)
   if (.not. is_missing(D_stack))            CALL check_source3('check table building effect ','<stack diameter [m]>'              ,  0.01 ,   5.0  , D_stack, error)
                                             CALL check_source3('check table building effect ','<width/length ratio building [-]>' ,  0.15 ,   1.0 , wlRatio, error)
                                             CALL check_source3('check table building effect ','<building length [m]>'             , 10.0  , 105.0  , building%length, error)
                                             CALL check_source3('check table building effect ','<building orientation [degrees w.r.t. x-axis]>' , 0.0  , 180.0  , building%orientation, error)
   
endif
   
RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE check_building_param


end module m_ops_emis_private

!==============================================
module m_ops_emis
!==============================================

use m_ops_emis_private

! Emission module, contains subroutines to read emissions. 
 
implicit none

PRIVATE ! default for module
PUBLIC  :: ops_emis_read_header, ops_emis_read_annual1

contains

!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_emis_read_header(fu_bron, brn_version, VsDs_opt, nrec, numbron, error)

! Read header from the emission file (brn-file; brn << bron = source)) and return information of the version of the source file (BRN-VERSION).
! The header may contain multiple lines which start with "!" in column 1.
! no BRN-VERSION header -> fixed format
! BRN-VERSION 0         -> fixed format
! BRN-VERSION 1         -> free format
! BRN-VERSION 2         -> free format, include stack parameters D_stack, V_stack, Ts_stack.
! BRN-VERSION 3         -> free format, include stack parameters D_stack, V_stack, Ts_stack, building type
! BRN-VERSION 4         -> free format, include include D_stack, V_stack, Ts_stack, building%length, building%width, building%height, building%orientation
! BRN-VERSION 5         -> free format, as BRN-VERSION4, but includes center x, y of the building and D_stack_outer
! The file pointer is left at start of the first data record.

USE m_error
USE m_fileutils

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                 
PARAMETER    (ROUTINENAAM = 'ops_emis_read_header')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER, INTENT(IN)                              :: fu_bron                    ! file unit brn-file (emission file)

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER, INTENT(OUT)                             :: brn_version                ! version of emission file
LOGICAL, INTENT(OUT)                             :: VsDs_opt                   ! read stack parameters Ds/Vs/Ts from source file
INTEGER, INTENT(OUT)                             :: nrec                       ! number of records read (= number of records in header)
INTEGER, INTENT(OUT)                             :: numbron                    ! number of (selected) sources (initial value = 0)
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
LOGICAL                                          :: end_of_info                ! end of info has been reached
CHARACTER*512                                    :: cbuf                       ! character buffer, used to store an emission record
CHARACTER*180                                    :: word                       ! word read from character buffer
INTEGER                                          :: ierr                       ! error status
INTEGER                                          :: idum                       ! dummy integer read from first header line
INTEGER                                          :: brn_version_read           ! source version, read from first line of header

!-------------------------------------------------------------------------------------------------------------------------------

! Initialisation: 
numbron     = 0
nrec        = 0
end_of_info = .FALSE.

! Default (if no ! BRN-VERSION can be found) -> old brn-file, no stack parameters:
brn_version = 0
VsDs_opt    = .FALSE. 

! Read first header line:
CALL sysread(fu_bron, cbuf, end_of_info, error)
nrec = nrec + 1
IF (error%haserror) GOTO 9999

! If first line is a line with first word a number (no header line), backspace the file:
READ (cbuf, '(i4)', IOSTAT = ierr) idum
if (ierr .eq. 0) then
   backspace(fu_bron)
   nrec = nrec - 1
endif

! Get version from first header line:
IF (cbuf(1:1) .EQ. "!") THEN
  READ (cbuf(2:len_trim(cbuf)),*,IOSTAT = ierr) word, brn_version_read
  if (ierr .eq. 0) then
     if (word .EQ. "BRN-VERSION") then
        brn_version = brn_version_read
        VsDs_opt    = (brn_version .GE. 2)
        CALL check_isource2('<BRN-version>', 0, 5, brn_version, error)
        if (error%haserror) goto 9999
     else
        CALL SetError('Cannot find BRN-VERSION in first line of header', error)
        goto 9999
     endif
  else
     call SetError('Error while reading BRN-VERSION version_number in first line of header', error)
     goto 9999
  endif
  
  ! Read rest of header lines:
  DO WHILE (.NOT. end_of_info)
    CALL sysread(fu_bron, cbuf, end_of_info, error)
    nrec = nrec + 1
    IF (error%haserror) GOTO 9999
    IF (cbuf(1:1) .NE. "!") THEN
       end_of_info = .TRUE.
      
       ! First real emission record has been reached, so we backspace 1 line:
       backspace(fu_bron)
       nrec = nrec - 1
    ENDIF
  ENDDO
ENDIF

RETURN

9999 CALL ErrorParam('emission record', cbuf, error)
CALL ErrorParam('record nr. in emission file', nrec, error)
CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE ops_emis_read_header

!-------------------------------------------------------------------------------------------------------------------------------

SUBROUTINE ops_emis_read_annual1(fu_bron, icm, check_psd, presentcode, brn_version, VsDs_opt, nrec, numbron, building_present1, &
               mm, x, y, qob, qww, hbron, diameter, sigz0, D_stack, D_stack_outer, V_stack, Ts_stack, emis_horizontal, building, ibtg, ibroncat, iland, idgr, end_of_file, error, allow_sigz0_point_source)

! Read one data line from the emission file (brn-file; brn << bron = source)) and return emission parameters.
! Emission parameters that lie outside a specified range generate an error.
! This subroutine supports old type of emission files (with no BRN-VERSION header or BRN-VERSION 1 
! both in fixed format (old type of brn-files) and free format and extended free format (with V_stack, D_stack, Ts_stack) .

USE m_error
USE m_fileutils
USE m_geoutils
USE m_commonconst_lib, only: EPS_DELTA, HUMAX, MAXDISTR, ncolBuildingEffectTable
USE Binas, only: T0
USE m_ops_utils, only: is_missing
USE m_ops_building

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                 
PARAMETER    (ROUTINENAAM = 'ops_emis_read_annual1')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER, INTENT(IN)                            :: fu_bron                    ! file unit brn-file (emission file)
INTEGER, INTENT(IN)                            :: icm                        ! component nummer
LOGICAL, INTENT(IN)                            :: check_psd                  ! check whether particle size distribution has been read
LOGICAL, INTENT(IN)                            :: presentcode(MAXDISTR,4)    ! which distribution codes are present
                                                                             ! presentcode(:,1): diurnal variations
                                                                             ! presentcode(:,2): particle size distributions
                                                                             ! presentcode(:,3): user-defined diurnal variation
                                                                             ! presentcode(:,4): user-defined particle size distributions
INTEGER, INTENT(IN)                            :: brn_version                ! version of emission file
LOGICAL, INTENT(IN)                            :: VsDs_opt                   ! read stack parameters Ds/Vs/Ts from source file
LOGICAL, INTENT(IN), optional                  :: allow_sigz0_point_source   ! allow initial sigma for point sources; optional argument (only for OPS-LT), 
                                                                             ! if not present: allow_sigz0_point_source_loc = true (need local variable for optional argument)

! SUBROUTINE ARGUMENTS - INPUT/OUTPUT
INTEGER, INTENT(INOUT)                         :: nrec                       ! record number of source file
INTEGER, INTENT(INOUT)                         :: numbron                    ! number of (selected) source
LOGICAL, INTENT(INOUT)                         :: building_present1          ! at least one building is present in the source file   

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER, INTENT(OUT)                           :: mm                         ! source identification number [-]
REAL   , INTENT(OUT)                           :: x                          ! x coordinate of source location (RDM [m])                 
REAL   , INTENT(OUT)                           :: y                          ! y coordinate of source location (RDM [m])
REAL   , INTENT(OUT)                           :: qob                        ! emission strength [g/s]
REAL   , INTENT(OUT)                           :: qww                        ! heat content [MW]
REAL   , INTENT(OUT)                           :: hbron                      ! emission height at source (stack height), without plume rise [m]
REAL   , INTENT(OUT)                           :: diameter                   ! diameter area source (NOT stack diameter) [m]
REAL   , INTENT(OUT)                           :: sigz0                      ! initial vertical dispersion length [m]
real   , INTENT(OUT)                           :: D_stack                    ! diameter of the stack [m]
real   , INTENT(OUT)                           :: D_stack_outer              ! outer diameter of the stack [m]
real   , INTENT(OUT)                           :: V_stack                    ! exit velocity of plume at stack tip [m/s]
real   , INTENT(OUT)                           :: Ts_stack                   ! temperature of effluent from stack [K]
logical, INTENT(OUT)                           :: emis_horizontal            ! horizontal outflow of emission
type(Tbuilding), INTENT(OUT)                   :: building                   ! structure with building parameters
INTEGER, INTENT(OUT)                           :: ibtg                       ! diurnal emission variation code [-]
INTEGER, INTENT(OUT)                           :: ibroncat                   ! emission category code [-]
INTEGER, INTENT(OUT)                           :: iland                      ! country/area code [-]
INTEGER, INTENT(OUT)                           :: idgr                       ! particle size distribution code [-]
LOGICAL, INTENT(OUT)                           :: end_of_file                ! end of file has been reached
TYPE (TError), INTENT(INOUT)                   :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                        :: ierr                       ! I/O error value
REAL                                           :: gl                         ! x coordinate of source location (longitude [degrees])                 
REAL                                           :: gb                         ! y coordinate of source location (latitude [degrees])                 
REAL                                           :: building_gl                ! x coordinate of the building (longitude [degrees])
REAL                                           :: building_gb                ! y coordinate of the building (latitude [degrees])
CHARACTER*512                                  :: cbuf                       ! character buffer, used to store an emission record
real                                           :: Ts_stack_C                 ! temperature of effluent from stack [C]
LOGICAL                                        :: allow_sigz0_point_source_loc ! allow initial sigma for point sources; local variable

!-------------------------------------------------------------------------------------------------------------------------------
100 FORMAT (i4, 2f8.3,  e10.3, f7.3, f6.1, f7.0, f6.1, 4i4)         ! format for reading fixed format file with lon-lat coordinates
150 FORMAT (i4, 2f8.0,  e10.3, f7.3, f6.1, f7.0, f6.1, 4i4)         ! format for reading fixed format file with RDM coordinates

! write(*,*) 'ops_emis_read_annual1, fu_bron, brn_version, VsDs_opt: ',  fu_bron, brn_version, VsDs_opt

! Check for optional srgument allow_sigz0_point_source:
if (present(allow_sigz0_point_source)) then
   allow_sigz0_point_source_loc = allow_sigz0_point_source
else
   allow_sigz0_point_source_loc = .true. ! default value
endif

! Default no horizontal outflow, no exit velocity/stack diameter/effluent gas temperature, no building
emis_horizontal = .FALSE.
D_stack         = -999.0
D_stack_outer   = -999.0
V_stack         = -999.0
Ts_stack        = -999.0
building%type   =      0 
building%length = -999.0
building%width  = -999.0
building%height = -999.0
building%orientation = -999.0
building%x      = -999.0
building%y      = -999.0

! Read string cbuf from emission file:
CALL sysread(fu_bron, cbuf, end_of_file, error)
IF (error%haserror) GOTO 9999 

IF (.NOT. end_of_file) THEN 
   !*************************************************************************
   !  New brn-file, free format 
   !  BRN-VERSION 0 -> Default (if no ! BRN-VERSION can be found) -> old brn-file, no stack parameters
   !  BRN-VERSION 1 -> no D_stack, V_stack, Ts_stack 
   !  BRN-VERSION 2 -> include D_stack, V_stack, Ts_stack 
   !  BRN-VERSION 3 -> include D_stack, V_stack, Ts_stack, building%type 
   !  BRN-VERSION 4 -> free format, include include D_stack, V_stack, Ts_stack, building%length, building%width, building%height, building%orientation
   !  BRN-VERSION 5 -> free format, as BRN-VERSION4, but includes center x, y of the building
   !*************************************************************************
      
   IF (brn_version .GE. 1) THEN
       ! Read emission line:
       IF (VsDs_opt) then
          IF (brn_version .GE. 5) THEN
             READ (cbuf, *, IOSTAT = ierr) mm, x, y, qob, qww, hbron, diameter, sigz0, D_stack, D_stack_outer, V_stack, Ts_stack_C, ibtg, ibroncat, iland, idgr,  &
                                           building%length, building%width, building%height, building%orientation, building%x, building%y

             ! Building orientation must be between 0 and 180 degrees:
             if (.not. is_missing (building%orientation)) building%orientation = modulo(building%orientation, 180.0)  
             
             ! Set flag if one building is present:
             if (.not. building_present1) building_present1 = (.not. (is_missing(building%length) &
                                                                      .or. is_missing(building%width) &
                                                                      .or. is_missing(building%height) &
                                                                      .or. is_missing(building%orientation) &
                                                                      .or. is_missing(building%x) &
                                                                      .or. is_missing(building%y)) )

          ELSEIF (brn_version .EQ. 4) THEN
             READ (cbuf, *, IOSTAT = ierr) mm, x, y, qob, qww, hbron, diameter, sigz0, D_stack, V_stack, Ts_stack_C, ibtg, ibroncat, iland, idgr,  &
                                           building%length, building%width, building%height, building%orientation
            
             ! Building orientation must be between 0 and 180 degrees:
             if (.not. is_missing (building%orientation)) building%orientation = modulo(building%orientation, 180.0)  
             
             ! Set flag if one building is present:
             if (.not. building_present1) building_present1 = (.not. (is_missing(building%length) &
                                                                      .or. is_missing(building%width) &
                                                                      .or. is_missing(building%height) &
                                                                      .or. is_missing(building%orientation)))
             
          ELSEIF (brn_version .EQ. 3) THEN
             READ (cbuf, *, IOSTAT = ierr) mm, x, y, qob, qww, hbron, diameter, sigz0, D_stack, V_stack, Ts_stack_C, ibtg, ibroncat, iland, idgr, building%type
          ELSE 
             ! VsDs_Opt = TRUE, so brn_version = 2
             READ (cbuf, *, IOSTAT = ierr) mm, x, y, qob, qww, hbron, diameter, sigz0, D_stack, V_stack, Ts_stack_C, ibtg, ibroncat, iland, idgr
          ENDIF ! Check BRN-VERION
          
          ! Negative V_stack in input -> horizontal outflow (except V_stack = -999 -> missing value):
          if (V_stack .lt. 0.0 .and. .not. is_missing(V_stack)) then
             V_stack = -V_stack
             emis_horizontal = .TRUE.
          endif
       
       ELSE
          ! BRN-VERSION = 1
          READ (cbuf, *, IOSTAT = ierr) mm, x, y, qob, qww, hbron, diameter, sigz0, ibtg, ibroncat, iland, idgr
       ENDIF ! VsDs_opt
       ! write(*,*) 'ops_read_source VsDs_opt = ',VsDs_opt
       ! write(*,'(a,i6,10(1x,e12.5),4(1x,i4),1x,l6)') 'ops_read_source a ',mm, x, y, qob, qww, hbron, diameter, sigz0, D_stack, V_stack, Ts_stack_C, ibtg, ibroncat, iland, idgr,emis_horizontal
       ! write(*,*) 'ops_read_source a, nrec, ierr = ',nrec,ierr  
       
       IF (ierr == 0) THEN
        
          ! Convert lon-lat coordinates to RDM coordinates; lon-lat coordinates are detected if the value read for y is less than 90 degrees:
          IF ( abs(y) .LT. 90 ) THEN
            gb = y
            gl = x
            CALL geo2amc(gb, gl, x, y) ! (x,y) in km
            x = AINT(x*1000.) ! [m]
            y = AINT(y*1000.) ! [m]
          ENDIF
          IF (brn_version .GE. 5 .AND. abs(building%y) .LT. 90 ) THEN
               building_gb = building%y 
               building_gl = building%x
               CALL geo2amc(building_gb, building_gl, building%x, building%y)
               building%x = AINT(building%x*1000.)
               building%y = AINT(building%y*1000.)
          ENDIF
       ELSE   
          CALL SetError('Error when reading record from emission file; see compiler manual for IO errors',error)
          CALL ErrorParam('IO error',ierr,error)
          goto 9999
       ENDIF
   ELSE
      !*******************************************************
      !  BRN-VERSION 0
      !  Old brn-file, fixed format 
      !  Reading of D_stack, V_stack, Ts_stack not supported.
      !*******************************************************
      ! In the old format, if there is a dot at position 9, coordinates are assumed to be lon-lat
      IF ( cbuf(9:9) .EQ. '.' ) THEN
    
         ! Read source record with lon-lat coordinates (gl,gb) 
         ! "g" << geographical coordinates; "l" << lengtegraad = longitude, "b" << breedtegraad = latitude
         READ (cbuf, 100, IOSTAT = ierr) mm, gl, gb, qob, qww, hbron, diameter, sigz0, ibtg, ibroncat, iland, idgr

         IF (ierr == 0) THEN
   
            ! Convert lon-lat coordinates to RDM coordinates
            CALL geo2amc(gb, gl, x, y) ! (x,y) in km
            x = AINT(x*1000.) ! [m]
            y = AINT(y*1000.) ! [m]
         ENDIF
      ELSE
      
         ! Read source record with RDM coordinates:
         READ (cbuf, 150, IOSTAT = ierr) mm, x, y, qob, qww, hbron, diameter, sigz0, ibtg, ibroncat, iland, idgr
      ENDIF
   ENDIF ! IF (brn_version .GE. 1)

   ! Current emission record has been read and coordinates have been converted to RDM; 
   ! add  1 to record number (unless ierr < 0 = end-of-file):
   IF (ierr .GE. 0 ) nrec = nrec + 1 
   ! write(*,*) 'nrec, ierr = ',nrec,ierr 
   ! write(*,'(a,a)') 'cbuf: ',trim(cbuf) 
   
   IF (ierr == 0) THEN
     
      ! Check emission strength, heat content, emission height and diameter area source.
      ! Note: check is only performed inside check_source2 if no error has occurred; 
      !       therefore there is no need to check for error%haserror here each time.
      
      ! 
    
      ! Check range for 
      !    deviation                     : 0 <= sigz0 <= 5000 m
      !    diurnal variation             : -999 <= ibtg <= 999
      !    emission category             : 1 <= ibroncat <= 9999
      !    country (= 'land')            : 1 <= iland <= 9999
      !    paricle size distribution code: -MAXDISTR <= idgr <= MAXDISTR
      if (brn_version .lt. 2) then
         ! Adjust value within range and continue OPS; write warning to log-file (backward compatibility for old emission files):
         CALL check_source (nrec, '<emission strength [g/s]>', 0., 99999., qob, error)
         if (.not. is_missing(qww)) CALL check_source(nrec, '<heat content [MW]>', 0., 999., qww, error) 
         CALL check_source (nrec, '<emission height [m]>', 0., 5000.0, hbron, error)
         CALL check_source (nrec, '<diameter area source [m]>',-999999., 999999., diameter, error)
         CALL check_source (nrec, '<deviatie>', 0., 5000.0, sigz0, error)
         CALL check_isource(nrec, '<variatie>', -999, 999, ibtg, error)
         CALL check_isource(nrec, '<categorie>', 1, 9999, ibroncat, error)
         CALL check_isource(nrec, '<land>', 1, 9999, iland, error)
         CALL check_isource(nrec, '<verdeling>', -MAXDISTR, MAXDISTR, idgr, error)
      else
         ! Generate error and stop OPS:
         CALL check_source2('<emission strength [g/s]>', 0., 99999., qob, error)
         if (.not. is_missing(qww)) CALL check_source2('<heat content [MW]>', 0., 999., qww, error) 
         ! CALL check_source2('<emission height [m]>', 0., HUMAX, hbron, error)
         CALL check_source2('<emission height [m]>', 0., 5000.0, hbron, error)
         CALL check_source2('<diameter area source [m]>',-999999., 999999., diameter, error)
         CALL check_source2 ('<deviatie>', 0., 5000.0, sigz0, error)
         CALL check_isource2('<variatie>', -999, 999, ibtg, error)
         CALL check_isource2('<categorie>', 1, 9999, ibroncat, error)
         CALL check_isource2('<land>', 1, 9999, iland, error)
         CALL check_isource2('<verdeling>', -MAXDISTR, MAXDISTR, idgr, error)
        
         ! Check stack parameters:
         call check_stack_param(qww, VsDs_opt, D_stack, D_stack_outer, V_stack, Ts_stack_C, error)

         ! Check building type:
         if (brn_version .ge. 3) then
              CALL check_isource2('<gebouw type voor gebouw effect>', 0, ncolBuildingEffectTable, building%type, error)
         endif

         ! Check building parameters:
         if (brn_version .ge. 4) then
            call check_building_param(brn_version, building, hbron, qww, D_stack, D_stack_outer, V_stack, error)
         endif
      endif
      
      ! sigz0 must be zero for point sources, unless you specify -allow_sigz0_point_source:
      if (diameter .eq. 0.0 .and. .not. allow_sigz0_point_source_loc .and. sigz0 .gt. 0.0) then
         CALL SetError('<initial sigma_z> must be zero for point sources (for backward compatibility); ', &
                       'either set sigma_z = 0 m or use option -allow_sigz0_point_source on the command line', error)
         CALL ErrorParam('diameter', diameter, error)
         CALL ErrorParam('initial sigma_z', sigz0, error)
         CALL ErrorParam('command line argument -allow_sigz0_point_source', allow_sigz0_point_source_loc, error)
      endif
         
      if (VsDs_opt) then
         ! Convert Ts_stack to K:
         if (is_missing(Ts_stack_C)) then
            Ts_stack = Ts_stack_C
         else
            Ts_stack = Ts_stack_C + T0   
         endif
      endif
    
      ! Check whether ibtg and idgr distributions in this record have been read (using presentcode array).
      ! Check whether ibtg is not for NH3 (icm=3) and NOx (icm=2) if a special diurnal variation (4 or 5) is used.
      ! Check whether particle size distribution has been read.       
      IF (.NOT.((icm == 2 .OR. icm == 3) .AND. (ibtg == 4 .OR. ibtg == 5)))  THEN
          CALL check_verdeling(ibtg, presentcode, 1, 3, 'ibtg', error)
      ENDIF
      IF (check_psd) THEN                                                     
        CALL check_verdeling(idgr, presentcode, 2, 4, 'idgr', error)
      ENDIF
      IF (error%haserror) GOTO 9999    

   ELSE

      ! IERR .NE. 0 ->  I/O error has occurred - set error message:
      CALL SetError('I/O error while reading emissions; for a list of I/O errors, see your compiler manual.', error)
      CALL ErrorParam('I/O error', ierr, error)
      goto 9999
   ENDIF  ! IF I/O error
ENDIF  ! IF (.NOT. end_of_file)

RETURN

9999 CALL ErrorParam('emission record', cbuf, error)
CALL ErrorParam('record nr. in emission file', nrec, error)
CALL ErrorParam('selected source number', numbron, error)
CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE ops_emis_read_annual1

END MODULE m_ops_emis
