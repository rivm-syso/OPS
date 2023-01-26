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
! DESCRIPTION           : Retrieves the command line arguments and determines whether syntax is correct. If so, the complete
!                         name (incl path) of the control file is derived, which may be require the current working directory.
!                         As a first parameter the diag flag is returned.
!                         As a second parameter the subbron flag is returned.
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_get_arg

implicit none

contains

SUBROUTINE ops_get_arg (diag, subbron, domlu, varz, perc, mindist, maxdist, error)

USE m_error
USE m_fileutils
USE m_utils
USE m_commonfile

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_get_arg')

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: diag                       ! = 1,3 (argument -r) -> print version number and quit
LOGICAL,   INTENT(INOUT)                         :: subbron                    
LOGICAL,   INTENT(INOUT)                         :: domlu                      ! use dominant land use instead of land use percentages
LOGICAL,   INTENT(INOUT)                         :: varz                    
LOGICAL,   INTENT(INOUT)                         :: perc                    
LOGICAL,   INTENT(INOUT)                         :: mindist                    ! option to use mimimum distance for which a result will be calculated 
LOGICAL,   INTENT(INOUT)                         :: maxdist                    ! option to use maximum distance for which a result will be calculated 
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: os                         ! operating system: 0 = UNIX, 1 = Windows
INTEGER*4                                        :: numarg                     ! number of arguments
INTEGER*4                                        :: ISTAT                      ! return code of GETCWD
INTEGER*4                                        :: iarg                       ! number of arguments
CHARACTER*55                                     :: commandname                ! command name of this program (excluding path)
CHARACTER*512                                    :: nfile                      ! temporary variable for directory name
CHARACTER*512                                    :: progpath                   ! full name of this program (including path)
CHARACTER*512, DIMENSION(:), POINTER             :: arg                        ! argument list excluding progpath
CHARACTER*1                                      :: slash                      ! "/" of "\"
LOGICAL*4                                        :: iexist                     ! TRUE if file exists

! FUNCTIONS
!     INTEGER*4     GETCWD

!-------------------------------------------------------------------------------------------------------------------------------

! Retrieve the command line arguments: progpath, numarg and arg. The procedure is irrespective of the operating system.
CALL GetCLArg(progpath, numarg, arg, error)
IF (error%haserror) GOTO 9999

! Retrieve commandname, which is the file part of progpath.
CALL getfilename(progpath, commandname, error)

! Check the number and contents of arguments.
diag = 0

! Set defaults for optional arguments
subbron = .TRUE.
domlu   = .FALSE.
varz    = .FALSE.
perc    = .FALSE.
mindist = .FALSE.
maxdist = .FALSE.

! Loop over arguments and check for optional arguments:
DO iarg = 3,numarg
  IF (arg(iarg) == '-nosub')   subbron = .FALSE.
  IF (arg(iarg) == '-domlu')   domlu   = .TRUE.
  IF (arg(iarg) == '-varz')    varz    = .TRUE.
  IF (arg(iarg) == '-perc')    perc    = .TRUE.
  IF (arg(iarg) == '-mindist') mindist = .TRUE.
  IF (arg(iarg) == '-maxdist') maxdist = .TRUE.
ENDDO

! Compute number of arguments left over:
IF (.NOT.subbron) numarg = numarg -1 
IF (domlu)        numarg = numarg -1 
IF (varz)         numarg = numarg -1 
IF (perc)         numarg = numarg -1 
IF (mindist)      numarg = numarg -1 
IF (maxdist)      numarg = numarg -1 

! Check number of arguments:
IF ((numarg < 1) .OR. (numarg > 3)) THEN
  WRITE (IOB_STDOUT,'('' Usage: '',A,'' [-v] -i stuurfile'')') commandname(:LEN_TRIM(commandname))
  CALL SetError('Incorrect number of arguments', error)
  GOTO 1000

! number of arguments = 1, argument -r -> print version number and quit
ELSEIF (numarg == 1) THEN
   IF (arg(1) == '-r') THEN
      diag = 1
      INQUIRE (FILE = "ops_core.dll", EXIST = iexist)
      IF (iexist) THEN
         INQUIRE (FILE = "depac.dll", EXIST = iexist)
         IF (iexist) THEN
           INQUIRE (FILE = "ops_utils.dll", EXIST = iexist)
           IF (iexist) diag = 3
         ENDIF
      ENDIF
      GOTO 2000
   ELSE
     WRITE (IOB_STDOUT,'('' Usage: '',A,'' [-v] -i stuurfile'')') commandname(:LEN_TRIM(commandname))
     CALL SetError('Invalid first and only argument', error)
     GOTO 1000
   ENDIF

! number of arguments = 2, -i control_file (= "stuurfile")
ELSEIF (numarg == 2) THEN
  IF (.NOT.(arg(1) == '-i')) THEN
    WRITE (IOB_STDOUT,'('' Usage: '',A,'' [-v] -i stuurfile'')') commandname(:LEN_TRIM(commandname))
    CALL SetError('Invalid combination of arguments', error)
    GOTO 1000
  ELSE
    ctrnam=arg(2)
  ENDIF

! number of arguments = 3, -v -> verbose (more output), -i control_file
ELSE
  IF (.NOT.(arg(1) == '-v' .AND. arg(2) == '-i')) THEN
    WRITE (IOB_STDOUT,'('' Usage: '',A,'' [-v] -i stuurfile'')') commandname(:LEN_TRIM(commandname))
    CALL SetError('Invalid combination of arguments', error)
    GOTO 1000
  ELSE
    diag = 2
    ctrnam=arg(3)
  ENDIF
ENDIF
!
! Check whether control file exists
!
INQUIRE (FILE = ctrnam, EXIST = iexist)
IF (.NOT. iexist) THEN
  WRITE (IOB_STDOUT,'(3a)') 'Controlfile: "', ctrnam(:LEN_TRIM(ctrnam)), '" does not exist.'
  CALL SetError('Control file does not exist', error)
  CALL ErrorParam('Control file name', ctrnam, error)
  GOTO 1000
ENDIF
!
! Paste path of working directory before name of control file.
! - needed if control file does not contain a path (when OPS is started in the current working directory (CWD))
! - needed when control file contains a relative path (starts with ..).
! Both situations do not occur, when OPS is started from the GUI.
!
! NB: Mooier zou zijn om de volledige filenaam te kunnen opvragen, zoals met de CVF functie FULPATHQQ, maar dan moet er ook een
!     HP equivalent zijn.
!

! Initialise directory name:
nfile=''

! Get directory name (= path) of control file into nfile:
CALL getdirectory(ctrnam, nfile, error)

! Check for empty path or relative path and prepend path to control file name:
IF (LEN_TRIM(nfile) == 0 .OR. nfile(1:2) == '..') THEN
  ISTAT=GETCWD(nfile)
  CALL getOS(os,slash)
  ctrnam = nfile(:LEN_TRIM(nfile))//slash// ctrnam(:LEN_TRIM(ctrnam))
ENDIF

2000 CALL DEALLOC(ARG)

RETURN

!1000 DEALLOCATE(ARG)
1000 CALL DEALLOC(ARG)

9999 CALL ErrorCall(ROUTINENAAM, error)

RETURN

END SUBROUTINE ops_get_arg

end module m_ops_get_arg
