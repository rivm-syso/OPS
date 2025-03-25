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

type TKwargs
   logical :: subbron = .TRUE.
   logical :: domlu = .FALSE.  ! use dominant land use instead of land use percentages
   logical :: varz = .FALSE.
   logical :: perc = .FALSE.
   logical :: mindist = .FALSE.  ! option to use mimimum distance for which a result will be calculated 
   logical :: maxdist = .FALSE.  ! option to use maximum distance for which a result will be calculated 
   logical :: class_output = .FALSE.  ! indicator whether results for receptors will be stored per wind sector/distance/particle/stability class
   logical :: allow_sigz0_point_source = .FALSE.  ! allow initial sigma for point sources
   integer :: diag  ! = 1,3 (argument -r) -> print version number and quit
   integer :: nthreads = 1  ! number of OMP threads that can be used
   character(len=256) :: varin_file = ""  ! file with input parameters
   character(len=256) :: dir_bg = ""
end type

contains

!--------------------------------------------------------------------------------------------------------
SUBROUTINE ops_get_arg (kwargs, error)

use omp_lib, only: omp_set_num_threads

! Get command line arguments 
! Note: Error messages are handled using the error-structure, but also by writing messages to screen.
! if -i control-file has not yet been given, the name of the error file is not yet known.

USE m_error
USE m_fileutils
USE m_utils
USE m_commonfile

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_get_arg')

! SUBROUTINE ARGUMENTS - OUTPUT
type(TKwargs), intent(out) :: kwargs
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER                                          :: os                         ! operating system: 0 = UNIX, 1 = Windows
INTEGER                                          :: numarg                     ! number of arguments
INTEGER                                          :: ISTAT                      ! return code of GETCWD
INTEGER                                          :: iarg                       ! number of arguments
CHARACTER*55                                     :: commandname                ! command name of this program (excluding path)
CHARACTER*512                                    :: nfile                      ! temporary variable for directory name
CHARACTER*512                                    :: progpath                   ! full name of this program (including path)
CHARACTER*512, DIMENSION(:), POINTER             :: arg                        ! argument list excluding progpath
CHARACTER*1                                      :: slash                      ! "/" of "\"
LOGICAL                                          :: iexist                     ! TRUE if file exists
INTEGER                                          :: ierr                       ! i/o status error

! FUNCTIONS
!     INTEGER       GETCWD

!-------------------------------------------------------------------------------------------------------------------------------

! Set defaults for optional arguments
error%debug = .FALSE.   ! if true -> debug parameters are written to screen; only useful for a limited number of receptors and sources
kwargs%subbron = .TRUE.
kwargs%domlu   = .FALSE.
kwargs%varz    = .FALSE.
kwargs%perc    = .FALSE.
kwargs%mindist = .FALSE.
kwargs%maxdist = .FALSE.
kwargs%class_output  = .FALSE.
kwargs%allow_sigz0_point_source = .FALSE. 
kwargs%varin_file    = ''
kwargs%nthreads      = 1
  
! Retrieve the command line arguments: progpath, numarg and arg. The procedure
! is irrespective of the operating system.
CALL GetCLArg(progpath, numarg, arg, error)
IF (error%haserror) GOTO 9999

! Retrieve commandname, which is the file part of progpath.
CALL getfilename(progpath, commandname, error)

! Check the number and contents of arguments.
kwargs%diag = 0

! Loop over arguments and check for optional arguments:
iarg = 0
DO 
   iarg = iarg + 1
   if (iarg>size(arg,1)) exit
   if (arg(iarg) == '-nosub') then
      kwargs%subbron = .FALSE.
   else if (arg(iarg) == '-domlu') then
      kwargs%domlu   = .TRUE.
   else if (arg(iarg) == '-varz') then
      kwargs%varz    = .TRUE.
   else if (arg(iarg) == '-perc') then
      kwargs%perc    = .TRUE.
   else if (arg(iarg) == '-mindist') then
      kwargs%mindist = .TRUE.
   else if (arg(iarg) == '-maxdist') then
      kwargs%maxdist = .TRUE.
   else if (arg(iarg) == '-classoutput') then
      kwargs%class_output = .TRUE.
   else if (arg(iarg) == '-debug') then
      error%debug = .TRUE.
   else if (arg(iarg) == '-varinfile') then
      iarg = iarg + 1

      if (iarg > size(arg,1)) then
         call ops_get_arg_usage(commandname)
         WRITE(IOB_STDOUT,'(/,a,/)') 'Incorrect number of arguments; expected: -varinfile <filename>'
         CALL SetError('Incorrect number of arguments;','expected: -varinfile <filename>', error)
         GOTO 1000
      end if

      kwargs%varin_file = arg(iarg)
   else if (arg(iarg) == '-r') then
     kwargs%diag = 1
     INQUIRE (FILE = "ops_core.dll", EXIST = iexist)

     IF (iexist) THEN
        INQUIRE (FILE = "depac.dll", EXIST = iexist)

        IF (iexist) THEN
          INQUIRE (FILE = "ops_utils.dll", EXIST = iexist)

          IF (iexist) kwargs%diag = 3
        ENDIF
     ENDIF

     goto 2000
   else if (arg(iarg) == '-v') then
     kwargs%diag = 2
   else if (arg(iarg) == '-nthreads') then
     iarg = iarg + 1
     
     ! Missing next argument:
     if (iarg > size(arg,1)) then
        call ops_get_arg_usage(commandname)
        WRITE(IOB_STDOUT,'(/,a,/)') 'Incorrect number of arguments; expected: -nthreads <number of threads>'
        CALL SetError('Incorrect number of arguments;','expected: -nthreads <number of threads>',error)
        GOTO 1000
     end if
     
     ! Read number of threads for parallel computing and pass to OMP:
     read(arg(iarg),*,iostat = ierr) kwargs%nthreads
     if (ierr .ne. 0) then
        WRITE(IOB_STDOUT,'(/,a,/)') 'Incorrect use of -nthreads option; expected: -nthreads <number of threads>'
        call SetError('Incorrect use of -nthreads option;','expected: -nthreads <number of threads>',error)
        call ErrorParam('argument read after -nthreads',arg(iarg),error)
        GOTO 1000
     endif
     
   else if (arg(iarg) == '-i') then
     ! Get name of control file:
     iarg = iarg + 1
     if (iarg > size(arg,1)) then
        call ops_get_arg_usage(commandname)
        WRITE(IOB_STDOUT,'(/,a,/)') 'Incorrect number of arguments; expected: -i <control-file>'
        CALL SetError('Incorrect number of arguments;','expected: -i <control-file>', error)
        GOTO 1000
     end if
     ctrnam = arg(iarg)
     
     ! Check whether control file exists:
     INQUIRE (FILE = ctrnam, EXIST = iexist)
     IF (.NOT. iexist) THEN
       WRITE (IOB_STDOUT,'(3a)') 'Controlfile: "', ctrnam(:LEN_TRIM(ctrnam)), '" does not exist.'
       CALL SetError('Control file does not exist', error)
       CALL ErrorParam('Control file name', ctrnam, error)
       GOTO 1000
     ENDIF
     
     ! Paste path of working directory before name of control file.
     ! - needed if control file does not contain a path (when OPS is started in the current working directory (CWD))
     ! - needed when control file contains a relative path (starts with ..).
     ! Both situations do not occur, when OPS is started from the GUI.
     !
     ! NB: Mooier zou zijn om de volledige filenaam te kunnen opvragen, zoals met de CVF functie FULPATHQQ, maar dan moet er ook een
     !     HP equivalent zijn.
     
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
     
     ! Make the file names for process monitoring (log, error and progress files):
     CALL MakeMonitorNames(error)
     IF (error%haserror) GOTO 1000 ! GOTO error handling 

   else if (arg(iarg) == '-allow_sigz0_point_source') then
      kwargs%allow_sigz0_point_source = .TRUE.
   else
     call ops_get_arg_usage(commandname)
     WRITE(IOB_STDOUT,'(/,a,a,/)') 'Incorrect command line argument ',trim(arg(iarg))
     CALL SetError('Incorrect command line argument', error)
     call ErrorParam('argument',arg(iarg),error)
     GOTO 1000
   end if
ENDDO

2000 CALL DEALLOC(ARG)

RETURN

!1000 DEALLOCATE(ARG)
1000 CALL DEALLOC(ARG)

9999 CALL ErrorCall(ROUTINENAAM, error)

RETURN

END SUBROUTINE ops_get_arg

!----------------------------------------------------------------------------------------------
subroutine ops_get_arg_usage(commandname)

! Print usage message

USE m_commonfile

CHARACTER(len=*)                                 :: commandname                ! command name of this program (excluding path)

WRITE (IOB_STDOUT,'(/,'' Usage: '',A,'' -i control-file [options]'')') commandname(:LEN_TRIM(commandname))
WRITE (IOB_STDOUT,'(a)')   ' Options: '
WRITE (IOB_STDOUT,'(a)')   ' -nosub                   : no sub receptors, no sub area sources'
WRITE (IOB_STDOUT,'(a)')   ' -domlu                   : dominant land use only'
WRITE (IOB_STDOUT,'(a)')   ' -varz                    : receptor height in receptor file'
WRITE (IOB_STDOUT,'(a)')   ' -perc                    : percentages land use classes in receptor file'
WRITE (IOB_STDOUT,'(a)')   ' -mindist                 : minimal source receptor distance of 5 km'
WRITE (IOB_STDOUT,'(a)')   ' -maxdist                 : maximal source receptor distance of 25 km'
WRITE (IOB_STDOUT,'(a)')   ' -classoutput             : output per meteo class, wind sector, distance class'
! only for developers WRITE (IOB_STDOUT,'(a)')   ' -varinfile varin_file    : file with varin-parameters'    
WRITE (IOB_STDOUT,'(a)')   ' -r                       : issue release number and date'
WRITE (IOB_STDOUT,'(a)')   ' -v                       : verbose - more output in PRNFILE'
WRITE (IOB_STDOUT,'(a)')   ' -nthreads nthreads       : number of threads for parallel computing'
WRITE (IOB_STDOUT,'(a)')   ' -allow_sigz0_point_source: allow initial sigma for point sources'
WRITE (IOB_STDOUT,'(a,/)') ' More information in the OPS user manual'

end subroutine ops_get_arg_usage

end module m_ops_get_arg
