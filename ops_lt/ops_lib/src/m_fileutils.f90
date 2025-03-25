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
! DESCRIPTION          : This module contains all utilities handling files.
! IMPLEMENTS           : - getdirectory: extracts directory from filepath.
!                      : - getfilename: extracts filename from filepath.
!                      : - getbasename: extracts basename from filepath.
!                      : - chkexist: checks existence of a file.
!                      : - sysopen: opens a file for reading or writing.
!                      : - sysclose: closes a file.
!                      : - sysread: reads a string from an input device.
!-------------------------------------------------------------------------------------------------------------------------------
MODULE m_fileutils

USE m_error
USE m_utils
USE m_string

IMPLICIT NONE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : getdirectory
! PURPOSE     : Extracts directory from the complete filepath.
! INPUTS      : fullpath (character*(*)). The full path of the file.
! OUTPUTS     : directory (character*(*)). The directory extracted from the file path.
!               error  (type TError). Is assigned when string function fails.
! RESULT      : .TRUE. when the file exists, .FALSE. if not.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE getdirectory
  MODULE PROCEDURE get_directory
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : getfilename
! PURPOSE     : Extracts filename from a complete filepath.
! INPUTS      : fullpath (character*(*)). The full path of the file.
! OUTPUTS     : filename (character*(*)). The filename extracted from the full file path.
!               error  (type TError). Is assigned when string function fails.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE getfilename
  MODULE PROCEDURE get_filename
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : getbasename
! PURPOSE     : Extracts basename from a complete filepath.
! INPUTS      : fullpath (character*(*)). The full path of the file.
! OUTPUTS     : basename (character*(*)). The basename extracted from the full file path (path and extension stripped).
!               error  (type TError). Is assigned when string function fails.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE getbasename
  MODULE PROCEDURE get_basename
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION    : chkexist
! PURPOSE     : Checking the existence of a file. If the file does not exist the error message is assigned. The callback of the
!               error is not assigned, so that it appears the non-existing error is detected in the calling procedure (which is
!               what the user wants to know).
! AUTHOR      : Martien de Haan (ARIS).
! INPUTS      : fname  (character*(*)). The full path of the file.
! OUTPUTS     : error  (type TError). Is assigned when the file does not exist.
! RESULT      : .TRUE. when the file exists, .FALSE. if not.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE chkexist
  MODULE PROCEDURE chk_file_exist
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION    : sysopen
! PURPOSE     : Opens a file for reading or writing.
! AUTHOR      : Erik Bobeldijk/Franka Loeve (Cap Volmac)
! INPUTS      : iu     (integer). Unit number of file.
!               filename (character*(*)). Path of file to be opened.
!               rw     (character*(*)). Whether reading or writing. Options:
!                      'r':  Reading of text file.
!                      'w':  Writing of text file.
!                      'rb':  Reading of binary file.
!                      'wb':  Writing of binary file.
!                      Any other values leads to a program error. filetype (character*(*)). Optional parameter. Description of
!                      type of file. Written in error messages.
! OUTPUTS     : error  (type TError). Is assigned when the file could not be opened.
! RESULT      : .FALSE. if an error is detected (error is then also set).
!               .TRUE.  if everything went all right.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE sysopen
  MODULE PROCEDURE sys_open_file
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : sysclose
! PURPOSE     : Closes a file. Low level.
! AUTHOR      : Erik Bobeldijk/Franka Loeve (Cap Volmac)
! ADAPTATIONS : 2002 - Error handling through error object (Martien de Haan, ARIS).
! INPUTS      : iu     (integer). Unit number of file.
!               filename (character*(*)). Name of file. Only relevant when error is written.
! OUTPUTS     : error  (type TError). Is assigned when the file could not be closed.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE sysclose
  MODULE PROCEDURE sys_close_file
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : sysread
! PURPOSE     : Reads a string from an input device.
! PRECONDITION: Input file: Ascii, recordlength <= 512
! AUTHOR      : Erik Bobeldijk/Franka Loeve (Cap Volmac)
! ADAPTATIONS : 2002 - Error handling through error object (Martien de Haan, ARIS).
! INPUTS      : iu     (integer). Unit number of file.
! OUTPUTS     : end_of_file (logical) Whether end-of-file was reached, so that nothing was read.
!               in_str (character*(*)) Character string to be read.
!               error  (type TError). Is assigned when the file could not be closed.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE sysread
  MODULE PROCEDURE sys_read_string
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! PRIVATE PROCDEDURES
!-------------------------------------------------------------------------------------------------------------------------------

PRIVATE sysopen_read                                                           ! Called by sysopen
PRIVATE sysopen_write                                                          ! Called by sysopen
PRIVATE sysopen_read_bin                                                       ! Called by sysopen
PRIVATE sysopen_direct                                                         ! Called by sysopen

!-------------------------------------------------------------------------------------------------------------------------------
! Implementation
!-------------------------------------------------------------------------------------------------------------------------------

CONTAINS
! Replace all 'wrong' slashes in filename by the correct slash:
!    windows: replace all '/'s by '\'s 
!    unix:    replace all '\'s by '/'s 
subroutine fix_slashes(filename)
USE m_utils, only: GetOS
implicit none
    character(len=*), intent(inout) :: filename

    integer   :: os, islash
    CHARACTER :: slash
    character(len=1), parameter :: slashes(2) = (/ '\', '/' /)

    call GetOS(os, slash)
    do islash = 1,size(slashes)
       if (slash == slashes(islash)) cycle
       do while (index(filename,slashes(islash)) /= 0)
          filename(index(filename,slashes(islash)):index(filename,slashes(islash))) = slash 
       end do
    end do
end subroutine fix_slashes


!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: getdirectory
! PURPOSE:    See interface definition.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE get_directory(fullpath, directory, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: get_directory

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'get_directory')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: fullpath                   ! 

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*(*), INTENT(OUT)                       :: directory                  ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER                                          :: positie                    ! position counter
LOGICAL                                          :: found                      ! \ or / has been found
INTEGER                                          :: os                         ! current operating system
CHARACTER*1                                      :: slash                      ! directory separator (\ or /)

!-------------------------------------------------------------------------------------------------------------------------------
!
! Determine directory separator slash
!
CALL GetOS(os, slash)
!
! Get position of last \ or /
!
positie=LEN_TRIM(fullpath)
found=.false.
DO WHILE (.not.found.and.positie.gt.0)
  IF (fullpath(positie:positie).eq. slash) THEN
      found=.true.
   ELSE
      positie=positie-1
   ENDIF
ENDDO
!
! Assign directory string
!
IF (found) THEN
   CALL StringCopy(fullpath, 1, positie, directory, error)
ELSE
   directory=''
ENDIF

IF (error%haserror) THEN
   CALL ErrorCall(ROUTINENAAM, error)
ENDIF

END SUBROUTINE get_directory

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: get_filename
! PURPOSE:    See interface definition.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE get_filename(fullpath, filename, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: get_filename

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'get_filename')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: fullpath                   ! 

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*(*), INTENT(OUT)                       :: filename                   ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record
!
! LOCAL VARIABLES
INTEGER                                          :: positie                    ! position counter
LOGICAL                                          :: found                      ! \ or / has been found
INTEGER                                          :: os                         ! current operating system
CHARACTER                                        :: slash                      ! directory separator (\ or /)
!-------------------------------------------------------------------------------------------------------------------------------
!
! First get filename
!
CALL GetOS(os, slash)
!
! Get position of first \ or / (go backwards)
!
positie=LEN_TRIM(fullpath)
found=.false.
DO WHILE (.not.found.and.positie.gt.0)
  IF (fullpath(positie:positie).eq.slash) THEN
      found=.true.
   ELSE
      positie=positie-1
   ENDIF
ENDDO
!
! Assign filename string
!
IF (found) THEN
   CALL StringCopy(fullpath,positie+1,LEN_TRIM(fullpath), filename, error)
ELSE
   filename=fullpath
ENDIF

IF (error%haserror) THEN
  CALL ErrorCall(ROUTINENAAM, error)
ENDIF

RETURN
END SUBROUTINE get_filename

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: get_basename
! PURPOSE:    See interface definition.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE get_basename(fullpath, basename, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: get_basename

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'get_basename')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER(LEN=*), INTENT(IN)                     :: fullpath                   ! full path and name of file

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER(LEN=*), INTENT(OUT)                    :: basename                   ! base name of file (path and extension (if present) stripped)
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record
!
! LOCAL VARIABLES
INTEGER                                          :: indx1                      ! index of last . in filename
!-------------------------------------------------------------------------------------------------------------------------------
!
! Get filename (path stipped, but extension still included)
!
call get_filename(fullpath, basename, error)

IF (.not. error%haserror) THEN 
   ! Get position of last dot (dot indicates start of extension): 
   indx1    = index(basename, '.' , .true.)
   IF (indx1 .gt. 1) basename = basename(1:indx1-1) ! if no (proper) extension, then just use the filename
ELSE
   CALL ErrorCall(ROUTINENAAM, error)
ENDIF

RETURN
END SUBROUTINE get_basename

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION: chkexist
! PURPOSE:  See interface definition.
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION chk_file_exist(fname, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: chk_file_exist

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'chk_file_exist')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: fname                      ! file naam

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! FUNCTION RESULT
LOGICAL                                          :: chk_file_exist             ! 

!-------------------------------------------------------------------------------------------------------------------------------
INQUIRE (FILE = fname, EXIST = chk_file_exist)

IF (.NOT. chk_file_exist) THEN
  CALL SetError('File does not exist', error)
  CALL ErrorParam('filename', fname, error)
  CALL ErrorCall(ROUTINENAAM, error)
ENDIF

RETURN
END FUNCTION chk_file_exist


! PURPOSE: convert string to upper case
subroutine upcase2(a)
implicit none
   character(len=*), intent(inout) :: a
   integer :: i
   do i = 1,len(a)
      if (ichar('a') <= ichar(a(i:i)) .and. ichar(a(i:i)) <= ichar('z')) a(i:i) = char(ichar(a(i:i)) + ichar('A') - ichar('a'))
   end do
end subroutine upcase2


! PURPOSE:    Open a file
FUNCTION sys_open_file(iu, filename, rw, filetype, error, LREC)
! !DEC$ ATTRIBUTES DLLEXPORT:: sys_open_file

   integer,   INTENT(IN)  :: iu        ! Unit number of file
   character(len=*),     INTENT(IN)  :: filename  ! File path
   character(len=*),     INTENT(IN)  :: rw        ! Whether reading or writing (see interface)
   character(len=*),     INTENT(IN)  :: filetype  ! Type of the file, written in error messages
   TYPE (TError),        INTENT(INOUT) :: error     ! Error handling record
   INTEGER, OPTIONAL,    INTENT(IN)  :: LREC      ! Lenght of a direct access record
   
   LOGICAL                           :: sys_open_file              ! .FALSE. when error detected
   
   CHARACTER(len=2)      :: rw_upcase    ! Character looked at
   integer    :: io_status    ! Status of opening file, indicates possible error
   logical               :: opened
   character(len=512), PARAMETER :: ROUTINENAAM = 'sysopen'
   
   ! Check wether string is 1 or 2 length, otherwise this is definitely a programming error.
   IF (LEN_TRIM(rw)== 0 .OR. LEN_TRIM(rw)>2) GOTO 1000

   inquire(unit=iu,opened=opened)
   if (opened) then
      CALL SetError('File "'//trim(filename)//'" is already open', error)
      goto 2000
   end if
   
   ! Check first character: whether reading or writing.
   rw_upcase = trim(rw)
   call upcase2(rw_upcase)
   IF (rw_upcase == 'R') then
      CALL sysopen_read(iu, filename, io_status)
   ELSE IF (rw_upcase == 'W') then
      CALL sysopen_write(iu, filename, io_status)
   ELSE IF (rw_upcase == 'D') then
      CALL sysopen_direct(iu, filename, LREC, io_status)
   ELSE IF (rw_upcase == 'WD') then
      CALL sysopen_direct_write(iu, filename, LREC, io_status)
   ELSE IF (rw_upcase == 'RB') then
      CALL sysopen_read_bin(iu, filename, io_status)
   ELSE IF (rw_upcase == 'WB') then
      CALL sysopen_write_bin(iu, filename, io_status)
   ELSE
      GOTO 1000
   ENDIF
   
   sys_open_file = io_status == 0
   if (.not. sys_open_file) then
      IF (index(rw_upcase,'W')==0) THEN
         CALL SetError('Could not open file for reading', error)
      ELSE
         CALL SetError('Could not open file for writing', error)
      ENDIF
      CALL ErrorParam('Io-status', io_status, error)
      IF (index(rw_upcase,'D')/=0) THEN
         CALL ErrorParam('File type', 'binary direct access', error)
      ELSE IF (index(rw_upcase,'B')/=0) THEN
         CALL ErrorParam('File type', 'binary', error)
      ELSE
         CALL ErrorParam('File type', 'text', error)
      ENDIF
      GOTO 2000
   ENDIF
   
   RETURN
   
1000 continue
   CALL SetError('PROGRAM ERROR: rw string incorrect', error)
   CALL ErrorParam('rw string', rw, error)
   
2000 continue
   CALL ErrorParam('Filename', filename, error)
   CALL ErrorParam('Filetype', filetype, error)
   CALL ErrorCall(ROUTINENAAM, error)
   sys_open_file = .FALSE.

END FUNCTION sys_open_file

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: sysclose
! PURPOSE:    See interface definition.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE sys_close_file(iu, filename, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: sys_close_file

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: iu                         ! Unit number of file
CHARACTER*(*), INTENT(IN)                        :: filename                   ! File path

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TError), INTENT(INOUT)                       :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: io_status                  ! Status of I/O action

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'sysclose')

!-------------------------------------------------------------------------------------------------------------------------------
CLOSE (iu, IOSTAT = io_status)

IF (io_status .GT. 0) THEN
   CALL SetError('Error on closing file', error)
   CALL ErrorParam('filename', filename, error)
   CALL ErrorParam('fileunit', iu, error)
   CALL ErrorParam('io-status', io_status, error)
   CALL ErrorCall(ROUTINENAAM, error)
ENDIF

RETURN
END SUBROUTINE sys_close_file

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: sysopen_read
! PURPOSE   : Opening of text file for reading. See interface definition
! AUTHOR    : Erik Bobeldijk/Franka Loeve (Cap Volmac)
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE sysopen_read(iu, fnam, io_status)

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: iu                         ! 
CHARACTER*(*), INTENT(IN)                        :: fnam                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER,   INTENT(OUT)                           :: io_status                  ! Status of I/O action

! LOCAL VARIABLES
INTEGER                                          :: flen                       ! Length of filename

!-------------------------------------------------------------------------------------------------------------------------------
flen = LEN_TRIM(fnam)

#ifndef UNIX
OPEN (iu, FILE=fnam(1:flen), ACTION='READ', SHARED, SHARE='denywr', FORM='formatted', IOSTAT=io_status)
#else
OPEN (iu, FILE=fnam(1:flen), ACTION='READ', FORM='formatted', IOSTAT=io_status)
#endif

RETURN
END SUBROUTINE sysopen_read

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: sysopen_read_bin
! PURPOSE   : Opening of binary file for reading.
! AUTHOR    : Erik Bobeldijk/Franka Loeve (Cap Volmac)
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE sysopen_read_bin(iu, fnam, io_status)

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: iu                         ! 
CHARACTER*(*), INTENT(IN)                        :: fnam                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER,   INTENT(OUT)                           :: io_status                  ! Status of I/O action

! LOCAL VARIABLES
INTEGER                                          :: flen                       ! 

!-------------------------------------------------------------------------------------------------------------------------------
flen = LEN_TRIM(fnam)

#ifndef UNIX
OPEN (iu, FILE=fnam(1:flen), ACTION='READ', SHARED, SHARE='denywr', FORM='unformatted', IOSTAT=io_status)
#else
OPEN (iu, FILE=fnam(1:flen), ACTION='READ', FORM='unformatted', IOSTAT=io_status)
#endif

RETURN
END SUBROUTINE sysopen_read_bin

! PURPOSE   : Opening of binary file for writing.
SUBROUTINE sysopen_write_bin(iu, fnam, io_status)
   integer,   INTENT(IN)  :: iu         ! file unit
   character(len=*),     INTENT(IN)  :: fnam       ! file name
   integer,   INTENT(OUT) :: io_status  ! Status of I/O action
   
   integer                                          :: flen                       ! 
   
   flen = LEN_TRIM(fnam)
   
#  ifndef UNIX
      OPEN (iu, FILE=fnam(1:flen), ACTION='WRITE', SHARED, SHARE='denywr', FORM='unformatted', IOSTAT=io_status)
#  else
      OPEN (iu, FILE=fnam(1:flen), ACTION='WRITE', FORM='unformatted', IOSTAT=io_status)
#  endif
END SUBROUTINE sysopen_write_bin

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: sysopen_write
! PURPOSE   : Opening of text file for writing.
! AUTHOR    : Erik Bobeldijk/Franka Loeve (Cap Volmac)
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE sysopen_write(iu, fnam, io_status)

USE m_error

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: iu                         ! Channel to write to
CHARACTER*(*), INTENT(IN)                        :: fnam                       ! Filename

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER,   INTENT(OUT)                           :: io_status                  ! Status of I/O action

! LOCAL VARIABLES
INTEGER                                          :: flen                       ! Length of filename

!-------------------------------------------------------------------------------------------------------------------------------
flen = LEN_TRIM(fnam)

#ifndef UNIX
OPEN (iu, FILE=fnam(1:flen), ACTION='READWRITE', SHARED, SHARE='denywr', IOSTAT=io_status)
#else
OPEN (iu, FILE=fnam(1:flen), ACTION='READWRITE', IOSTAT=io_status)
#endif

RETURN
END SUBROUTINE sysopen_write

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: sysopen_direct
! PURPOSE   : Opening of direct-access file for reading.
! AUTHOR    : Erik Bobeldijk/Franka Loeve (Cap Volmac)
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE sysopen_direct(iu, fnam, LREC, io_status)

USE m_error

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: iu                         ! Channel to write to
CHARACTER*(*), INTENT(IN)                        :: fnam                       ! Filename
INTEGER, optional,  INTENT(IN)                   :: LREC                       ! Lenght of a direct access record

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER,   INTENT(OUT)                           :: io_status                  ! Status of I/O action

! LOCAL VARIABLES
INTEGER :: lrec_loc

lrec_loc = 64
if (present(LREC)) lrec_loc = LREC

#ifndef UNIX
OPEN (iu,FILE=trim(fnam), ACTION='READ', SHARED, SHARE='denywr', ACCESS='direct', RECL=lrec_loc, IOSTAT=io_status)
#else
OPEN (iu,FILE=trim(fnam), ACTION='READ', ACCESS='direct', RECL=lrec_loc, IOSTAT=io_status)
#endif
END SUBROUTINE sysopen_direct

! PURPOSE   : Opening of direct-access file for writing
SUBROUTINE sysopen_direct_write(iu, fnam, LREC, io_status)
USE m_error
   integer,   INTENT(IN)   :: iu         ! Channel to write to
   character(len=*),     INTENT(IN)   :: fnam       ! Filename
   INTEGER, optional,    INTENT(IN)   :: LREC       ! Lenght of a direct access record
   integer,   INTENT(OUT)  :: io_status  ! Status of I/O action
   
   INTEGER :: lrec_loc
   
   lrec_loc = 64
   if (present(LREC)) lrec_loc = LREC
   
#  ifndef UNIX
      OPEN (iu,FILE=trim(fnam), ACTION='WRITE', SHARED, SHARE='denywr', ACCESS='direct', RECL=lrec_loc, IOSTAT=io_status)
#  else
      OPEN (iu,FILE=trim(fnam), ACTION='WRITE', ACCESS='direct', RECL=lrec_loc, IOSTAT=io_status)
#  endif
END SUBROUTINE sysopen_direct_write

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: sysread
! PURPOSE   : Reading a string from a file
! AUTHOR    : Erik Bobeldijk/Franka Loeve (Cap Volmac)
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE sys_read_string(fdin, in_str, end_of_file, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: sys_read_string

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'sys_read_string')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: fdin                       ! File id van de input file

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*(*), INTENT(OUT)                       :: in_str                     ! character string to be read
LOGICAL,   INTENT(OUT)                           :: end_of_file                ! TRUE if end of file
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER                                          :: io_status                  ! Status of IO-actions
logical itsopen

!-------------------------------------------------------------------------------------------------------------------------------
end_of_file = .FALSE.
in_str      = ' '
inquire(unit=fdin, opened=itsopen)
if (.not. itsopen) then
   CALL SetError('Error reading file', error)
   CALL ErrorParam('file is not open', io_status, error)
   CALL ErrorCall(ROUTINENAAM, error)
   return
end if
READ (fdin, '(A)', IOSTAT = io_status) in_str

IF (io_status .LT. 0) THEN
   end_of_file = .TRUE.
ELSE IF (io_status .GT. 0) THEN
   CALL SetError('Error reading file', error)
   CALL ErrorParam('io-status', io_status, error)
   CALL ErrorCall(ROUTINENAAM, error)
ENDIF

RETURN
END SUBROUTINE sys_read_string

!-------------------------------------------------------------------------------------------------------------------------------

END MODULE m_fileutils
