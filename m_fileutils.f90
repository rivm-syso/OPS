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
!                       Copyright by
!   National Institute of Public Health and Environment
!           Laboratory for Air Research (RIVM/LLO)
!                      The Netherlands
!   No part of this software may be used, copied or distributed without permission of RIVM/LLO (2002)
!
! MODULE               : fileutils
! NAME                 : %M%
! SCCS(SOURCE)         : %P%
! RELEASE - LEVEL      : %R% - %L%
! BRANCH -SEQUENCE     : %B% - %S%
! DATE - TIME          : %E% - %U%
! WHAT                 : %W%:%E%
! AUTHOR               : OPS-support
! FIRM/INSTITUTE       : RIVM/LLO/IS
! LANGUAGE             : FORTRAN-90
! DESCRIPTION          : This module contains all utilities handling files.
! IMPLEMENTS           : - getdirectory: extracts directory from filepath.
!                      : - getfilename: extracts filename from filepath.
!                      : - chkexist: checks existence of a file.
!                      : - sysopen: opens a file for reading or writing.
!                      : - sysclose: closes a file.
!                      : - sysread: reads a string from an input device.
! FILES AND OTHER      :
!   I/O DEVICES
! SYSTEM DEPENDENCIES  : HP-Fortran
! UPDATE HISTORY       :
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
! RESULT      : .TRUE. when the file exists, .FALSE. if not.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE getfilename
  MODULE PROCEDURE get_filename
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION    : chkexist
! PURPOSE     : Checking the existence of a file. If the file does not exist the error message is assigned. The callback of the
!               error is not assigned, so that it appears the non-existing error is detected in the calling procedure (which is
!               what the user wants to know).
! AUTHOR      : OPS-support
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
! AUTHOR      : OPS-support
! INPUTS      : iu     (integer*4). Unit number of file.
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
! AUTHOR      : OPS-support
! ADAPTATIONS : 2002 - Error handling through error object (Martien de Haan, ARIS).
! INPUTS      : iu     (integer*4). Unit number of file.
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
! AUTHOR      : OPS-support
! ADAPTATIONS : 2002 - Error handling through error object (Martien de Haan, ARIS).
! INPUTS      : iu     (integer*4). Unit number of file.
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

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: getdirectory
! PURPOSE:    See interface definition.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE get_directory(fullpath, directory, error)

!DEC$ ATTRIBUTES DLLEXPORT:: get_directory

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER     (ROUTINENAAM = 'get_directory')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: fullpath

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*(*), INTENT(OUT)                       :: directory
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: positie                    ! position counter
LOGICAL                                          :: found                      ! \ or / has been found
INTEGER*4                                        :: os                         ! current operating system
CHARACTER*1                                      :: slash                      ! directory separator (\ or /)

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida
sccsida = '%W%:%E%'// char (0)
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
! SUBROUTINE: getfilename
! PURPOSE:    See interface definition.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE get_filename(fullpath, filename, error)

!DEC$ ATTRIBUTES DLLEXPORT:: get_filename

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER     (ROUTINENAAM = 'get_filename')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: fullpath

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*(*), INTENT(OUT)                       :: filename
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record
!
! LOCAL VARIABLES
INTEGER*4                                        :: positie                    ! position counter
LOGICAL                                          :: found                      ! \ or / has been found
INTEGER*4                                        :: os                         ! current operating system
CHARACTER                                        :: slash                      ! directory separator (\ or /)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Determine directory separator slash
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
! FUNCTION: chkexist
! PURPOSE:  See interface definition.
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION chk_file_exist(fname, error)

!DEC$ ATTRIBUTES DLLEXPORT:: chk_file_exist

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER     (ROUTINENAAM = 'chk_file_exist')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: fname                      ! file naam

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! FUNCTION RESULT
LOGICAL                                          :: chk_file_exist

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida
sccsida = '%W%:%E%'// char (0)
!-------------------------------------------------------------------------------------------------------------------------------
INQUIRE (FILE = fname, EXIST = chk_file_exist)

IF (.NOT. chk_file_exist) THEN
  CALL SetError('File does not exist', error)
  CALL ErrorParam('filename', fname, error)
  CALL ErrorCall(ROUTINENAAM, error)
ENDIF

RETURN
END FUNCTION chk_file_exist

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: sysopen
! PURPOSE:    See interface definition.
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION sys_open_file(iu, filename, rw, filetype, error, LREC)

!DEC$ ATTRIBUTES DLLEXPORT:: sys_open_file

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: iu                         ! Unit number of file
CHARACTER*(*), INTENT(IN)                        :: filename                   ! File path
CHARACTER*(*), INTENT(IN)                        :: rw                         ! Whether reading or writing (see interface)
CHARACTER*(*), INTENT(IN)                        :: filetype                   ! Type of the file, written in error messages

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record
INTEGER,   INTENT(OUT), OPTIONAL                 :: LREC                       ! Lenght of a direct access record

! FUNCTION RESULT
LOGICAL                                          :: sys_open_file              ! .FALSE. when error detected

! LOCAL VARIABLES
CHARACTER                                        :: currentchar                ! Character looked at
INTEGER*4                                        :: io_status                  ! Status of opening file, indicates possible error
INTEGER*4                                        :: str_length                 ! Length of trimmed rw string
LOGICAL                                          :: isreading                  ! Whether reading of writing
LOGICAL                                          :: isbinary                   ! Whether reading/writing binary file
LOGICAL                                          :: isdirect                   ! Whether reading/writing binary file

! CONSTANTS
CHARACTER*512                                    :: tmp_ROUTINENAAM
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER    (ROUTINENAAM = 'sysopen')

!-------------------------------------------------------------------------------------------------------------------------------
!
! Check wether string is 1 or 2 length, otherwise this is definitely a programming error.
!
str_length = LEN_TRIM(rw)
IF (str_length == 0 .OR. str_length > 2) GOTO 1000
!
! Check first character: whether reading or writing.
!
currentchar = rw(1:1)
IF (currentchar == 'r' .OR. currentchar == 'R') THEN
  isreading = .TRUE.
  isdirect = .FALSE.
ELSE IF (currentchar == 'w' .OR. currentchar == 'W') THEN
  isreading = .FALSE.
  isdirect = .FALSE.
ELSE IF (currentchar == 'd' .OR. currentchar == 'D') THEN
  isreading = .TRUE.
  isdirect = .TRUE.
ELSE
  GOTO 1000
ENDIF

isbinary = str_length == 2
IF (isbinary) THEN
!
! Check whether second character is a b, otherwise tis is programming error.
!
  IF (rw(2:2) /= 'b' .AND. rw(2:2) /= 'B') GOTO 2000
!
! Read or write the binary file.
!
  IF (isreading) THEN
    CALL sysopen_read_bin(iu, filename, io_status)
  ELSE
!
!   Writing a binary file is not yet implemented, so make this a programming error for the time being.
!
!    CALL sysopen_write_bin(iu, filename, io_status)
    CALL SetError('PROGRAM ERROR: rb option not yet implemented', error)
    GOTO 2000
  ENDIF
ELSE
!
! Read or write the text file.
!
  IF (isreading) THEN
    IF (isdirect) THEN
      CALL sysopen_direct(iu, filename, LREC, io_status)
    ELSE
      CALL sysopen_read(iu, filename, io_status)
    ENDIF
  ELSE
    CALL sysopen_write(iu, filename, io_status)
  ENDIF
ENDIF

IF (io_status == 0) THEN
  sys_open_file = .TRUE.
ELSE
  IF (isreading) THEN
    CALL SetError('Could not open file for reading', error)
  ELSE
    CALL SetError('Could not open file for writing', error)
  ENDIF

  CALL ErrorParam('Io-status', io_status, error)

  IF (isbinary) THEN
    CALL ErrorParam('File type', 'binary', error)
  ELSE
    CALL ErrorParam('File type', 'text', error)
  ENDIF

   GOTO 2000
ENDIF
RETURN

1000 CALL SetError('PROGRAM ERROR: rw string incorrect', error)
CALL ErrorParam('rw string', rw, error)

2000 CALL ErrorParam('Filename', filename, error)
CALL ErrorParam('Filetype', filetype, error)
CALL ErrorCall(ROUTINENAAM, error)
sys_open_file = .FALSE.

RETURN
END FUNCTION sys_open_file

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: sysclose
! PURPOSE:    See interface definition.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE sys_close_file(iu, filename, error)

!DEC$ ATTRIBUTES DLLEXPORT:: sys_close_file

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: iu                         ! Unit number of file
CHARACTER*(*), INTENT(IN)                        :: filename                   ! File path

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: io_status                  ! Status of I/O action

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER    (ROUTINENAAM = 'sysclose')

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida
sccsida = '%W%:%E%'// char (0)
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
! AUTHOR    : OPS-support
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE sysopen_read(iu, fnam, io_status)

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: iu
CHARACTER*(*), INTENT(IN)                        :: fnam

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: io_status                  ! Status of I/O action

! LOCAL VARIABLES
INTEGER*4                                        :: flen                       ! Length of filename

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida
sccsida = '%W%:%E%'// char (0)
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
! AUTHOR    : OPS-support
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE sysopen_read_bin(iu, fnam, io_status)

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: iu
CHARACTER*(*), INTENT(IN)                        :: fnam

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: io_status                  ! Status of I/O action

! LOCAL VARIABLES
INTEGER*4                                        :: flen

!-------------------------------------------------------------------------------------------------------------------------------
flen = LEN_TRIM(fnam)

#ifndef UNIX
OPEN (iu, FILE=fnam(1:flen), ACTION='READ', SHARED, SHARE='denywr', FORM='unformatted', IOSTAT=io_status)
#else
OPEN (iu, FILE=fnam(1:flen), ACTION='READ', FORM='unformatted', IOSTAT=io_status)
#endif

RETURN
END SUBROUTINE sysopen_read_bin

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: sysopen_write
! PURPOSE   : Opening of text file for writing.
! AUTHOR    : OPS-support
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE sysopen_write(iu, fnam, io_status)

USE m_error

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: iu                         ! Channel to write to
CHARACTER*(*), INTENT(IN)                        :: fnam                       ! Filename

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: io_status                  ! Status of I/O action

! LOCAL VARIABLES
INTEGER*4                                        :: flen                       ! Length of filename

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida
sccsida = '%W%:%E%'// char (0)
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
! AUTHOR    : OPS-support
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE sysopen_direct(iu, fnam, LREC, io_status)

USE m_error

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: iu                         ! Channel to write to
CHARACTER*(*), INTENT(IN)                        :: fnam                       ! Filename
INTEGER,   INTENT(IN)                            :: LREC                       ! Lenght of a direct access record

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: io_status                  ! Status of I/O action

! LOCAL VARIABLES
INTEGER*4                                        :: flen                       ! Length of filename

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida
sccsida = '%W%:%E%'// char (0)
!-------------------------------------------------------------------------------------------------------------------------------
flen = LEN_TRIM(fnam)

#ifndef UNIX
OPEN (iu,FILE=fnam(1:flen), ACTION='READ', SHARED, SHARE='denywr', ACCESS='direct', RECL=LREC, IOSTAT=io_status)
#else
OPEN (iu,FILE=fnam(1:flen), ACTION='READ', ACCESS='direct', RECL=LREC, IOSTAT=io_status)
#endif

RETURN
END SUBROUTINE sysopen_direct

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: sysread
! PURPOSE   : Reading a string from a file
! AUTHOR    : OPS-support
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE sys_read_string(fdin, in_str, end_of_file, error)

!DEC$ ATTRIBUTES DLLEXPORT:: sys_read_string

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER     (ROUTINENAAM = 'sys_read_string')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: fdin                       ! File id van de input file

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*(*), INTENT(OUT)                       :: in_str                     ! character string to be read
LOGICAL*4, INTENT(OUT)                           :: end_of_file                ! TRUE if end of file
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: io_status                  ! Status of IO-actions

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida
sccsida = '%W%:%E%'// char (0)
!-------------------------------------------------------------------------------------------------------------------------------
end_of_file = .FALSE.
in_str      = ' '

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
