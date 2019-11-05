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
! MODULE             : m_error
! FILENAME           : %M%
! SCCS(SOURCE)       : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH - SEQUENCE  : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : Martien de Haan (ARIS)
! FIRM/INSTITUTE     : RIVM/LLO
! LANGUAGE           : FORTRAN-F90
! DESCRIPTION        : Handling of errors occurring in ops.
! IMPLEMENTS         : Functions SetError, ErrorCall, WriteError and generic function ErrorParam.
!                      Also implements simple string functions, mainly for m_error itself and also used by string module.
! INTERFACE TYPE     : TError
! INTERFACE ROUTINES : SetError
!                      ErrorParam
!                      ErrorCall
!                      WriteError
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP Fortran
! CALLED FUNCTIONS   :
! UPDATE HISTORY :
!-------------------------------------------------------------------------------------------------------------------------------
MODULE m_error

IMPLICIT NONE

!-------------------------------------------------------------------------------------------------------------------------------
! TYPE        : TErrorParam
! DESCRIPTION : Defines a parameter and its value passed in an error.
! REMARKS     : Local type, not relevant otherwise
!-------------------------------------------------------------------------------------------------------------------------------
TYPE TErrorParam
   CHARACTER*512                                 :: paramname                  ! name of parameter
   CHARACTER                                     :: paramtype                  ! type of parameter
   CHARACTER*512                                 :: stringvalue                ! string value
   INTEGER*4                                     :: intvalue                   ! integer value
   REAL*4                                        :: realvalue                  ! real value
   TYPE (TErrorParam), pointer                   :: nextparam                  ! pointer to next parameter
END TYPE TErrorParam

!-------------------------------------------------------------------------------------------------------------------------------
! TYPE        : TErrorCall
! DESCRIPTION : Defines a called subroutine in an error.
! REMARKS     : Local type, not relevant otherwise
!-------------------------------------------------------------------------------------------------------------------------------
TYPE TErrorCall
   CHARACTER*512                                 :: routinename                ! name of routine
   TYPE (TErrorCall), pointer                    :: nextcall                   ! pointer to next call (in call stack)
END TYPE TErrorCall

!-------------------------------------------------------------------------------------------------------------------------------
! TYPE        : TError
! DESCRIPTION : TError is the type used in error handling.
! FIELDS      : The only field important to the outside world is the HasError field. This should be initialised first and can be
!               enquired elsewhere. All other fields are only useful inside this module. The outside world accesses TError
!               through the four subroutines declared below.
!-------------------------------------------------------------------------------------------------------------------------------
TYPE TError
   LOGICAL                                       :: haserror                   ! error has occurred
   LOGICAL                                       :: blockparam                 ! 
   CHARACTER*512                                 :: message                    ! string with error message

   TYPE (TErrorCall), pointer                    :: callroutines               ! call stack
   TYPE (TErrorCall), pointer                    :: lastcall                   ! last called routine

   TYPE (TErrorParam), pointer                   :: firstparam                 ! first parameter in error message
   TYPE (TErrorParam), pointer                   :: lastparam                  ! last parameter in error message
END TYPE TError

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : SetError
! DESCRIPTION : This function sets an error message.
! INPUTS      : message     (character*(*)). Message that characterises the error.
!               second      optional, (character*(*)). Second string appended to first one in characterising the error message.
!                           A blank is inserted between both strings.
! OUTPUTS     : error       (type TError). The error object.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE SetError
   MODULE PROCEDURE set_error1
   MODULE PROCEDURE set_error2
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : ErrorParam
! DESCRIPTION : This function passes a parameter and its value to the error message.
! INPUTS      : paramname  (character*(*)). Name of the parameter of which the value is passed.
!               value      (type, type is generic). Value of the parameter which is written out.
!               wordonly   (optional, logical). Only possible with strings, not required. If the flag wordonly
!                          is set the first word of value is written, not the whole string.
! INPUT/OUTPUT: error      (type TError). The error object that is written out.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE ErrorParam
   MODULE PROCEDURE error_iparam                                               ! integer*4 parameter
   MODULE PROCEDURE error_lparam                                               ! logical parameter
   MODULE PROCEDURE error_rparam                                               ! real*4 parameter
   MODULE PROCEDURE error_sparam                                               ! character*(*) string parameter
   MODULE PROCEDURE error_wparam                                               ! character*(*) string parameter, but only first
                                                                               ! word is written
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : ErrorCall
! DESCRIPTION : If an error took place the name of a subroutine is added to the error object. Thus a call stack emerges that can
!               be written out later.
! INPUTS      : routine    (character*(*)). Name of the subroutine that is added onto the error stack.
! INPUT/OUTPUT: error      (type TError). The error object, to which the routine name is added.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE ErrorCall
   MODULE PROCEDURE error_call
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : WriteError
! DESCRIPTION : Single function for writing out the error details.
! INPUTS      : errorunit  (integer*4) Unit of the error file. This function does not open or close that unit, that is up to the
!                           caller.
! INPUT/OUTPUT: error      (type TError). The error object, of which the information is written out.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE WriteError
   MODULE PROCEDURE write_error
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! Simple string declarations
!-------------------------------------------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : SimpleCopy
! DESCRIPTION : Simple appending of parameter (string, integer, float) into another string.
! INPUTS      : indentpos  (integer*4) Optional. Number of blanks preceding the copying.
!               sourcestring(type, type is generic, but at the moment only a string.
! INPUT/OUTPUT: targetstring The string that is copied into.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE SimpleCopy
  MODULE PROCEDURE simple_s_copy                                               ! copies string
  MODULE PROCEDURE simple_sb_copy                                              ! copies string with preceding blanks
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : SimpleAppend
! DESCRIPTION : Simple appending of parameter (string, integer, float) onto another string. Blanks are put between both
!               parameters. If the appended object makes the targetstring too long the last character of the targetstring
!               becomes a ~.
! INPUTS      : blanks     (integer*4) Optional. Number of blanks preceding the appended string.
!               appended   (type, type is generic). String, float, integer or logical value appended.
!               significance (integer*4, only when appended is of real type) Significance in number of digits.
! INPUT/OUTPUT: targetstring The string that is appended to.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE SimpleAppend
  MODULE PROCEDURE simple_i_append                                             ! appends integer
  MODULE PROCEDURE simple_r_append                                             ! appends real
  MODULE PROCEDURE simple_l_append                                             ! appends logical value (.TRUE./.FALSE.)
  MODULE PROCEDURE simple_s_append                                             ! appends string

  MODULE PROCEDURE simple_ib_append                                            ! appends integer with preceding blanks
  MODULE PROCEDURE simple_rb_append                                            ! appends real with preceding blanks
  MODULE PROCEDURE simple_lb_append                                            ! appends logical value (.TRUE./.FALSE.)
  MODULE PROCEDURE simple_sb_append                                            ! appends string with preceding blanks
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! Private declarations
!-------------------------------------------------------------------------------------------------------------------------------

PRIVATE set_error                                                              ! used by SetError
PRIVATE make_parameter                                                         ! used by ErrorParam
PRIVATE appendblanks                                                           ! used by SimpleAppend

!-------------------------------------------------------------------------------------------------------------------------------
! Implementation
!-------------------------------------------------------------------------------------------------------------------------------
CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   set_error1
! Purpose      Initial function called when error occurs. Initialises the error record. Error message on one single line.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE set_error1(message, error)

!DEC$ ATTRIBUTES DLLEXPORT:: set_error1

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: message                    ! the error message

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! error object

!-------------------------------------------------------------------------------------------------------------------------------
IF (set_error(error)) THEN
  CALL SimpleCopy(message, error%message)
ENDIF
END SUBROUTINE set_error1

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   set_error2
! Purpose      Initial function called when error occurs. Initialises the error record. Error message on two lines.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE set_error2(message, message2, error)

!DEC$ ATTRIBUTES DLLEXPORT:: set_error2
! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: message                    ! the error message
CHARACTER*(*), INTENT(IN)                        :: message2                   ! the second part of the error message

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! error object

!-------------------------------------------------------------------------------------------------------------------------------
IF (set_error(error)) THEN
  CALL SimpleCopy(message, error%message)
  CALL SimpleAppend(1, message2, error%message)
ENDIF

RETURN
END SUBROUTINE set_error2

!-------------------------------------------------------------------------------------------------------------------------------
! Function     set_error
! Purpose      Initialisation of error message. Returns whether error messages are not yet blocked, so that error message
!              parameters can be assigned.
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION set_error(error)

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! error object

! FUNCTION RESULT
LOGICAL                                          :: set_error                  ! 

!-------------------------------------------------------------------------------------------------------------------------------
IF (error%haserror) THEN
!
! The error was already set for something else. The message that will come now is less relevant, such as could not close a file
! or something. Therefore this new message is ignored through setting the parameter blockparam.
!
  error%blockparam = .TRUE.
  set_error = .FALSE.
ELSE
!
! (Re)set error object (set haserror field of error object to TRUE and nullify pointers)
!
  error%haserror = .TRUE.
  error%blockparam = .FALSE.
  NULLIFY(error%lastcall)
  NULLIFY(error%lastparam)
  NULLIFY(error%callroutines)
  NULLIFY(error%lastcall)
  set_error = .TRUE.
ENDIF

RETURN
END FUNCTION set_error

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   error_iparam
! Purpose      Sets values for an integer parameter.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE error_iparam(paramname, value, error)

!DEC$ ATTRIBUTES DLLEXPORT:: error_iparam

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: paramname                  ! parameter name
INTEGER*4, INTENT(IN)                            :: value                      ! parameter value

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! error object

! LOCAL VARIABLES
TYPE (TErrorParam), POINTER                      :: param                      ! error parameter object
!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT.error%blockparam) THEN
  param => make_parameter(paramname, error)
  CALL SimpleAppend(value, param%stringvalue)
ENDIF

RETURN
END SUBROUTINE error_iparam
!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   error_lparam
! Purpose      Sets values for a logical parameter.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE error_lparam(paramname, value, error)

!DEC$ ATTRIBUTES DLLEXPORT:: error_lparam

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: paramname                  ! 
LOGICAL,   INTENT(IN)                            :: value                      ! 

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! 

! LOCAL VARIABLES
TYPE (TErrorParam), POINTER                      :: param                      ! 

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT.error%blockparam) THEN
  param => make_parameter(paramname, error)
  CALL SimpleAppend(value, param%stringvalue)
ENDIF

RETURN
END SUBROUTINE error_lparam

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   error_rparam
! Purpose      Sets values for a real parameter.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE error_rparam(paramname, value, error)

!DEC$ ATTRIBUTES DLLEXPORT:: error_rparam

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: paramname                  ! 
REAL*4,    INTENT(IN)                            :: value                      ! 

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! 

! LOCAL VARIABLES
TYPE (TErrorParam), POINTER                      :: param                      ! 
!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT.error%blockparam) THEN
  param => make_parameter(paramname, error)
  CALL SimpleAppend(value, 4, param%stringvalue)
ENDIF

RETURN
END SUBROUTINE error_rparam
!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   error_sparam
! Purpose      Sets values for a string parameter.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE error_sparam(paramname, value, error)

!DEC$ ATTRIBUTES DLLEXPORT:: error_sparam

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: paramname                  ! 
CHARACTER*(*), INTENT(IN)                        :: value                      ! 

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! 

!-------------------------------------------------------------------------------------------------------------------------------
CALL ErrorParam(paramname, value, .FALSE., error)

RETURN
END SUBROUTINE error_sparam

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   error_wparam
! Purpose      Sets values for a word as parameter. A word is that part of a string up to the first blank or end of string.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE error_wparam(paramname, value, wordonly, error)

!DEC$ ATTRIBUTES DLLEXPORT:: error_wparam

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: paramname                  ! 
CHARACTER*(*), INTENT(IN)                        :: value                      ! 
LOGICAL,   INTENT(IN)                            :: wordonly                   ! 

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! 

! LOCAL VARIABLES
INTEGER*4                                        :: startpos                   ! 
INTEGER*4                                        :: endpos                     ! 
INTEGER*4                                        :: length                     ! 
TYPE (TErrorParam), POINTER                      :: param                      ! 

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT.error%blockparam) THEN
  IF (.NOT. wordonly) THEN
!
!   We want the whole parameter, so implement that
!
    param => make_parameter(paramname, error)
    call SimpleAppend(value, param%stringvalue)
  ELSE
    length = LEN_TRIM(value)
    IF (length.EQ.0) THEN
      startpos = 1
      endpos = 0
    ELSE
!
!     Ignore initial blanks
!
      startpos = 1
      DO WHILE (startpos.LE.length .AND. value(startpos:startpos).EQ.' ')
        startpos = startpos + 1
      ENDDO
!
!     Go to end of the word, which is where the next blank occurs.
!
      endpos = startpos
      DO WHILE (endpos.LE.length .AND. value(startpos:startpos).NE.' ')
        endpos = endpos + 1
      ENDDO
      endpos = endpos - 1                                                      ! we were beyond last character in word
    ENDIF
!
!   Write down the word.
!
    IF (endpos.LT.startpos) THEN                                               ! no word at all
      CALL ErrorParam(paramname, value, error)
    ELSE
      CALL ErrorParam(paramname, value(startpos:endpos), error)
    ENDIF
  ENDIF
ENDIF

RETURN
END SUBROUTINE error_wparam

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   error_call
! Purpose      Routinename is added onto the call stack.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE error_call(routinename, error)

!DEC$ ATTRIBUTES DLLEXPORT:: error_call

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: routinename                ! 

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! 

! LOCAL VARIABLES
TYPE (TErrorCall), POINTER                       :: caller                     ! 

!-------------------------------------------------------------------------------------------------------------------------------
!
! A new call is a new pointer on the callroutines stack. Create that pointer (caller) first.
!
ALLOCATE(caller)
!
! Set all the pointers right. The firstparam is only assigned the first time.
!
IF (ASSOCIATED(error%lastcall)) THEN
  error%lastcall%nextcall => caller
ELSE
  error%callroutines => caller
ENDIF
error%lastcall => caller
NULLIFY(caller%nextcall)
!
! Initialise routinename.
!
caller%routinename = routinename

END SUBROUTINE error_call

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   write_error
! Purpose      Writing out of error message
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE write_error(unit, error)

!DEC$ ATTRIBUTES DLLEXPORT:: write_error

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: unit                       ! 
TYPE (TError), INTENT(IN)                        :: error                      ! 

! LOCAL VARIABLES
INTEGER*4                                        :: length                     ! 
INTEGER*4                                        :: maxlen                     ! 
LOGICAL                                          :: hascaller                  ! 

TYPE (TErrorParam), POINTER                      :: param                      ! 
TYPE (TErrorParam), POINTER                      :: nextparam                  ! 

TYPE (TErrorCall), POINTER                       :: caller                     ! 
TYPE (TErrorCall), POINTER                       :: nextcaller                 ! 

!-------------------------------------------------------------------------------------------------------------------------------
!
! Write out subroutine name where error occurred
!
caller => error%callroutines
hascaller = ASSOCIATED(caller)
IF (hascaller) THEN
  length = LEN_TRIM(caller%routinename)
  WRITE (unit,'(A, 1X, A)') 'Error in subroutine:', caller%routinename(:length)
ENDIF
!
! Write out the error message.
!
WRITE (unit,'(/, A, 1X, A)') 'Error:', error%message
WRITE (unit, '()')
!
! Determine the longest parameter. We want all the = signs neatly below each other.
!
maxlen = 0
param => error%firstparam
DO WHILE (ASSOCIATED(param))
  length = LEN_TRIM(param%paramname)
  IF (length.GT.maxlen) maxlen = length
  param => param%nextparam
ENDDO
!
! Write out parameter values. In the mean time the stack is deallocated as well.
!
WRITE(unit, '(A)') 'Relevant parameters:'

param => error%firstparam
DO WHILE (ASSOCIATED(param))
  nextparam => param%nextparam
  length = LEN_TRIM(param%stringvalue)
  WRITE(unit,'(2X, 3A)') param%paramname(:maxlen), ' = ', param%stringvalue(:length)
  DEALLOCATE(param)
  param => nextparam
ENDDO
!
! Write out the remainder of the call routine stack
!
IF (hascaller) THEN
  nextcaller => caller%nextcall
  IF (ASSOCIATED(nextcaller)) THEN
    length = LEN_TRIM(caller%routinename)
    WRITE(unit,'(/, 3A)') 'Procedure ''', caller%routinename(1:length), ''' was called by:'
  ENDIF
  DEALLOCATE(caller)

  DO WHILE (ASSOCIATED(nextcaller))
    caller => nextcaller
    nextcaller => caller%nextcall
    length = LEN_TRIM(caller%routinename)
    WRITE(unit,'(2X, A)') caller%routinename(:length)
    DEALLOCATE(caller)
  ENDDO
ENDIF

RETURN
END SUBROUTINE write_error

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   make_parameter
! Purpose      Allocates memory for parameter and sets it into the last parameter. Initialises paramname and the stringvalue of
!              the parameter.
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION make_parameter(paramname, error)

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: paramname                  ! 

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! 

! FUNCTION RESULT
TYPE (TErrorParam), pointer                      :: make_parameter             ! 
!-------------------------------------------------------------------------------------------------------------------------------
!
! Allocate memory for new parameter
!
ALLOCATE(make_parameter)
!
! Set allthe pointers right. The firstparam is only assigned the first time.
!
IF (ASSOCIATED(error%lastparam)) THEN
  error%lastparam%nextparam => make_parameter
ELSE
  error%firstparam => make_parameter
ENDIF
error%lastparam => make_parameter
NULLIFY(make_parameter%nextparam)
!
! Initialise value. Thus paramname is set and the stringvalue contains the = sign.
!
make_parameter%paramname = paramname

make_parameter%stringvalue(:) = ' '

RETURN
END FUNCTION make_parameter

!-------------------------------------------------------------------------------------------------------------------------------
! Simple string implementation
!-------------------------------------------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : simple_s_copy
! DESCRIPTION          : Simple copying of one string to another. No error can be generated!
! REMARK               : If the appended object makes the targetstring too long the last character of the targetstring becomes a
!                        ~.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE simple_s_copy(sourcestring, targetstring)

!DEC$ ATTRIBUTES DLLEXPORT:: simple_s_copy

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: sourcestring               ! string to be copied

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: targetstring               ! string where copied to

!-------------------------------------------------------------------------------------------------------------------------------

CALL SimpleCopy(0, sourcestring, targetstring)
END SUBROUTINE simple_s_copy

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : simple_sb_copy
! DESCRIPTION          : Simple copying of one string to another. Some blanks may precede the copying. No error can be
!                        generated!
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE simple_sb_copy(indentpos, sourcestring, targetstring)

!DEC$ ATTRIBUTES DLLEXPORT:: simple_sb_copy

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: indentpos                  ! indentation position
CHARACTER*(*), INTENT(IN)                        :: sourcestring               ! string to be copied

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: targetstring               ! string where copied to

! LOCAL VARIABLES
INTEGER*4                                        :: sourcelength               ! 
INTEGER*4                                        :: maxlength                  ! 

!-------------------------------------------------------------------------------------------------------------------------------
!
! Calculate how many characters can be appended maximum (maxlength)
!
sourcelength=LEN_TRIM(sourcestring)+indentpos
maxlength=LEN(targetstring)
!
! Append maximum of maxlength characters to targetstring.
!
IF (sourcelength.GT.maxlength) sourcelength=maxlength-1
targetstring(indentpos+1:sourcelength) =sourcestring(1:sourcelength-indentpos)
targetstring(maxlength:maxlength) = '~'
IF (sourcelength.LT.maxlength) THEN
  targetstring(sourcelength+1:maxlength)= ' '
ENDIF
IF (indentpos.GT.0) THEN
  targetstring(1:indentpos)= ' '
ENDIF

END SUBROUTINE simple_sb_copy

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : simple_s_append
! DESCRIPTION          : Simple appending of string behind another. No error can be generated!
! REMARK               : Called from error handling routines, because we do not want error in error.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE simple_s_append(sourcestring, targetstring)

!DEC$ ATTRIBUTES DLLEXPORT:: simple_s_append

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: sourcestring               ! string to be appended

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: targetstring               ! string waarachter wordt geplakt

!-------------------------------------------------------------------------------------------------------------------------------
CALL SimpleAppend(0, sourcestring, targetstring)

END SUBROUTINE simple_s_append

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : simple_sb_append
! DESCRIPTION          : Simple appending of string behind another. No error can be generated! Inserts input number of blanks in
!                        between.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE simple_sb_append(nrblanks, sourcestring, targetstring)

!DEC$ ATTRIBUTES DLLEXPORT:: simple_sb_append

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: nrblanks                   ! 
CHARACTER*(*), INTENT(IN)                        :: sourcestring               ! string to be appended

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: targetstring               ! string waarachter wordt geplakt

! LOCAL VARIABLES
INTEGER*4                                        :: position                   ! insertion position in targetstring
INTEGER*4                                        :: newtargetlength            ! final length of target string
INTEGER*4                                        :: maxtargetlength            ! maximum length in targetstring

!-------------------------------------------------------------------------------------------------------------------------------
!
! Append the blanks.
!
position = appendblanks(nrblanks, targetstring)
!
! Append sourcestring.
!
newtargetlength = position + LEN_TRIM(sourcestring)
maxtargetlength = LEN(targetstring)
IF (newtargetlength > maxtargetlength) THEN
  newtargetlength = maxtargetlength - 1
  targetstring(maxtargetlength:maxtargetlength) = '~'
ENDIF
targetstring(position+1:newtargetlength) = sourcestring(1:newtargetlength-position)

END SUBROUTINE simple_sb_append

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : simple_l_append
! DESCRIPTION          : Simple appending of a logical behind a string.
!                        No error can be generated! Inserts blank in between.
!                        When value is true .TRUE. is appended, otherwise .FALSE.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE simple_l_append(value, targetstring)

!DEC$ ATTRIBUTES DLLEXPORT:: simple_l_append

! SUBROUTINE ARGUMENTS - INPUT
LOGICAL,   INTENT(IN)                            :: value                      ! logical whose value is to be appended.

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: targetstring               ! string waarachter wordt geplakt

!-------------------------------------------------------------------------------------------------------------------------------
CALL SimpleAppend(0, value, targetstring)

END SUBROUTINE simple_l_append

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : simple_lb_append
! DESCRIPTION          : Simple appending of a logical behind a string.
!                        No error can be generated!
!                        When value is true .TRUE. is appended, otherwise .FALSE.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE simple_lb_append(nrblanks, value, targetstring)

!DEC$ ATTRIBUTES DLLEXPORT:: simple_lb_append

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: nrblanks                   ! 
LOGICAL,   INTENT(IN)                            :: value                      ! logical whose value is to be appended.

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: targetstring               ! string waarachter wordt geplakt

!-------------------------------------------------------------------------------------------------------------------------------
IF (value) THEN
  CALL SimpleAppend(nrblanks, '.TRUE.', targetstring)
ELSE
  CALL SimpleAppend(nrblanks, '.FALSE.', targetstring)
ENDIF

END SUBROUTINE simple_lb_append

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : simple_i_append
! DESCRIPTION          : Appending of an integer to a string.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE simple_i_append(intvalue, targetstring)

!DEC$ ATTRIBUTES DLLEXPORT:: simple_i_append

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: intvalue                   ! string to be appended

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: targetstring               ! string waarachter wordt geplakt

!-------------------------------------------------------------------------------------------------------------------------------

CALL SimpleAppend(0, intvalue, targetstring)

END SUBROUTINE simple_i_append

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : simple_ib_append
! DESCRIPTION          : Appending of an integer to a string. Blanks are set between string and integer.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE simple_ib_append(nrblanks, intvalue, targetstring)

!DEC$ ATTRIBUTES DLLEXPORT:: simple_ib_append

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: nrblanks                   ! 
INTEGER*4, INTENT(IN)                            :: intvalue                   ! string to be appended

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: targetstring               ! string waarachter wordt geplakt

! LOCAL VARIABLES
INTEGER*4                                        :: targetlength               ! current length of targetstring (LEN_TRIM)
INTEGER*4                                        :: stringlength               ! maximum length of targetstring (LEN)
INTEGER*4                                        :: position                   ! position counter in writing to string
INTEGER*4                                        :: nrdigits                   ! number of digits in intvalue
INTEGER*4                                        :: intcopy                    ! copy of integer
INTEGER*4                                        :: intcopy2                   ! copy of integer
INTEGER*4                                        :: char0                      ! '0' character

!-------------------------------------------------------------------------------------------------------------------------------
!
! Append the blanks.
!
targetlength= appendblanks(nrblanks, targetstring)

stringlength=LEN(targetstring)
char0 = ICHAR('0')
!
! First write a possible - sign.
!
IF (intvalue.LT.0 .AND. targetlength.LT.stringlength) THEN
  targetlength=targetlength+1
  targetstring(targetlength:targetlength) = '-'
  intcopy=-intvalue
ELSE
  intcopy=intvalue
ENDIF
!
! Determine number of characters in the integer.
!
intcopy2 = intcopy/10
nrdigits=1
DO WHILE (intcopy2.NE.0)
  intcopy2 = intcopy2 / 10
  nrdigits = nrdigits + 1
ENDDO
!
! Ignore last characters which can not be written.
!
position = targetlength + nrdigits
IF (position.GT.stringlength) THEN
  targetstring(stringlength:stringlength) = '~'
  DO WHILE (position.GE.stringlength)
    intcopy = intcopy/10
    position = position - 1
  ENDDO
ENDIF
!
! Write down the characters that can be written.
!
DO WHILE (position.GT.targetlength)
  intcopy2 = intcopy / 10
  targetstring(position:position) = ACHAR(intcopy-intcopy2*10+char0)
  intcopy = intcopy2
  position = position - 1
ENDDO

END SUBROUTINE simple_ib_append

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : simple_r_append
! DESCRIPTION          : Appending of a real value to a string. A blank is set between string and real. The number of
!                        significant digits is passed.
! REMARK               : Called from error handling routines, because we do not want error in error.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE simple_r_append(realvalue, significance, targetstring)

!DEC$ ATTRIBUTES DLLEXPORT:: simple_r_append

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: realvalue                  ! string to be appended
INTEGER*4, INTENT(IN)                            :: significance               ! number of significant digits

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: targetstring               ! string waarachter wordt geplakt

CALL SimpleAppend(0, realvalue, significance, targetstring)

END SUBROUTINE simple_r_append

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : simple_rb_append
! DESCRIPTION          : Appending of a real value to a string. A blank is set between string and real. The number of
!                        significant digits is passed.
! REMARK               : Called from error handling routines, because we do not want error in error.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE simple_rb_append(nrblanks, realvalue, significance, targetstring)

!DEC$ ATTRIBUTES DLLEXPORT:: simple_rb_append

USE m_commonconst                                                              ! EPS_DELTA only

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: nrblanks                   ! 
REAL*4,    INTENT(IN)                            :: realvalue                  ! string to be appended
INTEGER*4, INTENT(IN)                            :: significance               ! number of significant digits

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: targetstring               ! string waarachter wordt geplakt

! LOCAL VARIABLES
INTEGER*4                                        :: targetlength               ! current length of targetstring (LEN_TRIM)
INTEGER*4                                        :: position                   ! position counter in writing to string
INTEGER*4                                        :: power                      ! the e-value in the number
INTEGER*4                                        :: counter                    ! simple loop counter
REAL*4                                           :: realcopy                   ! copy of realvalue
INTEGER*4                                        :: intcopy                    ! copy of significant realvalue
INTEGER*4                                        :: intcopy2                   ! copy of significant realvalue
INTEGER*4                                        :: char0                      ! '0' character
LOGICAL                                          :: negative                   ! 

!-------------------------------------------------------------------------------------------------------------------------------
!
! Append the blanks.
!
targetlength= appendblanks(nrblanks, targetstring)

char0 = ICHAR('0')
IF (ABS(realvalue).LE.EPS_DELTA) THEN
!
! Write a zero.
!
  targetlength = targetlength + 1
  targetstring(targetlength:targetlength) = '0'
  targetlength = targetlength + 1
  targetstring(targetlength:targetlength) = '.'
  DO position = 2, significance
    targetlength = targetlength + 1
    targetstring(targetlength:targetlength) = '0'
  ENDDO
  targetstring(targetlength+1:targetlength+4) = 'E+00'
ELSE
!
! Retrieve the power of the real number.
!
  negative  = realvalue.LT.0
  IF (negative) THEN
    realcopy = -realvalue
  ELSE
    realcopy = realvalue
  ENDIF

  power = 0
  IF (realcopy.LT.1) THEN
    DO WHILE (realcopy.LT.0.1)
      realcopy = realcopy * 10
      power = power - 1
    ENDDO
  ELSE
    DO WHILE (realcopy.GE.1)
      realcopy = realcopy / 10
      power = power + 1
    ENDDO
  ENDIF
!
! Adjust realcopy for significance. Round off to nearest integer number.
!
  DO counter = 1, significance
    realcopy = realcopy * 10
  ENDDO
  intcopy = NINT(realcopy)
!
! Write intcopy and everything else. Not yet checked for going out of limits,
!
  targetlength = targetlength + 1
  IF (negative) THEN
    targetstring(targetlength:targetlength) = '-'
    targetlength = targetlength + 1
  ENDIF
  targetlength = targetlength + significance
  position = targetlength
  DO counter = 1, significance - 1
    intcopy2 = intcopy / 10
    targetstring(position:position) = ACHAR(intcopy-intcopy2*10+char0)
    intcopy = intcopy2
    position = position - 1
  ENDDO
  targetstring(position:position) = '.'
  IF (intcopy.EQ.10) THEN
!   Rare case; due to rounding off we ended up one digit more.
    intcopy = 1
    power = power + 1
  ENDIF
  targetstring(position-1:position-1) = ACHAR(intcopy+char0)

  targetlength = targetlength + 1
  targetstring(targetlength:targetlength) = 'E'

  targetlength = targetlength + 1
  power = power - 1
  IF (power.GE.0) THEN
    targetstring(targetlength:targetlength) = '+'
  ELSE
    targetstring(targetlength:targetlength) = '-'
    power = -power
  ENDIF
  intcopy = power / 10
  power = power - intcopy * 10
  targetstring(targetlength+1:targetlength+1) = ACHAR(intcopy+char0)
  targetstring(targetlength+2:targetlength+2) = ACHAR(power+char0)
ENDIF

END SUBROUTINE simple_rb_append

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION             : appendblanks
! DESCRIPTION          : Simple appending of blanks behind a string.
! RESULT               : New last position in the string.
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION appendblanks(nrblanks, targetstring)

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: nrblanks                   ! 

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: targetstring               ! string waarachter wordt geplakt

! FUNCTION RESULT
INTEGER*4                                        :: appendblanks               ! 

! LOCAL VARIABLES
INTEGER*4                                        :: position                   ! insertion position in targetstring
INTEGER*4                                        :: targetlength               ! new length of targetstring
INTEGER*4                                        :: maxtargetlength            ! maximim length in targetstring

!-------------------------------------------------------------------------------------------------------------------------------
!
! Calculate how many characters can be appended maximium (maxlength)
!
maxtargetlength=LEN(targetstring)
targetlength=LEN_TRIM(targetstring)
position = targetlength + nrblanks
IF (position > maxtargetlength) THEN
  position = maxtargetlength - 1
  targetstring(maxtargetlength:maxtargetlength) = '~'
  appendblanks = maxtargetlength + 1
ELSE
  appendblanks = position
ENDIF

targetstring(targetlength+1:position) = ' '

END FUNCTION appendblanks

!-------------------------------------------------------------------------------------------------------------------------------

END MODULE m_error
