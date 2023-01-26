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
! DESCRIPTION        : Checks name of parameter and extracts a value for that parameter, or sets a default.
!                    : Generic functions GetKeyValue and GetCheckedKey.
!                    : - GetKeyValue: checks and extract parameter value.
!                      - GetCheckedKey: also checks whether value is inside a range.
!-------------------------------------------------------------------------------------------------------------------------------

MODULE m_getkey

USE m_error
USE m_utils
USE m_commonconst_lib                                                          ! EPS_DELTA, MISVALNUM

IMPLICIT NONE

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION    : GetKeyValue
! DESCRIPTION : This function checks a string for the name of input parameter. Then the value of the parameter is extracted and
!               assigned to it. If no value is extracted a default is set.
! INPUTS      : parname    (character*(*). Name of the parameter; is required (logical, only when values are logical). Field does
!                           not have to be required if this flag set on .false. Value then automatically becomes .false.
! OUTPUTS     : value      (type). Value assigned to the parameter. Generic type
!               error      (TError object). Assigned when an error occurred.
! RESULT      : Logical.    False if an error was detected.
! REMARK      : GetKeyValue is generic for the following types:
!                           integer, integer followed by a string, real, logical, string
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE GetKeyValue
   MODULE PROCEDURE get_key_integer
   MODULE PROCEDURE get_key_integer_string
   MODULE PROCEDURE get_key_real
   MODULE PROCEDURE get_key_logical
   MODULE PROCEDURE get_key_string
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION    : GetCheckedKey
! DESCRIPTION : This function checks a string for the name of input parameter. Then the value of the parameter is extracted and
!               assigned to it. This function also checks whether the parameter is inside a specified range. If no value is
!               extracted a default is set.
! INPUTS      : parname    (character*(*)). Name of the parameter.
!               lower      (type, type is generic). Lower limit of value allowed.
!               upper      (type, type is generic). Upper limit of value allowed.
!               isrequired (logical) Whether a value is required. If not a default can be assigned.
! OUTPUTS     : value      (type, type is generic) value assigned to the parameter.
!               error      (TError object). Assigned when an error occurred.
! RESULT      : Logical.    False if an error was detected.
! REMARK      : GetCheckedKey is generic for the following types:
!                           real, integer, integer array, string
! REMARK2     : For integer array, isrequired is not a argument; an empty string after parnam returns nword = 0. 
! REMARK3     : A special checked key instance (check_exist_file) checks filepaths and has a different profile (isrequired is not passed):
!             : parname    (character*(*)). Name of the parameter. 
!               checkdefine(logical). If flag is set: test whether name was entered.
!               checkexists(logical) If flag is set: test whether file path is present, otherwise an error is returned.
!               value      (character*(*)) Output: the path of the file. 
!               error      (TError object). Assigned when an error occurred.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE GetCheckedKey
   MODULE PROCEDURE check_range_real
   MODULE PROCEDURE check_range_integer
   MODULE PROCEDURE check_range_integer_array
   MODULE PROCEDURE check_exist_file
   MODULE PROCEDURE check_range_string
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! Private procedures (used by GetKey only)
!-------------------------------------------------------------------------------------------------------------------------------
PRIVATE checkparname
PRIVATE cutfromstring

!-------------------------------------------------------------------------------------------------------------------------------
! Implementation
!-------------------------------------------------------------------------------------------------------------------------------
CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION             : get_key_integer
! DESCRIPTION          : Retrieves key for integer parameters.
! CALLED FUNCTIONS     : checkparname, getint
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION get_key_integer(parname, value, error)

!DEC$ ATTRIBUTES DLLEXPORT:: get_key_integer

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: parname                    ! 

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: value                      ! 
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! RESULT
LOGICAL                                          :: get_key_integer            ! 

! LOCAL VARIABLES
LOGICAL                                          :: isdefault                  ! 
CHARACTER*512                                    :: string                     ! 
!-------------------------------------------------------------------------------------------------------------------------------
!
! Check presence of parname and determine which part of the string should contain the value.
!
IF (.NOT. checkparname(parname, string, error)) GOTO 200
!
! Extract integer value from string and return this.
!
CALL GetNumber(string, value, isdefault, error)
IF (error%haserror) GOTO 100
!
! If no value was returned set at MISVALNUM.
!
IF (isdefault) THEN
  value = MISVALNUM
ENDIF
get_key_integer = .TRUE.

RETURN
!
! Error occurred. Append some parameters to error.
!
100 CALL ErrorParam('parameter', parname, error)

200 get_key_integer = .FALSE.

END function get_key_integer

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION             : get_key_integer_string
! DESCRIPTION          : Retrieves key for integer followed by a string. Note: everything after ! is considered as comment and ignored.
! CALLED FUNCTIONS     : checkparname, GetNumber/getint
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION get_key_integer_string(parname, value, strval, error)

!DEC$ ATTRIBUTES DLLEXPORT:: get_key_integer_string

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: parname                    ! parameter name

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: value                      ! output value (integer)
CHARACTER(LEN=*), INTENT(OUT)                    :: strval                     ! output value (string)
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! RESULT
LOGICAL                                          :: get_key_integer_string     ! true if succesfull

! LOCAL VARIABLES
LOGICAL                                          :: isdefault                  ! 
INTEGER                                          :: beyondpos                  ! first position beyond integer; 0 if end of line
INTEGER                                          :: i_excl                     ! index of exclamation mark
CHARACTER*512                                    :: string                     ! 
!-------------------------------------------------------------------------------------------------------------------------------
!
! Check presence of parname and determine which part of the string should contain the value.
!
IF (.NOT. checkparname(parname, string, error)) GOTO 200
!
! Extract integer value from string and return this.
!
CALL GetNumber(string, value, isdefault, error, beyondpos)
!write(*,*) 'gkis:==',trim(string),'==',value,error%haserror,beyondpos
IF (error%haserror) GOTO 100
!
! If no value was returned set at MISVALNUM.
!
IF (isdefault) THEN
  value = MISVALNUM
  strval = ''
ELSE
   ! Get string:
   if (beyondpos > 0) then
      strval = adjustl(string(beyondpos:))
    
      ! Skip comment:
      i_excl = index(strval,'!')
      if (i_excl > 0) strval = strval(1:i_excl-1)
   else
      strval = ''
   endif
   
ENDIF
get_key_integer_string = .TRUE.

RETURN
!
! Error occurred. Append some parameters to error.
!
100 CALL ErrorParam('parameter', parname, error)

200 get_key_integer_string = .FALSE.

END function get_key_integer_string

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION             : get_key_logical
! DESCRIPTION          : Retrieves key for logical parameters.
! CALLED FUNCTIONS     : get_key (logical argument)
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION get_key_logical(parname, isrequired, value, error)

!DEC$ ATTRIBUTES DLLEXPORT:: get_key_logical

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: parname                    ! 
LOGICAL,   INTENT(IN)                            :: isrequired                 ! 

! SUBROUTINE ARGUMENTS - OUTPUT
LOGICAL,   INTENT(OUT)                           :: value                      ! 
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: intvalue                   ! 

! RESULT
LOGICAL                                          :: get_key_logical            ! 

!-------------------------------------------------------------------------------------------------------------------------------
!
! Check the keyword and if okay read its INTEGER value.
!
IF (.NOT. GetKeyValue(parname, intvalue, error)) GOTO 9999
!
! Value must be 0 or 1. Check it.
!
IF (intvalue.EQ.1) THEN
  value=.TRUE.
ELSE IF (intvalue.EQ.0) THEN
  value=.FALSE.
ELSE IF (.NOT.isrequired.AND.intvalue.EQ.MISVALNUM) THEN
  value=.FALSE.
ELSE
  CALL SetError('Logical number should be 0 or 1', error)
  CALL ErrorParam('parameter', parname, .TRUE., error)
  CALL ErrorParam('value', intvalue, error)
  GOTO 9999
ENDIF

get_key_logical = .TRUE.

RETURN
!
! Error label. Return negative result.
!
9999 get_key_logical = .FALSE.

END FUNCTION get_key_logical

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION             : get_key_real
! DESCRIPTION          : Retrieves key for float parameters.
! CALLED FUNCTIONS     : checkparname, getreal
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION get_key_real(parname, value, error)

!DEC$ ATTRIBUTES DLLEXPORT:: get_key_real

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: parname                    ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: value                      ! 
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! RESULT
LOGICAL                                          :: get_key_real               ! 

! LOCAL VARIABLES
LOGICAL                                          :: nopart                     ! 
CHARACTER*512                                    :: string                     ! 
!-------------------------------------------------------------------------------------------------------------------------------
!
! Check the validity of the keyword and determine which part of the string should contain the value.
!
IF (.NOT. checkparname(parname, string, error)) GOTO 200
!
! Extract real value from string and return this.
!
CALL GetNumber(string, value, nopart, error)
IF (error%haserror) GOTO 100
!
! If no value was provided, assign a default value.
!
IF (nopart) THEN
  value = FLOAT(MISVALNUM)
ENDIF

get_key_real = .TRUE.

RETURN
!
! Error handling
!
100 CALL ErrorParam('parameter', parname, error)

200 get_key_real = .FALSE.

END FUNCTION get_key_real

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION             : get_key_string
! DESCRIPTION          : Retrieves key for string parameters. Note: everything after ! is considered as comment and ignored.
! CALLED FUNCTIONS     : checkparname, cutfromstring
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION get_key_string(parname, string, error)

!DEC$ ATTRIBUTES DLLEXPORT:: get_key_string

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: parname                    ! 

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*(*), INTENT(OUT)                       :: string                     ! parameter value
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! RESULT
LOGICAL                                          :: get_key_string             ! 

! LOCAL VARIABLES
INTEGER*4                                        :: i_excl                     ! index of exclamation mark in string
!INTEGER*4                                        :: length                     ! 
!INTEGER*4                                        :: cutpos                     ! 
!INTEGER*4                                        :: position                   ! 
!INTEGER*4                                        :: blankpos                   ! 
!LOGICAL                                          :: stillblank                 ! 
!-------------------------------------------------------------------------------------------------------------------------------
!
! Check whether a parname is located in string and determine the value of the parameter in the string.
!
get_key_string = checkparname(parname,string,error)
IF (error%haserror) GOTO 9999

! Skip comment:
i_excl = index(string,'!')
if (i_excl > 0) string = string(1:i_excl-1)

! Old method of removing comment
! !
! ! Move up until we reach commentary. Remove the commentary. This is either the first character a a blank followed by a !
! !
! length = LEN_TRIM(string)
! IF (length.GT.0) THEN
! !
! ! Determine cutpos, which is position from where remainder is commentary, if that exists. Otherwise cutpos ends up -1.
! !
!   cutpos = -1
!   DO position = length, 1, -1
!     IF (string(position:position).EQ.'                                         ! ') THEN  ! original statement (?)
!     ! IF (string(position:position).EQ.'!') THEN ! works better
!       blankpos = position - 1
!       stillblank=.TRUE.
!       DO WHILE (stillblank .AND. blankpos.GT.0)
!         IF (string(blankpos:blankpos).EQ.' ') THEN
!           blankpos = blankpos - 1
!         ELSE
!           stillblank=.FALSE.
!         ENDIF
!       ENDDO
!       blankpos = blankpos + 1
!       IF (blankpos.EQ.1 .OR.blankpos.NE.position) THEN
!         cutpos = blankpos
!       ENDIF
!     ENDIF
!   ENDDO
! 
!   IF (cutpos.NE.-1) THEN
! !
! !   Cut from cutpos onwards.
! !
!     CALL cutfromstring(cutpos, length, string)
!     write(*,*) 'string2=',string,'='
! 
!   ENDIF
! ENDIF

9999 RETURN

END FUNCTION get_key_string

!-------------------------------------------------------------------------------------------------------------------------------
! LOCAL FUNCTION       : checkparname
! DESCRIPTION          : This routine reads a line from the control file and then checks the occurrance of a parameter in this
!                        line. Commentary lines are ignored. After that a value is extracted as a string, which value should
!                        follow the parameter declaration.
!                        Any commentary following this declaration is also ignored.
! CALLED FUNCTIONS     : cutfromstring
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION checkparname(parname, string, error)

USE m_error
USE m_commonfile

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: parname                    ! parameter name, only capitals!

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*(*), INTENT(OUT)                       :: string                     ! string containing value for this parameter
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! RESULT
LOGICAL                                          :: checkparname               ! 

! CONSTANTS
INTEGER*4                                        :: capsdiff                   ! 'A' - 'a'
PARAMETER    (capsdiff    = ichar('A')-ichar('a'))
CHARACTER*14                                     :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'checkparname')

! LOCAL VARIABLES
INTEGER*4                                        :: length                     ! length of string
INTEGER*4                                        :: position                   ! counts position in string
INTEGER*4                                        :: teller                     ! counts position in parname
INTEGER*4                                        :: ierr                       ! read error number
INTEGER*4                                        :: parlength                  ! length of parname
LOGICAL*4                                        :: readnext                   ! FALSE if no comment line has been read
LOGICAL*4                                        :: nextpos                    ! test variable in loop
CHARACTER                                        :: stringchar                 ! character in string that is being looked at
CHARACTER                                        :: parchar                    ! character in parname that is being looked at

!-------------------------------------------------------------------------------------------------------------------------------
checkparname = .FALSE.                                                         ! If everything went right this is set to true
readnext     = .TRUE.

DO WHILE (readnext)
!
! Read next line into string. Check for errors during reading.
!
  READ (fu_input, '(a)', IOSTAT = ierr) string

  IF (ierr .GT. 0) GOTO 100                                                    ! Reading error
  IF (ierr .EQ. -1) GOTO 200                                                   ! End-of-file before required parameter is read
!
! Check whether this is an empty line or a commentary line, which are ignored.
!
  length = LEN_TRIM(string)
  IF (length.NE.0 .AND. string(1:1).NE.'*') THEN
    readnext=.false.
!
!   Ignore blanks.
!
    position=1
    nextpos=.TRUE.
    DO WHILE (nextpos)
      IF (string(position:position).NE.' ') THEN
        nextpos=.FALSE.
      ELSE
        IF (position.EQ.length) THEN
          nextpos = .FALSE.
          readnext = .TRUE.                                                    ! Still an empty line
        ENDIF
        position=position+1
      ENDIF
    ENDDO
  ENDIF
ENDDO
!
! Check now whether the string contains parname. The test is capital insensitive.
!
parlength=LEN_TRIM(parname)
DO teller=1,parlength
  IF (position.gt.length) GOTO 300
!
! Check whether characters in parname and string match. If not, we have an error. Remark: all characters in parname should be
! capitals.
! String is mixed case.
!
  parchar=parname(teller:teller)
  stringchar=string(position:position)
  IF (parchar.NE.stringchar) THEN
    IF ('A'.gt.parchar.or.parchar.gt.'Z') GOTO 300
    IF (ICHAR(parchar)-capsdiff.NE.ICHAR(stringchar)) GOTO 300
  ENDIF

  position=position+1
ENDDO
!
! Next position should be a blank.
!
IF (position.LE.length) THEN
  IF (string(position:position).ne.' ') GOTO 300
ENDIF
!
! Determine position beyond the blanks. This is output string.
!
nextpos=.true.
DO WHILE (nextpos)
  IF (position.gt.length) THEN
    nextpos=.false.
  ELSEIF (string(position:position).ne.' ') THEN
    nextpos=.false.
  ELSE
    position=position+1
  ENDIF
ENDDO
!
! Everything went fine.
!
CALL cutfromstring(1, position-1, string)

checkparname = .TRUE.
RETURN
!
! Label 100: Read error
!
100 CALL SetError('Control file read error', error)
CALL ErrorParam('parameter', parname, error)
CALL ErrorParam('line', string, error)
CALL ErrorParam('position', position, error)
GOTO 9999
!
! Label 200: Missing data
!
200 CALL SetError('Unexpected end-of-file', error)
CALL ErrorParam('missing parameter', parname, error)
GOTO 9999
!
! Label 300: Undeclared parameter.
!
300 CALL SetError('Undeclared parameter', error)
CALL ErrorParam('parameter', parname, error)
CALL ErrorParam('line', string, error)

9999 RETURN

END FUNCTION checkparname

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : cutfromstring
! DESCRIPTION          : Simple cutting of part of a string.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE cutfromstring(startpos, endpos, string)

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: startpos                   ! position from where cutting takes place
INTEGER*4, INTENT(IN)                            :: endpos                     ! position up to where cutting takes place

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: string                     ! string that is being cut

! LOCAL VARIABLES
INTEGER*4                                        :: lengte                     ! string length
INTEGER*4                                        :: newendpos                  ! new string length

! CONSTANTS
CHARACTER*14                                     :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'cutfromstring')
!
lengte = LEN(string)
newendpos = lengte - endpos + startpos - 1
string(startpos:newendpos) = string(endpos+1:lengte)
string(newendpos+1:lengte) = ' '
END SUBROUTINE cutfromstring

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : check_range_real
! DESCRIPTION          : This function checks a string for the name of the parameter. Then the real value of the parameter is
!                        extracted and assigned to the parameter.
!                        If no value is extracted a default MISVALNUM is returned. 
!                        If isrequired = true, it is checked whether the value lies within input limits.
! RESULT               : False if a range error was detected; other errors are set in error%haserror. 
! CALLED FUNCTIONS     : get_key
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION check_range_real(parname,lower,upper,isrequired, value, error)

!DEC$ ATTRIBUTES DLLEXPORT:: check_range_real

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: parname                    ! 
REAL*4,    INTENT(IN)                            :: lower                      ! lower limit of value
REAL*4,    INTENT(IN)                            :: upper                      ! upper limit of value
LOGICAL,   INTENT(IN)                            :: isrequired                 ! whether a value is required

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: value                      ! real value extracted
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! RESULT
LOGICAL                                          :: check_range_real           ! 

!-------------------------------------------------------------------------------------------------------------------------------
!
! Retrieve the real value for parname.
!
check_range_real = .TRUE.
IF (GetKeyValue(parname, value, error)) THEN
!
! Check whether a value is required. If not the default MISVALNUM is accepted.
!
  IF (isrequired.OR.ABS(value-float(MISVALNUM)).GT.EPS_DELTA) THEN
!
!   Check lower limit.
!
    IF (value.LT.lower) THEN
      CALL SetError('Value read is below allowed lower limit', error)
      GOTO 1000
    ENDIF
!
!   Check upper limit.
!
    IF (value.GT.upper) THEN
      CALL SetError('Value read is above allowed upper limit', error)
      GOTO 1000
    ENDIF
  ENDIF
ENDIF

RETURN
!
! Range error occurred. Append some parameters to error.
!
1000 CALL ErrorParam('parameter', parname, error)
CALL ErrorParam('value read', value, error)
CALL ErrorParam('lower limit', lower, error)
CALL ErrorParam('upper limit', upper, error)
check_range_real = .FALSE.

END FUNCTION check_range_real

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION             : check_range_integer
! DESCRIPTION          : This function checks a string for the name of the parameter. Then the integer value of the parameter is
!                        extracted and assigned to the parameter. If no value is extracted a default is set. If a value is
!                        extracted it is checked whether the value lies within input limits.
! RESULT               : False if a range error was detected; other errors are set in error%haserror. 
! CALLED FUNCTIONS     : GetKeyValue
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION check_range_integer(parname, lower, upper, isrequired, value, error)

!DEC$ ATTRIBUTES DLLEXPORT:: check_range_integer

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: parname                    ! 
INTEGER*4, INTENT(IN)                            :: lower                      ! lower limit of value
INTEGER*4, INTENT(IN)                            :: upper                      ! upper limit of value
LOGICAL,   INTENT(IN)                            :: isrequired                 ! whether a value is required

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: value                      ! integer value extracted
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! RESULT
LOGICAL                                          :: check_range_integer        ! 

!-------------------------------------------------------------------------------------------------------------------------------
!
! Retrieve the integer value for parname.
!
check_range_integer = .TRUE.
IF (GetKeyValue(parname, value, error)) THEN
!
! Check whether a value is required. If not the default MISVALNUM is accepted.
!
  IF (isrequired.OR.value.NE.MISVALNUM) THEN
!
!   Check lower limit.
!
    IF (value.LT.lower) THEN
      CALL SetError('Value read is below allowed lower limit', error)
      GOTO 1000
    ENDIF
!
!   Check upper limit.
!
    IF (value.GT.upper) THEN
      CALL SetError('Value read is above allowed upper limit', error)
      GOTO 1000
    ENDIF
  ENDIF
ENDIF

RETURN
!
! Range error occurred. Append some parameters to error.
!
1000 CALL ErrorParam( 'parameter', parname, error)
CALL ErrorParam( 'value read', value, error)
CALL ErrorParam( 'lower limit', lower, error)
CALL ErrorParam( 'upper limit', upper, error)
check_range_integer = .FALSE.

END FUNCTION check_range_integer

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION             : check_range_integer_array
! DESCRIPTION          : This function checks a string for the name of the parameter. 
!                        Next an integer array value of the parameter is extracted and assigned to the parameter. 
!                        If there is an empty string (no value) after the parameter name, then nword = 0 is returned i
!                        and value(*) is unchanged.
!                        There is no logical "isrequired" in this case, because an empty string is allowed (nword = 0).
! RESULT               : False if a range error was detected; other errors are set in error%haserror. 
! CALLED FUNCTIONS     : GetKeyValue
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION check_range_integer_array(parname, lower, upper, nword, value, error)

!DEC$ ATTRIBUTES DLLEXPORT:: check_range_integer_array

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: parname                    ! name of parameter looking for
INTEGER*4, INTENT(IN)                            :: lower                      ! lower limit of value
INTEGER*4, INTENT(IN)                            :: upper                      ! upper limit of value

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER,   INTENT(OUT)                           :: nword                      ! number of words read from inputline
INTEGER*4, INTENT(OUT)                           :: value(*)                   ! array with integer values extracted
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: beginpos                   ! begin-position of a single word
INTEGER                                          :: endpos                     ! end-position of a single word
INTEGER                                          :: endlpos                    ! end-position of a line
INTEGER                                          :: i                          ! loop-teller
LOGICAL                                          :: is_digit                   ! character read is a digit
LOGICAL                                          :: end_of_line                ! end of line has been reached
LOGICAL                                          :: found_new_word             ! a new word has been found
LOGICAL                                          :: isdefault                  ! output of GetNumber indicating that an empty string was input
INTEGER*4                                        :: value1                     ! one integer value extracted from input line
CHARACTER*512                                    :: string                     ! Help-string

! RESULT
LOGICAL                                          :: check_range_integer_array  ! 
!-------------------------------------------------------------------------------------------------------------------------------

! Retrieve the integer array value for parname:
check_range_integer_array = .TRUE.
IF (checkparname(parname,string , error)) THEN
   
   ! Initialisation:
   string  = adjustl(string)
   endlpos = len_trim(string)

   ! Fill array with the right numbers:
   IF (endlpos > 0) THEN
      nword    = 0
      beginpos = 1
      endpos   = 0
      is_digit = .FALSE.
      end_of_line = .FALSE.
      
      ! Loop over characters:
      i = 0
      DO WHILE (.not. end_of_line)
         
         ! Check for end of line (end position or comment delimiter):
         i = i + 1
         end_of_line = ((i == endlpos+1) .or. (string(i:i) == "!"))

         IF (end_of_line) THEN        
            ! Define end position of last word:
            endpos = i-1
         ELSE
            
            ! Define begin position of word if first numerical digit has been found (previous character was not a digit):
            IF ((ichar(string(i:i)) >= ichar("0") .and. ichar(string(i:i)) <= ichar("9")) .and. .not. is_digit) THEN
               beginpos       = i
               found_new_word = .TRUE. 
               is_digit       = .TRUE.
            ELSE
               ! Define end position of word, if first space has been found after word (previous character was a digit):
               IF ((ichar(string(i:i)) == ichar(" ")) .and. is_digit) THEN
                  is_digit = .FALSE.
                  endpos   = i-1
               ELSE 
                  CONTINUE ! character is next space or next digit or non-digit
               ENDIF
            ENDIF
         ENDIF
         
         ! If a non-empty word has been found:
         IF (found_new_word .and. (endpos >= beginpos)) THEN
         
            ! Extract value:
            CALL GetNumber(string(beginpos:endpos), value1, isdefault, error) ! note: string is non-empty, so isdefault = .false.
            IF (error%haserror) GOTO 1001

            ! Check lower limit:
            IF (value1.LT.lower) THEN
              CALL SetError('Value read is below allowed lower limit', error)
              GOTO 1000
            ENDIF
            
            ! Check upper limit:
            IF (value1.GT.upper) THEN
              CALL SetError('Value read is above allowed upper limit', error)
              GOTO 1000
            ENDIF
            
            ! Check succesfull, store value in array:
            nword          = nword + 1
            value(nword)   = value1
            found_new_word = .false. 
            ! write(*,*) '1 ',nword,value(nword),trim(string),'--',string(beginpos:endlpos),'--'
         ENDIF ! check if word has been found
      ENDDO ! Loop over characters
   ELSE
      ! endlpos = 0, empty string after parname:
      nword = 0
   ENDIF
ENDIF

RETURN
!
! Range error occurred. Append some parameters to error.
!
1000 CALL ErrorParam( 'parameter', parname, error)
CALL ErrorParam( 'value read', value1, error)
CALL ErrorParam( 'index of value', nword, error)
CALL ErrorParam( 'lower limit', lower, error)
CALL ErrorParam( 'upper limit', upper, error)
check_range_integer_array = .FALSE.

RETURN

! Error in reading integer
1001 CALL ErrorParam( 'parameter', parname, error)
CALL ErrorParam( 'index of number', nword+1, error)
check_range_integer_array = .FALSE.

END FUNCTION check_range_integer_array

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION             : check_exist_file
! DESCRIPTION          : This function extracts a filename from a string.
!                        Some flags decide what is tested for. The advantage of this combination of flags is that parameter
!                        names are more easily included in error messages.
!               parname    (character*(*)). Name of the parameter. 
!               checkdefine(logical). If flag is set: test whether name was entered.
!               checkexists(logical) If flag is set: test whether file path is present, otherwise an error is returned.
!               value      (character*(*)) Output: the path of the file. 
!               error      (TError object). Assigned when an error occurred.
! RESULT               : False if an error was detected.
! CALLED FUNCTIONS     : keystring, chkexist
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION check_exist_file(parname, checkdefine, checkexist, filename, error)

!DEC$ ATTRIBUTES DLLEXPORT:: check_exist_file

USE m_fileutils

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: parname                    ! parameter (key) name
LOGICAL,   INTENT(IN)                            :: checkdefine                ! if checkexist is set, filename must be defined
LOGICAL,   INTENT(IN)                            :: checkexist                 ! check whether filename exists

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*(*), INTENT(OUT)                       :: filename                   ! output file name
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! RESULT
LOGICAL                                          :: check_exist_file           ! 

!-------------------------------------------------------------------------------------------------------------------------------
!
! Retrieve filename for parname.
!
IF (.NOT.GetKeyValue(parname,filename,error)) GOTO 999

IF (checkexist) THEN
!
! Check whether a filename was defined
!
  IF (LEN_TRIM(filename).EQ.0) THEN
    IF (checkdefine) THEN
!
!     Filename should be defined. Write error.
!
      CALL SetError('Filename is required for this parameter', error)
      CALL ErrorParam('parameter', parname, error)
      GOTO 999
    ENDIF
  ELSE
!
!   Check existence of filename.
!
    IF (.NOT.chkexist(filename,error)) THEN
!
!     Filename does not exist. Include parameter name in the error message.
!
      CALL ErrorParam('parameter', parname, error)
      GOTO 999
    ENDIF
  ENDIF
ENDIF
!
! Everything went OK, so return a positive result.
!
check_exist_file = .TRUE.
RETURN

999 check_exist_file = .FALSE.
RETURN

END FUNCTION check_exist_file

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : check_range_string
! DESCRIPTION          : This function checks a string for the name of the parameter. Then the string value of the parameter is
!                        extracted and assigned to the parameter.
!                        If no value is extracted a default is set (empty string). If a value is extracted it is checked whether the value lies
!                        within input limits (for strings, the lower and upper limits are normally the same, which means that the input string 
!                        must be equal to the limit values. 
! RESULT               : False if a range error was detected; other errors are set in error%haserror. 
! CALLED FUNCTIONS     : get_key
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION check_range_string(parname,lower,upper,isrequired, value, error)

!DEC$ ATTRIBUTES DLLEXPORT:: check_range_real

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: parname                    ! parameter name
CHARACTER*(*), INTENT(IN)                        :: lower                      ! lower limit of value
CHARACTER*(*), INTENT(IN)                        :: upper                      ! upper limit of value
LOGICAL,       INTENT(IN)                        :: isrequired                 ! whether a value is required

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*(*), INTENT(OUT)                       :: value                      ! string value extracted
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! RESULT
LOGICAL                                          :: check_range_string         ! 

!-------------------------------------------------------------------------------------------------------------------------------
!
! Retrieve the string value for parname.
!
check_range_string = .TRUE.
IF (GetKeyValue(parname, value, error)) THEN
!
! Check whether a value is required
!
  IF (isrequired ) THEN
!
!   Check lower limit.
!
    IF (value.LT.lower) THEN
      CALL SetError('Value read is below allowed lower limit', error)
      GOTO 1000
    ENDIF
!
!   Check upper limit.
!
    IF (value.GT.upper) THEN
      CALL SetError('Value read is above allowed upper limit', error)
      GOTO 1000
    ENDIF
  ENDIF
ENDIF

RETURN
!
! Range error occurred. Append some parameters to error.
!
1000 CALL ErrorParam('parameter', parname, error)
CALL ErrorParam('value read', value, error)
CALL ErrorParam('lower limit', lower, error)
CALL ErrorParam('upper limit', upper, error)
check_range_string = .FALSE.

END FUNCTION check_range_string

END MODULE m_getkey
