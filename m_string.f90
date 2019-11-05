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
! MODULE             : string
! IMPLEMENTS         : Collection of useful string routines.
! INTERFACE ROUTINES : StringCopy
!                      StringAppend
!                      StringInsert
!                      StringMerge
! NAME               : %M%
! SCCS(SOURCE)       : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH -SEQUENCE   : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : Martien de Haan (ARIS)
! FIRM/INSTITUTE     : RIVM/LLO
! LANGUAGE           : FORTRAN-77/90
! EXIT CODES         :
! FILES AND OTHER    :
!   I/O DEVICES
! SYSTEM DEPENDENCIES: HP Fortran
! CALLED FUNCTIONS   :
! UPDATE HISTORY
!-------------------------------------------------------------------------------------------------------------------------------
MODULE m_string

USE m_error

IMPLICIT NONE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : StringCopy
! DESCRIPTION : Copies (part of) a string into another.
! INPUTS      : sourcestring (character*(*)). The string copied into targetstring.
!               startpos (integer*4). Optional parameter. Position from where sourcestring is copied into targetstring. Is
!                           either passed with endpos or not passed, in which case the whole string is copied.
!               endpos (integer*4). Also optional parameter. Position up to where sourcestring is copied into targetstring. If
!                           endpos == -1 the copy is until the end of the string.
! OUTPUT      : targetstring (character*(*)). The result of the copying. error (type TError). Error is when the copying fails.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE StringCopy
  MODULE PROCEDURE copystrpart                                                 ! copies part of a string
  MODULE PROCEDURE copystring                                                  ! copies a whole string
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : StringAppend
! DESCRIPTION : Appends (part of) a string behind another.
! INPUTS      : source  (generic, character*(*) or other type). The string, integer, float or whatever is appended to
!                           targetstring.
!               startpos (integer*4). Optional parameter. Position from where source is appended to targetstring.
!                           Is either passed with endpos or not passed, in which case the whole string is appended.
!               endpos (integer*4). Also optional parameter. Position up to where source is appended to targetstring. If endpos
!                           == -1 the string appended is until the end of source.
! INPUT/OUTPUT: targetstring (character*(*)). The string behind which sourcestring is appended.
!               error (type TError). Error is when the appending fails.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE StringAppend
  MODULE PROCEDURE appendstrpart                                               ! appends part of a string
  MODULE PROCEDURE appendstring                                                ! appends a whole string
  MODULE PROCEDURE appendinteger                                               ! appends an integer into string
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : StringInsert
! DESCRIPTION : Inserts (part of) a string into another.
! INPUTS      : insetPos (integer) Position where the source is inserted into targetstring.
!               source   (generic, character*(*) or other type). The string, integer, float or whatever is inserted into
!                           targetstring.
!               startpos (integer*4). Optional parameter. Position from where sourcestring is inserted into targetstring.
!                           Is either passed with endpos or not passed, in which case the whole string is inserted.
!               endpos (integer*4). Also optional parameter. Position up to where sourcestring is appended to targetstring. If
!                           endpos == -1 the string inserted is until the end of source.
! INPUT/OUTPUT: targetstring (character*(*)). The result of the insertion. error (type TError). Error is when the insertion
!               fails.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE StringInsert
  MODULE PROCEDURE insertstrpart                                               ! inserts part of a string
  MODULE PROCEDURE insertstring                                                ! inserts a whole string
  MODULE PROCEDURE insertinteger                                               ! inserts an integer into source
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : StringMerge
! DESCRIPTION : Merges two sources into one target string.
! INPUTS      : source1  (character*(*)). The string, that is merged into targetstring. This component will come first in the
!                           targetstring.
!               startpos1 (integer*4). Optional parameter. Position from where source1 is copied into targetstring.
!                           Is either passed with endpos1, or not passed, in which case the whole string is copied.
!               endpos1 (integer*4). Also optional parameter. Position up to where source1 is merged into targetstring.
!                           If endpos == -1 the is string copied is until the end of source1.
!               source2  (character*(*)). The string that will come behind source1 in the targetstring.
!               startpos2 (integer*4). Optional parameter. Position from where source2 is copied into targetstring.
!               endpos2 (integer*4). Also optional parameter. Position up to where source2 is merged into targetstring.
!                           If endpos == -1 the is string copied is until the end of source2.
! INPUT/OUTPUT: targetstring (character*(*)). The string behind which sourcestring is appended.
!               error (type TError). Error is when the merging fails.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE StringMerge
  MODULE PROCEDURE mergestrpart                                                ! merges parts of both sources
  MODULE PROCEDURE mergestring                                                 ! merges the whole source
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! Private functions
!-------------------------------------------------------------------------------------------------------------------------------
PRIVATE    stringsetlimits
PRIVATE    stringtestlimits

!-------------------------------------------------------------------------------------------------------------------------------
! Implementation
!-------------------------------------------------------------------------------------------------------------------------------

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : copystrpart
! PURPOSE            : Copying of (part of) string into other string. The limit sizes of both strings are checked.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE copystrpart(sourcestring, startpos, endpos, targetstring, error)

!DEC$ ATTRIBUTES DLLEXPORT:: copystrpart

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: sourcestring               ! string to be copied
INTEGER*4, INTENT(IN)                            :: startpos                   ! position from where copying takes place
INTEGER*4, INTENT(IN)                            :: endpos                     ! position up to where copying takes place.

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*(*), INTENT(OUT)                       :: targetstring               ! result string
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: eindsourcepos              ! 
INTEGER*4                                        :: eindtargetpos              ! 
LOGICAL                                          :: changeany                  ! false if nothing has been set

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'copystrpart')

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'// char (0)
!-------------------------------------------------------------------------------------------------------------------------------
IF (stringsetlimits(ROUTINENAAM, sourcestring, startpos, endpos, 0, targetstring, eindsourcepos, eindtargetpos, changeany,     &
                   & error)) THEN
!
! De toekenning, mits er iets toe te kennen valt.
!
  IF (changeany) THEN
    targetstring(1:eindtargetpos) =sourcestring(startpos:eindsourcepos)
  ENDIF
ENDIF

RETURN
END SUBROUTINE copystrpart

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : copystring
! DESCRIPTION          : Copy whole string to another, taking care of both string lengths.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE copystring(sourcestring, targetstring, error)

!DEC$ ATTRIBUTES DLLEXPORT:: copystring

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: sourcestring               ! string to be copied

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*(*), INTENT(OUT)                       :: targetstring               ! result string
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'copystring')
!-------------------------------------------------------------------------------------------------------------------------------
CALL copystrpart(sourcestring, 1, -1, targetstring, error)

IF (error%haserror) THEN
  CALL ErrorCall(ROUTINENAAM, error)
ENDIF

END SUBROUTINE copystring

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : appendstrpart
! DESCRIPTION          : Paste (part of) string after another string, taking care of both string lengths.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE appendstrpart(sourcestring, startpos, endpos, targetstring, error)

!DEC$ ATTRIBUTES DLLEXPORT:: appendstrpart

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: sourcestring               ! te copieren string
INTEGER*4, INTENT(IN)                            :: startpos                   ! positie vanaf waar moet worden gecopieerd
INTEGER*4, INTENT(IN)                            :: endpos                     ! positie tot waar moet worden gecopieerd.

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: targetstring               ! string waarachter wordt geplakt
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: targetlengte               ! 
INTEGER*4                                        :: eindsourcepos              ! 
INTEGER*4                                        :: eindtargetpos              ! 
LOGICAL                                          :: changeany                  ! false als er niets gezet wordt

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'appendstrpart')
!-------------------------------------------------------------------------------------------------------------------------------
targetlengte=LEN_TRIM(targetstring)
IF (stringsetlimits(ROUTINENAAM, sourcestring, startpos, endpos, targetlengte, targetstring, eindsourcepos, eindtargetpos,     &
                   &  changeany, error)) THEN
!
! De toekenning, mits er iets toe te kennen valt.
!
  IF (changeany) THEN
    targetstring(targetlengte+1:eindtargetpos) =sourcestring(startpos:eindsourcepos)
  ENDIF
ENDIF

END SUBROUTINE appendstrpart

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : appendstring
! DESCRIPTION          : Paste string after another string, taking care of both string lengths.
! REMARK               : targetstring is declared first because this is more logical when calling this subroutine
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE appendstring(targetstring, sourcestring, error)

!DEC$ ATTRIBUTES DLLEXPORT:: appendstring

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: sourcestring               ! te copieren string

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: targetstring               ! string waarachter wordt geplakt
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'appendstring')

CALL appendstrpart(sourcestring, 1, -1, targetstring, error)

IF (error%haserror) THEN
  CALL ErrorCall(ROUTINENAAM, error)
ENDIF

END SUBROUTINE appendstring

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : appendinteger
! DESCRIPTION          : Append integer to a string, taking care of string length.
! REMARK               : targetstring is declared first because this is more logical when calling this subroutine
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE appendinteger(targetstring, value, error)

!DEC$ ATTRIBUTES DLLEXPORT:: appendinteger

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: value                      ! te copieren integer waarde

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: targetstring               ! string waarachter wordt geplakt
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: strlen                     ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'appendinteger')

!-------------------------------------------------------------------------------------------------------------------------------
!
! Perform a simple copy.
!
CALL SimpleAppend(value, targetstring)
!
! When the copying failed the last character will be a ~.
!
strlen = LEN(targetstring)
IF (targetstring(strlen:strlen) == '~') THEN
  CALL SetError('Could not append integer value to string', error)
  CALL ErrorParam('Integer value', value, error)
  CALL ErrorParam('String', targetstring, error)
  CALL ErrorCall(ROUTINENAAM, error)
ENDIF

END SUBROUTINE appendinteger

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : insertstrpart
! DESCRIPTION          : Insert (part of) string into another string, taking care of both string lengths.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE insertstrpart(insertpos, sourcestring,startpos,endpos, targetstring, error)

!DEC$ ATTRIBUTES DLLEXPORT:: insertstrpart

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: insertpos                  ! positie waar moet worden ingevoegd
CHARACTER*(*), INTENT(IN)                        :: sourcestring               ! in te voegen string
INTEGER*4, INTENT(IN)                            :: startpos                   ! positie vanaf waar moet worden ingevoegd
INTEGER*4, INTENT(IN)                            :: endpos                     ! positie tot waar moet worden ingevoegd.

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: targetstring               ! string waarbinnen wordt ingevoegd
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: targetlengte               ! 
INTEGER*4                                        :: eindsourcepos              ! 
INTEGER*4                                        :: eindtargetpos              ! 
INTEGER*4                                        :: insertdiff                 ! 
LOGICAL                                          :: changeany                  ! false als er niets gezet wordt

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'insertstrpart')
!-------------------------------------------------------------------------------------------------------------------------------
targetlengte=LEN_TRIM(targetstring)
IF (stringsetlimits(ROUTINENAAM, sourcestring, startpos, endpos, targetlengte, targetstring, eindsourcepos, eindtargetpos,     &
                   &  changeany, error)) THEN

  IF (changeany) THEN
!
!   Verschuif gedeelte targetstring vanaf insertpos.
!
    insertdiff = endpos - startpos + 1
    targetstring(insertpos + insertdiff + 1:eindtargetpos) = sourcestring(insertpos:eindsourcepos)
!
!   De toevoeging, mits er iets toe te voegen valt.
!
    targetstring(insertpos:insertpos + insertdiff) = sourcestring(startpos:endpos)
  ENDIF
ENDIF

END SUBROUTINE insertstrpart

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : insertstring
! DESCRIPTION          : Insert string into another string, taking care of both string lengths.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE insertstring(sourcestring, insertpos, targetstring, error)

!DEC$ ATTRIBUTES DLLEXPORT:: insertstring

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: sourcestring               ! string to be copied
INTEGER*4, INTENT(IN)                            :: insertpos                  ! positie waar moet worden ingevoegd

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*(*), INTENT(OUT)                       :: targetstring               ! result string
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'insertstring')
!-------------------------------------------------------------------------------------------------------------------------------
CALL insertstrpart(insertpos, sourcestring, 1, -1, targetstring, error)

IF (error%haserror) THEN
  CALL ErrorCall(ROUTINENAAM, error)
ENDIF

END SUBROUTINE insertstring

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : insertinteger
! DESCRIPTION          : Insert integer into another string, taking care of string length.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE insertinteger(value, insertpos, targetstring, error)

!DEC$ ATTRIBUTES DLLEXPORT:: insertinteger

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: value                      ! te copieren integer waarde
INTEGER*4, INTENT(IN)                            :: insertpos                  ! invoeg positie

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: targetstring               ! string waarachter wordt geplakt

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! LOCAL VARIABLES
CHARACTER*(80)                                   :: intasstring                ! integer geconverteerd naar string

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'insertinteger')
!-------------------------------------------------------------------------------------------------------------------------------
!
! Clean the intasstring
!
intasstring(1:LEN(intasstring)) = ' '
!
! Perform a simple append of integer value to intasstring.
!
CALL appendinteger(intasstring, value, error)
IF (error%haserror) GOTO 1000
!
! Insert intasstring into targetstring.
!
CALL insertstring(intasstring, insertpos, targetstring, error)
IF (error%haserror) GOTO 1000
RETURN

1000 CALL ErrorCall(ROUTINENAAM, error)
END SUBROUTINE insertinteger

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : mergestrpart
! DESCRIPTION          : Paste (part of) string after (part of) another string, taking care of both string lengths.
!                        The result is in a new string.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE mergestrpart(sourcestring1,startpos1,endpos1, sourcestring2, startpos2, endpos2, targetstring, error)

!DEC$ ATTRIBUTES DLLEXPORT:: mergestrpart

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: sourcestring1              ! eerste te copieren string
INTEGER*4, INTENT(IN)                            :: startpos1                  ! positie vanaf waar moet worden gecopieerd
INTEGER*4, INTENT(IN)                            :: endpos1                    ! positie tot waar moet worden gecopieerd
CHARACTER*(*), INTENT(IN)                        :: sourcestring2              ! te copieren string
INTEGER*4, INTENT(IN)                            :: startpos2                  ! positie vanaf waar moet worden gecopieerd
INTEGER*4, INTENT(IN)                            :: endpos2                    ! positie tot waar moet worden gecopieerd

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: targetstring               ! string waarachter wordt geplakt
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'mergestrpart')
!-------------------------------------------------------------------------------------------------------------------------------
CALL copystrpart(sourcestring1,startpos1,endpos1, targetstring, error)

IF (.NOT.error%haserror) THEN
   CALL appendstrpart(sourcestring2, startpos2, endpos2, targetstring, error)
ENDIF

IF (error%haserror) THEN
  CALL ErrorCall(ROUTINENAAM, error)
ENDIF

END SUBROUTINE mergestrpart

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : mergestring
! DESCRIPTION          : Paste string after another string, taking care of both string lengths.
!                        The result is in a new string.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE mergestring(sourcestring1, sourcestring2, targetstring, error)

!DEC$ ATTRIBUTES DLLEXPORT:: mergestring

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: sourcestring1              ! eerste te copieren string
CHARACTER*(*), INTENT(IN)                        :: sourcestring2              ! te copieren string

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: targetstring               ! string waarachter wordt geplakt
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'mergestring')
!-------------------------------------------------------------------------------------------------------------------------------
CALL mergestrpart(sourcestring1, 1, -1, sourcestring2, 1, -1, targetstring, error)

IF (error%haserror) THEN
  CALL ErrorCall(ROUTINENAAM, error)
ENDIF

END SUBROUTINE mergestring

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : stringtestlimits
! DESCRIPTION          : Test string limits. When an error occurs a message is written and a negative result is
!                        returned.
! CALLING FUNCTIONS    : copystrpart, appendstrpart
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION stringtestlimits(routinename, string, startpos, endpos, finalpos, changeany, error)

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: routinename                ! naam of subroutine where error occurred
CHARACTER*(*), INTENT(IN)                        :: string                     ! string to be tested
INTEGER*4, INTENT(IN)                            :: startpos                   ! positie vanaf waar iets gaat gebeuren
INTEGER*4, INTENT(IN)                            :: endpos                     ! positie tot iets gaat gebeuren. Als -1 wordt

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: finalpos                   ! positie tot waar iets gaat gebeuren
LOGICAL,   INTENT(OUT)                           :: changeany                  ! false als startpos een hoger dan finalpos
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! RESULTAAT
LOGICAL                                          :: stringtestlimits           ! 

! LOCAL VARIABLES
INTEGER*4                                        :: lengte                     ! lengte van de string

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'stringtestlimits')
!-------------------------------------------------------------------------------------------------------------------------------
!
! Bepaal finalpos.
!
lengte=LEN_TRIM(string)
IF (endpos.eq.-1) THEN
  finalpos=lengte
ELSE
  finalpos=endpos
ENDIF
!
! Test de grenzen
!
stringtestlimits = .false.
IF (startpos.lt.1) THEN
  CALL SetError('Startposition less than 1', error)
  CALL ErrorParam('Startposition', startpos, error)

ELSEIF (finalpos.GT.lengte) THEN
  CALL SetError('Endposition beyond capacity', error)
  CALL ErrorParam('Endposition', finalpos, error)
  CALL ErrorParam('String length', lengte, error)

ELSEIF (finalpos.lt.startpos-1) THEN
  CALL SetError('Startposition beyond endposition', error)
  CALL ErrorParam('Startposition', startpos, error)
  CALL ErrorParam('Endposition', finalpos, error)

ELSE
  stringtestlimits = .true.
  changeany=finalpos.ge.startpos
ENDIF

IF (.NOT.stringtestlimits) THEN
  CALL ErrorParam('String', string, error)
  CALL ErrorCall(ROUTINENAAM, error)
  CALL ErrorCall(routinename, error)
ENDIF

END FUNCTION stringtestlimits
!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : stringsetlimits
! DESCRIPTION          : Test string limits. When an error occurs a message is written and a negative result is
!                        returned. Also set and check limits for the result (target) string.
! CALLING FUNCTIONS    : copystrpart, appendstrpart
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION stringsetlimits(inputroutine, sourcestring, startpos, endpos, targetlengte, targetstring, eindsourcepos,              &
                      &  eindtargetpos, changeany, error)

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: inputroutine               ! naam of subroutine where error occurred
CHARACTER*(*), INTENT(IN)                        :: sourcestring               ! te copieren string
INTEGER*4, INTENT(IN)                            :: startpos                   ! positie vanaf waar moet worden gecopieerd
INTEGER*4, INTENT(IN)                            :: endpos                     ! positie tot waar moet worden gecopieerd. Als -1
INTEGER*4, INTENT(IN)                            :: targetlengte               ! aantal karakters in targetstring dat blijft staan

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: targetstring               ! string waarachter wordt geplakt

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: eindsourcepos              ! eindpostie in source string
INTEGER*4, INTENT(OUT)                           :: eindtargetpos              ! eindpostie in target string
LOGICAL,   INTENT(OUT)                           :: changeany                  ! false als er niets gezet wordt
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! RESULT
LOGICAL                                          :: stringsetlimits            ! 

! LOCAL VARIABLES
INTEGER*4                                        :: teller                     ! 
INTEGER*4                                        :: targetmaxlengte            ! dclaratie lengte in targetstring
INTEGER*4                                        :: sourcelengte               ! bijdrage sourcestring in lengte

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'stringsetlimits')

!-------------------------------------------------------------------------------------------------------------------------------
!
! Test grenzen en bepaal eindtargetpos.
!
stringsetlimits = .false.
IF (stringtestlimits(inputroutine, sourcestring, startpos, endpos, eindsourcepos, changeany, error)) THEN
   sourcelengte=eindsourcepos-startpos+1
   eindtargetpos=targetlengte+sourcelengte
   targetmaxlengte=LEN(targetstring)
   IF (eindtargetpos.GT.targetmaxlengte) THEN
!
!     Limits not OK, so create an error.
!
     CALL SetError('Capacity output string less than required', error)
     CALL ErrorParam('Required', eindtargetpos, error)
     CALL ErrorParam('Allocated', targetmaxlengte, error)
     CALL ErrorCall(ROUTINENAAM, error)
     CALL ErrorCall(inputroutine, error)
RETURN

   ELSE
!
!     Grenzen OK. Plak een heleboel spaties achter de targetstring.
!
      stringsetlimits = .true.
      DO teller = eindtargetpos+1,targetmaxlengte
         targetstring(teller:teller) = ' '
      ENDDO
   ENDIF
ENDIF

END FUNCTION stringsetlimits

END MODULE m_string
