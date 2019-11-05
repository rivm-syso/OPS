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
!
! SUBROUTINE
! NAME               : %M%
! SCCS(SOURCE)       : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH -SEQUENCE   : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             :
! FIRM/INSTITUTE     : RIVM/LLO
! LANGUAGE           : FORTRAN-77/90
! DESCRIPTION        : Get file name of monitor files (log, error and progress files). 
! EXIT CODES         :
! REFERENCE          :
! FILES AND OTHER    :
!   I/O DEVICES
! SYSTEM DEPENDENCIES: HP-Fortran
! CALLED FUNCTIONS   :
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_monitor(lognam, error)

USE m_error
USE m_string
USE m_commonfile

IMPLICIT NONE
!
! USED VARIABLES
! indnam: name of progress indicator file
! errnam: name of error file

! SUBROUTINE ARGUMENTS - OUTPUT
character*(*), INTENT(OUT)                       :: lognam                     ! name of log file
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: extpos                     ! position in control file name where extension starts.
CHARACTER*512                                    :: base                       ! base name of monitor files (i.e. control file name without extension)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                
PARAMETER     (ROUTINENAAM = 'ops_monitor')

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'// char (0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Get the base of the control file name (so skip the extension)
!
extpos = INDEX(ctrnam, '.',.TRUE.) - 1
CALL CopyStrPart(ctrnam, 1, extpos, base, error)
IF (error%haserror) GOTO 9999
!
! Progress indicator file = base'.ind'
!
CALL StringMerge(base,'.ind', indnam, error)
IF (error%haserror) GOTO 9999
!
! Log file = base'.log'
!
CALL StringMerge(base,'.log', lognam, error)
IF (error%haserror) GOTO 9999
!
! Error file = base'.err'
!
CALL StringMerge(base,'.err', errnam, error)
IF (error%haserror) GOTO 9999

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)
RETURN

END SUBROUTINE ops_monitor
