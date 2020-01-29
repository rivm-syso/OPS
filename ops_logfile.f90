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
! FILENAME           : %M%
! SCCS (SOURCE)      : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH - SEQUENCE  : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : Martien de Haan (ARIS)
! FIRM/INSTITUTE     : RIVM/LLO
! LANGUAGE           : FORTRAN-F77/90
! DESCRIPTION        : Handling of log file. The log file is only opened and closed if something is written to it.
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP Fortran
! CALLED FUNCTIONS   :
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------------------------------------------------------------
! Function     ops_openlog
! Purpose      Opens log file if not done before.
! Uses         lognam in m_commonfile, which is the name of the log file.
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION ops_openlog(error)

USE m_fileutils
USE m_error
USE m_commonfile

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! set when error is detected

! LOCAL VARIABLES
LOGICAL                                          :: isopen                     ! file is open

! FUNCTION RESULT
LOGICAL                                          :: ops_openlog                ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_openlog')

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!
! Inquire whether the log file is already opened. If not so, open the log file.
!
ops_openlog =.TRUE.
INQUIRE(UNIT = fu_log, OPENED = isopen)

IF (.NOT. isopen) THEN
 IF (.NOT.sysopen(fu_log, lognam, 'w', 'log file', error)) THEN
    CALL ErrorCall(ROUTINENAAM, error)
    ops_openlog =.FALSE.
  ENDIF
ENDIF

RETURN
END FUNCTION ops_openlog

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   ops_closelog
! Purpose      Closes the log file if it is open.
! Uses         lognam in m_commonfile, which is the name of the log file.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_closelog(error)

USE m_fileutils
USE m_error
USE m_commonfile

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! set when error is detected

! LOCAL VARIABLES
LOGICAL                                          :: isopen                     ! file is open
LOGICAL                                          :: haderror                   ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_closelog')
!
! We also come here when an error has already occurred. In that case this routine should not appear in the traceback. This is
! prevented by the parameter haderror.
!
haderror = error%haserror
!
! Inquire whether the log file has been opened and, if so, close the log file.
!
INQUIRE(UNIT = fu_log, OPENED = isopen)
IF (isopen) THEN
  CALL sysclose(fu_log, lognam, error)
  IF (.NOT.haderror .AND. error%haserror) THEN
    CALL ErrorCall(ROUTINENAAM, error)
  ENDIF
ENDIF

RETURN
END SUBROUTINE ops_closelog
