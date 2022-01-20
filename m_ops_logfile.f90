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
module m_ops_logfile

! Different functions 
! logical function ops_openlog     Opens log file if not done before. Uses lognam in m_commonfile, which is the name of the log file.
! subroutine ops_closelog          Closes the log file if it is open. Uses lognam in m_commonfile, which is the name of the log file.  

implicit none

contains

!----------------------------------------------------------------------------------------------
logical function ops_openlog(error)

! logical function ops_openlog     Opens log file if not done before. Uses lognam in m_commonfile, which is the name of the log file.

use m_fileutils
use m_error
use m_commonfile

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! set when error is detected

! LOCAL VARIABLES
LOGICAL                                          :: isopen                     ! file is open

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_openlog')

! Inquire whether the log file is already opened. If not so, open the log file:
ops_openlog =.TRUE.
INQUIRE(UNIT = fu_log, OPENED = isopen)

IF (.NOT. isopen) THEN
  IF (.NOT.sysopen(fu_log, lognam, 'w', 'log file', error)) THEN
     CALL ErrorCall(ROUTINENAAM, error)
     ops_openlog =.FALSE.
   ENDIF
ENDIF

end function ops_openlog

!----------------------------------------------------------------------------------------------
SUBROUTINE ops_closelog(error)

! subroutine ops_closelog          Closes the log file if it is open. Uses lognam in m_commonfile, which is the name of the log file.  

USE m_fileutils
USE m_error
USE m_commonfile

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! set when error is detected

! LOCAL VARIABLES
LOGICAL                                          :: isopen                     ! file is open
LOGICAL                                          :: haderror                   ! at input, error is already set

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_closelog')
!
! We also come here when an error has already occurred. In that case this routine should not appear in the traceback. This is
! prevented by the parameter haderror:
haderror = error%haserror

! Inquire whether the log file has been opened and, if so, close the log file:
INQUIRE(UNIT = fu_log, OPENED = isopen)
IF (isopen) THEN
   CALL sysclose(fu_log, lognam, error)
   IF (.NOT.haderror .AND. error%haserror) THEN
      CALL ErrorCall(ROUTINENAAM, error)
   ENDIF
ENDIF

END SUBROUTINE ops_closelog

end module m_ops_logfile
