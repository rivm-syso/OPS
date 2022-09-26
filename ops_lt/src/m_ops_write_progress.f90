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
! USAGE                :
! DESCRIPTION          : Write progress of run to screen and to a progress file.
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_write_progress

implicit none

contains

SUBROUTINE ops_write_progress(progress, formatstring, numbs, memdone)

use m_utils
use m_commonfile

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_write_progress')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: progress                   ! percentage of progress reached
CHARACTER*(*), INTENT(IN)                        :: formatstring               ! formatstring for writing progress
INTEGER*4, INTENT(IN)                            :: numbs                      ! number of characters which have to be backspaced
                                                                               ! in order to remain at the same position of the screen 

! SUBROUTINE ARGUMENTS - I/O
INTEGER*4, INTENT(INOUT)                         :: memdone                     

! LOCAL VARIABLES
INTEGER*4                                        :: idx                        ! do-loop index

!-------------------------------------------------------------------------------------------------------------------------------
!
! Tests showed that, using a Compaq Fortran compiler:
!    WRITE(*, formatstring, ADVANCE='NO') works ok, but
!    WRITE(6, formatstring, ADVANCE='NO') does not print anything on screen.
! Anyhow, the statement WRITE(6,..) should be discarded, since it is compiler dependent.
!
! The statement:
!    WRITE(IOB_STDOUT,FMT='(A)', ADVANCE='NO') CHAR(8) works fine under Compaq Fortran. But the statement:
!    WRITE(*,FMT='(A)', ADVANCE='NO') CHAR(8) does not work using Compaq WinNT!
!
!  FLUSH statements are needed HP-Ux.

! Check whether to write progress, this is done each 2% 
! See definition of memdone (2% hidden in the MOD statement).
! memdone is either INT(progress) or INT(progress)-1, with progress the progress of the previous call

!
IF (INT(progress)-memdone > 1) THEN
!
! Write the progress according to formatstring format.
!
  WRITE(*, formatstring, ADVANCE='NO') progress
  CALL FLUSH (IOB_STDOUT)
!
! Back-space cursor by numbs characters.
!
  DO idx=1, numbs
    WRITE(IOB_STDOUT,FMT='(A)', ADVANCE='NO') CHAR(8)
    CALL FLUSH (IOB_STDOUT)
  ENDDO
!
! Update memdone.
! MOD(INT(progress)),2) is either 0 or 1 
! memdone is either INT(progress) or INT(progress)-1 
!
  memdone = INT(progress) - MOD(INT(progress),2)
!
! Update the progress information in progress file.
!
  REWIND (fu_progress)
  WRITE (fu_progress,'(I3)') memdone
  CALL FLUSH(fu_progress)
ENDIF

RETURN
END SUBROUTINE ops_write_progress

end module m_ops_write_progress
