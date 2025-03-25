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
! DESCRIPTION        : Check if we should calculate this source <-> receptor due to a minimum or maximum distance or not
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_check_reken

implicit none

contains

SUBROUTINE ops_check_reken(reken, mindist, maxdist, xm, ym, bx, by)

use m_commonconst_lt                                                              ! EPS_DELTA only

IMPLICIT NONE

CHARACTER(len=*), parameter :: ROUTINENAAM = 'ops_check_reken'

LOGICAL,   INTENT(IN) :: mindist, maxdist ! indicator whether results for receptors starting from mindist/upto maxdist will be calculated
REAL,      INTENT(IN) :: xm, ym           ! (x,y)-coordinates of receptor (m RDM)
INTEGER,   INTENT(IN) :: bx, by           ! (x,y)-coordinates of source (m RDM)                    
LOGICAL,   INTENT(OUT) :: reken                      ! indicator whether results for receptors should be calculated or skipped

REAL   :: disx  ! linear distance between source and receptor [m]

reken = .true.
IF (mindist .or. maxdist) THEN
    disx  = SQRT(((bx-xm)*(bx-xm)) + ((by-ym)*(by-ym)))
    IF (mindist .and. maxdist) THEN
        IF (disx .LE. DISTMIN .or. disx .GT. DISTMAX) reken = .FALSE. ! next source <-> receptor calculation.
    ELSEIF (mindist) THEN
        IF (disx .LE. DISTMIN) reken = .FALSE. ! next source <-> receptor calculation.
    ELSE
        IF (disx .GT. DISTMAX) reken = .FALSE. ! next source <-> receptor calculation.
    ENDIF
ENDIF

RETURN
END SUBROUTINE ops_check_reken

end module m_ops_check_reken
