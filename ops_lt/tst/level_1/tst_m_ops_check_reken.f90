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

module m_tst_ops_check_reken

contains 

SUBROUTINE tst_ops_check_reken

use no_pfunit
use m_ops_check_reken
use m_commonconst_lt
use m_utils,           only: alloc

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
LOGICAL                              :: mindist                    ! indicator whether results for receptors starting from mindist will be calculated
LOGICAL                              :: maxdist                    ! indicator whether results for receptors upto maxdist will be calculated
REAL,   TARGET                       :: xm(1)                         ! x-coordinates of receptors (m RDM)
REAL,   TARGET                       :: ym(1)                         ! y-coordinates of receptors (m RDM)
INTEGER,   TARGET                    :: bx(LSBUF)                  ! x-coordinates of sources (m RDM)                    
INTEGER,   TARGET                    :: by(LSBUF)                  ! y-coordinates of sources (m RDM
INTEGER                              :: ircp
INTEGER                              :: mmm

! SUBROUTINE ARGUMENTS - OUTPUT
LOGICAL                              :: reken                      ! indicator whether results for receptors should be calculated or skipped

! initialize
reken = .true.
mindist = .false.
maxdist = .false.
xm(1) = 116184
ym(1) = 456917
ircp = 1
mmm = 1

! just larger than distmin
bx(1) = xm(1) + DISTMIN/SQRT(2.0) + 1
by(1) = ym(1) - DISTMIN/SQRT(2.0) - 1

! test 1, reken .true.
call ops_check_reken(reken, mindist, maxdist, xm(ircp), ym(ircp), bx(mmm), by(mmm))
call assertEqual(.true., reken, "test 1",__LINE__,__FILE__)

! test 2, reken .true.
mindist = .true.
call ops_check_reken(reken, mindist, maxdist, xm(ircp), ym(ircp), bx(mmm), by(mmm))
call assertEqual(.true., reken, "test 2",__LINE__,__FILE__)

! test 3, reken .true.
mindist = .true.
maxdist = .true.
call ops_check_reken(reken, mindist, maxdist, xm(ircp), ym(ircp), bx(mmm), by(mmm))
call assertEqual(.true., reken, "test 3",__LINE__,__FILE__)

! just smaller than distmax
bx(1) = xm(1) + DISTMAX/SQRT(2.0) - 1
by(1) = ym(1) - DISTMAX/SQRT(2.0) + 1

! test 4, reken .true.
mindist = .false.
maxdist = .true.
call ops_check_reken(reken, mindist, maxdist, xm(ircp), ym(ircp), bx(mmm), by(mmm))
call assertEqual(.true., reken, "test 4",__LINE__,__FILE__)

! test 5, reken .true.
mindist = .true.
maxdist = .true.
call ops_check_reken(reken, mindist, maxdist, xm(ircp), ym(ircp), bx(mmm), by(mmm))
call assertEqual(.true., reken, "test 5",__LINE__,__FILE__)

! outside distmax
bx(1) = xm(1) + DISTMAX/SQRT(2.0) + 1
by(1) = ym(1) - DISTMAX/SQRT(2.0) - 1

! test 6, reken .false.
reken = .true.
mindist = .false.
maxdist = .true.
call ops_check_reken(reken, mindist, maxdist, xm(ircp), ym(ircp), bx(mmm), by(mmm))
call assertEqual(.false., reken, "test 6",__LINE__,__FILE__)

! test 7, reken .false.
reken = .true.
mindist = .true.
maxdist = .true.
call ops_check_reken(reken, mindist, maxdist, xm(ircp), ym(ircp), bx(mmm), by(mmm))
call assertEqual(.false., reken, "test 7",__LINE__,__FILE__)

! outside distmin
bx(1) = xm(1) + DISTMIN/SQRT(2.0) - 1
by(1) = ym(1) - DISTMIN/SQRT(2.0) + 1

! test 8, reken .false.
reken = .true.
mindist = .true.
maxdist = .false.
call ops_check_reken(reken, mindist, maxdist, xm(ircp), ym(ircp), bx(mmm), by(mmm))
call assertEqual(.false., reken, "test 8",__LINE__,__FILE__)

! test 9, reken .false.
reken = .true.
mindist = .true.
maxdist = .true.
call ops_check_reken(reken, mindist, maxdist, xm(ircp), ym(ircp), bx(mmm), by(mmm))
call assertEqual(.false., reken, "test 9",__LINE__,__FILE__)

RETURN

END SUBROUTINE tst_ops_check_reken

end module m_tst_ops_check_reken
 
program p_tst_ops_check_reken
use m_tst_ops_check_reken
use no_pfunit
implicit none
   call tst_ops_check_reken
   call conclusion()
end program p_tst_ops_check_reken

