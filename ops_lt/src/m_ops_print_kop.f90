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
! DESCRIPTION        : Print page header ("kop"= head)
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_print_kop

implicit none

contains

SUBROUTINE ops_print_kop(project, namco)

use m_commonconst_lt
USE m_commonfile

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_print_kop')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: project                    ! projectnaam
CHARACTER*(*), INTENT(IN)                        :: namco                      ! naam van de component

! LOCAL VARIABLES
INTEGER                                          :: dattim(8)                  ! date and time values
INTEGER                                          :: i                          ! teller
CHARACTER*10                                     :: date                       ! datum
CHARACTER*8                                      :: time                       ! tijd
INTEGER                                          :: versielen                  ! length of version string
INTEGER                                          :: releaselen                 ! length of release date string
INTEGER                                          :: marginlen                  ! number of = in margin

!-------------------------------------------------------------------------------------------------------------------------------
!
! Get date and time
!
CALL DATE_AND_TIME (VALUES=dattim)
WRITE (date,'(I2.2,"-",I2.2,"-",I4.4)') dattim(3), dattim(2), dattim(1)
WRITE (time,'(I2.2,":",I2.2,":",I2.2)') dattim(5), dattim(6), dattim(7)
!
! write header
!
WRITE (fu_prt, '()')
WRITE (fu_prt,'('' Project  : '',a)') project(:LEN_TRIM(project))
WRITE (fu_prt,'('' Substance: '',a)') namco(:LEN_TRIM(namco))
WRITE (fu_prt,'('' Date/time: '',A10,"; ",A8)') date, time

versielen = LEN_TRIM(MODVERSIE)+15
releaselen = LEN_TRIM(RELEASEDATE)
marginlen = (63 - versielen - releaselen) / 2

#ifndef UNIX
  WRITE (fu_prt, '(1x,80a)') ('=', i = 1, marginlen), ' ', "OPS-version: W-", MODVERSIE(1:LEN_TRIM(MODVERSIE)), ' ', RELEASEDATE(1:releaselen), ' ', ('=',i = 1, marginlen)
#else
  WRITE (fu_prt, '(1x,80a)') ('=', i = 1, marginlen), ' ', "OPS-version: L-", MODVERSIE(1:LEN_TRIM(MODVERSIE)), ' ', RELEASEDATE(1:releaselen), ' ', ('=',i = 1, marginlen)
#endif

WRITE (fu_prt, '()')

RETURN
END SUBROUTINE ops_print_kop

end module m_ops_print_kop
