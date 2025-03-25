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
! DESCRIPTION          : Prepares values for roughness at source location.
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_src_char 

implicit none

contains

SUBROUTINE ops_src_char (f_z0user, z0_user, xb, yb, z0nlgrid, z0eurgrid, z0_src, error)

use m_commonconst_lt
use m_commonfile
use m_error
use m_aps
use m_ops_getz0

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_src_char')

! SUBROUTINE ARGUMENTS - INPUT
LOGICAL,   INTENT(IN)                            :: f_z0user                   ! user overwrites z0 values from meteo input
REAL,      INTENT(IN)                            :: z0_user                    ! roughness length specified by the user [m]
INTEGER,   INTENT(IN)                            :: xb                         ! x-coordinaat van huidige bron in buffer
INTEGER,   INTENT(IN)                            :: yb                         ! y-coordinaat van huidige bron in buffer
TYPE (TApsGridInt), INTENT(IN)                   :: z0nlgrid                   ! map of roughness lengths in NL [m]
TYPE (TApsGridInt), INTENT(IN)                   :: z0eurgrid                  ! map of roughness lengths in Europe [m]

! SUBROUTINE ARGUMENTS - INPUT/OUTPUT
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: z0_src                     ! roughness length at source; from z0-map [m]

!-------------------------------------------------------------------------------------------------------------------------------
!
! If user specified z0 then set z0_src
!
IF (f_z0user) THEN
  z0_src = z0_user
ELSE
!
! If not user specified z0 then get z0_src from grid
!
  CALL ops_getz0(float(xb), float(yb), z0nlgrid, z0eurgrid, z0_src, error)
  IF (error%haserror) goto 9999
ENDIF

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE ops_src_char

end module m_ops_src_char 
