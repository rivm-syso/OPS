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
!                       copyright by
!   National Institute of Public Health and Environment
!           Laboratory for Air Research (RIVM/LLO)
!                      The Netherlands
!   No part of this software may be used, copied or distributed without permission of RIVM/LLO (2002)
!
! SUBROUTINE
! NAME                 : %M%
! SCCS(SOURCE)         : %P%
! RELEASE - LEVEL      : %R% - %L%
! BRANCH -SEQUENCE     : %B% - %S%
! DATE - TIME          : %E% - %U%
! WHAT                 : %W%:%E%
! AUTHOR               : OPS-support 
! FIRM/INSTITUTE       : RIVM/LLO
! LANGUAGE             : FORTRAN-77/90
! USAGE                :
! DESCRIPTION          : Prepares values for roughness at source location.
! EXIT CODES           :
! FILES AND OTHER      :
!      I/O DEVICES
! SYSTEM DEPENDENCIES  : HP-Fortran
! CALLED FUNCTIONS     :
! UPDATE HISTORY       :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_src_char (f_z0user, z0_user, xb, yb, z0nlgrid, z0eurgrid, z0_src, error)

USE m_commonconst
USE m_commonfile
USE m_error
USE m_aps

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_src_char')

! SUBROUTINE ARGUMENTS - INPUT
LOGICAL,   INTENT(IN)                            :: f_z0user                   ! user overwrites z0 values from meteo input
REAL*4,    INTENT(IN)                            :: z0_user                    ! roughness length specified by the user [m]
INTEGER*4, INTENT(IN)                            :: xb                         ! x-coordinaat van huidige bron in buffer
INTEGER*4, INTENT(IN)                            :: yb                         ! y-coordinaat van huidige bron in buffer
TYPE (TApsGridInt), INTENT(IN)                   :: z0nlgrid                   ! map of roughness lengths in NL [m]
TYPE (TApsGridInt), INTENT(IN)                   :: z0eurgrid                  ! map of roughness lengths in Europe [m]

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: z0_src                     ! roughness length at source; from z0-map [m]
TYPE (TError)                                    :: error  

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
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
  CALL ops_getz0(float(xb), float(yb), z0nlgrid, z0eurgrid, z0_src)
ENDIF

RETURN
END SUBROUTINE ops_src_char
