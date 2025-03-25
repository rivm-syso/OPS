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
! DESCRIPTION        : Compute sum of primary and secondary (if idep) concentration, and optionally concentration per classes
!                      (if class_output). Used in ops_reken.
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_conc_sum
use m_commonconst_lt
implicit none
contains
SUBROUTINE ops_conc_sum(c_zrcp, consec, percvk, iter, niter, class_output, idep, isec, &
                        cpri, csec, cpri_class, percvk_class, nsrc_class)
IMPLICIT NONE
CHARACTER(len=*), parameter :: ROUTINENAAM = 'ops_conc_sum'
REAL,      INTENT(IN) :: c_zrcp       ! concentration at receptor height zm [ug/m3]
REAL,      INTENT(IN) :: consec       ! concentration of secondary component at receptor height zm [ug/m3] 
REAL,      INTENT(IN) :: percvk       ! fraction of occurrence of {distance/stability/wind-direction} class
INTEGER,   INTENT(IN) :: iter         ! iteration index for road correction
INTEGER,   INTENT(IN) :: niter        ! number of iterations for road correction
LOGICAL,   INTENT(IN) :: class_output ! indicator whether results for receptors will be stored per wind sector/distance/particle/stability class
LOGICAL,   INTENT(IN) :: idep         ! TRUE if deposition is modelled
LOGICAL,   INTENT(IN) :: isec         ! TRUE if component=[SO2, NOx, NH3]
DOUBLE PRECISION, INTENT(INOUT) :: cpri          ! concentration of primary component at receptor points and height zm [ug/m3] 
DOUBLE PRECISION, INTENT(INOUT) :: cpri_class    ! concentration of primary component at receptor points and height zm, per class [ug/m3]
DOUBLE PRECISION, INTENT(INOUT) :: percvk_class  ! percvk of primary component at receptor points and height zm, per class [factor of occurrence] 
DOUBLE PRECISION, INTENT(INOUT) :: csec          ! concentration of secondary component ar receptor points [ug/m3] 
INTEGER,          INTENT(INOUT) :: nsrc_class    ! number of sources present in wind/distance sector (-classoutput only) [-]
!--------------------------------------------------------------------------------------------------------

! Update summed concentration for primary concentration:                                            
cpri = cpri + c_zrcp*percvk

! Update summed concentration for secondary concentration:
IF (idep .and. isec) csec = csec + consec*percvk

! Update summed concentration for primary concentration per class:
IF (iter == niter .and. class_output) THEN
    IF (percvk > EPS_DELTA .and. c_zrcp > EPS_DELTA) THEN
        ! only if percvk & c_zrcp are larger than zero
        percvk_class = percvk_class + percvk
        cpri_class = cpri_class + c_zrcp*percvk
        ! Count source:
        nsrc_class = nsrc_class + 1
    ENDIF
ENDIF

RETURN

END SUBROUTINE ops_conc_sum
end module m_ops_conc_sum
