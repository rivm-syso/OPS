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

module m_tst_ops_conc_sum

contains

SUBROUTINE tst_ops_conc_sum

use no_pfunit
use m_ops_conc_sum
use m_utils,           only: alloc
use m_commonconst_lib, only: NTRAJ, NSTAB, NSEK
use m_commonconst_lt,  only: NPARTCLASS

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
REAL                           :: c                          ! concentration at receptor height zm [ug/m3]
REAL                           :: consec                     ! concentration of secondary component at receptor height zm [ug/m3] 
REAL                           :: percvk                     ! fraction of occurrence of {distance/stability/wind-direction} class
INTEGER                        :: iter                       ! iteration index for road correction
INTEGER                        :: niter                      ! number of iterations for road correction
LOGICAL                        :: class_output               ! indicator whether results for receptors will be stored per wind sector/distance/particle/stability class
LOGICAL                        :: idep                       ! TRUE if deposition is modelled
LOGICAL                        :: isec                       ! TRUE if component = SO2, NOx, NH3

! SUBROUTINE ARGUMENTS - I/O
DOUBLE PRECISION            :: cpri         ! concentration of primary component at receptor points and height zm [ug/m3] 
DOUBLE PRECISION            :: cpri_class   ! concentration of primary component at receptor points and height zm, per class [ug/m3]
DOUBLE PRECISION            :: percvk_class ! percvk of primary component at receptor points and height zm, per class [factor of occurrence] 
DOUBLE PRECISION            :: csec         ! concentration of secondary component ar receptor points [ug/m3] 
INTEGER                     :: nsrc_class   ! number of sources present in wind/distance sector (-classoutput only) [-]

real      :: tol = 1e-4

! initialize the variables for this test
c = 1.3
consec = 0.8
percvk = 0.2
iter = 1
niter = 1
class_output = .false.
idep = .false.
isec = .true.

! default case without idep, class_output
cpri = 1.0
CALL ops_conc_sum(c, consec, percvk, iter, niter, class_output, idep, isec, &
                  cpri, csec, cpri_class, percvk_class, nsrc_class)
call assertEqual(1.0+c*percvk, real(cpri), tol, "cpri, default, no idep/class_output",__LINE__,__FILE__)

! with idep, without class_output
cpri = 1.0
csec = 0.5
idep = .true.
CALL ops_conc_sum(c, consec, percvk, iter, niter, class_output, idep, isec, &
                  cpri, csec, cpri_class, percvk_class, nsrc_class)
call assertEqual(1.0+c*percvk, real(cpri), tol, "cpri, idep=T, class_output=F",__LINE__,__FILE__)
call assertEqual(0.5+consec*percvk, real(csec), tol, "csec, idep=T, class_output=F",__LINE__,__FILE__)

! with idep & class_output
cpri = 1.0
csec = 0.5
idep = .true.
class_output = .true.
cpri_class = 0.9
percvk_class = 0.1
nsrc_class = 0
CALL ops_conc_sum(c, consec, percvk, iter, niter, class_output, idep, isec, &
                  cpri, csec, cpri_class, percvk_class, nsrc_class)
call assertEqual(1.0+c*percvk, real(cpri), tol, "cpri, idep=T, class_output=T",__LINE__,__FILE__)
call assertEqual(0.5+consec*percvk, real(csec), tol, "csec, idep=T, class_output=T",__LINE__,__FILE__)
call assertEqual(0.9+c*percvk, real(cpri_class), tol, "cpri_class, idep=T, class_output=T",__LINE__,__FILE__)
call assertEqual(0.1+percvk, real(percvk_class), tol, "percvk_class, idep=T, class_output=T",__LINE__,__FILE__)
call assertEqual(1, nsrc_class, "nsrc_class, idep=T, class_output=T",__LINE__,__FILE__)

! test incrementing nsrc_class
nsrc_class = 5
CALL ops_conc_sum(c, consec, percvk, iter, niter, class_output, idep, isec, &
                  cpri, csec, cpri_class, percvk_class, nsrc_class)
call assertEqual(6, nsrc_class, "nsrc_class increment",__LINE__,__FILE__)

! with idep & class_output, but iter != niter
niter = 2
cpri = 1.0
csec = 0.5
idep = .true.
class_output = .true.
cpri_class = 0.9
percvk_class = 0.1
nsrc_class = 3
CALL ops_conc_sum(c, consec, percvk, iter, niter, class_output, idep, isec, &
                  cpri, csec, cpri_class, percvk_class, nsrc_class)
call assertEqual(1.0+c*percvk     , real(cpri)        , tol, "cpri, idep=T, class_output=T, iter!=niter",__LINE__,__FILE__)
call assertEqual(0.5+consec*percvk, real(csec)        , tol, "csec, idep=T, class_output=T, iter!=niter",__LINE__,__FILE__)
call assertEqual(0.9              , real(cpri_class)  , tol, "cpri_class, idep=T, class_output=T, iter!=niter",__LINE__,__FILE__)
call assertEqual(0.1              , real(percvk_class), tol, "percvk_class, idep=T, class_output=T, iter!=niter",__LINE__,__FILE__)
call assertEqual(3                , nsrc_class        ,      "nsrc_class, idep=T, class_output=T, iter!=niter",__LINE__,__FILE__)

RETURN

END SUBROUTINE tst_ops_conc_sum

end module m_tst_ops_conc_sum

program p_tst_ops_conc_sum
use m_tst_ops_conc_sum
use no_pfunit
implicit none
   call tst_ops_conc_sum()
   call conclusion()

end program p_tst_ops_conc_sum
