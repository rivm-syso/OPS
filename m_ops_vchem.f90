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
!                       Copyright by
!   National Institute of Public Health and Environment
!           Laboratory for Air Research (RIVM/LLO)
!                      The Netherlands
!   No part of this software may be used, copied or distributed without permission of RIVM/LLO (2002)
!
! MODULE             : m_ops_vchem
! FILENAME           : %M%
! SCCS(SOURCE)       : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH - SEQUENCE  : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : OPS-support   
! FIRM/INSTITUTE     : RIVM
! LANGUAGE           : FORTRAN-F90
! DESCRIPTION        : Define structure for chemical conversion rates
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: 
! CALLED FUNCTIONS   :
! UPDATE HISTORY :
!-------------------------------------------------------------------------------------------------------------------------------
MODULE m_ops_vchem

USE m_aps

IMPLICIT NONE

type Tvchem
  
   TYPE (TApsGridReal) :: mass_prec_grid               ! APS grid with column averaged mass of precursor pre chemistry step (from chemistry model, e.g. EMEP) [ug/m2]
   TYPE (TApsGridReal) :: mass_conv_dtfac_grid         ! APS grid with (100/dt) * column averaged mass, converted during chemistry step (from chemistry model, e.g. EMEP) [(ug/m2) (%/h)]

   real                :: mass_prec_tra                ! column averaged mass of precursor pre chemistry step, average between source - receptor [ug/m2]
   real                :: mass_conv_dtfac_tra          ! (100/dt) * column averaged mass, converted during chemistry step, average between source - receptor [(ug/m2) (%/h)]

   real                :: vchem                        ! chemical conversion rates for net reaction primary -> secondary species [%/h]

end type Tvchem

END MODULE m_ops_vchem
