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
! DESCRIPTION          : Prepares values for landuse and roughness and background concentrations over trajectory.
! EXIT CODES           :
! FILES AND OTHER      :
!      I/O DEVICES
! SYSTEM DEPENDENCIES  : HP-Fortran
! CALLED FUNCTIONS     :
! UPDATE HISTORY       :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_tra_char (icm, iopt_vchem, f_z0user, z0_user, nrrcp, x_rcp, y_rcp, x_src, y_src,                           &
                      &  lugrid, z0nlgrid, z0eurgrid, so2bggrid, no2bggrid, nh3bggrid, vchem2, domlu, & 
                      &  z0_tra, lu_tra_per, so2bgtra, no2bgtra, nh3bgtra,       &
                      &  error)

USE m_commonconst
USE m_commonfile
USE m_error
USE m_aps
USE m_ops_vchem

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_tra_char')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: icm                        ! 
INTEGER*4, INTENT(IN)                            :: iopt_vchem                 ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
LOGICAL,   INTENT(IN)                            :: f_z0user                   ! user overwrites z0 values from meteo input
real,      INTENT(IN)                            :: z0_user                    ! roughness length specified by the user [m]
INTEGER*4, INTENT(IN)                            :: nrrcp                      ! aantal receptorpunten
real,      INTENT(IN)                            :: x_rcp                      ! array met x-coordinaat van receptorpunten (RDM)
real,      INTENT(IN)                            :: y_rcp                      ! array met y-coordinaat van receptorpunten (RDM)
INTEGER*4, INTENT(IN)                            :: x_src                      ! array met x-coordinaat van bronnen in buffer
INTEGER*4, INTENT(IN)                            :: y_src                      ! array met y-coordinaat van bronnen in buffer
TYPE (TApsGridInt), INTENT(IN)                   :: lugrid                     ! land use grid
TYPE (TApsGridInt), INTENT(IN)                   :: z0nlgrid                   ! map of roughness lengths in NL [m]
TYPE (TApsGridInt), INTENT(IN)                   :: z0eurgrid                  ! map of roughness lengths in Europe [m]
TYPE (TApsGridReal), INTENT(IN)                  :: so2bggrid                  ! 
TYPE (TApsGridReal), INTENT(IN)                  :: no2bggrid                  ! 
TYPE (TApsGridReal), INTENT(IN)                  :: nh3bggrid                  ! 
TYPE (Tvchem)      , INTENT(INOUT)               :: vchem2                     !
LOGICAL,   INTENT(IN)                            :: domlu

! SUBROUTINE ARGUMENTS - OUTPUT
real,      INTENT(OUT)                           :: z0_tra                     ! roughness length representative for trajectory [m]
real,      INTENT(OUT)                           :: lu_tra_per(NLU)            ! percentages of landuse classes over trajectorie (summed over intermediate points)
real,      INTENT(OUT)                           :: so2bgtra
real,      INTENT(OUT)                           :: no2bgtra
real,      INTENT(OUT)                           :: nh3bgtra
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES:

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! If user specified z0 then set z0_tra and lu_tra_per (We assume grass=lu=1 if user specified z0): 
!
IF (f_z0user) THEN
  z0_tra        = z0_user
  lu_tra_per(1) = 100
ELSE
!
!      Calculate average roughness length and land use for path between source and receptor
!
  CALL ops_getz0_tra(x_rcp, y_rcp, float(x_src), float(y_src), z0nlgrid, z0eurgrid, z0_tra)
IF (ANY(icm == (/1,2,3/))) CALL ops_getlu_tra(x_rcp, y_rcp, float(x_src), float(y_src), lugrid, domlu, lu_tra_per)
ENDIF
!
!   Calculate average (actual) concentration levels of SO2, NO2 and NH3 between source and receptor
!   from background concentration maps which are scaled on the basis of measurements
!
IF (ANY(icm == (/1,3/)))   CALL ops_bgcon_tra(x_rcp, y_rcp, float(x_src), float(y_src), so2bggrid, so2bgtra)
IF (ANY(icm == (/2,3/)))   CALL ops_bgcon_tra(x_rcp, y_rcp, float(x_src), float(y_src), no2bggrid, no2bgtra)
IF (ANY(icm == (/1,2,3/))) CALL ops_bgcon_tra(x_rcp, y_rcp, float(x_src), float(y_src), nh3bggrid, nh3bgtra)

! Compute average mass_prec and mass_conv_dtfac values ocver trajectory (EMEP option iopt_vchem = 1):
IF ((icm == 1 .or. icm == 2 .or. icm == 3) .and. iopt_vchem .eq. 1) then
   CALL ops_bgcon_tra(x_rcp, y_rcp, float(x_src), float(y_src), vchem2%mass_prec_grid, vchem2%mass_prec_tra)
   CALL ops_bgcon_tra(x_rcp, y_rcp, float(x_src), float(y_src), vchem2%mass_conv_dtfac_grid, vchem2%mass_conv_dtfac_tra)
   ! write(*,*) 'ops_tra_char: ',vchem2%mass_prec_tra,vchem2%mass_conv_dtfac_tra,vchem2%mass_conv_dtfac_tra/vchem2%mass_prec_tra
ENDIF

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)
RETURN

END SUBROUTINE ops_tra_char
