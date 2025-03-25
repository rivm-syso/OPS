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
! DESCRIPTION          : Prepares values for landuse and roughness and background concentrations over trajectory.
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_tra_char

implicit none

contains

SUBROUTINE ops_tra_char (icm, iopt_vchem, f_z0user, z0_user, x_rcp, y_rcp, x_src, y_src, &
                      &  lugrid, z0nlgrid, z0eurgrid, so2bggrid, no2bggrid, nh3bggrid, gwgrid, o3bggrid, vchem_emep, domlu, & 
                      &  mass_prec_tra, mass_conv_dtfac_tra, z0_tra, lu_tra_per, so2bgtra, no2bgtra, nh3bgtra, gwtra, o3bgtra,      &
                      &  error)

use m_commonconst_lt
use m_commonfile
use m_error
use m_aps
use m_ops_vchem
use m_commonconst_lib, only: NLU
use m_ops_getz0_tra
use m_ops_getlu_tra
use m_ops_bgcon_tra
use m_ops_getlu_tra
use m_ops_getz0_tra

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_tra_char')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: icm                        ! 
INTEGER,   INTENT(IN)                            :: iopt_vchem                 ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
LOGICAL,   INTENT(IN)                            :: f_z0user                   ! user overwrites z0 values from meteo input
REAL,      INTENT(IN)                            :: z0_user                    ! roughness length specified by the user [m]
REAL,      INTENT(IN)                            :: x_rcp                      ! array met x-coordinaat van receptorpunten (RDM)
REAL,      INTENT(IN)                            :: y_rcp                      ! array met y-coordinaat van receptorpunten (RDM)
INTEGER,   INTENT(IN)                            :: x_src                      ! array met x-coordinaat van bronnen in buffer
INTEGER,   INTENT(IN)                            :: y_src                      ! array met y-coordinaat van bronnen in buffer
TYPE (TApsGridInt), INTENT(IN)                   :: lugrid                     ! grid with land use class information (1: dominant land use, 2:NLU+1: percentages land use class)
TYPE (TApsGridInt), INTENT(IN)                   :: z0nlgrid                   ! map of roughness lengths in NL [m]
TYPE (TApsGridInt), INTENT(IN)                   :: z0eurgrid                  ! map of roughness lengths in Europe [m]
TYPE (TApsGridReal), INTENT(IN)                  :: so2bggrid                  ! grid with SO2 background concentration [ppb] (read as ug/m3, converted to ppb)
TYPE (TApsGridReal), INTENT(IN)                  :: no2bggrid                  ! grid with NO2 background concentration [ppb] (read as ug/m3, converted to ppb)
TYPE (TApsGridReal), INTENT(IN)                  :: nh3bggrid                  ! grid with NH3 background concentration [ppb] (read as ug/m3, converted to ppb)
TYPE (TApsGridReal), INTENT(IN)                  :: gwgrid                     ! grid with NH3 background concentration [ppb] (read as ug/m3, converted to ppb)
TYPE (TApsGridReal), INTENT(IN)                  :: o3bggrid                   ! grids with O3 background concentration per wind sector [ug/m3]
TYPE (Tvchem)      , INTENT(IN)                  :: vchem_emep                 ! grids with EMEP precursor mass and converted mass for computing chemical conversion rates                
LOGICAL,   INTENT(IN)                            :: domlu                      ! use dominant land use instead of land use percentages

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: mass_prec_tra, mass_conv_dtfac_tra 
REAL,      INTENT(OUT)                           :: z0_tra                     ! roughness length representative for trajectory [m] 
REAL,      INTENT(INOUT)                         :: lu_tra_per(NLU)            ! percentages of landuse classes over trajectorie (summed over intermediate points)
                                                                               ! NB: intent was changed from OUT to INOUT because not all entries are always set. 
REAL,      INTENT(INOUT)                         :: so2bgtra                   ! NB: intent was changed from OUT to INOUT because not all entries are always set.  
REAL,      INTENT(INOUT)                         :: no2bgtra                   ! NB: intent was changed from OUT to INOUT because not all entries are always set.  
REAL,      INTENT(INOUT)                         :: nh3bgtra                   ! NB: intent was changed from OUT to INOUT because not all entries are always set.  
REAL,      INTENT(INOUT)                         :: gwtra                      ! NB: intent was changed from OUT to INOUT because not all entries are always set.  
REAL,      INTENT(OUT)                           :: o3bgtra(NSEK)              ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES:

!-------------------------------------------------------------------------------------------------------------------------------

IF (f_z0user) THEN
   ! If user specified z0 then set z0_tra and lu_tra_per (We assume grass=lu=1 if user specified z0): 
   z0_tra        = z0_user
   lu_tra_per(1) = 100
ELSE
   ! Calculate average roughness length and land use for path between source and receptor:
   CALL ops_getz0_tra(x_rcp, y_rcp, float(x_src), float(y_src), z0nlgrid, z0eurgrid, z0_tra, error)
   IF (error%haserror) THEN
      CALL ErrorParam('Error along path between source and receptor', '', error)
      CALL ErrorParam('source coordinates', (/ x_src, y_src /), error)
      CALL ErrorParam('receptor coordinates', (/ x_rcp, y_rcp /), error)
      goto 9999
   ENDIF
   
   IF (ANY(icm == (/icm_SO2,icm_NOx,icm_NH3/))) THEN
       CALL ops_getlu_tra(x_rcp, y_rcp, float(x_src), float(y_src), lugrid, domlu, lu_tra_per, error)
       IF (error%haserror) THEN
          CALL ErrorParam('Error along path between source and receptor', '', error)
          CALL ErrorParam('source coordinates', (/ x_src, y_src /), error)
          CALL ErrorParam('receptor coordinates', (/ x_rcp, y_rcp /), error)
          goto 9999
       ENDIF
   ENDIF
ENDIF

! Calculate average (actual) concentration levels of SO2, NO2, NH3 and gamma water between source and receptor
! from background concentration maps which are scaled on the basis of measurements:
IF (ANY(icm == (/icm_SO2,icm_NH3/)))   CALL ops_bgcon_tra(x_rcp, y_rcp, float(x_src), float(y_src), so2bggrid, so2bgtra, error)
IF (ANY(icm == (/icm_NOx,icm_NH3/)))   CALL ops_bgcon_tra(x_rcp, y_rcp, float(x_src), float(y_src), no2bggrid, no2bgtra, error)
IF (ANY(icm == (/icm_SO2,icm_NOx,icm_NH3/))) CALL ops_bgcon_tra(x_rcp, y_rcp, float(x_src), float(y_src), nh3bggrid, nh3bgtra, error)
IF (icm == icm_NH3) CALL ops_bgcon_tra(x_rcp, y_rcp, float(x_src), float(y_src), gwgrid, gwtra, error)
! IF (icm == icm_NOx .and. nemcat_road .gt. 0) CALL ops_bgcon_tra(x_rcp, y_rcp, float(x_src), float(y_src), o3bggrid,  o3bgtra,  error) 
o3bgtra = -999.0 

IF (error%haserror) goto 9999

! Compute average mass_prec and mass_conv_dtfac values ocver trajectory (EMEP option iopt_vchem = 1):
IF ((icm == icm_SO2 .or. icm == icm_NOx .or. icm == icm_NH3) .and. iopt_vchem .eq. 1) then
   CALL ops_bgcon_tra(x_rcp, y_rcp, float(x_src), float(y_src), vchem_emep%mass_prec_grid, mass_prec_tra, error)
   CALL ops_bgcon_tra(x_rcp, y_rcp, float(x_src), float(y_src), vchem_emep%mass_conv_dtfac_grid, mass_conv_dtfac_tra, error)
   IF (error%haserror) goto 9999
   ! write(*,*) 'ops_tra_char: ',mass_prec_tra,mass_conv_dtfac_tra,mass_conv_dtfac_tra/mass_prec_tra
ENDIF

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)
RETURN

END SUBROUTINE ops_tra_char

end module m_ops_tra_char
