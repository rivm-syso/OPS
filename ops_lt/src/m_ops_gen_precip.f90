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
! DESCRIPTION           : Generate precipitation for receptors (sum of precipitation over
!                         all wind direction sectors and stability classes).
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_gen_precip

implicit none

contains

SUBROUTINE ops_gen_precip(varin_meteo, varin_unc, uurtot, astat, trafst, precip, error)

use m_ops_varin
use m_error
use m_commonconst_lt
use m_ops_statparexp

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_gen_precip')

! SUBROUTINE ARGUMENTS - INPUT
TYPE(Tvarin_meteo), INTENT(IN)                   :: varin_meteo                ! input variables for meteo
TYPE(Tvarin_unc), INTENT(IN)                   :: varin_unc                    ! Noise value for uncertainty analyses.
REAL,      INTENT(IN)                            :: uurtot                     ! total number of hours from meteo statistics
REAL,      INTENT(IN)                            :: astat(NTRAJ, NCOMP, NSTAB, NSEK)  
REAL,      INTENT(IN)                            :: trafst(NTRAJ)               

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: precip                     ! total precipitation per year [mm/year]
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER                                          :: isek                       ! index of wind sector, isek = 1,NSEK
INTEGER                                          :: isec1                      ! dummy output of ops_statparexp
INTEGER                                          :: isec_in                    ! dummy output of ops_statparexp
INTEGER                                          :: istab                      ! index of stability class
INTEGER                                          :: iwd                        ! wind direction if wind is from source to receptor (degrees)
INTEGER                                          :: itra                       ! dummy output of ops_statparexp
REAL                                             :: hbron                      ! source height, dummy input for ops_statparexp
REAL                                             :: disx                       ! distance source receptor, dummy input for ops_statparexp
REAL                                             :: disxx                      ! dummy output of ops_statparexp
REAL                                             :: radius                     ! source diameter, dummy input for ops_statparexp
REAL                                             :: qww                        ! heat content of source, dummy input for ops_statparexp; 
                                                                               ! setting it to 0 prevents unnecessary computation of plume rise
                                                                               ! in ops_statparexp 
REAL                                             :: V_stack                    ! here a dummy
REAL                                             :: Ts_stack                   ! here a dummy         
LOGICAL                                          :: emis_horizontal            ! here a dummy
REAL                                             :: D_stack                    ! here a dummy
REAL                                             :: vw10                       ! here a dummy
REAL                                             :: h0                         ! here a dummy
REAL                                             :: hum                        ! here a dummy
REAL                                             :: ol                         ! here a dummy
REAL                                             :: shear                      ! here a dummy
REAL                                             :: rc_aer_ms                  ! here a dummy
REAL                                             :: rc_nh3_ms                  ! here a dummy
REAL                                             :: rc_no2_ms                  ! here a dummy
REAL                                             :: temp_C                     ! here a dummy
REAL                                             :: uster                      ! here a dummy
REAL                                             :: pcoef                      ! here a dummy
REAL                                             :: htot                       ! here a dummy
REAL                                             :: htt                        ! here a dummy
REAL                                             :: aant                       ! here a dummy
REAL                                             :: xl                         ! here a dummy
REAL                                             :: rb                         ! here a dummy
REAL                                             :: ra_ms_4                    ! here a dummy
REAL                                             :: ra_ms_zra                  ! here a dummy
REAL                                             :: xvglbr                     ! here a dummy
REAL                                             :: xvghbr                     ! here a dummy
REAL                                             :: xloc                       ! here a dummy
REAL                                             :: xl100                      ! here a dummy
REAL                                             :: rad                        ! here a dummy
REAL                                             :: rc_so2_ms                  ! here a dummy
REAL                                             :: coef_space_heating         ! here a dummy
REAL                                             :: buil                       ! here a dummy
REAL                                             :: regenk                     ! rain probability [-]
REAL                                             :: rint                       ! rain intensity [mm/h]
REAL                                             :: percvk                     ! fraction of occurrence of {distance/stability/wind-direction} class  

!-------------------------------------------------------------------------------------------------------------------------------
!
! Initialise dummy source; this is needed because ops_statparexp needs source information
! in order to retrieve the correct meteo data from meteo statistics; 
! the source influences the distance class, and wind shear (via plume rise).
disx   = 100 ! (first distance class, i.e. local meteo) 
hbron  = 10
radius = 0
qww    = 0
D_stack = -999.
V_stack = -999.
Ts_stack = -999.
emis_horizontal = .FALSE.

! Initialise summed precipitation for this receptorpoint:
precip=0

! Loop over wind sectors and stability classes:
DO isek = 1, NSEK
  iwd=(isek-1)*360/NSEK ! wind direction [degrees]
  DO istab = 1, NSTAB
!
!   Compute relevant parameters regenk (rain probability), rint (rain intensity) and 
!   percvk (fraction of occurrence of meteo class) for this wind direction sector and stability class
!

    CALL ops_statparexp(varin_meteo, varin_unc, istab, hbron, qww, D_stack, V_stack, Ts_stack, emis_horizontal, iwd, radius, uurtot, astat, trafst, disx, isek, disxx, isec1, vw10, h0,  &
                     &  hum, ol, shear, rc_aer_ms, rc_nh3_ms, rc_no2_ms, temp_C, uster, pcoef, htot, htt, itra, aant, xl, rb, ra_ms_4,   &
                     &  ra_ms_zra, xvglbr, xvghbr, xloc, xl100, rad, rc_so2_ms, coef_space_heating, regenk, buil, rint, percvk, isec_in, error)
    IF (error%haserror) GOTO 9999
!
!   Add contribution to precipitation amount in mm/year (8760 = number of hours in a year)
!   regenk : probability of wet deposition [-] (regen << rain, k << kans = chance)
!   rint   : rain intensity [mm/h]
    precip = precip + regenk*rint*percvk*8760.  
  ENDDO
ENDDO
RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)
RETURN

END SUBROUTINE ops_gen_precip

end module m_ops_gen_precip
