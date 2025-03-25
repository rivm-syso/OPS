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
!                               copyright by
!   National Institute of Public Health and Environment
!             Laboratory for Air Research (RIVM/LLO)
!                              The Netherlands
!   No part of this software may be used, copied or distributed without permission of RIVM/LLO (2002)
!
! SUBROUTINE
! NAME               : %M%
! SCCS(SOURCE)       : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH -SEQUENCE   : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : HvJ/Franka Loeve (Cap Volmac)
! FIRM/INSTITUTE     : RIVM/LLO/IS
! LANGUAGE           : FORTRAN-77/90
! DESCRIPTION        : Compute initial (undepleted) concentrations due to transport and dispersion; no removal processes yet.
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP-Fortran
! CALLED FUNCTIONS   :
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_conc_ini

implicit none

contains

SUBROUTINE ops_conc_ini(varin_meteo, varin_unc, gasv, vw10, htt, pcoef, disx, disxx, zm, kdeel, qbpri, z0_src, sigz0, road_disp, lroad_corr, rond, uster_src, ol_src, ircp, istab, iwd, qww,    &
                     &  hbron,dispg, radius, xl, onder, htot, grof, c0_undepl_total, c0_undepl_mix, c_zrcp_undepl_mix, sigz, ueff, virty, error)
                     
use m_ops_varin
use m_commonconst_lt
use m_error
use m_ops_conltexp

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_conc_ini')

! SUBROUTINE ARGUMENTS - INPUT
TYPE(Tvarin_meteo), INTENT(IN)                   :: varin_meteo                ! input variables for meteo
type(Tvarin_unc), intent(in)                     :: varin_unc
LOGICAL,   INTENT(IN)                            :: gasv                       ! TRUE if component is a gas
REAL,      INTENT(IN)                            :: vw10                       ! wind speed at 10 m height [m/s]
REAL,      INTENT(IN)                            :: htt                        ! plume height at source, including plume rise [m]
REAL,      INTENT(IN)                            :: pcoef                      ! coefficient in wind speed power law
REAL,      INTENT(IN)                            :: disx                       ! linear distance between source and receptor [m] (here only used for debug write statement)
REAL,      INTENT(IN)                            :: disxx                      ! effective travel distance between source and receptor [m] 
REAL,      INTENT(IN)                            :: zm                         ! z-coordinate of receptor points (m)
INTEGER,   INTENT(IN)                            :: kdeel                      ! index of particle class
REAL,      INTENT(IN)                            :: qbpri                      ! source strength current source (for current particle class) [g/s]
REAL,      INTENT(IN)                            :: z0_src                     ! roughness length at source; from z0-map [m] 
REAL,      INTENT(IN)                            :: sigz0                      ! initial vertical dispersion length [m]
LOGICAL,   INTENT(IN)                            :: road_disp                  ! TRUE if user wants OPS to interpret sigz0 as SRM2 initial spread
LOGICAL,   INTENT(IN)                            :: lroad_corr                 ! TRUE if current emission category is a road and linear distance is within dist_road_corr
INTEGER,   INTENT(IN)                            :: rond                       ! 
REAL,      INTENT(IN)                            :: uster_src                  ! 
REAL,      INTENT(IN)                            :: ol_src                     ! 
INTEGER,   INTENT(IN)                            :: ircp                       ! index of receptorpoint (here only used for debug write statement)
INTEGER,   INTENT(IN)                            :: istab                      ! index of stability class 
INTEGER,   INTENT(IN)                            :: iwd                        ! wind direction if wind is from source to receptor (degrees)
REAL,      INTENT(IN)                            :: qww                        ! 
REAL,      INTENT(IN)                            :: hbron                      ! emission height at source (stack height), without plume rise [m]
REAL,      INTENT(IN)                            :: dispg                      ! coefficient for vertical dispersion coefficient sigma_z; sigma_z = dispg*x^disp [-]

! SUBROUTINE ARGUMENTS - I/O
REAL,      INTENT(INOUT)                         :: radius                     ! 
REAL,      INTENT(INOUT)                         :: xl                         ! maximal mixing height over transport distance [m] (extrapolated when x > 1000km, largest distance category in meteo statistics; xl = 2.*htt when xl < htt)
REAL,      INTENT(INOUT)                         :: onder                      ! fraction of emission below mixing height [-]
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record 


! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: htot                       ! plume height, including plume descent due to heavy particles [m]
                                                                               ! htot = htt - pldaling 
REAL,      INTENT(OUT)                           :: grof                       ! = 1 -> coarse particles
REAL,      INTENT(OUT)                           :: c0_undepl_total            ! undepleted concentration at z = 0 m (including part of plume above mixing layer); is needed for secondary species
REAL,      INTENT(OUT)                           :: c0_undepl_mix              ! undepleted concentration at z = 0 m (only due to part of plume inside the mixing layer)
REAL,      INTENT(OUT)                           :: c_zrcp_undepl_mix          ! undepleted concentration at z = zrcp (only due to part of plume inside the mixing layer)
REAL,      INTENT(OUT)                           :: sigz                       ! vertical dispersion length [m]
REAL,      INTENT(OUT)                           :: ueff                       ! wind speed at effective transport height heff; 
                                                                               ! for short distances heff = plume height;
                                                                               ! for large distances heff = 1/2 mixing height;
                                                                               ! heff is interpolated for intermediate distances.
REAL,      INTENT(OUT)                           :: virty                      ! distance virtual point source - centre area source [m]


! LOCAL VARIABLES
REAL                                             :: ff                         ! 
REAL                                             :: pldaling                   ! 
REAL                                             :: c_zrcp_undepl_total        ! undepleted concentration at z = zrcp (including part of plume above mixing layer)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Correct plume height for the effect of heavy particles.  
! hbron  : emission height at source (stack height), without plume rise [m]
! htt    : plume height at source, including plume rise [m]
! htot   : plume height at receptor, including plume descent due to heavy particles; htot = htt - pldaling [m]
!
!  htt \
!      | \
!      |    \
!      |        \
!      |            \
!      |                \
!      |                    \
!      |                         \
! hbron|                              \
!     | |                                   \ htot
!     | |        
!     | |        
!     | |        
!     | |        
!     | |        
!     | |        
!     | |        
!     | |        
! -------------------------------------------------
!   source                                 receptor
!
! "grof"  = coarse, indicates coarse particles
! "onder" = below, fraction of plume below the mixing height
!
! STOKES = sedimentation velocity [m/s], according to Stokes law:
!
!      (rho_p - rho_air) D_p**2 g
! vt = -------------------------- ,
!               18 mu
!
! with vt     : sedimentation or terminal settling velocity [m/s]
!      rho_p  : density of particle [kg/m3] ~ 1000 kg/m3
!      rho_air: density of air [kg/m3]  = 1.293 kg/m3 (0 C), 1.205 kg/m3 (20 C)
!      D_p    : diameter of particle [m] 
!      g      : accelaration of gravity = 9.807 m/s2
!      mu     : viscosity of air = 1.81e-5 [Pa s = N s/m2 = kg /(s m)]
!
grof = 0.

IF (.NOT.gasv) THEN

   ! Compute wind speed at htt/2 (power law)
   ff = vw10*(htt/20.)**pcoef

   ! Plume descent between source and receptor = travel_time * sedimentation_velocity =
   ! (travel_distance/wind_speed) * sedimentation velocity
   pldaling = disxx/ff*STOKES(kdeel)  ! pl << plume, "daling" = descent

! Heavy particles if sedimentation velocity > 2 cm/s: 
  IF (STOKES(kdeel) .GT. (.02 + EPS_DELTA)) THEN
        grof = 1.
!
!       Heavy plume descends:
!
        htot = htt - pldaling
!
!       Heavy particles are not influenced by inversion;
!       work around -> increase mixing height such that plume is inside mixing layer
!       Note that xl is reset to its original value in loop over kdeel in ops_reken
!
        IF (xl .LT. (htt - EPS_DELTA)) THEN
             xl = 2.*htt
        ENDIF
        onder = 1.
   ELSE
      ! No heavy particles, no correction
       htot = htt
   ENDIF
ELSE
  ! Gaseous component; no correction
   htot = htt
ENDIF

! Compute concentration for this distance and meteo class, for plume below and/or above the mixing layer:
CALL ops_conltexp(varin_meteo, varin_unc, zm, rond, ol_src, qbpri, sigz0, road_disp, lroad_corr, uster_src, z0_src, htt, onder, vw10, pcoef, ircp, istab, disx, disxx, grof, iwd, qww, hbron,   &
               &  dispg, radius, htot, c0_undepl_total, c_zrcp_undepl_total, sigz, ueff, xl, virty, error)
!
! Mass above the mixing layer does not contribute to concentration at surface, so compute concentration 
! only due to emission in mixing layer:
c0_undepl_mix     = c0_undepl_total*onder
c_zrcp_undepl_mix = c_zrcp_undepl_total*onder

RETURN

END SUBROUTINE ops_conc_ini

end module m_ops_conc_ini
