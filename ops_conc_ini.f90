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
! AUTHOR             : OPS-support
! FIRM/INSTITUTE     : RIVM/LLO/IS
! LANGUAGE           : FORTRAN-77/90
! DESCRIPTION        : Compute initial concentrations due to transport and dispersion; no removal processes yet.
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP-Fortran
! CALLED FUNCTIONS   :
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_conc_ini(gasv, vw10, htt, pcoef, disx, kdeel, qbpri, z0_src, szopp, rond, uster_src, ol_src, istab, iwd, qww,    &
                     &  hbron,dispg, radius, xl, onder, htot, grof, c, sigz, ueff, virty, ccc, error)

USE m_commonconst
USE m_error

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                !
PARAMETER    (ROUTINENAAM = 'ops_conc_ini')

! SUBROUTINE ARGUMENTS - INPUT
LOGICAL,   INTENT(IN)                            :: gasv                       !
REAL*4,    INTENT(IN)                            :: vw10                       !
REAL*4,    INTENT(IN)                            :: htt                        ! plume height, excluding plume descent due to heavy particles [m]
REAL*4,    INTENT(IN)                            :: pcoef                      !
REAL*4,    INTENT(IN)                            :: disx                       !
INTEGER*4, INTENT(IN)                            :: kdeel                      !
REAL*4,    INTENT(IN)                            :: qbpri                      !
REAL*4,    INTENT(IN)                            :: z0_src                     ! roughness length at source; from z0-map [m]
REAL*4,    INTENT(IN)                            :: szopp                      !
INTEGER*4, INTENT(IN)                            :: rond                       !
REAL*4,    INTENT(IN)                            :: uster_src                  !
REAL*4,    INTENT(IN)                            :: ol_src                     !
INTEGER*4, INTENT(IN)                            :: istab                      !
INTEGER*4, INTENT(IN)                            :: iwd                        !
REAL*4,    INTENT(IN)                            :: qww                        !
REAL*4,    INTENT(IN)                            :: hbron                      !
REAL*4,    INTENT(IN)                            :: dispg(NSTAB)               !

! SUBROUTINE ARGUMENTS - I/O
REAL*4,    INTENT(INOUT)                         :: radius                     !
REAL*4,    INTENT(INOUT)                         :: xl                         !
REAL*4,    INTENT(INOUT)                         :: onder                      !
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record


! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: htot                       ! plume height, including plume descent due to heavy particles [m]
                                                                               ! htot = htt - pldaling
REAL*4,    INTENT(OUT)                           :: grof                       !
REAL*4,    INTENT(OUT)                           :: c                          !
REAL*4,    INTENT(OUT)                           :: sigz                       !
REAL*4,    INTENT(OUT)                           :: ueff                       ! wind speed at effective transport height heff;
                                                                               ! for short distances heff = plume height;
                                                                               ! for large distances heff = 1/2 mixing height;
                                                                               ! heff is interpolated for intermediate distances.

REAL*4,    INTENT(OUT)                           :: virty                      !
REAL*4,    INTENT(OUT)                           :: ccc                        !

! LOCAL VARIABLES
REAL*4                                           :: ff                         !
REAL*4                                           :: pldaling                   !

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    !
sccsida = '%W%:%E%'//char(0)
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
   pldaling = disx/ff*STOKES(kdeel)  ! pl << plume, "daling" = descent

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
!
! Compute concentration for this distance and meteo class
!
CALL ops_conltexp(rond, ol_src, qbpri, szopp, uster_src, z0_src, htt, onder, vw10, pcoef, istab, disx, grof, iwd, qww, hbron,   &
               &  dispg, radius, htot, ccc, sigz, ueff, xl, virty, error)
!
! Correct for plume below or above the mixing layer; mass above the mixing layer does not contribute to
! concentration at surface.
! c  : concentration at z = 0 m (without the part above the mixing layer); is needed for e.g. dry deposition
! ccc: concentration at z = 0 m (including part above mixing layer); is needed for e.g. wet deposition.
!
c = ccc*onder

RETURN

END SUBROUTINE ops_conc_ini
