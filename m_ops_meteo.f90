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
module m_ops_meteo

! module for meteo processes
!
! ops_wv_log_profile          : compute wind velocity at a certain height according to a log-profile + stability correction
! ops_wv_powerlaw_metstat     : compute wind velocity at a certain height according to a power law, with reference wind speed 
!                               and power law coefficient from meteo statistics
! ops_powerlaw_coeff_obs      : compute powerlaw coefficient from observations
! ops_powerlaw_coeff_metstat  ! get powerlaw coefficient from meteo statistics (METPRO)

implicit none

contains

!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_wv_log_profile(z0, zu, uster, ol, uz)

! ops_wv_log_profile calculates the wind velocity at a certain height, assuming a logarithmic wind profile.
! 
! Holtslag A.A.M. (1984) Estimates of diabatic wind speed profiles from near surface weather observations. Boundary-Layer Meteorol. 29, 225-250
! Van Ulden A.P. and Holtslag A.A.M. (1985) Estimation of atmospheric boundary layer parameters for diffusion applications. J. Climate Appl. Meteorol. 24, 1196-1207. 

USE Binas, only: pi 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_wv_log_profile')

! CONSTANTS
REAL*4                                           :: K                          ! von Karman constante
PARAMETER    (K = 0.4)

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: z0                         ! roughness length (m)
REAL*4,    INTENT(IN)                            :: zu                         ! height at which the wind velocity has to be compute
REAL*4,    INTENT(IN)                            :: uster                      ! friction velocity (m)
REAL*4,    INTENT(IN)                            :: ol                         ! Monin-Obukhov length  (m)

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: uz                         ! wind velocity (m/s)

! LOCAL VARIABLES
REAL*4                                           :: phim                       ! stability correction function for momentum
REAL*4                                           :: y                          ! help variable for computation of phim

!-------------------------------------------------------------------------------------------------------------------------------

! Stability function:
IF (ol .GT. 0.0) THEN  
 
   !---------------------------
   ! L > 0, stable atmosphere
   !---------------------------

   phim = -17.*(1. - EXP( -0.29*zu/ol)) + 17.*(1. - EXP( -0.29*z0/ol))

   ! Note: ol -> 0   : phim = -17*(1 - EXP( -Inf)) + 17*(1 - EXP( -Inf)) = -17*1 + 17*1 = 0
   ! Note: ol -> Inf : phim = -17*(1 - EXP(0)) + 17*(1 - EXP(0)) = 0

ELSE

   !--------------------------------------
   ! L <= 0, unstable/neutral atmosphere
   !--------------------------------------

   y    = (1. - 15.*zu/ol)**0.25
   phim = 2.*LOG((1. + y)/2.) + LOG((1. + y*y)/2.) - 2.*ATAN(y) + pi/2.

   ! Note: ol -> 0   : y = Inf, phim = 2*LOG((1 + Inf)/2) + LOG((1 + Inf)/2) - 2*ATAN(Inf) + pi/2 = 
   !                                 = 2*LOG((1 + Inf)/2) + LOG((1 + Inf)/2) - pi + pi/2 = Inf  -> uz = uster/K*(ALOG(zu/z0) - phim) = 0
   ! Note: ol -> Inf : y = 1,   phim = 2*LOG(1) + LOG(1) - 2*ATAN(1) + pi/2 = -2*pi/4 + + pi/2 = 0
   ! -> no continuity at L = 0, but continuity at L = Inf (as it should be)
ENDIF

! Compute wind speed (2.3 OPS report)
uz = uster/K*(ALOG(zu/z0) - phim)

! Set lower limit for wind at 0.75 m/s
IF (uz .LT. 0.75) THEN
  uz = 0.75
ENDIF                                                                          ! 950310

RETURN
END SUBROUTINE ops_wv_log_profile

!------------------------------------------------------------------------------------------------
subroutine ops_wv_powerlaw_metstat(istab,isek,astat,z,uz,vw10,pcoef)

! Compute wind profile based on power law, with reference wind speed and power law coefficient 
! from meteo statistics. Note that below the reference height of zmet_u = 10 m, 
! the wind profile is assumed to be constant: uz(z < zmet_u) = uz(zmet_u).

USE m_commonconst_lib, only: NTRAJ, NCOMP, NSTAB, NSEK, EPS_DELTA, zmet_u

implicit none

! Input:
integer, intent(in)  :: istab    ! index of stability class
integer, intent(in)  :: isek     ! index of wind sector
real   , intent(in)  :: astat(NTRAJ, NCOMP, NSTAB, NSEK) ! statistical meteo parameters
real   , intent(in)  :: z        ! heigth where to compute wind velocity [m]

! Output:
real   , intent(out) :: uz       ! wind velocity at height z [m/s]
real   , intent(out) :: vw10     ! wind velocity at reference height 10 m [m/s]
real   , intent(out) :: pcoef    ! coefficient in wind speed power law

! Local:
real :: VWREP(NSTAB)             ! representative (long term average) wind speed per stability class
DATA VWREP /2.6, 3.8, 4.0, 6.9, 1.4, 2.5/

! Get wind speed [m/s] at reference height zmet_u = 10 m, from meteo statistics; 
! use VWREP if we have a zero wind speed from meteo statistics:
IF (ABS(astat(1, 3, istab, isek)) .LE. EPS_DELTA) THEN
   vw10 = VWREP(istab)
ELSE
   vw10 = astat(1, 3, istab, isek)
ENDIF

! Get coefficient in wind speed power law:
call ops_powerlaw_coeff_metstat(astat, istab, isek, pcoef)

! use power law to determine wind velocity:
! (zmet_u = 10.0 = reference height for wind speed [m] in m_commonconst_lib)
IF (z .GT. zmet_u) THEN
   uz = vw10*(z/zmet_u)**pcoef
ELSE
   uz = vw10
ENDIF

end subroutine ops_wv_powerlaw_metstat

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! subroutine powerlaw
!
! calculate power law coeff. from tower observations and 10m wind
! this coeff. is used to describe the vertical wind speed profile
! if no tower observations, derive coeff. from relations given by
! van Ulden and Holtslag (1985)
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
subroutine ops_powerlaw_coeff_obs(vwland,vwlok,vhwind,htower,z0l,ollok,vwcoef)

implicit none

! input
real,    intent(in) :: vwland       ! wind speed, regional scale [m/s]
real,    intent(in) :: vwlok        ! wind speed, local scale [m/s]
real,    intent(in) :: vhwind       ! wind speed at tower height htower (h << high) [m/s]
real,    intent(in) :: htower       ! height of tower used for wind observations at large height
real,    intent(in) :: z0l          ! roughness length, local scale, taking into account the presence of snow [m]
real,    intent(in) :: ollok        ! Monin-Obukhov length, local scale [m]

! output
real,    intent(out)   :: vwcoef    ! wind speed power law coefficient, local scale 

! local
real :: zra   ! reference height? [m]
real :: o     ! local variable for Monin-Obukhov length [m]
real :: psi10 ! psi @ 10m
real :: psih  ! psi @ zra
real :: vh    ! reference wind speed @ 10m [m/s]

if(vwland.gt.0.and.vhwind.gt.0) then
    ! RV: vhwind is missing after '96 -> not used currently
    vwcoef=alog(vhwind/vwland)/alog(htower/10.)
    vwcoef=amax1(vwcoef,0.)           !2006/09/07
else
    zra=50.
    o=ollok
    if(o.gt.0.and.o.lt.20.) o=20.  
    if(o.lt.0.and.o.gt.-5.) o=-5.  
    if(o.lt.0) then
      psi10=(1.-16*10./o)**0.25-1.
      psih=(1.-16*zra/o)**0.25-1.
    else
      psi10=-17.*(1.-exp(-0.29*10./o))
      psih=-17.*(1.-exp(-0.29*zra/o))
    endif
    vh=vwlok*(alog(zra/z0l)-psih)/(alog(10/z0l)-psi10)
    vwcoef=alog(vh/vwlok)/alog(zra/10.)
endif
   
end subroutine ops_powerlaw_coeff_obs

!----------------------------------------------------------------
! Subroutine ops_powerlaw_coeff_metstat
! Get coefficient in wind speed power law from meteo statistics.
!----------------------------------------------------------------
subroutine ops_powerlaw_coeff_metstat(astat, istab, isek, pcoef)

USE m_commonconst_lib, only: NTRAJ, NCOMP, NSTAB, NSEK

implicit none

! Input:
integer, intent(in)  :: istab    ! index of stability class
integer, intent(in)  :: isek     ! index of wind sector
real   , intent(in)  :: astat(NTRAJ, NCOMP, NSTAB, NSEK) ! statistical meteo parameters

! Output:
real   , intent(out) :: pcoef ! power law coefficient

pcoef = astat(1, 15, istab, isek)

end subroutine ops_powerlaw_coeff_metstat

end module m_ops_meteo

