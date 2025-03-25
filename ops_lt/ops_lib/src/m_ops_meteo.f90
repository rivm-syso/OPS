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
SUBROUTINE ops_wv_log_profile(z0, zu, uster, ol, varin_meteo, uz)

! ops_wv_log_profile calculates the wind velocity at a certain height, assuming a logarithmic wind profile.
! 
! Holtslag A.A.M. (1984) Estimates of diabatic wind speed profiles from near surface weather observations. Boundary-Layer Meteorol. 29, 225-250
! Van Ulden A.P. and Holtslag A.A.M. (1985) Estimation of atmospheric boundary layer parameters for diffusion applications. J. Climate Appl. Meteorol. 24, 1196-1207. 

USE Binas, only: pi 
USE m_ops_varin

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_wv_log_profile')

! CONSTANTS
REAL                                             :: K                          ! von Karman constante
PARAMETER    (K = 0.4)

! SUBROUTINE ARGUMENTS - INPUT
REAL,      INTENT(IN)                            :: z0                         ! roughness length (m)
REAL,      INTENT(IN)                            :: zu                         ! height at which the wind velocity has to be compute
REAL,      INTENT(IN)                            :: uster                      ! friction velocity (m)
REAL,      INTENT(IN)                            :: ol                         ! Monin-Obukhov length  (m)
type(Tvarin_meteo), INTENT(IN)                   :: varin_meteo                ! meteo_variables

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: uz                         ! wind velocity (m/s)

! LOCAL VARIABLES
REAL                                             :: phim                       ! stability correction function for momentum
REAL                                             :: y                          ! help variable for computation of phim
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

   ! phim = 2.*LOG((1. + y)/2.) + LOG((1. + y*y)/2.) - 2.*ATAN(y) + pi/2.
   phim = LOG(((1. + y)/2.)**2 * ((1. + y*y)/2.)) - 2.*ATAN(y) + pi/2. ! LOG() operation is expensive, so do it only once

   ! Note: ol -> 0   : y = Inf, phim = 2*LOG((1 + Inf)/2) + LOG((1 + Inf)/2) - 2*ATAN(Inf) + pi/2 = 
   !                                 = 2*LOG((1 + Inf)/2) + LOG((1 + Inf)/2) - pi + pi/2 = Inf  -> uz = uster/K*(ALOG(zu/z0) - phim) = 0
   ! Note: ol -> Inf : y = 1,   phim = 2*LOG(1) + LOG(1) - 2*ATAN(1) + pi/2 = -2*pi/4 + + pi/2 = 0
   ! -> no continuity at L = 0, but continuity at L = Inf (as it should be)
ENDIF

! Compute wind speed (2.3 OPS report)
uz = uster/K*(ALOG(zu/z0) - phim)

! Set lower limit to wind speed:
call ops_meteo_cutoff(varin_meteo%uz_cutoff2, uz)

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
subroutine ops_powerlaw_coeff_obs(vwland,vwlok,vhwind,htower,z0l,ollok,varin_meteo,vwcoef)

use m_ops_varin

implicit none

! input
real,    intent(in) :: vwland       ! wind speed, regional scale [m/s]
real,    intent(in) :: vwlok        ! wind speed, local scale, reference height 10 m [m/s]
real,    intent(in) :: vhwind       ! wind speed at tower height htower (h << high) [m/s]
real,    intent(in) :: htower       ! height of tower used for wind observations at large height
real,    intent(in) :: z0l          ! roughness length, local scale, taking into account the presence of snow [m]
real,    intent(in) :: ollok        ! Monin-Obukhov length, local scale [m]
type(Tvarin_meteo), intent(in) :: varin_meteo ! meteo_variables

! output
real,    intent(out)   :: vwcoef    ! wind speed power law coefficient, local scale 

! local
real :: zra   ! reference height? [m]
real :: o     ! local variable for Monin-Obukhov length [m]
real :: psi10 ! psi @ 10m
real :: psih  ! psi @ zra
real :: vh    ! reference wind speed @ 10m [m/s]

if(vwland.gt.0 .and. vhwind.gt.0) then
    ! RV: vhwind is missing after '96 -> not used currently
    vwcoef = alog(vhwind/vwland)/alog(htower/10.)
    vwcoef = amax1(vwcoef,0.)           !2006/09/07
else
    ! Upper height for wind profile (lower height = 10 m):
    zra = 50.

    ! Cut off for Obukhov length:
    o = ollok
    call ops_meteo_cutoff_obukhov(varin_meteo%ol_cutoff_iopt1, varin_meteo%ol_add_stable0, varin_meteo%ol_max_unstable1, varin_meteo%ol_min_stable3, &
                                  varin_meteo%ol_z0_ratio_cutoff1, varin_meteo%ol_z0_ratio_cutoff1, z0l, o)
    
    ! Stability correction function:
    if (o.lt.0) then
      psi10 = (1.-16*10./o)**0.25-1.
      psih  = (1.-16*zra/o)**0.25-1.
    else
      psi10 = -17.*(1.-exp(-0.29*10./o))
      psih  = -17.*(1.-exp(-0.29*zra/o))
    endif
    
    ! Wind speed at height zra:
    vh = vwlok*(alog(zra/z0l)-psih)/(alog(10/z0l)-psi10)
    
    ! Power law coefficient p from 
    !    vh = vw10 * (zra/10)^p <=> vh/vw10 = (zra/10)^p <=> log(vh/vw10) = p*log(zra/10) <=>
    !    p =  log(vh/vw10) / log(zra/10)
    vwcoef = alog(vh/vwlok)/alog(zra/10.)
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

!----------------------------------------------------------------
subroutine ops_meteo_cutoff_obust(varin_meteo,z0,ol,uster)

! Cut-off of secondary meteo parameters Obukhov length (obu) and friction velocity u* (ust)

use m_ops_varin

! Input:
type(Tvarin_meteo), intent(in) :: varin_meteo    ! input model paramaters for meteo processes
real, intent(in)               :: z0             ! roughness length [m]
                                                 
! Input/output:                                  
real, intent(inout)            :: ol             ! Obukhov length [m]
real, intent(inout)            :: uster          ! friction velocity [m/s]

! Local variables:
integer         :: ol_cutoff_iopt             ! option for cutoff of Obukhov length; see ops_meteo_cutoff_obukhov 
                                              ! 1 -> fixed values for ol_min_stable and ol_max_unstable
                                              ! 2 -> from NNM: L > 100*z0 for stable, L < -5 for unstable
real            :: ol_add_stable              ! extra offset for Obukhov length in stable situations (may be zero) [m]
real            :: ol_max_unstable            ! cut-off value for unstable situations (ol < 0) [m]
real            :: ol_min_stable              ! cut-off value for stable situations (ol > 0) [m]
real            :: ol_z0_ratio_cutoff         ! cut-off value of ol/z0 ratio (from NNM)
integer         :: uster_cutoff_iopt          ! option for cutoff of friction velocity
                                              ! 1 -> fixed values for uster_min
                                              ! 2 -> correction according to the altered Obukhov length (uster2 = uster1*(ol2/ol1)**0.33)
real            :: uster_min                  ! cut-off value for friction velocity [m]
real            :: ol_old                     ! old value of Obukhov length (before cut-off)

! Use cut-off values (only ....1) from varin-meteo:
ol_cutoff_iopt     = varin_meteo%ol_cutoff_iopt1
ol_add_stable      = varin_meteo%ol_add_stable0
ol_max_unstable    = varin_meteo%ol_max_unstable1
ol_min_stable      = varin_meteo%ol_min_stable1
ol_z0_ratio_cutoff = varin_meteo%ol_z0_ratio_cutoff1
uster_cutoff_iopt  = varin_meteo%uster_cutoff_iopt1
uster_min          = varin_meteo%uster_min1

ol_old = ol
call ops_meteo_cutoff_obukhov(ol_cutoff_iopt, ol_add_stable, ol_max_unstable, ol_min_stable, &
                              ol_z0_ratio_cutoff, ol_z0_ratio_cutoff, z0, ol)
if (uster  .gt. 0) call ops_meteo_cutoff_uster(uster_cutoff_iopt, uster_min, ol_old, ol, uster)
   
end subroutine ops_meteo_cutoff_obust

!----------------------------------------------------------------
subroutine ops_meteo_cutoff_obukhov(ol_cutoff_iopt, ol_add_stable, ol_max_unstable, ol_min_stable, &
                                    ol_z0_ratio_cutoff_stable, ol_z0_ratio_cutoff_unstable, z0, ol)

! Cut-off for small values of Obukhov length

use m_ops_varin
use m_commonconst_lib, only: EPS_DELTA 

! Input:
integer,            intent(in)    :: ol_cutoff_iopt             ! option for cutoff of Obukhov length; see ops_meteo_cutoff_obukhov 
                                                                ! 1 -> fixed values for ol_min_stable and ol_max_unstable
                                                                ! 2 -> from NNM   : L > ol_z0_ratio_cutoff_stable*z0 for stable, 
                                                                !                   L < ol_max_unstable for unstable
                                                                !                   NNM: ol_z0_ratio_cutoff_stable = 100, ol_max_unstable = -5 m
                                                                ! 3 -> from OPS-ST: L > ol_z0_ratio_cutoff_stable*z0 for stable, 
                                                                !                   L < -ol_z0_ratio_cutoff_unstable*z0 for unstable
                                                                !                   OPS-ST: ol_z0_ratio_cutoff_stable = ol_z0_ratio_cutoff_unstable*z0 = 100
real,               intent(in)    :: ol_add_stable              ! extra offset for Obukhov length in stable situations (may be zero) [m]
real,               intent(in)    :: ol_max_unstable            ! cutoff value for unstable situations (ol < 0) [m]
real,               intent(in)    :: ol_min_stable              ! cutoff value for stable situations (ol > 0) [m]
real,               intent(in)    :: ol_z0_ratio_cutoff_stable  ! minimal value of ol/z0 ratio (from NNM) for stable conditions
real,               intent(in)    :: ol_z0_ratio_cutoff_unstable! minimal value of ol/z0 ratio (from NNM) for unstable conditions
real,               intent(in)    :: z0                         ! roughness length [m]

! Input/output:
real,               intent(inout) :: ol                         ! Obukhov length [m]

! Extra offset for stable situations:
IF (ol .gt. 0.0) ol = ol + ol_add_stable
    
IF (ol_cutoff_iopt .eq. 1) THEN  
   ! Cutoff with fixed values:
   call ops_meteo_cutoff2(ol_max_unstable, ol_min_stable, ol)
ELSEIF (ol_cutoff_iopt .eq. 2) THEN  
   ! Cut-off at L = ol_z0_ratio_cutoff*z0 for stable, L = ol_max_unstable for unstable
   ! from NNM: ol_z0_ratio_cutoff = 100, ol_max_unstable = -5 m
   call ops_meteo_cutoff2(ol_max_unstable,ol_z0_ratio_cutoff_stable*z0,ol)
ELSEIF (ol_cutoff_iopt .eq. 3) THEN  
   call ops_meteo_cutoff2(-ol_z0_ratio_cutoff_unstable*z0,ol_z0_ratio_cutoff_stable*z0,ol)
ELSE
   ! Range for ol_cutoff_iopt has already been checked.
   write(*,* ) 'unexpected error in ops_meteo_cutoff_obukhov, ol_cutoff_iopt = ',ol_cutoff_iopt
   stop
ENDIF
          
! VERSION WITH DIFFERENT OPTIONS PROGRAMMED HERE, INSTEAD IN CALLS TO THIS ROUTINE
! CAN BE REMOVED IF ALL WORKS WELL.           
!IF (varin_meteo%cutoff_ol_iopt .eq. 1) THEN
!   ! From ops_stab_rek:
!   ! Correction for very stable conditions based on prairie grass data; 
!   ! important -> sensitive for very stable conditions.
!   !           L > 0 -> L = max(ol_min_stable1, L + ol_add_stable); 
!   ! original: L > 0 -> L = max(10, L+5)
!   IF (ol .GT. (0. + EPS_DELTA)) THEN 
!      ol = max(varin_meteo%ol_min_stable1, ol+varin_meteo%ol_add_stable)
!   ENDIF
!   
!   
!ELSEIF (varin_meteo%cutoff_ol_iopt .eq. 2) THEN
!   ! From ops_stab_rek:
!   ! The same correction for Obukhov length and also 
!   !           L < 0 -> L = min(L,ol_max_unstable) for unstable situations
!   ! original: L < 0 -> L = min(L, -7)
!   IF (ol .GT. (0. + EPS_DELTA)) THEN
!      ol = max(varin_meteo%ol_min_stable1, ol + varin_meteo%ol_add_stable)
!   ELSEIF (ol .LT. (0. - EPS_DELTA)) THEN
!      ol = min(ol, varin_meteo%ol_max_unstable1)
!   ENDIF
!   
!ELSEIF (varin_meteo%cutoff_ol_iopt .eq. 3) THEN
!
!   ! From ops_z0corr
!   ! limit L, u* such that 
!   !           ol_max_unstable2 < L < 0              -> L = ol_max_unstable2, 
!   !                          0 < L < ol_min_stable2 -> L = ol_min_stable2
!   ! original: -5 < L < 0 -> L = -5, 
!   !            0 < L < 5 -> L =  5
!
!   IF ((ol .LT. (0.0 - EPS_DELTA)) .AND. (ol .GT. (varin_meteo%ol_max_unstable2 + EPS_DELTA))) THEN
!      ol = varin_meteo%ol_max_unstable2
!   ELSE IF ((ol .GT. (0.0 + EPS_DELTA)) .AND. (ol .LT. (varin_meteo%ol_min_stable2 - EPS_DELTA))) THEN
!      ol = varin_meteo%ol_min_stable2
!   ENDIF
!ELSEIF (varin_meteo%cutoff_ol_iopt .eq. 4) THEN
!   ! from NNM: L < 100*z0 For stable/unstable??
!   IF (ol .GT. 0.0) THEN
!      call ops_meteo_cutoff(varin_meteo%ol_z0_ratio_cutoff*z0,ol)  ! NNM: ol_z0_ratio_cutoff = 100.0
!   ELSE
!      call ops_meteo_cutoff(-varin_meteo%ol_z0_ratio_cutoff*z0,ol) 
!   ENDIF
!ELSE
!   write(*,*) 'Internal programming error in ops_meteo_cutoff_obukhov'
!   write(*,*) 'unkown value for varin_meteo%cutoff_ol_iopt: ',varin_meteo%cutoff_ol_iopt
!   stop
!ENDIF
 
end subroutine ops_meteo_cutoff_obukhov

!----------------------------------------------------------------
subroutine ops_meteo_cutoff_uster(uster_cutoff_iopt, uster_min, ol_old, ol_new, uster)

! Cut-off for small values of friction velocity

use m_ops_varin

! Input:
integer,            intent(in)    :: uster_cutoff_iopt          ! option for cutoff of uster 
                                                                ! 1 -> fixed values for uster_min
                                                                ! 2 -> correction according to the altered Obukhov length (uster2 = uster1*(ol2/ol1)**0.33) 
real,               intent(in)    :: uster_min                  ! fixed cutoff value for uster [m/s]
real,               intent(in)    :: ol_old                     ! Obukhov length before setting it to the cutoff value in ops_meteo_cutoff_obukhov
real,               intent(in)    :: ol_new                     ! Obukhov length after setting it to the cutoff value in ops_meteo_cutoff_obukhov

! Input/output:
real,               intent(inout) :: uster                      ! friction velocity [m/s]

IF (uster_cutoff_iopt .eq. 1) THEN  
   ! Cutoff with fixed values:
   call ops_meteo_cutoff(uster_min, uster)
ELSE
   ! Correction for uster applied when the Obukhov length is cut off. 
   ! L = (-theta_v * uster**3)/(kappa * g * (w'theta_v')_s) (Stull 2000 pp 181)
   ! Assuming that only L and uster change when L changes, uster changes accordingly to the power 1/3
   ! Note that in function OBUK in m_lusthof.f90 the power 3 does not occur, since L is not determined with above formula, but with thetaster
   ! thetaster_SL (surface scale) = -(w'theta_v')_s / uster (Stull 2000 pp 68)
   ! --> L = theta_v * uster**2 / (kappa * g * thetaster)
   uster = uster*(ol_new/ol_old)**0.33
ENDIF
 
end subroutine ops_meteo_cutoff_uster

!-------------------------------------------------------------------------------------------------------------------------------
subroutine ops_meteo_cutoff(var_cutoff,var)

! One sided cutoff of a variable, to prevent (very) small values

! Input:
real, intent(in)    :: var_cutoff                ! cutoff value for variable var

! Input/output:
real, intent(inout) :: var                       ! variable

! Cutoff either for positive values or for negative values.
! --- allowed range   xxx range not allowed 
!
!      var_cutoff    0                            0       var_cutoff
!  -------|xxxxxxxxxx|xxxxxxxxxxxx      xxxxxxxxxx|xxxxxxxxxx|-----------------    



IF (var_cutoff .lt. 0.0) THEN
  ! var = min(var,var_cutoff)  ! this does not work if var = NaN
  IF (var .gt. var_cutoff) var = var_cutoff
ELSE
  ! var = max(var,var_cutoff)
  IF (var .lt. var_cutoff) var = var_cutoff
ENDIF

end subroutine ops_meteo_cutoff

!-------------------------------------------------------------------------------------------------------------------------------
subroutine ops_meteo_cutoff2(var_cutoff1,var_cutoff2,var)

! Two sided cutoff of a variable, to prevent (very) small values

use m_commonconst_lib, only: EPS_DELTA 

! Input:
real, intent(in)    :: var_cutoff1               ! cutoff value <= 0 
real, intent(in)    :: var_cutoff2               ! cutoff value >= 0 

! Input/output:
real, intent(inout) :: var                       ! variable

! if |var| is small -> var = var_cutoff1 for negative values of var and 
!                      var = var_cutoff2 for positive (or zero) values of var.
! --- allowed range   xxx range not allowed 
!
!      var_cutoff1   0           var_cutoff2
!  -------|xxxxxxxxxx|xxxxxxxxxxxxxx|-----------------    

IF (var_cutoff1 .le. 0.0 .and. var_cutoff2 .ge. 0.0) THEN
   IF (var .gt. (0. + EPS_DELTA)) THEN
      var = max(var,var_cutoff2)
   ELSEIF (var .lt. (0. - EPS_DELTA)) THEN
      var = min(var,var_cutoff1)
   ELSE
      write(*,*) 'var = NaN or |var| <= EPS_DELTA in ops_meteo_cutoff2 ',var,var_cutoff1,var_cutoff2  
      stop
   ENDIF
ELSE
   write(*,*) 'unexpected error in ops_meteo_cutoff2 ',var,var_cutoff1,var_cutoff2  
   stop
ENDIF

end subroutine ops_meteo_cutoff2

end module m_ops_meteo

