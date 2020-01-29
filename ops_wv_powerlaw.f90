!-------------------------------------------------------------------------------------------------------------------------------
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
!                       Copyright (C) 2002 by
!   National Institute of Public Health and Environment
!           Laboratory for Air Research (RIVM/LLO)
!                      The Netherlands
!-------------------------------------------------------------------------------------------------------------------------------
subroutine ops_wv_powerlaw(istab,isek,astat,z,uz,vw10,pcoef)

! Compute wind profile based on power law. Note that below the reference height of 10 m,
! the wind profile is assumed to be constant: uz(z < 10) = uz(z = 10).

USE m_commonconst

implicit none

! Input:
integer, intent(in)  :: istab    ! index of stability class
integer, intent(in)  :: isek     ! index of wind sector
real   , intent(in)  :: astat(NTRAJ, NCOMP, NSTAB, NSEK) ! statistical meteo parameters
real   , intent(in)  :: z        ! heigth where to compute wind velocity [m]

! Output:
real   , intent(out) :: uz       ! wind velocity at height z [m/s]
real   , intent(out) :: vw10     ! wind velocity at 10 m heigth [m/s]
real   , intent(out) :: pcoef    ! coefficient in wind speed power law

! Local:
real, parameter :: zref = 10.0   ! reference height for wind speed [m]
real            :: VWREP(NSTAB)  ! representative (long term average) wind speed per stability class
DATA VWREP /2.6, 3.8, 4.0, 6.9, 1.4, 2.5/

!-------------------------------------------------------------------------------------------------------------------------------
! Get wind speed [m/s] at 10 m height from meteo statistics.
! Use VWREP if we have a zero wind speed from meteo statistics.
!
IF (ABS(astat(1, 3, istab, isek)) .LE. EPS_DELTA) THEN
   vw10 = VWREP(istab)
ELSE
   vw10 = astat(1, 3, istab, isek)
ENDIF

! Get coefficient in wind speed power law
pcoef = astat(1, 15, istab, isek)

! use power law to determine wind velocity:
IF (z .GT. zref) THEN
   uz = vw10*(z/zref)**pcoef
ELSE
   uz = vw10
ENDIF

end subroutine ops_wv_powerlaw
