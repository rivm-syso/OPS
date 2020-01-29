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
module Binas

  implicit none

  public

  !
  !ProTeX: 1.14-AJS
  !
  !BOI
  !
  ! !TITLE:        Binas - geometrical and physical constants
  ! !AUTHORS:      Arjo Segers
  ! !AFFILIATION:  KNMI
  ! !DATE:         \today
  !
  ! !INTRODUCTION: Introduction
  !
  !   'BINAS' is the name an in The Netherlands common used table-book
  !   with scientific constants and formulae.
  !
  !
  ! !INTRODUCTION: Constants
  !
  !BOC

  ! ---------------------------------------------------------------
  ! gonio
  ! ---------------------------------------------------------------

  ! defintions for pi :
  !  o old definition:
  !real, parameter         ::  pi    = 3.1415927
  !  o EMOS definition (emos/interpolation/strlat.F, parameter PPI)
  real, parameter         ::  pi = 3.14159265358979

  ! two pi :
  real, parameter         ::  twopi = 2*pi

  ! factors to convert to radians from degrees and the otrher way around;
  ! alpha_deg = alpha_rad*rad2deg
  ! alpha_rad = alpha_deg*deg2rad
  real, parameter         ::  deg2rad = pi/180.0     ! rad/deg
  real, parameter         ::  rad2deg = 180.0/pi     ! deg/rad


  ! ---------------------------------------------------------------
  ! earth
  ! ---------------------------------------------------------------

  ! Radius of earth as used in EMOS library (ECMWF model),
  ! see for example "jvod2uv.F"
  ! NOTE: the value 6.375e6 was used in TM !
  real, parameter         ::  ae = 6.371e6     ! m

  ! acceleration of gravity:
  !real, parameter         ::  grav = 9.81       ! m/s2
  real, parameter         ::  grav = 9.80665    ! m/s2

  ! Earth's angular speed of rotation
  !  Omega =  2 * pi * (365.25/364.25) / (86400.0)
  real, parameter         ::  Omega = 7.292e-5    ! rad/s


  ! ---------------------------------------------------------------
  ! molecules, mols, etc
  ! ---------------------------------------------------------------

  ! Avogadro number
  real, parameter        ::  Avog = 6.02205e23      ! mlc/mol

  ! Dobson units:
  real,parameter         ::  Dobs = 2.68668e16    ! (mlc/cm2) / DU


  !
  ! molar weights of components
  !

  ! naming convention:
  !  o old names 'xm***' are in g/mol
  !  o new names 'xm_***' are in kg/mol
  !

  ! atomic weights:
  real, parameter        ::  xm_H     =    1.00790e-3     ! kg/mol
  real, parameter        ::  xm_N     =   14.00670e-3     ! kg/mol
  real, parameter        ::  xm_C     =   12.01115e-3     ! kg/mol
  real, parameter        ::  xm_S     =   32.06400e-3     ! kg/mol
  real, parameter        ::  xm_O     =   15.99940e-3     ! kg/mol
  real, parameter        ::  xm_F     =   18.99840e-3     ! kg/mol
  real, parameter        ::  xm_Na    =   22.98977e-3     ! kg/mol
  real, parameter        ::  xm_Cl    =   35.45300e-3     ! kg/mol
  real, parameter        ::  xm_Rn222 =  222.0e-3         ! kg/mol
  real, parameter        ::  xm_Pb210 =  210.0e-3         ! kg/mol

  ! molecule weights:
  real, parameter        ::  xm_h2o   = xm_H * 2 + xm_O    ! kg/mol
  real, parameter        ::  xm_o3    = xm_O * 3         ! kg/mol
  real, parameter        ::  xm_N2O5  = xm_N * 2 + xm_O * 5    ! kg/mol
  real, parameter        ::  xm_HNO3  = xm_H + xm_N + xm_O * 3 ! kg/mol
  real, parameter        ::  xm_NH4   = xm_N + xm_O * 4         ! kg/mol
  real, parameter        ::  xm_SO4   = xm_S + xm_O * 4         ! kg/mol
  real, parameter        ::  xm_NO3   = xm_N + xm_O * 3         ! kg/mol

  ! mass of air
  real, parameter        ::  xm_air   =  28.964e-3        ! kg/mol
  real, parameter        ::  xmair    =  28.94            ! g/mol; old name!

  ! dummy weight, used for complex molecules:
  real, parameter        ::  xm_dummy =  1000.0e-3   ! kg/mol

  ! * seasalt

  ! sesalt composition:
  ! (Seinfeld and Pandis, "Atmospheric Chemistry and Physics",
  !  table 7.8 "Composition of Sea-Salt", p. 444)
  real, parameter        ::  massfrac_Cl_in_seasalt  = 0.5504  ! (kg Cl )/(kg seasalt)
  real, parameter        ::  massfrac_Na_in_seasalt  = 0.3061  ! (kg Na )/(kg seasalt)
  real, parameter        ::  massfrac_SO4_in_seasalt = 0.0768  ! (kg SO4)/(kg seasalt)

  ! other numbers (wikipedia ?)
  real, parameter         ::   xm_seasalt =  74.947e-3     ! kg/mol : NaCl, SO4, ..
  real, parameter         ::  rho_seasalt = 2.2e3 ! kg/m3

  ! * amonium sulphate

  real, parameter         ::   xm_NH4HSO4  =  xm_NH4 + xm_H + xm_SO4  ! kg/mol
  real, parameter         ::  rho_NH4HSO4a = 1.8e3  ! kg/m3


  !                  mlc/mol
  ! [cdob] = ------------------------ =   DU / (kg/m2)
  !          kg/mol cm2/m2 mlc/cm2/DU
  !

  real, parameter :: cdob_o3 = Avog / ( xm_o3 * 1.0e4 * Dobs )  ! DU/(kg/m2)

  ! ---------------------------------------------------------------
  ! gas
  ! ---------------------------------------------------------------

  ! gas constant
  real, parameter        ::  Rgas = 8.3144     ! J/mol/K

  ! gas constant for dry air
  !real, parameter        ::  rgas_x  = 287.05
  ! NEW:
  !   Rgas_air = Rgas / xmair = 287.0598
  real, parameter        ::  Rgas_air = Rgas / xm_air    ! J/kg/K

  ! water vapour
  !real,parameter         ::  rgasv = 461.51
  real, parameter        ::  Rgas_h2o = Rgas / xm_h2o   ! J/kg/K

  ! standard pressure
  real, parameter        ::  p0 = 1.0e5    ! Pa
  !real, parameter        ::  p0 = 1.01325e5    ! Pa  <-- suggestion Bram Bregman

  ! global mean pressure:
  real,parameter         ::  p_global = 98500.0   ! Pa

  ! specific heat of dry air at constant pressure
  !real, parameter        ::  cp0 = 1004.0           ! J/kg/K
  real, parameter        ::  cp_air = 1004.0           ! J/kg/K

  ! Latent heat of evaporation
  real, parameter        ::  lvap = 2.5e6     !  [J kg-1]

  ! Latent heat of condensation at 0 deg Celcius
  ! (heat (J) necesarry to evaporate 1 kg of water)
  real, parameter       ::  Lc = 22.6e5           ! J/kg

  ! kappa = R/cp = 2/7
  real, parameter        ::  kappa = 2.0/7.0
  ! 'kapa' is probably 'kappa' ....
  !real, parameter        ::  kapa = 0.286

  ! Von Karman constant (dry_dep)
  real, parameter        ::  vkarman = 0.4

  ! Boltzmann constant:
  real, parameter       ::  kbolz = 1.38066e-23    ! J/K

  ! ---------------------------------------------------------------
  ! virtual temperature :  Tv = T * ( 1 + eps1*q )
  ! ---------------------------------------------------------------

  real, parameter        ::  eps  = Rgas_air / Rgas_h2o
  real, parameter        ::  eps1 = ( 1.0 - eps )/eps


  ! ---------------------------------------------------------------
  ! other
  ! ---------------------------------------------------------------

  ! melting point
  real, parameter        ::  T0 = 273.16    ! K

  ! Rv/Rd
  real, parameter        ::  gamma = 6.5e-3

  ! density of pure water at 0 deg C
  real, parameter                  ::  rol   = 1000.         ! kg/m^3

  ! density of ice water at 0 deg C
  real, parameter                  ::  roi   = 917.          ! kg/m^3

  ! density of pure water at 15 deg C
  real, parameter                  ::  rho_water = 999.0     ! kg/m^3

  ! density of dry air at 20 oC and 1013.25 hPa :
  real, parameter                  ::  rho_dry_air_20C_1013hPa = 1.2041 ! kg/m3

  ! Planck times velocity of light
  real, parameter                  ::  hc = 6.626176e-34 * 2.997924580e8  ! Jm


  ! ---------------------------------------------------------------
  ! end
  ! ---------------------------------------------------------------

  !EOC

end module Binas
