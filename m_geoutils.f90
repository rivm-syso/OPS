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
! MODULE             : geoutils
! IMPLEMENTS         : - amc2geo: conversion of RDM to geographical lon-lat coordinates
!                    : - geo2amc: conversion of geographical lon-lat coordinates to RDM coordinates
! NAME               : %M%
! SCCS(SOURCE)       : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH -SEQUENCE   : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : OPS-support 
! FIRM/INSTITUTE     : RIVM LLO
! LANGUAGE           : FORTRAN-90
! DESCRIPTION        : This module contains geographical utilities.
!                    : - amcgeo: conversion of RDM to geographical lon-lat coordinates
!                    : - flrs: conversion of geographical lon-lat coordinates to RDM coordinates
!                      RDM coordinates are based on a km grid over the Netherlands, centred at Amersfoort; 
!                      RDM coordinates are also called "Amersfoortse coordinaten". RDM stands for RijksDriehoeksMeting,
!                      since this grid is determined by triangulation (driehoek = triangle) measurements (meting = meeasurements)
!                      of the Netherlands government (Rijk ~ government).
! USAGE              :
! EXIT CODES         :
! REFERENCE          :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP-Fortran
! CONTAINS           : amc2geo, geo2amc, amc2lam, geo2lam
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------
MODULE m_geoutils

IMPLICIT NONE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : amc2geo
! PURPOSE     : conversion of RDM to geographical lon-lat coordinates
! DESCRIPTION : Given input RDM coordinates (x,y), amcgeo uses an iterative method to compute the geographical coordinates (gl,gb).
!               Given an initial guess for (geol,geob), the corresponding RDM coordinates (x0,y0) are computed and compared with the 
!               input coordinates (amcx,amcy). In a next step, (geol,geob) are adjusted as function of the difference (dx,dy) = (xi-x,yi-y).
!               This iterative procedure continues until (dx,dy) is samller than a specified threshold (difx,dify).
!               limits: ca. 10000 km oost (y < 3000 km)  100 lon 16 lat
!                       ca.  8000 km oost (y < 4500 km)  105 lon 35 lat
!                       ca. 10000 km west (y < 4000 km)  -96 lon 17 lat
!                       ca.  6000 km west (y < 5000 km)  -95 lon 47 lat
!
! INPUTS      : amcx     (real), RDM x-coordinate [km]
!               amcy     (real), RDM y-coordinate [km]
! OUTPUTS     : geol     (real), longitude [degrees]
!               geob     (real), latitude  [degrees]
!               "geo" << geographical coordinates; "l" << lengtegraad = longitude, "b" << breedtegraad = latitude
!-------------------------------------------------------------------------------------------------------------------------------

INTERFACE amc2geo
  MODULE PROCEDURE amc2geo
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : geo2amc
! PURPOSE     : Convert greographical lon-lat coordinates to RDM coordinates
! INPUTS      : geob    (real), latitude, phi (degrees)
!               geob    (real), longitude, lambda (degrees)
! OUTPUTS     : amcx    (real), RDM x-coordinate
!               amcx    (real), RDM y-coordinate
!-------------------------------------------------------------------------------------------------------------------------------

INTERFACE geo2amc
  MODULE PROCEDURE geo2amc
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : amc2lam
! PURPOSE     : Berekenen van de lambert azimuthal equal area coordinaten (x,y) in km. uit de topografische (amersfoortse) coordinaten.
! INPUTS      : amcx   (real), x-coordinaat, Amersfoorts
!               amcy   (real), y-coordinaat, Amersfoorts
! OUTPUTS     : lamx   (real), x-coordinaat, Lambert azimuthaal
!             : lamy   (real), y-coordinaat, Lambert azimuthaal
!-------------------------------------------------------------------------------------------------------------------------------

INTERFACE amc2lam
  MODULE PROCEDURE amc2lam
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : geo2lam
! PURPOSE     : Berekenen van de lambert azimuthal equal area coordinaten (x,y) in km. uit de geografische coordinaten.
! INPUTS      : geob   (real), breedtegraad, phi (dec.)
!               geol   (real), lengtegraad, labda (dec.)
! OUTPUTS     : lamx   (real), x-coordinaat, Lambert azimuthaal
!             : lamy   (real), y-coordinaat, Lambert azimuthaal
!-------------------------------------------------------------------------------------------------------------------------------

INTERFACE geo2lam
  MODULE PROCEDURE geo2lam
END INTERFACE

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : amc2geo
! CALLED FUNCTIONS   : geo2amc
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE amc2geo(amcx, amcy, geol, geob)

!DEC$ ATTRIBUTES DLLEXPORT:: amc2geo

USE m_commonconst                                                              ! EPS_DELTA only

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'amc2geo')

! SUBROUTINE ARGUMENTS - INPUT
real,      INTENT(IN)                            :: amcx                       ! RDM x-coordinate (km)
real,      INTENT(IN)                            :: amcy                       ! RDM y-coordinate (km)

! SUBROUTINE ARGUMENTS - OUTPUT
real,      INTENT(OUT)                           :: geol                       ! longitude [degrees]
real,      INTENT(OUT)                           :: geob                       ! latitude  [degrees]

! LOCAL VARIABLES
INTEGER*4                                        :: tel                        ! iteration index
real                                             :: difx                       ! threshold value for dx
real                                             :: dify                       ! threshold value for dy
real                                             :: dx                         ! x - x0
real                                             :: dy                         ! y - y0
real                                             :: amcx0                      ! RDM x-coordinate that corresponds with (gb,gl)
real                                             :: amcy0                      ! RDM y-coordinate that corresponds with (gb,gl)

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------

! Initialise lon-lat coordinates (location near Utrecht, centre of NL) and iteration index
geob   = 52.
geol   = 5.
tel  = 0

! Set threshold values for dx and dy (mix of relative and absolute threshold)
IF (ABS(amcy) .GT. (1500. + EPS_DELTA)) THEN
   dify = amcy*1.e-3
ELSE
   dify = 1.e-2
ENDIF
IF (ABS(amcx) .GT. (1500. + EPS_DELTA)) THEN
   difx = amcx*1.e-3
ELSE
   difx = 1.e-2
ENDIF

!-----------------------------
! START ITERATION
!-----------------------------
50 CONTINUE

   ! Compute (x0,y0) = RDM coordinates that correspond with (gb,gl) 
   CALL geo2amc(geob, geol, amcx0, amcy0)

   ! Compute difference between input (x,y) and iterand (x0,y0); if x or y is relatively large,
   ! apply an extra relaxation factor (1/4) or (1/2) to the differences dx and dy.
   IF ((ABS(amcx) .GT. (3000. + EPS_DELTA)) .OR. (ABS(amcy) .GT. (3000. + EPS_DELTA))) THEN
      dx = (amcx - amcx0)/4.
      dy = (amcy - amcy0)/4.
   ELSE IF ((ABS(amcx) .GT. (1000. + EPS_DELTA)) .OR. (ABS(amcy) .GT. (1000. + EPS_DELTA))) THEN
      dx = (amcx - amcx0)/2.
      dy = (amcy - amcy0)/2.
   ELSE
      dx = amcx - amcx0
      dy = amcy - amcy0
   ENDIF

   ! Check for convergence:
   IF ((ABS(dx) .GE. (difx - EPS_DELTA)) .OR. (ABS(dy) .GE. (dify - EPS_DELTA))) THEN

      ! No convergence yet; adjust lon-lat for next iteration;
      ! 111.1984 is R*CONV = R*180/pi in km, with R = earth radius. See also ops_reken
      geob  = geob + (dy/111.1984)
      geol  = geol + (dx/(111.1984*COS(geob/57.2958))) 
      tel = tel + 1
   
      ! Goto next iteration (if number of iterations < 300):
      IF (tel .LT. 300) THEN
         GOTO 50
      ENDIF
      
   !   WRITE (*, '( '' x and/or y coord. in subr. amcgeo beyond limits'')')
   !   WRITE (*, '( '' x ='', f6.0, '' y ='', f6.0, '' km'')') x, y
   
   ENDIF
   
! Iteration has converged or tel >= 300

RETURN
END SUBROUTINE amc2geo

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : flrs
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE geo2amc(geob, geol, amcx, amcy)

!DEC$ ATTRIBUTES DLLEXPORT:: geo2amc

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'geo2amc')

! CONSTANTS
real                                             :: AMFI                       ! longitude (phi) of Amersfoort (centre of RDM grid)
real                                             :: AMLA                       ! latitude (lambda) of Amersfoort (centre of RDM grid)
PARAMETER (AMFI = 18.7762) 
PARAMETER (AMLA =  1.9395) 
! 1 degree = 3600 seconds; AMFI, AMLA in units of 10000 seconds; conversion factor to degrees = 10000/3600:
! 10000*[AMFI, AMLA]/3600 = [AMFI, AMLA]/0.36 = [18.7762 1.9395]/0.36 = [52.1561 5.3875] = [Lat, Lon]_Amersfoort

! SUBROUTINE ARGUMENTS - INPUT
real,      INTENT(IN)                            :: geol                       ! longitude (phi)   [degrees]
real,      INTENT(IN)                            :: geob                       ! latitude (lambda) [degrees]

! SUBROUTINE ARGUMENTS - OUTPUT
real,      INTENT(OUT)                           :: amcx                       ! RDM x-coordinate (km)
real,      INTENT(OUT)                           :: amcy                       ! RDM y-coordinate (km

! LOCAL VARIABLES
real                                             :: f1
real                                             :: l1

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------

f1 = (.36*geob) - AMFI
l1 = (.36*geol) - AMLA
amcx  = 190.06691*l1 - 11.831*f1*l1 + 155. - 0.1142*(f1**2)*l1 - 0.03239*l1**3
amcy  = 309.02034*f1 + 3.63836*l1**2 + 463. + 0.07292*f1**2 - 0.15797*f1*(l1**2) + 0.05977*f1**3

RETURN
END SUBROUTINE geo2amc

!-------------------------------------------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : amc2lam
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE amc2lam(amcx, amcy, lamx, lamy)

!DEC$ ATTRIBUTES DLLEXPORT:: amc2lam

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'amc2lam')

! CONSTANTS

! SUBROUTINE ARGUMENTS - INPUT
real,      INTENT(IN)                            :: amcx                       ! amersfoortse x-coordinaat (km)
real,      INTENT(IN)                            :: amcy                       ! amersfoortse y-coordinaat (km)

! SUBROUTINE ARGUMENTS - OUTPUT
real,      INTENT(OUT)                           :: lamx                       ! lambert azimuthal x-coordinaat (km)
real,      INTENT(OUT)                           :: lamy                       ! lambert azimuthal y-coordinaat (km)

! LOCAL VARIABLES
real                                             :: geol                       ! hulpvariabele voor phi
real                                             :: geob                       ! hulpvariabele voor lambda

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Converteer amerfoortse naar geografische coordinaten.
!
call amc2geo(amcx, amcy, geol, geob)
!
! Converteer dan geografische naar Lambert azimuthal coordinaten.
!
call geo2lam(geob, geol, lamx, lamy)

RETURN
END SUBROUTINE amc2lam

!-------------------------------------------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : geo2lam
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE geo2lam(geob, geol, lamx, lamy)

!DEC$ ATTRIBUTES DLLEXPORT:: geo2lam

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'geo2lam')

! CONSTANTS
double precision                                 :: false_east                 ! linear value added to the x-coordinate values (longitude)
double precision                                 :: false_north                ! linear value added to the y-coordinate values (latitude)
double precision                                 :: R                          ! radius of the earth (equatorial)
double precision                                 :: degtorad                   ! degrees to radians
double precision                                 :: cen_med                    ! central median or central longitude
double precision                                 :: lat_ori                    ! latitude of origen or standard parallel

PARAMETER (false_east  = 4321000)
PARAMETER (false_north = 3210000)
PARAMETER (R           = 6378137)
PARAMETER (degtorad    =.017453293)
PARAMETER (cen_med     = 10)
PARAMETER (lat_ori     = 52)

! SUBROUTINE ARGUMENTS - INPUT
real,      INTENT(IN)                            :: geob                         ! breedtegraad (lambda)     (dec.) (lat)
real,      INTENT(IN)                            :: geol                         ! lengtegraad (phi)         (dec.) (lon)

! SUBROUTINE ARGUMENTS - OUTPUT
real,      INTENT(OUT)                           :: lamx                         ! lambert azimuthal x-coordinaat (km)
real,      INTENT(OUT)                           :: lamy                         ! lambert azimuthal y-coordinaat (km)

! LOCAL VARIABLES
double precision                                 :: ksp
double precision                                 :: lon                          ! longitude of the original grid
double precision                                 :: lat                          ! latitude of the original grid
double precision                                 :: sin_lat
double precision                                 :: cos_lat
double precision                                 :: sin_lon
double precision                                 :: cos_lon
double precision                                 :: sin_lat_ori
double precision                                 :: cos_lat_ori
double precision                                 :: sin_lon_delta
double precision                                 :: cos_lon_delta
double precision                                 :: x
double precision                                 :: y

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Documentation: http://mathworld.wolfram.com/LambertAzimuthalEqual-AreaProjection.html 
!
lat = dble(geob)
lon = dble(geol)
!
sin_lat_ori=sin(lat_ori*degtorad)
cos_lat_ori=cos(lat_ori*degtorad)

sin_lat=sin(lat*degtorad)
cos_lat=cos(lat*degtorad)

sin_lon=sin(lon*degtorad)
cos_lon=cos(lon*degtorad)

sin_lon_delta=sin((lon-cen_med)*degtorad)
cos_lon_delta=cos((lon-cen_med)*degtorad)

ksp = SQRT( 2 / ( 1 + sin_lat_ori * sin_lat + cos_lat_ori * cos_lat * cos_lon_delta ) )

x = R * ksp * cos_lat * sin_lon_delta
y = R * ksp * ( cos_lat_ori * sin_lat - sin_lat_ori * cos_lat * cos_lon_delta )

lamx = sngl(((x + false_east)/1000))
lamy = sngl(((y + false_north)/1000))

RETURN
END SUBROUTINE geo2lam

!-------------------------------------------------------------------------------------------------------------------------------

END MODULE m_geoutils
