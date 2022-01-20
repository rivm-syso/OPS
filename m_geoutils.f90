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
! DESCRIPTION        : This module contains geographical utilities.
! IMPLEMENTS         : - amc2geo: conversion of RDM to geographical lon-lat coordinates
!                    : - geo2amc: conversion of geographical lon-lat coordinates to RDM coordinates
!                    : - flrs: conversion of geographical lon-lat coordinates to RDM coordinates
!                      RDM coordinates are based on a km grid over the Netherlands, centred at Amersfoort; 
!                      RDM coordinates are also called "Amersfoortse coordinaten". RDM stands for RijksDriehoeksMeting,
!                      since this grid is determined by triangulation (driehoek = triangle) measurements (meting = meeasurements)
!                      of the Netherlands government (Rijk ~ government).
!-------------------------------------------------------------------------------------------------------------------------------
MODULE m_geoutils

IMPLICIT NONE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : amc2geo
! PURPOSE     : conversion of RDM to geographical lon-lat coordinates
! DESCRIPTION : Given input RDM coordinates (x,y), amc2geo uses an iterative method to compute the geographical coordinates (gl,gb).
!               Given an initial guess for (geol,geob), the corresponding RDM coordinates (x0,y0) are computed and compared with the 
!               input coordinates (amcx,amcy). In a next step, (geol,geob) are adjusted as function of the difference (dx,dy) = (xi-x,yi-y).
!               This iterative procedure continues until (dx,dy) is samller than a specified threshold (difx,dify).
!               limits: ca. 10000 km oost (y < 3000 km)  100 lon 16 lat
!                       ca.  8000 km oost (y < 4500 km)  105 lon 35 lat
!                       ca. 10000 km west (y < 4000 km)  -96 lon 17 lat
!                       ca.  6000 km west (y < 5000 km)  -95 lon 47 lat
!
! INPUTS      : amcx     (real*4), RDM x-coordinate [km]
!               amcy     (real*4), RDM y-coordinate [km]
! OUTPUTS     : geol     (real*4), longitude [degrees]
!               geob     (real*4), latitude  [degrees]
!               "geo" << geographical coordinates; "l" << lengtegraad = longitude, "b" << breedtegraad = latitude
!-------------------------------------------------------------------------------------------------------------------------------

INTERFACE amc2geo
  MODULE PROCEDURE amc2geo
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : geo2amc
! PURPOSE     : Convert greographical lon-lat coordinates to RDM coordinates
! INPUTS      : geob    (real*4), latitude, phi (degrees)
!               geob    (real*4), longitude, lambda (degrees)
! OUTPUTS     : amcx    (real*4), RDM x-coordinate
!               amcx    (real*4), RDM y-coordinate
!-------------------------------------------------------------------------------------------------------------------------------

INTERFACE geo2amc
  MODULE PROCEDURE geo2amc
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : amc2lam
! PURPOSE     : Berekenen van de lambert azimuthal equal area coordinaten (x,y) in km. uit de topografische (amersfoortse) coordinaten.
! INPUTS      : amcx   (real*4), x-coordinaat, Amersfoorts
!               amcy   (real*4), y-coordinaat, Amersfoorts
! OUTPUTS     : lamx   (real*4), x-coordinaat, Lambert azimuthaal
!             : lamy   (real*4), y-coordinaat, Lambert azimuthaal
!-------------------------------------------------------------------------------------------------------------------------------

INTERFACE amc2lam
  MODULE PROCEDURE amc2lam
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : geo2lam
! PURPOSE     : Berekenen van de lambert azimuthal equal area coordinaten (x,y) in km. uit de geografische coordinaten.
! INPUTS      : geob   (real*4), breedtegraad, phi (dec.)
!               geol   (real*4), lengtegraad, labda (dec.)
! OUTPUTS     : lamx   (real*4), x-coordinaat, Lambert azimuthaal
!             : lamy   (real*4), y-coordinaat, Lambert azimuthaal
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

USE m_commonconst_lib                                                         ! EPS_DELTA only

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'amc2geo')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: amcx                       ! RDM x-coordinate (km)
REAL*4,    INTENT(IN)                            :: amcy                       ! RDM y-coordinate (km)

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: geol                       ! longitude [degrees]
REAL*4,    INTENT(OUT)                           :: geob                       ! latitude  [degrees]

! LOCAL VARIABLES
INTEGER*4                                        :: tel                        ! iteration index
REAL*4                                           :: difx                       ! threshold value for dx
REAL*4                                           :: dify                       ! threshold value for dy
REAL*4                                           :: dx                         ! x - x0
REAL*4                                           :: dy                         ! y - y0
REAL*4                                           :: amcx0                      ! RDM x-coordinate that corresponds with (gb,gl)
REAL*4                                           :: amcy0                      ! RDM y-coordinate that corresponds with (gb,gl)

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
      
   !   WRITE (*, '( '' x and/or y coord. in subr. amc2geo beyond limits'')')
   !   WRITE (*, '( '' x ='', f6.0, '' y ='', f6.0, '' km'')') x, y
   
   ENDIF
   
! Iteration has converged or tel >= 300
! write(*,*) 'tel in amc2geo = ',tel 

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
REAL*4                                           :: AMFI                       ! longitude (phi) of Amersfoort (centre of RDM grid)
REAL*4                                           :: AMLA                       ! latitude (lambda) of Amersfoort (centre of RDM grid)
PARAMETER (AMFI = 18.7762) 
PARAMETER (AMLA =  1.9395) 
! 1 degree = 3600 seconds; AMFI, AMLA in units of 10000 seconds; conversion factor to degrees = 10000/3600:
! 10000*[AMFI, AMLA]/3600 = [AMFI, AMLA]/0.36 = [18.7762 1.9395]/0.36 = [52.1561 5.3875] = [Lat, Lon]_Amersfoort

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: geol                       ! longitude (phi)   [degrees]
REAL*4,    INTENT(IN)                            :: geob                       ! latitude (lambda) [degrees]

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: amcx                       ! RDM x-coordinate (km)
REAL*4,    INTENT(OUT)                           :: amcy                       ! RDM y-coordinate (km

! LOCAL VARIABLES
REAL*4                                           :: f1                         ! 
REAL*4                                           :: l1                         ! 

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
REAL*4,    INTENT(IN)                            :: amcx                       ! amersfoortse x-coordinaat (km)
REAL*4,    INTENT(IN)                            :: amcy                       ! amersfoortse y-coordinaat (km)

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: lamx                       ! lambert azimuthal x-coordinaat (km)
REAL*4,    INTENT(OUT)                           :: lamy                       ! lambert azimuthal y-coordinaat (km)

! LOCAL VARIABLES
REAL*4                                           :: geol                       ! hulpvariabele voor phi
REAL*4                                           :: geob                       ! hulpvariabele voor lambda

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
REAL*8                                           :: false_east                 ! linear value added to the x-coordinate values (longitude)
REAL*8                                           :: false_north                ! linear value added to the y-coordinate values (latitude)
REAL*8                                           :: R                          ! radius of the earth (equatorial)
REAL*8                                           :: degtorad                   ! degrees to radians
REAL*8                                           :: cen_med                    ! central median or central longitude
REAL*8                                           :: lat_ori                    ! latitude of origen or standard parallel

PARAMETER (false_east  = 4321000)    ! false easting [m]; a linear value added to the x-coordinate values, usually to ensure that all map coordinates are positive
PARAMETER (false_north = 3210000)    ! false northing [m]; a linear value added to the y-coordinate values, usually to ensure that all map coordinates are positive
PARAMETER (R           = 6378137)    ! semi-major axis [m]; radius of the equatorial axis of the ellipsoid
PARAMETER (degtorad    =.017453293)  ! conversion factor degrees to radians = pi/180
PARAMETER (cen_med     = 10)         ! central median [degrees]; line of longitude at the centre of a map projection generally used as the basis for constructing the projection
PARAMETER (lat_ori     = 52)         ! latitude of origin [degrees]; latitude chosen as the origin of rectangular coordinates for a map projection

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: geob                         ! breedtegraad (lambda)     (dec.) (lat)
REAL*4,    INTENT(IN)                            :: geol                         ! lengtegraad (phi)         (dec.) (lon)

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: lamx                         ! lambert azimuthal x-coordinaat (km)
REAL*4,    INTENT(OUT)                           :: lamy                         ! lambert azimuthal y-coordinaat (km)

! LOCAL VARIABLES
real*8                                           :: ksp
real*8                                           :: lon                          ! longitude of the original grid
real*8                                           :: lat                          ! latitude of the original grid
real*8                                           :: sin_lat
real*8                                           :: cos_lat
real*8                                           :: sin_lon
real*8                                           :: cos_lon
real*8                                           :: sin_lat_ori
real*8                                           :: cos_lat_ori
real*8                                           :: sin_lon_delta
real*8                                           :: cos_lon_delta
real*8                                           :: x
real*8                                           :: y

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
