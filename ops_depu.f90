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
! SUBROUTINE
! NAME               : %M%
! SCCS(SOURCE)       : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH -SEQUENCE   : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : OPS-support 
! FIRM/INSTITUTE     : RIVM/LLO
! LANGUAGE           : FORTRAN-77/90
! USAGE              :
! DESCRIPTION        : This routine calculates deposition parameters vg, ra and rb with vg=1./(ra+rb+rc)
!                      - ra,the aerodynamic resistance in s/m over a layer with height z is the integral from z0 to z of
!                      1/K(z)dz with K(z)=k.u*.z/phi(h) and phi(h)=(1-16z/l)**-0.5 for l<0 and 1.+5.(z/l) for l>0. (Dyer and
!                      Hicks (1977) relations)
!                      By integration we get ra=1./(k.u*).(ln(z0/z)+ghi(z0)-ghi(z)) with ghi(z), the stability correction for
!                      heat (Beljaars et al., 1989).
!                      - rb, the laminair layer resistance in s/m: rb=2(ku*)**-1*(Sc/Pr)**p with Sc=Schmidt number, Pr=Prandtl
!                      number, p=2/3 by experiment.
!                      - rc, the surface resistance in s/m must be defined.
!
!                      References:
!                      1. Businger J.A. (1973) in: workshop on micrometeorology
!                         Atm. Meteor. Soc.
!                      2. Wesely M.L. and Hicks B.B. (1977)
!                         Some factors that effect the deposition rates on so2 and simular gases on vegetation.
!                         J. Air Pollut. Contr. Org. 27 no.11, pp 1110-1116.
!                      3. Hicks B.B., Baldocchi D.D., Meyers T.P., Hosker Jr. R.P., Matt D.R. (1987)
!                         A preliminary multiple resistance routine for deriving dry deposition velocities from measured
!                         quantities.
!                         J. Water, Air and Soil Pollut.,36,311-330.
!                      4. Erisman J.W., Versluis A.H., Verplanke T.A.J.W., de Haan D., Anink D., van Elzakker B.G. and van Aalst
!                         R.M. (1990)
!                         Monitoring the dry deposition of SO2 in the Netherlands.
!                         Report nr. 228601002, RIVM, Bilthoven, the Netherlands.
!                      5. Beljaars A.C.M., Holtslag A.A.M. and Westrhenen R.M. van (1989)
!                         Description oa a software library for the calculation of surface fluxes.
!                         Technical report TR-112, KNMI, De Bilt.
!
! EXIT CODES         :
! REFERENCE          :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP-Fortran
! UPDATE HISTORY :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_depu(icnr, z0, zra, d, rc, ol, uster, vg, ra, rb)

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_depu')

! CONSTANTS
REAL*4                                           :: P                          ! exponent (experimenteel bepaald)
REAL*4                                           :: PR                         ! Prandtl number
REAL*4                                           :: VONK                       ! Von Karman constante

PARAMETER   (VONK = 0.4  )
PARAMETER   (P    = 2./3.)
PARAMETER   (PR   = 0.72 )

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: icnr                       ! component number for calculation of rb
REAL*4,    INTENT(IN)                            :: z0                         ! surface roughness length in meters.
REAL*4,    INTENT(IN)                            :: zra                        ! height for which deposition velocity is calculated (m)
REAL*4,    INTENT(IN)                            :: d                          ! displacement height (usually 0.7 * vegetation height) (m)
REAL*4,    INTENT(IN)                            :: rc                         ! canopy resistance in (s/m)
REAL*4,    INTENT(IN)                            :: ol                         ! monin-obukhov length (m)
REAL*4,    INTENT(IN)                            :: uster                      ! friction velocity u* (m/s)

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: vg                         ! depositin velocity at zra (m/s)
REAL*4,    INTENT(OUT)                           :: ra                         ! aerodynamic resistance at zra (s/m)
REAL*4,    INTENT(OUT)                           :: rb                         ! laminar layer resistance for component incr (s/m)

! LOCAL VARIABLES
REAL*4                                           :: sc                         ! Schmidt number
REAL*4                                           :: zru                        ! correction for displacement height

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Schmidt number:
! (note: in calling routine only icnr = 1 or 9 
!
IF (icnr .EQ. 1 .OR. icnr .LE. 0) THEN                                         ! SO2 or user-defined substance
   sc = 1.25
ELSE IF (icnr .EQ. 5) THEN                                                     ! O3
   sc = 0.88
ELSE IF (icnr .EQ. 6) THEN                                                     ! NO
   sc = 1.07
ELSE IF (icnr .EQ. 7) THEN                                                     ! NO2
   sc = 1.07
ELSE IF (icnr .EQ. 8) THEN                                                     ! HNO3
   sc = 1.25
ELSE                                                                           ! NH3  (op basis van molgewicht)
   sc = 0.69
ENDIF
!
! correction for the displacement height is made
!
zru = zra - d
!
! calculation of the aerodynamic resistance (Ra):
!
ra = (1./(VONK*uster))*(alog((zru)/z0) - fpsih(zru/ol) + fpsih(z0/ol))
!
! calculation of the laminar layer resistance (Rb):
!
rb = (2./(VONK*uster))*((sc/PR)**P)
!
! calculation of the deposition velocity (Vd):
!
vg = 1./(ra + rb + rc)

RETURN

!-------------------------------------------------------------------------------------------------------------------------------

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION           : fpsih
! DESCRIPTION        : Stability correction function in the surface layer temperature profile. The present model is an empirical
!                      fit by Holtslag and De Bruin(1987) of data by Hicks (1976, Quart. J. R. Meteor. Soc., 102, 535-551).
!                      See also Holtslag (1984, BLM, 29, 225-250)
! AUTHOR             : OPS-support 
!-------------------------------------------------------------------------------------------------------------------------------
REAL FUNCTION fpsih(eta)

USE m_commonconst                                                              ! EPS_DELTA only

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'fpsih')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: eta                        ! stabiliteitsparameter z/l

! OUTPUT
!     Return value
!
! LOCAL VARIABLES
REAL*4                                           :: y                          ! hulpvariabele bij de berekening

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
IF (eta .LT. (0. - EPS_DELTA)) THEN
   y         = SQRT(1. - 16.*eta)
   fpsih = 2.*ALOG((1. + y)/2.)
ELSE
   fpsih = -(0.7*eta) - (0.75*eta - 10.72)* EXP( -0.35*eta) - 10.72
ENDIF

RETURN
END FUNCTION fpsih

END SUBROUTINE ops_depu
