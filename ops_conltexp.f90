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
!
! SUBROUTINE
! NAME               : %M%
! SCCS(SOURCE)       : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH -SEQUENCE   : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : HvJ/Franka Loeve (Cap Volmac)
! FIRM/INSTITUTE     : RIVM/LLO
! LANGUAGE           : FORTRAN-77/90
! DESCRIPTION        : Compute long term concentration for a given source and source-receptor distance;
!                      special version including correction for heavy plumes. 
!                      Concentration due to transport and dispersion only; no removal processes yet.
!                      Here, the concentration is computed at z = 0 m height. May be a problem very near a source,
!                      where there is a strong concentration profile. Later on, we apply a concentration profile, due to deposition,
!                      and there we calculate concentrations at measuring height.
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP Fortran
! CONTAINED FUNCTIONS: par_oppbr, par_puntbr
! CALLED FUNCTIONS   : ops_wvprofile
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_conltexp(rond, ol, qbron, szopp, uster, z0, htt, onder, vw10, pcoef, istab, disx, grof, iwd, qww, hbron,        &
                     &  dispg, radius, htot, c, sigz, ueff, xl, virty)

USE m_commonconst

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_conltexp')

! CONSTANTS
REAL*4                                           :: ZWCOR(NSTAB)               ! correctie voor vallende bronnen
REAL*4                                           :: PICON                      ! = fac/[pi * sqrt(2*pi)], fac = conversion factor g -> ug; fac = 1e6
REAL*4                                           :: PS                         ! = fac/(2*pi), fac = conversion factor g -> ug; fac = 1e6

PARAMETER  (PICON = 126987.)
PARAMETER  (PS    = 159155.)

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: rond                       ! 
REAL*4,    INTENT(IN)                            :: ol                         ! Monin-Obukhov lengte
REAL*4,    INTENT(IN)                            :: qbron                      ! 
REAL*4,    INTENT(IN)                            :: szopp                      ! initial vertical dispersion of source
REAL*4,    INTENT(IN)                            :: uster                      ! frictiesnelheid
REAL*4,    INTENT(IN)                            :: z0                         ! ruwheidslengte (m)
REAL*4,    INTENT(IN)                            :: htt                        ! 
REAL*4,    INTENT(IN)                            :: onder                      ! 
REAL*4,    INTENT(IN)                            :: vw10                       ! 
REAL*4,    INTENT(IN)                            :: pcoef                      ! 
INTEGER*4, INTENT(IN)                            :: istab                      ! 
REAL*4,    INTENT(IN)                            :: disx                       ! 
REAL*4,    INTENT(IN)                            :: grof                       ! 
INTEGER*4, INTENT(IN)                            :: iwd                        ! 
REAL*4,    INTENT(IN)                            :: qww                        ! 
REAL*4,    INTENT(IN)                            :: hbron                      ! 
REAL*4,    INTENT(IN)                            :: dispg(NSTAB)               ! 

! SUBROUTINE ARGUMENTS - I/O
REAL*4,    INTENT(INOUT)                         :: radius                     ! 
REAL*4,    INTENT(INOUT)                         :: htot                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: c                          ! long-term concentation at receptor at z = 0; excluding removal processes
REAL*4,    INTENT(OUT)                           :: sigz                       ! 
REAL*4,    INTENT(OUT)                           :: ueff                       ! wind speed at effective transport height heff; 
                                                                               ! for short distances heff = plume height;
                                                                               ! for large distances heff = 1/2 mixing height;
                                                                               ! heff is interpolated for intermediate distances.
REAL*4,    INTENT(OUT)                           :: xl                         ! 
REAL*4,    INTENT(OUT)                           :: virty                      ! 

! LOCAL VARIABLES
REAL*4                                           :: a                          ! reflection term source-surface-mixing height-surface
REAL*4                                           :: b                          ! reflection term source-mixing height-surface
REAL*4                                           :: cls                        ! 
REAL*4                                           :: disp                       ! 
REAL*4                                           :: f                          ! 
REAL*4                                           :: f1                         ! 
REAL*4                                           :: f2                         ! 
REAL*4                                           :: h                          ! 
REAL*4                                           :: hf                         ! effective transport height [m]
REAL*4                                           :: pld                        ! pluimdaling
REAL*4                                           :: pp                         ! 
REAL*4                                           :: qq                         ! 
REAL*4                                           :: rr                         ! 
REAL*4                                           :: sz                         ! 
REAL*4                                           :: tl                         ! 
REAL*4                                           :: u1                         ! 
REAL*4                                           :: utl                        ! 

! FUNCTIONS
REAL*4                                           :: ops_virtdist               ! 

!DATA
DATA ZWCOR/1.2, 1.1, 0.8, 0.6, 0.75, 0.6/

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
!--------------------------------------------------------------------
! Compute transport and dispersion parameters
!--------------------------------------------------------------------
!
! pld = plume descent
!
pld  = htt - htot
!
! Plume completely above mixing layer
!
IF (ABS(onder) .LE. EPS_DELTA) THEN
!
!  Set h = plume height, including plume descent due to heavy particles and limit to HUMAX 
!
   IF (htot .GT. (HUMAX + EPS_DELTA)) THEN
      h = HUMAX
   ELSE
      h = htot
   ENDIF
!
!  Compute wind speed at plume height using power law (2.9 OPS report)
!
   ueff = vw10*((h/10.)**pcoef)
!
!  Default concentration
!
   c = 0.
!
!  Compute sigma_z (representative over the trajectory) as function of the source receptor distance:
!  sigma_z = dispg*(disx**disph) (3.16 new OPS report); section 3.2.1. OPS report
!
   sigz = dispg(istab)*(disx**DISPH(istab))
   IF (sigz .LT. (1. - EPS_DELTA)) THEN
      sigz = 1
   ENDIF
!
!  If plume above mixing layer -> concentration at ground level = 0, but concentration still needed for e.g. wet deposition
!  pluimdikte boven menglaag 2*sigz
!
   IF (sigz .LT. (40. - EPS_DELTA)) THEN
      xl = 80.
   ELSE
      xl = 2.*sigz
   ENDIF
!
! Compute distance between (virtual point source) and (centre of area source);
! for a point source, virty = 0.
!
   virty = ops_virtdist(radius, rond)

ELSE
!
!  Plume (partly) inside mixing layer;
!  compute sigma_z for this class and source-receptor distance
!
   disp = DISPH(istab)
   IF (ABS(grof - 1.) .LE. EPS_DELTA) THEN
!
!     grof = 1 ("grof" = coarse): heavy particles
!     correction for large distances and heavy plumes
!
!     Compute u1 = wind speed at htt/2 
!     htt = plume height, excluding plume descent due to heavy particles [m]

      u1 = vw10*((htt/20.)**pcoef)

      ! tl: characteristic time
      IF (htt .GT. (50. + EPS_DELTA)) THEN
         tl = 1000.
      ELSE
         tl = 500.
      ENDIF
      
      ! utl = u1 * tl + radius = characteristic travel distance [m]
      utl = (u1*tl) + radius
      IF (utl .LT. (disx - EPS_DELTA)) THEN
         disp=ALOG(utl**(DISPH(istab)-.5)*disx**.5)/ALOG(disx)
      ENDIF
   ENDIF
!
!  compute dispersion parameters for an area source:
!
   IF (radius .GT. (0. + EPS_DELTA)) THEN
      CALL par_oppbr(rond, iwd, disx, istab, disp, htt, grof, dispg, ZWCOR, radius, sz, virty, rr, sigz, pld, htot)

   ELSE
!
!     compute dispersion parameters for a point source:
!
      CALL par_puntbr(qww, istab, disx, disp, htt, htot, hbron, dispg, sigz, hf, a, virty)
   ENDIF
!
!  Compute help variable pp = sigma_z/mixing_height = sigma_z/zi
!
   pp = sigz/xl
!
!--------------------------------------------------------------------
! Compute long term concentration due to transport and dispersion
!--------------------------------------------------------------------
!
!  Distinguish three cases (zi = xl = mixing height); see manual "lange-termijnmodel", April 1976, section 10.2.8)
!
!  (2*zi - htot)**2                           (2*zi - htot)**2                                                  sigma_z          1          htot
!  ----------------- = x <=> (2*sigma_z**2) = ------------------ <=> sigma_z = sqrt[1/(2x)] * (2*zi - htot) <=> -------- = sqrt(---) * (2 - ----)
!  (2*sigma_z**2)                                     x                                                            zi           2x           zi
!

!  case 1: short distance,                                           0 < sigma_z/zi <= 0.6*sqrt(1-htot/zi)
!  case 2: intermediate distance,                  0.6*sqrt(1-htot/zi) < sigma_z/zi <= 0.9
!  case 3: large distance and/or well mixed plume,                 0.9 < sigma_z/zi
!
!  Case 3 (large distance > 100 km and/or well mixed plume (sigma_z > 1.6*mixing_height)  AND grof < 0.2
!
   IF ((pp .GT. (1.6 + EPS_DELTA) .OR. disx .GT. (100000. + EPS_DELTA)) .AND. grof .LT. (.2 - EPS_DELTA)) THEN
!
!     At large distances, the plume is well mixed over the mixing layer;
!     in this case, the effective transport height = hf = mixing_height/2 (limited by HUMAX)
!
      IF (xl/2. .GT. (HUMAX + EPS_DELTA)) THEN
         hf = HUMAX
      ELSE
         hf = xl/2.
      ENDIF
!
!     Compute effective wind velocity at height hf (power law)
!
      ueff = vw10*((hf/10.)**pcoef)                                            ! 920906
!
!     Compute concentration at receptor for case 3:
!    
!             Q(x)                Q(x)   NSEK     1
!     C(x) = ----- D (x) D (x) = ----- --------  ---- ,  3.7, 3.8, 3.9 OPS report
!              u    y     z        u    2 pi x    zi
!
!     qbron in g/s, u in m/s, x and zi in m; c in ug/m3; PS = fac/(2*pi), fac = conversion factor g -> ug; fac = 1e6
!
      c = (qbron*NSEK*PS)/(ueff*(disx + virty)*xl)
   ELSE
!
!     Case 2,3 (intermediate, short distance)
!
      rr = sigz*sigz*2.
!
!
      qq = .6*sqrt(1. - htot/xl)
!
!     Compute hf = effective transport height over trajectory, by interpolating effective plume height at short distance and 
!     (mixing heigth)/2 at large distance (where plume is assumed to be well mixed over whole mixing layer).
!     For heavy particles (grof > 0.2) hf = plume_height/2
!     hf = (1. - pp/1.6)*htt + (pp/1.6)*xl/2. ?
!     pp = sigma_z/mixing_height




      IF (grof .GT. (.2 + EPS_DELTA)) THEN
         hf = htt/2.
      ELSE
         hf = (1. - pp/1.6)*htt + (pp/1.6)*xl/2.
      ENDIF
      IF (hf .GT. (HUMAX + EPS_DELTA)) THEN
         hf = HUMAX
      ENDIF
!
!     Compute wind speed at effective transport height by either a power law (hf > 50 m)
!     or by a logarithmic wind profile (hf <= 50 m).
!      
      IF ( hf .GT. 50 ) THEN
         ueff = vw10*(hf/10)**pcoef
      ELSE
         CALL ops_wvprofile(z0, hf, uster, ol, ueff)
      ENDIF
! 
!     qq = .6*sqrt(1. - htot/xl)
!     pp = sigma_z/mixing_height
!
!     For coarse particles (grof = 1) switch pp > qq/3 instead of pp > qq;
!     idea is that for coarse particles we have a descending plume and the reflection
!     at the earth surface takes place earlier 

      IF (pp .GT. (qq/(grof*2. + 1.) + EPS_DELTA)) THEN 
!
!        Case 2 (intermediate distance)
!
!        a = reflection term from source-surface-mixing_layer-surface
!        b = reflection term from source-mixing_layer-surface
!
!        3.15 in OPS report is rewritten as follows
!                                              
!        In the following h = htot = htt - pld
!
!                        2                   -h**2                 -(2 zi + h)**2           -(2 zi - h)**2
!        D (x) = ------------------- { exp[--------------] + exp [----------------] + exp [---------------] } = 
!         z      sqrt(2 pi) sigma_z         2 sigma_z**2            2 sigma_z**2             2 sigma_z**2
!
!                   2                  -h**2                    -[(2 zi + h)**2 - h**2]          -[(2 zi - h)**2 - h**2]
!         = ------------------- exp[--------------] * {1 + exp [------------------------] + exp [----------------------] }
!           sqrt(2 pi) sigma_z       2 sigma_z**2                    2 sigma_z**2                      2 sigma_z**2
!
!                   2                   -h**2                    
!         = ------------------- exp[--------------] * {1 + a + b} = 
!           sqrt(2 pi) sigma_z       2 sigma_z**2                
!
!
!                   2                   -h**2                    
!         = ------------------- exp[--------------] * cls .
!           sqrt(2 pi) sigma_z       2 sigma_z**2                
!
!         rr = sigz*sigz*2.
!
         a   = EXP( - 4.*xl*pld/rr)* EXP( - ((2.*xl + htot)**2 - htot*htot)/rr) 
         b   = EXP( - 4*(xl-htt)*pld/rr)* EXP(-((2*xl-htt-pld)**2 - htot*htot)/rr) 
         cls = 1. + a + b
      ELSE
!
!        Case 1 (short distance); no reflection terms
!
         cls = 1.
      ENDIF
!
!     Compute concentration at receptor for cases 1 and 2
!    
!             Q(x)                Q(x)   NSEK        2                      -h**2      
!     C(x) = ----- D (x) D (x) = ----- --------  ------------------- exp[--------------] * cls,  3.7, 3.8, 3.15 OPS report
!              u    y     z        u    2 pi x   sqrt(2 pi) sigma_z       2 sigma_z**2
!
!     rr = sigz*sigz*2.
!     qbron in g/s, u in m/s, x and zi in m; c in ug/m3; PICON = fac/[pi * sqrt(2*pi)]
!
      c = qbron*EXP( - (htot*htot/rr))*NSEK*PICON* cls/(ueff*(disx + virty)*sigz)
   ENDIF
!
!  Correction for concentration within area source.
!  Because this model assumes horizontal dispersion in a sector of 30 deg., consequently the emission
!  is spread out over a segment instead of a circle. This gives an underestimation at the outer parts
!  of the area source when the emission height is very low.
!
   IF (disx .LT. (radius - EPS_DELTA)) THEN
      f  = (disx + virty)/virty
      f1 = (radius - disx)/radius
      f2 = EXP( - (htt*htt)/(sigz*sigz))
      c  = c*((f - 1)*(f1**.4)*f2 + 1.) 
   ENDIF
ENDIF

RETURN

!-------------------------------------------------------------------------------------------------------------------------------

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : par_oppbr
! DESCRIPTION        : Compute different parameters for an area source.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE par_oppbr(rond, iwd, disx, istab, disp, htt, grof, dispg, zwcor, radius, sz, virty, rr, sigz, pld, htot)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'par_oppbr')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: rond                       ! 
INTEGER*4, INTENT(IN)                            :: iwd                        ! 
REAL*4,    INTENT(IN)                            :: disx                       ! 
INTEGER*4, INTENT(IN)                            :: istab                      ! 
REAL*4,    INTENT(IN)                            :: disp                       ! 
REAL*4,    INTENT(IN)                            :: htt                        ! 
REAL*4,    INTENT(IN)                            :: grof                       ! 
REAL*4,    INTENT(IN)                            :: dispg(NSTAB)               ! 
REAL*4,    INTENT(IN)                            :: zwcor(NSTAB)               ! 

! SUBROUTINE ARGUMENTS - I/O
REAL*4,    INTENT(INOUT)                         :: radius                     ! 
REAL*4,    INTENT(INOUT)                         :: sz                         ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: virty                      ! 
REAL*4,    INTENT(OUT)                           :: rr                         ! 
REAL*4,    INTENT(OUT)                           :: sigz                       ! 
REAL*4,    INTENT(OUT)                           :: pld                        ! 
REAL*4,    INTENT(OUT)                           :: htot                       ! 

! LOCAL VARIABLES
REAL*4                                           :: cr                         ! 
REAL*4                                           :: radr                       ! 
REAL*4                                           :: dx                         ! 
REAL*4                                           :: dy                         ! 
REAL*4                                           :: sta1                         ! 
REAL*4                                           :: sta2                         ! 
REAL*4                                           :: s1                         ! 
REAL*4                                           :: s2                         ! 
REAL*4                                           :: dsx                        ! 

! FUNCTIONS
REAL*4                                           :: ops_virtdist               ! 

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Compute distance between (virtual point source) and (centre of area source);
! note that for a point source, virty = 0.

!
virty = ops_virtdist(radius, rond)

IF (rond .EQ. 1) THEN
   cr    = 1.
ELSE
!
!   Square area source;
!   Compute correction factor cr for corrected source radius r' = r*cr, such that r' represents a square area source 
!
   dx    = ABS(radius*SIN(FLOAT(iwd)/CONV))
   dy    = ABS(radius*COS(FLOAT(iwd)/CONV))
   rr    = AMAX1(dx, dy)
   cr    = radius/rr
ENDIF
!
! radr = corrected radius; includes correction factor 1.006 because wind direction iwd is integer
!
radr = radius*cr/1.006
!
! Compute integrated sigma_z over area source (in the form sigma_z = dispg*x**disph).
! The approximation of this integral is exact for DISPH(istab) = 1.0.
! See figure 3.6 OPS report.
! First compute s1 = sigma_z at area source's edge near receptor (disx-radr) (s1 = 0 inside area source)
!           and s1 = sigma_z at area source's edge far from receptor (disx+radius)


IF (disx .LT. (radr - EPS_DELTA)) THEN
   s1  = 0.
   dsx = radius
ELSE
   s1  = dispg(istab)*((disx - radr)**disp)
   dsx = disx
ENDIF
s2 = 0.92*dispg(istab)*((dsx + radius)**disp) 

! sz OPS report: represents the distribution of source heights within the area source
sz = 0.1      

IF (abs(s2-s1) .LE. 1.E-04) s2 = s1*1.001

! Compute sigma_z (3.39 OPS report); szopp = spread in source height to represent different sources in an area source:
sigz   = (s2-s1)/alog((sz+s2)/(sz+s1))
sigz   = sqrt(sigz*sigz+szopp*szopp)
radius = radr
!
! Correction plume descent of heavy plumes inside area source; empirical relation that is adequate
! correctie pluimdaling van zware pluimen binnen opp bron dit is een empirische relatie die redelijk voldoet
!
IF ((disx .LE. (radius + EPS_DELTA)) .AND. (ABS(grof - 1.) .LE. EPS_DELTA)) THEN
   pld  = (sigz*zwcor(istab)) + (pld/15.)
   htot = htt - pld
ENDIF

RETURN
END SUBROUTINE par_oppbr

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : par_puntbr
! DESCRIPTION        : Deze routine berekent verschillende parameters voor een puntbron.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE par_puntbr(qww, istab, disx, disp, htt, htot, hbron, dispg, sigz, hf, a, virty)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'par_puntbr')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: qww                        ! 
INTEGER*4, INTENT(IN)                            :: istab                      ! 
REAL*4,    INTENT(IN)                            :: disx                       ! 
REAL*4,    INTENT(IN)                            :: disp                       ! 
REAL*4,    INTENT(IN)                            :: htt                        ! 
REAL*4,    INTENT(IN)                            :: htot                       ! 
REAL*4,    INTENT(IN)                            :: hbron                      ! 
REAL*4,    INTENT(IN)                            :: dispg(NSTAB)               ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: sigz                       ! 
REAL*4,    INTENT(OUT)                           :: hf                         ! 
REAL*4,    INTENT(OUT)                           :: a                          ! 
REAL*4,    INTENT(OUT)                           :: virty                      ! 

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Pasquill dispersion ( point sources)
!
IF (ABS(qww) .LE. EPS_DELTA) THEN
   sigz = dispg(istab)*(disx**disp)
ELSE
!
!  Buoyant plumes: sigma_z taken from OML model (Berkowicz and Olesen, 1986)
!  sigma_z in the form dispg*x**disp
!
   IF (htt .GT. (HUMAX + EPS_DELTA)) THEN
      hf = HUMAX
   ELSE
      hf = htt
   ENDIF
   sigz = dispg(istab)*(disx**disp)

   ! Add extra vertical dispersion due to buoyant plumes; 2.5066 = sqrt(2*pi);
   ! taken from OML model (Berkowicz and Olesen, 1986)
   a = (htot - hbron)/2.5066 
   a = AMIN1(sigz, a)/1.5                                                      ! 960115
   sigz = SQRT((sigz*sigz) + (a*a))
ENDIF

! Point source, so extra virtual distance for area source = 0
virty = 0.

RETURN
END SUBROUTINE par_puntbr

END SUBROUTINE ops_conltexp
