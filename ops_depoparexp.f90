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
! DESCRIPTION        : Get parameters needed for dry deposition, wet deposition and chemical conversion.
!                      Not only deposition velocities, but especially those parameters that deal with the mass loss 
!                      due to these processes over a certain travel distance. In an analytical model, this mass loss is not
!                      summed on a step-by-step basis along a trajectory, but instead, the integral of the mass loss at
!                      the receptor is computed.
!
!                      When computing mass loss due to deposition, different situations can occur, with respect to the transport
!                      of substances from a source:
!                      a. vertical Gaussian distribution near the source
!                         aa. plume above the mixing layer
!                         ab. plume within the mixing layer, does not touch the earth's surface
!                         ac. plume within the mixing layer, reflecting at earth's surface or at the top of the mixing layer
!                      b. homegeneous mixing within a wind sector at larger distances from the source.
!
!                      For an area source, we have to consider the possibility that a receptor is within the area source.

!                      For wet deposition, the cloud base is assumed to be equal to the mixing height.
!
!                      In this routine, we distinguish gaseous and particulate substances; the deposition of particles depends
!                      on the particle size. Heavy particles cause a plume to descend. Coarse particles (>20 um) are treated 
!                      separately; it is assumed that the deposition velocity of coarse particles equals half the sedimentation
!                      velocity.
! 
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP Fortran
! CALLED FUNCTIONS   :
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_depoparexp(kdeel, c, ol, qbstf, ra4_rcp, ra50_rcp, raz_rcp, rb_rcp, sigz, ueff, uster, z0, virty, gasv, itra,    &
                       &  rb, ra4, istab, grof, ra50, xvghbr, xvglbr, regenk, rint, buil, zf, isek, iseiz, mb, disx, radius,    &
                       &  xl, onder, dg, knatdeppar, scavcoef, irev, htt, xloc, xl100, vw10, pcoef, vchem, dispg, htot,         &
                       &  error, pr, twt, cratio, rc_rcp, grad, rc, utr, vg50_rcp, routpri, vg50trans, rkc, ri, vnatpri,        &
                       &  cgt, cgt_z, cq2, cdn, cch, z0_src, ol_src, uster_src, z0_tra, rctra_0, rcsrc, ra4src, rb_src,         &
                       &  ra50src, ra4tra, ra50tra, rb_tra, vgpart, xm, ym, zm, bx, by, xg)
                       

USE m_commonconst
USE m_commonfile
USE m_error

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_depoparexp')

! CONSTANTS
REAL*4                                           :: RCDEEL(NPARTCLASS)         ! waarden van rc per deeltjesklasse
REAL*4                                           :: RDS(NSTAB)                 ! grenslaagweerstand per stab. klasse
REAL*4                                           :: RORATIO(NPARTCLASS)        ! (geschatte) waarden scavenging ratio per deeltjesklasse
REAL*4                                           :: VGDEEL(NPARTCLASS)         ! 
REAL*4                                           :: RA4S(NSTAB)                ! 

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: kdeel                      ! 
REAL*4,    INTENT(IN)                            :: c                          ! 
REAL*4,    INTENT(IN)                            :: ol                         ! Monin-Obukhov lengte
REAL*4,    INTENT(IN)                            :: qbstf                      ! source strength current source (for current particle class)
REAL*4,    INTENT(IN)                            :: ra4_rcp                    ! ra at receptor (4m)
REAL*4,    INTENT(IN)                            :: ra50_rcp                   ! ra at receptor (50m)
REAL*4,    INTENT(IN)                            :: raz_rcp                    ! height dependent ra on receptor
REAL*4,    INTENT(IN)                            :: rb_rcp                     ! 
REAL*4,    INTENT(IN)                            :: sigz                       ! 
REAL*4,    INTENT(IN)                            :: ueff                       ! wind speed at effective transport height heff; 
                                                                               ! for short distances heff = plume height;
                                                                               ! for large distances heff = 1/2 mixing height;
                                                                               ! heff is interpolated for intermediate distances.
REAL*4,    INTENT(IN)                            :: uster
REAL*4,    INTENT(IN)                            :: z0
REAL*4,    INTENT(IN)                            :: virty                      ! 
LOGICAL,   INTENT(IN)                            :: gasv                       ! 
INTEGER*4, INTENT(IN)                            :: itra                       ! 
REAL*4,    INTENT(IN)                            :: rb                         ! 
REAL*4,    INTENT(IN)                            :: ra4                        ! 
INTEGER*4, INTENT(IN)                            :: istab                      ! 
REAL*4,    INTENT(IN)                            :: grof                       ! 
REAL*4,    INTENT(IN)                            :: ra50                       ! 
REAL*4,    INTENT(IN)                            :: xvghbr                     ! 
REAL*4,    INTENT(IN)                            :: xvglbr                     ! 
REAL*4,    INTENT(IN)                            :: regenk                     ! 
REAL*4,    INTENT(IN)                            :: rint                       ! 
REAL*4,    INTENT(IN)                            :: buil                       ! 
REAL*4,    INTENT(IN)                            :: zf                         ! 
INTEGER*4, INTENT(IN)                            :: isek                       ! 
INTEGER*4, INTENT(IN)                            :: iseiz                      ! 
INTEGER*4, INTENT(IN)                            :: mb                         ! 
REAL*4,    INTENT(IN)                            :: disx                       ! 
REAL*4,    INTENT(IN)                            :: radius                     ! 
REAL*4,    INTENT(IN)                            :: xl                         ! 
REAL*4,    INTENT(IN)                            :: onder                      ! 
REAL*4,    INTENT(IN)                            :: dg                         ! 
INTEGER*4, INTENT(IN)                            :: knatdeppar                 ! 
REAL*4,    INTENT(IN)                            :: scavcoef                   ! 
LOGICAL,   INTENT(IN)                            :: irev                       ! 
REAL*4,    INTENT(IN)                            :: htt                        ! 
REAL*4,    INTENT(IN)                            :: xloc                       ! 
REAL*4,    INTENT(IN)                            :: xl100                      ! 
REAL*4,    INTENT(IN)                            :: vw10                       ! 
REAL*4,    INTENT(IN)                            :: pcoef                      ! 
REAL*4,    INTENT(IN)                            :: vchem                      ! 
REAL*4,    INTENT(IN)                            :: dispg(NSTAB)               ! 
REAL*4,    INTENT(IN)                            :: z0_src                     ! roughness length at source; from z0-map [m]
REAL*4,    INTENT(IN)                            :: ol_src                     !
REAL*4,    INTENT(IN)                            :: uster_src                  !
REAL*4,    INTENT(IN)                            :: z0_tra                     ! roughness length representative for trajectory [m]
REAL*4,    INTENT(IN)                            :: ra4src                     !
REAL*4,    INTENT(IN)                            :: rb_src                     !
REAL*4,    INTENT(IN)                            :: ra50src                    !
REAL*4,    INTENT(IN)                            :: ra4tra                     !
REAL*4,    INTENT(IN)                            :: rb_tra                     !
REAL*4,    INTENT(IN)                            :: ra50tra                    !
REAL*4,    INTENT(IN)                            :: xm
REAL*4,    INTENT(IN)                            :: ym
REAL*4,    INTENT(IN)                            :: zm                         ! z-coordinate of receptor points (RDM)
INTEGER*4, INTENT(IN)                            :: bx 
INTEGER*4, INTENT(IN)                            :: by

! SUBROUTINE ARGUMENTS - I/O
REAL*4,    INTENT(INOUT)                         :: rctra_0                    !
REAL*4,    INTENT(INOUT)                         :: htot                       ! 
REAL*4,    INTENT(INOUT)                         :: rcsrc                      !
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: pr                         ! 
REAL*4,    INTENT(OUT)                           :: twt                        ! 
REAL*4,    INTENT(OUT)                           :: cratio                     ! 
REAL*4,    INTENT(OUT)                           :: rc_rcp                     ! 
REAL*4,    INTENT(OUT)                           :: grad                       ! 
REAL*4,    INTENT(OUT)                           :: rc                         
REAL*4,    INTENT(OUT)                           :: utr                        ! average wind speed over the trajectory (m/s)
REAL*4,    INTENT(OUT)                           :: vg50_rcp                   ! 
REAL*4,    INTENT(OUT)                           :: vgpart                     ! 
REAL*4,    INTENT(OUT)                           :: routpri                    ! in-cloud scavenging ratio for primary component
                                                                               ! (rout << rain-out = in-cloud) [-]
REAL*4,    INTENT(OUT)                           :: vg50trans                  ! 
REAL*4,    INTENT(OUT)                           :: rkc                        ! 
REAL*4,    INTENT(OUT)                           :: ri                         ! 
REAL*4,    INTENT(OUT)                           :: vnatpri                    ! wet deposition loss rate for primary components [%/h]
REAL*4,    INTENT(OUT)                           :: cgt                        ! 
REAL*4,    INTENT(OUT)                           :: cgt_z                      ! height dependent cgt
REAL*4,    INTENT(OUT)                           :: cq2                        ! 
REAL*4,    INTENT(OUT)                           :: cdn                        ! 
REAL*4,    INTENT(OUT)                           :: cch                        ! 

! LOCAL VARIABLES
INTEGER*4                                        :: flag                       ! stable meteo class and stack emitting above mixing layer 
REAL*4                                           :: a                          ! 
REAL*4                                           :: cq1                        ! 
REAL*4                                           :: diameter                   ! 
REAL*4                                           :: dxeff                      ! effective distance over which deposition takes place within an area source
REAL*4                                           :: grad_z                     ! height dependent grad
REAL*4                                           :: hf                         ! 
REAL*4                                           :: p1                         ! 
REAL*4                                           :: p2                         ! 
REAL*4                                           :: pldaling                   ! 
REAL*4                                           :: sigzr                      ! 
REAL*4                                           :: ux0                        ! wind speed near source at plume height (m/s)
REAL*4                                           :: uxr                        ! wind speed representative for plume over area source (m/s)
REAL*4                                           :: ugem                       ! average wind speed depending on phase of plume development (m/s)
REAL*4                                           :: vg0tra                     ! 
REAL*4                                           :: vg50tra                    ! 
REAL*4                                           :: xg                         ! 
REAL*4                                           :: zu                         ! 
LOGICAL                                          :: ops_openlog                ! function for opening log file

!
! VGDEEL = deposition velocity per particle class [m/s];
! Sehmel G.A. and Hodgson W.H. (1980) A model for predicting dry deposition of particles and gases to environmental surfaces.
! AIChE Symposium Series 86, 218-230. See also 
!
! DATA
DATA VGDEEL/.00030, .0013, .0046, .009, .024, .054/
!
! RCDEEL: surface resistance Rc per per particle class computed from Ra and Rb values per particle class and 
! averaged dry deposition velocities vg (weighed over stability classes).
! See new OPS report, Table 5.2
! 
DATA RCDEEL/3200., 700., 150., 50., 2., -17./
!
! roratio: scavenging ratio per particle class .
! Estimated from measurements of rainwater concentration and air concentration for different substances.

! See nok/luk reports "Luchtverontreniging t.g.v. de uitworp van kolengestookte installaties"
! Potma C.J., Onderdelinden D. and Slanina J. (1986): bijdrage van een kolengestookte elecktriciteitscentrale
!       aan de lokale luchtconcentratie- en depositiveniveaus. PEO report NOK-LUK 3, no. 20.70-017.10, 
!       RIVM report 22822 02 004, Bilthoven.
! van Jaarsveld en Onderdelinden (1986): Modelmatige beschrijving van concentratie en depositie van kolen relevante 
!       componenten in NL veroorzaakt door emissies in Europa, PEO report NOK-LUK 3, RIVM report 2282 02 002, Bilthoven.
!       See Table III measurements in Lelystad and Bilthoven .
! DATA RORATIO / 120000.,1000000.,5000000.,2*9000000./ aangepast (zie EUTREND) 931120
!
DATA RORATIO/240000., 1000000., 1000000., 5000000., 2*9000000./
!
! RDS: boundary layer resistance Rb per stability class:
!
DATA RDS  /25., 17., 20., 11., 119., 40./
!
! RA4S: aerodynamic resistance at 4 m height per stability class
!
DATA RA4S /24., 17., 23., 13., 226., 52./

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Initialisation
!
sigzr = 0.0

! Square area source is represented by a circular area source with the same area;
! (area circle with radius r) = (area square with 1/2 side = radius) <=> pi*r**2 = (2*radius)**2 <=> 
! <=> r = (2/sqrt(pi))*radius <=> r = 1.128*radius
! See ops_virtdist: virty = (radius*12.)/PI*1.128
diameter = (virty*PI)/(6.*1.128) 
!
! Set surface resistances Rc for particles at different locations (receptor, source, average over trajectory)
! and set routpri = in-cloud scavenging ratio [-] for the current particle class
!
IF (.NOT.gasv) THEN
   rc       = RCDEEL(kdeel)
   rc_rcp  = rc
   rcsrc    = rc
   rctra_0  = rc
   routpri  = RORATIO(kdeel)
ENDIF
!
! Gas or fine particles (grof .ne. 1);
! compute deposition velocity vg = 1/(Ra + Rb + Rc) at different locations/heights and
! grad = depositon velocity gradient over height = vg(50)/vg(4)
!
IF (ABS(grof - 1.) .GT. EPS_DELTA) THEN
   vg50_rcp = 1./(ra50_rcp + rb_rcp + rc_rcp)
   vg50tra   = 1./(ra50tra + rb_tra + rctra_0)
   !write(*,'(a,4(1x,e12.5))') 'ops_depoparexp1/vg50tra,ra50tra,rb_tra,rctra_0: ',vg50tra,ra50tra,rb_tra,rctra_0
   vg0tra    = 1./(rb_tra + rctra_0) ! vd(z = 0)              ! 970809
   grad      = (ra4_rcp + rb_rcp + rc_rcp)/ (ra50_rcp + rb_rcp + rc_rcp)
   grad_z    = (raz_rcp + rb_rcp + rc_rcp)/ (ra50_rcp + rb_rcp + rc_rcp)
!
! Coarse particles (grof = 1);
! get deposition velocity vg = 1/(Ra + Rb + Rc) at different locations/heights and
! set grad = depositon velocity gradient over height = 1
!
ELSE
   vg50_rcp  = VGDEEL(kdeel)
   vgpart    = VGDEEL(kdeel)
   vg50tra   = vg50_rcp
   vg0tra    = vg50_rcp
   grad      = 1.
   grad_z    = 1.
ENDIF
!
! vg50tra   = average deposition velocity over trajectory, when the plume is not yet fully mixed over the mixing layer
! vg50trans = average deposition velocity over trajectory, when the plume is fully mixed over the mixing layer
! xvglbr    = ratio effective dry deposition velocity over transport distance and 
!             average dry deposition velocity over transport distance for low sources [-]
!             See OPS-doc/meteo, bookmark correction_factor_deposition_velocity
! xvghbr    = ratio effective dry deposition velocity over transport distance and 
!             average dry deposition velocity over transport distance for high sources [-]
!             See OPS-doc/meteo, bookmark correction_factor_deposition_velocity
! xvghbr and xvglbr are read from the meteo statistics file
!
! Distinguish between very high, high and low sources in the computation of deposition velocity over a trajectory;
! very high source -> the effective dry deposition velocity is lower than for low sources, because high sources contribute
!                     relatively little at low wind speeds (e.g. in the night). The factor 0.7 is derived from model experiments.
IF (htot .GT. (350. + EPS_DELTA)) THEN
   vg50trans = xvghbr*vg50tra*.7

! Check whether source emits above the mixing layer or not:
!ELSE IF (htot .GT. (200. + EPS_DELTA)) THEN
ELSE IF (htot .GT. xloc) THEN 
   vg50trans = xvghbr*vg50tra
ELSE
   vg50trans = xvglbr*vg50tra
ENDIF
!write(*,'(a,5(1x,e12.5))') 'ops_depoparexp2/htot,vg50tra,xvglbr,xvghbr,vg50trans: ',htot,vg50tra,xvglbr,xvghbr,vg50trans
!
! rkc: obsolete factor
!
rkc = 1
!
! Compute wet deposition
!
CALL par_nat(regenk, rint, buil, zf, isek, iseiz, mb, disx, radius, diameter, ueff, xl, onder, sigz, htot, gasv, dg,           &
          &  knatdeppar, scavcoef, routpri, kdeel, irev, c, qbstf, virty, twt, pr, cratio, ri, a, vnatpri)
!
! Gradient term; grad = vg(50)/vg(4); assuming a constant flux over height, we derive:
!
!                                                  vd(50)    c(4)
! F(50) = F(4) <=> -vd(50)*c(50) = -vd(4)*c(4) <=> ------ = ------ <=> c(4) = grad*c(50) 
!                                                   vd(4)    c(50)
!                                vg(z2)
! Note: cgt = (1 - grad) = (1 - -------) is used as input for ops_brondepl (see there).
!                                vg(z1)
!
cgt   = 1. - grad
cgt_z = 1. - grad_z      ! EvdS

! Default value for wind speed for area source  
uxr = ueff
!
! onder is fraction of plume in mixing layer ("onder"= below)
! onder = 1 -> plume completely below mixing height
! onder = 0 -> plume completely above mixing height
!
! Plume above mixing height or coarse particles 
!
IF ((ABS(onder) .LE. EPS_DELTA) .OR. (ABS(grof - 1.) .LE. EPS_DELTA)) THEN
   cq2   = 1.
   cdn   = 1.
   cq1   = 1.
   cgt   = 0.
   cgt_z = 0.
   utr   = ueff  

   ! dxeff is effective travel distance, computed by using a large number of point sources and by
   ! comparing concentrations from the area source with those point sources; this means that sources
   ! that are close have more effect than point sources further away.
   !  
   dxeff = diameter/4.
ELSE
!
! Compute xg (g << ground) = location where the plume is fully mixed for stable classes; we use the mixing height at the source xloc.
! Deposition starts from this point on, assuming a homogeneously mixed plume.
! This is applied to those plumes which start above the mixing layer and are later being caught within the mixing layer.
!
! hbron  : emission height at source (stack height), without plume rise [m]
! htt    : plume height at source, including plume rise [m]
! htot   : plume height at receptor, including plume descent due to heavy particles; htot = htt - pldaling [m]
!
!
! ------------- xl (mixing height) -------------------------.-------------------------------------------------------------                                                                    .
!                                                  .
!                                         .                      3 stages of plume development:
!                                .                               1. plume does not reach the ground                                                      
!                        .                                       2. plume reaches the ground, but is not yet fully mixed
!              .                                                 3. plume is fully mixed over the mixing layer
!       .                                                                                     
!  htt -
!      |  .    -  
!      |     .        - central
!      |        .               - axis 
!      |           .                   - of
!      |              .                       - plume 
!      |                 .                             -
!      |                    .                                 -
! hbron|                       .                                    - htot
!     | |                         .                                       
!     | |                            .                          
!     | |                               .                          
!     | |                                  .                     
!     | |                                     .                         
! -----.11111111111111111111111111111111111111111.2222222222.3333333.33333333
!   source                                                  xg      receptor
!
   flag = 0
!
! xloc : mixing height near source at x = 0
! xl100: mixing height at 100 km from source.
! 
! A. (flag = 1) Plume is released above the mixing layer, then the plume travels a while above the mixing layer
!    before it is caught inside the mixing layer and transported to the ground.
!    We assume:
!    1. the mixing layer rises with increasing downwind distance
!    2. the travel time above the mixing layer is dominant (the time needed to transport
!       the plume within the mixing layer can be neglected).
!    3. the plume travels without dispersion above the mixing layer.
!
!                                                                          .C' xl(100)                                                                             
!                                                                  .                                                                              
!                                                          .                                                                              
!            plume above mixing layer              .
!  htot-------------------------------------.C
!      |                             .      @
!      |                       .            @ 
!      |               .  layer             @
!      |       .   mixing                   @
! xl(0).A   rising                          @ B                             B'      
!      |                                    @ 
!      |                                    @ 
! hbron|                                    @ plume 
!     | |                                   @ transported 
!     | |                                   @ to ground
!     | |                                   @
!     | |                                   @
!     | |                                   @
! -----.-------------------------------------------------------------------|--------------
!   source                                  xg                             x=100 km
!
!                                     CB     C'B'
! similar triangles ABC and AB'C' -> ---- = ------ <=>
!                                     BA     B'A
!
!  htot - xl(0)     xl(100) - xl(0)                      xg                                   htot - xloc
! --------------- = ---------------- <=> htot - xloc = ------(xl100 - xloc) <=> xg = 100 * ----------------, in km 
!     xg - 0            100 - 0                         100                                  xl100 - xloc
!
! Special cases: htot = xloc  -> xg =   0 km (direct downward mixing of plume) 
!                htot = xl100 -> xg = 100 km
!
! B. (flag = 0) Plume is released inside the mixing layer
!
! ------------- xl (mixing height) -------------------------.-------------------------------------------------------------                                                                    .
!                                                  .
!                                         .                      
!                                .                               
!                        .                                       
!                .                                                 
!         .                                                                                     
!  htot.
!      |  .    
!      |     . 
!      |        .
!      |           .
!      |              .
!      |                 .
!      |                    .
! hbron|                       .
!     | |                         .                                       
!     | |                            .                          
!     | |                               .                          
!     | |                                  .                     
!     | |                                     .                         
! -----.----------------------------------------------------|-----------------
!   source                                                  xg
!
!    sigma_z = dispg*x**disph  <=> x = (sigma_z/dispg)**(1/disph);
!    xg is the location where the plume is fully mixed, this means that there the width of the plume (sigma_z)
!    equals the mixing height (xloc) -> xg = (xloc/dispg)**(1/disph);
!    Here we assume no mixing layer growth with distance 
!
   IF (htot .LT. (htt/2. - EPS_DELTA)) THEN
      htot = htt/2.
   ELSE IF (htot .LT. (htt - EPS_DELTA)) THEN
      htot = (htt - htot)/2. + htot ! = 1/2 (htt + htot)
   ELSE
      CONTINUE
   ENDIF

   ! istab > 4 -> stable classes; in this case we may have stacks that emit 
   ! above the mixing layer 
   IF ((istab .GT. 4) .AND. (htot .GT. (xloc + EPS_DELTA))) THEN
      
      ! A. (flag = 1) htot > xloc, so plume starts above mixing layer
      IF (xl100 .GT. (xloc + EPS_DELTA)) THEN
         xg = (htot - xloc)*100000./(xl100 - xloc) ! in m
      ELSE
         xg = 100000.
      ENDIF
      IF ((xg .GT. (disx + EPS_DELTA)) .OR. (xg .LT. (0. - EPS_DELTA))) THEN
         xg = 100000.
      ENDIF
      flag = 1
      !write(*,'(a,4(1x,e12.5))') 'ops_depoparexpA/xloc,xl100,htot,xg: ',xloc,xl100,htot,xg
   ELSE
      ! B. (flag = 0) istab <= 4 OR htot <= xloc (plume starts inside mixing layer)
      ! sigma_z = dispg*x**disph  <=> x = (sigma_z/dispg)**(1/disph);
      ! xg is the location where the plume is fully mixed, this means that there the width of the plume (sigma_z)
      ! equals the mixing height (xloc) -> xg = (xloc/dispg)**(1/disph);
      xg = (xloc/dispg(istab))**(1./DISPH(istab))
      !write(*,'(a,4(1x,e12.5))') 'ops_depoparexpB/xloc,dispg(istab),DISPH(istab),xg: ',xloc,dispg(istab),DISPH(istab),xg
   ENDIF
   IF (xg .LT. (radius - EPS_DELTA)) THEN
      xg = radius
   ENDIF
!
!  Compute ux0 = wind speed at the beginning of the trajectory (near source) at plume height;
!  this is needed in order to compute the mass loss, since the wind speed increases 
!  with increasing sigma_z (especially for low sources).
!
   IF (htot .LT. (1. - EPS_DELTA)) THEN
      hf = 1.
   ELSE IF (htot .GT. (HUMAX + EPS_DELTA)) THEN
      hf = HUMAX
   ELSE
      hf = htot
   ENDIF
   CALL ops_wvprofile(z0_src, hf, uster_src, ol_src, ux0)
!
!  Compute source depletion (brondepl << "bron" = source, depl << depletion) and compute source depletion factors cdn, cq1, cq2
!  (to compute a depleted source strength) and a gradient factor cgt (to correct for the fact that concentrations at the 
!  surface are lower than the plume average).
!





   CALL ops_brondepl(disx, xg, c, ux0, ueff, sigz, vg50trans, xl, istab, xloc, xl100, vw10, pcoef, virty, radius, zm,           &
                  &  ra4_rcp, raz_rcp, rc_rcp, rb_rcp, z0_src, ol_src, uster_src, htot, ra4src, rb_src, rcsrc, qbstf, vg0tra,   &
                  &  onder, flag, vchem, vnatpri, diameter, dispg, cgt, cgt_z, cdn, ugem, hf, a, cq1, cq2, uxr, zu, sigzr, dxeff, error)
!
!  In order to compute utr = average wind speed over the trajectory, the plume is split into three parts
!  (x: source receptor distance, R: radius area source, u: wind speed):
!  1. plume inside area source                                    0 < x < R,     u = uxr
!  2. plume reaches the ground, but is not yet fully mixed        R < x < xg,    u = ugem
!  3. plume is fully mixed over the mixing layer                 xg < x,         u = ueff
!  
!  1+2+3: x*utr = R*uxr + (xg-R)*ugem + (x-xg)*ueff
!  1+2  : x*utr = R*uxr + (xg-R)*ugem
!  1    : utr = uxr = ueff (see ops_brondepl: inside an area source, we have uxr = ueff)
!
   IF (disx .GT. (xg + EPS_DELTA)) THEN
      utr = ((radius*uxr) + (xg - radius)*ugem + (disx - xg)*ueff)/disx
   ELSE IF (disx .GT. (radius + EPS_DELTA)) THEN
      utr = ((radius*uxr) + (disx - radius)*ugem)/disx
   ELSE
      utr = ueff
   ENDIF
ENDIF
!
! Compute cch = source depletion factor as a result of chemical conversion and wet deposition; 3.5 OPS report;
! factor 3600 seconds per hour and 100 to convert loss rate from %/h to fraction per hour
! dxeff: effective distance over which deposition takes place within an area source
! a    : effective distance for chemical conversion and wet deposition
!
IF (disx .LE. (radius + EPS_DELTA)) THEN
   a = dxeff
ELSE
   a = disx - dxeff 
ENDIF
cch = EXP( - (a/utr*(vnatpri + vchem)/360000.))
!
! Check whether 0 <= cq1 <= 1 AND 0 <= cq2 <= 1 AND cch <= 1; if not write a warning message
!
IF ((cq1 .LT. (0. - EPS_DELTA)) .OR. (cq1 .GT. (1. + EPS_DELTA)) .OR. (cq2 .LT. (0. - EPS_DELTA)) .OR.                          &
 &  (cq2 .GT. (1. + EPS_DELTA)) .OR. (cch .GT. (1. + EPS_DELTA))) THEN
   IF (.NOT. ops_openlog(error)) GOTO 9999
   WRITE(fu_log,'("WARNING: OPS has detected a value outside limits", " in routine ",A)') ROUTINENAAM(:LEN_TRIM(ROUTINENAAM))
   WRITE (fu_log, '(''  stab, disx, xl, sigz, sigzr, dxeff, ueff, uxr, cq1, cq2, cch, c:'', i4, 5f10.0, 6f10.4)') istab,        &
  &  disx, xl, sigz, sigzr, dxeff, ueff, uxr, cq1, cq2, cch, c/cq1
   WRITE (fu_log, '(''  vchem, vnatpri, utr, a, x_rcp, y_rcp, bronx, brony:'', 6f10.0, 2i10)')                                  &
  &  vchem, vnatpri, utr, a, xm, ym, bx, by
 ENDIF
!
! Compute combined effect of source depletion inside or outside area source;
! Gaussian part of plume (not yet homogeneously mixed) 
!
cq2 = cq2*cq1
!
! Compute depletion factor due to dry deposition of heavy plume; based on work of Onderdelinden; no ref 
!
!       htt - pldaling         htt + pldaling
! p1 = ----------------, p2 = ----------------  (sqrt(2) = 1.414
!       sqrt(2)*sigz           sqrt(2)*sigz
!
!                                 1                              1                           1                            
! htot > 0 ->       cq2 = 1 - ----------*EXP( -p1^2)*[------------------------  - -------------------------]
!                              SQRT(PI)                p1 + SQRT(p1^2 + 4/PI)       p2 + SQRT(p2^2 + 4/PI))
! 
! 
!                                 1                              1                           1                            
! htot = 0 -> p1 = -p1, cq2 = ----------*EXP( -p1^2)*[------------------------  + -------------------------]
!                              SQRT(PI)                p1 + SQRT(p1^2 + 4/PI)       p2 + SQRT(p2^2 + 4/PI))
! 
!
IF (ABS(grof - 1.) .LE. EPS_DELTA) THEN
   pldaling = htt - htot
   p1 = (htt - pldaling)/(1.414*sigz)
   p2 = (htt + pldaling)/(1.414*sigz)
   IF (htot .GT. (0. + EPS_DELTA)) THEN
      cq2 = (1 - 1/SQRT(PI)*EXP( -p1*p1)* (1/(p1 + SQRT(p1*p1 + 4/PI)) - 1/(p2 + SQRT(p2*p2 + 4/PI))))
   ELSE
      p1  = -p1
      cq2 = 1/(SQRT(PI))*(EXP( -p1*p1)* (1/(p1 + SQRT(p1*p1 + 4/PI)) + 1/(p2 + SQRT(p2*p2 + 4/PI))))
   ENDIF
ENDIF
RETURN
9999 CALL ErrorCall(ROUTINENAAM, error)

RETURN

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : par_nat
! DESCRIPTION        : Compute rain intensity and the wet deposition loss rate for primary components vnatpri
! AUTHOR             : OPS-support   
! SYSTEM DEPENDENCIES: NON-ANSI F77
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE par_nat(regenk, rint, buil, zf, isek, iseiz, mb, disx, radius, diameter, ueff, xl, onder, sigz, htot, gasv, dg,      &
                &  knatdeppar, scavcoef, routpri, kdeel, irev, c, qbstf, virty, twt, pr, cratio, ri, a, vnatpri)

! CONSTANTS
REAL*4                                           :: PS                         ! 
PARAMETER   (PS    = 1.e6/(2.*PI))
REAL*4                                           :: TWETZ(NSEK)                ! duration of rain shower in summer
REAL*4                                           :: TWETW(NSEK)                ! duration of rain shower in winter
REAL*4                                           :: RIW(NSEK)                  ! rain intensity winter
REAL*4                                           :: RIZ(NSEK)                  ! rain intensity summer
REAL*4                                           :: CMND(NMONTH)               ! monthly correction shower duration
REAL*4                                           :: EPSILON(NPARTCLASS)        ! 

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: regenk                     ! 
REAL*4,    INTENT(IN)                            :: rint                       ! 
REAL*4,    INTENT(IN)                            :: buil                       ! 
REAL*4,    INTENT(IN)                            :: zf                         ! 
INTEGER*4, INTENT(IN)                            :: isek                       ! 
INTEGER*4, INTENT(IN)                            :: iseiz                      ! 
INTEGER*4, INTENT(IN)                            :: mb                         ! 
REAL*4,    INTENT(IN)                            :: disx                       ! 
REAL*4,    INTENT(IN)                            :: radius                     ! 
REAL*4,    INTENT(IN)                            :: diameter                   ! 
REAL*4,    INTENT(IN)                            :: ueff                       ! wind speed at effective transport height heff; 
                                                                               ! for short distances heff = plume height;
                                                                               ! for large distances heff = 1/2 mixing height;
                                                                               ! heff is interpolated for intermediate distances.
REAL*4,    INTENT(IN)                            :: xl                         ! 
REAL*4,    INTENT(IN)                            :: onder                      ! 
REAL*4,    INTENT(IN)                            :: sigz                       ! 
REAL*4,    INTENT(IN)                            :: htot                       ! 
LOGICAL,   INTENT(IN)                            :: gasv                       ! 
REAL*4,    INTENT(IN)                            :: dg                         ! 
INTEGER*4, INTENT(IN)                            :: knatdeppar                 ! 
REAL*4,    INTENT(IN)                            :: scavcoef                   ! 
REAL*4,    INTENT(IN)                            :: routpri                    ! in-cloud scavenging ratio for primary component
                                                                               ! (rout << rain-out = in-cloud) [-]
INTEGER*4, INTENT(IN)                            :: kdeel                      ! 
LOGICAL,   INTENT(IN)                            :: irev                       ! 
REAL*4,    INTENT(IN)                            :: c                          ! 
REAL*4,    INTENT(IN)                            :: qbstf                      ! 
REAL*4,    INTENT(IN)                            :: virty                      ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: twt                        ! 
REAL*4,    INTENT(OUT)                           :: pr                         ! 
REAL*4,    INTENT(OUT)                           :: cratio                     ! 
REAL*4,    INTENT(OUT)                           :: ri                         ! 
REAL*4,    INTENT(OUT)                           :: a                          ! 
REAL*4,    INTENT(OUT)                           :: vnatpri                    ! wet deposition loss rate for primary components [%/h]

! LOCAL VARIABLES
REAL*4                                           :: twet                       ! 
REAL*4                                           :: treis                      ! 
REAL*4                                           :: h                          ! thickness over which wet deposition takes place [m]
REAL*4                                           :: hl                         ! 
REAL*4                                           :: vnatrain                   ! wet deposition loss rate for rainout (in-cloud) [%/h]
REAL*4                                           :: epsi                       ! 
REAL*4                                           :: beta                       ! 
REAL*4                                           :: lambda0                    ! 
REAL*4                                           :: vnatwash                   ! wet deposition loss rate for washout (below-cloud) [%/h]

! DATA figure 4.1 OPS-report (depends on particle class) and
! Slinn W.G.N (1983) Predictions for particle deposition to vegetative surfaces. Atmospheric Environment 16, 1785-1794.
DATA EPSILON /.00012, .0003, .0005, .31, .9, 1./
!
! Shower duration and rain intensity averaged over 12 wind sectors; determined at 1985-04-18 over the period 19810101 -
! 19850301 from 12 KNMI rain stations, separately for summer and winter. Only to be used if these parameters
! are not available in the meteo statistics file.
!
DATA TWETW /1.5, 1.7, 1.6, 1.6, 2.0, 1.7, 1.6, 1.7, 1.5, 1.3, 1.2, 1.2/
DATA TWETZ /1.6, 1.6, 1.4, 1.3, 1.2, 1.2, 1.2, 1.1, 1.2, 1.3, 1.4, 1.7/
DATA RIW /1.1, 0.9, 1.0, 0.9, 0.9, 0.9, 1.1, 1.2, 1.2, 1.4, 1.4, 1.2/
DATA RIZ /1.6, 1.6, 1.5, 1.7, 1.9, 2.0, 1.9, 2.1, 2.0, 1.7, 1.7, 1.4/
!
! Monthly correction shower duration and rain intensity:
!
DATA CMND /0.8, 0.9, 1.2, 0.8, 1.0, 1.2, 1.2, 1.0, 0.8, 1.2, 1.0, 0.9/

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! rain intensity ri [mm/h]
! average duration of a rainfall period (Euler) twet [h]
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

IF (regenk .GT. (0. + EPS_DELTA)) THEN
!
!  ri   = rain intensity in mm/h
!  twet = tau_w,Euler = average duration of a rainfall period [h]  (4.23) OPS report
!  buil = shower duration (from meteo statistics file);
!         buil is computed per day; this means that every hour gets the same value for buil.
!         (buil << "bui" = shower, l << length)
!
   IF ((rint .GT. (0. + EPS_DELTA)) .AND. (buil .GT. (0. + EPS_DELTA))) THEN
      twet = buil*.7 
      ri   = rint
   ELSE
!
!     Meteo parameters rint OR buil not available in meteo statistics;
!     interpolate between average summer and winter values:
      twet = (zf*TWETZ(isek)) + ((1. - zf)*TWETW(isek))
      ri   = (zf*RIZ(isek)) + ((1. - zf)*RIW(isek))
!
!     Correction of twet [h] and ri [mm/h] for one month;
!     iseiz = 4 -> one month in winter -> correction needed
!
      IF (iseiz .EQ. 4) THEN       ! IF (iseiz .EQ. 4 .OR. iseiz .EQ. 5) THEN   
         twet = twet/CMND(mb)
         ri   = ri*CMND(mb)
      ENDIF
   ENDIF

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  Average duration of a rainfall period (Lagrange) twt [h]
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Check whether the receptor is inside an area source
!  and determine a = effective travel distance over which deposition takes place
!  a = diameter/4 is based on simulations with a large number of point sources.
!
   IF (disx .LE. (radius + EPS_DELTA)) THEN
      a = diameter/4.
   ELSE
      a = disx - diameter/4.
   ENDIF
!
!  Determine treis = travel time [h] ("reis"= travel)
!   
   treis = a/(ueff*3600.)
!
!  twt  = tau_w       = average duration of a rainfall period, dependent on source - receptor distance [h]  (4.23) OPS report
!  twet = tau_w,Euler = average duration of a rainfall period [h]  (4.23) OPS report

!  twt depends on the travel time; note that exp(-y) ~ 1 - y
!  1. x small -> twt ~ 0.4*treis
! 
   twt = 1.0*twet*(1. - EXP( - 0.4*treis/twet))                              ! 950119
   IF (twt .LT. (.01 - EPS_DELTA)) THEN
      twt = .01
   ENDIF
!
!  Compute thickness h over which wet deposition takes place;
!  (onder = 0 -> plume completely above mixing height)
!  
   IF (ABS(onder - 0.) .LE. EPS_DELTA) THEN
      h = 2.*sigz
   ELSE
      h = xl
   ENDIF
   
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  Distribution factor between washout (below cloud) and rainout (in cloud) pr [-]
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Distribution of washout (below cloud) and rainout (in cloud) depending of plume height and mixing height;
!  pr = distribution factor, 0 <= pr <= 1; pr = 0 -> washout (below-cloud), pr = 1 -> rainout (in-cloud)
!  (4.19), (4.20) ... OPS report; a = factor cw in OPS report 4.19
!
   IF (radius .GT. (0. + EPS_DELTA)) THEN
      
      ! Area source; check for inside or outside area source
      
      IF (disx .LT. (radius - EPS_DELTA)) THEN
         hl = xl - htot + sigz*(radius - disx)/radius 
         a  = 3.
      ELSE
         hl = xl - htot - (radius**3)/(200*disx**2) ! radius = sa/2 -> extra factor 2**3 = 8 accounts for factor 1600 in OPS report
         a  = 1.
      ENDIF
   ELSE
      ! point source
      hl = xl - htot
      a  = 1.
   ENDIF
   IF ((ABS(onder - 0) .LE. EPS_DELTA) .OR. (hl .LT. (0. - EPS_DELTA))) THEN
      hl = 0.
   ENDIF
   pr = EXP(-(hl+5)**2/(2*sigz*sigz*a)) 
   
   ! Correction near source (travel time < 1 hour); disx/(ueff*3600) is travel time in h:
   pr = pr*AMIN1(1., disx/(ueff*3600.)) ! 950316 
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  Washout (below-cloud) loss rate vnatwash [%/h] (gasses, irreversible)
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!  Parameterisation of (irreversible) washout coefficient of gasses (A.J Janssen en H.M ten Brink (ECN)) 
!  droplet spectrum according to Best, smallest droplet radius .0125 cm.
!  4.22 OPS report: LAMBDA_eff = (Pp/twt)*(1 - exp(-LAMBDA*twt)
!  Pp = regenk  : probability of wet deposition [-] (regen << rain, k << kans = chance)
!  LAMBDA       : wet scavenging rate [1/h]
!  twt  = tau_w = average duration of a rainfall period, dependent on source - receptor distance [h]  (4.23) OPS report
!  4.17 OPS report: LAMBDA = alpha1*(D_g**alpha2)*(Ri**alpha3); 
!                   alpha1 = 1.21, alpha2 = 0.744, alpha3 = 0.628; dg = diffusion coefficient [cm2/s]
!
   IF (gasv) THEN
      vnatwash = regenk*100./twt*(1. - EXP( -1.21*twt*ri**.628*dg**.744))
   ENDIF

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  Rainout (in-cloud) loss rate vnatrain [%/h] 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!  Parameterisation of rain out (in-cloud), considered as a total loss process by rain, 
!  when a plume is mixed homogeneously in the mixing layer.
!  vnatrain = loss rate due to rain out [%/h]
!  4.22 OPS report: LAMBDA_eff = (Pp/twt)*(1 - exp(-LAMBDA*twt)
!
   IF (knatdeppar .EQ. 1) THEN
!
!     scavenging rate LAMBDA [1/h] = scavcoef/100; scavcoef [%/h] is defined in ops_init
!
      vnatrain = regenk*100./twt*(1 - EXP( -scavcoef/100.*twt))
   ELSE
!
!     scavenging ratio W = routpri (rout << rain-out = in-cloud) has been defined, 
!     either read (ops_read_ctr) or set inside OPS
!     (see ops_resist_rek for acidifying components or RORATIO for particles)
!     LAMBDA = W Ri/h (6.1 new OPS report); plume completely above mixing height -> h = 2 sigma_z 
!                                           otherwise ->  h = zi.
!     Ri in mm/h, zi = h in m -> extra factor 1000.
!
      vnatrain = regenk*100./twt*(1. - EXP( - routpri*twt*ri/1000./h))
   ENDIF

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  Washout (below-cloud) loss rate vnatwash [%/h] (particles)
!  Ratio cratio = C(z=0)/C_average, with C_average = averaged mixing layer concentration
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
   IF (.NOT.gasv) THEN
!
!     Parametrisation of wash out (below-cloud) of particles according to Slinn
!     (4.18) in OPS-report: LAMBDA = alpha4*epsilon*(Ri**alpha5);
!                           beta = 1 - alpha5, lambda0 = alpha4, epsi = epsilon of figure 4.1.
!     vnatwash = loss rate due to wash out (below-cloud) [%/h]
!
      epsi     = EPSILON(kdeel)
      beta     = .184
      lambda0  = 1.326
      vnatwash = regenk*100./twt*(1 - EXP( - twt*epsi*lambda0*ri**(1. - beta)))
   ELSE
!
!     gas
!     Wash out (below-cloud), rain out (in-cloud); in-cloud scavenging is more efficient than below-cloud;
!     therefore vnatwash must be smaller than vnatrain.
!
      IF (vnatwash .GT. (vnatrain + EPS_DELTA)) THEN
         vnatwash = vnatrain
      ENDIF
      
      IF (irev) THEN
!
!        Reversible wet deposition
!        cratio = ratio surface concentration (= c) and concentration at full mixing in mixing layer (needed for reversible wet deposition). 
!        (4.16) OPS report, figure 6.1 new OPS report
!
         cratio = c/(qbstf*NSEK*PS/(ueff*(disx + virty)*xl))
         IF (cratio .GT. (1. + EPS_DELTA)) THEN
            cratio = 1.
         ENDIF
      ELSE
!
!        irreversible wet deposition
!        
         cratio = 1.
      ENDIF
   ENDIF
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  Wet deposition loss rate for primary components vnatpri [%/h] 
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!  Combine wash out and rain out (4.19 OPS report)
!
   vnatpri = cratio*(vnatwash*(1. - pr) + vnatrain*pr)
ELSE
!
!  precipitation probability = 0
!  
   vnatpri = 0.
ENDIF

RETURN
END SUBROUTINE par_nat

!-------------------------------------------------------------------------------------------------------------------------------

END SUBROUTINE ops_depoparexp
