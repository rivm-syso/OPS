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
! DESCRIPTION        : Get parameters needed for dry deposition, wet deposition and chemical conversion.
!                      Not only deposition velocities, but especially those parameters (source depletion factors) that deal 
!                      with the mass loss due to these processes over a certain travel distance. In an analytical model, 
!                      this mass loss is not summed on a step-by-step basis along a trajectory, but instead, the integral 
!                      of the mass loss at the receptor is computed. Also gradient factor that describes the vertical 
!                      concentration gradient due to deposition. 
!
!                      When computing mass loss due to deposition, different situations can occur, with respect to the transport
!                      of substances from a source:
!                      a. vertical Gaussian distribution near the source
!                         aa. plume above the mixing layer
!                         ab. plume within the mixing layer, does not touch the earth's surface
!                         ac. plume within the mixing layer, reflecting at earth's surface or at the top of the mixing layer
!                      b. homogeneous mixing within a wind sector at larger distances from the source.
!
!                      For an area source, we have to consider the possibility that a receptor is within the area source.

!                      For wet deposition, the cloud base is assumed to be equal to the mixing height.
!
!                      In this routine, we distinguish gaseous and particulate substances; the deposition of particles depends
!                      on the particle size. Heavy particles cause a plume to descend. Coarse particles (>20 um) are treated 
!                      separately; it is assumed that the deposition velocity of coarse particles equals half the sedimentation
!                      velocity.
! 
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_depoparexp

implicit none

contains 

SUBROUTINE ops_depoparexp(varin, do_proc, kdeel, c0_undepl_mix, qbstf, ra_rcp_4, ra_rcp_zra, ra_rcp_zrcp, rb_rcp, sigz, ueff, virty, gasv, &
                       &  ircp, istab, grof, xvghbr, xvglbr, regenk, rint, buil, zf, isec1, iseiz, mb, disx, disxx, radius, &
                       &  xl, onder, dg, knatdeppar, scavcoef, irev, htt, xloc, xl100, vw10, pcoef, vchem, dispg, htot, &
                       &  error, pr, twt, cratio, rc_eff_rcp_4_pos, grad, utr, routpri, vd_eff_trj_zra, rkc, ri, vnatpri, &
                       &  cgt, cgt_z, cq2, cdn, cch, z0_src, ol_src, uster_src, rc_eff_trj_4_pos, rc_eff_src_4_pos, ra_src_4, rb_src, &
                       &  ra_trj_zra, rb_trj, vd_coarse_part, xm, ym, zm, bx, by)
                       
use m_commonconst_lt, only: DISPH, EPS_DELTA, HUMAX, NPARTCLASS, PI
USE m_commonfile
USE m_error
use m_ops_brondepl
USE m_ops_logfile
USE m_ops_meteo
use m_ops_tdo_proc
use m_ops_varin, only: Tvarin

IMPLICIT NONE

! CONSTANTS
CHARACTER(len=*), parameter :: ROUTINENAAM = 'ops_depoparexp'

! VDDEEL = deposition velocity per particle class [m/s];
! Sehmel G.A. and Hodgson W.H. (1980) A model for predicting dry deposition of particles and gases to environmental surfaces.
! AIChE Symposium Series 86, 218-230. 
REAL,   PARAMETER :: VDDEEL(NPARTCLASS) = (/.00030, .0013, .0046, .009, .024, .054/)

! SUBROUTINE ARGUMENTS - INPUT
TYPE(Tvarin), INTENT(IN)                         :: varin
TYPE(Tdo_proc), INTENT(IN)                       :: do_proc                    ! options to switch on/off specific processes
INTEGER,   INTENT(IN)                            :: kdeel                      ! index of particle class
REAL,      INTENT(IN)                            :: c0_undepl_mix              ! undepleted concentration at z = 0 m (only due to part of plume inside the mixing layer) [ug/m3]
REAL,      INTENT(IN)                            :: qbstf                      ! source strength current source (for current particle class) [g/s] (= qbpri in calling routine)
REAL,      INTENT(IN)                            :: ra_rcp_4                   ! aerodynamic resistance at receptor, 4 m height [s/m]
REAL,      INTENT(IN)                            :: ra_rcp_zra                 ! aerodynamic resistance at receptor, height zra [s/m];
                                                                               ! zra is height where concentration profile is undisturbed by deposition = 50 m
REAL,      INTENT(IN)                            :: ra_rcp_zrcp                ! aerodynamic resistance at receptor, height zrcp [s/m]
REAL,      INTENT(IN)                            :: rb_rcp                     ! boundary layer resistance at receptor [s/m] 
REAL,      INTENT(IN)                            :: sigz                       ! vertical dispersion length [m]
REAL,      INTENT(IN)                            :: ueff                       ! wind speed at effective transport height heff; 
                                                                               ! for short distances heff = plume height;
                                                                               ! for large distances heff = 1/2 mixing height;
                                                                               ! heff is interpolated for intermediate distances.
REAL,      INTENT(IN)                            :: virty                      ! distance virtual point source - centre area source [m]
LOGICAL,   INTENT(IN)                            :: gasv                       ! 
INTEGER,   INTENT(IN)                            :: ircp                       ! index of receptorpoint (here only used in debug write statement)
INTEGER,   INTENT(IN)                            :: istab                      ! index of stability class  
REAL,      INTENT(IN)                            :: grof                       ! = 1 -> coarse particles
REAL,      INTENT(IN)                            :: xvghbr                     ! ratio effective dry deposition velocity over transport distance and average dry deposition velocity over transport distance for high sources [-] 
REAL,      INTENT(IN)                            :: xvglbr                     ! ratio effective dry deposition velocity over transport distance and average dry deposition velocity over transport distance for low sources [-]
REAL,      INTENT(IN)                            :: regenk                     ! rain probability [-] 
REAL,      INTENT(IN)                            :: rint                       ! rain intensity [mm/h]
REAL,      INTENT(IN)                            :: buil                       ! length of rain event [0.01 hours]
REAL,      INTENT(IN)                            :: zf                         ! 
INTEGER,   INTENT(IN)                            :: isec1                      ! first of two interpolating wind sectors; only used if rint or buil are not availabel
                                                                               
INTEGER,   INTENT(IN)                            :: iseiz                      ! 
INTEGER,   INTENT(IN)                            :: mb                         ! 
REAL,      INTENT(IN)                            :: disx                       ! linear distance between source and receptor [m] (here only used for debug write statement)
REAL,      INTENT(IN)                            :: disxx                      ! effective travel distance between source and receptor [m] 
REAL,      INTENT(IN)                            :: radius                     ! 
REAL,      INTENT(IN)                            :: xl                         ! maximal mixing height over transport distance [m] (extrapolated when x > 1000km, largest distance category in meteo statistics; xl = 2.*htt when xl < htt)
REAL,      INTENT(IN)                            :: onder                      ! fraction of emission below mixing height [-]
REAL,      INTENT(IN)                            :: dg                         ! 
INTEGER,   INTENT(IN)                            :: knatdeppar                 ! 
REAL,      INTENT(IN)                            :: scavcoef                   ! 
LOGICAL,   INTENT(IN)                            :: irev                       ! 
REAL,      INTENT(IN)                            :: htt                        ! plume height at source, including plume rise [m]
REAL,      INTENT(IN)                            :: xloc                       ! local mixing height (near source) [m]
REAL,      INTENT(IN)                            :: xl100                      ! mixing height at 100 km [m]
REAL,      INTENT(IN)                            :: vw10                       ! wind speed at 10 m height [m/s]
REAL,      INTENT(IN)                            :: pcoef                      ! coefficient in wind speed power law
REAL,      INTENT(IN)                            :: vchem                      ! 
REAL,      INTENT(IN)                            :: dispg                      ! coefficient for vertical dispersion coefficient sigma_z; sigma_z = dispg*x^disp [-]
REAL,      INTENT(IN)                            :: z0_src                     ! roughness length at source; from z0-map [m]
REAL,      INTENT(IN)                            :: ol_src                     !
REAL,      INTENT(IN)                            :: uster_src                  !
REAL,      INTENT(IN)                            :: ra_src_4                   ! aerodynamic resistance at source, 4 m height [s/m]
REAL,      INTENT(IN)                            :: rb_src                     ! boundary layer resistance at source [s/m] 
REAL,      INTENT(IN)                            :: rb_trj                     ! boundary layer resistance for trajectory [s/m]
REAL,      INTENT(IN)                            :: ra_trj_zra                 ! aerodynamic resistance for trajectory, height zra [s/m];
                                                                               ! zra is height where concentration profile is undisturbed by deposition = 50 m  
REAL,      INTENT(IN)                            :: xm                         ! x-coordinate of receptor (m RDM)
REAL,      INTENT(IN)                            :: ym                         ! y-coordinate of receptor (m RDM)
REAL,      INTENT(IN)                            :: zm                         ! z-coordinate of receptor points (m)
INTEGER,   INTENT(IN)                            :: bx 
INTEGER,   INTENT(IN)                            :: by
REAL,      INTENT(IN)                            :: rc_eff_rcp_4_pos           ! effective canopy resistance at receptor, 4 m height, no re-emission (Rc is positive) [s/m] 
REAL,      INTENT(IN)                            :: rc_eff_src_4_pos           ! effective canopy resistance at source, 4 m height, no re-emission (Rc is positive) [s/m]
REAL,      INTENT(IN)                            :: rc_eff_trj_4_pos           ! effective canopy resistance for trajectory, 4 m height, no re-emission (Rc is positive) [s/m]
REAL,      INTENT(IN)                            :: routpri                    ! in-cloud scavenging ratio for primary component [-] (rout << rain-out = in-cloud)

! SUBROUTINE ARGUMENTS - I/O
REAL,      INTENT(INOUT)                         :: htot                       ! plume height at receptor, including plume descent due to heavy particles [m]
REAL,      INTENT(INOUT)                         :: cratio                     ! ratio surface concentration (= c0_undepl_mix) and concentration at full mixing in mixing layer (needed for reversible wet deposition).
REAL,      INTENT(INOUT)                         :: vd_coarse_part             ! deposition velocity coarse particles [m/s] 
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: pr                         ! Distribution factor between washout (below cloud) and rainout (in cloud). pr = 0 -> washout, pr = 1 -> rainout. 
REAL,      INTENT(OUT)                           :: twt                        ! average duration of a rainfall period, dependent on source - receptor distance [h] 
REAL,      INTENT(OUT)                           :: grad                       ! depositon velocity gradient over height = vd(zra)/vd(4)
REAL,      INTENT(OUT)                           :: utr                        ! average wind speed over the trajectory (m/s)
REAL,      INTENT(OUT)                           :: vd_eff_trj_zra             ! effective deposition velocity over trajectory, taking into account amount of time that plume is above mixing
                                                                               ! height and no deposition takes place [m/s]
REAL,      INTENT(OUT)                           :: rkc                        ! obsolete factor 
REAL,      INTENT(OUT)                           :: ri                         ! rain intensity [mm/h].
REAL,      INTENT(OUT)                           :: vnatpri                    ! wet deposition loss rate for primary components [%/h]
REAL,      INTENT(OUT)                           :: cgt                        ! gradient factor at 4 m height [-] (= 1-grad; grad = vd(zra)/vd(4))
REAL,      INTENT(OUT)                           :: cgt_z                      ! gradient factor at receptor height zm [-]
REAL,      INTENT(OUT)                           :: cq2                        ! source depletion ratio for dry deposition for phase 2 (plume not yet mixed over mixing layer)
REAL,      INTENT(OUT)                           :: cdn                        ! source depletion ratio for dry deposition for phase 3 (plume fully mixed over mixing layer)
REAL,      INTENT(OUT)                           :: cch                        ! source depletion factor for wet deposition/chemical conversion

! LOCAL VARIABLES
INTEGER                                          :: flag                       ! stable meteo class and stack emitting above mixing layer 
REAL                                             :: a                          ! 
REAL                                             :: cq1                        ! source depletion ratio for all removal processes for phase 1 (area source)
REAL                                             :: diameter                   ! 
REAL                                             :: dxeff                      ! effective distance over which deposition takes place within an area source = (S/4)*exp[-k*t] (MS: includes conversion rates for various loss terms??)
REAL                                             :: grad_z                     ! height dependent grad
REAL                                             :: hf                         ! effective transport height [m] 
real                                             :: humax_scaled
REAL                                             :: p1                         ! 
REAL                                             :: p2                         ! 
REAL                                             :: pldaling                   ! 
REAL                                             :: sigzr                      ! vertical dispersion length calculated for an area source  [m]
REAL                                             :: ux0                        ! wind speed near source at plume height (m/s)
REAL                                             :: uxr                        ! wind speed representative for plume over area source (m/s)
REAL                                             :: ugem                       ! average wind speed depending on phase of plume development (m/s)
REAL                                             :: vd_trj_z0                  ! deposition velocity for trajectory, height z0 [m/s]; used in source depletion factor cq2
REAL                                             :: vd_uncorr_trj_zra          ! deposition velocity over trajectory, uncorrected for effect that plume is above mixing
                                                                               ! height and no deposition takes place [m/s]
REAL                                             :: xg                         ! 
REAL                                             :: zu                         ! 
!LOGICAL                                          :: ops_openlog                ! function for opening log file


!-------------------------------------------------------------------------------------------------------------------------------
!
! Initialisation
!
sigzr = 0.0
humax_scaled = varin%varin_unc%unc_meteo%xl_fact * HUMAX

! Square area source is represented by a circular area source with the same area;
! (area circle with radius r) = (area square with 1/2 side = radius) <=> pi*r**2 = (2*radius)**2 <=> 
! <=> r = (2/sqrt(pi))*radius <=> r = 1.128*radius
! See ops_virtdist: virty = (radius*12.)/PI*1.128
diameter = (virty*PI)/(6.*1.128) 

! Gas or fine particles (grof .ne. 1);
! compute deposition velocity vd = 1/(Ra + Rb + Rc) at different locations/heights and
! grad = depositon velocity gradient over height = vd(zra)/vd(4)
!
IF (ABS(grof - 1.) .GT. EPS_DELTA) THEN
   
   ! Gas or fine particles:
   vd_uncorr_trj_zra = 1./(ra_trj_zra + rb_trj + rc_eff_trj_4_pos)  
                                                                    
                                                                    ! On the other hand: would rc_eff_trj_4_pos not be equal to rc_eff_trj_zra_pos (which does not exist in the code)
                                                                    ! since this is used in vd_eff_trj_zra used in source depletion for dry deposition for phase 3 (well mixed) of the plume? 
                                                                    ! Note however that vd_eff_trj_zra is also used in determining e1_pri (source depletion factor for primary species), 
                                                                    ! as well as factor b for surface sources used to determine the distance over which production of secondary species takes place.                                                                    
                       ! write(*,'(a,4(1x,e12.5))') 'ops_depoparexp1/vd_uncorr_trj_zra,ra_trj_zra,rb_trj,rc_eff_trj_4_pos: ',vd_uncorr_trj_zra,ra_trj_zra,rb_trj,rc_eff_trj_4_pos
   vd_trj_z0         = 1./(rb_trj + rc_eff_trj_4_pos) ! Note: Ra(z0) = 0              ! 970809
   grad              = (ra_rcp_4    + rb_rcp + rc_eff_rcp_4_pos)/ (ra_rcp_zra + rb_rcp + rc_eff_rcp_4_pos)  
   grad_z            = (ra_rcp_zrcp + rb_rcp + rc_eff_rcp_4_pos)/ (ra_rcp_zra + rb_rcp + rc_eff_rcp_4_pos)  
   
ELSE
   ! Coarse particles (grof = 1);
   ! get deposition velocity vd = 1/(Ra + Rb + Rc) at different locations/heights and
   ! set grad = depositon velocity gradient over height = 1
   vd_coarse_part    = VDDEEL(kdeel)
   vd_uncorr_trj_zra = VDDEEL(kdeel)
   vd_trj_z0         = VDDEEL(kdeel)
   grad              = 1.
   grad_z            = 1.
ENDIF
!
! vd_uncorr_trj_zra = deposition velocity over trajectory, uncorrected for effect that plume is above mixing
!                     height and no deposition takes place [m/s]
! vd_eff_trj_zra    = effective deposition velocity over trajectory, taking into account amount of time that plume is above mixing
!                     height and no deposition takes place [m/s]
! xvglbr            = ratio effective dry deposition velocity over transport distance and 
!                     average dry deposition velocity over transport distance for low sources; parameter from meteo statistics [-]
!                     See OPS-doc/meteo, bookmark correction_factor_deposition_velocity
! xvghbr            = ratio effective dry deposition velocity over transport distance and 
!                     average dry deposition velocity over transport distance for high sources; parameter from meteo statistics [-]
!                     See OPS-doc/meteo, bookmark correction_factor_deposition_velocity
!
! Distinguish between very high, high and low sources in the computation of deposition velocity over a trajectory;
! very high source -> the effective dry deposition velocity is lower than for low sources, because high sources contribute
!                     relatively little at low wind speeds (e.g. in the night). The factor 0.7 is derived from model experiments.
IF (htot .GT. (350. + EPS_DELTA)) THEN
   vd_eff_trj_zra = xvghbr*vd_uncorr_trj_zra*.7

! Check whether source emits above the mixing layer or not:
!ELSE IF (htot .GT. (200. + EPS_DELTA)) THEN
ELSE IF (htot .GT. xloc) THEN 
   vd_eff_trj_zra = xvghbr*vd_uncorr_trj_zra
ELSE
   vd_eff_trj_zra = xvglbr*vd_uncorr_trj_zra
ENDIF
!Edit
! Adjustments relevant for sensitivity analyses. Multiplication factors are 1.0 by default.
vd_eff_trj_zra = varin%varin_unc%unc_sourcedepl%vd_drydep_pri_fact * vd_eff_trj_zra
vd_trj_z0 = varin%varin_unc%unc_sourcedepl%vd_drydep_pri_fact * vd_trj_z0 
!End Edit
!write(*,'(a,5(1x,e12.5))') 'ops_depoparexp2/htot,vd_uncorr_trj_zra,xvglbr,xvghbr,vd_eff_trj_zra: ',htot,vd_uncorr_trj_zra,xvglbr,xvghbr,vd_eff_trj_zra
!
! rkc: obsolete factor
!
rkc = 1
!
! Compute wet deposition
!
CALL par_nat(varin%varin_unc, regenk, rint, buil, zf, isec1, iseiz, mb, disxx, radius, diameter, ueff, xl, onder, sigz, htot, gasv, dg,           &
          &  knatdeppar, scavcoef, routpri, kdeel, irev, c0_undepl_mix, qbstf, virty, twt, pr, cratio, ri, a, vnatpri)
!
! Gradient term; grad = vd(zra)/vd(4); assuming a constant flux over height, we derive:
!
!                                                  vd(zra)   c(4)
! F(zra) = F(4) <=> -vd(zra)*c(zra) = -vd(4)*c(4) <=> ------ = ------ <=> c(4) = grad*c(zra) 
!                                                   vd(4)   c(zra)
!                                vd(z2)
! Note: cgt = (1 - grad) = (1 - -------) is used as input for ops_brondepl (see there).
!                                vd(z1)
!
IF (do_proc%grad_drydep) THEN
   cgt   = 1. - grad
   cgt_z = 1. - grad_z      ! EvdS
ELSE
   cgt   = 0.0
   cgt_z = 0.0
ENDIF

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
! hbron|                       .                                    - htot - plume descent for heavy particles
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
      IF ((xg .GT. (disxx + EPS_DELTA)) .OR. (xg .LT. (0. - EPS_DELTA))) THEN
         xg = 100000.
      ENDIF
      flag = 1
      !write(*,'(a,4(1x,e12.5))') 'ops_depoparexpA/xloc,xl100,htot,xg: ',xloc,xl100,htot,xg
   ELSE
      ! B. (flag = 0) istab <= 4 OR htot <= xloc (plume starts inside mixing layer)
      ! sigma_z = dispg*x**disph  <=> x = (sigma_z/dispg)**(1/disph);
      ! xg is the location where the plume is fully mixed, this means that there the width of the plume (sigma_z)
      ! equals the mixing height (xloc) -> xg = (xloc/dispg)**(1/disph);
      xg = (xloc/dispg)**(1./DISPH(istab))
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
   ELSE IF (htot .GT. (humax_scaled + EPS_DELTA)) THEN
      hf = humax_scaled
   ELSE
      hf = htot
   ENDIF
   CALL ops_wv_log_profile(z0_src, hf, uster_src, ol_src, varin%varin_meteo, ux0)

   !  Compute source depletion (brondepl << "bron" = source, depl << depletion) and compute source depletion factors cdn, cq1, cq2
   !  (to compute a depleted source strength) and a gradient factor cgt (to correct for the fact that concentrations at the 
   !  surface are lower than the plume average).
   !
   
   
   
   
   CALL ops_brondepl(varin, do_proc, disx, disxx, xg, c0_undepl_mix, ux0, ueff, sigz, vd_eff_trj_zra, xl, ircp, istab, xloc, xl100, vw10, pcoef, virty, radius, zm, &
                  &  ra_rcp_4, ra_rcp_zrcp, rc_eff_rcp_4_pos, rb_rcp, z0_src, ol_src, uster_src, htot, ra_src_4, rb_src, rc_eff_src_4_pos, qbstf, vd_trj_z0, &
                  &  onder, flag, vchem, vnatpri, diameter, dispg, cgt, cgt_z, cdn, ugem, hf, cq1, cq2, uxr, zu, sigzr, dxeff, error)
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
   IF (disxx .GT. (xg + EPS_DELTA)) THEN
      utr = ((radius*uxr) + (xg - radius)*ugem + (disxx - xg)*ueff)/disxx
   ELSE IF (disxx .GT. (radius + EPS_DELTA)) THEN
      utr = ((radius*uxr) + (disxx - radius)*ugem)/disxx
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
IF (disxx .LE. (radius + EPS_DELTA)) THEN
   a = dxeff
ELSE
   a = disxx - dxeff 
                    
                    ! However, this occurs via dxeff which is rather confusing as this suggests a distance. Also now there seems to be a double exp? Check this.
ENDIF

! cch = EXP( - (a/utr*(vnatpri + vchem)/360000.))
cch = 1.0
if (do_proc%chem)   cch = cch*EXP( - (a/utr*vchem/360000.))
if (do_proc%depl_wetdep) cch = cch*EXP( - (a/utr*vnatpri/360000.))

! Check whether 0 <= cq1 <= 1 AND 0 <= cq2 <= 1 AND cch <= 1; if not write a warning message
IF ((cq1 .LT. (0. - EPS_DELTA)) .OR. (cq1 .GT. (1. + EPS_DELTA)) .OR. (cq2 .LT. (0. - EPS_DELTA)) .OR.                          &
 &  (cq2 .GT. (1. + EPS_DELTA)) .OR. (cch .GT. (1. + EPS_DELTA))) THEN
   IF (.NOT. ops_openlog(error)) GOTO 9999
   WRITE(fu_log,'("WARNING: OPS has detected a value outside limits", " in routine ",A)') ROUTINENAAM(:LEN_TRIM(ROUTINENAAM))
   WRITE (fu_log, '(''  stab, disxx, xl, sigz, sigzr, dxeff, ueff, uxr, cq1, cq2, cch, c0_undepl_mix:'', i4, 5f10.0, 6f10.4)') istab,        &
  &  disxx, xl, sigz, sigzr, dxeff, ueff, uxr, cq1, cq2, cch, c0_undepl_mix/cq1
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

END SUBROUTINE ops_depoparexp

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : par_nat
! DESCRIPTION        : Compute rain intensity and the wet deposition loss rate for primary components vnatpri
! AUTHOR             : HvJ/Franka Loeve (Cap Volmac)
! SYSTEM DEPENDENCIES: NON-ANSI F77
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE par_nat(varin_unc, regenk, rint, buil, zf, isec1, iseiz, mb, disxx, radius, diameter, ueff, xl, onder, sigz, htot, gasv, dg,      &
                &  knatdeppar, scavcoef, routpri, kdeel, irev, c0_undepl_mix, qbstf, virty, twt, pr, cratio, ri, a, vnatpri)

use m_commonconst_lt, only: PI, NPARTCLASS, NSEK, NMONTH, EPS_DELTA
use m_ops_varin, only: Tvarin_unc
IMPLICIT NONE

! CONSTANTS
REAL                                             :: PS                         ! 
PARAMETER   (PS    = 1.e6/(2.*PI))
REAL                                             :: TWETZ(NSEK)                ! duration of rain shower in summer
REAL                                             :: TWETW(NSEK)                ! duration of rain shower in winter
REAL                                             :: RIW(NSEK)                  ! rain intensity winter
REAL                                             :: RIZ(NSEK)                  ! rain intensity summer
REAL                                             :: CMND(NMONTH)               ! monthly correction shower duration
REAL                                             :: EPSILON(NPARTCLASS)        ! 

! SUBROUTINE ARGUMENTS - INPUT
TYPE(Tvarin_unc), INTENT(IN)                     :: varin_unc
REAL,      INTENT(IN)                            :: regenk                     ! rain probability [-]
REAL,      INTENT(IN)                            :: rint                       ! rain intensity [mm/h] 
REAL,      INTENT(IN)                            :: buil                       ! length of rain event [0.01 hours]
REAL,      INTENT(IN)                            :: zf                         ! 
INTEGER,   INTENT(IN)                            :: isec1                      ! 
INTEGER,   INTENT(IN)                            :: iseiz                      ! 
INTEGER,   INTENT(IN)                            :: mb                         ! 
REAL,      INTENT(IN)                            :: disxx                      ! effective travel distance between source and receptor [m]  
REAL,      INTENT(IN)                            :: radius                     ! 
REAL,      INTENT(IN)                            :: diameter                   ! 
REAL,      INTENT(IN)                            :: ueff                       ! wind speed at effective transport height heff; 
                                                                               ! for short distances heff = plume height;
                                                                               ! for large distances heff = 1/2 mixing height;
                                                                               ! heff is interpolated for intermediate distances.
REAL,      INTENT(IN)                            :: xl                         ! maximal mixing height over transport distance [m] (extrapolated when x > 1000km, largest distance category in meteo statistics; xl = 2.*htt when xl < htt)
REAL,      INTENT(IN)                            :: onder                      ! fraction of emission below mixing height [-]
REAL,      INTENT(IN)                            :: sigz                       ! vertical dispersion length [m]
REAL,      INTENT(IN)                            :: htot                       ! plume height at receptor, including plume descent due to heavy particles [m]
LOGICAL,   INTENT(IN)                            :: gasv                       ! 
REAL,      INTENT(IN)                            :: dg                         ! 
INTEGER,   INTENT(IN)                            :: knatdeppar                 ! 
REAL,      INTENT(IN)                            :: scavcoef                   ! 
REAL,      INTENT(IN)                            :: routpri                    ! in-cloud scavenging ratio for primary component [-] (rout << rain-out = in-cloud)
INTEGER,   INTENT(IN)                            :: kdeel                      ! 
LOGICAL,   INTENT(IN)                            :: irev                       ! 
REAL,      INTENT(IN)                            :: c0_undepl_mix              ! undepleted concentration at z = 0 m (only due to part of plume inside the mixing layer)                       

REAL,      INTENT(IN)                            :: qbstf                      ! 
REAL,      INTENT(IN)                            :: virty                      ! distance virtual point source - centre area source [m]

! SUBROUTINE ARGUMENTS - INPUT/OUTPUT
REAL,      INTENT(INOUT)                         :: twt                        ! average duration of a rainfall period, dependent on source - receptor distance [h] 
                                                                               ! INOUT, because twt remains unchanged when regenk<=EPS_DELTA
REAL,      INTENT(INOUT)                         :: pr                         ! Distribution factor between washout (below cloud) and rainout (in cloud). pr = 0 -> washout, pr = 1 -> rainout. 
                                                                               ! pr is INOUT, because pr remains unchanged when regenk<=EPS_DELTA
REAL,      INTENT(INOUT)                         :: cratio                     ! ratio surface concentration (= c0_undepl_mix) and concentration at full mixing in mixing layer (needed for reversible wet deposition).
                                                                               ! cratio is INOUT, because it remains unchanged when .not. gasv
REAL,      INTENT(INOUT)                         :: ri                         ! rain intensity [mm/h]. INOUT, because ri remains unchanged when regenk<=EPS_DELTA
REAL,      INTENT(INOUT)                         :: a                          ! INOUT, because a remains unchanged when regenk<=EPS_DELTA 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: vnatpri                    ! wet deposition loss rate for primary components [%/h]

! LOCAL VARIABLES
REAL                                             :: twet                       ! 
REAL                                             :: treis                      ! 
REAL                                             :: h                          ! thickness over which wet deposition takes place [m]
REAL                                             :: hl                         ! 
REAL                                             :: vnatrain                   ! wet deposition loss rate for rainout (in-cloud) [%/h]
REAL                                             :: epsi                       ! 
REAL                                             :: vnatwash                   ! wet deposition loss rate for washout (below-cloud) [%/h]
REAL                                             :: lambda_b                   ! Scavenging rate for below-cloud scavenging (1/h)

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
      twet = (zf*TWETZ(isec1)) + ((1. - zf)*TWETW(isec1))
      ri   = (zf*RIZ(isec1)) + ((1. - zf)*RIW(isec1))
!
!     Correction of twet [h] and ri [mm/h] for one month;
!     iseiz = 4 -> one month in winter -> correction needed
!
      IF (iseiz .EQ. 4) THEN       ! IF (iseiz .EQ. 4 .OR. iseiz .EQ. 5) THEN    FS
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
   IF (disxx .LE. (radius + EPS_DELTA)) THEN
      a = diameter/4.
   ELSE
      a = disxx - diameter/4.
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
      
      IF (disxx .LT. (radius - EPS_DELTA)) THEN
         hl = xl - htot + sigz*(radius - disxx)/radius 
         a  = 3.
      ELSE
         hl = xl - htot - (radius**3)/(200*disxx**2) ! radius = sa/2 -> extra factor 2**3 = 8 accounts for factor 1600 in OPS report
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
   
   ! Correction near source (travel time < 1 hour); disxx/(ueff*3600) is travel time in h:
   pr = pr*AMIN1(1., disxx/(ueff*3600.)) ! 950316 
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
      !Edit
      !Original code:
      !vnatwash = regenk*100./twt*(1. - EXP( -1.21*twt*ri**.628*dg**.744))
      !New code: 
      !Hulpvariabele lambda_b toegevoegd
      !Ik heb de volgorde in lambda_b ook veranderd zodat dit overeenkomt met OPS-beschrijving.
      !Indien akkoord: lokale variabele lambda_b toevoegen met comment 'Scavenging rate for below-cloud scavenging (1/h)'
      lambda_b = 1.21*(dg**.744)*(ri**.628)
      ! Adjustment relevant for sensitivity analyses. Multiplication factor is 1.0 by default.
      lambda_b = varin_unc%unc_sourcedepl%washout_pri_fact * lambda_b
      vnatwash = regenk*100./twt*(1. - EXP( -lambda_b*twt))
      !End Edit
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
!     either read (ops_read_ctr) or set inside OPS (ops_resist_rek).
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
      !Edit
      !Original code:
      !beta     = .184
      !lambda0  = 1.326
      !vnatwash = regenk*100./twt*(1 - EXP( - twt*epsi*lambda0*ri**(1. - beta)))
      !New code: 
      !Hulpvariabele lambda_b toegevoegd en lambda0 en beta verwijderd zodat dit nu consistent is met bepaling 
      !vnatwash voor gasv = 1 (daar worden ook niet de constanten expliciet benoemd). 
      !Bovendien is lambda0 in OPS-beschrijving alpha4 en (1-beta) gelijk aan alpha5 --> verwarrend.
      !Volgorde ook meteen aangepast overeenkomstig met OPS-beschrijving.
      !Indien akkoord: bovenstaande toelichting iets wijzigingen en lokale variabelen beta en lambda0 bovenaan subroutine verwijderen.
      !Deze worden nergens anders gebruikt.
      !Lokale variabele lambda_b juist toevoegen met comment 'Scavenging rate for below-cloud scavenging (1/h)'
      lambda_b = 1.326*epsi*ri**0.816
      ! Adjustment relevant for sensitivity analyses. Multiplication factor is 1.0 by default.
      lambda_b = varin_unc%unc_sourcedepl%washout_pri_fact * lambda_b
      vnatwash = regenk*100./twt*(1. - EXP( -lambda_b*twt))
      !End Edit
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
!        cratio = ratio surface concentration (= c0_undepl_mix) and concentration at full mixing in mixing layer (needed for reversible wet deposition). 
!        (4.16) OPS report, figure 6.1 new OPS report
!
         cratio = c0_undepl_mix/(qbstf*NSEK*PS/(ueff*(disxx + virty)*xl))
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


end module m_ops_depoparexp
