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
! DESCRIPTION        : Compute source depletion (brondepl << "bron" = source, depl << depletion).
!                      (1) Source depletion
!                      In a source depletion model, the loss of airborne material due to deposition or other removal processes
!                      is accounted for by appropriately reducing the source strength as a function of down-wind distance.
!                      (2) Surface correction
!                      Since more material is deposited near the ground surface, a vertical concentration profile is established 
!                      that is characterised by a gradient factor cgt.
!                      Dry deposition at ground surface -> use c0_undepl_mix.
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_brondepl
implicit none

contains

SUBROUTINE ops_brondepl(varin,do_proc, disx, disxx, xg, c0_undepl_mix, ux0, ueff, sigz, vd_eff_trj_zra, xl, ircp, istab, xloc, xl100, vw10, pcoef, virty, radius, zm, &
                     &  ra_rcp_4, ra_rcp_zrcp, rc_eff_rcp_4_pos, rb_rcp, z0_src, ol_src, uster_src, htot, ra_src_4, rb_src, rc_eff_src_4_pos, qbstf, &
                     &  vd_trj_z0, onder, flag, vchem, vnatpri, diameter, dispg, cgt, cgt_z, cdn, ugem, hf, cq1, cq2, uxr, zu, &
                     &  sigzr, dxeff, error)
use m_ops_varin, only: Tvarin
use m_commonconst_lt
use m_ops_tdo_proc, only: tdo_proc
use m_ops_meteo
use m_error
use m_ops_vertdisp

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_brondepl')

! SUBROUTINE ARGUMENTS - INPUT
TYPE(Tvarin), INTENT(IN)                         :: varin
TYPE(Tdo_proc), INTENT(IN)                       :: do_proc                    ! options to switch on/off specific processes
REAL,      INTENT(IN)                            :: disx                       ! linear distance between source and receptor [m] (here only used for debug write statement)
REAL,      INTENT(IN)                            :: disxx                      ! effective travel distance between source and receptor [m] 
REAL,      INTENT(IN)                            :: xg                         ! location where phase II of source depletion starts, see section 5.2 in OPS 4.5.2. doc
REAL,      INTENT(IN)                            :: c0_undepl_mix              ! undepleted concentration at z = 0 m (only due to part of plume inside the mixing layer) [ug/m3]
REAL,      INTENT(IN)                            :: ux0                        ! wind speed near source at plume height (m/s)
REAL,      INTENT(IN)                            :: ueff                       ! wind speed at effective transport height heff (m/s); 
                                                                               ! for short distances heff = plume height;
                                                                               ! for large distances heff = 1/2 mixing height;
                                                                               ! heff is interpolated for intermediate distances.
REAL,      INTENT(IN)                            :: sigz                       ! vertical dispersion length [m]
REAL,      INTENT(IN)                            :: vd_eff_trj_zra             ! 
REAL,      INTENT(IN)                            :: xl                         ! maximal mixing height over transport distance [m] (extrapolated when x > 1000km, largest distance category in meteo statistics; xl = 2.*htt when xl < htt) 
INTEGER,   INTENT(IN)                            :: ircp                       ! index of receptorpoint (here only used in debug write statement)
INTEGER,   INTENT(IN)                            :: istab                      ! index of stability class 
REAL,      INTENT(IN)                            :: xloc                       ! local mixing height (near source) [m]
REAL,      INTENT(IN)                            :: xl100                      ! mixing height at 100 km [m]
REAL,      INTENT(IN)                            :: vw10                       ! wind speed at 10 m height [m/s]
REAL,      INTENT(IN)                            :: pcoef                      ! coefficient in wind speed power law
REAL,      INTENT(IN)                            :: virty                      ! distance virtual point source - centre area source [m]
REAL,      INTENT(IN)                            :: radius                     ! 
REAL,      INTENT(IN)                            :: ra_rcp_4                   ! aerodynamic resistance at receptor, 4 m height [s/m]
REAL,      INTENT(IN)                            :: ra_rcp_zrcp                ! aerodynamic resistance at receptor, height zrcp [s/m]
REAL,      INTENT(IN)                            :: rc_eff_rcp_4_pos           ! effective canopy resistance at receptor, 4 m height, no re-emission (Rc is positive) [s/m]
REAL,      INTENT(IN)                            :: rb_rcp                     ! boundary layer resistance at receptor [s/m] 
REAL,      INTENT(IN)                            :: z0_src                     ! roughness length at source; from z0-map [m]
REAL,      INTENT(IN)                            :: ol_src                     ! 
REAL,      INTENT(IN)                            :: uster_src                  ! 
REAL,      INTENT(IN)                            :: htot                       ! plume height at receptor, including plume descent due to heavy particles [m]
REAL,      INTENT(IN)                            :: ra_src_4                   ! aerodynamic resistance at source, 4 m height [s/m] 
REAL,      INTENT(IN)                            :: rb_src                     ! boundary layer resistance at source [s/m] 
REAL,      INTENT(IN)                            :: rc_eff_src_4_pos           ! effective canopy resistance at source, 4 m height, no re-emission (Rc is positive) [s/m]
REAL,      INTENT(IN)                            :: qbstf                      ! source strength current source (for current particle class) [g/s]
REAL,      INTENT(IN)                            :: vd_trj_z0                  ! deposition velocity for trajectory, height z0 [m/s]; used in source depletion factor cq2
REAL,      INTENT(IN)                            :: onder                      ! fraction of mass inside the mixing layer. 
                                                                               ! Mass flux of that part of the plume that is below the mixing layer 
                                                                               ! and that takes part in the deposition process = onder*qbstf
INTEGER,   INTENT(IN)                            :: flag                       ! stable meteo class and stack emitting above mixing layer 
REAL,      INTENT(IN)                            :: vchem                      ! chemical conversion rate [%/h]
REAL,      INTENT(IN)                            :: vnatpri                    ! wet deposition loss rate for primary components [%/h]
REAL,      INTENT(IN)                            :: diameter                   ! 
REAL,      INTENT(IN)                            :: dispg                      ! coefficient for vertical dispersion coefficient sigma_z; sigma_z = dispg*x^disp [-]
REAL,      INTENT(IN)                            :: zm                         ! z-coordinate of receptor points (m)

! SUBROUTINE ARGUMENTS - I/O
REAL,      INTENT(INOUT)                         :: cgt                        ! gradient factor at 4 m height [-]. At input (1 - grad), at output (1 - grad) (1 - exp[-t/tau]). (grad = vd(zra)/vd(4))
REAL,      INTENT(INOUT)                         :: cgt_z                      ! gradient factor at receptor height zm [-]. At input (1 - grad), at output (1 - grad) (1 - exp[-t/tau])
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record


! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: cdn                        ! source depletion ratio for dry deposition for phase 3 (plume fully mixed over mixing layer)
REAL,      INTENT(OUT)                           :: ugem                       ! average wind speed depending on phase of plume development (m/s)
REAL,      INTENT(INOUT)                         :: hf                         ! effective transport height [m]
                                                                               ! NB: hf is INOUT because it remains unchanged when disxx <= xg-EPS_DELTA

REAL,      INTENT(OUT)                           :: cq1                        ! source depletion ratio for all removal processes for phase 1 (area source)
REAL,      INTENT(OUT)                           :: cq2                        ! source depletion ratio for dry deposition for phase 2 (plume not yet mixed over mixing layer) 
REAL,      INTENT(INOUT)                         :: uxr                        ! wind speed representative for plume over area source (m/s)
                                                                               ! NB: uxr is INOUT because it remains unchanged when radius <= EPS_DELTA
REAL,      INTENT(INOUT)                         :: zu                         ! representative plume height (m), taking into account reflection at the top of the mixing layer and at the ground surface
                                                                               ! NB: zu is INOUT because it remains unchanged when radius <= EPS_DELTA
                                                                               !     and also when disxx <= radius + EPS_DELTA
REAL,      INTENT(INOUT)                         :: sigzr                      ! vertical dispersion length calculated for an area source  [m]
                                                                               ! NB: sigzr is INOUT because it remains unchanged when radius <= EPS_DELTA

REAL,      INTENT(OUT)                           :: dxeff                      ! effective distance over which deposition takes place within an area source = (S/4)*exp[-k*t] (MS: includes conversion rates for various loss terms??)

! LOCAL VARIABLES
REAL                                             :: a                          ! representative distance between source and receptor
REAL                                             :: kt_area                    ! help variable (k_chem + k_wetdep + k_drydep_src)*(diameter/(2*uxr)) possibly with corrections for large kt_area
REAL                                             :: cxx                        ! representative concentration (undepleted) for plume in phase 2
REAL                                             :: xx                         ! representative distance for plume in phase 2
REAL                                             :: sigzxg                     ! sigma_z at xx
REAL                                             :: xlxg                       ! 
REAL                                             :: uxg                        ! 
REAL                                             :: s2                         ! 
REAL                                             :: k_drydep_src               ! dry deposition rate = vd/sigma_z for area source, 4 m height [1/s]
REAL                                             :: sh                         ! 
REAL                                             :: al                         ! 
real                                             :: humax_scaled
character*13                                     :: debugnam                   ! name for debug write statement since ops_vertdisp is called 3 times in various circumstances: either at_areasource, at_rcp_with_meteo_src, at_rcp_with_meteo_rcp 

humax_scaled = varin%varin_unc%unc_meteo%xl_fact * humax

!-------------------------------------------------------------------------------------------------------------------------------
!
!
! ------------- xl (mixing height) -------------------------.-------------------------------------------------------------                                                                    .
!                                                  .
!                                         .                      3 phases of plume development which play a role in dry deposition:
!                                .                               1. inside an area source 
!                        .                                       2. plume reaches the ground, but is not yet fully mixed
!              .                                                 3. plume is fully mixed over the mixing layer.
!       .                                                        
!  htt -
!      |  .    -  
!      |     .        - central
!      |        .               - axis 
!      |           .                   - of          
!      |              .                       - plume                               ---- xl (mixing height) ---------------------.--- 
!      |                 .                             -                                                                    .
!      |                    .                                 -                                                        .
! hbron|                       .                                    - htot                                        .
!     | |                         .                                                                         .    
!     | |                            .                                                                 .
!     | |                               .                                                          .
!     | |                                  .                                                   .      
!     | |                                     .                                            area source
! -----.-----------------------------------------.2222222222.3333333.33333333        -----|11111111111|22222222222222222222222222.33333333333
!   source           no deposition               xg              receptor                                     
!
! Note: the mixing height is also rising as function of the distance.
!
! Compute source depletion ratio = ratio of source strengths of depleted and undepleted source = 
!
!             x
!             /  vd(z)
!     = exp[- | ------- D (x) dx ]  4.11 OPS report
!             /    u     z
!            0
!
! Important parameters:
! c0_undepl_mix: undepleted concentration at z = 0 m (only due to part of plume inside the mixing layer) [ug/m3]
! cdn   : source depletion ratio for dry deposition for phase 3 (plume fully mixed over mixing layer);
!         cdn = 1 for phase 1 (area source) and phase 2
! cq1   : source depletion ratio for all removal processes for phase 1 (area source); cq1 = 1 for a point source
! cq2   : source depletion ratio for dry deposition for phase 2 (plume not yet mixed over mixing layer).
!         This region starts at x = 0 (source) or at the edge of an area source and ends at x = xg. 
!         cq2 = 1 outside this region, and also in the case that we have a stable meteo class (class 5,6) and 
!         a stack emitting above the mixing layer.
! disxx : location of receptor (source at x = 0) --> ! effective travel distance between source and receptor [m]
! xg    : location where the plume is just fully mixed
! xx    : representative distance for plume in phase 2
! cxx   : representative concentration (undepleted) for plume in phase 2
! sigzxg: sigma_z at location xx 
!

!-----------------------------------------------------------------------------------------------------------
! Compute cdn = source depletion ratio for dry deposition for phase 3 (plume fully mixed over mixing layer);
!         cdn = 1 for phase 1 (area source) and phase 2.
! Also compute other parameters, such as source-receptor distance, concentration, wind speed, effective height
! for different phases of the plume.
!-----------------------------------------------------------------------------------------------------------
IF (disxx .LT. (xg - EPS_DELTA)) THEN
  
  ! Receptor located where plume is in phase 1 (area source) or 2 (plume not yet fully mixed over mixing height):
  cdn = 1.

  ! Representative distance and concentration for phase 2 of plume is that of the receptor 
  xx  = disxx
  cxx = c0_undepl_mix

  ! Compute ugem and sigma_z for phase 1,2;
  ! ugem: average wind speed depending on phase of plume development.
  ! In phase 1 or 2, ugem is averaged over near source (ux0) and effective transport height (ueff):
  ugem   = (ux0 + ueff)/2.
  sigzxg = sigz

ELSE
 
   ! Receptor located where plume is in phase 3: homogeneously mixed part of the plume -> Dz(x) = 1/xl = 1/mixing_height,  4.14 OPS report
   !
   !             x
   !             /  vd(z)                  (x - xg) vd(z)
   ! cdn = exp[- | ------- dksi ] = exp[-  -------------- ], in which we substitute x = disxx, vd(z) = vd(zra) = vd(50) = vd_eff_trj_zra, u = ueff
   !             /  u xl                        u xl
   !           ksi=xg
   !
   !            

   if (do_proc%depl_drydep) then
      cdn = EXP( - ((disxx - xg)/ueff*vd_eff_trj_zra/xl))
   else
      cdn = 1.0
   endif

   ! Set representative distance for phase 2 of the plume and compute sigma_z there:
   xx     = xg
   sigzxg = dispg*(xx**DISPH(istab))
 
   ! Compute xlxg = mixing height at xg [m] by linear interpolation of xloc (near source at x = 0) and 
   ! xl100 (at 100 km from source), assuming a linear growth of the maximal mixing height with distance 
   !
   ! xl(xg) - xl(0)    xl(100) - xl(0)                     xg                                     xg
   ! --------------- = ---------------- <=> xlxg - xloc = ------(xl100 - xloc) <=> xlxg = xloc + ------(xl100 - xloc), in km 
   !     xg - 0            100 - 0                         100                                    100
 
   xlxg   = xloc + xg/100000.*(xl100 - xloc) ! in m
 
   ! Compute wind speed uxg, using a power law.
   ! ux0 : wind speed at the beginning of the trajectory (near source) at plume height
   ! uxg : wind speed at (x,z) = (xg,mixing_height/2)
   ! ugem: average wind speed depending on phase of plume development; for phase 3, ugem
   !       is the average of near source wind speed (ux0) and uxg
   ! c0_undepl_mix: undepleted concentration at z = 0 m (only due to part of plume inside the mixing layer) [ug/m3]
   !
   !
   !   |--------------------|------------------------------------------------|
   !  x=0 (source)         xg (fully mixed)                                 x=disxx (receptor)
   !
   ! cxx : undepleted concentration at xg; is used in the computation of cq2 (phase 2); we cannot use concentration c
   !       (at receptor) here since the receptor is further away (where the plume is in phase 3); 
   !       compute cxx (upstream) using linear scaling of concentration c at receptor with distance (factor disxx/xg)
   !       and mixing layer height (factor xl/xlxg):
   !       cxx     disxx   xl
   !       ---   = ----- ------ (scaling factors are > 1, so cxx > c).
   !        c       xg    xlxg
   !
   IF (xlxg/2. .GT. (humax_scaled + EPS_DELTA)) THEN
     hf = humax_scaled
   ELSE
     hf = xlxg/2.
   ENDIF
   uxg   = vw10*((hf/10.)**pcoef)
   ugem  = (ux0 + uxg)/2.
   cxx   = (disxx + virty)/(xg + virty)*c0_undepl_mix*xl/xlxg
ENDIF

!
!-----------------------------------------------------------------------------------------------------------
! Compute cgt = 1 - concentration gradient between heights z1 = 4 m and z2 = zra = 50 m.
!-----------------------------------------------------------------------------------------------------------
! A concentration gradient is caused by the deposition flux; 
! it is assumed to start when the plume is well mixed in the mixing layer.
!
!                                          vd(z2)
! Note: at input  cgt = (1 - grad) = (1 - -------)
!                                          vd(z1)
!
!       at output cgt = (1 - grad) (1 - exp[-t/tau]), with t   = travel time = disxx/ugem
!                                                          tau = z1/vd(z1), 4.10 OPS report
!                                                          new formula for tau;
!
! According to OPS report 4.9, the concentration gradient at height z1 is: c(x,z1) = (1 - cgt) c(x,z2).
!                                                            
! Note: for t = 0   : cgt = 0;        c(x,z1) = (1 - cgt)c(x,z2) = c(x,z2)    (No concentration gradient installed yet)
!
!                                                                                   vd(z2)
!       for t -> Inf: cgt = 1 - grad; c(x,z1) = (1 - cgt)c(x,z2) = grad c(x,z2) = ------- c(x,z2)  (vertical concentration profile)
!                                                                                   vd(z1)

! First set a = representative distance between source and receptor;
! a = radius/4,        for a receptor inside the area source (see dxeff in ops_depoparexp)
! a = x - radius/(4/3) = 
!   = x - (3/4)radius, for a receptor outside the area source.
!                      At the edge of the area source, a = radius - (3/4)radius = radius/4, 
!                      so we have a continuous transition here.
if (disxx.lt.(radius-EPS_DELTA)) THEN
   a=radius/4
ELSE
   a=disxx-radius/1.33
ENDIF

! Compute cgt = (1 - grad)(1 - exp[-t/tau]), t = a/ugem, tau = z1/vd(z1) = 4*(Ra(4) + Rb + Rc):
cgt   = cgt  *(1.-exp(-a/(4.*(ra_rcp_4    + rb_rcp + rc_eff_rcp_4_pos)*ugem))) 
cgt_z = cgt_z*(1.-exp(-a/(zm*(ra_rcp_zrcp + rb_rcp + rc_eff_rcp_4_pos)*ugem)))
!-----------------------------------------------------------------------------------------------------------
! Compute help variables for an area source, such as sigma_z, wind speeds ugem and uxr, effective height hf 
! and dry deposition rate k_drydep_src = vd(z=4 m)/sigma_z.
!-----------------------------------------------------------------------------------------------------------
! Source depletion inside an area source is computed separately, because the behaviour of sigma_z is different.
! We choose here an approach as if sigma_z is constant within the area source and the concentration 
! within height sigma_z is homogeneously distributed.
!
IF (radius .GT. (0. + EPS_DELTA)) THEN

  ! Receptor outside area source
  IF (disxx .GT. (radius + EPS_DELTA)) THEN

    ! Compute vertical dispersion coefficient s2 
    ! x = downwind distance = 2*radius = sa; see OPS report figure 3.6
    ! zu : representative plume height (m), taking into account reflection 
    !      at the top of the mixing layer and at the ground surface
    ! s2 : sigma_z at x
    ! 
    debugnam = 'at_areasource'
    CALL ops_vertdisp(varin, z0_src, xl, ol_src, uster_src, htot, radius*2., ircp, istab, debugnam, uxr, zu, s2, error) ! output uxr is not used here
    ! NB: s2 may have been adjusted with a multiplication factor for the sensitivity analysis. However, for the sensitivity analysis, the total sigzr must be adjusted
    ! and not only s2. Hence we divide s2 by the multiplication factor, and multiply the total sigzr with the multiplication factor. 
    s2 = s2 / varin%varin_unc%sigz_fact
       
    sigzr = s2/alog((htot + s2)/htot) ! (see OPS-doc/dispersion, bookmark area_source_sigma_z) for sigma_zi = htot 
                                      ! s2 = sigma_z(r2), s1 = sigma_z(r1) = 0
    ! Adjustment relevant for sensitivity analyses. Multiplication factor is 1.0 by default.
    sigzr = varin%varin_unc%sigz_fact * sigzr
    
    ! Compute uxr = wind speed representative for plume over area source; (x = near source, h = effective plume height area source hf)
    hf = (sigzr/4 + htot + 6.)/2. 
    CALL ops_wv_log_profile(z0_src, hf, uster_src, ol_src, varin%varin_meteo, uxr)

    ! Compute ugem = average wind speed, average between uxr (plume over area source) and ueff (at effective transport height)
    ugem = (uxr + ueff)/2.
  ELSE
    ! Receptor inside area source; ueff is the near source wind speed, so we can use uxr = ueff
    ! Note: ugem is not defined for a receptor inside area source; this is ok, because ugem
    !       is not used in this case (see ops_depoparexp).
    sigzr = sigz
    uxr   = ueff
  ENDIF
  IF (sigzr .GT. (xl100 + EPS_DELTA)) THEN
    sigzr = xl100
  ENDIF
  k_drydep_src = 1./((ra_src_4 + rb_src + rc_eff_src_4_pos)*sigzr)  
  !Edit
  ! Adjustments relevant for sensitivity analyses. Multiplication factors are 1.0 by default.
  k_drydep_src = varin%varin_unc%unc_sourcedepl%vd_drydep_pri_fact * k_drydep_src
  !End Edit

ENDIF

!-----------------------------------------------------------------------------------------------------------
! Compute cq2 = source depletion ratio for dry deposition for phase 2 
! (plume is not yet homogeneously mixed over the mixing layer).
!-----------------------------------------------------------------------------------------------------------
! This region starts at x = 0 (source) or at the edge of an area source and ends at x = xg; cq2 = 1 outside this region.
! cq2 is not computed for stable meteo classes (5,6) AND a stack emitting above mixing layer; in this case the source
! depletion only takes place in phase 3 and cq2 = 1.

!               2                       h**2
! D (x) = ------------------- exp [- --------------],  4.12 OPS report,
!  z      sqrt(2 pi) sigma_z          2 sigma_z**2
!
! S = diameter area source; edge of area source at S/2
! => 5.24 OPS doc 4.5.2:
!             
!              2 beta vd(z) (x-R)   x C(x,0) u   2 pi 
! cq2 = exp(- --------------------- --------- ------ ), 
!                     u                Q0       Ns
! beta  : = a1, see (5.25) OPS doc 4.5.2
! vd(z) : deposition velocity at z = z0 (= vd_trj_z0)
! C(x,0): concentration at receptor, taking into account the gradient due to deposition = cxx*(1-cgt)
! Q0    : mass flux of that part of the plume that is below the mixing layer and that takes part in the deposition process = onder*qbstf
! ueff  : wind speed at receptor at effective transport height heff; for short distances heff = plume height;
!         for large distances heff = 1/2 mixing height; heff is interpolated for intermediate distances.
! ugem  : average wind speed depending on phase of plume development.
!
!                 2.*al*1.e-6*vd_trj_z0*(xx - radius)  (xx + virty)*cxx*ueff*(1.-cgt)   2 pi   
!    cq2 = exp( - -----------------------------------  -------------------------------- -----)  
!                              ugem                         onder*qbstf                  12     
! or rearranged:
!                  2.*al*1.e-6*vd_trj_z0*2*pi*(xx + virty)*cxx*ueff*(xx - radius)*(1.-cgt)     
!    cq2 = exp( - ------------------------------------------------------------------------) .
!                              ugem*onder*qbstf*12                                               
!
! Note: error in (2.5.15) thesis van Jaarsveld with factor 2 instead of 4

IF (disxx .GT. (radius + EPS_DELTA) .AND. xg .GT. (radius + EPS_DELTA) .and. do_proc%depl_drydep) THEN

  ! Compute help variables sh = sigma_z**2/h**2 and al = beta:
  sh  = (sigzxg/htot)**2
  al  = 8./PI*sh/((1.+SQRT(1.+8/PI*sh))**2)
  
  ! If NOT (stable meteo class and stack emitting above mixing layer), compute cq2 (else cq2 = 1):
  IF (flag .NE. 1) THEN 
    cq2 = EXP( -(2.*al/qbstf*1.e-6*vd_trj_z0/12*2*PI*(xx + virty)* cxx*ueff/ugem/onder*(xx - radius)*(1.-cgt)))
  ELSE
     cq2 = 1.0
  ENDIF
ELSE
   cq2 = 1.0
ENDIF

!-----------------------------------------------------------------------------------------------------------------
! Compute cq1 = source depletion ratio for all removal processes for phase 1 (area source) (cq1 = 1 for a point source). 
!-----------------------------------------------------------------------------------------------------------------
!
!               2
! D (x) = --------------------, 4.12 OPS report,
!  z      sqrt(2 pi) sigma_z
!
! Because all terms inside the integral are independent of x, we get:
!             x                                             
!             /      2         vd(z)                       x        2          1          
! cq1 = exp[- | ----------- ----------- dx ] = exp[-vd(z) ---  ----------- --------- ] 
!             / sqrt(2 pi)   u sigma_z                     u    sqrt(2 pi)  sigma_z        
!            0                                             
! x: effective distance over which deposition takes place within an area source = diameter/4 (thesis van Jaarsveld 2.5.13)
! u: wind speed representative for area source = uxr
! Note: k_drydep_src = vd/sigma_z for area source
!
! source code:
!                k_drydep_src*dxeff           vd/sigma_z (S/4) exp(-kt_area/18)           vd (S/4) exp(-((vchem+vnatpri+vd/sigma_z) S)/(18 uxr ))     
!   cq1 = EXP( - ------------------) = EXP( - ---------------------------) = EXP( - --------------------------------------------------------)   
!                      uxr                               uxr                                      uxr sigma_z                                   
!
! factor 3.6e5: conversion from percentage per hour -> fraction per second
! note: k_drydep_src = vd/sigma_z [1/s] (same dimension as vchem*3.6e5 and vnatpri*3.6e5) 

!
! k_drydep_src = conversion rate for dry deposition      = vd/sigma_z          [1/s]
! k_wetdep     = conversion rate for wet deposition      = vnatpri/(3600*100)  [1/s]
! k_chem       = conversion rate for chemical conversion = vchem/(3600*100)    [1/s]
!
! dxeff = distance over which deposition takes place = (S/4)*exp[-k*t], 
!         with t = travel time from centre of the area source to the edge = (radius/uxr) = diameter/(2*uxr)
!         Parameterisation based on comparison with surface depletion model.
!
! help variable kt_area = k*t = (k_chem + k_wetdep + k_drydep_src)*(diameter/(2*uxr)) =  
!                       = ((vchem + vnatpri)/3.6e5 + k_drydep_src)*(diameter/(2*uxr)) 
!
!                       .49  
! kt_area > 15 -> kt_area = 15 (kt_area/15)    
!
! See also ops_seccmp for dxeff = effective distance over which deposition takes place
!

! for the area sources that are nowadays (2011) common in OPS, this correction is probably not important

! Initialize kt_area (otherwise error occurs when writing debut output and source is not an area source.
kt_area = -999.
IF (ABS(radius) .GT. EPS_DELTA .and. do_proc%depl_drydep) THEN
  kt_area = ((vchem + vnatpri)/3.6e5 + k_drydep_src)*diameter/uxr
  IF (kt_area .GT. (15. + EPS_DELTA)) THEN 
    kt_area = (kt_area/15.)**.49*15.
  ENDIF
  dxeff = diameter/4.*EXP( -kt_area/18.) ! factor 18 (or 9) is calibration factor from multipointsource test
  cq1   = EXP( -(k_drydep_src*dxeff/uxr))
ELSE
  dxeff = 0.
  cq1   = 1.
ENDIF

if (error%debug) write(*,'(3a,2(1x,i6,";"),99(1x,e12.5,";"))') & 
   trim(ROUTINENAAM),',A; ','ircp; istab; disx; disxx; cgt; cgt_z; cdn; ugem; hf; kt_area; cq1; cq2; uxr; zu; sigzr; dxeff; ', &
                             ircp, istab, disx, disxx, cgt, cgt_z, cdn, ugem, hf, kt_area, cq1, cq2, uxr, zu, sigzr, dxeff

RETURN
END SUBROUTINE ops_brondepl

end module m_ops_brondepl
