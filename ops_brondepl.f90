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
! LANGUAGE           : FORTRAN(HP-UX, HP-F77)
! DESCRIPTION        : Compute source depletion (brondepl << "bron" = source, depl << depletion).
!                      (1) Source depletion
!                      In a source depletion model, the loss of airborne material due to deposition or other removal processes
!                      is accounted for by appropriately reducing the source strength as a function of down-wind distance.
!                      (2) Surface correction
!                      Since more material is deposited near the ground surface, a vertical concentration profile is established 
!                      that is characterised by a gradient factor cgt.
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: NON-ANSI F77
! CALLED FUNCTIONS   : ops_vertdisp ops_wvprofile
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_brondepl(disx, xg, c, ux0, ueff, sigz, vg50trans, xl, istab, xloc, xl100, vw10, pcoef, virty, radius, zm,       &
                     &  ra4_rcp, raz_rcp, rc_rcp, rb_rcp, z0_src, ol_src, uster_src, htot, ra4src, rb_src, rcsrc, qbstf,       &
                     &  vg0tra, onder, flag, vchem, vnatpri, diameter, dispg, cgt, cgt_z, cdn, ugem, hf, a, cq1, cq2, uxr, zu, &
                     &  sigzr, dxeff, error)

USE m_commonconst
USE m_error

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_brondepl')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: disx                       ! 
REAL*4,    INTENT(IN)                            :: xg                         ! 
REAL*4,    INTENT(IN)                            :: c                          ! undepleted concentration at z = 0 m
                                                                               ! (without part of plume above mixing layer)
REAL*4,    INTENT(IN)                            :: ux0                        ! wind speed near source at plume height (m/s)
REAL*4,    INTENT(IN)                            :: ueff                       ! wind speed at effective transport height heff (m/s); 
                                                                               ! for short distances heff = plume height;
                                                                               ! for large distances heff = 1/2 mixing height;
                                                                               ! heff is interpolated for intermediate distances.
REAL*4,    INTENT(IN)                            :: sigz                       ! 
REAL*4,    INTENT(IN)                            :: vg50trans                  ! 
REAL*4,    INTENT(IN)                            :: xl                         ! 
INTEGER*4, INTENT(IN)                            :: istab                      ! 
REAL*4,    INTENT(IN)                            :: xloc                       ! 
REAL*4,    INTENT(IN)                            :: xl100                      ! 
REAL*4,    INTENT(IN)                            :: vw10                       ! 
REAL*4,    INTENT(IN)                            :: pcoef                      ! 
REAL*4,    INTENT(IN)                            :: virty                      ! 
REAL*4,    INTENT(IN)                            :: radius                     ! 
REAL*4,    INTENT(IN)                            :: ra4_rcp                    ! 
REAL*4,    INTENT(IN)                            :: raz_rcp                    ! EvdS: hoogte afhankelijkheid 
REAL*4,    INTENT(IN)                            :: rc_rcp                     ! 
REAL*4,    INTENT(IN)                            :: rb_rcp                     ! 
REAL*4,    INTENT(IN)                            :: z0_src                     ! roughness length at source; from z0-map [m]
REAL*4,    INTENT(IN)                            :: ol_src                     ! 
REAL*4,    INTENT(IN)                            :: uster_src                  ! 
REAL*4,    INTENT(IN)                            :: htot                       ! 
REAL*4,    INTENT(IN)                            :: ra4src                     ! 
REAL*4,    INTENT(IN)                            :: rb_src                      ! 
REAL*4,    INTENT(IN)                            :: rcsrc                      ! 
REAL*4,    INTENT(IN)                            :: qbstf                      ! 
REAL*4,    INTENT(IN)                            :: vg0tra                     ! 
REAL*4,    INTENT(IN)                            :: onder                      ! 
INTEGER*4, INTENT(IN)                            :: flag                       ! stable meteo class and stack emitting above mixing layer 
REAL*4,    INTENT(IN)                            :: vchem                      ! 
REAL*4,    INTENT(IN)                            :: vnatpri                    ! 
REAL*4,    INTENT(IN)                            :: diameter                   ! 
REAL*4,    INTENT(IN)                            :: dispg(NSTAB)               ! 
REAL*4,    INTENT(IN)                            :: zm                         ! z-coordinate of receptor points (RDM)

! SUBROUTINE ARGUMENTS - I/O
REAL*4,    INTENT(INOUT)                         :: cgt                        ! 
REAL*4,    INTENT(INOUT)                         :: cgt_z                      ! height dependent cgt
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record


! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: cdn                        ! 
REAL*4,    INTENT(OUT)                           :: ugem                       ! average wind speed depending on phase of plume development (m/s)
REAL*4,    INTENT(OUT)                           :: hf                         ! 
REAL*4,    INTENT(OUT)                           :: a                          ! 
REAL*4,    INTENT(OUT)                           :: cq1                        ! 
REAL*4,    INTENT(OUT)                           :: cq2                        ! 
REAL*4,    INTENT(OUT)                           :: uxr                        ! wind speed representative for plume over area source (m/s)
REAL*4,    INTENT(OUT)                           :: zu                         ! representative plume height (m), taking into account reflection 
                                                                               ! at the top of the mixing layer and at the ground surface
REAL*4,    INTENT(OUT)                           :: sigzr                      ! 
REAL*4,    INTENT(OUT)                           :: dxeff                      ! 

! LOCAL VARIABLES
REAL*4                                           :: cxx                        ! representative concentration (undepleted) for plume in phase 2
REAL*4                                           :: xx                         ! representative distance for plume in phase 2
REAL*4                                           :: sigzxg                     ! sigma_z at xx
REAL*4                                           :: xlxg                       ! 
REAL*4                                           :: uxg                        ! 
REAL*4                                           :: s2                         ! 
REAL*4                                           :: vdoppb                     ! 
REAL*4                                           :: sh                         ! 
REAL*4                                           :: al                         ! 

! SUBROUTINE AND FUNCTION CALLS
EXTERNAL ops_vertdisp
EXTERNAL ops_wvprofile

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
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
! c     : undepleted concentration at z = 0 m (without part of plume above mixing layer)
! cdn   : source depletion ratio for dry deposition for phase 3 (plume fully mixed over mixing layer);
!         cdn = 1 for phase 1 (area source) and phase 2
! cq1   : source depletion ratio for dry deposition for phase 1 (area source);
!         cq1 = 1 for a point source
! cq2   : source depletion ratio for dry deposition for phase 2 (plume not yet mixed over mixing layer).
!         This region starts at x = 0 (source) or at the edge of an area source and ends at x = xg. 
!         cq2 = 1 outside this region, and also in the case that we have a stable meteo class (class 5,6) and 
!         a stack emitting above the mixing layer.
! disx  : location of receptor (source at x = 0)
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
IF (disx .LT. (xg - EPS_DELTA)) THEN

  ! Receptor located where plume is in phase 1 (area source) or 2 (plume not yet fully mixed over mixing height):
  cdn = 1.

  ! Representative distance and concentration for phase 2 of plume is that of the receptor 
  xx  = disx
  cxx = c

  ! Compute ugem and sigma_z for phase 1,2;
  ! ugem: average wind speed depending on phase of plume development.
  ! In phase 1 or 2, ugem is averaged over near source (ux0) and effective transport height (ueff):
  ugem   = (ux0 + ueff)/2.
  sigzxg = sigz

ELSE
 
   ! Receptor located where plume is in phase 3: homogeneously mixed part of the plume -> Dz(x) = 1/xl = 1/mixing_height,  4.14 OPS report

   !             x
   !             /  vd(z)                  (x - xg) vd(z)
   ! cdn = exp[- | ------- dksi ] = exp[-  -------------- ], in which we substitute x = disx, vd(z) = vd(50) = vg50trans, u = ueff
   !             /  u xl                        u xl
   !           ksi=xg

   !          
   cdn    = EXP( - ((disx - xg)/ueff*vg50trans/xl))

   ! Set representative distance for phase 2 of the plume and compute sigma_z there:
   xx     = xg
   sigzxg = dispg(istab)*(xx**DISPH(istab))
 
   ! Compute xlxg = mixing height at xg [m] by linear interpolation of xloc (near source at x = 0) and 
   ! xl100 (at 100 km from source), assuming a linear growth of the maximal mixing height with distance 

   ! xl(xg) - xl(0)    xl(100) - xl(0)                     xg                                     xg
   ! --------------- = ---------------- <=> xlxg - xloc = ------(xl100 - xloc) <=> xlxg = xloc + ------(xl100 - xloc), in km 
   !     xg - 0            100 - 0                         100                                    100
 
   xlxg   = xloc + xg/100000.*(xl100 - xloc) ! in m
 
   ! Compute wind speed uxg, using a power law.
   ! ux0 : wind speed at the beginning of the trajectory (near source) at plume height
   ! uxg : wind speed at (x,z) = (xg,mixing_height/2)
   ! ugem: average wind speed depending on phase of plume development; for phase 3, ugem
   !       is the average of near source wind speed (ux0) and uxg
   ! c   : undepleted concentration at receptor at z = 0 m (without part of plume above mixing layer)


   !   |--------------------|------------------------------------------------|
   !  x=0 (source)         xg (fully mixed)                                 x=disx (receptor)

   ! cxx : undepleted concentration at xg; is used in the computation of cq2 (phase 2); we cannot use concentration c
   !       (at receptor) here since the receptor is further away (where the plume is in phase 3); 
   !       compute cxx (upstream) using linear scaling of concentration c at receptor with distance (factor disx/xg)
   !       and mixing layer height (factor xl/xlxg):
   !       cxx     disx    xl
   !       ---   = ----- ------ (scaling factors are > 1, so cxx > c).
   !        c       xg    xlxg

   IF (xlxg/2. .GT. (HUMAX + EPS_DELTA)) THEN
     hf = HUMAX
   ELSE
     hf = xlxg/2.
   ENDIF
   uxg   = vw10*((hf/10.)**pcoef)
   ugem  = (ux0 + uxg)/2.
   cxx   = (disx + virty)/(xg + virty)*c*xl/xlxg
ENDIF

!
!-----------------------------------------------------------------------------------------------------------
! Compute cgt = 1 - concentration gradient between heights z1 = 4 m and z2 = 50 m.
!-----------------------------------------------------------------------------------------------------------
! A concentration gradient is caused by the deposition flux; 
! it is assumed to start when the plume is well mixed in the mixing layer.
!
!                                          vg(z2)
! Note: at input  cgt = (1 - grad) = (1 - -------)
!                                          vg(z1)
!
!       at output cgt = (1 - grad) (1 - exp[-t/tau]), with t   = travel time = disx/ugem
!                                                          tau = z1/vd(z1), 4.10 OPS report
!                                                          new formula for tau;
!
! According to OPS report 4.9, the concentration gradient at height z1 is: c(x,z1) = (1 - cgt) c(x,z2).
!                                                          
! Note: for t = 0   : cgt = 0;        c(x,z1) = (1 - cgt)c(x,z2) = c(x,z2)    (No concentration gradient installed yet)
!
!                                                                                   vg(z2)
!       for t -> Inf: cgt = 1 - grad; c(x,z1) = (1 - cgt)c(x,z2) = grad c(x,z2) = ------- c(x,z2)  (vertical concentration profile)
!                                                                                   vg(z1)

! First set a = representative distance between source and receptor;
! a = radius/4,        for a receptor inside the area source (see dxeff in ops_depoparexp)
! a = x - radius/(4/3) = 
!   = x - (3/4)radius, for a receptor outside the area source.
!                      At the edge of the area source, a = radius - (3/4)radius = radius/4, 
!                      so we have a continuous transition here.
if (disx.lt.(radius-EPS_DELTA)) THEN
   a=radius/4
ELSE
   a=disx-radius/1.33
ENDIF

! Compute cgt = (1 - grad)(1 - exp[-t/tau]), t = a/ugem, tau = z1/vd(z1) = 4*(Ra(4) + Rb + Rc):
cgt   = cgt*(1.-exp(-a/(4.*(ra4_rcp+rb_rcp+rc_rcp)*ugem)))
cgt_z = cgt_z*(1.-exp(-a/(zm*(raz_rcp+rb_rcp+rc_rcp)*ugem)))

!-----------------------------------------------------------------------------------------------------------
! Compute help variables for an area source, such as sigma_z, wind speeds ugem and uxr, effective height hf 
! and vdoppb 
!-----------------------------------------------------------------------------------------------------------
! Source depletion inside an area source is computed separately, because the behaviour of sigma_z is different.
! We choose here an approach as if sigma_z is constant within the area source and the concentration 
! within height sigma_z is homogeneously distributed.
!
IF (radius .GT. (0. + EPS_DELTA)) THEN

  ! Receptor outside area source
  IF (disx .GT. (radius + EPS_DELTA)) THEN

    ! Compute vertical dispersion coefficient s2 
    ! x = downwind distance = 2*radius = sa; see OPS report figure 3.6
    ! zu : representative plume height (m), taking into account reflection 
    !      at the top of the mixing layer and at the ground surface
    ! s2 : sigma_z at x
    ! 
    CALL ops_vertdisp(z0_src, xl, ol_src, uster_src, htot, radius*2., uxr, zu, s2, error) ! output uxr is not used here
    sigzr = s2/alog((htot + s2)/htot) ! (see OPS-doc/dispersion, bookmark area_source_sigma_z) for sigma_zi = htot 
                                      ! s2 = sigma_z(r2), s1 = sigma_z(r1) = 0
  
    ! Compute uxr = wind speed representative for plume over area source; (x = near source, h = effective plume height area source hf)
    hf = (sigzr/4 + htot + 6.)/2. 
    CALL ops_wvprofile(z0_src, hf, uster_src, ol_src, uxr)

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
  vdoppb = 1./((ra4src + rb_src + rcsrc)*sigzr)
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
! => 5.20 new OPS report:
!           
!              2 beta vd(z) (x-R)   x C(x) u   2 pi 
! cq2 = exp(- --------------------- --------- ------ ) and C(x) = cxx*(1-cgt), Q0 = qbstf 
!                     u                Q0       Ns
!          
! ueff: wind speed at receptor at effective transport height heff; for short distances heff = plume height;
!       for large distances heff = 1/2 mixing height; heff is interpolated for intermediate distances.
! ugem: average wind speed depending on phase of plume development
!
! source code:
!                 2.*al*1.e-6*vg0tra*(xx - radius)     (xx + virty)*cxx*ueff*(1.-cgt)   2 pi    FS
!    cq2 = EXP( - ----------------------------------  -------------------------------- ------) 
!                              ugem                         onder*qbstf                  12  

! cxx/onder is the concentration including the part above the mixing layer ??
! Note error in (2.5.15) thesis van Jaarsveld with factor 2 instead of 4
cq2 = 1.
IF (disx .GT. (radius + EPS_DELTA) .AND. xg .GT. (radius + EPS_DELTA)) THEN

  ! Compute help variables sh = sigma_z**2/h**2 and al = beta:
  sh  = (sigzxg/htot)**2
  al  = 8./PI*sh/((1.+SQRT(1.+8/PI*sh))**2)

  ! If NOT (stable meteo class and stack emitting above mixing layer), compute cq2 (else cq2 = 1):
  IF (flag .NE. 1) THEN 
    cq2 = EXP( -(2.*al/qbstf*1.e-6*vg0tra/12*2*PI*(xx + virty)* cxx*ueff/ugem/onder*(xx - radius)*(1.-cgt)))
  ENDIF
ENDIF

!-----------------------------------------------------------------------------------------------------------------
! Compute cq1 = source depletion ratio for dry deposition for phase 1, area source (cq1 = 1 for a point source). 
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
! Note: vdoppb = vd/sigma_z 
!
! source code:
!                vdoppb*dxeff           vd/sigma_z (S/4) exp(-a/18)           vd (S/4) exp(-((vchem+vnatpri+vd/sigma_z) S)/(18 uxr ))   
!   cq1 = EXP( - ------------) = EXP( - ---------------------------) = EXP( - --------------------------------------------------------) 
!                     uxr                      uxr                                      uxr sigma_z                                     
!
! factor 3.6e5: conversion from percentage per hour -> fraction per second


!
! k_drydep = conversion rate for dry deposition      = vd/sigma_z = vdoppb  [1/s]
! k_wetdep = conversion rate for wet deposition      = vnatpri/(3600*100)   [1/s]
! k_chem   = conversion rate for chemical conversion = vchem/(3600*100)     [1/s]
!
! dxeff = distance over which deposition takes place = (S/4)*exp[-k*t], 
!         with t = travel time from centre of the area source to the edge = (radius/uxr) = diameter/(2*uxr)
!         Parameterisation based on comparison with surface depletion model.
!
! help variable a = k*t = (k_chem + k_wetdep + k_drydep)*(diameter/(2*uxr)) =
!                 = ((vchem + vnatpri)/3.6e5 + vdoppb)*(diameter/(2*uxr)) 
!
!                       .49
! a > 15 -> a = 15 (a/15)  
!
! See also ops_seccmp for dxeff = effective distance over which deposition takes place
!

! for the area sources that are nowadays (2011) common in OPS, this correction is probably not important
IF (ABS(radius) .GT. EPS_DELTA) THEN
  a = ((vchem + vnatpri)/3.6e5 + vdoppb)*diameter/uxr
  IF (a .GT. (15. + EPS_DELTA)) THEN 
    a = (a/15.)**.49*15.
  ENDIF
  dxeff = diameter/4.*EXP( -a/18.) ! factor 18 (or 9) is calibration factor from multipointsource test
  cq1   = EXP( -(vdoppb*dxeff/uxr))
ELSE
  dxeff = 0.
  cq1   = 1.
ENDIF

RETURN
END SUBROUTINE ops_brondepl
