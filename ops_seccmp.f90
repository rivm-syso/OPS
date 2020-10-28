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
! FIRM/INSTITUTE     : RIVM LLO
! LANGUAGE           : FORTRAN-77/90
! DESCRIPTION        : Compute concentration of secondary component (SO4,NO3,NH4) 
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP Fortran
! CALLED FUNCTIONS   : seccd
! UPDATE HISTORY :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_seccmp(qbpri, ueff, rcsec, routsec, ccc, vv, amol1, amol2, xvg, sigz, grad, utr, radius, disx, xl, xloc, vw10,   &
                   &  pcoef, virty, regenk, htot, onder, twt, ri, rb, ra50, cgt, xvghbr, xvglbr, vnatpri, vchem, ra4_rcp,       &
                   &  ra50_rcp, rb_rcp, rc_sec_rcp, pr, vnatsec, cgtsec, vgsec, qsec, consec, vg50trans, ra50tra, rb_tra, xg)

USE m_commonconst
USE m_ops_vchem

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_seccmp')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: qbpri                      ! cross-wind integrated mass flux [g/s] of primary species emitted from source
REAL*4,    INTENT(IN)                            :: ueff                       ! effective transport velocity of plume [m/s]
REAL*4,    INTENT(IN)                            :: rcsec                      ! opp. weerstand sec. component
REAL*4,    INTENT(IN)                            :: routsec                    ! in-cloud scavenging ratio for secondary component
                                                                               ! (rout << rain-out = in-cloud) [-] 
REAL*4,    INTENT(IN)                            :: ccc                        ! undepleted concentration including part above mixing layer; 
                                                                               ! is needed for e.g. wet deposition.
REAL*4,    INTENT(IN)                            :: vv                         ! total source depletion factor for primary component
REAL*4,    INTENT(IN)                            :: amol1                      ! molgewicht primaire component
REAL*4,    INTENT(IN)                            :: amol2                      ! molgewicht secundaire component
REAL*4,    INTENT(IN)                            :: xvg                        ! factor not used; xvg = 1
REAL*4,    INTENT(IN)                            :: sigz                       ! 
REAL*4,    INTENT(IN)                            :: grad                       ! 
REAL*4,    INTENT(IN)                            :: utr                        ! average wind speed over the trajectory (m/s)
REAL*4,    INTENT(IN)                            :: radius                     ! 
REAL*4,    INTENT(IN)                            :: disx                       ! 
REAL*4,    INTENT(IN)                            :: xl                         ! 
REAL*4,    INTENT(IN)                            :: xloc                       ! 
REAL*4,    INTENT(IN)                            :: vw10                       ! 
REAL*4,    INTENT(IN)                            :: pcoef                      ! 
REAL*4,    INTENT(IN)                            :: virty                      ! 
REAL*4,    INTENT(IN)                            :: regenk                     ! 
REAL*4,    INTENT(IN)                            :: htot                       ! 
REAL*4,    INTENT(IN)                            :: onder                      ! 
REAL*4,    INTENT(IN)                            :: twt                        ! 
REAL*4,    INTENT(IN)                            :: ri                         ! 
REAL*4,    INTENT(IN)                            :: rb                         ! 
REAL*4,    INTENT(IN)                            :: ra50                       ! 
REAL*4,    INTENT(IN)                            :: cgt                        ! 
REAL*4,    INTENT(IN)                            :: xvghbr                     ! 
REAL*4,    INTENT(IN)                            :: xvglbr                     ! 
REAL*4,    INTENT(IN)                            :: vnatpri                    ! 
REAL*4,    INTENT(IN)                            :: vchem                      ! chemical conversion rate [%/h]
REAL*4,    INTENT(IN)                            :: ra4_rcp                    ! 
REAL*4,    INTENT(IN)                            :: ra50_rcp                   ! 
REAL*4,    INTENT(IN)                            :: rb_rcp                     ! 
REAL*4,    INTENT(IN)                            :: rc_sec_rcp                 ! 
REAL*4,    INTENT(IN)                            :: ra50tra                    ! 
REAL*4,    INTENT(IN)                            :: rb_tra                     ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: pr                         ! 
REAL*4,    INTENT(OUT)                           :: vnatsec                    ! 
REAL*4,    INTENT(OUT)                           :: cgtsec                     ! 
REAL*4,    INTENT(OUT)                           :: vgsec                      ! deposition velocity secondary component [m/s[
REAL*4,    INTENT(OUT)                           :: qsec                       ! cross-wind integrated mass flux of secondary species [g/s]
REAL*4,    INTENT(OUT)                           :: consec                     ! concentration secondary component [ug/m3]
REAL*4,    INTENT(OUT)                           :: vg50trans                  ! 

! LOCAL VARIABLES
REAL*4                                           :: a                          ! 
REAL*4                                           :: diameter                   ! 
REAL*4                                           :: h                          ! 
REAL*4                                           :: hl                         ! 
REAL*4                                           :: gradsec                    ! 
REAL*4                                           :: qpri                       ! cross-wind integrated mass flux [g/s] of primary species of depleted source

REAL*4                                           :: rcrs                       ! 
REAL*4                                           :: s                          ! 
REAL*4                                           :: sigzsec                    ! 
REAL*4                                           :: vgsect                     ! 
REAL*4                                           :: vnatrainv                  ! uitregensnelheid
REAL*4                                           :: vnatwashv                  ! uitwassnelheid
REAL*4                                           :: vw                         ! 
REAL*4                                           :: qsec_uncorr                ! uncorrected qsec (from seccd)
REAL*4                                           :: xg

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Verical dispersion of secondary component
!
sigzsec = sigz
!
! Area source
!
IF (radius .GT. (0. + EPS_DELTA)) THEN
!
!  disx < radius: receptor inside area source 
!
   IF (disx .LT. (radius - EPS_DELTA)) THEN
      sigzsec = 1.5*sigz 
   ENDIF
!
!  Compute vw = wind speed at transport height (either xl/2 or sigma_z) 

!  For a secondary species, and short distance (inside area source), the concentrations is still low, so
!  there is relatively more mass in the higher part of the plume (further away), than in the lower part;
!  this means that the transport speed for a secondary species (vw) is higher than the transport speed of primary species (ueff).
!  For larger distances, vw = ueff.
!
   IF (sigzsec .GT. (xl/2. + EPS_DELTA)) THEN
      a = xl/2.
   ELSE
      a = sigzsec 
   ENDIF
   vw = vw10*(a/10)**pcoef
   IF (vw .LT. (ueff - EPS_DELTA) .OR. disx .GT. (1.01*radius + EPS_DELTA)) THEN
      vw = ueff
   ENDIF
!
! Compute diameter of area source from given virty.
! Square area source is represented by a circular area source with the same area;
! (area circle with radius r) = (area square with 1/2 side = radius) <=> pi*r**2 = (2*radius)**2 <=> 
! <=> r = 2/sqrt(pi)*radius <=> r = 1.128*radius.

! See ops_virtdist: virty = (radius*12.)/PI*1.128 
!
   diameter = virty*PI/(6.*1.128)
ELSE
!
!  Point source
!
   diameter = 0.
   vw = ueff
ENDIF

!---------------------------------------------------------------
! Wet deposition parameters
!---------------------------------------------------------------

IF (regenk .GT. (0. + EPS_DELTA)) THEN
!
!  Distribution of washout (below cloud) and rainout (in cloud) depending of plume height and mixing height;
!  pr = distribution factor, 0 <= pr <= 1; pr = 0 -> washout, pr = 1 -> rainout, (4.19), (4.20) OPS report
!  a = factor cw in OPS report 4.19
!  See also ops_depoparexp/par_nat
!
   hl = xl - htot
   a  = 1.
   IF (radius .GT. (0. + EPS_DELTA)) THEN
      IF (disx .LT. (radius - EPS_DELTA)) THEN
         hl = xl - htot + sigzsec*(radius - disx)/radius
         a  = 3.
      ELSE
         hl = xl - htot - (radius**3)/(200*disx**2)
         a  = 1.
      ENDIF
   ENDIF
   IF ((ABS(onder) .LE. ( 0 + EPS_DELTA )) .OR. (hl .LT. (0. - EPS_DELTA))) THEN
      hl = 0.
   ENDIF
   pr = EXP(-(hl + 5)**2/(2*sigzsec*sigzsec*a)) 
 
!  Note: in ops_depoparexp/par_nat pr = pr*AMIN1(1., disx/(ueff*3600.)) (correction near source)
!        but for secondary components, concentrations near source are relatively low

!  Wash out (below cloud) coefficient:

!  epsilon = particle - droplet collision efficiency; 
!            for secondary particles, a collision efficiency = 0.31 has been taken (= EPSILON(class 4), see ops_depoparexp)
!
   vnatwashv = regenk*100./twt*(1. - EXP( -twt*0.31*1.326*ri**.816))
!
!  Rain out (in-cloud) coefficient:

! 
   vnatrainv = regenk*100./twt*(1. - EXP( -routsec*twt*ri/1000./xl))
 
!  Wash out (below-cloud), rain out (in-cloud); in-cloud scavenging is more efficient than below-cloud;
!  therefore vnatwash must be smaller than vnatrain.
!
   IF (vnatwashv .GT. (vnatrainv + EPS_DELTA)) THEN
      vnatwashv = vnatrainv
   ENDIF
!
!  Interpolate between wash out and rain out:
! 
   vnatsec = vnatwashv*(1. - pr) + vnatrainv*pr
ELSE
   ! rain probability = 0 for this meteo class:
   vnatsec = 0.
ENDIF

!---------------------------------------------------------------
! Dry deposition parameters
!---------------------------------------------------------------
!                                                            vg_sec(50)
! Compute gradsec = gradient in vg of secondary component = ------------
!                                                            vg_sec(4)
!
! cgtsec = (1 - gradsec) (1 - exp[-t/tau]), with t   = travel time (see ops_brondepl for more documentation).
!                                                tau = z1/vd(z1), 4.10 OPS report
!                                                               cgt
! cgt = (1 - grad) (1 - exp[-t/tau]) <=>  (1 - exp[-t/tau]) = ----------
!                                                             (1 - grad)
!                                          cgt
! and it follows cgtsec = (1.-gradsec) ----------- 
!                                       (1.-grad)
!

rcrs=rc_sec_rcp+rb_rcp
gradsec=(ra4_rcp+rcrs)/(ra50_rcp+rcrs)
IF (grad .NE. 1) THEN
   cgtsec=(1.-gradsec)*cgt/(1.-grad)
ELSE
   cgtsec=(1.-gradsec)
ENDIF
!
! Dry deposition velocity of secondary component at z = 50 m, average over trajectory
!
vgsec=1/(ra50tra+rcsec+rb_tra)
!
! vgsect = dry deposition velocity averaged over transport distance
! xvglbr = ratio effective dry deposition velocity over transport distance and 
!          average dry deposition velocity over transport distance for low sources [-]
!          See OPS-doc/meteo, bookmark correction_factor_deposition_velocity
! xvghbr = ratio effective dry deposition velocity over transport distance and 
!          average dry deposition velocity over transport distance for high sources [-]
!          See OPS-doc/meteo, bookmark correction_factor_deposition_velocity
!
! Note that xvg = 1
!
IF (htot .GT. xloc) THEN
   vgsect = (vgsec*xvghbr)/xvg
ELSE
   vgsect = (vgsec*xvglbr)/xvg
ENDIF

IF (ABS(onder) .LE. EPS_DELTA) THEN
   vg50trans = 0.
   vgsec     = 0.
   vgsect    = 0.
ENDIF

!---------------------------------------------------------------
! Compute concentration of secondary component
!---------------------------------------------------------------

! First compute qpri and qsec, cross-wind integrated mass fluxes of primary and secondary substances;
! seccd uses a numerical procedure, assuming constant parameters such as mixing height and transport speed:
CALL seccd(qbpri, disx, radius, utr, xl, vg50trans, vnatpri, vchem, vgsect, vnatsec, amol1, amol2, diameter, sigz, qpri,       &
        &  qsec)
!
! In reality, we have to deal with variable mixing heigth and a transport speed that depends on emission height ->
!  a correction is needed, using the 'exact' depletion factor for primary species vv: 
!
! vv      : total source depletion factor for primary species
! qbpri   : cross-wind integrated mass flux [g/s] of primary species emitted from source
! qbpri*vv: cross-wind integrated mass flux [g/s] of primary species of depleted source, using 'exact' depletion factor vv
! qpri    : cross-wind integrated mass flux [g/s] of primary species of depleted source (numerical approximation from subroutine seccd)
! qsec    : cross-wind integrated mass flux [g/s] of secondary species (numerical approximation from subroutine seccd)
! 
! Correct qsec:
!                qbpri*vv
!   qsec = qsec ----------
!                  qpri
!
!   but, qsec can never be greater than qbpri
!
IF (qpri .GT. (0. + EPS_DELTA)) qsec = min(qbpri,(qsec*qbpri*vv)/qpri)
!
! Compute concentration of secondary species 
!
! 1. sigma_z < 1.6*xl -> in Gaussian plume OPS report 3.7, 3.15 FS
!   
!         q           q   NSEK            2                      -h^2            -(2z - h)^2          -(2z + h)^2
! csec = --- Dy Dz = --- -------- -------------------- [ exp(------------) + exp(------------) + exp(-------------) ]
!         u           u   2 pi x   sqrt(2 pi) sigma_z         2 sigmaz^2          2 sigmaz^2          2 sigmaz^2
!
!         NSEK      2              12              
! factor ------ ---------- = --------------- = 1.5238
!         2 pi  sqrt(2 pi)    pi sqrt(2 pi)        
!
! factor 1e6 for conversion g -> ug
!
! 2. sigma_z > 1.6*xl (well mixed plume) AND depleted source strength > 1e-4*undepleted source strength
!    assume that ratio of concentration and cross-wind integrated mass flux at the receptor is the same for primary and secondary species:
!
!      csec   cpri            qsec        qsec   qbpri        qsec
!      ---- = ----  -> csec = ---- cpri = -----  ----- cpri = ----- ccc,
!      qsec   qpri            qpri        qbpri  qpri         qbpri 
!
!                                                              qbpri
!      with ccc = undepleted concentration primary species = ----- cpri
!                                                              qpri
!
! 3. sigma_z > 1.6*xl (well mixed plume) AND depleted source strength <= 1e-4*undepleted source strength -> (3.7, 3.9 OPS report)
!   
!         q           q    NSEK    1  
! csec = --- Dy Dz = --- -------- --- ; 2 pi = 6.2832
!         u           u   2 pi x   xl
!
IF (sigzsec .LT. (1.6*xl - EPS_DELTA)) THEN
   s = 2.*sigzsec*sigzsec
   h = htot
   consec = qsec*1.e6*1.5238/(vw*sigzsec*(disx + virty))* (EXP( -h*h/s) + EXP( -(2.*xl - h)**2/s) + EXP(-(2*xl + h)**2/s))
ELSE IF (qpri .GT. (qbpri*0.0001 + EPS_DELTA)) THEN 
   ! loss due to deposition/chemical conversion not so large 
   consec = (qsec/qbpri)*ccc 
ELSE
   ! loss due to deposition/chemical conversion large -> further away from source -> fully mixed
   consec = qsec*1.e6*12/(xl*6.2832*(disx + virty)*ueff)
ENDIF

RETURN

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : seccd
! DESCRIPTION        : Compute cross-wind integrated mass fluxes Q for primary and secondary substances.
!                      A numerical time stepping scheme is used here.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE seccd(qbpri, disx, radius, vw, xl, vgpri, vnatpri, vchem, vgsec, vnatsec, amol1, amol2, diameter, sigz, qpri,       &
              &  qsec)

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'seccd')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: qbpri                      ! cross-wind integrated mass flux [g/s] of primary species emitted from source
REAL*4,    INTENT(IN)                            :: disx                       ! 
REAL*4,    INTENT(IN)                            :: radius                     ! 
REAL*4,    INTENT(IN)                            :: vw                         ! average wind speed over trajectory [m/s]
REAL*4,    INTENT(IN)                            :: xl                         ! 
REAL*4,    INTENT(IN)                            :: vgpri                      ! 
REAL*4,    INTENT(IN)                            :: vnatpri                    ! loss rate due to wet deposition of primary component [%/h]
REAL*4,    INTENT(IN)                            :: vchem                      ! chemical conversion rate [%/h]
REAL*4,    INTENT(IN)                            :: vgsec                      ! 
REAL*4,    INTENT(IN)                            :: vnatsec                    ! loss rate due to wet deposition of secondary component [%/h]
REAL*4,    INTENT(IN)                            :: amol1                      ! molecular weight primary component
REAL*4,    INTENT(IN)                            :: amol2                      ! molecular weight secondary component
REAL*4,    INTENT(IN)                            :: diameter                   ! 
REAL*4,    INTENT(IN)                            :: sigz                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: qpri                       ! cross-wind integrated mass flux of primary species at receptor [g/s]
REAL*4,    INTENT(OUT)                           :: qsec                       ! cross-wind integrated mass flux of secondary species at receptor [g/s]

! LOCAL VARIABLES
INTEGER*4                                        :: itim                       ! time step index
INTEGER*4                                        :: ntim                       ! number of time steps
REAL*4                                           :: a                          ! effective transport distance over which conversion takes place
REAL*4                                           :: a1                         ! 
REAL*4                                           :: b                          ! 
REAL*4                                           :: dt                         ! length of time step [s]
integer                                          :: it                         ! iteration count
integer, parameter                               :: nit = 10                   ! maximal number of iterations
logical                                          :: converged                  ! iteration procedure for Q(it) has converged : abs(Q(it-1) = Q(it)) < epsa + epsr * Q(it)
real, parameter                                  :: epsa = 0.001               ! absolute tolerance for iterative procedure (g/s)
real                                             :: epsr = 0.01                ! relative tolerance for iterative procedure (-)
real                                             :: qpri_prev_tim              ! cross-wind integrated mass flux of primary species at end of previous time step (g/s)
real                                             :: qsec_prev_tim              ! cross-wind integrated mass flux of secondary species at end of previous time step (g/s)
real                                             :: qpri_prev_it               ! cross-wind integrated mass flux of primary species at current time step, previous iteration (g/s)
real                                             :: qsec_prev_it               ! cross-wind integrated mass flux of secondary species at current time step, previous iteration (g/s)
real                                             :: loss_pri                   ! loss term of primary species (g/s)
real                                             :: prod_sec                   ! production term of secondary species (g/s)
real                                             :: loss_sec                   ! loss term of secondary species (g/s)
real                                             :: e3_pri_sec                 ! factor in production term of secondary species = (Msec/Mpri) * delta_t * k_chem
real                                             :: e1_pri                     ! source depletion factor for primary species, due to dry deposition, wet deposition and chemical conversion
real                                             :: e1_sec                     ! source depletion factor for secondary species, due to dry deposition, wet deposition and chemical conversion
REAL*4                                           :: xseg                       ! end point of plume segment [m]
REAL*4                                           :: dx                         ! travelled distance during one time step = length of plume segment [m]
logical                                          :: lfound_seg_depos           ! plume segment where deposition starts has been found
                                                                             
! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------

! Parameterisation of a = distance over which production of secondary species takes place; 
! a = x, point source; a = R*exp(-kt), inside area source; a = x - R*(1-exp(-kt)), outside area source.
! Production takes place, where the concentration of the primary species is > 0, hence
! the loss term b = exp(-k*t), with k = loss rate primary species (due to dry and wet deposition 
! and chemical conversion), t = travel time = radius/u = diameter/(2*u), u wind speed.
! The loss rate for dry deposition is k_dry_depos = vgpri/a1, a1 = effective plume thickness.


IF (radius .GT. (0. + EPS_DELTA)) THEN
   IF (1.5*sigz .GT. (xl + EPS_DELTA)) THEN
      a1 = xl
   ELSE
      a1 = 1.5*sigz
   ENDIF
   b = EXP( - (diameter/(vw*3.)*(vgpri/a1 + (vchem + vnatpri)/360000.)))
   IF (disx .LE. (radius + EPS_DELTA)) THEN
      a = diameter/2.*b
   ELSE
      a = disx - (diameter/2.)*(1. - b)
   ENDIF
ELSE
   a = disx
ENDIF

! Set ntim = number of time steps; start with 6 time steps for each travel distance < 50 km
! and add 1 time step for each further 50 km: 
ntim = NINT(a)/50000 + 6 

! Set dt = length of time step [s]; end time = ntim*dt = a/wind_velocity
! and dx = distance travelled in one time step [m]
!
! Table: time step dt [s] as function of distance a [m] and wind velocity vw [m/s].
! ------------------------------------------------------------------------
!  vw (m/s) |a (m) 100   1000   10,000  100,000  1000,000
! ----------|-------------------------------------------------------------
!    1.0    |      17     167    1667    12500     38462 
!    5.0    |       3      33     333     2500      7692 
!   10.0    |       2      17     167     1250      3846 
!
dt = a/vw/ntim
dx = dt*vw

! Initialise 
!            qpri      = cross-wind integrated mass flux of primary species [g/s]
!            qsec      = cross-wind integrated mass flux of secondary species [g/s]
!            xseg      = end point of plume segment after each time step [m]
!     lfound_seg_depos = segment where deposition starts has been found
qpri = qbpri
qsec = 0.0
xseg = 0.0 
lfound_seg_depos = .false. 

!
! factor 3.6e5 = 3600*100 conversion from %/h to 1/s
!
!    dC
!   ---- = -k C --> C(t) = C(0) exp(-k t); k = k_drydep + k_chem + k_wetdep
!    dt
! Source depletion -> effect on C is translated into depleted source strength: Q(t) = Q(0) exp(-k t).
!
! k_drydep = conversion rate for dry deposition      = vgpri/xl           [1/s]
! k_wetdep = conversion rate for wet deposition      = vnatpri/(3600*100) [1/s]
! k_chem   = conversion rate for chemical conversion = vchem/(3600*100)   [1/s]
! delta_t  = time step                               = dt                 [s]
! 
! In order to resolve the interdependency between the primary and secondary species, we use an extra iteration within
! each time step. In tests, this iteration only needs 2-3 iterations to converge. 
!
! qpri          = cross-wind integrated mass flux of primary species at current time step, current iteration (g/s)
! qpri_prev_tim = cross-wind integrated mass flux of primary species at end of previous time step (g/s)
! qpri_prev_it  = cross-wind integrated mass flux of primary species at current time step, previous iteration (g/s)
! qsec, qsec_prev_tim, qsec_prev_it: the same for secondary species
!
! prod_sec   = production term of secondary species (g/s) = (Msec/Mpri) * (average mass primary) * k_chem = 
!                                                         = (Msec/Mpri) * delta_t*(qpri_prev_tim + qpri)/2 * k_chem
! e3_pri_sec = factor in production term of secondary species = (Msec/Mpri) * delta_t * k_chem
!
! mass flux at start of time interval                                      : Q(t)
! mass flux at end of time interval, after deposition, chemical conversion : Q(t+dt) = Q(t) exp(-k dt)
! 
! loss_pri = loss term of primary species (g/s) = Q(t) - Q(t+dt) = Q(t) [1 - exp(-k dt)], k = k_drydep + k_wetdep + k_chem;
!            Q = qpri_prev_tim.
! loss_sec = loss term of secondary species (g/s) = Q(t) - Q(t+dt) = Q(t) [1 - exp(-k dt)], k = k_drydep + k_wetdep + k_chem;
!            Q is evaluated by means of a 'midpoint' apprimation: Q = (qsec_prev_tim + 0.5*prod_sec); this makes
!            larger time steps possible.
!
! e1_pri = source depletion factor for primary species, due to dry deposition, wet deposition and chemical conversion
!        = 1 - EXP( -delta_t*(k_drydep + k_wetdep + k_chem)) = 1 - EXP( -dt*(vgpri/xl + (vnatpri + vchem)/3.6e5))
! e1_sec = source depletion factor for secondary species, due to dry deposition and wet deposition
!        = 1 - EXP( -delta_t*(k_drydep + k_wetdep)) = 1 - EXP( -dt*(vgsec/xl + vnatsec/3.6e5))

e1_pri = 1. - exp( - dt*(vgpri/xl + (vnatpri + vchem)/3.6e5));
e1_sec = 1. - exp( - dt*(vgsec/xl +  vnatsec/3.6e5));
e3_pri_sec = (amol2/amol1)*dt*vchem/3.6e+05;

! Loop over time steps:
DO itim = 1, ntim

    ! Store mass fluxes of previous time step:
    qpri_prev_tim = qpri
    qsec_prev_tim = qsec
  
    ! Loop over iterations:
    ! NOTE; iteration is only needed if we include both reactions NH3 -> NH4 and
    ! NH4 -> NH3; if we use the net reaction NH3 -> NH4 only, we don't need an iteration.
    ! 
    !it = 0
    !converged = .false.
    !do while (it .lt. nit .and. .not. converged)
    !    it = it + 1
  
        ! Store mass fluxes of previous iteration:
        qpri_prev_it = qpri
        qsec_prev_it = qsec
      
        ! Primary species:
        loss_pri  = qpri_prev_tim*e1_pri
        qpri      = qpri_prev_tim - loss_pri

        ! Secondary species:
        prod_sec = 0.5*(qpri_prev_tim + qpri)*e3_pri_sec
        loss_sec = (qsec_prev_tim + 0.5*prod_sec)*e1_sec 
        !loss_sec = (qsec_prev_tim - 0.5*prod_sec)*e1_sec 
        qsec     = qsec_prev_tim + prod_sec - loss_sec

        !! Check for convergence:
        !converged = (abs(qpri - qpri_prev_it) .lt. epsa + epsr*qpri .and. abs(qsec - qsec_prev_it) .lt. epsa + epsr*qsec)
	!! write(*,*) 'seccd: ',it,qpri,abs(qpri-qpri_prev_it),qsec,abs(qsec-qsec_prev_it)

    !enddo ! loop over iterations
    
ENDDO ! end loop over time steps

RETURN
END SUBROUTINE seccd

END SUBROUTINE ops_seccmp
