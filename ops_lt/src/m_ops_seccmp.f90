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
! DESCRIPTION        : Compute concentration of secondary component (SO4,NO3,NH4) 
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_seccmp

implicit none

contains

SUBROUTINE ops_seccmp(varin_unc,do_proc, qbpri, ueff, rc_sec_trj, routsec, c0_undepl_total, vv, amol1, amol2, xvg, sigz, grad, utr, radius, disxx, xl, xloc, vw10,  &
                   &  pcoef, virty, regenk, htot, onder, twt, ri, cgt, xvghbr, xvglbr, vnatpri, vchem, ra_rcp_4,  &
                   &  ra_rcp_zra, rb_rcp, rc_sec_rcp, pr, vnatsec, cgtsec, qsec, consec, vd_eff_trj_zra, ra_trj_zra, rb_trj)

use m_commonconst_lt
use m_ops_vchem
use m_ops_varin, only: Tvarin_unc
use m_ops_tdo_Proc, only: Tdo_proc

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_seccmp')

! SUBROUTINE ARGUMENTS - INPUT
TYPE(Tvarin_unc), INTENT(IN)                     :: varin_unc
TYPE(Tdo_proc), INTENT(IN)                       :: do_proc                    ! options to switch on/off specific processes
REAL,      INTENT(IN)                            :: qbpri                      ! cross-wind integrated mass flux [g/s] of primary species emitted from source
REAL,      INTENT(IN)                            :: ueff                       ! effective transport velocity of plume [m/s]
REAL,      INTENT(IN)                            :: rc_sec_trj                 ! canopy resistance secondary aerosol (SO4, NO3, NH4) for trajectory [s/m]
REAL,      INTENT(IN)                            :: routsec                    ! in-cloud scavenging ratio for secondary component
                                                                               ! (rout << rain-out = in-cloud) [-] 
REAL,      INTENT(IN)                            :: c0_undepl_total            ! undepleted concentration at z = 0 (including part of plume above mixing layer); 
                                                                               ! is needed for secondary species (note that this is only used for 'well-mixed', where c(0) = c(zrcp))
REAL,      INTENT(IN)                            :: vv                         ! total source depletion factor for primary component
REAL,      INTENT(IN)                            :: amol1                      ! molgewicht primaire component
REAL,      INTENT(IN)                            :: amol2                      ! molgewicht secundaire component
REAL,      INTENT(IN)                            :: xvg                        ! factor not used; xvg = 1
REAL,      INTENT(IN)                            :: sigz                       ! vertical dispersion length [m]
REAL,      INTENT(IN)                            :: grad                       ! depositon velocity gradient over height = vd(zra)/vd(4)
REAL,      INTENT(IN)                            :: utr                        ! average wind speed over the trajectory (m/s)
REAL,      INTENT(IN)                            :: radius                     ! 
REAL,      INTENT(IN)                            :: disxx                      ! effective travel distance between source and receptor [m] 
REAL,      INTENT(IN)                            :: xl                         ! maximal mixing height over transport distance [m] (extrapolated when x > 1000km, largest distance category in meteo statistics; xl = 2.*htt when xl < htt)
REAL,      INTENT(IN)                            :: xloc                       ! local mixing height (near source) [m]
REAL,      INTENT(IN)                            :: vw10                       ! wind speed at 10 m height [m/s]
REAL,      INTENT(IN)                            :: pcoef                      ! coefficient in wind speed power law
REAL,      INTENT(IN)                            :: virty                      ! distance virtual point source - centre area source [m]
REAL,      INTENT(IN)                            :: regenk                     ! rain probability [-] 
REAL,      INTENT(IN)                            :: htot                       ! plume height at receptor, including plume descent due to heavy particles [m]
REAL,      INTENT(IN)                            :: onder                      ! fraction of emission below mixing height [-]
REAL,      INTENT(IN)                            :: twt                        ! average duration of a rainfall period, dependent on source - receptor distance [h] 
REAL,      INTENT(IN)                            :: ri                         ! rain intensity [mm/h]. 
REAL,      INTENT(IN)                            :: cgt                        ! gradient factor at 4 m height [-]
REAL,      INTENT(IN)                            :: xvghbr                     ! ratio effective dry deposition velocity over transport distance and average dry deposition velocity over transport distance for high sources [-]
REAL,      INTENT(IN)                            :: xvglbr                     ! ratio effective dry deposition velocity over transport distance and average dry deposition velocity over transport distance for low sources [-]
REAL,      INTENT(IN)                            :: vnatpri                    ! wet deposition loss rate for primary components [%/h]
REAL,      INTENT(IN)                            :: vchem                      ! chemical conversion rate [%/h]
REAL,      INTENT(IN)                            :: ra_rcp_4                   ! aerodynamic resistance at receptor, 4 m height [s/m]
REAL,      INTENT(IN)                            :: ra_rcp_zra                 ! aerodynamic resistance at receptor, height zra [s/m];
                                                                               ! zra is height where concentration profile is undisturbed by deposition = 50 m 
REAL,      INTENT(IN)                            :: rb_rcp                     ! boundary layer resistance at receptor [s/m] 
REAL,      INTENT(IN)                            :: rc_sec_rcp                 ! canopy resistance secondary aerosol (SO4, NO3, NH4) at the receptor [s/m]
REAL,      INTENT(IN)                            :: ra_trj_zra                 ! aerodynamic resistance for trajectory at height zra [s/m]
REAL,      INTENT(IN)                            :: rb_trj                     ! boundary layer resistance for trajectory [s/m]

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(INOUT)                         :: pr                         ! Distribution factor between washout (below cloud) and rainout (in cloud). pr = 0 -> washout, pr = 1 -> rainout. 
                                                                               ! INOUT, because pr remains unchanged when  regenk <= EPS_DELTA
REAL,      INTENT(OUT)                           :: vnatsec                    ! 
REAL,      INTENT(OUT)                           :: cgtsec                     ! 
REAL,      INTENT(OUT)                           :: qsec                       ! cross-wind integrated mass flux of secondary species [g/s]
REAL,      INTENT(OUT)                           :: consec                     ! concentration secondary component [ug/m3]
REAL,      INTENT(INOUT)                         :: vd_eff_trj_zra             ! effective deposition velocity over trajectory, taking into account amount of time that plume is above mixing
                                                                               ! height and no deposition takes place [m/s] 

! LOCAL VARIABLES
REAL                                             :: a                          ! 
REAL                                             :: diameter                   ! 
REAL                                             :: h                          ! 
REAL                                             :: hl                         ! 
REAL                                             :: gradsec                    ! 
REAL                                             :: lambda_b                   ! Scavenging rate for below-cloud scavenging (1/h)
REAL                                             :: qpri                       ! cross-wind integrated mass flux [g/s] of primary species of depleted source

REAL                                             :: s                          ! 
REAL                                             :: sigzsec                    !
REAL                                             :: vd_sec_uncorr_trj_zra      ! deposition velocity secondary component for trajectory, height zra [m/s]; uncorrected 
                                                                               ! for time that plume is above mixing height
REAL                                             :: vd_sec_eff_trj_zra         ! effective deposition velocity secondary component for trajectory, height zra [m/s]; taking 
                                                                               ! into account the time that plume is above mixing height and there is no deposition
REAL                                             :: vnatrainv                  ! rain out rate
REAL                                             :: vnatwashv                  ! wash out rate
REAL                                             :: vw                         ! 

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
!  disxx < radius: receptor inside area source 
!
   IF (disxx .LT. (radius - EPS_DELTA)) THEN
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
   IF (vw .LT. (ueff - EPS_DELTA) .OR. disxx .GT. (1.01*radius + EPS_DELTA)) THEN
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
      IF (disxx .LT. (radius - EPS_DELTA)) THEN
         hl = xl - htot + sigzsec*(radius - disxx)/radius
         a  = 3.
      ELSE
         hl = xl - htot - (radius**3)/(200*disxx**2)
         a  = 1.
      ENDIF
   ENDIF
   IF ((ABS(onder) .LE. ( 0 + EPS_DELTA )) .OR. (hl .LT. (0. - EPS_DELTA))) THEN
      hl = 0.
   ENDIF
   pr = EXP(-(hl + 5)**2/(2*sigzsec*sigzsec*a)) 
   
!  Note: in ops_depoparexp/par_nat pr = pr*AMIN1(1., disxx/(ueff*3600.)) (correction near source)
!        but for secondary components, concentrations near source are relatively low

!  Wash out (below cloud) coefficient:

!  epsilon = particle - droplet collision efficiency; 
!            for secondary particles, a collision efficiency = 0.31 has been taken (= EPSILON(class 4), see ops_depoparexp)
!
   lambda_b = 1.326*0.31*ri**0.816
   ! Adjustment relevant for sensitivity analyses. Multiplication factor is 1.0 by default.
   lambda_b = varin_unc%unc_sourcedepl%washout_sec_fact * lambda_b
   vnatwashv = regenk*100./twt*(1. - EXP( -lambda_b*twt))
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
!                                                            vd_sec(50)
! Compute gradsec = gradient in vd of secondary component = ------------
!                                                            vd_sec(4)
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

gradsec = (ra_rcp_4 + rb_rcp + rc_sec_rcp)/(ra_rcp_zra + rb_rcp + rc_sec_rcp) 
IF (do_proc%grad_drydep) THEN
   IF (grad .NE. 1) THEN
      cgtsec = (1.-gradsec)*cgt/(1.-grad)
   ELSE
      cgtsec = (1.-gradsec)
   ENDIF
ELSE
  cgtsec = 0.0
ENDIF
!
! vd_sec_uncorr_trj_zra = dry deposition velocity of secondary component at z = zra = 50 m, average over trajectory, uncorrected
!                         for time that plume is above mixing height
vd_sec_uncorr_trj_zra = 1/(ra_trj_zra + rb_trj + rc_sec_trj)  
!
! vd_sec_eff_trj_zra = effective dry deposition velocity averaged over transport distance, height zra, taking into account
!                      the time that plume is above mixing height (when there is no deposition) [m/s]
! xvglbr             = ratio effective dry deposition velocity over transport distance and 
!                      average dry deposition velocity over transport distance for low sources [-]
!                      See OPS-doc/meteo, bookmark correction_factor_deposition_velocity
! xvghbr             = ratio effective dry deposition velocity over transport distance and 
!                      average dry deposition velocity over transport distance for high sources [-]
!                      See OPS-doc/meteo, bookmark correction_factor_deposition_velocity
!
! Note that xvg = 1
!
IF (htot .GT. xloc) THEN
   vd_sec_eff_trj_zra = (vd_sec_uncorr_trj_zra*xvghbr)/xvg 
ELSE
   vd_sec_eff_trj_zra = (vd_sec_uncorr_trj_zra*xvglbr)/xvg
ENDIF

IF (ABS(onder) .LE. EPS_DELTA) THEN
   vd_eff_trj_zra        = 0.  
   vd_sec_uncorr_trj_zra = 0.
   vd_sec_eff_trj_zra    = 0.
ENDIF

! Adjustments relevant for sensitivity analyses. Multiplication factors are 1.0 by default.
vd_sec_eff_trj_zra = varin_unc%unc_sourcedepl%vd_drydep_sec_fact * vd_sec_eff_trj_zra
!End Edit

!---------------------------------------------------------------
! Compute concentration of secondary component
!---------------------------------------------------------------

! First compute qpri and qsec, cross-wind integrated mass fluxes of primary and secondary substances;
! seccd uses a numerical procedure, assuming constant parameters such as mixing height and transport speed:
CALL seccd(do_proc, qbpri, disxx, radius, utr, xl, vd_eff_trj_zra, vnatpri, vchem, vd_sec_eff_trj_zra, vnatsec, amol1, amol2, diameter, sigz, qpri, &
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
!      ---- = ----  -> csec = ---- cpri = -----  ----- cpri = ----- c0_undepl_total,
!      qsec   qpri            qpri        qbpri  qpri         qbpri 
!
!                                                                        qbpri
!      with c0_undepl_total = undepleted concentration primary species = ----- cpri
!                                                                        qpri
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
   consec = qsec*1.e6*1.5238/(vw*sigzsec*(disxx + virty))* (EXP( -h*h/s) + EXP( -(2.*xl - h)**2/s) + EXP(-(2*xl + h)**2/s))
ELSE IF (qpri .GT. (qbpri*0.0001 + EPS_DELTA)) THEN 
   ! loss due to deposition/chemical conversion not so large 
   consec = (qsec/qbpri)*c0_undepl_total 
ELSE
   ! loss due to deposition/chemical conversion large -> further away from source -> fully mixed
   consec = qsec*1.e6*12/(xl*6.2832*(disxx + virty)*ueff)
ENDIF

RETURN

END SUBROUTINE ops_seccmp

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : seccd
! DESCRIPTION        : Compute cross-wind integrated mass fluxes Q for primary and secondary substances.
!                      A numerical time stepping scheme is used here.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE seccd(do_proc, qbpri, disxx, radius, vw, xl, vd_eff_trj_zra, vnatpri, vchem, vd_sec_eff_trj_zra, vnatsec, amol1, amol2, diameter, sigz, qpri, &
              &  qsec)
use m_commonconst_lt

use m_ops_tdo_proc, only: Tdo_proc

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'seccd')

! SUBROUTINE ARGUMENTS - INPUT
TYPE(Tdo_proc), INTENT(IN)                       :: do_proc                    ! options to switch on/off specific processes
REAL,      INTENT(IN)                            :: qbpri                      ! cross-wind integrated mass flux [g/s] of primary species emitted from source
REAL,      INTENT(IN)                            :: disxx                      ! effective travel distance between source and receptor [m] 
REAL,      INTENT(IN)                            :: radius                     ! 
REAL,      INTENT(IN)                            :: vw                         ! average wind speed over trajectory [m/s]
REAL,      INTENT(IN)                            :: xl                         ! maximal mixing height over transport distance [m] (extrapolated when x > 1000km, largest distance category in meteo statistics; xl = 2.*htt when xl < htt)
REAL,      INTENT(IN)                            :: vd_eff_trj_zra             ! effective deposition velocity primary component over trajectory, taking into account amount of time that plume is above mixing
REAL,      INTENT(IN)                            :: vnatpri                    ! loss rate due to wet deposition of primary component [%/h]
REAL,      INTENT(IN)                            :: vchem                      ! chemical conversion rate [%/h]
REAL,      INTENT(IN)                            :: vd_sec_eff_trj_zra         ! effective deposition velocity secondary component for trajectory, height zra [m/s]; taking 
                                                                               ! into account the time that plume is above mixing height and there is no deposition
REAL,      INTENT(IN)                            :: vnatsec                    ! loss rate due to wet deposition of secondary component [%/h]
REAL,      INTENT(IN)                            :: amol1                      ! molecular weight primary component
REAL,      INTENT(IN)                            :: amol2                      ! molecular weight secondary component
REAL,      INTENT(IN)                            :: diameter                   ! 
REAL,      INTENT(IN)                            :: sigz                       ! vertical dispersion length [m]

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: qpri                       ! cross-wind integrated mass flux of primary species at receptor [g/s]
REAL,      INTENT(OUT)                           :: qsec                       ! cross-wind integrated mass flux of secondary species at receptor [g/s]

! LOCAL VARIABLES
INTEGER                                          :: itim                       ! time step index
INTEGER                                          :: ntim                       ! number of time steps
REAL                                             :: a                          ! effective transport distance over which conversion takes place
REAL                                             :: a1                         ! 
REAL                                             :: b                          ! 
REAL                                             :: dt                         ! length of time step [s]
real                                             :: loss_pri                   ! loss term of primary species (g/s)
real                                             :: prod_sec                   ! production term of secondary species (g/s)
real                                             :: loss_sec                   ! loss term of secondary species (g/s)
real                                             :: e3_pri_sec                 ! factor in production term of secondary species = (Msec/Mpri) * delta_t * k_chem
real                                             :: e1_pri                     ! source depletion factor for primary species, due to dry deposition, wet deposition and chemical conversion
real                                             :: e1_sec                     ! source depletion factor for secondary species, due to dry deposition, wet deposition and chemical conversion
REAL                                             :: xseg                       ! end point of plume segment [m]
REAL                                             :: dx                         ! travelled distance during one time step = length of plume segment [m]
logical                                          :: lfound_seg_depos           ! plume segment where deposition starts has been found

! Variables for iteration (not used now)
! integer                                          :: it                         ! iteration count (not used now)
! integer, parameter                               :: nit = 10                   ! maximal number of iterations
! logical                                          :: converged                  ! iteration procedure for Q(it) has converged : abs(Q(it-1) = Q(it)) < epsa + epsr * Q(it)
! real, parameter                                  :: epsa = 0.001               ! absolute tolerance for iterative procedure (g/s)
! real                                             :: epsr = 0.01                ! relative tolerance for iterative procedure (-)
                                                                               
!-------------------------------------------------------------------------------------------------------------------------------

! Parameterisation of a = distance over which production of secondary species takes place; 
! a = x, point source; a = R*exp(-kt), inside area source; a = x - R*(1-exp(-kt)), outside area source.
! Production takes place, where the concentration of the primary species is > 0, hence
! the loss term b = exp(-k*t), with k = loss rate primary species (due to dry and wet deposition 
! and chemical conversion), t = travel time = radius/u = diameter/(2*u), u wind speed.
! The loss rate for dry deposition is k_dry_depos = vd_eff_trj_zra/a1, a1 = effective plume thickness.


IF (radius .GT. (0. + EPS_DELTA)) THEN
   IF (1.5*sigz .GT. (xl + EPS_DELTA)) THEN
      a1 = xl
   ELSE
      a1 = 1.5*sigz
   ENDIF
   
   ! b = EXP( - (diameter/(vw*3.)*(vd_eff_trj_zra/a1 + (vchem + vnatpri)/360000.)))
   b = 1.0
   if (do_proc%chem)   b = b*EXP( - (diameter/(vw*3.)*vchem/360000.))  
   if (do_proc%depl_drydep) b = b*EXP( - (diameter/(vw*3.)*vd_eff_trj_zra/a1))  
   if (do_proc%depl_wetdep) b = b*EXP( - (diameter/(vw*3.)*vnatpri/360000.))  
   IF (disxx .LE. (radius + EPS_DELTA)) THEN
      a = diameter/2.*b
   ELSE
      a = disxx - (diameter/2.)*(1. - b)
   ENDIF
ELSE
   a = disxx
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
! k_drydep = conversion rate for dry deposition      = vd_eff_trj_zra/xl           [1/s]
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
!        = 1 - EXP( -delta_t*(k_drydep + k_wetdep + k_chem)) = 1 - EXP( -dt*(vd_eff_trj_zra/xl + (vnatpri + vchem)/3.6e5))
! e1_sec = source depletion factor for secondary species, due to dry deposition and wet deposition
!        = 1 - EXP( -delta_t*(k_drydep + k_wetdep)) = 1 - EXP( -dt*(vd_sec_eff_trj_zra/xl + vnatsec/3.6e5))

! e1_pri = 1. - exp( - dt*(vd_eff_trj_zra/xl + (vnatpri + vchem)/3.6e5))
e1_pri = 1.0
if (do_proc%chem)   e1_pri = e1_pri*exp(-dt*(vchem/3.6e5))
if (do_proc%depl_drydep) e1_pri = e1_pri*exp(-dt*vd_eff_trj_zra/xl)
if (do_proc%depl_wetdep) e1_pri = e1_pri*exp(-dt*(vnatpri/3.6e5))
e1_pri = 1.0 - e1_pri

! e1_sec = 1. - exp( - dt*(vd_sec_eff_trj_zra/xl +  vnatsec/3.6e5));
e1_sec = 1.0
if (do_proc%depl_drydep) e1_sec = e1_sec*exp( - dt*vd_sec_eff_trj_zra/xl)
if (do_proc%depl_wetdep) e1_sec = e1_sec*exp( - dt*vnatsec/3.6e5)
e1_sec = 1.0 - e1_sec

! e3_pri_sec = (amol2/amol1)*dt*vchem/3.6e+05
if (do_proc%chem) then
   e3_pri_sec = (amol2/amol1)*dt*vchem/3.6e+05
else
   e3_pri_sec = 0.0
endif

! Loop over time steps:
DO itim = 1, ntim

    ! Production, loss:
    prod_sec = qpri*(1-e1_pri/2)*e3_pri_sec
    loss_sec = qsec*e1_sec + 0.5*qpri*(1-e1_pri/2)*e3_pri_sec*e1_sec   ! IS CORRECT 
    loss_pri = qpri * e1_pri

    ! Updates
    qpri     = qpri - loss_pri
    qsec     = qsec + prod_sec - loss_sec

ENDDO ! end loop over time steps

RETURN
END SUBROUTINE seccd


end module m_ops_seccmp
