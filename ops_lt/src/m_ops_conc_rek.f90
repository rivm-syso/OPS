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
! DESCRIPTION        : Compute concentration, taking into account source depletion factors for dry deposition,
!                      wet deposition and chemical conversion and the gradient between z = zra = 50 m and z = 4 m.
!
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: NON-ANSI F77
! CALLED FUNCTIONS   : ops_seccmp
! UPDATE HISTORY :
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_conc_rek

implicit none

contains

SUBROUTINE ops_conc_rek(do_proc, ueff, qbpri, isec, rc_sec_trj, routsec, ccc, amol1, amol2, sigz, utr, rc_sec_rcp, ra_rcp_4, ra_rcp_zra, &
                     &  rb_rcp, amol21, ugmoldep, cch, cgt, cgt_z, grof, percvk, onder, regenk, virty, ri, vw10, hbron, pcoef, &
                     &  rkc, disx, vnatpri, vchem, radius, xl, xloc, htot, twt, xvghbr, xvglbr, grad, frac, &
                     &  cdn, cq2, c, sdrypri, sdrysec, snatsec, somvnsec, telvnsec, vvchem, vtel, snatpri, somvnpri, &
                     &  telvnpri, ddepri, wdepri, drydep, wetdep, qsec, consec, pr, vd_eff_trj_zra, ra_trj_zra, rb_trj, rc_eff_rcp_4, vd_coarse_part, &
                     &  buildingFact, nparout, parout_val, parout_name, parout_unit, parout_write)

use m_commonconst_lt
use m_ops_output_lt
use m_ops_seccmp
use m_ops_brondepl, only: Tdo_proc

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_conc_rek')

! SUBROUTINE ARGUMENTS - INPUT
TYPE(Tdo_proc), INTENT(IN)                       :: do_proc                    ! options to switch on/off specific processes
REAL*4,    INTENT(IN)                            :: ueff                       ! 
REAL*4,    INTENT(IN)                            :: qbpri                      ! source strength current source (for current particle class) [g/s]
LOGICAL,   INTENT(IN)                            :: isec                       ! 
REAL*4,    INTENT(IN)                            :: rc_sec_trj                 ! canopy resistance secondary aerosol (SO4, NO3, NH4) for trajectory [s/m]
REAL*4,    INTENT(IN)                            :: routsec                    ! in-cloud scavenging ratio for secondary component
                                                                               ! (rout << rain-out = in-cloud) [-]
REAL*4,    INTENT(IN)                            :: ccc                        ! undepleted concentration at z = 0 m (including part of plume above mixing layer); 
                                                                               ! is needed for secondary species.
REAL*4,    INTENT(IN)                            :: amol1                      ! 
REAL*4,    INTENT(IN)                            :: amol2                      ! 
REAL*4,    INTENT(IN)                            :: sigz                       ! 
REAL*4,    INTENT(IN)                            :: utr                        ! average wind speed over the trajectory (m/s)
REAL*4,    INTENT(IN)                            :: rc_sec_rcp                 ! canopy resistance secondary aerosol (SO4, NO3, NH4) at the receptor [s/m]
REAL*4,    INTENT(IN)                            :: ra_rcp_4                   ! aerodynamic resistance at receptor, 4 m height [s/m];
REAL*4,    INTENT(IN)                            :: ra_rcp_zra                 ! aerodynamic resistance at receptor, height zra [s/m];
                                                                               ! zra is height where concentration profile is undisturbed by deposition = 50 m
REAL*4,    INTENT(IN)                            :: rb_rcp                     ! boundary layer resistance at receptor [s/m] 
REAL*4,    INTENT(IN)                            :: amol21                     ! 
REAL*4,    INTENT(IN)                            :: ugmoldep                   ! 
REAL*4,    INTENT(IN)                            :: cch                        ! source depletion factor for wet deposition/chemical conversion
REAL*4,    INTENT(IN)                            :: cgt                        ! gradient factor at 4 m height [-]
REAL*4,    INTENT(IN)                            :: cgt_z                      ! gradient factor at receptor height zm [-]
REAL*4,    INTENT(IN)                            :: grof                       ! 
REAL*4,    INTENT(IN)                            :: percvk                     ! fraction of occurrence of {distance/stability/wind-direction} class
REAL*4,    INTENT(IN)                            :: onder                      ! 
REAL*4,    INTENT(IN)                            :: regenk                     ! rain probability [-]
REAL*4,    INTENT(IN)                            :: virty                      ! 
REAL*4,    INTENT(IN)                            :: ri                         ! 
REAL*4,    INTENT(IN)                            :: vw10                       ! 
REAL*4,    INTENT(IN)                            :: hbron                      ! emission height at source (stack height), without plume rise [m]
REAL*4,    INTENT(IN)                            :: pcoef                      ! 
REAL*4,    INTENT(IN)                            :: rkc                        ! 
REAL*4,    INTENT(IN)                            :: disx                       ! 
REAL*4,    INTENT(IN)                            :: vnatpri                    ! 
REAL*4,    INTENT(IN)                            :: vchem                      ! 
REAL*4,    INTENT(IN)                            :: radius                     ! 
REAL*4,    INTENT(IN)                            :: xl                         ! 
REAL*4,    INTENT(IN)                            :: xloc                       ! 
REAL*4,    INTENT(IN)                            :: htot                       ! plume height at receptor, including plume descent due to heavy particles [m]
REAL*4,    INTENT(IN)                            :: twt                        ! 
REAL*4,    INTENT(IN)                            :: xvghbr                     ! 
REAL*4,    INTENT(IN)                            :: xvglbr                     ! 
REAL*4,    INTENT(IN)                            :: grad                       ! 
REAL*4,    INTENT(IN)                            :: frac                       ! fraction of this grid cell that is relevant
REAL*4,    INTENT(IN)                            :: ra_trj_zra                 ! aerodynamic resistance for trajectory, height zra [s/m];
                                                                               ! zra is height where concentration profile is undisturbed by deposition = 50 m  
REAL*4,    INTENT(IN)                            :: rb_trj                     ! boundary layer resistance for trajectory [s/m]
REAL*4,    INTENT(IN)                            :: rc_eff_rcp_4               ! effective canopy resistance at receptor, 4 m height, re-emission allowed [s/m] 
REAL*4,    INTENT(IN)                            :: vd_coarse_part             ! deposition velocity coarse particles [m/s]
REAL*4,    INTENT(IN)                            :: buildingFact               ! Building Effect interpolated from building table
INTEGER*4, INTENT(IN)                            :: nparout                    ! number of extra output parameters (besides concentration, deposition)
LOGICAL,   INTENT(IN)                            :: parout_write               ! write parout parameters to output

! SUBROUTINE ARGUMENTS - I/O
REAL*4,    INTENT(INOUT)                         :: cdn                        ! source depletion ratio for dry deposition for phase 3 (plume fully mixed over mixing layer)
REAL*4,    INTENT(INOUT)                         :: cq2                        ! source depletion ratio for dry deposition for phase 2 (plume not yet mixed over mixing layer) 
REAL*4,    INTENT(INOUT)                         :: c                          ! concentration at receptor height zm [ug/m3]
DOUBLE PRECISION, INTENT(INOUT)                  :: sdrypri                    ! 
DOUBLE PRECISION, INTENT(INOUT)                  :: sdrysec                    ! 
DOUBLE PRECISION, INTENT(INOUT)                  :: snatsec                    ! 
DOUBLE PRECISION, INTENT(INOUT)                  :: somvnsec                   ! 
DOUBLE PRECISION, INTENT(INOUT)                  :: telvnsec                   ! 
DOUBLE PRECISION, INTENT(INOUT)                  :: vvchem                     ! 
DOUBLE PRECISION, INTENT(INOUT)                  :: vtel                       ! 
DOUBLE PRECISION, INTENT(INOUT)                  :: snatpri                    ! 
DOUBLE PRECISION, INTENT(INOUT)                  :: somvnpri                   ! 
DOUBLE PRECISION, INTENT(INOUT)                  :: telvnpri                   ! 
DOUBLE PRECISION, INTENT(INOUT)                  :: ddepri                     ! 
DOUBLE PRECISION, INTENT(INOUT)                  :: wdepri                     ! 
DOUBLE PRECISION, INTENT(INOUT)                  :: drydep                     ! 
DOUBLE PRECISION, INTENT(INOUT)                  :: wetdep                     ! 
REAL*4,    INTENT(INOUT)                         :: parout_val(nparout)        ! values for extra output parameters, for current receptor

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: qsec                       ! 
REAL*4,    INTENT(OUT)                           :: consec                     ! 
REAL*4,    INTENT(OUT)                           :: pr                         ! 
REAL*4,    INTENT(OUT)                           :: vd_eff_trj_zra             ! effective deposition velocity over trajectory, taking into account amount of time that plume is above mixing
                                                                               !   height and no deposition takes place [m/s] 
CHARACTER(len=*), INTENT(OUT)                    :: parout_name(nparout)       ! names of extra output parameters                      
CHARACTER(len=*), INTENT(OUT)                    :: parout_unit(nparout)       ! units of extra output parameters                      

! LOCAL VARIABLES
REAL*4                                           :: qpri_depl                  ! depleted source strength = integrated mass flux [g/s]
REAL*4                                           :: vv                         ! 
REAL*4                                           :: drypri                     ! 
REAL*4                                           :: ddrup                      ! 
REAL*4                                           :: vdrup                      ! 
REAL*4                                           :: umid                       ! 
REAL*4                                           :: virnat                     ! 
REAL*4                                           :: dn                         ! 
REAL*4                                           :: dnatpri                    ! 
REAL*4                                           :: xvg                        ! factor not used; xvg = 1
REAL*4                                           :: cgtsec                     ! 
REAL*4                                           :: vd_sec_rcp_zra             ! deposition velocity secondary component at receptor, height zra [m/s]
REAL*4                                           :: vnatsec                    ! 
REAL*4                                           :: drysec                     ! 
REAL*4                                           :: dnatsec                    ! 
REAL*4                                           :: vd_rcp_4                   ! deposition velocity at receptor, 4 m height [m/s]
REAL*4                                           :: c_z                        ! concentration at receptor height zm [ug/m3]

!-------------------------------------------------------------------------------------------------------------------------------
! Initialisation 
drysec  = 0.
dnatsec = 0.
!
! Compute concentration, taking into account source depletion factors for dry deposition (cdn, cq2),
! source depletion factor for wet deposition/chemical conversion (cch), gradient factor (1-cgt) and building effect factor buildingFact.
!
! grof = 0 -> cdn*cch*(1. - cgt)*(1. - (1. - cq2)/(1. + grof)) = cdn*cch*(1. - cgt)*cq2
! grof = 1 -> cdn*cch*(1. - cgt)*(1. - (1. - cq2)/(1. + grof)) = cdn*cch*(1. - cgt)*(1 + cq2)/2    
!             (1+cq2)/2 = 0.5 for cq2 = 0 (all depleted)
!             (1+cq2)/2 = 1   for cq2 = 1 (nothing depleted), and linear for 0 < cq2 < 1 
!             meaning that the concentration is higher due to sedimentation 
! vv   = total source depletion factor for primary component
!
c_z = c*cdn*cch*(1. - cgt_z)*(1. - (1. - cq2)/(1. + grof))*buildingFact
c   = c*cdn*cch*(1. - cgt  )*(1. - (1. - cq2)/(1. + grof))*buildingFact
vv  = cdn*cq2*cch

!
! Dry deposition velocity
!
IF (grof .EQ. 1) THEN
   vd_rcp_4 = 1./(ra_rcp_4 + rb_rcp) + vd_coarse_part
ELSE
   vd_rcp_4 = 1./(ra_rcp_4 + rb_rcp + rc_eff_rcp_4)
ENDIF
!
! Compute drypri  = dry deposition mass flux [ug/m2/h] of primary component = -vd*concentration
! and     sdrypri = summed dry deposition [ug/m2/h] of primary component (weighed with fraction cell inside NL)  
!
! grof = 0 -> (1. - .5*grof) = 1 
! grof = 1 -> (1. - .5*grof) = 1/2 
!
! factor 3600   -> flux in ug/m2/h
! factor percvk -> fraction of occurrence of {distance/stability/wind-direction} class
!
drypri  = c*percvk*vd_rcp_4*3600.*(1. - .5*grof)
sdrypri = sdrypri + drypri*frac
!
! Correction of dry deposition source depletion factors (cdn, cq2) for plume above mixing layer.
! onder = fraction of plume below mixing height.
! Use a linear interpolation for source depletion factor between cq2 and 1 (i.e. no source depletion)
! plume completely below mixing height -> onder = 1 -> cq2 = cq2
! plume completely above mixing height -> onder = 0 -> cq2 = 1 (no source depletion)

!
cq2 = cq2 + (1. - cq2)*(1. - onder)
cdn = cdn + (1. - cdn)*(1. - onder)
!
! Correction for droplets that do not fall down perpendicularly;
! virnat = horizontal displacement of droplet = (hbron/v_droplet)*(average horizontal wind speed).
! Empirical relation a.o. Scott
!
IF ((regenk .GT. (0. + EPS_DELTA)) .AND. (ABS(virty) .LE. EPS_DELTA)) THEN
   ddrup  = .000895*(ri**.21)
   vdrup  = 130.*ddrup**.50
   umid   = vw10*(hbron/20.)**pcoef
   virnat = hbron/vdrup*umid
ELSE
   virnat = 0.
ENDIF
!
! Compute help parameter for wet deposition dn ("n" << nat = wet)
! rkc   : rkc = 1, obsolete factor [-]
! percvk: fraction of occurrence of {distance/stability/wind-direction} class [-]
! ueff  : wind speed [m/s]
! dn    = percvk*1e6*dt/A [s/m2 ug/g]; A = area on which wet deposition falls, during time step dt
!         (percvk dimensionless; 1e6: conversion from g -> ug)
!
!                            2 pi
! Sector with angle alpha = ------, N = nsek = number of sectors
!                             N
!                  B
!                 ___
!               .|   |
!          .     |   |
!     .          |   |                                     percvk
!.-------- x ----| A |L   A = L*B = (alpha*x)*(u*dt); dn = ------*1e6/(u*alpha*x)
!     .          |   |                                       100
!          .     |   |
!               .|___|
!
!
! Extra shift of 3 m to avoid divide by 0 (3 m ~ stack diameter)
!
dn = rkc/100.*percvk*1.e6/(ueff*2.*PI/12.* (disx + virty + 3. + virnat))
!
! Compute qpri_depl = Q(x) = depleted source strength (effect of all source depletion factors on source strength qbpri)
!
qpri_depl = qbpri*cdn*cq2*cch
!
! Compute dnatpri = wet deposition flux [ug/m2/h] of primary component 
! and     snatpri = summed wet deposition of primary component (weighed with fraction cell inside NL)
!         vnatpri: [%/h] wet deposition loss rate for primary components
!         qpri_depl     : [g/s]  
!         dn            : [s/m2 ug/g]
!         qpri_depl*dn  : [ug/m2] deposited mass per area, during time step dt; qpri_depl*dn = Q(x)*dt*percvk*1e6/A
! 
IF ((disx + virty) .LT. (virnat - EPS_DELTA)) THEN
   dnatpri = 0.
ELSE
   dnatpri = vnatpri*qpri_depl*dn
ENDIF
snatpri = snatpri + dnatpri*frac
!
! Sum wet deposition flux [ug/m2/h] of primary component and sum deposited mass per area qpri_depl*dn [ug/m2];
! later on we use this for computing effective wet deposition rate wdrpri = somvnpri/telvnpri [%/h]
!
somvnpri = somvnpri + dnatpri
telvnpri = telvnpri + qpri_depl*dn
!
! Compute concentration and deposition of secondary component (SO4, NO3, NH4)
! 
IF (isec) THEN
   IF (vchem .GT. (0. + EPS_DELTA)) THEN
      xvg  = 1.
      qsec = 0.
      CALL ops_seccmp(do_proc, qbpri, ueff, rc_sec_trj, routsec, ccc, vv, amol1, amol2, xvg, sigz, grad, utr, radius, disx, xl, xloc, vw10, &
                   &  pcoef, virty, regenk, htot, onder, twt, ri, cgt, xvghbr, xvglbr, vnatpri, vchem, ra_rcp_4, &
                   &  ra_rcp_zra, rb_rcp, rc_sec_rcp, pr, vnatsec, cgtsec, qsec, consec, vd_eff_trj_zra, ra_trj_zra, rb_trj)
      consec = consec*buildingFact
!
!     Compute for secondary component: 
!     vd_sec_rcp_zra: dry deposition velocity at receptor, height zra [m/s]
!     drysec        : dry deposition flux = -vd*C                     [ug/m3 m/s s/h] = [ug/m2/h]
!     consec        : concentration of plume below mixing layer,      
!                     assuming a vertical profile (factor 1-cgtsec)   [ug/m3]
!     dnatsec       : wet deposition flux                             [g/s 1/h s/m2 ug/g] = [ug/m2/h] 
!     sdrysec       : summed dry deposition flux                      [ug/m2/h]
!     snatsec       : summed wet deposition flux                      [ug/m2/h]
!     Summed fluxed (drysec and snatsec) are weighed with the fraction of cell inside NL
!                   
      vd_sec_rcp_zra = 1./(ra_rcp_zra + rb_rcp + rc_sec_rcp) 
      drysec         = consec*percvk*vd_sec_rcp_zra*3600*onder
      sdrysec        = sdrysec + drysec*frac
      consec         = consec*onder*(1. - cgtsec)
      dnatsec        = qsec*vnatsec*dn 
      snatsec        = snatsec + dnatsec*frac
!
!     Sum wet deposition flux for secondary component 
!
      IF (regenk .GT. (0. + EPS_DELTA)) THEN
         somvnsec = somvnsec + dnatsec
         telvnsec = telvnsec + (dnatsec/vnatsec)
      ENDIF
   ENDIF
ENDIF
!
! Sum chemical conversion rate (weighed with qpri_depl*dn = deposited mass per area [ug/m2]) 
!
vvchem = vvchem + (vchem*qpri_depl*dn)
vtel   = vtel + (qpri_depl*dn)
!
! Sum deposition (drydep = dry/primary+secondary, ddepri = dry/primary, wetdep = wet/primary+secondary);
! convert from ug/m2/h to mol/ha/y 
!
drydep = drydep + (drypri*amol21 + drysec)*ugmoldep
ddepri = ddepri +  drypri*amol21*ugmoldep
wetdep = wetdep + (dnatpri*amol21 + dnatsec)*ugmoldep
wdepri = wdepri +  dnatpri*amol21*ugmoldep

! Fill arrays with extra output parameters:
IF (parout_write) CALL ops_parout_fill(nparout, ra_rcp_4, rb_rcp, rc_eff_rcp_4, percvk, parout_val, parout_name, parout_unit)

! Output concentration is concentration at receptor height:
c = c_z

RETURN
END SUBROUTINE ops_conc_rek

end module m_ops_conc_rek
