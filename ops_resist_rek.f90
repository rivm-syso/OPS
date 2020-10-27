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
! LANGUAGE           : FORTRAN-F77/90
! USAGE              : %M%
! DESCRIPTION        : Compute resistances for dry deposition.
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP Fortran
! CALLED FUNCTIONS   : ops_depu
! UPDATE HISTORY :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_resist_rek(vchemc, iopt_vchem, vchemv, rad, isec, icm, rcso2, regenk, rcaerd, iseiz, istab, itra, ar,            &
                          rno2nox, rcnh3d, vchemnh3, vchem2, hum, uster_rcp, ol_rcp, uster_tra, ol_tra,                         &
                          z0_rcp, z0_metreg_rcp, rcno2d, kdeel, mb, vw10, temp_C, disx, zm, koh,                                   &
                          rations, rhno3, rcno, rhno2, rchno3, croutpri, rrno2nox, rhno3_rcp,                                   &
                          rb, ra4, ra50, rc, routpri, vchem, rcsec, uh, rc_sec_rcp, rc_rcp, rb_rcp,                             &
                          ra4_rcp, ra50_rcp, raz_rcp, z0_src, ol_src, uster_src, z0_tra, rctra_0, rcsrc, ra4src,                &
                          rb_src, ra50src, ra4tra, ra50tra, rb_tra, rclocal, nh3bg_rcp, nh3bgtra,                               &
                          so2bg_rcp, so2bgtra, gym, depudone, gasv, lu_rcp_per, lu_tra_per, rnox)

USE m_commonconst
USE m_ops_vchem

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_resist_rek')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: vchemc                     ! chemical conversion rate [%/h]
INTEGER*4, INTENT(IN)                            :: iopt_vchem                 ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
REAL*4,    INTENT(IN)                            :: vchemv                     ! 
REAL*4,    INTENT(IN)                            :: rad                        ! 
LOGICAL,   INTENT(IN)                            :: isec                       ! TRUE als component=[SO2, NOx, NH3]
INTEGER*4, INTENT(IN)                            :: icm                        ! 
REAL*4,    INTENT(IN)                            :: rcso2                      ! 
REAL*4,    INTENT(IN)                            :: regenk                     ! 
REAL*4,    INTENT(IN)                            :: rcaerd                     ! surface resistance NO3_aerosol [s/m]
INTEGER*4, INTENT(IN)                            :: iseiz                      ! 
INTEGER*4, INTENT(IN)                            :: istab                      ! 
INTEGER*4, INTENT(IN)                            :: itra                       ! 
REAL*4,    INTENT(IN)                            :: ar                         ! 
REAL*4,    INTENT(IN)                            :: rno2nox                    ! 
REAL*4,    INTENT(IN)                            :: rcnh3d                     ! 
REAL*4,    INTENT(IN)                            :: vchemnh3
type(Tvchem), INTENT(IN)                         :: vchem2                     ! 
REAL*4,    INTENT(IN)                            :: hum                        ! 
REAL*4,    INTENT(IN)                            :: uster_rcp                  ! friction velocity at receptor; for z0 at receptor [m/s]
REAL*4,    INTENT(IN)                            :: ol_rcp                     ! Monin-Obukhov length at receptor; for z0 at receptor [m/s]
REAL*4,    INTENT(IN)                            :: uster_tra                  ! 
REAL*4,    INTENT(IN)                            :: ol_tra                     ! 
REAL*4,    INTENT(IN)                            :: z0_rcp                     ! roughness length at receptor; from z0-map [m]
REAL*4,    INTENT(IN)                            :: z0_metreg_rcp              ! roughness length at receptor; interpolated from meteo regions [m]
REAL*4,    INTENT(IN)                            :: rcno2d                     ! 
INTEGER*4, INTENT(IN)                            :: kdeel                      ! 
INTEGER*4, INTENT(IN)                            :: mb                         ! 
REAL*4,    INTENT(IN)                            :: vw10                       ! 
REAL*4,    INTENT(IN)                            :: temp_C                     ! temperature at height zmet_T [C]
REAL*4,    INTENT(IN)                            :: disx                       ! 
REAL*4,    INTENT(IN)                            :: zm                         ! 
REAL*4,    INTENT(IN)                            :: koh                        ! 
REAL*4,    INTENT(IN)                            :: rations                    ! 
REAL*4,    INTENT(IN)                            :: rhno3                      ! 
REAL*4,    INTENT(IN)                            :: rcno                       ! surface resistance for NO [s/m]
REAL*4,    INTENT(IN)                            :: rhno2                      ! ration hno2/nox
REAL*4,    INTENT(IN)                            :: rchno3                     ! HNO3
REAL*4,    INTENT(IN)                            :: croutpri                   ! constant (initial) in-cloud scavenging ratio [-] for primary component                 
REAL*4,    INTENT(IN)                            :: rrno2nox                   ! ruimtelijke variatie in no2/nox verhouding
REAL*4,    INTENT(IN)                            :: rhno3_rcp                  ! 
REAL*4,    INTENT(IN)                            :: z0_src                     ! roughness length at source; from z0-map [m]
REAL*4,    INTENT(IN)                            :: ol_src                     ! 
REAL*4,    INTENT(IN)                            :: uster_src                  ! 
REAL*4,    INTENT(IN)                            :: z0_tra                     ! roughness length representative for trajectory [m]
REAL*4,    INTENT(IN)                            :: nh3bg_rcp                  ! 
REAL*4,    INTENT(IN)                            :: nh3bgtra                   ! 
REAL*4,    INTENT(IN)                            :: so2bg_rcp                  ! 
REAL*4,    INTENT(IN)                            :: so2bgtra                   ! 
REAL*4,    INTENT(IN)                            :: gym
LOGICAL,   INTENT(IN)                            :: gasv
REAL*4,    INTENT(IN)                            :: lu_rcp_per(NLU)            ! land use percentages for all land use classes of receptor
REAL*4,    INTENT(IN)                            :: lu_tra_per(NLU)            ! land use percentages for all land use classes over trajectory

! SUBROUTINE ARGUMENTS - I/O
REAL*4,    INTENT(INOUT)                         :: rb                         ! 
REAL*4,    INTENT(INOUT)                         :: ra4                        ! 
REAL*4,    INTENT(INOUT)                         :: ra50                       ! 
LOGICAL,   INTENT(INOUT)                         :: depudone

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: routpri                    ! in-cloud scavenging ratio for primary component
                                                                               ! (rout << rain-out = in-cloud) [-] 
REAL*4,    INTENT(OUT)                           :: vchem                      ! chemical conversion rate [%/h]
REAL*4,    INTENT(OUT)                           :: uh                         ! 

! Canopy resistances
! Note: for particles, Rc is defined in ops_depoparexp 
REAL*4,    INTENT(OUT)                           :: rc                         ! 
REAL*4,    INTENT(OUT)                           :: rcsec                      ! 
REAL*4,    INTENT(OUT)                           :: rc_sec_rcp                 ! 
REAL*4,    INTENT(OUT)                           :: rcsrc                      ! canopy resistance at the source, no re-emission allowed [s/m]; is used for the computation of 
                                                                               ! cq1 = source depletion ratio for dry deposition for phase 1, area source
REAL*4,    INTENT(OUT)                           :: rctra_0                    ! canopy resistance representative for the trajectory, no re-emission allowed [s/m];
                                                                               ! is used for source depletion (loss over trajectory)
REAL*4,    INTENT(OUT)                           :: rc_rcp                     ! canopy resistance at receptor, no re-emission allowed [s/m];
                                                                               ! is used for deposition gradient at receptor
REAL*4,    INTENT(OUT)                           :: rclocal                    ! canopy resistance at receptor, re-emission allowed [s/m];
                                                                               ! is used for the computation of drypri, the local depsosition at the receptor
																			 
REAL*4,    INTENT(OUT)                           :: rb_rcp                     ! 
REAL*4,    INTENT(OUT)                           :: ra4_rcp                    ! 
REAL*4,    INTENT(OUT)                           :: ra50_rcp                   ! 
REAL*4,    INTENT(OUT)                           :: ra4src                     ! 
REAL*4,    INTENT(OUT)                           :: rb_src                     ! 
REAL*4,    INTENT(OUT)                           :: ra50src                    ! 
REAL*4,    INTENT(OUT)                           :: ra4tra                     ! 
REAL*4,    INTENT(OUT)                           :: ra50tra                    ! 
REAL*4,    INTENT(OUT)                           :: rb_tra                     ! 
REAL*4,    INTENT(OUT)                           :: raz_rcp
REAL*4,    INTENT(OUT)                           :: rnox                       ! NO2/NOx ratio
 

! LOCAL VARIABLES
INTEGER*4                                        :: day_of_year                ! 
INTEGER*4                                        :: icmpsec
INTEGER*4                                        :: ipar                       ! 
INTEGER*4                                        :: mnt                        ! 
INTEGER*4, DIMENSION(2)                          :: mnt_select                 ! 
INTEGER*4                                        :: nwet                       ! 
INTEGER*4                                        :: icnr                       ! 
INTEGER*4                                        :: luclass                    ! 
REAL*4                                           :: percn                      ! 
REAL*4                                           :: chemn                      ! 
REAL*4                                           :: scno2nox                   ! 
REAL*4                                           :: chemr                      ! 
REAL*4                                           :: rcno2                      ! 
REAL*4                                           :: r                          ! 
REAL*4                                           :: glrad                      ! 
REAL*4                                           :: d                          ! 
REAL*4                                           :: ratns                      ! 
REAL*4                                           :: rcc                        ! 
REAL*4                                           :: vdc                        ! 
REAL*4                                           :: rcaer                      ! 
REAL*4                                           :: vdaer                      ! 
REAL*4                                           :: vg                         ! 
REAL*4                                           :: rchno2                     ! 
REAL*4                                           :: dh                         ! 
REAL*4                                           :: fx                         ! weegfactor
REAL*4                                           :: som_rc_rcp 
REAL*4                                           :: som2_rc_rcp 
REAL*4                                           :: som_rc_local 
REAL*4                                           :: som2_rctra_0 
REAL*4                                           :: som_rctra_0 
REAL*4                                           :: som_rcsrc
REAL*4                                           :: telmaand 
REAL*4                                           :: catm 
REAL*4                                           :: c_ave_prev_nh3
REAL*4                                           :: c_ave_prev_so2
REAL*4                                           :: cfact
REAL*4                                           :: ccomp_tot
REAL*4                                           :: rc_tot
REAL*4                                           :: rc_sum
REAL*4                                           :: sinphi
INTEGER                                          :: i
REAL*4, PARAMETER                                :: catm_min = 0.1E-05

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------

! Convert global radiation from J/cm2/h to J/m2/s = W/m2;
! conversion factor = 1.0e4/3600 = 2.78
glrad = rad*2.78

! Chemical conversion rate is partly constant and partly radiation dependent (OPS report Eq. 7.2):
vchem = vchemc + vchemv*glrad

if ( gasv .OR. (.not.gasv .AND. .not.depudone)) THEN

   ! displacement height (usually 0.7 * vegetation height) (m)
   ! the effect of the displacement height is neglected here;
 
   d = 0.

   ! Component number icnr for calculation of Rb;
   ! currently only for SO2 or NH3; for NO2 Rc values are high and Rb not so important.
   IF (icm .EQ. 3) THEN
      icnr = 9
   ELSE
      icnr = 1
   ENDIF

   ! Calculate deposition parameters vg, Ra and Rb (=rb) with vg=1./(Ra+Rb+Rc) at source/4 m height, 
   ! receptor/4 m height and receptor/50 m height:
   ! Only results for ra... and rb... are used, vg NOT
   CALL ops_depu(icnr, z0_rcp,    zm, d, rc_rcp,  ol_rcp,   uster_rcp,   vg, raz_rcp,   rb_rcp)
   CALL ops_depu(icnr, z0_src,    4., d, rcsrc,   ol_src,   uster_src,   vg, ra4src,    rb_src)
   CALL ops_depu(icnr, z0_rcp,    4., d, rc_rcp,  ol_rcp,   uster_rcp,   vg, ra4_rcp,   rb_rcp)
   CALL ops_depu(icnr, z0_rcp,   50., d, rc_rcp,  ol_rcp,   uster_rcp,   vg, ra50_rcp,  rb_rcp)

   ! Set Ra at source/50m height equal to Ra at receptor/50 m height:
   ra50src = ra50_rcp

   ! Compute Ra averaged over trajectory at 4 m height and 50 m height; 
   ! ra4 = Ra(4 m) from meteo statistics, based on z0_metreg_rcp, which is interpolated from meteo regions.
   ! 4.3 OPS report, neglecting the stability corrections psi_h and assuming that u* remains the same.

   !  Ra(z=4,trajectory)       z=4              z=4
   ! ------------------- = ln(-------) / ln(-------------)
   !    Ra(z=4)               z0_tra        z0_metreg_rcp
    
   ra4tra  = ra4*alog(4/z0_tra)/alog(4/z0_metreg_rcp)
   ra50tra = ra50*alog(50/z0_tra)/alog(50/z0_metreg_rcp)

   ! Compute Rb averaged over trajectory 4.4 OPS report:
   rb_tra = rb_rcp*uster_rcp/uster_tra

   ! Deposition parameters have been computed
   depudone = .TRUE.
ENDIF

! For acidifying components:
IF (isec) THEN

   ! SO2:
   IF (icm .EQ. 1) THEN
!
!     Compute conversion rates and resistances for SO2.
!     vchem  : chemical conversion rate for SO2 -> SO4 (%/h)
!     routpri: in-cloud scavenging ratio for primary component; for SO2 depends on NH3/SO2 ratio
!             (rout << rain-out = in-cloud) [-])
!     rc     : surface resistance primary component
!     rcsec  : surface resistance secondary component over trajectory; taken as 0.8*Rc(NO3_aerosol)
!
!  ar = proportionality constant [ppb J-1 cm2 h] in relation [OH] = ar Qr, with 
!  [OH] = OH radical concentration [ppb] , Qr = global radiation in J/cm2/h, see 
!  Van Egmond N.D. and Kesseboom H. (1985) A numerical mesoscale model for long-term average NOx and NO2-concentration. 
!  Atmospheric Environment 19, 587-595.
!  Table 6.1 OPS-report:
!  ar(summer) = 7345 molec cm-3 W-1 m2
!  ar(winter) = 3540 molec cm-3 W-1 m2.
!  Cnversion factors seconds_per_hour = 3600; cm2_per_m2 = 10^4.
!     at T = 20 C, p = 1013 mb:
!        ppbfac = conversion factor ppb -> molec/cm3 = 2.5029e10 molec/cm3/ppb
!        ar(summer) = 7345*cm2_per_m2/(ppbfac*seconds_per_hour) = 81.5e-8 ppb J-1 cm2 h.
!        ar(winter) = 3540*cm2_per_m2/(ppbfac*seconds_per_hour) = 39.3e-8 ppb J-1 cm2 h.
!        ar(year)   = average of ar_winter) and ar_wummer)      = 60.4e-8 ppb J-1 cm2 h.
!     at T = 25 C, p = 1013 mb:
!        ppbfac     =  2.4610e+010 molec/cm3/ppb
!        ar(summer) =  82.9e-8 ppb J-1 cm2 h.
!        ar(winter) =  40.0e-8 ppb J-1 cm2 h.
!        ar(year)   =  61.4e-8 ppb J-1 cm2 h.
!
!  For a specific month, a cos-function over the year is used, such that ar(average) = 62e-8, ar(Feb) = 40e-8, ar(Aug) = 83e-8.
!     Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec
!     32.1 40.1 54.0 70.0 83.9 91.9 91.9 83.9 70.0 54.0 40.1 32.1 *1e-8
!
! k_ho	SO2 + OH -> sulphate (gas phase)      3.44 x 10-12 cm3 molec-1 s-1 
!        at T = 20 C, p = 1013 mb: ppbfac = conversion factor ppb -> molec/cm3 = 2.5029e10 molec/cm3/ppb ->
!        k_ho = 3.44e-12*3600*2.5029e10 = 309.96 ppb-1 h-1     
!        k_ho*[OH] = k_ho*ar*Q = 100*309.96*62e-8*Q %/h = 0.0192*Q %/h
! k_he	SO2 -> sulphate (Particle phase) 1.7 x 10-6 s-1 = 0.612 %/h
! k_aq	SO2 -> sulphate (Aqueous phase)  4.0 x 10-5 s-1 = 14.4  %/h
!
! Note: in source code 1.2*[0.016 0.5 12] = [0.0192 0.6 14.4] = [ar khe kaq]

      IF (iopt_vchem .eq. 0) THEN
         ! OPS parameterisation:
         vchem = 1.2*((rad*.016) + .5 + (regenk*12.))
      ELSE
         ! EMEP maps:
         vchem = vchem2%vchem
      ENDIF
      ! write(*,*) 'ops_resist_rek, vchem: ',vchem
      routpri  = croutpri*rations ! Note factor 2 in rations
      rc       = rcso2 
      rcsec    = rcaerd*0.8

   ! NOx: 
   ELSE IF (icm .EQ. 2) THEN

      ! Compute percn = fraction of nighttime hours, depending on season;
      ! NACHTZOMER and NACHTWINTER are relative occurrences (%) of nighttime hours in summer and winter,
      ! for each stability class and distance class. ("NACHT" = night, "ZOMER" = summer)
      IF ((iseiz .EQ. 3) .OR. (iseiz .EQ. 5)) THEN
         percn = FLOAT(NACHTZOMER(istab, itra))/100.
      ELSE IF ((iseiz .EQ. 2) .OR. (iseiz .EQ. 4)) THEN
         percn = FLOAT(NACHTWINTER(istab, itra))/100.
      ELSE
         percn = FLOAT(NACHTWINTER(istab, itra) + NACHTZOMER(istab, itra))/200.
      ENDIF

      ! Compute chemn = chemical conversion rate for NO2+O3 -> NO3 (nigthttime), assuming a 2%/h conversion rate
      ! Van Egmond N.D. and Kesseboom H. (1983) Mesoscale air pollution dispersion models-II. Lagrangian PUFF model,
      ! and comparison with Eulerian GRID model. Atmospheric Environment, 17, 265-274. 
      chemn = percn*2.

      ! rnox = [NO2]/[NOx] ratio consists of a space varying component (rrno2nox, computed in ops_par_chem),
      ! a season dependent component (rno2nox, set in ops_init)
      ! and a stability class dependent component (scno2nox, only in winter)
      IF ((iseiz .EQ. 2) .OR. (iseiz .EQ. 4)) THEN
         scno2nox = SCWINTER(istab)
      ELSE
         scno2nox = 1.
      ENDIF
      rnox = rrno2nox*rno2nox*scno2nox

      ! chemr : chemical conversion rate for NO2 + OH -> HNO3; [%/h] (factor 100 is to make percentage instead of fractions)
      !         rad : global radiation [J/cm2/h]
      !         ar  : proportionality constant [ppb J-1 cm2 h] in relation [OH] = ar Qr, with 
      !               [OH] = OH radical concentration [ppb] , Qr = global radiation [J/cm2/h] 
      !         koh : reaction constant [ppb-1 h-1] (Van Aalst en Bergsma, 1981)
      ! vchem  : total chemical conversion rate, split into daytime and nighttime part
       chemr    = 100*rad*ar*koh*rnox

      ! vchem  : total chemical conversion rate, split into daytime and nighttime part
      IF (iopt_vchem .eq. 0) THEN
         ! OPS parameterisation:
         vchem = chemr + chemn
      ELSE
         ! EMEP maps:
         vchem = vchem2%vchem
      ENDIF 
    
      ! routpri: in-cloud scavenging ratio for primary component (rout << rain-out = in-cloud) [-]
      routpri  = croutpri*rnox
!
!     Set surface resistance.
!     The primary substance is calculated as NO2 (because emissions are specified as such) but contains in reality a mixture of
!     NO, NO2 and HNO2. The whole is (finally) mentioned NOx and specified in ppb. Therefore dry deposition velocities have to
!     be calculated as representative for the NO-NO2-HNO2 mixture
!
      rcno2  = rcno2d      ! interpolated value from meteo statistics (ops_statparexp)
      r      = rb + ra4    ! R = Rb + Ra = Rb + Ra = boundary layer resistance + aerodynamic resistance 
      rchno2 = rcso2       ! Rc(HNO2) = Rc(SO2) 

      ! 
      ! 
      !            1             [NO2]/[NOx]         (1-[NO2]/[NOx])        [HNO2]/[NOx]
      ! ------------------- = ------------------ + -------------------- + --------------------
      !  Rc(NOx) + Rb + Ra     Rc(NO2)+ Rb + Ra      Rc(NO) + Rb + Ra      Rc(HNO2) + Rb + Ra
      ! 

      !            1             [HNO3]/[NO3]_totaal     (1-[HNO3]/[NO3]_totaal)
      ! ------------------- = ----------------------- + ---------------------------- 
      !  Rc(NO3) + Rb + Ra       Rc(HNO3)+ Rb + Ra        Rc(NO3_aerosol) + Rb + Ra    
          
      rc    = 1./(rnox/(rcno2+r) + (1.-rnox)/(rcno+r) + rhno2/(rchno2+r)) - r
      rcsec = 1./(rhno3/(r+rchno3) + (1.-rhno3)/(r+rcaerd)) - r
!
!   icm = 3: NH3
!
   ELSE IF (icm .EQ. 3) THEN

!     Compute conversion rates and resistances for NH3.
!
!                                                    Rb(NH3)     M(NH3)  1/3   17  1/3
!     rb     : boundary layer resistance Rb [s/m];  -------- = (--------)  = (----)  = 0.6431
!              (4.5 OPS report)                      Rb(SO2)     M(SO2)        64
!
!     vchem  : chemical conversion rate for NH3 -> NH4 conversion [%/h]
!     routpri: in-cloud scavenging ratio for primary component
!              (rout << rain-out = in-cloud) [-]
!     rc     : surface resistance primary component [s/m]
!     rcsec  : surface resistance secondary component over trajectory [s/m]; taken as 0.8*Rc(NO3_aerosol)

      rb       = rb*0.64
      IF (iopt_vchem .eq. 0) THEN
         ! OPS parameterisation:
         vchem = vchemnh3
      ELSE
         ! EMEP maps:
         vchem = vchem2%vchem
      ENDIF
      routpri  = croutpri
      rc       = rcnh3d
      rcsec    = rcaerd*0.8
   ELSE
      CONTINUE
   ENDIF ! IF icm = 1,2 or 3

!-------------------------------------------------------------------------------------------
!  Compute surface resistance Rc for acidifying components using the DEPAC module 
!      rcsrc  : canopy resistance at the source, re-emission allowed [s/m]; is used for the computation of 
!               cq1 = source depletion ratio for dry deposition for phase 1, area source
!      rctra_0: canopy resistance representative for the trajectory, no re-emission allowed [s/m];
!               is used for source depletion (loss over trajectory)
!      rc_rcp : canopy resistance at receptor, no re-emission allowed [s/m];
!               is used for deposition gradient at receptor
!      rclocal: canopy resistance at receptor, re-emission allowed [s/m]; 
!               is used for the computation of drypri, the local depsosition at the receptor
!-------------------------------------------------------------------------------------------

   ! Wesely parameterization
   ipar  = 2

   ! Vapour pressure deficit (mbar)
   d = 0.

   ! N/S ratio indicator
   ratns = 2.

   ! Wind speed for rough surfaces, such as forests (z0 > 0.5 m) -> from logarithmic wind profile at 8*z0(receptor);
   ! for smoother surfaces (z0 <= 0.5) -> set equal to wind speed at 10 m height.
   IF (z0_rcp .GT. 0.5 + EPS_DELTA) THEN
      CALL ops_wvprofile(z0_rcp,8*z0_rcp,uster_rcp,ol_rcp, uh)
   ELSE
      uh = vw10
   ENDIF
!
!
!  Compute nwet = wetness indicator depending on percipitation probability and humidity;
!                 dry(=0),wet(=1) or snow(=9); 6.25 OPS report
!
   nwet=NINT((regenk * 0.4 + hum/59 - 0.4)**5*.3)                              ! 990324
   IF(nwet.gt.1) nwet=1

!   1. Compute average surface resistance Rc over trajectory.
!      In case of NH3 a resistance scheme with compensationpoints is used for which two extra inputs are needed.
!      c_ave_prev   air concentration averaged over a previous period (e.g. previous year or month) (ug/m3);
!                   we use here the NH3 background concentration along the trajectory
!      catm         actual atmospheric concentration (ug/m3); we use here the NH3 background concentration along the trajectory;
!      the output Rc is returned in rctra_0 = effective Rc over the trajectory. 
!
!      (17/24) = conversion factor ppb -> ug/m3.
!
   catm           = nh3bgtra*17/24
   c_ave_prev_nh3 = nh3bgtra*17/24
   c_ave_prev_so2 = so2bgtra*64/24
!
   CALL ops_depos_rc(icm, iseiz, mb, gym ,temp_C, uster_tra, glrad, hum, nwet, ratns, catm, c_ave_prev_nh3, c_ave_prev_so2, lu_tra_per, &
                  &  ra4tra, rb_tra, rctra_0, rclocal)
   rcsrc   = rctra_0
!
! 2. Compute surface resistance Rc near the receptor.
!    The same as above, but now we use the NH3 background concentration at the receptor as inputs;
!    the output Rc is returned in rc_rcp = effective Rc near the receptor (always positive). 
!    rclocal = effective Rc near the receptor (might become a negative value)
!    Note: catm and c_ave_prev are only used for NH3.
!    Conversion from ppb -> ug/m3 for nh3bg_rcp already done in ops_rcp_char.
!
   catm           = nh3bg_rcp
   c_ave_prev_nh3 = nh3bg_rcp
   c_ave_prev_so2 = so2bg_rcp
!
   CALL ops_depos_rc(icm, iseiz, mb, gym ,temp_C, uster_rcp, glrad, hum, nwet, ratns, catm, c_ave_prev_nh3, c_ave_prev_so2, lu_rcp_per, &
                  &  ra4_rcp, rb_rcp, rc_rcp, rclocal)
!------------------------------------------------------------------
!  Compute surface resistance Rc for the secondary component
!------------------------------------------------------------------
!
!  Set component number for secondary component of SO2, NO2, NH3:
!
!  11: sulphate
!  12: nitrate
!  13: ammonium
!
!  and set a default secondary component = 11 (treated as sulphate)
!
   IF (ANY(icm == (/1,2,3/))) THEN
      IF (CNAME(icm,5) .EQ. "SO2") THEN
         icmpsec = 11
      ELSEIF (CNAME(icm,5) .EQ. "NO2") THEN
         icmpsec = 12
      ELSEIF (CNAME(icm,5) .EQ. "NH3") THEN
         icmpsec = 13
      ENDIF
   ELSE
      icmpsec = 11
   ENDIF
!
!  Compute vdaer = deposition velocity at receptor for secondary aerosols
! 
   CALL vdsecaer (uster_rcp, ol_rcp, vdaer, hum, nwet, uh, ra50_rcp, z0_rcp, icmpsec) 

   IF (icm .EQ. 2) THEN   
!
!     NOx
!     Rc for NOx is, uptil now, Rc for NO2 (from DEPAC); now we compute the 
!     effective Rc for NOx (= NO+NO2+HNO2)
! 
!            1                [NO2]/[NOx]         (1-[NO2]/[NOx])        [HNO2]/[NOx]
!     ------------------- = ------------------ + -------------------- + --------------------
!      Rc(NOx) + Rb + Ra     Rc(NO2)+ Rb + Ra      Rc(NO) + Rb + Ra      Rc(HNO2) + Rb + Ra
! 
      r          = rb + ra4
      rc_rcp    = 1./(rnox/(rc_rcp+r)  + (1.-rnox)/(rcno+r) + rhno2/(rchno2+r)) - r 
      rclocal    = rc_rcp
      rctra_0    = 1./(rnox/(rctra_0+r) + (1.-rnox)/(rcno+r) + rhno2/(rchno2+r)) - r
      rcsrc      = rctra_0
!
!     Rc for secondary component: 1/vd = Ra + Rb + Rc and Rb is neglected for aerosols
!
      rc_sec_rcp = 1./vdaer-ra50_rcp
!
!     rc_sec_rcp is valid for NO3 aerosol. Calculate now a weighted value for the NO3+HNO3 mixture 
!
!              1               [HNO3]/[NO3]_totaal     (1-[HNO3]/[NO3]_totaal)
!     ------------------- = ----------------------- + ---------------------------- 
!      Rc(NO3) + Rb + Ra       Rc(HNO3)+ Rb + Ra        Rc(NO3_aerosol) + Rb + Ra    

      rc_sec_rcp=1./(rhno3_rcp/(r+rchno3) + (1.-rhno3_rcp)/(r+rc_sec_rcp)) - r

   ELSE
!
!     Secondary components not from NOx; 1/vd = Ra + Rb + Rc and Rb is neglected
!
      rc_sec_rcp = 1./vdaer - ra50_rcp
   ENDIF

ELSE
!
!  Non-acidifying components
!
   rc_rcp    = rc
   rctra_0    = rc
   rcsrc      = rc
   rclocal    = rc
ENDIF

RETURN

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : vdsecaer
! DESCRIPTION        : calculation of dry deposition velocity for particles according
!                      to Wesely et al. (1985) for low vegetation and other areas with
!                      a roughness length (znul) < 0.5m and to Erisman et al (1994) and
!                      Ruijgrok et al. (1994) for forest and other areas with a roughness
!                      length above 0.5m.
! AUTHOR             : OPS-support 
!-------------------------------------------------------------------------------------------------------------------------------

SUBROUTINE vdsecaer (ust, ol, vd, rh, nwet,Uh, ra, znul, icmp)

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'vdsecaer')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: icmp                       ! 
INTEGER*4, INTENT(IN)                            :: nwet                       ! 
REAL*4,    INTENT(IN)                            :: ust                        ! 
REAL*4,    INTENT(IN)                            :: ol                         ! 
REAL*4,    INTENT(IN)                            :: rh                         ! 
REAL*4,    INTENT(IN)                            :: Uh                         ! 
REAL*4,    INTENT(IN)                            :: ra                         ! 
REAL*4,    INTENT(IN)                            :: znul                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: vd                         ! 

! LOCAL VARIABLES
REAL*4                                           :: E                          ! 

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
! components:
! 11 = SO4
! 12 = NO3
! 13 = NH4
!
! Calculation of the collecting efficiency E for different components
! for different relative humidity and surface wetness. AI.15 OPS report
!
! Sulphate:
!
IF (icmp.eq.11) THEN
  IF (rh.le.80) THEN
    IF (nwet.ge.1) THEN
      E = 0.08*ust**0.45
    ELSE
      E = 0.05*ust**0.28
    ENDIF
  ELSE
    IF (nwet.ge.1) THEN
      E = 0.08*ust**0.45*(1+0.37*EXP((rh-80)/20))
    ELSE
      E = 0.05*ust**0.28*(1+0.18*EXP((rh-80)/20))
    ENDIF
  ENDIF
!
! Nitrate:
!
ELSE IF (icmp.eq.12) THEN
  IF (rh.le.80) THEN
    IF (nwet.ge.1) THEN
      E = 0.10*ust**0.43
    ELSE
      E = 0.063*ust**0.25
    ENDIF
  ELSE
    IF (nwet.ge.1) THEN
      E = 0.10*ust**0.43*(1+0.37*EXP((rh-80)/20))
    ELSE
      E = 0.063*ust**0.25*(1+0.18*EXP((rh-80)/20))
    ENDIF
  ENDIF
!
! Ammonium:
!
ELSE IF (icmp.eq.13) THEN
  IF (rh.le.80) THEN
    IF (nwet.ge.1) THEN
      E = 0.066*ust**0.41
    ELSE
      E = 0.05*ust**0.23
    ENDIF
  ELSE
    IF (nwet.ge.1) THEN
      E = 0.066*ust**0.41*(1+0.37*EXP((rh-80)/20))
    ELSE
      E = 0.05*ust**0.23*(1+0.18*EXP((rh-80)/20))
    ENDIF
  ENDIF
ENDIF
!
! Calculation of the deposition velocity vd
! for low vegetation and other areas with a roughness length
! below 0.5m by Wesely et al (1985) (for stable/neutral conditions
! (ol<0) and stable conditions (ol>0)), and for forest and other
! areas with a roughness length above 0.5m by Erisman et al (1994), AI.12, AI.13, AI.14 OPS report
!
! Sulphate, nitrate and ammonium
!
IF (znul.lt.0.5) THEN
  IF (ol.lt.0) THEN
    vd = (ust/500.)*(1.+((300./(-1.*ol))**(2./3.)))  ! 960515
  ELSE
    vd = ust/500
  ENDIF
  vd = 1./(1./vd+ra)
ELSE
  vd = 1./((1./(((ust*ust)/Uh)*E)) + ra) 
ENDIF

RETURN

END SUBROUTINE vdsecaer

END SUBROUTINE ops_resist_rek
