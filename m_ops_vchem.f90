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
! DESCRIPTION        : Module for chemical conversion rates
!-------------------------------------------------------------------------------------------------------------------------------
MODULE m_ops_vchem

use m_aps

IMPLICIT NONE

type Tvchem
  
   TYPE (TApsGridReal) :: mass_prec_grid               ! APS grid with column averaged mass of precursor pre chemistry step (from chemistry model, e.g. EMEP) [ug/m2]
   TYPE (TApsGridReal) :: mass_conv_dtfac_grid         ! APS grid with (100/dt) * column averaged mass, converted during chemistry step (from chemistry model, e.g. EMEP) [(ug/m2) (%/h)]

   real                :: mass_prec_tra                ! column averaged mass of precursor pre chemistry step, average between source - receptor [ug/m2]
   real                :: mass_conv_dtfac_tra          ! (100/dt) * column averaged mass, converted during chemistry step, average between source - receptor [(ug/m2) (%/h)]

   real                :: vchem                        ! chemical conversion rates for net reaction primary -> secondary species [%/h]

end type Tvchem

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_vchem(icm, isec, iopt_vchem, vchemc, vchemv, vchemnh3, vchem2, rad, rad_W_m2, regenk, iseiz, istab, itra, ar, koh, rnox, &
                     vchem)

use m_commonconst_lt

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_vchem')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: icm                        ! component number
LOGICAL,   INTENT(IN)                            :: isec                       ! TRUE if component=[SO2, NOx, NH3]
INTEGER*4, INTENT(IN)                            :: iopt_vchem                 ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
REAL*4,    INTENT(IN)                            :: vchemc                     ! chemical conversion rate [%/h]
REAL*4,    INTENT(IN)                            :: vchemv                     ! light dependent part of chemical conversion rate
REAL*4,    INTENT(IN)                            :: vchemnh3                   ! chemical conversion rate for NH3 -> NH4 [%/h]
type(Tvchem), INTENT(IN)                         :: vchem2                     ! structure for chemical conversion rates
REAL*4                                           :: rad                        ! global radiation [J/cm2/h]
REAL*4,    INTENT(IN)                            :: rad_W_m2                   ! global radiation [W/m2]
REAL*4,    INTENT(IN)                            :: regenk                     ! rain probability [-]
INTEGER*4, INTENT(IN)                            :: iseiz                      ! 0 = long term, 1 = year, 2 = winter, 3 = summer, 4 = month in winter, 5 = month in summer
INTEGER*4, INTENT(IN)                            :: istab                      ! index of stability class
INTEGER*4, INTENT(IN)                            :: itra                       ! index of trajectory class
REAL*4,    INTENT(IN)                            :: ar                         ! proportionality constant [ppb J-1 cm2 h] in relation [OH] = ar Qr
REAL*4,    INTENT(IN)                            :: koh                        ! reaction constant [ppb-1 h-1] for NO2 + OH -> HNO3
REAL*4,    INTENT(IN)                            :: rnox                       ! NO2/NOx ratio [-]

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: vchem                      ! chemical conversion rate [%/h]
                                                                              
! LOCAL VARIABLES
REAL*4                                           :: frac_night_hours           ! fraction of nighttime hours [-]
REAL*4                                           :: chemn                      ! chemical conversion rate for NO2+O3 -> NO3 (nigthttime) [%/h]
REAL*4                                           :: chemr                      ! chemical conversion rate for NO2 + OH -> HNO3 (daytime) [%/h]

!-------------------------------------------------------------------------------------------------------------------------------

IF (isec) THEN


   !---------------------------------------------------------------------------------------------------------
   ! Compute chemical conversion rate (%/h) for acidifying components (icm = 1,2,3)
   !---------------------------------------------------------------------------------------------------------

   IF (iopt_vchem .eq. 1) THEN
   
      !------------------------------------------------------
      ! Chemical conversion rate from EMEP maps
      !------------------------------------------------------
      vchem = vchem2%vchem

   ELSE
   
      !------------------------------------------------------
      ! Old parameterisation of chemical conversion rates
      !------------------------------------------------------
      IF (icm .EQ. 1) THEN
         
         !-----------------------------------
         ! icm = 1: SO2
         !-----------------------------------
         ! Compute vchem    : chemical conversion rate for SO2 -> SO4 (%/h)
         !
         ! ar = proportionality constant [ppb J-1 cm2 h] in relation [OH] = ar Qr, with 
         ! [OH] = OH radical concentration [ppb] , Qr = global radiation in J/cm2/h, see 
         ! Van Egmond N.D. and Kesseboom H. (1985) A numerical mesoscale model for long-term average NOx and NO2-concentration. 
         ! Atmospheric Environment 19, 587-595.
         ! Table 6.1 OPS-report: 
         ! ar(summer) = 7345 molec cm-3 W-1 m2
         ! ar(winter) = 3540 molec cm-3 W-1 m2.
         ! Cnversion factors seconds_per_hour = 3600; cm2_per_m2 = 10^4.
         !    at T = 20 C, p = 1013 mb:
         !       ppbfac = conversion factor ppb -> molec/cm3 = 2.5029e10 molec/cm3/ppb
         !       ar(summer) = 7345*cm2_per_m2/(ppbfac*seconds_per_hour) = 81.5e-8 ppb J-1 cm2 h.
         !       ar(winter) = 3540*cm2_per_m2/(ppbfac*seconds_per_hour) = 39.3e-8 ppb J-1 cm2 h.
         !       ar(year)   = average of ar_winter) and ar_wummer)      = 60.4e-8 ppb J-1 cm2 h.
         !    at T = 25 C, p = 1013 mb:
         !       ppbfac     =  2.4610e+010 molec/cm3/ppb
         !       ar(summer) =  82.9e-8 ppb J-1 cm2 h.
         !       ar(winter) =  40.0e-8 ppb J-1 cm2 h.
         !       ar(year)   =  61.4e-8 ppb J-1 cm2 h.
         !
         ! For a specific month, a cos-function over the year is used, such that ar(average) = 62e-8, ar(Feb) = 40e-8, ar(Aug) = 83e-8.
         !    Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec
         !    32.1 40.1 54.0 70.0 83.9 91.9 91.9 83.9 70.0 54.0 40.1 32.1 *1e-8
         !
         ! k_ho	SO2 + OH -> sulphate (gas phase)      3.44 x 10-12 cm3 molec-1 s-1 
         !        at T = 20 C, p = 1013 mb: ppbfac = conversion factor ppb -> molec/cm3 = 2.5029e10 molec/cm3/ppb ->
         !        k_ho = 3.44e-12*3600*2.5029e10 = 309.96 ppb-1 h-1       
         !        k_ho*[OH] = k_ho*ar*Q = 100*309.96*62e-8*Q %/h = 0.0192*Q %/h  
         ! k_he	SO2 -> sulphate (Particle phase) 1.7 x 10-6 s-1 = 0.612 %/h
         ! k_aq	SO2 -> sulphate (Aqueous phase)  4.0 x 10-5 s-1 = 14.4  %/h
         !
         ! Note: in source code 1.2*[0.016 0.5 12] = [0.0192 0.6 14.4] = [ar khe kaq]
      
         vchem = 1.2*((rad*.016) + .5 + (regenk*12.))
         ! write(*,*) 'ops_vchem, vchem: ',vchem
      
      ELSE IF (icm .EQ. 2) THEN
      
         !-----------------------------------
         ! icm = 2: NOx
         !-----------------------------------
         ! Compute frac_night_hours = fraction of nighttime hours, depending on season;
         ! NACHTZOMER and NACHTWINTER are relative occurrences (%) of nighttime hours in summer and winter,
         ! for each stability class and distance class. ("NACHT" = night, "ZOMER" = summer)
         IF ((iseiz .EQ. 3) .OR. (iseiz .EQ. 5)) THEN
            frac_night_hours = FLOAT(NACHTZOMER(istab, itra))/100.
         ELSE IF ((iseiz .EQ. 2) .OR. (iseiz .EQ. 4)) THEN
            frac_night_hours = FLOAT(NACHTWINTER(istab, itra))/100.
         ELSE
            frac_night_hours = FLOAT(NACHTWINTER(istab, itra) + NACHTZOMER(istab, itra))/200.
         ENDIF
      
         ! Compute chemn = chemical conversion rate for NO2+O3 -> NO3 (nigthttime), assuming a 2%/h conversion rate
         ! Van Egmond N.D. and Kesseboom H. (1983) Mesoscale air pollution dispersion models-II. Lagrangian PUFF model,
         ! and comparison with Eulerian GRID model. Atmospheric Environment, 17, 265-274.   
         chemn = frac_night_hours*2.
      
         ! chemr : chemical conversion rate for NO2 + OH -> HNO3 (daytime); [%/h] (factor 100 is to make percentage instead of fractions)
         !         rad : global radiation [J/cm2/h]
         !         ar  : proportionality constant [ppb J-1 cm2 h] in relation [OH] = ar Qr, with 
         !               [OH] = OH radical concentration [ppb] , Qr = global radiation [J/cm2/h] 
         !         koh : reaction constant [ppb-1 h-1] (Van Aalst en Bergsma, 1981)
         ! vchem  : total chemical conversion rate, split into daytime and nighttime part
         chemr    = 100*rad*ar*koh*rnox
      
         ! vchem  : total chemical conversion rate, split into daytime and nighttime part
         vchem = chemr + chemn
         
      ELSE IF (icm .EQ. 3) THEN
      
         !-----------------------------------
         !   icm = 3: NH3  
         !-----------------------------------
         ! Compute vchem = chemical conversion rate for NH3 -> NH4 conversion [%/h]
         vchem = vchemnh3
      ELSE
         write(*,*) 'internal programming error in ops_vchem; isec and icm < 1 or icm > 3'
         write(*,*) isec,icm
         stop
      ENDIF ! IF icm = 1,2 or 3
   ENDIF ! iopt_vchem
   
ELSE
      !-------------------------------------------------------------------------------
      ! Compute chemical conversion rate for non-acidifying components (.not. isec)
      !--------------------------------------------------------------------------------
      
      ! Chemical conversion rate is partly constant and partly radiation dependent (OPS report Eq. 7.2):
      vchem = vchemc + vchemv*rad_W_m2
ENDIF

END SUBROUTINE ops_vchem

!---------------------------------------------------------------------
SUBROUTINE ops_vchem_ratio_no2_nox(iseiz,istab,rrno2nox,rno2nox,rnox)

use m_commonconst_lt

! SUBROUTINE ARGUMENTS - INPUT
INTEGER, INTENT(IN)                            :: iseiz                      ! 0 = long term, 1 = year, 2 = winter, 3 = summer, 4 = month in winter, 5 = month in summer
INTEGER, INTENT(IN)                            :: istab                      ! index of stability class
REAL,    INTENT(IN)                            :: rno2nox                    ! season dependent component of NO2/NOx ratio
REAL,    INTENT(IN)                            :: rrno2nox                   ! space varying component in ratio NO2/NOx 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,    INTENT(OUT)                           :: rnox                       ! NO2/NOx ratio

! LOCAL VARIABLES
REAL                                           :: scno2nox                   ! stability class dependent component in NO2/NOx-ratio (only in winter)

! Compute rnox = [NO2]/[NOx] ratio.
! rnox consists of a space varying component (rrno2nox, computed in ops_par_chem), a season dependent component (rno2nox, set in ops_init)
! and a stability class dependent component (scno2nox, only in winter)
IF ((iseiz .EQ. 2) .OR. (iseiz .EQ. 4)) THEN
   scno2nox = SCWINTER(istab)
ELSE
   scno2nox = 1.
ENDIF
rnox = rrno2nox*rno2nox*scno2nox

END SUBROUTINE ops_vchem_ratio_no2_nox

END MODULE m_ops_vchem
