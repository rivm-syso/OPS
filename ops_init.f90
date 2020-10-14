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
!                       copyright by
!   National Institute of Public Health and Environment
!           Laboratory for Air Research (RIVM/LLO)
!                      The Netherlands
!   No part of this software may be used, copied or distributed without permission of RIVM/LLO (2002)
!
! SUBROUTINE
!
! NAME                  : %M%
! SCCS (SOURCE)         : %P%
! RELEASE - LEVEL       : %R% - %L%
! BRANCH -SEQUENCE      : %B% - %S%
! DATE - TIME           : %E% - %U%
! WHAT                  : %W%:%E%
! AUTHOR                : OPS-support 
! FIRM/INSTITUTE        : RIVM/LLO
! LANGUAGE              : FORTRAN-77/90
! DESCRIPTION           : Initialisation of variables based on data from the control file and on meteo statistics.
! EXIT CODES            :
! FILES AND OTHER       :
!   I/O DEVICES
! SYSTEM DEPENDENCIES   : HP-Fortran
! CALLED FUNCTIONS      : ops_masknew, amcgeo
! UPDATE HISTORY        :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_init (gasv, idep, building_present1, kdeppar, knatdeppar, ddeppar, wdeppar, amol2, ideh, icm, isec, nsubsec, iseiz, mb, astat, dverl,       &
                  &  usdverl, dv, usdv, namco, amol1, dg, irev, vchemc, vchemv, emtrend, rc, coneh, amol21, depeh, namsec,     &
                  &  namse3, ugmoldep, scavcoef, rcno, rhno2, rchno3, routsec, routpri, conc_cf, koh, croutpri, somcsec,       &
                  &  ar, rno2nox, ecvl, nam_subsec, buildingEffect, error)
                  
USE m_commonconst
USE m_error
USE m_ops_building

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_init')

! SUBROUTINE ARGUMENTS - INPUT
LOGICAL,   INTENT(IN)                            :: gasv                        
LOGICAL,   INTENT(IN)                            :: idep   
LOGICAL,   INTENT(IN)                            :: building_present1           ! at least one building is present in the source file   
INTEGER*4, INTENT(IN)                            :: kdeppar                     
REAL*4,    INTENT(IN)                            :: ddeppar                     
REAL*4,    INTENT(IN)                            :: wdeppar                     
INTEGER*4, INTENT(IN)                            :: ideh                        
INTEGER*4, INTENT(IN)                            :: icm                         
LOGICAL,   INTENT(IN)                            :: isec 
INTEGER*4, INTENT(IN)                            :: nsubsec                      ! number of sub-secondary species                       
INTEGER*4, INTENT(IN)                            :: iseiz                       
INTEGER*4, INTENT(IN)                            :: mb                          
REAL*4,    INTENT(IN)                            :: astat(NTRAJ, NCOMP, NSTAB, NSEK) 
REAL*4,    INTENT(IN)                            :: dverl(NHRBLOCKS,MAXDISTR)    
REAL*4,    INTENT(IN)                            :: usdverl(NHRBLOCKS,MAXDISTR)  
INTEGER*4, INTENT(IN)                            :: dv                          
INTEGER*4, INTENT(IN)                            :: usdv                        

! SUBROUTINE ARGUMENTS - I/O
INTEGER*4, INTENT(INOUT)                         :: knatdeppar                  
REAL*4,    INTENT(INOUT)                         :: amol2                       
CHARACTER*(*), INTENT(INOUT)                     :: namco                       
REAL*4,    INTENT(INOUT)                         :: amol1                       
REAL*4,    INTENT(INOUT)                         :: dg                          
LOGICAL,   INTENT(INOUT)                         :: irev                        
REAL*4,    INTENT(INOUT)                         :: vchemc                      
REAL*4,    INTENT(INOUT)                         :: vchemv                      
REAL*4,    INTENT(INOUT)                         :: emtrend                     

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: rc                          
CHARACTER*(*), INTENT(OUT)                       :: coneh                       
REAL*4,    INTENT(OUT)                           :: amol21                      
CHARACTER*(*), INTENT(OUT)                       :: depeh                       
CHARACTER*(*), INTENT(OUT)                       :: namsec                      
CHARACTER*(*), INTENT(OUT)                       :: namse3                      
REAL*4,    INTENT(OUT)                           :: ugmoldep                    
REAL*4,    INTENT(OUT)                           :: scavcoef                    
REAL*4,    INTENT(OUT)                           :: rcno                       
REAL*4,    INTENT(OUT)                           :: rhno2                      
REAL*4,    INTENT(OUT)                           :: rchno3                      
REAL*4,    INTENT(OUT)                           :: routsec                    ! in-cloud scavenging ratio for secondary component
                                                                               ! (rout << rain-out = in-cloud) [-] 
REAL*4,    INTENT(OUT)                           :: routpri                    ! in-cloud scavenging ratio for primary component
                                                                               ! (rout << rain-out = in-cloud) [-]
REAL*4,    INTENT(OUT)                           :: conc_cf
REAL*4,    INTENT(OUT)                           :: koh                         
REAL*4,    INTENT(OUT)                           :: croutpri                   ! constant (initial) in-cloud scavenging ratio [-] for primary component                   
REAL*4,    INTENT(OUT)                           :: somcsec                     
REAL*4,    INTENT(OUT)                           :: ar                          
REAL*4,    INTENT(OUT)                           :: rno2nox                     
REAL*4,    INTENT(OUT)                           :: ecvl(NSTAB, NTRAJ, *)       
CHARACTER*(*), INTENT(OUT)                       :: nam_subsec(nsubsec) 
type(TbuildingEffect), INTENT(OUT)               :: buildingEffect             ! structure with building effect tables
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record
                

! LOCAL VARIABLES
INTEGER*4                                        :: i                           
INTEGER*4                                        :: j                           
INTEGER*4                                        :: ndv                         
INTEGER*4                                        :: itraj                       
INTEGER*4                                        :: istab                       
INTEGER*4                                        :: iu                          
REAL*4                                           :: vgmax                       
REAL*4                                           :: som   
CHARACTER*512                                    :: line                      

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Limit emission trend correction factor; if emtrend = 0 -> emtrend = 1 
!
IF (ABS(emtrend) .LE. EPS_DELTA) emtrend = 1.
!
! Parameterisation substance properties
!

! Molecular weight [g/mol] for secondary component equals amol1, by default 
amol2 = amol1

! Default concentration correction factor
conc_cf = 1

! Set maximal deposition velocity [m/s] (only for kdeppar = 1)

vgmax=0.034

! Set default ratio amol2/amol1
amol21 = 1.

IF (gasv) THEN                                                              ! if gas
  IF (idep) THEN                                                               ! if deposition has to be computed

    IF (.NOT.isec) THEN

      ! Secondary components not present (so not SO2, NOx or NH3).
      ! Compute surface resistance Rc [s/m], scavenging rate (scavcoef [%/h]) or scavenging ratio W (routpri [-]), 
      ! diffusion coefficient in air (dg [cm^2/s]) (according to Durham et al, 1981: Atm Env. 15, 1059-1068)
      ! and logical irev (reversible uptake of gas in droplets is possible)
      IF (kdeppar .EQ. 1) rc = 1./ddeppar - 1./SQRT(vgmax*ddeppar)             ! if ddeppar = vgmax -> Rc = 0; 
                                                                               ! ddeppar < vgmax is not possible (check in ops_read_ctr)
      IF (kdeppar .EQ. 2) rc = ddeppar

       ! Depending on knatdeppar, set scavenging rate (scavcoef [%/h]) or scavenging ratio (routpri [-]) (see ops_par_nat)
       ! note: amol2 = amol1
       IF (knatdeppar .EQ. 1) THEN
        scavcoef = wdeppar
        dg       = SQRT(1./amol2) 
        irev     = .FALSE.
      ELSEIF (knatdeppar .EQ. 2) THEN
        routpri = wdeppar
        dg      = SQRT(1./amol2)
        irev    = .FALSE.
      ELSE
        routpri=wdeppar
      ENDIF
      
    ELSE

      ! secondary components present [SO2, NO2, NH3] -> knatdeppar = 3.
      ! scavenging rate (scavcoef [%/h]) can be set to 0, because it is only used in case knatdeppar = 1.
      ! Set following parameters:
      ! amol2   :  molecular weight [g/mol] of secondary component
      ! croutpri: constant (initial) in-cloud scavenging ratio [-] for primary component
      ! routsec : in-cloud scavenging ratio for secondary component
      !           (rout << rain-out = in-cloud) [-])
      ! conc_cf : concentration correction factor for output.
      ! Section 6.3 OPS report FS
 
      knatdeppar = 3
      scavcoef = 0

      ! icm = 1: SO2 (secondary component SO4)
      IF (icm .EQ. 1) THEN
         amol2 = 96. 
         croutpri = 100000.
         routsec  = 2.0e6
         conc_cf  = 1.

      ! icm = 2: NOx (secondary component NO3)
      ELSE IF (icm .EQ. 2) THEN
         amol2    = 62.
         croutpri = 20000.
         routsec  = 1.4e7

         ! Set parameters specific for NOx 
         ! rhno2  : ratio [HNO2]/[NOx] based on measurements Speuld, Slanina et al 1990, but they report 4% (p. 66 OPS report) FS
         ! koh    : second order reaction rate constant of reaction NO2 + OH -> HNO3 [cm3/(molec s)] 
         !          Baulch et al 1982 (OPS report Table 6.2 FS): kOH = 1.035e-11 cm3/(molec s) = 1000.9 ppb-1 h-1, at T = 0 C
         !                                                                                     =  932.6 ppb-1 h-1, at T = 20 C
         !                                                                                     =  917.0 ppb-1 h-1, at T = 25 C
         !          Baulch D.L., Cox, R.A. Crutzen P.J., Hampson R.F. Jr., Kerr, F.A. Troe, J. and Watson R.P. (1982) 
         !          Evaluated kinetic and photochemical data for atmospheric chemistry: J. Phys. Chem. Ref. Data 11 (Suppl. 1), 327-496.
         ! conc_cf: correction factor (8%) for NOx (to account for HNO2 and PAN contributions to NO2)
         ! rcno   : surface resistance NO (set at 'high' value) [s/m] 
         ! rchno3 : surface resistance HNO3 (set at 'low' value) [s/m]
         
         rhno2   = 0.03
         koh     = 1020.*0.9 ! = 918 ppb-1 h-1
         rcno    = 2000
         rchno3  = 10
         conc_cf = 1./1.08

      !
      ! icm = 3: NH3 (secondary component NH4)
      !
      ELSE IF (icm .EQ. 3) THEN
         amol2    = 18.
         croutpri = 1.4e6
         routsec  = 1.4e7
         conc_cf  = 1.
      ENDIF
    ENDIF

    ! Set ratio of molecular weights of primary and secondary component
    amol21=amol2/amol1
  ENDIF

!
! particles
!
ELSE
  vchemc=0
  vchemv=0
ENDIF
!
! Component names (see m_commonconst for definition of CNAME)
!
IF (isec) THEN
  namco      = CNAME(icm,1)
  namsec     = CNAME(icm,2)
  nam_subsec = CNAME_SUBSEC
  namse3     = CNAME(icm,4)
ELSE
  namsec = namco
  namse3 = namco
ENDIF
!
! Units for concentration and deposition and conversion factors
! (see m_commonconst for definition of UNITS and DEPUNITS).
! Note: 
!    1/(number of seconds in an hour) = 1/3600 = 0.278e-3
!    number of hours in a year = 8760
!    amol2 = molecular weight secondary component in g/mol 
!
IF (icm .EQ. 2) THEN                                                           ! NOx
  coneh = UNITS(2)                                                             ! ug/m3 NO2
ELSE
  coneh = UNITS(1)                                                             ! default ug/m3
ENDIF

IF (idep) THEN
  depeh = DEPUNITS(ideh)
  IF (ideh .EQ. 1) THEN       ! conversion ug/m2/h -> mmol/m2/s:
    ugmoldep = .278e-6/amol2
  ELSE IF (ideh .EQ. 2) THEN  ! conversion ug/m2/h -> g/m2/s:
    ugmoldep = .278e-9
  ELSE IF (ideh .EQ. 3) THEN  ! conversion ug/m2/h -> mol/ha/j:
    ugmoldep = 87.6/amol2
  ELSE IF (ideh .EQ. 4) THEN  ! conversion ug/m2/h -> kg/ha/j:
    ugmoldep = 87.6/1000.
  ELSE IF (ideh .EQ. 5) THEN  ! conversion ug/m2/h -> mmol/m2/j:
    ugmoldep = 8.76/amol2
  ELSE                        ! conversion ug/m2/h -> g/m2/j:
    ugmoldep = 8.76/1000.
  ENDIF
ELSE
   ugmoldep = 1.0
ENDIF

IF (icm .EQ. 2) THEN
!
!  Set ar and rno2nox.
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
!  rno2nox = season dependent [NO2]/[NOx] ratio, see Table  6.3 OPS report for stability class S2: 
!            rno2nox(summer) = 0.78, rno2nox(winter) = 0.58; rno2nox(year) = average of summer and winter value = 0.68.
!            For a specific month, 2 cos-functions are used, such that rno2nox(Feb) = 0.57, rno2nox(Aug) = 0.78:
!            Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec
!            0.53 0.57 0.62 0.70 0.78 0.82 0.82 0.78 0.70 0.62 0.57 0.53
!
!  iseiz: 0=long term; 1=year; 2=winter; 3=summer; 4=month in winter; 5=month in summer)
!
   IF (iseiz .LE. 1) THEN
     ar      = 62.e-8
     rno2nox = .65
   ELSE IF (iseiz .EQ. 2) THEN
     ar      = 40.e-8
     rno2nox = .58
   ELSE IF (iseiz .EQ. 3) THEN
     ar      = 83.e-8
     rno2nox = .78
   ELSE IF (iseiz .EQ. 4) THEN
     ar      = 62.e-8 - COS((FLOAT(mb) - .5)/NMONTH*2.*PI)*31.e-8
     rno2nox = .65 - COS((FLOAT(mb) - .5)/NMONTH*2.*PI)*.12
   ELSE
     ar      = 62.e-8 - COS((FLOAT(mb) - .5)/NMONTH*2.*PI)*31.e-8
     rno2nox = .65 - COS((FLOAT(mb) - .5)/NMONTH*2.*PI)*.18
   ENDIF
ENDIF
!
! Compute percentage of emission per stability class for traffic related sources and for low sources.
! User specified and standard diurnal emission variation distributions are averaged into one array (ecvl).
!
ndv = dv + usdv

! Loop over distance classes and stability classes:
DO itraj = 1, NTRAJ
   DO istab = 1, NSTAB
!
!     Initialise sum and ecvl.
!
      som = 0.
      ecvl(istab, itraj, :ndv) = 0.

      ! Loop over hour blocks and compute, for each stability/distance class, the average over hours: 
      ! ecvl = weighed average of diurnal emission variation; 
      ! weighing factors are astat(17): distribution of stability classes over day, source oriented [-].
      
      ! Factor 1.0e-2 is to convert from dverl as percentage to ecvl as fraction.
      ! Note: astat() has been filled with meteo parameters of most central region in NL (region 5); see ops_statfil

      DO iu = 1, NHRBLOCKS
         som = som + astat(itraj, 17, istab, iu)
!
!        Add contribution of standard diurnal emission variation.
!
         DO i = 1, dv
            ecvl(istab, itraj, i) = ecvl(istab, itraj, i) + dverl(iu, i) * astat(itraj, 17, istab, iu)*1.e-2
         ENDDO
!
!        Add contribution of user specified diurnal emission variation. If there is no user specified emission variation 
!       (usdvnam has string length 0), then ndv = dv and we have an empty loop.
!
         DO i = dv+1, ndv
            ecvl(istab, itraj, i) = ecvl(istab, itraj, i) + usdverl(iu, (i - dv)) * astat(itraj, 17, istab, iu)*1.e-2
         ENDDO
      ENDDO

      ! Compute average diurnal emission variation:
      IF (som .GT. (0. + EPS_DELTA)) THEN
         ecvl(istab, itraj, :ndv) = ecvl(istab, itraj, :ndv)/som
      ENDIF
   ENDDO
ENDDO

! Read building effect tables:   
if (building_present1) then
   call ops_building_read_tables(buildingEffect,error)
   !write(*,*) 'ops_init/classdefinitionArray: ',buildingEffect%classdefinitionArray
   !write(*,*) 'ops_init/buildingFactArray:',buildingEffect%buildingFactArray
   if (error%haserror) goto 9999
endif

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error) 

END SUBROUTINE ops_init
