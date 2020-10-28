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
! USAGE              :
! DESCRIPTION        : Compute parameters which depend on stability class (friction velocity, Monin-Obukhov length, plume rise,
!                      vertical dispersion coefficient). Adjust yearly averaged emission for the current {stability, distance} class.
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP Fortran
! CALLED FUNCTIONS   :
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_stab_rek(icm, rb, temp_C, h0, z0_metreg_rcp, disx, z0_rcp, xl, radius, qtr, qrv, dv, ecvl, coef_space_heating, ibtg,                         &
                     &  uster_metreg_rcp, hbron, qww, D_stack, V_stack, Ts_stack, emis_horizontal, istab, itra, qob, xloc, regenk, ra4, z0_tra, z0_src, ol_metreg_rcp,error, &
                     &  uster_rcp, ol_rcp, uster_src, ol_src, uster_tra, ol_tra, htot, htt, onder, uh, zu, qruim, qbron,                                    &
                     &  dispg)

USE m_commonconst
USE m_commonfile
USE m_error
! USE m_ops_plumerise, only ops_plumerise
USE m_ops_plumerise
use m_ops_utils, only: is_missing

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_stab_rek')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: icm                        ! componentnummer
REAL*4,    INTENT(IN)                            :: rb                         ! 
REAL*4,    INTENT(IN)                            :: temp_C                     ! temperature at height zmet_T [C] 
REAL*4,    INTENT(IN)                            :: h0                         ! 
REAL*4,    INTENT(IN)                            :: z0_metreg_rcp              ! roughness length at receptor; interpolated from meteo regions [m]
REAL*4,    INTENT(IN)                            :: disx                       ! 
REAL*4,    INTENT(IN)                            :: z0_rcp                     ! roughness length at receptor; from z0-map [m]
REAL*4,    INTENT(IN)                            :: xl                         ! 
REAL*4,    INTENT(IN)                            :: radius                     ! 
REAL*4,    INTENT(IN)                            :: qtr                        ! 
REAL*4,    INTENT(IN)                            :: qrv                        ! 
INTEGER*4, INTENT(IN)                            :: dv                         ! 
REAL*4,    INTENT(IN)                            :: ecvl(NSTAB, NTRAJ, *)      ! 
REAL*4,    INTENT(IN)                            :: coef_space_heating         ! space heating coefficient (degree-day values in combination with a wind speed correction) [C m^1/2 / s^1/2] 
INTEGER*4, INTENT(IN)                            :: ibtg                       ! 
REAL*4,    INTENT(IN)                            :: uster_metreg_rcp           ! 
REAL*4,    INTENT(IN)                            :: hbron                      ! 
REAL*4,    INTENT(IN)                            :: qww                        ! 
REAL*4,    INTENT(IN)                            :: D_stack                    ! diameter of the stack [m]
REAL*4,    INTENT(IN)                            :: V_stack                    ! exit velocity of plume at stack tip [m/s]
REAL*4,    INTENT(IN)                            :: Ts_stack                   ! temperature of effluent from stack [K]
LOGICAL,   INTENT(IN)                            :: emis_horizontal            ! horizontal outflow of emission
INTEGER*4, INTENT(IN)                            :: istab                      ! 
INTEGER*4, INTENT(IN)                            :: itra                       ! 
REAL*4,    INTENT(IN)                            :: qob                        ! 
REAL*4,    INTENT(IN)                            :: xloc                       ! 
REAL*4,    INTENT(IN)                            :: regenk                     ! 
REAL*4,    INTENT(IN)                            :: ra4                        ! 
REAL*4,    INTENT(IN)                            :: z0_tra                     ! roughness length representative for trajectory [m]
REAL*4,    INTENT(IN)                            :: z0_src                     ! roughness length at source; from z0-map [m]

! SUBROUTINE ARGUMENTS - I/O
REAL*4,    INTENT(INOUT)                         :: ol_metreg_rcp                         ! Monin-Obukhov length
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: uster_rcp                  ! friction velocity at receptor; for z0 at receptor [m/s]
REAL*4,    INTENT(OUT)                           :: ol_rcp                     ! Monin-Obukhov length at receptor; for z0 at receptor [m/s]
REAL*4,    INTENT(OUT)                           :: uster_src                  ! friction velocity u* at source [m/s]
REAL*4,    INTENT(OUT)                           :: ol_src                     ! Monin-Obukhov length at source [m]
REAL*4,    INTENT(OUT)                           :: uster_tra                  ! friction velocity u*, trajectory averaged [m/s]
REAL*4,    INTENT(OUT)                           :: ol_tra                     ! Monin-Obukhov length, trajectory averaged  [m]
REAL*4,    INTENT(OUT)                           :: htot                       ! 
REAL*4,    INTENT(OUT)                           :: htt                        ! 
REAL*4,    INTENT(OUT)                           :: onder                      ! 
REAL*4,    INTENT(OUT)                           :: uh                         ! 
REAL*4,    INTENT(OUT)                           :: zu                         ! 
REAL*4,    INTENT(OUT)                           :: qruim                      ! 
REAL*4,    INTENT(OUT)                           :: qbron                      ! 
REAL*4,    INTENT(OUT)                           :: dispg(NSTAB)               ! 

! LOCAL VARIABLES
REAL*4                                           :: uster_metreg_from_rb_rcp   ! friction velocity at receptor from Rb(SO2); for z0 interpolated from meteo regions [m/s]
REAL*4                                           :: ol_metreg_from_rb_rcp      ! Monin-Obukhov length at receptor from Rb(SO2); for z0 interpolated from meteo regions [m/s]
REAL*4                                           :: dsx                        ! ratio disx/radius, i.e. 
!                                                                              ! (source-receptor distance)/(radius of area source)
REAL*4                                           :: sz_rcp_stab_src            ! vertical dispersion coefficient sigma_z at receptor with (z0,u*,L,uh,zu) of source site 
REAL*4                                           :: uh_rcp                     ! 
REAL*4                                           :: zu_rcp                     ! 
REAL*4                                           :: sz_rcp                     ! 
REAL*4                                           :: qobb                       ! 
REAL*4                                           :: qvk                        ! 
REAL*4                                           :: qrvv                       ! 
REAL*4                                           :: tcor                       ! 
REAL*4                                           :: rcor                       ! 
REAL*4                                           :: dncor                      ! 
REAL*4                                           :: emf                        ! 
logical                                          :: VsDs_opt                   ! read stack parameters Ds/Vs/Ts from source file

! SUBROUTINE AND FUNCTION CALLS
EXTERNAL ops_z0corr
EXTERNAL ops_plrise71
EXTERNAL ops_vertdisp
LOGICAL                                          :: ops_openlog                ! function for opening log file

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
!--------------------------------------------------------------------------------
! Determine friction velocity uster and Monin-Obukhov length ol 
!--------------------------------------------------------------------------------

! Compute friction velocity u* at the receptor, but still for the standard roughness length of the meteo region.
! Note: u* from meteo statistics is for each distance/meteo class.
! u* as function of Rb = boundary layer resistance Rb(SO2) according eq. 4.4 OPS report
! kappa = 0.4; Sc = 1.25 (SO2), Pr = 0.72 -> Rb u* = (2/kappa)*(sc/pr)^(2/3) = 7.22
! and it follows u* = 7.22/Rb.
! Rb is in the range 15 - 500 s/m (but may be zero ?); in order to avoid division by zero use Rb+1 instead of Rb.
! also impose an lower limit of 0.06 m/s for u*.

!
uster_metreg_from_rb_rcp = AMAX1(7.22/(rb + 1),0.06)

! Monin-Obukhov length ol at the receptor, but still for the standard roughness length; 

! important -> sensitive for very stable conditions.
! 0 < L <= 5 -> L = 10
! L > 5      -> L = L + 5
! 
!                        T rho_a cp (u*)^3
! (2.1) OPS report: L = -------------------
!                         g H0 kappa
!
!  rho_a : air density  = 1.292 kg/m3 (0 C), 1.247 kg/m3 (20 C), 1.204 kg/m3 (20 C), pressure = 1 atm
!  cp    : specific heat capacity = 1003.5 J/(kg K), sea level, dry, T=0 C; 1012 J/(kg/K), typical room conditions (T = 23 C)
!  kappa : von Karman constant = 0.4 [-]
!  g     : accelaration of gravity = 9.81 m/s2 
!  T     : absolute temperature [K]
!  H0    : surface heat flux [W/m2]
!
! actual values in code: rho= 1.29 kg/m3, cp = 1005 J/(kg K), kappa=0.4, g=9.8 m/s2.
! 
ol_metreg_from_rb_rcp = -uster_metreg_from_rb_rcp**3*1.29*1005*(273 + temp_C)/(0.4*9.8*h0)
IF (ol_metreg_rcp .GT. (0. + EPS_DELTA)) THEN
   IF (ol_metreg_rcp .LE. 5.) THEN                                                        ! MdH: EPS_DELTA overbodig, want deze is continue
      ol_metreg_rcp = 10.
   ELSE
      ol_metreg_rcp = ol_metreg_rcp + 5.
   ENDIF
ENDIF
!
! Correction Monin-Obukhov length at receptor
! 0 < L <= 5 -> L = 10
! L > 5      -> L = L + 5
! -7 < L < 0 -> L = -7 for unstable conditions
! 
IF (ol_metreg_from_rb_rcp .GT. (0. + EPS_DELTA)) THEN
   IF (ol_metreg_from_rb_rcp .LE. 5.) THEN                                                     ! MdH: EPS_DELTA overbodig, want deze is continue
      ol_metreg_from_rb_rcp = 10.
   ELSE 
      ol_metreg_from_rb_rcp = ol_metreg_from_rb_rcp + 5.
   ENDIF
ELSEIF (ol_metreg_from_rb_rcp .LT. (0. - EPS_DELTA)) THEN                                      ! MdH: EPS_DELTA  overbodig, want deze is continue
   IF (ol_metreg_from_rb_rcp .GT. -7.) THEN            
      ol_metreg_from_rb_rcp = -7.
   ENDIF
ENDIF
!
! Determine friction velocity (uster) and Monin-Obukhov length (ol), which are given at a standard roughness length 
! from the meteo regions, at the specific roughness length for source and receptor:

CALL ops_z0corr(z0_metreg_rcp, uster_metreg_from_rb_rcp, ol_metreg_from_rb_rcp, z0_rcp, uster_rcp, ol_rcp)
CALL ops_z0corr(z0_metreg_rcp, uster_metreg_rcp, ol_metreg_rcp, z0_src, uster_src, ol_src)
CALL ops_z0corr(z0_metreg_rcp, uster_metreg_rcp, ol_metreg_rcp, z0_tra, uster_tra, ol_tra)


!CALL ops_z0corr(z0_metreg_rcp, uster_metreg_from_rb_rcp, ol_metreg_from_rb_rcp, z0_rcp, uster_rcp, ol_rcp)
!CALL ops_z0corr(z0_metreg_src, uster_metreg_rcp, ol_metreg_rcp, z0_src, uster_src, ol_src)
!CALL ops_z0corr(z0_metreg_tra, uster_metreg_rcp, ol_metreg_rcp, z0_tra, uster_tra, ol_tra)

!--------------------------------------------------------------------------
! Compute plume rise and inverse penetration according to Briggs (1971)
!--------------------------------------------------------------------------
!CALL ops_plrise71(z0_src, xl, ol_src, uster_src, hbron, qww, xloc, htt, onder)
VsDs_opt = .not. is_missing(V_stack)
call ops_plumerise(z0_src, hbron, uster_src, ol_src, qww, VsDs_opt, D_stack, V_stack, Ts_stack, emis_horizontal, temp_C, xl, xloc, htt, onder, error)
! write(*,'(a,4(1x,e12.5))') 'after call ops_plumerise: ',hbron,htt,htt-hbron,onder
htot = htt
if (error%haserror) goto 9999

!------------------------------------------------
! Compute vertical dispersion coefficient sigma_z 
!------------------------------------------------

! Skip computation of vertical dispersion if point source AND receptor very near point source (disx = disx, disx <= 1)
! in other cases (area source, point source and receptor further away) compute vertical dispersion.
dsx = AMAX1(disx, radius)
IF (dsx .GT. (1. + EPS_DELTA)) THEN

   ! Compute vertical dispersion coefficient at receptor with (z0,u*,L,uh,zu) of source site
   CALL ops_vertdisp(z0_src, xl, ol_src, uster_src, htot, dsx, uh, zu, sz_rcp_stab_src, error)
   if (error%debug) write(*,'(3a,2(1x,i6),99(1x,e12.5))') trim(ROUTINENAAM),',A,', &
      ' ircp,istab,z0_src, xl, ol_src, uster_src, htot, dsx, uh, zu, sz_rcp_stab_src:', &
        -999,istab,z0_src, xl, ol_src, uster_src, htot, dsx, uh, zu, sz_rcp_stab_src

   ! Compute vertical dispersion coefficient at receptor with (z0,u*,L,uh,zu) of receptor site
   CALL ops_vertdisp(z0_rcp, xl, ol_rcp, uster_rcp, htot, dsx, uh_rcp, zu_rcp, sz_rcp, error)
   if (error%debug) write(*,'(3a,2(1x,i6),99(1x,e12.5))') trim(ROUTINENAAM),',B,', &
      ' ircp,istab,z0_rcp, xl, ol_rcp, uster_rcp, htot, dsx, uh_rcp, zu_rcp, sz_rcp:', &
        -999,istab,z0_rcp, xl, ol_rcp, uster_rcp, htot, dsx, uh_rcp, zu_rcp, sz_rcp

!
!  Limit sigma_z at source, such that sigma_z(source) < sigma_z(receptor)
!
   IF (sz_rcp_stab_src .GT. (sz_rcp + EPS_DELTA)) THEN
      sz_rcp = sz_rcp_stab_src
   ENDIF
!
!  Compute dispersion coefficient dispg of average between sigma_z at source and receptor;
!  sigma_z = dispg*disx**disph <=> dispg = sigma_z/(disx**disph),  3.16 new! OPS report
!  Since in the rest of the code the old formula sigma_z = dispg*disx**disph is still used,
!  we need dispg and disph and we do not use sz_rcp_stab_src and sz_rcp hereafter. 
   dispg(istab) = (sz_rcp_stab_src + sz_rcp)*0.5/(dsx**DISPH(istab))
   if (error%debug) write(*,'(3a,2(1x,i6),99(1x,e12.5))') trim(ROUTINENAAM),',C,', ' ircp,istab,dispg(istab):', -999,istab,dispg(istab)
      
   ! Check limits 0 <= dispg <= 50; if outside limits, generate warning:
   IF ((dispg(istab) .LT. (0. - EPS_DELTA)) .OR. (dispg(istab) .GT. (50. + EPS_DELTA))) THEN
      IF (.NOT. ops_openlog(error)) GOTO 9999
      WRITE (fu_log,'("WARNING: OPS has detected a value", " outside its limits in routine ", A)')                              &
          &  ROUTINENAAM(:LEN_TRIM(ROUTINENAAM))
      WRITE (fu_log, '("istab,uster_metreg_from_rb_rcp,ol_metreg_rcp, ol_rcp, sz_rcp_stab_src,", "sz_rcp: ", i4, 4f8.2, f10.2)') istab,   &
           &  uster_metreg_from_rb_rcp, ol_metreg_rcp, ol_rcp, sz_rcp_stab_src, sz_rcp
   ENDIF
ENDIF

!-----------------------------------------------------------------------------------
! Adjust yearly averaged emission for the current {stability, distance} class.
!-----------------------------------------------------------------------------------
!
! Compute space ("ruimte" = space) heating emission as function of the temperature.
! space_heating_coefficent = (19 - T24)*sqrt(u10/3.2), for T24 < 12 C;  5.1 OPS report
! T24 = daily average outdoor temperature (C).
! u10 = wind speed at 10 m (m/s)
! 0.1042 = 1/mean(space_heating_coefficient), longterm average, is used to normalise the space_heating_coefficent.
qruim = .1042*coef_space_heating*qrv 
!
! Choose type of diurnal variation of emission, depending on ibtg 
! and current {stability,distance} class and adjust source strengths.
! qob   -> qobb = source strength of industrial source
! qruim -> qrvv = source strength of space heating source (rv << "ruimteverwarming" = space heating)
! qtr   -> qvk  = source strenght traffic source (vk << "verkeer" = traffic)
!
IF (ibtg .GE. 0) THEN

   ! ibtg > 0 -> pre-defined diurnal variation for industrial sources, space heating and traffic  
   ! ibtg = 0 -> homogeneous industrial sources, pre-defined diurnal variation for space heating and traffic
   qrvv = qruim*ecvl(istab, itra, 2)
   qvk  = qtr*ecvl(istab, itra, 3)
   IF (ibtg .EQ. 0) THEN
     qobb = qob
   ELSE
     qobb = qob*ecvl(istab, itra, ibtg)
   ENDIF
ELSE  
   ! ibtg < 0 -> user-specified diurnal variation
   qrvv = qruim*ecvl(istab, itra, (dv + 2))
   qvk  = qtr*ecvl(istab, itra, (dv + 3))
   qobb = qob*ecvl(istab, itra, (dv + ABS(ibtg)))
ENDIF
!
! NH3 and NOx emissions from animal housing, application and pasture depend on meteo; 
! split between correction for emissions from animal housing and other (= application and pasture)
!
IF (icm .EQ. 2 .OR. icm .EQ. 3) THEN
      
  IF  (ibtg .EQ. 4) THEN
    ! Emissions from animal housing; TNO: (Bas Nijenhuis, 990207)
    ! temperature correction for NH3 emissions from animal housing systems; OPS report 6.33.
    ! Tavg = 10 C
    ! Temperature correction tcor = 1 + (T - Tavg)/f = 1 + T/f - 10/f = (1-10/f) + T/f = (f-10)/f + T/f = (T + f-10)/f; 
    ! Here f = 34, corresponding with a factor 1/34 = 0.0294 (0.04 in 6.33 OPS report). FS

    tcor=amax1((temp_C+24)/34, 0.2)
                                                 
!   Influence of day/night rithm of animals on emissions; half the industrial emission variation

    dncor=1.-(1.-ecvl(istab,itra,1))/2                                        ! day
    qobb=qob*tcor*dncor                                                       ! 990227
   
  ELSEIF (ibtg .EQ. 5) THEN
    ! application, fertiliser and other; 6.32 OPS report
  
    ! Corrections are based on DEPASS model
    !                                                                      
    rcor=(1.069-regenk)**2                                                    ! 980922
    rcor=amax1(rcor,0.5)
    rcor=amin1(rcor,1.5)
  
    emf=0.0000155*((100./(ra4+rb))**0.8*(temp_C+23)**2.3)**1.25               ! 981209 
   
    qobb=qob*rcor*emf                                                         ! 980922; corr 990227
  ELSE
    CONTINUE
  ENDIF
ENDIF
!
! Total source strength qbron in g/s:
!
qbron = qobb + qrvv + qvk

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)
END SUBROUTINE ops_stab_rek
