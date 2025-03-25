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
! USAGE              :
! DESCRIPTION        : Compute parameters which depend on stability class (friction velocity, Monin-Obukhov length, plume rise,
!                      vertical dispersion coefficient). Adjust yearly averaged emission for the current {stability, distance} class.
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_stab_rek

implicit none

contains

SUBROUTINE ops_stab_rek(varin, icm, rb_ms, temp_C, h0, z0_metreg_rcp, disxx, disx, z0_rcp, xl, radius, qtr, qrv, dv, ecvl, coef_space_heating, ibtg,                         &
                     &  uster_metreg_rcp, hbron, qww, D_stack, V_stack, Ts_stack, emis_horizontal, ircp, istab, itra, qob, xloc, regenk, ra_ms_4, z0_tra, z0_src, ol_metreg_rcp, error, &
                     &  uster_rcp, ol_rcp, uster_src, ol_src, uster_tra, ol_tra, htot, htt, onder, uh, zu, qruim, qbron,                                    &
                     &  dispg)

use m_ops_varin, only: TVarin
use m_commonconst_lt
use m_commonfile
use m_error
use m_ops_plumerise
use m_ops_utils, only: is_missing
use m_ops_logfile
use m_ops_z0corr
use m_ops_vertdisp
use m_ops_meteo, only: ops_meteo_cutoff_obukhov, ops_meteo_cutoff_uster

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_stab_rek')
! HACK:
! sensitivity analyses. For its initial use, this scale is limited to 20% in
! either direction. Ideally, this limit should be imposed by the source that
! sets the value in the varin file. Currently this constant is used to perform
! inverse sampling to transform a random variate from U(0, 1) to U(0.8, 1.2).
real, parameter :: DIURNAL_SCALE_RANGE = 0.2  ! Maximum deviation of the slope of the temperature correction factor.
real, parameter :: Tavg = 10.0  ! Point around which to scale the temperature.

! SUBROUTINE ARGUMENTS - INPUT
TYPE(Tvarin), INTENT(IN)                         :: varin
INTEGER,   INTENT(IN)                            :: icm                        ! componentnummer
REAL,      INTENT(IN)                            :: rb_ms                      ! boundary layer resistance SO2 from meteo statistics [s/m] 
REAL,      INTENT(IN)                            :: temp_C                     ! temperature at height zmet_T [C] 
REAL,      INTENT(IN)                            :: h0                         ! sensible heat flux H0 [W/m2]
REAL,      INTENT(IN)                            :: z0_metreg_rcp              ! roughness length at receptor; interpolated from meteo regions [m]
REAL,      INTENT(IN)                            :: disxx                      ! effective travel distance between source and receptor [m] 
REAL,      INTENT(IN)                            :: disx                       ! linear distance between source and receptor [m] (here only used for debug write statement)
REAL,      INTENT(IN)                            :: z0_rcp                     ! roughness length at receptor; from z0-map [m]
REAL,      INTENT(IN)                            :: xl                         ! maximal mixing height over transport distance [m] (extrapolated when x > 1000km, largest distance category in meteo statistics)
REAL,      INTENT(IN)                            :: radius                     ! 
REAL,      INTENT(IN)                            :: qtr                        ! 
REAL,      INTENT(IN)                            :: qrv                        ! 
INTEGER,   INTENT(IN)                            :: dv                         ! 
REAL,      INTENT(IN)                            :: ecvl(:, :, :)              ! ecvl(NSTAB, NTRAJ, *) we need only ecvl(:,:,:max(3,3+dv,ibtg+dv))
REAL,      INTENT(IN)                            :: coef_space_heating         ! space heating coefficient (degree-day values in combination with a wind speed correction) [C m^1/2 / s^1/2] 
INTEGER,   INTENT(IN)                            :: ibtg                       ! 
REAL,      INTENT(IN)                            :: uster_metreg_rcp           ! friction velocity u* [m/s]
REAL,      INTENT(IN)                            :: hbron                      ! emission height at source (stack height), without plume rise [m]
REAL,      INTENT(IN)                            :: qww                        ! 
REAL,      INTENT(IN)                            :: D_stack                    ! diameter of the stack [m]
REAL,      INTENT(IN)                            :: V_stack                    ! exit velocity of plume at stack tip [m/s]
REAL,      INTENT(IN)                            :: Ts_stack                   ! temperature of effluent from stack [K]
LOGICAL,   INTENT(IN)                            :: emis_horizontal            ! horizontal outflow of emission
INTEGER,   INTENT(IN)                            :: ircp                       ! index of receptorpoint (here only used in debug write statement)
INTEGER,   INTENT(IN)                            :: istab                      ! 
INTEGER,   INTENT(IN)                            :: itra                       ! 
REAL,      INTENT(IN)                            :: qob                        ! 
REAL,      INTENT(IN)                            :: xloc                       ! local mixing height (near source) [m]
REAL,      INTENT(IN)                            :: regenk                     ! rain probability [-] 
REAL,      INTENT(IN)                            :: ra_ms_4                    ! aerodynamic resistance at 4 m from meteo statistics [s/m] 
REAL,      INTENT(IN)                            :: z0_tra                     ! roughness length representative for trajectory [m]
REAL,      INTENT(IN)                            :: z0_src                     ! roughness length at source; from z0-map [m]

! SUBROUTINE ARGUMENTS - I/O
REAL,      INTENT(INOUT)                         :: ol_metreg_rcp              ! Monin-Obukhov length [m]
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: uster_rcp                  ! friction velocity at receptor; for z0 at receptor [m/s]
REAL,      INTENT(OUT)                           :: ol_rcp                     ! Monin-Obukhov length at receptor; for z0 at receptor [m/s]
REAL,      INTENT(OUT)                           :: uster_src                  ! friction velocity u* at source [m/s]
REAL,      INTENT(OUT)                           :: ol_src                     ! Monin-Obukhov length at source [m]
REAL,      INTENT(OUT)                           :: uster_tra                  ! friction velocity u*, trajectory averaged [m/s]
REAL,      INTENT(OUT)                           :: ol_tra                     ! Monin-Obukhov length, trajectory averaged  [m]
REAL,      INTENT(OUT)                           :: htot                       ! plume height at receptor, including plume descent due to heavy particles [m]
REAL,      INTENT(OUT)                           :: htt                        ! plume height at source, including plume rise [m]
REAL,      INTENT(OUT)                           :: onder                      ! fraction of emission below mixing height [-]
REAL,      INTENT(OUT)                           :: uh                         ! windspeed (m/s) at receptor at height zu using z0, u* and L of source site
REAL,      INTENT(OUT)                           :: zu                         ! representative plume height (m) at receptor using z0, u* and L of source site, taking reflection into account
REAL,      INTENT(OUT)                           :: qruim                      ! space heating emission as function of the temperature before diurnal variation correction 
REAL,      INTENT(OUT)                           :: qbron                      ! total source strength qbron (g/s) (qobb + qrvv + qvk)
REAL,      INTENT(INOUT)                         :: dispg                      ! coefficient for vertical dispersion coefficient sigma_z; sigma_z = dispg*x^disp [-]
                                                                               ! dispg is INOUT because dispg remains unchanged when radius==0 .and. disxx==0.

! LOCAL VARIABLES
REAL                                             :: uster_metreg_from_rb_rcp   ! friction velocity at receptor from Rb(SO2); for z0 interpolated from meteo regions [m/s]
REAL                                             :: ol_metreg_from_rb_rcp      ! Monin-Obukhov length at receptor from Rb(SO2); for z0 interpolated from meteo regions [m/s]
REAL                                             :: dsx                        ! ratio disxx/radius, i.e. (source-receptor distance)/(radius of area source).
REAL                                             :: sz_rcp_stab_src            ! vertical dispersion coefficient sigma_z at receptor with (z0,u*,L,uh,zu) of source site 
REAL                                             :: uh_rcp                     ! windspeed (m/s) at receptor at height zu using z0, u* and L of receptor site
REAL                                             :: zu_rcp                     ! representative plume height (m) at receptor using z0, u* and L of receptor site
REAL                                             :: sz_rcp                     ! vertical dispersion coefficient at receptor using z0, u* and L of receptor site
REAL                                             :: qobb                       ! 
REAL                                             :: qvk                        ! 
REAL                                             :: qrvv                       ! 
REAL                                             :: tcor                       ! 
REAL                                             :: rcor                       ! 
REAL                                             :: dncor                      ! 
REAL                                             :: emf                        ! 
CHARACTER*21                                     :: debugnam                   ! name for debug write statement since ops_vertdisp is called 3 times in various circumstances: either at_areasource, at_rcp_with_meteo_src, at_rcp_with_meteo_rcp 
real :: temp_C_rescaled  ! Class-conditional temperature rescaled based on diurnal scaling index.
real :: temp_scale  ! Rescale diurnal scaling index applied to temperature.

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

uster_metreg_from_rb_rcp = AMAX1(7.22/(rb_ms + 1),0.06) 
! call ops_meteo_cutoff_uster(uster_cutoff_iopt, uster_min, ol_old, ol, uster)  

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

! limit L, u* such that very small values do not occurr:
call ops_meteo_cutoff_obukhov(varin%varin_meteo%ol_cutoff_iopt1, varin%varin_meteo%ol_add_stable1, &
     varin%varin_meteo%ol_max_unstable0, varin%varin_meteo%ol_min_stable1, varin%varin_meteo%ol_z0_ratio_cutoff1, &
     varin%varin_meteo%ol_z0_ratio_cutoff2, z0_metreg_rcp, ol_metreg_rcp)

call ops_meteo_cutoff_obukhov(varin%varin_meteo%ol_cutoff_iopt1, varin%varin_meteo%ol_add_stable1, &
     varin%varin_meteo%ol_max_unstable2, varin%varin_meteo%ol_min_stable2, varin%varin_meteo%ol_z0_ratio_cutoff1, &
     varin%varin_meteo%ol_z0_ratio_cutoff2, z0_metreg_rcp, ol_metreg_from_rb_rcp)

! Determine friction velocity (uster) and Monin-Obukhov length (ol), which are given at a standard roughness length 
! from the meteo regions, at the specific roughness length for source and receptor:

CALL ops_z0corr(varin%varin_meteo, z0_metreg_rcp, uster_metreg_from_rb_rcp, ol_metreg_from_rb_rcp, z0_rcp, uster_rcp, ol_rcp,error)
CALL ops_z0corr(varin%varin_meteo, z0_metreg_rcp, uster_metreg_rcp, ol_metreg_rcp, z0_src, uster_src, ol_src,error)
CALL ops_z0corr(varin%varin_meteo, z0_metreg_rcp, uster_metreg_rcp, ol_metreg_rcp, z0_tra, uster_tra, ol_tra,error)

!CALL ops_z0corr(varin_meteo, z0_metreg_rcp, uster_metreg_from_rb_rcp, ol_metreg_from_rb_rcp, z0_rcp, uster_rcp, ol_rcp,error)
!CALL ops_z0corr(varin_meteo, z0_metreg_src, uster_metreg_rcp, ol_metreg_rcp, z0_src, uster_src, ol_src,error)
!CALL ops_z0corr(varin_meteo, z0_metreg_tra, uster_metreg_rcp, ol_metreg_rcp, z0_tra, uster_tra, ol_tra,error)

if (error%debug) then
   write(*,'(3a,2(1x,i6,";"),99(1x,e12.5,";"))') trim(ROUTINENAAM),',A1; ', &
      'ircp; istab; disx; disxx; uster_metreg_rcp; uster_rcp; uster_src; uster_tra; ',    &
       ircp, istab, disx, disxx, uster_metreg_rcp, uster_rcp, uster_src, uster_tra
   write(*,'(3a,2(1x,i6,";"),99(1x,e12.5,";"))') trim(ROUTINENAAM),',B1; ', &
      'ircp; istab; disx; disxx; ol_metreg_rcp; ol_rcp; ol_src; ol_tra; ',    &
       ircp, istab, disx, disxx, ol_metreg_rcp, ol_rcp, ol_src, ol_tra
endif

if (error%haserror) goto 9999

!--------------------------------------------------------------------------
! Compute plume rise and inversion penetration according to Briggs (1971)
!--------------------------------------------------------------------------
call ops_plumerise( &
   z0_src, hbron, uster_src, ol_src, qww, D_stack, V_stack, Ts_stack, emis_horizontal, &
   temp_C, xl, xloc, varin%varin_meteo, varin%varin_unc, htt, onder, error &
)
htot = htt
if (error%debug) then
   write(*,'(a,2(1x,i6,";"),99(1x,e12.5,";"))') 'ops_plumerise,A; ircp; istab; disx; disxx; hbron; htt; htt-hbron; htot; onder; xl; xloc; ',&
                                                              ircp, istab, disx, disxx, hbron, htt, htt-hbron, htot, onder, xl, xloc
endif
if (error%haserror) goto 9999

!------------------------------------------------
! Compute vertical dispersion coefficient sigma_z 
!------------------------------------------------

! Skip computation of vertical dispersion if point source AND receptor very near point source (disxx = disxx, disxx <= 1)
! in other cases (area source, point source and receptor further away) compute vertical dispersion.
dsx = AMAX1(disxx, radius)
IF (dsx .GT. (1. + EPS_DELTA)) THEN

   ! Compute vertical dispersion coefficient at receptor with (z0,u*,L,uh,zu) of source site
   debugnam = 'at_rcp_with_meteo_src'
   CALL ops_vertdisp(varin, z0_src, xl, ol_src, uster_src, htot, dsx, ircp, istab, debugnam, uh, zu, sz_rcp_stab_src, error)
   if (error%debug) write(*,'(3a,2(1x,i6,";"),99(1x,e12.5,";"))') trim(ROUTINENAAM),',A2; ', &
      'ircp; istab; disx; disxx; z0_src; xl; ol_src; uster_src; htot; dsx; uh; zu; sz_rcp_stab_src; ', &
       ircp, istab, disx, disxx, z0_src, xl, ol_src, uster_src, htot, dsx, uh, zu, sz_rcp_stab_src

   ! Compute vertical dispersion coefficient at receptor with (z0,u*,L,uh,zu) of receptor site
   debugnam = 'at_rcp_with_meteo_rcp'
   CALL ops_vertdisp(varin, z0_rcp, xl, ol_rcp, uster_rcp, htot, dsx, ircp, istab, debugnam, uh_rcp, zu_rcp, sz_rcp, error)
   if (error%debug) write(*,'(3a,2(1x,i6,";"),99(1x,e12.5,";"))') trim(ROUTINENAAM),',B2; ', &
      'ircp; istab; disx; disxx; z0_rcp; xl; ol_rcp; uster_rcp; htot; dsx; uh_rcp; zu_rcp; sz_rcp; ', &
       ircp, istab, disx, disxx, z0_rcp, xl, ol_rcp, uster_rcp, htot, dsx, uh_rcp, zu_rcp, sz_rcp

!
!  Limit sigma_z at source, such that sigma_z(source) < sigma_z(receptor)
!
   IF (sz_rcp_stab_src .GT. (sz_rcp + EPS_DELTA)) THEN
      sz_rcp = sz_rcp_stab_src
   ENDIF
!
!  Compute dispersion coefficient dispg of sigma_z at receptor, averaged over 
!     1) sz_rcp_stab_src = sigma_z at receptor using stability parameters at source;
!     2) sz_rcp          = sigma_z at receptor using stability parameters at receptor.
!  sigma_z = dispg*disxx**disph <=> dispg = sigma_z/(disxx**disph),  3.16 new! OPS report
!  Since in the rest of the code the old formula sigma_z = dispg*disxx**disph is still used,
!  we need dispg and disph and we do not use sz_rcp_stab_src and sz_rcp hereafter. 
   dispg = (sz_rcp_stab_src + sz_rcp)*0.5/(dsx**DISPH(istab))
   if (error%debug) write(*,'(3a,2(1x,i6,";"),99(1x,e12.5,";"))') trim(ROUTINENAAM),',C; ', 'ircp; istab; disx; disxx; dispg; disph; ', &
                                                                                             ircp, istab, disx, disxx, dispg, DISPH(istab)
        
   ! Check limits 0 <= dispg <= 50; if outside limits, generate warning:
   IF (dispg < 0. - EPS_DELTA .OR. dispg > 50. + EPS_DELTA) THEN
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
IF (icm==icm_NOx .OR. icm==icm_NH3) THEN
        
  IF  (ibtg .EQ. 4) THEN
    ! Map (0, 1) to (1 - DIURNAL_SCALE_RANGE, 1 + DIURNAL_SCALE_RANGE), see the
    ! comment above the declaration of `DIURNAL_SCALE_RANGE`. A scale index of
    ! 0.5 results in no change.
    temp_scale = ( &
         1.0 &
       + 2.0 * varin%varin_unc%diurn_scale_index * DIURNAL_SCALE_RANGE &
       - DIURNAL_SCALE_RANGE &
    )
    ! Scale the temperature to allow performing sensitivity analyses.
    temp_C_rescaled = (temp_C - Tavg) * temp_scale + Tavg

    ! Emissions from animal housing; TNO: (Bas Nijenhuis, 990207)
    ! temperature correction for NH3 emissions from animal housing systems; OPS report 6.33.
    ! Tavg = 10 C
    ! Temperature correction tcor = 1 + (T - Tavg)/f = 1 + T/f - 10/f = (1-10/f) + T/f = (f-10)/f + T/f = (T + f-10)/f; 
    ! Here f = 34, corresponding with a factor 1/34 = 0.0294 (0.04 in 6.33 OPS report). FS
    !
    tcor=max((temp_C_rescaled + 24) / 34, 0.2)
                                                   
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
    
    emf=0.0000155*((100./(ra_ms_4+rb_ms))**0.8*(temp_C+23)**2.3)**1.25        ! 981209 
     
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

end module m_ops_stab_rek
