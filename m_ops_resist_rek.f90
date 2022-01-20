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

module m_ops_resist_rek

!-------------------------------------------------------------------------------------------------------------------------------
! USAGE              : %M%
! DESCRIPTION        : Compute resistances for dry deposition, in-cloud scavenging ratio and chemical conversion rate.
!
! contains 
! ops_resist_rek               : computes all resistances needed for dry deposition + chemical conversion rate + in-cloud scavenging ratio; calls all routines below.
! ops_resist_ra_all            : compute aerodynamic resistances Ra at different locations and different heights
! ops_resist_ra                : compute aerodynamic resistances Ra at a specific location and height
! ops_resist_rb_all            : compute laminar boundary layer resistances Rb at different locations
! ops_resist_rb                : compute laminar boundary layer resistance Rb at a specific location
! ops_resist_rc_all            : compute canopy resistances Rc at different locations
! ops_resist_rc                : compute canopy resistance Rc at a specific location [s/m] for acidifying components (SO2, NOx, NH3), calling DEPAC.
! ops_resist_rain_out_scav_rate: compute rain out scavenging rate
! ops_resist_rc_sec_trj        : compute aerodynamic resistance for secondary component for trajectory
! ops_resist_rcaer             : compute aerodynamic resistance for aerosols at the receptor
!-------------------------------------------------------------------------------------------------------------------------------

implicit none

contains

SUBROUTINE ops_resist_rek(vchemc, iopt_vchem, vchemv, rad, isec, icm, rc_so2_ms, regenk, rc_aer_ms, iseiz, istab, itra, ar, &
                          rno2nox, vchemnh3, vchem2, hum, uster_rcp, ol_rcp, uster_tra, &
                          z0_rcp, z0_metreg_rcp, kdeel, mb, vw10, temp_C, zm, koh, &
                          rations, rhno3_trj, rc_no, rhno2, rc_hno3, croutpri, rrno2nox, rhno3_rcp, &
                          rb_ms, ra_ms_4, ra_ms_zra, rc_user, routpri, vchem, rc_sec_trj, uh, rc_sec_rcp, rc_eff_rcp_4_pos, rb_rcp, &
                          ra_rcp_4, ra_rcp_zra, ra_rcp_zrcp, z0_src, ol_src, uster_src, z0_tra, rc_eff_trj_4_pos, rc_eff_src_4_pos, ra_src_4, &
                          rb_src, ra_trj_4, ra_trj_zra, rb_trj, rc_eff_rcp_4, nh3bg_rcp, nh3bgtra, &
                          so2bg_rcp, so2bgtra, gym, depudone, gasv, lu_rcp_per, lu_tra_per, rnox)

use m_commonconst_lt, only: EPS_DELTA, CNAME
use m_ops_vchem
use m_ops_meteo
use m_commonconst_lib, only: NLU

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_resist_rek')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: vchemc                     ! chemical conversion rate [%/h]
INTEGER*4, INTENT(IN)                            :: iopt_vchem                 ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
REAL*4,    INTENT(IN)                            :: vchemv                     ! light dependent part of chemical conversion rate
REAL*4,    INTENT(IN)                            :: rad                        ! global radiation [J/cm2/h]
LOGICAL,   INTENT(IN)                            :: isec                       ! TRUE if component=[SO2, NOx, NH3]
INTEGER*4, INTENT(IN)                            :: icm                        ! component number
REAL*4,    INTENT(IN)                            :: rc_so2_ms                  ! Rc(SO2) from meteo statistics [s/m] 
REAL*4,    INTENT(IN)                            :: regenk                     ! rain probability [-]
REAL*4,    INTENT(IN)                            :: rc_aer_ms                  ! Rc(SO4-aerosol) from meteo statistics [s/m]
INTEGER*4, INTENT(IN)                            :: iseiz                      ! 0 = long term, 1 = year, 2 = winter, 3 = summer, 4 = month in winter, 5 = month in summer
INTEGER*4, INTENT(IN)                            :: istab                      ! index of stability class
INTEGER*4, INTENT(IN)                            :: itra                       ! index of trajectory class
REAL*4,    INTENT(IN)                            :: ar                         ! proportionality constant [ppb J-1 cm2 h] in relation [OH] = ar Qr
REAL*4,    INTENT(IN)                            :: rno2nox                    ! season dependent component of NO2/NOx ratio
REAL*4,    INTENT(IN)                            :: vchemnh3                   ! chemical conversion rate for NH3 -> NH4 [%/h]
type(Tvchem), INTENT(IN)                         :: vchem2                     ! structure for chemical conversion rates
REAL*4,    INTENT(IN)                            :: hum                        ! relative humidity [%]
REAL*4,    INTENT(IN)                            :: uster_rcp                  ! friction velocity at receptor; for z0 at receptor [m/s]
REAL*4,    INTENT(IN)                            :: ol_rcp                     ! Monin-Obukhov length at receptor; for z0 at receptor [m/s]
REAL*4,    INTENT(IN)                            :: uster_tra                  ! friction velocity representative for trajectory [m/s]
REAL*4,    INTENT(IN)                            :: z0_rcp                     ! roughness length at receptor; from z0-map [m]
REAL*4,    INTENT(IN)                            :: z0_metreg_rcp              ! roughness length at receptor; interpolated from meteo regions [m]
INTEGER*4, INTENT(IN)                            :: kdeel                      ! index of particle size class
INTEGER*4, INTENT(IN)                            :: mb                         ! month number (only used for iseiz = 4,5)
REAL*4,    INTENT(IN)                            :: vw10                       ! wind speed at 10 m height [m/s]
REAL*4,    INTENT(IN)                            :: temp_C                     ! temperature at height zmet_T [C]
REAL*4,    INTENT(IN)                            :: zm                         ! z-coordinate of receptor point (m)
REAL*4,    INTENT(IN)                            :: koh                        ! reaction constant [ppb-1 h-1] for NO2 + OH -> HNO3
REAL*4,    INTENT(IN)                            :: rations                    ! NH3/SO2 ratio over trajectory 
REAL*4,    INTENT(IN)                            :: rhno3_trj                  ! ratio [HNO3]/[NO3_total] for trajectory [-]
REAL*4,    INTENT(IN)                            :: rc_no                      ! canopy resistance for NO [s/m]
REAL*4,    INTENT(IN)                            :: rhno2                      ! ratio HNO2/NOx [-]
REAL*4,    INTENT(IN)                            :: rc_hno3                    ! canopy resistance for HNO3 [s/m]
REAL*4,    INTENT(IN)                            :: croutpri                   ! constant (initial) in-cloud scavenging ratio [-] for primary component                   
REAL*4,    INTENT(IN)                            :: rrno2nox                   ! space varying component in ratio NO2/NOx 
REAL*4,    INTENT(IN)                            :: rhno3_rcp                  ! ratio [HNO3]/[NO3_totaal] at the receptor [-]
REAL*4,    INTENT(IN)                            :: z0_src                     ! roughness length at source; from z0-map [m]
REAL*4,    INTENT(IN)                            :: ol_src                     ! Monin-Obukhov length at source [m]
REAL*4,    INTENT(IN)                            :: uster_src                  ! friction velocity at source [m/s]
REAL*4,    INTENT(IN)                            :: z0_tra                     ! roughness length representative for trajectory [m]
REAL*4,    INTENT(IN)                            :: nh3bg_rcp                  ! background NH3-concentration at receptor [ug/m3]
REAL*4,    INTENT(IN)                            :: nh3bgtra                   ! background NH3-concentration over trajectory [ug/m3]
REAL*4,    INTENT(IN)                            :: so2bg_rcp                  ! background SO2-concentration at receptor [ug/m3]
REAL*4,    INTENT(IN)                            :: so2bgtra                   ! background SO2-concentration over trajectory [ug/m3]
REAL*4,    INTENT(IN)                            :: gym                        ! y-coordinate of receptor (lon-lat) [degrees]
LOGICAL,   INTENT(IN)                            :: gasv                       ! TRUE for gasuous component
REAL*4,    INTENT(IN)                            :: lu_rcp_per(NLU)            ! land use percentages for all land use classes of receptor
REAL*4,    INTENT(IN)                            :: lu_tra_per(NLU)            ! land use percentages for all land use classes over trajectory
REAL*4,    INTENT(IN)                            :: rc_user                    ! canopy resistance specified by user in control file [s/m]
REAL*4,    INTENT(IN)                            :: ra_ms_4                    ! aerodynamic resistance at 4 m from meteo statistics [s/m]
REAL*4,    INTENT(IN)                            :: ra_ms_zra                  ! aerodynamic resistance at height zra from meteo statistics [s/m] 

! SUBROUTINE ARGUMENTS - I/O
REAL*4,    INTENT(INOUT)                         :: rb_ms                      ! (in) boundary layer resistance SO2 from meteo statistics [s/m]
                                                                               ! (out) boundary layer resistance SO2 or NH3 from meteo statistics [s/m]
LOGICAL,   INTENT(INOUT)                         :: depudone                   ! Ra, Rb have been computed (no need to repeat this for all particle size classes)
                                                                               
! SUBROUTINE ARGUMENTS - I/O aerodynamid (Ra) and boundary layer (Rb) resistances
! Ra, Rb values for particles (.not. gasv) are only computed for the first particle size class (intent(OUT) and for the 
! next classes they use the values from the call of the first class (intent(IN))
REAL*4,    INTENT(INOUT)                         :: ra_src_4                   ! aerodynamic resistance at source, 4 m height [s/m] 
REAL*4,    INTENT(INOUT)                         :: rb_src                     ! boundary layer resistance at source [s/m] 

REAL*4,    INTENT(INOUT)                         :: ra_trj_4                   ! aerodynamic resistance for trajectory, 4 m height [s/m] 
REAL*4,    INTENT(INOUT)                         :: ra_trj_zra                 ! aerodynamic resistance for trajectory, height zra [s/m];
                                                                               ! zra is height where concentration profile is undisturbed by deposition = 50 m 
REAL*4,    INTENT(INOUT)                         :: rb_trj                     ! boundary layer resistance for trajectory [s/m]

REAL*4,    INTENT(INOUT)                         :: ra_rcp_zrcp                ! aerodynamic resistance at receptor, height zrcp [s/m]
REAL*4,    INTENT(INOUT)                         :: ra_rcp_4                   ! aerodynamic resistance at receptor, 4 m height [s/m] 
REAL*4,    INTENT(INOUT)                         :: ra_rcp_zra                 ! aerodynamic resistance at receptor, height zra [s/m];
                                                                               ! zra is height where concentration profile is undisturbed by deposition = 50 m
REAL*4,    INTENT(INOUT)                         :: rb_rcp                     ! boundary layer resistance at receptor [s/m] 


! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: routpri                    ! in-cloud scavenging ratio [-] for primary component (rout << rain-out = in-cloud) 
REAL*4,    INTENT(OUT)                           :: vchem                      ! chemical conversion rate [%/h]
REAL*4,    INTENT(OUT)                           :: uh                         ! wind speed used in parametrisation of vd for aerosols [m/s]
REAL*4,    INTENT(OUT)                           :: rnox                       ! NO2/NOx ratio

! Canopy resistances Rc
REAL*4,    INTENT(OUT)                           :: rc_eff_src_4_pos           ! effective canopy resistance at source, 4 m height, no re-emission (Rc is positive) [s/m]; 
                                                                               !   used for the computation of cq1 = source depletion ratio for all removal processes for phase 1 (area source) 
REAL*4,    INTENT(OUT)                           :: rc_eff_trj_4_pos           ! effective canopy resistance for trajectory, 4 m height, no re-emission (Rc is positive) [s/m]
                                                                               !   used for source depletion (loss over trajectory)
REAL*4,    INTENT(OUT)                           :: rc_eff_rcp_4_pos           ! effective canopy resistance at receptor, 4 m height, no re-emission (Rc is positive) [s/m];
                                                                               !   used for deposition gradient at receptor
REAL*4,    INTENT(OUT)                           :: rc_eff_rcp_4               ! effective canopy resistance at receptor, 4 m height, re-emission allowed [s/m];  
                                                                               !   used for the computation of drypri, the deposition at the receptor
! Rc for secondary component:
REAL*4,    INTENT(OUT)                           :: rc_sec_trj                 ! canopy resistance secondary aerosol (SO4, NO3, NH4) for trajectory [s/m]
REAL*4,    INTENT(OUT)                           :: rc_sec_rcp                 ! canopy resistance secondary aerosol (SO4, NO3, NH4) at the receptor [s/m]
                                                                               
! LOCAL VARIABLES
INTEGER*4                                        :: icmpsec                    ! component number secondary component used in ops_resist_rek_rcaer
INTEGER*4                                        :: nwet                       ! wetness indicator depending on percipitation probability and humidity
REAL*4                                           :: rad_W_m2                   ! global radiation [W/m2]
REAL*4, PARAMETER                                :: zra = 50.0                 ! zra is height where concentration profile is undisturbed by deposition = 50 m
!-------------------------------------------------------------------------------------------------------------------------------

! Wind speed for rough surfaces, such as forests (z0 > 0.5 m) -> from logarithmic wind profile at 8*z0(receptor);
! for smoother surfaces (z0 <= 0.5) -> set equal to wind speed at 10 m height.
IF (z0_rcp .GT. 0.5 + EPS_DELTA) THEN
   CALL ops_wv_log_profile(z0_rcp,8*z0_rcp,uster_rcp,ol_rcp, uh)
ELSE
   uh = vw10
ENDIF

! Convert global radiation from J/cm2/h to J/m2/s = W/m2; conversion factor = 1.0e4/3600 = 2.78
rad_W_m2 = rad*2.78

!  Compute nwet = wetness indicator depending on percipitation probability and humidity;
!                 dry(=0),wet(=1) or snow(=9); 6.25 OPS report
nwet = NINT((regenk * 0.4 + hum/59 - 0.4)**5*.3)                              ! 990324
IF(nwet.gt.1) nwet=1

! Compute rnox = [NO2]/[NOx] ratio:
if (icm .eq. 2) call ops_vchem_ratio_no2_nox(iseiz,istab,rrno2nox,rno2nox,rnox)

! Compute chemical conversion rate:
call ops_vchem(icm, isec, iopt_vchem, vchemc, vchemv, vchemnh3, vchem2, rad, rad_W_m2, regenk, iseiz, istab, itra, ar, koh, rnox, &
               vchem)
                     
! Continue for a gas and in case of particles if depudone is false (to prevent repeating this for each particle size class):
if ( gasv .OR. (.not.gasv .AND. .not.depudone)) THEN  ! this is equivalent with (gasv .OR. (.not.depudone))

   ! Compute aerodynamic resistance at different locations (src, trj, rcp) and different heights (4 m, zm=zrcp, zra):
   call ops_resist_ra_all(zm,zra,z0_src,z0_tra,z0_rcp,z0_metreg_rcp,ol_src,ol_rcp,uster_src,uster_rcp,ra_ms_4,ra_ms_zra, &
                             ra_src_4,ra_trj_4,ra_trj_zra,ra_rcp_zrcp,ra_rcp_4,ra_rcp_zra)

   ! Compute boundary layer resistance at different locations (src, trj, rcp):
   call ops_resist_rb_all(icm,uster_src,uster_tra,uster_rcp,rb_src,rb_trj,rb_rcp)
   
   ! Deposition parameters have been computed
   depudone = .TRUE.
ENDIF ! (gasv .OR. (.not.depudone))

! Compute rb_ms for NH3  



IF (icm .eq. 3) THEN
   !                                                   Rb(NH3)     M(NH3)  1/3   17  1/3
   ! rb_ms  : boundary layer resistance rb_ms [s/m];  -------- = (--------)  = (----)  = 0.6431
   !          (4.5 OPS report)                         Rb(SO2)     M(SO2)        64
   
   rb_ms = rb_ms*0.64
ENDIF

! Compute routpri = in-cloud scavenging ratio for primary component; (rout << rain-out = in-cloud) [-])
call ops_resist_rain_out_scav_rate(icm,gasv,isec,kdeel,croutpri,rations,rnox,routpri)   

! Compute canopy resistances at different locations and different heights (effective Rc may depend on height via Ra):
call ops_resist_rc_all(icm,isec,gasv,kdeel,iseiz,mb,gym,uster_tra,uster_rcp, &
                       nwet,hum,temp_C,rad_W_m2,lu_tra_per,lu_rcp_per,so2bgtra,nh3bgtra,so2bg_rcp,nh3bg_rcp, &
                       ra_ms_4,ra_trj_4,ra_rcp_4,rb_trj,rb_ms,rb_rcp,rc_user,rc_so2_ms,rc_no,rnox,rhno2, &
                       rc_eff_src_4_pos,rc_eff_trj_4_pos,rc_eff_rcp_4,rc_eff_rcp_4_pos)

IF (isec) THEN
  
   ! Compute rc_sec_trj = canopy resistance secondary aerosol representative for trajectory:
   call ops_resist_rc_sec_trj(icm,ra_ms_4,rb_ms,rc_aer_ms,rc_hno3,rhno3_trj,rc_sec_trj)   
                  
   !---------------------------------------------------------------------------------------------------
   !  Compute rc_sec_rcp = canopy resistance secondary aerosol (SO4, NO3, NH4) at the receptor [s/m]
   !---------------------------------------------------------------------------------------------------
   
   !  Set component number for secondary component of SO2, NO2, NH3:
   !  11: sulphate
   !  12: nitrate
   !  13: ammonium
   !  and set a default secondary component = 11 (note that in this IF-branch, isec = .true., so icm = 1,2,3)
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

   ! Compute rc_sec_rcp = canopy resistance secondary aerosol (SO4, NO3, NH4) at the receptor [s/m]:
   CALL ops_resist_rcaer (icmpsec, z0_rcp, uster_rcp, ol_rcp, hum, nwet, uh, ra_ms_4, rb_ms, rc_hno3, rhno3_rcp, rc_sec_rcp)   

ENDIF

RETURN

END SUBROUTINE ops_resist_rek

!-----------------------------------------------------------------------------------------------------
SUBROUTINE ops_resist_ra_all(zm,zra,z0_src,z0_tra,z0_rcp,z0_metreg_rcp,ol_src,ol_rcp,uster_src,uster_rcp,ra_ms_4,ra_ms_zra, &
                             ra_src_4,ra_trj_4,ra_trj_zra,ra_rcp_zrcp,ra_rcp_4,ra_rcp_zra)

! Compute aerodynamic resistances Ra at different locations (src, trj, rcp)

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: zm                         ! z-coordinate of receptor point (m)
REAL*4,    INTENT(IN)                            :: zra                        ! zra is height where concentration profile is undisturbed by deposition = 50 m
REAL*4,    INTENT(IN)                            :: z0_src                     ! roughness length at source; from z0-map [m]
REAL*4,    INTENT(IN)                            :: z0_tra                     ! roughness length representative for trajectory [m]
REAL*4,    INTENT(IN)                            :: z0_rcp                     ! roughness length at receptor; from z0-map [m]
REAL*4,    INTENT(IN)                            :: z0_metreg_rcp              ! roughness length at receptor; interpolated from meteo regions [m]
REAL*4,    INTENT(IN)                            :: ol_src                     ! Monin-Obukhov length at source [m]
REAL*4,    INTENT(IN)                            :: ol_rcp                     ! Monin-Obukhov length at receptor; for z0 at receptor [m/s]
REAL*4,    INTENT(IN)                            :: uster_src                  ! friction velocity at source [m/s]
REAL*4,    INTENT(IN)                            :: uster_rcp                  ! friction velocity at receptor; for z0 at receptor [m/s]
REAL*4,    INTENT(IN)                            :: ra_ms_4                    ! aerodynamic resistance at 4 m from meteo statistics [s/m]
REAL*4,    INTENT(IN)                            :: ra_ms_zra                  ! aerodynamic resistance at height zra from meteo statistics [s/m] 
                                                                               
! SUBROUTINE ARGUMENTS - OUTPUT aerodynamic resistances Ra
REAL*4,    INTENT(OUT)                           :: ra_src_4                   ! aerodynamic resistance at source, 4 m height [s/m] 
REAL*4,    INTENT(OUT)                           :: ra_trj_4                   ! aerodynamic resistance for trajectory, 4 m height [s/m] 
REAL*4,    INTENT(OUT)                           :: ra_trj_zra                 ! aerodynamic resistance for trajectory, height zra [s/m];
                                                                               ! zra is height where concentration profile is undisturbed by deposition = 50 m 
REAL*4,    INTENT(OUT)                           :: ra_rcp_zrcp                ! aerodynamic resistance at receptor, height zrcp [s/m]
REAL*4,    INTENT(OUT)                           :: ra_rcp_4                   ! aerodynamic resistance at receptor, 4 m height [s/m] 
REAL*4,    INTENT(OUT)                           :: ra_rcp_zra                 ! aerodynamic resistance at receptor, height zra [s/m];
                                                                               ! zra is height where concentration profile is undisturbed by deposition = 50 m
! LOCAL VARIABLES
REAL                                             :: d                          ! displacement height [m]

! displacement height (usually 0.7 * vegetation height) (m)

d = 0.

! Calculate aerodynamic resistance Ra at receptor/receptor height, source/4 m height, 
! receptor/4 m height and receptor/zra;
! zra is height where concentration profile is undisturbed by deposition = 50 m
CALL ops_resist_ra(z0_src,  4., d, ol_src, uster_src, ra_src_4)
CALL ops_resist_ra(z0_rcp,  zm, d, ol_rcp, uster_rcp, ra_rcp_zrcp)
CALL ops_resist_ra(z0_rcp,  4., d, ol_rcp, uster_rcp, ra_rcp_4)
CALL ops_resist_ra(z0_rcp, zra, d, ol_rcp, uster_rcp, ra_rcp_zra)

! Compute Ra averaged over trajectory at 4 m height and zra = 50 m height; 
! ra_ms_4 = Ra(4 m) from meteo statistics, based on z0_metreg_rcp, which is interpolated from meteo regions.
! 4.3 OPS report, neglecting the stability corrections psi_h and assuming that u* remains the same.
!
!  Ra(z=4,trajectory)       z=4              z=4
! ------------------- = ln(-------) / ln(-------------)
!    Ra(z=4)               z0_tra        z0_metreg_rcp
   
ra_trj_4   = ra_ms_4   * alog( 4./z0_tra)/alog( 4./z0_metreg_rcp)
ra_trj_zra = ra_ms_zra * alog(zra/z0_tra)/alog(zra/z0_metreg_rcp)

END SUBROUTINE ops_resist_ra_all

!-----------------------------------------------------------------------------------------
SUBROUTINE ops_resist_ra(z0, z_dep, d, ol, uster, ra)

! AUTHOR             : J.W. mrt 1990/ Franka Loeve (Cap Volmac)
! DESCRIPTION        : This routine calculates aerodynamic resistance Ra [s/m]
!                      the aerodynamic resistance over a layer with height z is the integral from z0 to z of
!                      1/K(z)dz with K(z)=k.u*.z/phi(h) and phi(h)=(1-16z/l)**-0.5 for l<0 and 1.+5.(z/l) for l>0. (Dyer and
!                      Hicks (1977) relations)
!                      By integration we get ra=1./(k.u*).(ln(z0/z)+ghi(z0)-ghi(z)) with ghi(z), the stability correction for
!                      heat (Beljaars et al., 1989).
!
!                      References:
!                      1. Businger J.A. (1973) in: workshop on micrometeorology
!                         Atm. Meteor. Soc.
!                      2. Wesely M.L. and Hicks B.B. (1977)
!                         Some factors that effect the deposition rates on so2 and simular gases on vegetation.
!                         J. Air Pollut. Contr. Org. 27 no.11, pp 1110-1116.
!                      3. Hicks B.B., Baldocchi D.D., Meyers T.P., Hosker Jr. R.P., Matt D.R. (1987)
!                         A preliminary multiple resistance routine for deriving dry deposition velocities from measured
!                         quantities.
!                         J. Water, Air and Soil Pollut.,36,311-330.
!                      4. Erisman J.W., Versluis A.H., Verplanke T.A.J.W., de Haan D., Anink D., van Elzakker B.G. and van Aalst
!                         R.M. (1990)
!                         Monitoring the dry deposition of SO2 in the Netherlands.
!                         Report nr. 228601002, RIVM, Bilthoven, the Netherlands.
!                      5. Beljaars A.C.M., Holtslag A.A.M. and Westrhenen R.M. van (1989)
!                         Description oa a software library for the calculation of surface fluxes.
!                         Technical report TR-112, KNMI, De Bilt.

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_resist_ra')

! CONSTANTS
REAL*4                                           :: VONK                       ! Von Karman constant
PARAMETER   (VONK = 0.4  )

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: z0                         ! surface roughness length in meters.
REAL*4,    INTENT(IN)                            :: z_dep                      ! height for which deposition velocity is calculated (m)
REAL*4,    INTENT(IN)                            :: d                          ! displacement height (usually 0.7 * vegetation height) (m)
REAL*4,    INTENT(IN)                            :: ol                         ! monin-obukhov length (m)
REAL*4,    INTENT(IN)                            :: uster                      ! friction velocity u* (m/s)

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: ra                         ! aerodynamic resistance at z_dep (s/m)

! LOCAL VARIABLES
REAL*4                                           :: zru                        ! height, corrected for displacement height

! Correction for the displacement height:
zru = z_dep - d

! Compute aerodynamic resistance Ra:
ra = (1./(VONK*uster))*(alog((zru)/z0) - fpsih(zru/ol) + fpsih(z0/ol))

END SUBROUTINE ops_resist_ra

!-----------------------------------------------------------------------------------------------------
SUBROUTINE ops_resist_rb_all(icm,uster_src,uster_tra,uster_rcp,rb_src,rb_trj,rb_rcp)

! Compute laminar boundary layer resistances Rb at different locations (src, trj, rcp)

! SUBROUTINE ARGUMENTS - INPUT
INTEGER, INTENT(IN)                              :: icm                        ! component number
REAL,    INTENT(IN)                              :: uster_src                  ! friction velocity at source [m/s]
REAL,    INTENT(IN)                              :: uster_tra                  ! friction velocity representative for trajectory [m/s]
REAL,    INTENT(IN)                              :: uster_rcp                  ! friction velocity at receptor; for z0 at receptor [m/s]
                                   
! SUBROUTINE ARGUMENTS - OUTPUT    
REAL,    INTENT(OUT)                             :: rb_src                     ! boundary layer resistance at source [s/m] 
REAL,    INTENT(OUT)                             :: rb_trj                     ! boundary layer resistance for trajectory [s/m]
REAL,    INTENT(OUT)                             :: rb_rcp                     ! boundary layer resistance at receptor [s/m] 

! LOCAL VARIABLES
INTEGER                                          :: icnr                       ! component number for calculation of rb

! 
! Component number icnr for calculation of Rb;
! Rb currently only for SO2 or NH3; for NO2 Rc values are high and Rb not so important.
IF (icm .EQ. 3) THEN
   icnr = 9
ELSE
   icnr = 1
ENDIF

! Calculate boundary layer resistance Rb at source and receptor:
CALL ops_resist_rb(icnr, uster_src, rb_src)
CALL ops_resist_rb(icnr, uster_rcp, rb_rcp)

! Compute Rb for trajectory 4.4 OPS report:
rb_trj = rb_rcp*uster_rcp/uster_tra

END SUBROUTINE ops_resist_rb_all

!-----------------------------------------------------------------------------------------
SUBROUTINE ops_resist_rb(icnr, uster, rb)

! AUTHOR             : J.W. mrt 1990/ Franka Loeve (Cap Volmac)
! DESCRIPTION        : This routine calculates boundary layer resistance Rb [s/m].
!                      The laminair boundary layer resistance : rb=2(ku*)**-1*(Sc/Pr)**p with Sc=Schmidt number, Pr=Prandtl
!                      number, p=2/3 by experiment.
!
!                      References:
!                      1. Businger J.A. (1973) in: workshop on micrometeorology
!                         Atm. Meteor. Soc.
!                      2. Wesely M.L. and Hicks B.B. (1977)
!                         Some factors that effect the deposition rates on so2 and simular gases on vegetation.
!                         J. Air Pollut. Contr. Org. 27 no.11, pp 1110-1116.
!                      3. Hicks B.B., Baldocchi D.D., Meyers T.P., Hosker Jr. R.P., Matt D.R. (1987)
!                         A preliminary multiple resistance routine for deriving dry deposition velocities from measured
!                         quantities.
!                         J. Water, Air and Soil Pollut.,36,311-330.
!                      4. Erisman J.W., Versluis A.H., Verplanke T.A.J.W., de Haan D., Anink D., van Elzakker B.G. and van Aalst
!                         R.M. (1990)
!                         Monitoring the dry deposition of SO2 in the Netherlands.
!                         Report nr. 228601002, RIVM, Bilthoven, the Netherlands.
!                      5. Beljaars A.C.M., Holtslag A.A.M. and Westrhenen R.M. van (1989)
!                         Description oa a software library for the calculation of surface fluxes.
!                         Technical report TR-112, KNMI, De Bilt.
!

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_resist_rb')

! CONSTANTS
REAL*4                                           :: P                          ! exponent (experimenteel bepaald)
REAL*4                                           :: PR                         ! Prandtl number
REAL*4                                           :: VONK                       ! Von Karman constante

PARAMETER   (VONK = 0.4  )
PARAMETER   (P    = 2./3.)
PARAMETER   (PR   = 0.72 )

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: icnr                       ! component number for calculation of rb
REAL*4,    INTENT(IN)                            :: uster                      ! friction velocity u* (m/s)

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: rb                         ! laminar layer resistance for component incr (s/m)

! LOCAL VARIABLES
REAL*4                                           :: sc                         ! Schmidt number


! Schmidt number:
IF (icnr .EQ. 1 .OR. icnr .LE. 0) THEN                                         ! SO2 or user-defined substance
   sc = 1.25
ELSE IF (icnr .EQ. 5) THEN                                                     ! O3
   sc = 0.88
ELSE IF (icnr .EQ. 6) THEN                                                     ! NO
   sc = 1.07
ELSE IF (icnr .EQ. 7) THEN                                                     ! NO2
   sc = 1.07
ELSE IF (icnr .EQ. 8) THEN                                                     ! HNO3
   sc = 1.25
ELSE                                                                           ! NH3  (op basis van molgewicht)
   sc = 0.69
ENDIF

! Compute laminar boundary layer resistance Rb:
rb = (2./(VONK*uster))*((sc/PR)**P)

END SUBROUTINE ops_resist_rb

!-------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_resist_rc_all(icm,isec,gasv,kdeel,iseiz,mb,gym,uster_tra,uster_rcp, &
                             nwet,hum,temp_C,rad_W_m2,lu_tra_per,lu_rcp_per,so2bgtra,nh3bgtra,so2bg_rcp,nh3bg_rcp, &
                             ra_ms_4,ra_trj_4,ra_rcp_4,rb_trj,rb_ms,rb_rcp,rc_user,rc_so2_ms,rc_no,rnox,rhno2, &
                             rc_eff_src_4_pos,rc_eff_trj_4_pos,rc_eff_rcp_4,rc_eff_rcp_4_pos)

use m_commonconst_lt, only: EPS_DELTA, NPARTCLASS
use m_commonconst_lib, only: NLU

LOGICAL,   INTENT(IN)                            :: isec                       ! TRUE if component=[SO2, NOx, NH3]
INTEGER*4, INTENT(IN)                            :: icm                        ! component number
REAL*4,    INTENT(IN)                            :: rc_so2_ms                  ! Rc(SO2) from meteo statistics [s/m] 
INTEGER*4, INTENT(IN)                            :: iseiz                      ! 0 = long term, 1 = year, 2 = winter, 3 = summer, 4 = month in winter, 5 = month in summer
INTEGER*4, INTENT(IN)                            :: nwet                       ! wetness indicator depending on percipitation probability and humidity
REAL*4,    INTENT(IN)                            :: hum                        ! relative humidity [%]
REAL*4,    INTENT(IN)                            :: uster_rcp                  ! friction velocity at receptor; for z0 at receptor [m/s]
REAL*4,    INTENT(IN)                            :: uster_tra                  ! friction velocity representative for trajectory [m/s]
INTEGER*4, INTENT(IN)                            :: kdeel                      ! index of particle size class
INTEGER*4, INTENT(IN)                            :: mb                         ! month number (only used for iseiz = 4,5)
REAL*4,    INTENT(IN)                            :: temp_C                     ! temperature at height zmet_T [C]
REAL*4,    INTENT(IN)                            :: rad_W_m2                   ! global radiation [W/m2]
REAL*4,    INTENT(IN)                            :: rc_no                      ! canopy resistance for NO [s/m]
REAL*4,    INTENT(IN)                            :: rhno2                      ! ratio HNO2/NOx [-]
REAL*4,    INTENT(IN)                            :: nh3bg_rcp                  ! background NH3-concentration at receptor [ug/m3]
REAL*4,    INTENT(IN)                            :: nh3bgtra                   ! background NH3-concentration over trajectory [ug/m3]
REAL*4,    INTENT(IN)                            :: so2bg_rcp                  ! background SO2-concentration at receptor [ug/m3]
REAL*4,    INTENT(IN)                            :: so2bgtra                   ! background SO2-concentration over trajectory [ug/m3]
REAL*4,    INTENT(IN)                            :: gym                        ! y-coordinate of receptor (lon-lat) [degrees]
LOGICAL,   INTENT(IN)                            :: gasv                       ! TRUE for gasuous component
REAL*4,    INTENT(IN)                            :: lu_rcp_per(NLU)            ! land use percentages for all land use classes of receptor
REAL*4,    INTENT(IN)                            :: lu_tra_per(NLU)            ! land use percentages for all land use classes over trajectory
REAL*4,    INTENT(IN)                            :: rc_user                    ! canopy resistance specified by user in control file [s/m]
REAL*4,    INTENT(IN)                            :: rnox                       ! NO2/NOx ratio
REAL*4,    INTENT(IN)                            :: ra_ms_4                    ! aerodynamic resistance at 4 m from meteo statistics [s/m]
REAL*4,    INTENT(IN)                            :: ra_trj_4                   ! aerodynamic resistance for trajectory, 4 m height [s/m] 
REAL*4,    INTENT(IN)                            :: ra_rcp_4                   ! aerodynamic resistance at receptor, 4 m height [s/m] 
REAL*4,    INTENT(IN)                            :: rb_ms                      ! boundary layer resistance from meteo statistics [s/m]
REAL*4,    INTENT(IN)                            :: rb_trj                     ! boundary layer resistance for trajectory [s/m]
REAL*4,    INTENT(IN)                            :: rb_rcp                     ! boundary layer resistance at receptor [s/m] 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: rc_eff_src_4_pos           ! effective canopy resistance at source, 4 m height, no re-emission (Rc is positive) [s/m]; 
                                                                               !   used for the computation of cq1 = source depletion ratio for all removal processes for phase 1 (area source) 
REAL*4,    INTENT(OUT)                           :: rc_eff_trj_4_pos           ! effective canopy resistance for trajectory, 4 m height, no re-emission (Rc is positive) [s/m]
                                                                               !   used for source depletion (loss over trajectory)
REAL*4,    INTENT(OUT)                           :: rc_eff_rcp_4_pos           ! effective canopy resistance at receptor, 4 m height, no re-emission (Rc is positive) [s/m];
                                                                               !   used for deposition gradient at receptor
REAL*4,    INTENT(OUT)                           :: rc_eff_rcp_4               ! effective canopy resistance at receptor, 4 m height, re-emission allowed [s/m];  
                                                                               !   used for the computation of drypri, the deposition at the receptor

! LOCAL VARIABLES
INTEGER*4                                        :: iratns                     ! N/S ratio indicator (nitrogen/sulphur) (1 = low, 2 = high, 3 = very low ratio; see DEPAC)
REAL*4                                           :: rc_dummy                   ! Rc value not used (dummy output) [s/m]
REAL*4                                           :: r                          ! help variable = Ra + Rb [s/m]
REAL*4                                           :: catm 
REAL*4                                           :: c_ave_prev_nh3
REAL*4                                           :: c_ave_prev_so2
REAL*4                                           :: rc_hno2                    ! canopy resistance HNO2 [s/m]

! CONSTANTS
REAL*4                                           :: RCDEEL(NPARTCLASS)         ! canopy resistance for particle size classes [s/m]

! RCDEEL: canopy resistance Rc per per particle class computed from Ra and Rb values per particle class and 
! averaged dry deposition velocities vd (weighed over stability classes).
! See new OPS report, Table 5.2
! See also definition of VDDEEL in ops_depoparexp 
DATA RCDEEL/3200., 700., 150., 50., 2., -17./

! Compute canopy resistances Rc at different locations (src, trj, rcp) and different heights;
! note that the effective canopy resistance may depend on height via Ra
!
! rc_eff_src_4_pos: canopy resistance at the source, no re-emission allowed [s/m]; is used for the computation of 
!                   cq1 = source depletion ratio for all removal processes for phase 1 (area source) in ops_brondepl
! rc_eff_trj_4_pos: effective canopy resistance for trajectory, 4 m height, no re-emission allowed (Rc is positive) [s/m];
!                   is used for source depletion (loss over trajectory)
! rc_eff_rcp_4_pos: effective canopy resistance at receptor, 4 m height, no re-emission (Rc is positive) [s/m] 
!                   is used for deposition gradient at receptor
! rc_eff_rcp_4    : effective canopy resistance at receptor, 4 m height, re-emission allowed [s/m]; 
!                   is used for the computation of drypri, the deposition at the receptor


IF (isec) THEN

   ! Acidifying components
   
   ! N/S ratio indicator (nitrogen/sulphur) (1 = low, 2 = high, 3 = very low ratio; see DEPAC)
   iratns = 2
         
   !   1. Compute average canopy resistance Rc over trajectory.
   !      In case of NH3 a resistance scheme with compensationpoints is used for which two extra inputs are needed.
   !      c_ave_prev   air concentration averaged over a previous period (e.g. previous year or month) (ug/m3);
   !                   we use here the NH3 background concentration along the trajectory
   !      catm         actual atmospheric concentration (ug/m3); we use here the NH3 background concentration along the trajectory;
   !      the output Rc is returned in rc_eff_trj_4_pos = effective Rc over the trajectory (positive value, no net re-emission allowed)
   !
   !      (17/24) = conversion factor ppb -> ug/m3.
   catm           = nh3bgtra*17/24
   c_ave_prev_nh3 = nh3bgtra*17/24
   c_ave_prev_so2 = so2bgtra*64/24
   
   CALL ops_resist_rc(icm, iseiz, mb, gym ,temp_C, uster_tra, rad_W_m2, hum, nwet, iratns, catm, c_ave_prev_nh3, c_ave_prev_so2, lu_tra_per, &
                  &  ra_trj_4, rb_trj, rc_eff_trj_4_pos, rc_dummy)
   rc_eff_src_4_pos = rc_eff_trj_4_pos !
   
   ! 2. Compute canopy resistance Rc near the receptor.
   !    The same as above, but now we use the NH3 background concentration at the receptor as inputs;
   !    the output Rc is returned in rc_eff_rcp_4_pos = effective Rc near the receptor (always positive). 
   !    rc_eff_rcp_4 = effective Rc near the receptor (might become a negative value -> re-emission)
   !    Note: catm and c_ave_prev are only used for NH3.
   !    Conversion from ppb -> ug/m3 for nh3bg_rcp already done in ops_rcp_char.
   !
   catm           = nh3bg_rcp
   c_ave_prev_nh3 = nh3bg_rcp
   c_ave_prev_so2 = so2bg_rcp
   
   CALL ops_resist_rc(icm, iseiz, mb, gym ,temp_C, uster_rcp, rad_W_m2, hum, nwet, iratns, catm, c_ave_prev_nh3, c_ave_prev_so2, lu_rcp_per, &
                  &  ra_rcp_4, rb_rcp, rc_eff_rcp_4_pos, rc_eff_rcp_4)
                  
   !---------------------------------------------------------------------------------------------------
   !  Compute Rc-values for mixture NOx = NO + NO2 + HNO2 and for mixture HNO3 + NO3-aerosol
   !---------------------------------------------------------------------------------------------------
   IF (icm .EQ. 2) THEN     

      ! NOx
      ! The primary substance is calculated as NO2 (because emissions are specified as such) but contains in reality a mixture of
      ! NO, NO2 and HNO2. The whole is (finally) mentioned NOx and specified in ppb. Therefore dry deposition velocities have to
      ! be calculated as representative for the NO-NO2-HNO2 mixture
      ! Rc for NOx is, uptil now, Rc for NO2 (from DEPAC); now we compute the effective Rc for the NOx mixture as a weighed mean
      ! of the Rc-values for NO, NO2, HNO2.
      !
      !        1                [NO2]/[NOx]         (1-[NO2]/[NOx])        [HNO2]/[NOx]
      ! ------------------- = ------------------ + -------------------- + --------------------
      !  Rc(NOx) + Rb + Ra     Rc(NO2)+ Rb + Ra      Rc(NO) + Rb + Ra      Rc(HNO2) + Rb + Ra
      !
      r                = rb_ms + ra_ms_4
      rc_hno2          = rc_so2_ms        ! Rc(HNO2) = Rc(SO2); here Rc(SO2) from meteo statistics is used, so not Rc(SO2) of DEPAC
      rc_eff_rcp_4_pos = 1./(rnox/(rc_eff_rcp_4_pos+r)  + (1.-rnox)/(rc_no+r) + rhno2/(rc_hno2+r)) - r   
      rc_eff_rcp_4     = rc_eff_rcp_4_pos  ! for NOx no re-emission, only for NH3
      rc_eff_trj_4_pos = 1./(rnox/(rc_eff_trj_4_pos+r) + (1.-rnox)/(rc_no+r) + rhno2/(rc_hno2+r)) - r
      rc_eff_src_4_pos = rc_eff_trj_4_pos  
   ENDIF
ELSE
   
   ! (.not. isec): non-acidifying components
   
   IF (.NOT.gasv) THEN
      ! Set canopy resistances Rc for particles at different locations (receptor, source, average over trajectory and rc_eff_rcp_4)
      ! (Rc's all the same) and set routpri = in-cloud scavenging ratio [-] for the current particle class:
      rc_eff_rcp_4_pos = RCDEEL(kdeel)
      rc_eff_src_4_pos = RCDEEL(kdeel)
      rc_eff_trj_4_pos = RCDEEL(kdeel)
      rc_eff_rcp_4     = RCDEEL(kdeel)
   ELSE

      ! Other non-acidifying components. Set all Rc-values to user specified value read from control-file:
      rc_eff_rcp_4_pos = rc_user
      rc_eff_trj_4_pos = rc_user
      rc_eff_src_4_pos = rc_user
      rc_eff_rcp_4     = rc_user
   ENDIF
ENDIF 
   
END SUBROUTINE ops_resist_rc_all

!------------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_resist_rc(icm, iseiz, mb, gym ,temp_C, uster, rad_W_m2, hum, nwet, iratns, catm, c_ave_prev_nh3, c_ave_prev_so2, &
                      & lu_per, ra, rb, rc_eff_pos, rc_eff)

! Compute canopy resistance Rc [s/m] for acidifying components (SO2, NOx, NH3), calling DEPAC.
! ops_resist_rc uses one or more representative months and computes the average over different land use classes 
! (via averaging of deposition velocities).

use m_commonconst_lt, only: CNAME
use m_depac318
use m_commonconst_lib, only: NLU

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_resist_rc')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: icm                        ! component number (1 = SO2, 2 = NOx, 3 = NH3)
INTEGER*4, INTENT(IN)                            :: iseiz                      ! 0 = long term, 1 = year, 2 = winter, 3 = summer, 4 = month in winter, 5 = month in summer
INTEGER*4, INTENT(IN)                            :: mb                         ! month number (only used for iseiz = 4,5)
INTEGER*4, INTENT(IN)                            :: nwet                       ! wetness indicator depending on percipitation probability and humidity
REAL*4,    INTENT(IN)                            :: hum                        ! relative humidity [%] 
REAL*4,    INTENT(IN)                            :: uster                      ! friction velocity [m/s]
REAL*4,    INTENT(IN)                            :: temp_C                     ! temperature at height zmet_T [C]
REAL*4,    INTENT(IN)                            :: gym                        ! y-coordinate of receptor (lon-lat) [degrees]
REAL*4,    INTENT(IN)                            :: rad_W_m2                   ! global radiation [W/m2]
INTEGER*4, INTENT(IN)                            :: iratns                     ! N/S ratio indicator (nitrogen/sulphur) (1 = low, 2 = high, 3 = very low ratio; see DEPAC)
REAL*4,    INTENT(IN)                            :: catm                       ! actual atmospheric concentration used in compensation point [ug/m3]
REAL*4,    INTENT(IN)                            :: c_ave_prev_nh3             ! NH3 air concentration averaged over a previous period (e.g. previous year or month) [ug/m3]
REAL*4,    INTENT(IN)                            :: c_ave_prev_so2             ! SO2 air concentration averaged over a previous period (e.g. previous year or month) [ug/m3]
REAL*4,    INTENT(IN)                            :: ra                         ! aerodynamic resistance [s/m]
REAL*4,    INTENT(IN)                            :: rb                         ! boundary layer resistance [s/m]
REAL*4,    INTENT(IN)                            :: lu_per(NLU)                ! land use percentages for all land use classes [%]

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: rc_eff_pos                 ! canopy resistance, no re-emission [s/m]  
REAL*4,    INTENT(OUT)                           :: rc_eff                     ! canopy resistance, re-emission allowed [s/m];  

! LOCAL VARIABLES
INTEGER*4                                        :: day_of_year                ! 
INTEGER*4                                        :: mnt                        ! 
INTEGER*4, DIMENSION(2)                          :: mnt_select                 ! 
INTEGER*4                                        :: luclass                    ! 
REAL*4                                           :: som_vd_month               ! summed vd over representative months
REAL*4                                           :: som_vd_eff_ave             ! summed vd over land use classes (vd = 1/Ra + Rb + Rc_eff)
REAL*4                                           :: som_vd_eff_ave_pos         ! summed vd over land use classes (vd = 1/Ra + Rb + Rc_eff_pos)
REAL*4                                           :: telmaand 
REAL*4                                           :: rc_eff_ave                 ! canopy resistance, re-emission allowed, averaged over representative months
REAL*4                                           :: rc_eff_ave_pos             ! canopy resistance, no re-emission, averaged over representative months
REAL*4                                           :: rc_tot
REAL*4                                           :: sinphi
REAL*4                                           :: ccomp_tot
REAL*4, PARAMETER                                :: catm_min = 0.1E-05
REAL*4                                           :: rc_eff_depac               ! canopy resistance from depac, re-emission allowed [s/m];  

!-------------------------------------------------------------------------------------------------------------------------------

! Initialise sums:
som_vd_month       = 0.0
som_vd_eff_ave     = 0.0
som_vd_eff_ave_pos = 0.0
   
! loop over land use classes:
DO luclass = 1,NLU
  IF (lu_per(luclass) /= 0.0) THEN

    telmaand = 0.0
    som_vd_month = 0.0
!
!  Select representative month(s)
!
! iseiz: 0 = long term, 1 = year, 2 = winter, 3 = summer, 4 = month in winter, 5 = month in summer
!
! Arable land and long term or year run; average over April and July;
! April and July are representative months in order to be able to compute deposition over a whole year;
! this has been tested against results for 12 separate months
!
! No arable land; choose one representative month;
! long term , year run -> May
! winter               -> November
! summer               -> June
! 1 month              -> use mb, which has been read in ops_read_meteo
!
    SELECT CASE(iseiz)
    CASE(0, 1)
      IF (luclass == 2) THEN
        mnt_select(1)=4
        mnt_select(2)=7
      ELSE
        mnt_select=5
      ENDIF
    CASE(2)
       mnt_select=11
    CASE(3)
      mnt_select=6
    CASE DEFAULT
      mnt_select=mb
    END SELECT
!      Compute Rc only for mnt_select(1) and if necessary mnt_select(2) 
!
    DO mnt=1,12
      IF (mnt .EQ. mnt_select(1) .OR. mnt .EQ. mnt_select(2) ) THEN
!
!          Set approximate day of year:
!
        day_of_year = mnt*30-15   
!
!          Set sin of solar elevation angle; 
!          fit of sinphi is based on hourly data of global radiation (cloudy hours are filtered out)
!
        sinphi = 0.00237*rad_W_m2 - .00000186*rad_W_m2*rad_W_m2  
!
!          Update month counter:
! 
       telmaand = telmaand+1
!
!           Compute canopy resistance Rc with DEPAC for icm = 1,2,3 (CNAM = 'SO2', 'NO2', 'NH3').
!           DEPAC has 3 outputs:
!           rc_tot      : total canopy resistance Rc (is not used here)
!           ccomp_tot   : total compensation point (is not used here)
!           rc_eff_depac: effective Rc (includes effect of compensation point); rc_eff_depac depends on the value of Ra and Rb.
!          
        CALL depac318(CNAME(icm,5), day_of_year, gym ,temp_C, uster, rad_W_m2, sinphi, hum, nwet, luclass, iratns,   & 
                    & rc_tot, c_ave_prev_nh3, c_ave_prev_so2, max(catm,catm_min), ccomp_tot, ra, rb, rc_eff_depac)
!
!          Detect missing values and set default values
!
        IF (rc_eff_depac  .EQ. -9999) rc_eff_depac = 10000
      
        som_vd_month = som_vd_month + 1/(rc_eff_depac + ra + rb)
      ENDIF
    ENDDO ! loop over representative months
!
!   Compute average over selected months:
!
    rc_eff_ave  = telmaand / som_vd_month  - (ra + rb)
!
!   Negative values for effective Rc (re-emission) is not allowed in _pos variables; reset Rc = 1000 
! 
    IF (rc_eff_ave .GT. 0 ) THEN
      rc_eff_ave_pos = rc_eff_ave
    ELSE
      rc_eff_ave_pos = 1000  
    ENDIF
!
!      Compute average weighted conductance over the landuse types
!
    som_vd_eff_ave_pos = som_vd_eff_ave_pos + lu_per(luclass)/sum(lu_per(1:NLU)) * 1/(rc_eff_ave_pos + (ra + rb))
    som_vd_eff_ave     = som_vd_eff_ave     + lu_per(luclass)/sum(lu_per(1:NLU)) * 1/(rc_eff_ave     + (ra + rb))
  ENDIF
ENDDO  ! loop over land use classes

! Compute Rc without (_pos) and with re-emission: 
rc_eff_pos = 1/som_vd_eff_ave_pos - (ra + rb)
rc_eff     = 1/som_vd_eff_ave     - (ra + rb)

END SUBROUTINE ops_resist_rc

!-------------------------------------------------------------------------------------------------------------------------------
REAL FUNCTION fpsih(eta)

! FUNCTION           : fpsih
! DESCRIPTION        : Stability correction function in the surface layer temperature profile. The present model is an empirical
!                      fit by Holtslag and De Bruin(1987) of data by Hicks (1976, Quart. J. R. Meteor. Soc., 102, 535-551).
!                      See also Holtslag (1984, BLM, 29, 225-250)
! AUTHOR             : ANTON BELJAARS (KNMI 25-5-87) / Franka Loeve (Cap Volmac)

use m_commonconst_lt                                                              ! EPS_DELTA only

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'fpsih')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: eta                        ! stabiliteitsparameter z/l

! OUTPUT
!     Return value
!
! LOCAL VARIABLES
REAL*4                                           :: y                          ! hulpvariabele bij de berekening

!-------------------------------------------------------------------------------------------------------------------------------
IF (eta .LT. (0. - EPS_DELTA)) THEN
   y         = SQRT(1. - 16.*eta)
   fpsih = 2.*ALOG((1. + y)/2.)
ELSE
   fpsih = -(0.7*eta) - (0.75*eta - 10.72)* EXP( -0.35*eta) - 10.72
ENDIF

END FUNCTION fpsih

!----------------------------------------------------------------------------------------
SUBROUTINE ops_resist_rain_out_scav_rate(icm,gasv,isec,kdeel,croutpri,rations,rnox,routpri)   

! Compute in-cloud scavenging ratio for acidifying component; (rout << rain-out = in-cloud) [-])

use m_commonconst_lt, only: NPARTCLASS

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: icm                        ! component number
LOGICAL,   INTENT(IN)                            :: gasv                       ! TRUE for gasuous component
LOGICAL,   INTENT(IN)                            :: isec                       ! TRUE if component=[SO2, NOx, NH3] (acidifying components)
INTEGER*4, INTENT(IN)                            :: kdeel                      ! index of particle size class
REAL*4,    INTENT(IN)                            :: croutpri                   ! constant (initial) in-cloud scavenging ratio [-] for primary component (rout << rain-out = in-cloud) 
REAL*4,    INTENT(IN)                            :: rations                    ! NH3/SO2 ratio over trajectory
REAL*4,    INTENT(IN)                            :: rnox                       ! NO2/NOx ratio

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: routpri                    ! in-cloud scavenging ratio for primary component

! CONSTANTS
REAL*4                                           :: RORATIO(NPARTCLASS)        ! (geschatte) waarden scavenging ratio per deeltjesklasse

! roratio: scavenging ratio per particle class 
! Estimated from measurements of rainwater concentration and air concentration for different substances.

! See nok/luk reports "Luchtverontreniging t.g.v. de uitworp van kolengestookte installaties"
! Potma C.J., Onderdelinden D. and Slanina J. (1986): bijdrage van een kolengestookte elecktriciteitscentrale
!       aan de lokale luchtconcentratie- en depositiveniveaus. PEO report NOK-LUK 3, no. 20.70-017.10, 
!       RIVM report 22822 02 004, Bilthoven.
! van Jaarsveld en Onderdelinden (1986): Modelmatige beschrijving van concentratie en depositie van kolen relevante 
!       componenten in NL veroorzaakt door emissies in Europa, PEO report NOK-LUK 3, RIVM report 2282 02 002, Bilthoven.
!       See Table III measurements in Lelystad and Bilthoven .
! DATA RORATIO / 120000.,1000000.,5000000.,2*9000000./ aangepast (zie EUTREND) 931120
!
DATA RORATIO/240000., 1000000., 1000000., 5000000., 2*9000000./

IF (isec) THEN

   ! Acidifying componentsL
   IF (icm .EQ. 1) THEN
      
      ! icm = 1: SO2
      routpri = croutpri*rations ! depends on N/S ratio
   
   ELSE IF (icm .EQ. 2) THEN
   
      ! icm = 2: NOx
      routpri = croutpri*rnox  ! depends on NO2/NOx ratio
   
   ELSE IF (icm .EQ. 3) THEN
   
      !   icm = 3: NH3  
      routpri = croutpri
   
   ELSE
      write(*,*) 'internal programming error in ops_resist_rek; isec and icm < 1 or icm > 3'
      write(*,*) icm
      stop
   ENDIF ! IF icm = 1,2 or 3
ELSE
   ! (.not. isec): non-acidifying components
   
   IF (.NOT.gasv) THEN
      ! Particles:
      routpri = RORATIO(kdeel)
   ELSE
      ! Gases, but not acidifying -> routpri retains the value set in ops_init
      CONTINUE   
   ENDIF
ENDIF

END SUBROUTINE ops_resist_rain_out_scav_rate

!----------------------------------------------------------------------------------------
SUBROUTINE ops_resist_rc_sec_trj(icm,ra_ms_4,rb_ms,rc_aer_ms,rc_hno3,rhno3_trj,rc_sec_trj)

! Compute rc_sec_trj: canopy resistance secondary component representative for trajectory

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: icm                        ! component number
REAL*4,    INTENT(IN)                            :: ra_ms_4                    ! aerodynamic resistance at 4 m from meteo statistics [s/m]
REAL*4,    INTENT(IN)                            :: rb_ms                      ! boundary layer resistance from meteo statistics [s/m]
REAL*4,    INTENT(IN)                            :: rc_aer_ms                  ! Rc(SO4-aerosol) from meteo statistics [s/m]
REAL*4,    INTENT(IN)                            :: rc_hno3                    ! canopy resistance for HNO3 [s/m]
REAL*4,    INTENT(IN)                            :: rhno3_trj                      ! ratio [HNO3]/[NO3_totaal] [-]


! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: rc_sec_trj                 !  canopy resistance secondary aerosol (SO4, NO3, NH4) for trajectory [s/m]

! LOCAL VARIABLES
REAL*4                                           :: r                          ! help variable = Ra + Rb

IF (icm .EQ. 1) THEN
   
   ! icm = 1: SO2, secondary component = SO4
   ! rc_sec_trj: canopy resistance secondary aerosol representative for trajectory; taken as 0.8*Rc(SO4_aerosol)
   rc_sec_trj = rc_aer_ms*0.8

ELSE IF (icm .EQ. 2) THEN

   ! icm = 2: NOx, secondary component = NO3
   !
   !            1             [HNO3]/[NO3]_totaal     (1-[HNO3]/[NO3]_totaal)
   ! ------------------- = ----------------------- + ---------------------------- 
   !  Rc(NO3) + Rb + Ra       Rc(HNO3)+ Rb + Ra        Rc(NO3_aerosol) + Rb + Ra      
         
   r          = rb_ms + ra_ms_4                                                 
   rc_sec_trj = 1./(rhno3_trj/(r+rc_hno3) + (1.-rhno3_trj)/(r+rc_aer_ms)) - r   
   
ELSE IF (icm .EQ. 3) THEN

   ! icm = 3: NH3, secondary component = NH4 
   ! rc_sec_trj = 0.8*Rc(SO4_aerosol)
   rc_sec_trj = rc_aer_ms*0.8
ELSE
   write(*,*) 'internal programming error in ops_resist_rc_sec_trj; isec and icm < 1 or icm > 3'
   write(*,*) icm
   stop
ENDIF 

END SUBROUTINE ops_resist_rc_sec_trj

!----------------------------------------------------------------------------------------
SUBROUTINE ops_resist_rcaer (icmpsec, znul, ust, ol, hum, nwet, Uh, ra_ms_4, rb_ms, rc_hno3, rhno3, rc_aer)  

! SUBROUTINE         : ops_resist_rcaer
! DESCRIPTION        : calculation of canopy resistance for aerosols SO4, NO3, NH4 according
!                      to Wesely et al. (1985) for low vegetation and other areas with
!                      a roughness length (znul) < 0.5m and to Erisman et al (1994) and
!                      Ruijgrok et al. (1994) for forest and other areas with a roughness
!                      length above 0.5m.
! AUTHOR             : Hans van Jaarsveld

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER      (ROUTINENAAM = 'ops_resist_rcaer')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: icmpsec                    ! component number (11 = SO4, 12 = NO3, 13 = NH4)
REAL*4,    INTENT(IN)                            :: znul                       ! roughness length [m]
REAL*4,    INTENT(IN)                            :: ust                        ! friction velocity [m/s]
REAL*4,    INTENT(IN)                            :: ol                         ! Monin-Obukhov length [m][m]
REAL*4,    INTENT(IN)                            :: hum                        ! relative humidity [%]
INTEGER*4, INTENT(IN)                            :: nwet                       ! wetness indicator depending on percipitation probability and humidity
REAL*4,    INTENT(IN)                            :: Uh                         ! wind speed used in parametrisation of vd for aerosols [m/s]
REAL*4,    INTENT(IN)                            :: ra_ms_4                    ! aerodynamic resistance at 4 m from meteo statistics [s/m]
REAL*4,    INTENT(IN)                            :: rb_ms                      ! boundary layer resistance SO2 from meteo statistics [s/m]
REAL*4,    INTENT(IN)                            :: rc_hno3                    ! canopy resistance for HNO3 [s/m]
REAL*4,    INTENT(IN)                            :: rhno3                      ! ratio [HNO3]/[NO3_total] [-]

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: rc_aer                     ! canopy resistance aerosols SO4, NO3, NH4 [s/m]

! LOCAL VARIABLES
REAL*4                                           :: E                          ! collecting efficiency
REAL*4                                           :: r                          ! help variable = Ra + Rb [s/m]
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
IF (icmpsec.eq.11) THEN
  IF (hum.le.80) THEN
    IF (nwet.ge.1) THEN
      E = 0.08*ust**0.45
    ELSE
      E = 0.05*ust**0.28
    ENDIF
  ELSE
    IF (nwet.ge.1) THEN
      E = 0.08*ust**0.45*(1+0.37*EXP((hum-80)/20))
    ELSE
      E = 0.05*ust**0.28*(1+0.18*EXP((hum-80)/20))
    ENDIF
  ENDIF
!
! Nitrate:
!
ELSE IF (icmpsec.eq.12) THEN
  IF (hum.le.80) THEN
    IF (nwet.ge.1) THEN
      E = 0.10*ust**0.43
    ELSE
      E = 0.063*ust**0.25
    ENDIF
  ELSE
    IF (nwet.ge.1) THEN
      E = 0.10*ust**0.43*(1+0.37*EXP((hum-80)/20))
    ELSE
      E = 0.063*ust**0.25*(1+0.18*EXP((hum-80)/20))
    ENDIF
  ENDIF
!
! Ammonium:
!
ELSE IF (icmpsec.eq.13) THEN
  IF (hum.le.80) THEN
    IF (nwet.ge.1) THEN
      E = 0.066*ust**0.41
    ELSE
      E = 0.05*ust**0.23
    ENDIF
  ELSE
    IF (nwet.ge.1) THEN
      E = 0.066*ust**0.41*(1+0.37*EXP((hum-80)/20))
    ELSE
      E = 0.05*ust**0.23*(1+0.18*EXP((hum-80)/20))
    ENDIF
  ENDIF
ENDIF

! Calculation of canopy resistance Rc.
! For low vegetation and other areas with a roughness length
! below 0.5m by Wesely et al (1985) (for stable/neutral conditions
! (ol<0) and stable conditions (ol>0)), and for forest and other
! areas with a roughness length above 0.5m by Erisman et al (1994), AI.12, AI.13, AI.14 OPS report
!
! Note: 2020-07-06 - we don't use vd anymore, only rc_aer
!
! Sulphate, nitrate and ammonium
! Note: Rb is neglected for aerosols
!
IF (znul.lt.0.5) THEN
  IF (ol.lt.0) THEN
    rc_aer = 1.0/((ust/500.)*(1.+((300./(-1.*ol))**(2./3.))))  ! 960515
  ELSE
    rc_aer = 1.0/(ust/500)
  ENDIF
ELSE
  rc_aer = 1.0/(E*ust**2/Uh) 
ENDIF

IF (icmpsec .EQ. 12) THEN     

   !  rc_aer is valid for NO3 aerosol. Calculate now a weighted value for the NO3+HNO3 mixture 
   !
   !          1               [HNO3]/[NO3]_totaal     (1-[HNO3]/[NO3]_totaal)
   ! ------------------- = ----------------------- + ---------------------------- 
   !  Rc(NO3) + Rb + Ra       Rc(HNO3)+ Rb + Ra        Rc(NO3_aerosol) + Rb + Ra      

   r      = rb_ms + ra_ms_4  
   rc_aer = 1./(rhno3/(r+rc_hno3) + (1.-rhno3)/(r+rc_aer)) - r

ENDIF

RETURN

END SUBROUTINE ops_resist_rcaer

end module m_ops_resist_rek
