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
! DESCRIPTION        : Get chemical parameters (conversion rates, concentration ratios).
!                      Chemical conversion rate here does not depend on meteo class,
!                      see also ops_vchem for conversion rates that depend on meteo class.

!-------------------------------------------------------------------------------------------------------------------------------

module m_ops_par_chem

implicit none

contains

SUBROUTINE ops_par_chem (icm, isec, iopt_vchem, isec_prelim, so2sek, no2sek, so2bgtra, no2bgtra, &
           nh3bgtra, o3bgtra, mass_prec_tra, mass_conv_dtfac_tra, disx, diameter, &
           varin_unc, vchem, vchemnh3, rhno3_trj, r_no2_nox_year_bg_tra, rations)

use m_commonconst_lt
USE m_aps
USE m_ops_varin, ONLY: Tvarin_unc
USE m_ops_vchem

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_par_chem')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: icm                        ! component index
LOGICAL,   INTENT(IN)                            :: isec                       ! TRUE if component=[SO2, NOx, NH3]
INTEGER,   INTENT(IN)                            :: iopt_vchem                 ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)                      
INTEGER,   INTENT(IN)                            :: isec_prelim                ! index of preliminary source-receptor wind sector (wind shear not yet taken into account)
REAL,      INTENT(IN)                            :: so2sek(NSEK)               
REAL,      INTENT(IN)                            :: no2sek(NSEK)               
REAL,      INTENT(IN)                            :: so2bgtra                    
REAL,      INTENT(IN)                            :: no2bgtra                    
REAL,      INTENT(IN)                            :: nh3bgtra                    
REAL,      INTENT(IN)                            :: o3bgtra(NSEK)                    
real,      intent(in)                            :: mass_prec_tra, mass_conv_dtfac_tra 

REAL,      INTENT(IN)                            :: disx                       
REAL,      INTENT(IN)                            :: diameter              

TYPE(Tvarin_unc), INTENT(IN)                     :: varin_unc

! SUBROUTINE ARGUMENTS - INPUT/OUTPUT
REAL,      INTENT(INOUT)                         :: vchem                      ! chemical conversion rate (only set here for option EMEP, see ops_vchem for other options)
REAL,      INTENT(INOUT)                         :: vchemnh3                   ! chemical conversion rate for NH3 (old parameterisation)
REAL,      INTENT(INOUT)                         :: r_no2_nox_year_bg_tra      ! component of NO2/NOx ratio based on yearly averaged background concentrations over a trajectory              
REAL,      INTENT(INOUT)                         :: rations                    ! NH3/SO2 ratio over trajectory                   

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: rhno3_trj                  ! HNO3/NO3-total ratio for trajectory [-]     

! LOCAL VARIABLES
REAL                                             :: ch                         ! 
REAL                                             :: cr                         ! 
REAL                                             :: wdc_so2                    ! 
REAL                                             :: wdc_no2                    ! 
REAL                                             :: so2bgtra_corr              ! SO2 background concentration x wind sector correction
REAL                                             :: no2bgtra_corr              ! NO2 background concentration x wind sector correction
REAL                                             :: nh3bgtra_corr              ! NH3 background concentration x wind sector correction (= 1)
REAL                                             :: o3bgtra_corr               ! O3 background concentration for current wind sector
REAL                                             :: nox_threshold              ! threshold value for NOx in log-function in NOx -> NO2 conversion
REAL                                             :: no2_threshold              ! threshold value for NO2 in exp-function in NO2 -> NOx conversion
REAL                                             :: alpha                      ! slope of linear function NOx -> NO2 conversion
REAL                                             :: noxbgtra_corr              ! conversion of no2bgtra_corr to NOx

!-------------------------------------------------------------------------------------------------------------------------------

! Only for SO2, NOx, NH3:
IF (isec) THEN

   ! Compute correction factors for SO2 and NO2 background concentration for each wind direction sector;
   ! derived from regional LML stations over 2003 (SO2) or 2004 (NO2) (eastern wind -> higher SO2 and NO2)
   ! if the receptor is inside an area source (0 < cr < 1), there are contributions of all directions and 
   ! the wind sector correction is not done (cr = 0, receptor in centre of area source -> wdc_ = 1) or
   ! maximal (cr = 1 -> edge of area source -> wdc_ = so2sek)
   IF (diameter > 0 ) THEN
     cr      = amin1(disx/diameter,1.)
     wdc_so2 = cr*so2sek(isec_prelim)+ (1.-cr)
     wdc_no2 = cr*no2sek(isec_prelim)+ (1.-cr)
   ELSE
     wdc_so2 = so2sek(isec_prelim)
     wdc_no2 = no2sek(isec_prelim)
   ENDIF
   
   ! Correct trajectory averaged background concentration as function of wind direction.
   ! NH3 is not corrected, because for NH3 the background is more locally determined.  
   ! For O3, we just get the ozone concentration for the actual wind sector.
   nh3bgtra_corr = nh3bgtra
   IF (ANY(icm == (/icm_NOx,icm_NH3/))) then
      no2bgtra_corr = no2bgtra*wdc_no2
      o3bgtra_corr  = o3bgtra(isec_prelim)  
   ENDIF
   
   IF (icm == icm_SO2) THEN
      
      ! SO2
      ! compute N/S ratio; factor 2 is because of (NH4)_2 (SO4)
      so2bgtra_corr = so2bgtra*wdc_so2
      rations = nh3bgtra_corr/(2*so2bgtra_corr)
   
   ELSE IF (icm == icm_NOx) THEN
      
      ! NOx
      ! rhno3_trj = ratio [HNO3]/[NO3]_total (NO3_total = HNO3+NO3_aerosol) for trajectory
      ! This ratio has been computed with the help of a 1D chemistry model (model chemie5)
      ! (fit between daily averaged HNO3 and NO3 concentrations for november and december 1989);
      ! see also ops_rcp_char.
      ! Here we use the trajectory averaged, wind sector corrected background NH3 concentration.
        rhno3_trj = amin1(0.024*(nh3bgtra_corr/1000)**(-0.44),0.8)
      
      ! r_no2_nox_year_bg_tra is the spatially variable component in the [NO2]/[NOx] ratio, based on 
      ! yearly averaged background concentrations and using an average [NO2]/[NOx] ratio in NL equal to 0.65
      ! (see also ops_init). Average over a trajectory (intermediate points between source and receptor).
      ! This empirical [NO2]/[NOx] ratio follows from a fit of measured yearly averaged concentrations in NL (1993).
      
      !
      ! In ops_read_bg, the grid with corrected NOx background concentrations (in ppbv) is converted cellwise to NO2 (in ppbv).
      ! [NO2] = beta1*log([NOx]) + beta2; coefficients are defined in m_commonconst_lt. Tag: NOx-NO2 relation
      ! Since this function drops below zero for low values of [NOx], a linear function is used for [NOx] <= NOx_threshold ppbv,
      ! that touches the log-function at the threshold value and is zero for [NOx] = 0 ppbv.
      ! g(x) = alpha*x, f(x) = beta1*log(x) + beta2.
      ! First derivative equal at threshold x0: alpha = beta1/x0.
      ! Function equal at x0: (beta1/x0)*x0 = beta1*log(x0) + beta2 <=> x0 = exp(1-beta2/beta1).
      !
      ! Here we need the inverse function of this function:
      ! NO2_threshold = alpha*x0 = beta1
      ! NO2  > NO2_threshold -> [NOx] = exp(([NO2]-beta2)/beta1)
      ! NO2 <= NO2_threshold -> [NOx] = [NO2]/alpha
      !
      nox_threshold = exp(1-(nox_no2_beta(2)/nox_no2_beta(1)))
      no2_threshold = nox_no2_beta(1)
      alpha = nox_no2_beta(1)/nox_threshold
      IF (no2bgtra_corr .GT. no2_threshold) THEN
         noxbgtra_corr = exp((no2bgtra_corr-nox_no2_beta(2))/nox_no2_beta(1))
      ELSE
         noxbgtra_corr = no2bgtra_corr/alpha
      ENDIF
      r_no2_nox_year_bg_tra = no2bgtra_corr/(0.65*noxbgtra_corr)
      ! write(*,'(a,1x,i6,99(1x,e12.5))') 'isek,disx,no2bgtra,wdc_no2,no2bgtra_corr, noxbgtra_corr: ', isek,disx,no2bgtra,wdc_no2,no2bgtra_corr,noxbgtra_corr
   
   ELSE IF (icm==icm_NH3) then
         
      ! icm = icm_NH3, NH3
      ! Compute conversion rate NH3 -> NH4;
      ! ch = [SO2]/[NH3]
      
      ! note that 1.7*[0.1 0.8 6.3 1.8 -0.17]  =  [0.17    1.36   10.71    3.06   -0.29] 
      
      ! Chemistry model computes hourly concentrations for one column (including emissions, deposition);
      ! then relations between different components are derived.
     
      so2bgtra_corr = so2bgtra*wdc_so2
      ch       = amin1(so2bgtra_corr/nh3bgtra_corr,3.0)
      vchemnh3 = 0.1 + 0.8*no2bgtra_corr/nh3bgtra_corr + 6.3*ch + 1.8*ch**4 - 0.17*ch**6 
      vchemnh3 = amax1(1.0,vchemnh3*3.0+0.5) ! calibration to bulk measurements (yearly averaged NH3/NH4 ratios)
   ENDIF
   
   ! Compute chemical conversion rates [%/h] from averaged mass pre chemistry and mass converted during time step (EMEP option iopt_vchem = 1);
   ! for acidifying components (isec = TRUE):
   IF (iopt_vchem .eq. 1) THEN
      vchem = mass_conv_dtfac_tra/mass_prec_tra ! note: factor (100.0/dt) is already in mass_conv_dtfac_tra 

      ! Adjustment relevant for sensitivity analyses. Multiplication factor is 1.0 by default.
      vchem = varin_unc%unc_sourcedepl%vchem_fact * vchem
   ENDIF
!ELSE
   ! vchem is set in ops_vchem as vchem = vchemc + vchemv*rad_W_m2 and rad_W_m2 depends on meteo class
ENDIF

RETURN
END SUBROUTINE ops_par_chem

end module m_ops_par_chem
