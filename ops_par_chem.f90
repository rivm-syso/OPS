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
! DESCRIPTION        : Get chemical parameters (conversion rates, concentration ratios).
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES:
! CALLED FUNCTIONS   :
! UPDATE HISTORY :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_par_chem (icm, iopt_vchem, isek, so2sek, no2sek, so2bgtra, no2bgtra, nh3bgtra, vchem2, disx, diameter, vchemnh3, rhno3,            &
                      &  rrno2nox, rations)

USE m_commonconst
USE m_aps
USE m_ops_vchem

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_par_chem')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: icm  
INTEGER*4, INTENT(IN)                            :: iopt_vchem                 ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)                      
INTEGER*4, INTENT(IN)                            :: isek                        
REAL*4,    INTENT(IN)                            :: so2sek(NSEK)               
REAL*4,    INTENT(IN)                            :: no2sek(NSEK)               
REAL*4,    INTENT(IN)                            :: so2bgtra                    
REAL*4,    INTENT(IN)                            :: no2bgtra                    
REAL*4,    INTENT(IN)                            :: nh3bgtra                    
type(Tvchem),    INTENT(INOUT)                   :: vchem2
REAL*4,    INTENT(IN)                            :: disx                       
REAL*4,    INTENT(IN)                            :: diameter              

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: vchemnh3
REAL*4,    INTENT(OUT)                           :: rhno3                       
REAL*4,    INTENT(OUT)                           :: rrno2nox                   
REAL*4,    INTENT(OUT)                           :: rations                    

! LOCAL VARIABLES
REAL*4                                           :: C1                         ! 
REAL*4                                           :: C2                         ! 
REAL*4                                           :: ch                         ! 
REAL*4                                           :: cr                         ! 
REAL*4                                           :: wdc_so2                    ! 
REAL*4                                           :: wdc_no2                    ! 
REAL*4                                           :: so2bgtra_corr              ! 
REAL*4                                           :: no2bgtra_corr              ! 
REAL*4                                           :: nh3bgtra_corr              ! 
REAL*4                                           :: nox_threshold              ! threshold value for NOx in log-function in NOx -> NO2 conversion
REAL*4                                           :: no2_threshold              ! threshold value for NO2 in exp-function in NO2 -> NOx conversion
REAL*4                                           :: alpha                      ! slope of linear function NOx -> NO2 conversion
REAL*4                                           :: noxbgtra_corr              ! conversion of no2bgtra_corr to NOx

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'// char (0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Compute correction factors for SO2 and NO2 background concentration for each wind direction sector;
! derived from regional LML stations over 2003 (SO2) or 2004 (NO2) (eastern wind -> higher SO2 and NO2)
! if the receptor is inside an area source (0 < cr < 1), there are contributions of all directions and 
! the wind sector correction is not done (cr = 0, receptor in centre of area source -> wdc_ = 1) or
! maximal (cr = 1 -> edge of area source -> wdc_ = so2sek)
!  
IF (diameter > 0 ) THEN
  cr      = amin1(disx/diameter,1.)
  wdc_so2 = cr*so2sek(isek)+ (1.-cr)
  wdc_no2 = cr*no2sek(isek)+ (1.-cr)
ELSE
  wdc_so2 = so2sek(isek)
  wdc_no2 = no2sek(isek)
ENDIF
!
! Correct trajectory averaged background concentration as function of wind direction.
! NH3 is not corrected, because for NH3 the background is more locally determined.  
!
so2bgtra_corr = so2bgtra*wdc_so2
no2bgtra_corr = no2bgtra*wdc_no2
nh3bgtra_corr = nh3bgtra

IF (icm == 1) THEN
   
   ! SO2
   ! compute N/S ratio; factor 2 is because of (NH4) (SO4)
   rations = nh3bgtra_corr/(2*so2bgtra_corr)

ELSE
   IF (icm == 2) THEN
   
      ! NOx
      ! rhno3 = ratio [HNO3]/[NO3]_total (NO3_total = HNO3+NO3_aerosol)
      ! This ratio has been computed with the help of a 1D chemistry model (model chemie5)
      ! (fit between daily averaged HNO3 and NO3 concentrations for november and december 1989);
      ! see also ops_rcp_char.
      ! Here we use the trajectory averaged, wind sector corrected background NH3 concentration.
        rhno3 = amin1(0.024*(nh3bgtra_corr/1000)**(-0.44),0.8)
      
      ! rrno2nox is the spatially variable component in the [NO2]/[NOx] ratio, using an average [NO2]/[NOx] ratio
      ! in NL equal to 0.65 (see also ops_init).
      ! This empirical [NO2]/[NOx] ratio follows from a fit of measured yearly averaged concentrations in NL (1993).
      
      !
      ! In ops_read_bg, the grid with corrected NOx background concentrations (in ppbv) is converted cellwise to NO2 (in ppbv).
      ! [NO2] = beta1*log([NOx]) + beta2; coefficients are defined in m_commonconst. Tag: NOx-NO2 relation
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
      rrno2nox=no2bgtra_corr/(0.65*noxbgtra_corr)

   ELSE
         
      ! icm = 3, NH3
      ! Compute conversion rate NH3 -> NH4;
      ! ch = [SO2]/[NH3]
      
      ! note that 1.7*[0.1 0.8 6.3 1.8 -0.17]  =  [0.17    1.36   10.71    3.06   -0.29] 
      
      ! Chemistry model computes hourly concentrations for one column (including emissions, deposition);
      ! then relations between different components are derived.
     
      ch       = amin1(so2bgtra_corr/nh3bgtra_corr,3.0)
      vchemnh3 = 0.1 + 0.8*no2bgtra_corr/nh3bgtra_corr + 6.3*ch + 1.8*ch**4 - 0.17*ch**6 
      vchemnh3 = amax1(1.0,vchemnh3*3.0+0.5) ! calibration to bulk measurements (yearly averaged NH3/NH4 ratios)
   ENDIF
ENDIF

! Compute chemical conversion rates [%/h] from averaged mass pre chemistry and mass converted during time step (EMEP option iopt_vchem = 1):
IF ((icm == 1 .or. icm == 2 .or. icm == 3) .and. iopt_vchem .eq. 1) THEN
   vchem2%vchem = vchem2%mass_conv_dtfac_tra/vchem2%mass_prec_tra ! note: factor (100.0/dt) is already in mass_conv_dtfac_tra 
ENDIF
    
RETURN
END SUBROUTINE ops_par_chem
