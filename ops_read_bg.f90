!-------------------------------------------------------------------------------------------------------------------------------
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
!                       Copyright (C) 2002 by
!   National Institute of Public Health and Environment
!           Laboratory for Air Research (RIVM/LLO)
!                      The Netherlands
!
! SUBROUTINE
! FILENAME           : %M%
! SCCS(SOURCE)       : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH - SEQUENCE  : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : Hans van Jaarveld/Martien de Haan
! FIRM/INSTITUTE     : RIVM/LLO/IS
! LANGUAGE           : FORTRAN(HP-UX, HP-F77, HP-F90)
! DESCRIPTION        : Handling of background concentrations.
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES:
! CALLED FUNCTIONS   :
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   ops_read_bg
! Purpose      Reads background concentrations for SO2, NO2 and NH3.
!              Called only when isec is set (icm = 1, 2 or 3).
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_read_bg(icm, year, so2bggrid, no2bggrid, nh3bggrid, error)

USE m_aps
USE m_error
USE m_commonconst
USE m_commonfile

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: icm                        
INTEGER*4, INTENT(IN)                            :: year                       ! year under consideration

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TApsGridReal), INTENT(OUT)                 :: so2bggrid                  ! grid with SO2 background concentration [ppb]
TYPE (TApsGridReal), INTENT(OUT)                 :: no2bggrid                  ! grid with NO2 background concentration [ppb]
TYPE (TApsGridReal), INTENT(OUT)                 :: nh3bggrid                  ! grid with NH3 background concentration [ppb]
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: i                          ! column index in grid
INTEGER*4                                        :: j                          ! row index in grid
INTEGER*4                                        :: mapnumber                  ! number of background map
INTEGER*4                                        :: ji                         ! year index, i.e. the index in the trendfactor 
                                                                               ! arrays tf_... of the current year
REAL*4                                           :: factor                     ! combined correction factor (calibration with 
                                                                               ! measurements and correction for year)
LOGICAL*1                                        :: future                     ! TRUE if year is closer to FUTUREYEAR than to last
                                                                               ! historic year
REAL*4                                           :: nox_threshold              ! threshold value for NOx in log-function in NOx -> NO2 conversion
REAL*4                                           :: alpha                      ! slope of linear function NOx -> NO2 conversion


! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_read_bg')

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Get number of map that is the basis for the calculation of the background concentration.
! Maps are, in general, created by previous runs of OPS. 
!
! For each of the components SO2, NOx en NH3, there are 4 maps available; for 3 historic years (1984, 1994, 2005) and for a 
! future year (FUTUREYEAR). The maps for the historic years are corrected by OPS for the deviation between model and measurements.
! For other years than these three historic years, one of these corrected maps is subsequently scaled to the average Dutch
! concentration for that year. This scaling factor (trend factor) is purely determined from measurements. 
! The map with future concentrations is used in case that the simulation year is closer to the future year than to the last year
! for which the average Dutch concentration is known (i.e. FIRSTYEAR + NYEARS - 1):
!
future = (year - FIRSTYEAR - NYEARS + 1) > ABS(FUTUREYEAR - year)
!
! Get map number:
!
mapnumber=0
IF (year < 1990) THEN
  mapnumber=1
ELSEIF (year >= 1990 .AND. year < 2000) THEN
  mapnumber=2
ELSEIF (year >= 2000 .AND. year < 2007) THEN
  mapnumber=3
ELSEIF (year >= 2007 .AND. .not.future) THEN
  mapnumber=4
ELSE
  mapnumber=5
ENDIF
!
! Get index of year for which we need background concentrations
! (index starts at FIRSTYEAR -> 1 and ends at NYEARS; for future years the index = NYEARS+1)
!
ji = year - FIRSTYEAR + 1
IF (ji.LT.1) THEN
  ji = 1
ELSE IF (ji.GT.NYEARS) THEN
  IF (future) THEN
    ji = NYEARS + 1
  ELSE
    ji = NYEARS
  ENDIF
ENDIF
!
! Of which components the background concentration has to be calculated depends on the substance for which the OPS calculation takes place.
! 1: SO2 >> {(NH4)2 SO4}            -> SO2, NH3; 
! 2: NO2 >> {NH4 NO3}               -> NO2, NH3;
! 3: NH3 >> {(NH4)2 SO4}, {NH4 NO3} -> SO2, NO2, NH3
!
IF (icm /= 2) THEN
!
! Read and allocate the background grids for SO2 [ug/m3].
! SO2 background values are not required for icm = 2 (component = NOx).
! 
  CALL read_bg_file(map_so2(mapnumber), 'SO2', so2bggrid, error)
  IF (error%haserror) GOTO 9999
!
! Conversion from ug/m3 to ppb, fit to LML-measurements (cf_so2) and correction for actual year (tf_so2).
! SetAverage multiplies all grid values with factor and computes a grid average.

! Molw(SO2) = 64; 24.04 l is the volume of 1 mole of gas at STP (20 deg C, 1013 mbar)
! concentration_(ppb) = 24.04/ molecular_weight x concentration_(ug/m3) , 
!
  factor = 24./64. * cf_so2(mapnumber) * tf_so2(ji)
  CALL SetAverage(factor, so2bggrid) 
ENDIF

IF (icm /= 1) THEN
!
! Read and allocate the background grids for NOx [ug NO2/m3] to calculate the NO2 background concentration. 
! NO2 background values are not required for icm = 1 (component = SO2).
!
  CALL read_bg_file(map_nox(mapnumber), 'NOx', no2bggrid, error)
  IF (error%haserror) GOTO 9999
!
! First, the NOx background concentration is corrected for the difference between model and measurements (cf_no2). 
! Simultaneously the unit is converted from ug NO2 per m3 to ppb. 
! The latter is done to be able to use the existing empirical relation for NOx --> NO2. 
! Molw(NO2) = 46;  24.04 l is the volume of 1 mole of gas at STP (20 deg C, 1013 mbar)
!
  factor = cf_nox(mapnumber) * 24./46.
  CALL SetAverage(factor, no2bggrid)
!
! Now, the grid with corrected NOx background concentrations (in ppbv) is converted cellwise to NO2 (in ppbv).
! [NO2] = beta1*log([NOx]) + beta2; coefficients are defined in m_commonconst. Tag: NOx-NO2 relation
! Since this function drops below zero for low values of [NOx], a linear function is used for [NOx] <= NOx_threshold ppbv,
! that touches the log-function at the threshold value and is zero for [NOx] = 0 ppbv.
! g(x) = alpha*x, f(x) = beta1*log(x) + beta2.
! First derivative equal at threshold x0: alpha = beta1/x0.
! Function equal at x0: (beta1/x0)*x0 = beta1*log(x0) + beta2 <=> x0 = exp(1-beta2/beta1).
!
! In ops_par_chem, the inverse of this function is used.

  ! Note that at the start of these loops, no2bggrid contains the NOx-concentration
  nox_threshold = exp(1-(nox_no2_beta(2)/nox_no2_beta(1)))
  alpha = nox_no2_beta(1)/nox_threshold
  DO i=1,no2bggrid%gridheader%nrcol
    DO j=1,no2bggrid%gridheader%nrrow
      IF(no2bggrid%value(i,j,1) .GT. nox_threshold) THEN
        no2bggrid%value(i,j,1) = nox_no2_beta(1)*log(no2bggrid%value(i,j,1)) + nox_no2_beta(2)
      ELSE
        no2bggrid%value(i,j,1) = alpha*no2bggrid%value(i,j,1)
      ENDIF
    ENDDO
  ENDDO
  ! Now, no2bggrid contains the NO2-concentration
  
!  
! Now the correction for the actual year (factor tf_no2) is done.
!
  factor = tf_no2(ji)
  CALL SetAverage(factor, no2bggrid)
ENDIF
!
! Read and allocate the background grids for NH3 [ug/m3]. NH3 background values are always required. 
!
CALL read_bg_file(map_nh3(mapnumber), 'NH3', nh3bggrid, error)
IF (error%haserror) GOTO 9999
!
! Conversion from ug/m3 to ppb, fit to LML-measurements (cf_nh3) and correction for actual year (tf_nh3).
! Molw(NH3) = 17; 24.04 l is the volume of 1 mole of gas at STP (20 deg C, 1013 mbar)
!
factor = 24./17. * cf_nh3(mapnumber) * tf_nh3(ji)
CALL SetAverage(factor, nh3bggrid)
! 
RETURN
!
! Error section
!
9999 CALL ErrorCall(ROUTINENAAM, error)
RETURN

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   read_bg_file
! Purpose      Allocates grid and read background concentrations from a file into this grid.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE read_bg_file(filename, compname, bggrid, error)

USE m_string

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: filename                   ! name of file to read background concentration from
CHARACTER*(*), INTENT(IN)                        :: compname                   ! component name for which to read background concentration

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TApsGridReal), INTENT(OUT)                 :: bggrid                     ! background concentration grid
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
CHARACTER*512                                    :: apsfile                    ! full file name of APS-file to read

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'read_bg_file')
!-------------------------------------------------------------------------------------------------------------------------------

IF (error%haserror) GOTO 9999
!
! Determine full name of the file to be read
!
CALL StringMerge(datadir, filename, apsfile, error)
IF (error%haserror) GOTO 9999
!
! Read the file into bggrid.
!
CALL ReadAps(apsfile, 'background concentration grid', bggrid, error)
IF (error%haserror) GOTO 9999

RETURN

9999 CALL ErrorParam('Background component', compname, error)
CALL ErrorCall(ROUTINENAAM, error)
RETURN

END SUBROUTINE read_bg_file

END SUBROUTINE ops_read_bg
