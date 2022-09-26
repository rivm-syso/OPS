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
! DESCRIPTION        : Handling of background concentrations and other maps needed for chemistry.
!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   ops_read_bg
! Purpose      Reads background concentrations for SO2, NO2 and NH3.
!              Reads EMEP grids with column averaged masses and mass converted [ug/m2] used for chemical conversion rate.
!              Called only when isec is set (icm = 1, 2 or 3).
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_read_bg

implicit none

contains

SUBROUTINE ops_read_bg(icm, iopt_vchem, nsubsec, year, chem_meteo_prognosis, so2bggrid, no2bggrid, nh3bggrid, f_subsec_grid, &
                       vchem2, error, dir_chem, fnames_used_chem)

use m_aps
use m_error
use m_commonconst_lt
use m_commonfile
use m_ops_vchem

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: icm                        ! substance index
INTEGER*4, INTENT(IN)                            :: iopt_vchem                 ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
INTEGER*4, INTENT(IN)                            :: nsubsec                    ! number of sub-secondary species                       
INTEGER*4, INTENT(IN)                            :: year                       ! year under consideration
logical                                          :: chem_meteo_prognosis       ! use meteo prognosis in chemistry maps

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TApsGridReal), INTENT(OUT)                 :: so2bggrid                  ! grid with SO2 background concentration [ppb]
TYPE (TApsGridReal), INTENT(OUT)                 :: no2bggrid                  ! grid with NO2 background concentration [ppb]
TYPE (TApsGridReal), INTENT(OUT)                 :: nh3bggrid                  ! grid with NH3 background concentration [ppb]
TYPE (TApsGridReal), INTENT(OUT)                 :: f_subsec_grid              ! grids of fractions for sub-secondary species, HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total [-]
TYPE (Tvchem),       INTENT(INOUT)               :: vchem2                     ! 
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record
CHARACTER(LEN = *), INTENT(OUT)                  :: dir_chem                   ! directory where to read chemistry files from 
CHARACTER(LEN = *), INTENT(OUT)                  :: fnames_used_chem           ! string with names of files used 

! LOCAL VARIABLES
INTEGER*4                                        :: i                          ! column index in grid
INTEGER*4                                        :: j                          ! row index in grid
INTEGER*4                                        :: mapnumber                  ! number of background map
INTEGER*4                                        :: ji                         ! year index, i.e. the index in the trendfactor tf_... of the current year
REAL*4                                           :: factor                     ! combined correction factor (calibration with 
                                                                               ! measurements and correction for year)
REAL*4                                           :: nox_threshold              ! threshold value for NOx in log-function in NOx -> NO2 conversion
REAL*4                                           :: alpha                      ! slope of linear function NOx -> NO2 conversion
INTEGER                                          :: nfield                     ! number of fields in file with NO3-distribution grids (f_subsec_grid)
INTEGER                                          :: ifield                     ! field number in f_subsec_grid

! TYPE (TApsGridReal)                              :: qq                         ! test grid output (for debugging)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_read_bg')

!-------------------------------------------------------------------------------------------------------------------------------
! OPS needs 'chemical maps' with OPS background concentrations and/or EMEP maps with column averaged masses and 
! mass converted [ug/m2] used for chemical conversion rates. There a two 'flavours':
! 1) actual - uses meteo and emission from the actual year
! 2) prognosis - uses emission from the actual year, but 'prognosis meteo', i.e. long-term meteo for OPS, meteo of 2009 for EMEP. 
!    2009 is considered a 'standard' meteo year; long term meteo is not available for EMEP. 
!
! The following files must be present:
!
! 1. OPS,  background concentrations,  actual meteo   : for reference years 1984/1994/2005/2012/2018; LAST_REFERENCE_YEAR = 2018
! 2. OPS,  background concentrations,  prognosis meteo: for every year in LAST_REFERENCE_YEAR - 2030.
! 3. EMEP, masses precursor/converted, actual meteo   : for every year in 2014 - (FUTUREYEAR-1).
! 4. EMEP, masses precursor/converted, prognosis meteo: for every year in LAST_REFERENCE_YEAR - 2030.
! Data for prognosis meteo are computed for LAST_REFERENCE_YEAR and 2030 and linearly interpolated in between.
!
! Get number of map that is the basis for the calculation of the background concentration.
! Maps are, in general, created by previous runs of OPS. 
!
! For each of the components SO2, NOx en NH3, there are 5 historic maps available for reference years 1984, 1994, 2005, 2012, 2018.
! The maps for the reference years are corrected with a correction factor (cf_) for the deviation between model and measurements.
! A) YEAR in the past (year < FUTUREYEAR)
!    For a year in the past, we use the map of the reference year closest to YEAR and perform two scaling operations.
!    1) Scale with a correction (calibration) factor cf_ = C_obs(reference year)/C_model(reference year), with 
!          C_obs   = average Dutch concentration at measuring stations (observed),
!          C_model = average Dutch concentration at measuring stations, computed by OPS. 
!    2) Scale with a trend factor tf_ = C_obs(year)/C_obs(reference year); the trend factor is purely determined from measurements. 
!    Trend factors are available from FIRSTYEAR to FUTUREYEAR-2 (measurements not available for FUTUREYEAR-1) and we assume 
!    tf_(FUTUREYEAR-1) = tf_(FUTUREYEAR-2). 
! 
! B) YEAR in the future (year >= FUTUREYEAR)
!    For each year in the future, correction is the same as the last available correction.
!    For each year in the future, there is a background map available, so the trend factor is not needed (tf_...= 1).

! Set indices for year (map number for reference year, index in trend factor tf_) and set directory for chemical maps:
call set_bg_year_indices(year, chem_meteo_prognosis, dir_chem, mapnumber, ji)

! Initialise:
fnames_used_chem = ''

! Of which components the background concentration has to be read depends on the substance for which the OPS calculation takes place.
! 1: SO2 >> {(NH4)2 SO4}            -> SO2, NH3; 
! 2: NO2 >> {NH4 NO3}               -> NO2, NH3;
! 3: NH3 >> {(NH4)2 SO4}, {NH4 NO3} -> SO2, NO2, NH3
IF (icm /= 2) THEN

  ! Read and allocate the background grids for SO2 [ug/m3].
  ! SO2 background values are not required for icm = 2 (component = NOx)
  call read_bg_file(year, 'so2', 'background concentration', dir_chem, map_so2(mapnumber), fnames_used_chem, so2bggrid, error)
  IF (error%haserror) GOTO 9999

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
  call read_bg_file(year, 'nox', 'background concentration', dir_chem, map_nox(mapnumber), fnames_used_chem, no2bggrid, error)
  IF (error%haserror) GOTO 9999

! First, the NOx background concentration is corrected for the difference between model and measurements (cf_nox). 
! Simultaneously the unit is converted from ug NO2 per m3 to ppb. 
! The latter is done to be able to use the existing empirical relation for NOx --> NO2. 
! Molw(NO2) = 46;  24.04 l is the volume of 1 mole of gas at STP (20 deg C, 1013 mbar)
!
  factor = cf_nox(mapnumber) * 24./46.
  CALL SetAverage(factor, no2bggrid)
!
! Now, the grid with corrected NOx background concentrations (in ppbv) is converted cellwise to NO2 (in ppbv).
! [NO2] = beta1*log([NOx]) + beta2; coefficients are defined in m_commonconst_lt. Tag: NOx-NO2 relation
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
  
  ! Now the correction for the actual year (factor tf_no2) is done:
  factor = tf_no2(ji)
  CALL SetAverage(factor, no2bggrid)
ENDIF

! Read and allocate the background grids for NH3 [ug/m3]. NH3 background values are always required:
call read_bg_file(year, 'nh3', 'background concentration', dir_chem, map_nh3(mapnumber), fnames_used_chem, nh3bggrid, error)
IF (error%haserror) GOTO 9999

! Conversion from ug/m3 to ppb, fit to LML-measurements (cf_nh3) and correction for actual year (tf_nh3).
! Molw(NH3) = 17; 24.04 l is the volume of 1 mole of gas at STP (20 deg C, 1013 mbar)
factor = 24./17. * cf_nh3(mapnumber) * tf_nh3(ji)
CALL SetAverage(factor, nh3bggrid)

! iopt_vchem = 1 -> read EMEP grids with column averaged masses and mass converted [ug/m2] used for chemical conversion rate vchem:
if (iopt_vchem .eq. 1) then

   ! Read MASS_PRE for this year from file 'xxx_mass_prec_yyyy.ops'; xxx = name primary species (SO2, NOx, NH3), yyyy = year (e.g. 2019):
   CALL read_bg_file(year, CNAME(icm,1), 'mass precursor', dir_chem, map_mass_prec, fnames_used_chem, vchem2%mass_prec_grid, error)
   if (error%haserror) GOTO 9999
 
   call SetAverage(grid = vchem2%mass_prec_grid) 
 
   ! Read MASS_CONV_DTFAC for this year:
   CALL read_bg_file(year, CNAME(icm,1), '(100/dt) * mass converted chemistry', dir_chem, map_mass_conv_dtfac, fnames_used_chem, vchem2%mass_conv_dtfac_grid, error)
   if (error%haserror) GOTO 9999
 
   call SetAverage(grid = vchem2%mass_conv_dtfac_grid)  
 
   ! write(*,*) 'average of mass_prec_grid: ', vchem2%mass_prec_grid%average            
   ! write(*,*) 'average of mass_conv_dtfac_grid: ', vchem2%mass_conv_dtfac_grid%average
   ! write(*,*) 'average conversion rate [%/h]: ', vchem2%mass_conv_dtfac_grid%average/vchem2%mass_prec_grid%average 
   
   ! Read distribution maps for NO3_total: HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total;
   ! from file 'no3_distr_yyyy.ops'; yyyy = year (e.g. 2019)
   if (icm .eq. 2) then

      ! Read fractions for sub-secondary species:
      CALL read_bg_file(year, CNAME(icm,1), 'fractions of NO3', dir_chem, map_no3_distr, fnames_used_chem, f_subsec_grid, error)
      if (error%haserror) GOTO 9999
      
      ! Get number of fields in f_subsec_grid; should be equal to nsubsec-1 
      ! (3 fields HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total; 4 sub species NO3_aerosol, HNO3, NO3_C, NO3_F)
      nfield = size(f_subsec_grid%value,3)
      if (nfield .ne. nsubsec-1) then
         write(*,'(/,/,a)') 'internal programming error'
         write(*,'(a,a)')   'incorrect number of fields in last file in ',trim(fnames_used_chem)
         write(*,'(a,i6)')  'number of fields read: ',nfield
         write(*,'(a,i6)')  'number of sub species: ',nsubsec
         write(*,'(a)')     'number of fields must be equal to number of sub species - 1: '
         stop
      endif

      ! Set average of grid (is used in ops_bgcon for missing (negative) values or values outside grid):
      do ifield = 1,nfield
         call SetAverage(grid = f_subsec_grid, fieldnumber = ifield)
         ! write(*,*) 'average of grid of secondary component ',ifield,' = ',f_subsec_grid%average(ifield) 
      enddo
   endif

   ! ! START TEST write to APS file --------------------------------------------------------------------------------------------
   !  qq%gridheader%xorgl = 1000*vchem2%mass_prec_grid(1)%gridheader%xorgl
   !  qq%gridheader%yorgl = 1000*vchem2%mass_prec_grid(1)%gridheader%yorgl
   !  qq%gridheader%grixl = 1000*vchem2%mass_prec_grid(1)%gridheader%grixl
   !  qq%gridheader%griyl = 1000*vchem2%mass_prec_grid(1)%gridheader%griyl
   !  qq%gridheader%nrcol = vchem2%mass_prec_grid(1)%gridheader%nrcol
   !  qq%gridheader%nrrow = vchem2%mass_prec_grid(1)%gridheader%nrrow
   !  allocate(qq%value(qq%gridheader%nrcol, qq%gridheader%nrrow, 1))
   !  qq%value = vchem2%mass_conv_dtfac_grid(1)%value/vchem2%mass_prec_grid(1)%value
   !  write(*,*) 'grid for conversion factor'
   !  open(unit = 34, file = 'cvr_tst1.aps')
   !  !
   !  !
   !  !  character*(*)     coord_sys       ! coordinate system, either 'RDM' or 'lon-lat'
   !  !  integer           lu
   !  !  real              xul_cell_centre, yul_cell_centre
   !  !  real              gridx,gridy
   !  !  integer           matx,maty
   !  !  integer           ijg,img,idg,iug
   !  !  real*4            cpri(matx,maty)
   !  !  character*8       unit_conc
   !  !  character*10      namco
   !  !  character*10      modversie
   !  !  character*12      kname
   !  !  character*(*)     namegr          ! name of grid file (used for error message)
   !  !
   !  !  character*12      quantity
   !  !subroutine saveaps(coord_sys,lu,namegr,xul_cell_centre,yul_cell_centre,gridx,gridy,matx,maty,cpri,namco,unit_conc,modversie,kname,quantity,ijg,img,idg,iug)
   !  call saveaps('RDM',34,'qq0',qq%gridheader%xorgl,qq%gridheader%yorgl,qq%gridheader%grixl,qq%gridheader%griyl,qq%gridheader%nrcol,qq%gridheader%nrrow,qq%value(:,:,1),'conv_rate ','%/h     ','OPS_tst   ','qq1         ','qq2         ',10,0,0,0)
   !  close(34)
   !  !!     TYPE TGridHeader
   
   !  !!    REAL*4                                        :: xul_cell_centre            ! x-coordinate of centre of upper-left grid cell [m]
   !  !!    REAL*4                                        :: yul_cell_centre            ! y-coordinate of centre of upper-left grid cell [m]
   !  !!    INTEGER*4                                     :: nrcol                      ! number of grid columns
   !  !!    INTEGER*4                                     :: nrrow                      ! number of grid rows
   !  !!    REAL*4                                        :: grixl                      ! horizontal size of grid cell [km]
   !  !!    REAL*4                                        :: griyl                      ! vertical size of grid cell [km]
   !  !! END TYPE TGridHeader
   ! ! END TEST write to APS file --------------------------------------------------------------------------------------------
   
   IF (error%haserror) GOTO 9999
endif

RETURN
!
! Error section
!
9999 CALL ErrorCall(ROUTINENAAM, error)
RETURN

END SUBROUTINE ops_read_bg

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   read_bg_file
! Purpose      Define file name, allocates grid and read background concentrations from file
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE read_bg_file(year, compname, gridtitle, dir_chem, fnam, fnames_used_chem, bggrid, error)

use m_string
use m_aps
use m_error

! SUBROUTINE ARGUMENTS - INPUT
INTEGER      , INTENT(IN)                        :: year                       ! year for chemical maps
CHARACTER*(*), INTENT(IN)                        :: compname                   ! component name for which to read background concentration (used in error message only)
CHARACTER*(*), INTENT(IN)                        :: gridtitle                  ! description of grid shown in error messages
CHARACTER*(*), INTENT(IN)                        :: dir_chem                   ! directory where to read chemistry files from 
CHARACTER*(*), INTENT(IN)                        :: fnam                       ! name of file to read background concentration from (possible xxx=compnae, yyyy = year)

! SUBROUTINE ARGUMENTS - INPUT/OUTPUT
CHARACTER*(*), INTENT(INOUT)                     :: fnames_used_chem           ! string with names of file used (with xxx and yyyy replaced by actual values)

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TApsGridReal), INTENT(OUT)                 :: bggrid                     ! background concentration grid
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER                                          :: i1                         ! index of substring in filename
CHARACTER*512                                    :: fnam_used                  ! name of file used (with xxx and yyyy replaced by actual values)
CHARACTER*512                                    :: apsfile                    ! full file name of APS-file to read

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'read_bg_file')
!-------------------------------------------------------------------------------------------------------------------------------

IF (error%haserror) GOTO 9999

! Replace 'xxx' (if present) with component name: 
fnam_used = fnam
i1 = index(fnam_used,'xxx');
if (i1 > 0) write(fnam_used(i1:i1+2),'(A3)') compname

! Replace yyyy (if present) in file name with value of year:
i1 = index(fnam_used,'yyyy');
if (i1 > 0) write(fnam_used(i1:i1+3),'(I4)') year

! Append name of used file:
fnames_used_chem = trim(fnames_used_chem) // ' ' // trim(fnam_used) 

! Merge dir_chem and fnam (Note: MakeCommonpath cannot be used because of different datadir):
CALL StringMerge(trim(dir_chem), trim(fnam_used), apsfile, error)
IF (error%haserror) GOTO 9998
IF (.NOT.chkexist(apsfile, error)) GOTO 9998

! Read the file into bggrid:
CALL ReadAps(apsfile, gridtitle, bggrid, error)
IF (error%haserror) GOTO 9999

RETURN

9998 CALL ErrorParam('Grid', gridtitle, error)
9999 CALL ErrorParam('Component', compname, error)
CALL ErrorCall(ROUTINENAAM, error)

RETURN

END SUBROUTINE read_bg_file

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   set_bg_year_indices
! Purpose      Set indices for YEAR (map number for reference year, index in trend factor tf_)
!-------------------------------------------------------------------------------------------------------------------------------

subroutine set_bg_year_indices(year, chem_meteo_prognosis, dir_chem, mapnumber, ji)

use m_commonconst_lt, only: FUTUREYEAR, FIRSTYEAR, NYEARS, NBGMAPS
use m_commonfile, only: datadir, dir_chem_meteo
use m_utils, only: GetOS

INTEGER*4, INTENT(IN)                             :: year                       ! year for chemical maps
LOGICAL, INTENT(IN)                               :: chem_meteo_prognosis       ! use meteo prognosis in chemistry maps

CHARACTER*(*), INTENT(OUT)                        :: dir_chem                   ! directory where to read chemistry files from 
INTEGER*4, INTENT(OUT)                            :: mapnumber                  ! number of background map
INTEGER*4, INTENT(OUT)                            :: ji                         ! year index, i.e. the index in the trendfactor tf_... of the current year

LOGICAL*1                                         :: future                     ! year >= FUTUREYEAR
INTEGER*4                                         :: os                         ! index for Operating system (0 = Unix, 1 = Windows)
CHARACTER(LEN=1)                                  :: slash                      ! slash for directory separation

! Get number of map that is the basis for the calculation of the background concentration.
! See documentation in calling routine.

! Get slash:
CALL GetOS(os, slash)

! Check consistency:
if (FIRSTYEAR+NYEARS .ne. FUTUREYEAR-1) then
   write(*,*) 'Internal programming error in read_ctr_year'
   write(*,*) 'FIRSTYEAR  = ',FIRSTYEAR
   write(*,*) 'NYEARS     = ',NYEARS
   write(*,*) 'FUTUREYEAR = ',FUTUREYEAR
   stop
endif

! Define future:
future = (year .ge. FUTUREYEAR)

! Set data directory and map number; see file names map_so2, map_nox, map_nh3 in m_commonfile:
if (chem_meteo_prognosis) then

   ! Data directory and map number for chemical files with prognosis meteo; 
   dir_chem = trim(datadir)//trim(dir_chem_meteo(2))//slash
   mapnumber = NBGMAPS+1 ! future map and correction factor
else
   ! Data directory for chemical files with actual meteo:
   dir_chem = trim(datadir)//trim(dir_chem_meteo(1))//slash

   ! Map nunber:   
   IF (year < 1990) THEN
     mapnumber = 1  ! 1984
   ELSEIF (year >= 1990 .AND. year < 2000) THEN
     mapnumber = 2  ! 1994
   ELSEIF (year >= 2000 .AND. year < 2007) THEN
     mapnumber = 3  ! 2005
   ELSEIF (year >= 2007 .AND. year < 2014) THEN
     mapnumber = 4  ! 2012
   ELSEIF (year >= 2014 .AND. year < FUTUREYEAR) THEN
     mapnumber = 5  ! 2018
   ELSE
     ! Year in the future (year .ge. FUTUREYEAR);
     ! define data directory and map number for chemical files with prognosis meteo:
     dir_chem  = trim(datadir)//trim(dir_chem_meteo(2))//slash
     mapnumber = NBGMAPS+1
   ENDIF
endif

! Get index of year for which we need background concentrations; needed for trend factor tf_.
! Note: 1 -> FIRSTYEAR, 2 -> FIRSTYEAR+1, NYEARS -> FIRSTYEAR+NYEARS-1 = FUTUREYEAR-2
! For prognosis or future years (year >= FUTUREYEAR), background maps are already linearly interpolated in time, 
!    so a trendfactor is not needed (tf_(NYEARS+1) = 1.0).
! For FUTUREYEAR-1, we use tf_(NYEARS) = trendfactor for FUTUREYEAR-2, because measurements are not yet available for FUTUREYEAR-1 and
!    the trendfactor for FUTUREYEAR-2 is the best estimate for the trend in emissions.
if (chem_meteo_prognosis .or. (year .ge. FUTUREYEAR)) then
   ! tf_(NYEARS+1) = 1.0
   ji = NYEARS+1
elseif (year .eq. FUTUREYEAR-1) then
   ! year = FUTUREYEAR-1 -> ji = NYEARS -> 'last valid trend factor'
   ji = NYEARS
else
   ji = max(1,year - FIRSTYEAR + 1)
endif

end subroutine set_bg_year_indices

end module m_ops_read_bg
