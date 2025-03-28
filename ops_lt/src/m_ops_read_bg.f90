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
! Purpose      Reads background concentrations for SO2, NO2, NH3 and O3.
!              Reads EMEP grids with column averaged masses and mass converted [ug/m2] used for chemical conversion rate.
!              Called only when isec is set (icm = icm_SO2, icm_NOx or icm_NH3).
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_read_bg

implicit none

contains

SUBROUTINE ops_read_bg( &
   icm, iopt_vchem, nsubsec, year, chem_meteo_prognosis, nemcat_road, road_chem, dir_bg, &
   so2bggrid, no2bggrid, nh3bggrid, o3bggrid, gwgrid, f_subsec_grid, vchem_emep, &
   error, dir_chem, fnames_used_chem, dir_bg_actual)

use m_aps
use m_error
use m_commonconst_lt
use m_commonfile
use m_ops_vchem

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: icm                        ! substance index
INTEGER,   INTENT(IN)                            :: iopt_vchem                 ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
INTEGER,   INTENT(IN)                            :: nsubsec                    ! number of sub-secondary species
INTEGER,   INTENT(IN)                            :: year                       ! year under consideration
LOGICAL,   INTENT(IN)                            :: chem_meteo_prognosis       ! use meteo prognosis in chemistry maps
INTEGER,   INTENT(IN)                            :: nemcat_road                ! number of road emission categories (for vdHout NO2/NOx ratio)
LOGICAL,   INTENT(IN)							 :: road_chem					 !switch for road chemistry GTHO
character(len=*), intent(in) :: dir_bg  ! Directory containing background maps.

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TApsGridReal), INTENT(OUT)                 :: so2bggrid                  ! grid with SO2 background concentration [ppb] (read as ug/m3, converted to ppb)
TYPE (TApsGridReal), INTENT(OUT)                 :: no2bggrid                  ! grid with NO2 background concentration [ppb] (read as ug/m3, converted to ppb)
TYPE (TApsGridReal), INTENT(OUT)                 :: nh3bggrid                  ! grid with NH3 background concentration [ppb] (read as ug/m3, converted to ppb)
TYPE (TApsGridReal), INTENT(OUT)                 :: gwgrid                     ! grid with gamma water values

TYPE (TApsGridReal), INTENT(OUT)                 :: o3bggrid                   ! grids with O3 background concentration per wind sector [ug/m3]
TYPE (TApsGridReal), INTENT(OUT)                 :: f_subsec_grid              ! grids of fractions for sub-secondary species, HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total [-]
TYPE (Tvchem),       INTENT(INOUT)               :: vchem_emep                 ! grids with EMEP precursor mass and converted mass for computing chemical conversion rates
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record
CHARACTER(LEN = *), INTENT(OUT)                  :: dir_chem                   ! directory where to read chemistry files from
CHARACTER(LEN = *), INTENT(OUT)                  :: fnames_used_chem           ! string with names of files used
character(len=*), intent(out) :: dir_bg_actual  ! The directory from which background maps are read.

! LOCAL VARIABLES
INTEGER                                          :: mapnumber                  ! number of background map
INTEGER                                          :: ji                         ! year index, i.e. the index in the trendfactor tf_... of the current year
INTEGER                                          :: nfield                     ! number of fields in file with NO3-distribution grids (f_subsec_grid)
INTEGER                                          :: ifield                     ! field number in f_subsec_grid
CHARACTER(LEN = 512)                             :: gwfile                     ! full file name of gamma water map
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
! 3. EMEP, masses precursor/converted, actual meteo   : for every year in ... -  (FUTUREYEAR-1).
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

! If no background directory was set, point it to the pre-corrected background maps within the current data directory.
dir_bg_actual = dir_bg
if (len(trim(dir_bg)) == 0) then
   ! dir_bg_actual = trim(datadir) // "Chem_meteo_corrected"

   if (chem_meteo_prognosis) then
      ! Use prognosis background maps.
      dir_bg_actual = trim(datadir) // dir_chem_meteo(2)
   else
      ! Use diagnosis background maps.
      dir_bg_actual = trim(datadir) // dir_chem_meteo(1)
   endif
endif

! Of which components the background concentration has to be read depends on the substance for which the OPS calculation takes place.
! 1: SO2 >> {(NH4)2 SO4}            -> SO2, NH3;
! 2: NO2 >> {NH4 NO3}               -> NO2, NH3;
! 3: NH3 >> {(NH4)2 SO4}, {NH4 NO3} -> SO2, NO2, NH3
call read_bg_map("nh3", year, trim(dir_bg_actual), nh3bggrid, error)
if (icm /= icm_NOx) call read_bg_map("so2", year, trim(dir_bg_actual), so2bggrid, error)
if (icm /= icm_SO2) call read_bg_map("nox", year, trim(dir_bg_actual), no2bggrid, error)

! Read gamma water
! call read_bg_file(year, 'gw', 'gamma water', '', map_gamma, fnames_used_chem, gwgrid, error)
CALL MakeCommonPath(map_gamma, gwfile, error)
CALL ReadAps(gwfile, 'gamma water', gwgrid, error)

IF (error%haserror) GOTO 9999
CALL SetAverage(1.0, gwgrid)

! Read ozone concentrations (needed for NO2/NOx ratio):
if (icm == icm_NOx) then
      
      ! Read and allocate the background grids for O3 (for NSEK windsectors; needed for NO2/NOx ratio) [ug/m3]:  
      if (nemcat_road .gt. 0 .and. road_chem) then
         CALL read_bg_file(year, 'o3' , 'background concentration', dir_chem, map_o3, fnames_used_chem, o3bggrid, error)
         if (error%haserror) GOTO 9999

         if (error%debug) then
            write(*,*) '------------------ops_read_bg------------------------'
            write(*,*) 'size(o3bggrid%value):',size(o3bggrid%value,1),'x',size(o3bggrid%value,2),'x',size(o3bggrid%value,3)
            write(*,*) o3bggrid%gridheader%nrcol, 'x', o3bggrid%gridheader%nrrow
            write(*,*) error%haserror
            write(*,*) '------------------------------------------'
         endif
         IF (error%haserror) GOTO 9999

         ! Check number of fields read from file:
         nfield = size(o3bggrid%value,3)
         if (nfield .ne. NSEK) then
             CALL SetError('unexpected number of fields in file for background concentrations O3 ', error)
             CALL ErrorParam('file is last file in this list', fnames_used_chem, error)
             CALL ErrorParam('number of fields read from file', nfield, error)
             CALL ErrorParam('expected number of fields = number of wind sectors', NSEK, error)
             goto 9999
         endif

         ! Compute average grid value (to be used outside background grid and for missing values):
         DO ifield = 1,nfield
            CALL SetAverage(1.0, o3bggrid, ifield)
         ENDDO

         if (error%debug) then
            write(*,*) 'o3bggrid%value(2,2:)'
            write(*,*) o3bggrid%value(2,2,1:NSEK)
            write(*,*) 'o3bggrid%value(400,400,:)'
            write(*,*) o3bggrid%value(400,400,1:NSEK)
            write(*,*) 'o3bggrid%average'
            write(*,*) o3bggrid%average
         endif
      endif
endif

! iopt_vchem = 1 -> read EMEP grids with column averaged masses and mass converted [ug/m2] used for chemical conversion rate vchem:
if (iopt_vchem .eq. 1) then

   ! Read MASS_PRE for this year from file 'xxx_mass_prec_yyyy.ops'; xxx = name primary species (SO2, NOx, NH3), yyyy = year (e.g. 2019):
   CALL read_bg_file(year, CNAME(icm,1), 'mass precursor', dir_chem, map_mass_prec, fnames_used_chem, vchem_emep%mass_prec_grid, error)
   if (error%haserror) GOTO 9999

   call SetAverage(grid = vchem_emep%mass_prec_grid)

   ! Read MASS_CONV_DTFAC for this year:
   CALL read_bg_file(year, CNAME(icm,1), '(100/dt) * mass converted chemistry', dir_chem, map_mass_conv_dtfac, fnames_used_chem, vchem_emep%mass_conv_dtfac_grid, error)
   if (error%haserror) GOTO 9999

   call SetAverage(grid = vchem_emep%mass_conv_dtfac_grid)

   ! write(*,*) 'average of mass_prec_grid: ', vchem_emep%mass_prec_grid%average
   ! write(*,*) 'average of mass_conv_dtfac_grid: ', vchem_emep%mass_conv_dtfac_grid%average
   ! write(*,*) 'average conversion rate [%/h]: ', vchem_emep%mass_conv_dtfac_grid%average/vchem_emep%mass_prec_grid%average

   ! Read distribution maps for NO3_total: HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total;
   ! from file 'no3_distr_yyyy.ops'; yyyy = year (e.g. 2019)
   if (icm == icm_NOx) then

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
         stop 1
      endif

      ! Set average of grid (is used in ops_bgcon for missing (negative) values or values outside grid):
      do ifield = 1,nfield
         call SetAverage(grid = f_subsec_grid, fieldnumber = ifield)
         ! write(*,*) 'average of grid of secondary component ',ifield,' = ',f_subsec_grid%average(ifield)
      enddo
   endif

   ! ! START TEST write to APS file --------------------------------------------------------------------------------------------
   !  qq%gridheader%xorgl = 1000*vchem_emep%mass_prec_grid(1)%gridheader%xorgl
   !  qq%gridheader%yorgl = 1000*vchem_emep%mass_prec_grid(1)%gridheader%yorgl
   !  qq%gridheader%grixl = 1000*vchem_emep%mass_prec_grid(1)%gridheader%grixl
   !  qq%gridheader%griyl = 1000*vchem_emep%mass_prec_grid(1)%gridheader%griyl
   !  qq%gridheader%nrcol = vchem_emep%mass_prec_grid(1)%gridheader%nrcol
   !  qq%gridheader%nrrow = vchem_emep%mass_prec_grid(1)%gridheader%nrrow
   !  allocate(qq%value(qq%gridheader%nrcol, qq%gridheader%nrrow, 1))
   !  qq%value = vchem_emep%mass_conv_dtfac_grid(1)%value/vchem_emep%mass_prec_grid(1)%value
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
   !  !  real              cpri(matx,maty)
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
   
   !  !!    REAL                                          :: xul_cell_centre            ! x-coordinate of centre of upper-left grid cell [m]
   !  !!    REAL                                          :: yul_cell_centre            ! y-coordinate of centre of upper-left grid cell [m]
   !  !!    INTEGER                                       :: nrcol                      ! number of grid columns
   !  !!    INTEGER                                       :: nrrow                      ! number of grid rows
   !  !!    REAL                                          :: grixl                      ! horizontal size of grid cell [km]
   !  !!    REAL                                          :: griyl                      ! vertical size of grid cell [km]
   !  !! END TYPE TGridHeader
   ! ! END TEST write to APS file --------------------------------------------------------------------------------------------

   IF (error%haserror) GOTO 9999
endif ! if (iopt_vchem = 1)

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
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

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

INTEGER,   INTENT(IN)                             :: year                       ! year for chemical maps
LOGICAL, INTENT(IN)                               :: chem_meteo_prognosis       ! use meteo prognosis in chemistry maps

CHARACTER*(*), INTENT(OUT)                        :: dir_chem                   ! directory where to read chemistry files from
INTEGER,   INTENT(OUT)                            :: mapnumber                  ! number of background map
INTEGER,   INTENT(OUT)                            :: ji                         ! year index, i.e. the index in the trendfactor tf_... of the current year

LOGICAL*1                                         :: future                     ! year >= FUTUREYEAR
INTEGER                                           :: os                         ! index for Operating system (0 = Unix, 1 = Windows)
CHARACTER(LEN=1)                                  :: slash                      ! slash for directory separation

! Get number of map that is the basis for the calculation of the background concentration.
! See documentation in calling routine.

! Get slash:
CALL GetOS(os, slash)

! Check consistency:
if (FIRSTYEAR+NYEARS .ne. FUTUREYEAR-1) then
   write(*,*) 'Internal programming error in set_bg_year_indices'
   write(*,*) 'FIRSTYEAR  = ',FIRSTYEAR
   write(*,*) 'NYEARS     = ',NYEARS
   write(*,*) 'FUTUREYEAR = ',FUTUREYEAR
   stop 1
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

subroutine convert_nox_to_no2(grid)
   use m_commonconst_lt, only: nox_no2_beta

   use m_aps, only: TApsGridReal, SetAverage

   implicit none

   type(TApsGridReal), intent(inout) :: grid

   real :: nox_threshold
   real :: alpha
   integer :: i, j

   nox_threshold = exp(1-(nox_no2_beta(2)/nox_no2_beta(1)))
   alpha = nox_no2_beta(1)/nox_threshold

   DO i=1, grid%gridheader%nrcol
      DO j=1, grid%gridheader%nrrow

         IF(grid%value(i,j,1) .GT. nox_threshold) THEN
            grid%value(i,j,1) = nox_no2_beta(1) * log(grid%value(i,j,1)) + nox_no2_beta(2)
         ELSE
            grid%value(i,j,1) = alpha * grid%value(i,j,1)
         ENDIF

      ENDDO
   ENDDO

   call SetAverage(1.0, grid)
end subroutine convert_nox_to_no2

subroutine convert_no2_to_nox(grid)
   ! Inverse of `convert_nox_to_no2`.
   use m_commonconst_lt, only: nox_no2_beta

   use m_aps, only: TApsGridReal, SetAverage

   implicit none

   type(TApsGridReal), intent(inout) :: grid

   real :: nox_threshold
   real :: alpha
   integer :: i, j

   nox_threshold = exp(1-(nox_no2_beta(2)/nox_no2_beta(1)))
   alpha = nox_no2_beta(1) / nox_threshold

   do i=1, grid%gridheader%nrcol
      do j=1, grid%gridheader%nrrow

         if (grid%value(i,j,1) .gt. nox_no2_beta(1)) then
            grid%value(i,j,1) = exp( &
               (grid%value(i,j,1) - nox_no2_beta(2)) / nox_no2_beta(1) &
            )
         else
            grid%value(i,j,1) = grid%value(i,j,1) / alpha
         endif

      enddo
   enddo

   call SetAverage(1.0, grid)
end subroutine convert_no2_to_nox

subroutine write_bg_map(compound, year, grid, error)
   ! Writes a grid to an APS file.
   use m_aps, only: TApsGridReal, WriteAps
   use m_error, only: TError

   implicit none

   character(len=*), intent(in) :: compound
   integer, intent(in) :: year
   type(TApsGridReal), intent(in) :: grid

   type(TError), intent(inout) :: error

   character(len=99) :: year_char
   character(len=512) :: filename

   write(year_char, "(I4)") year

   write(filename,*) "bg_maps/bg" // compound // "c" // trim(adjustl(year_char)) // ".aps"
   write(*,*) "Writing background map", trim(filename)

   call WriteAps(filename, filename, grid, error, zero_dxy_allowed = .true.)

end subroutine write_bg_map

subroutine read_bg_map(compound, year, directory, grid, error)
   ! Reads an APS file containing the background map for a given compound and year.
   use m_commonconst_lt, only: FUTUREYEAR

   use m_aps, only: TApsGridReal, ReadAps, SetAverage
   use m_error, only: TError, ErrorCall
   use m_string, only: LoCase

   implicit none

   character(len=*), intent(in) :: compound
   integer, intent(in) :: year
   character(len=*), intent(in) :: directory  ! Directory to look for the map file.

   type(TApsGridReal), intent(out) :: grid

   type(TError), intent(inout) :: error

   character(len=8) :: year_char  ! String representation of the given year.
   character(len=512) :: filename
   character(len=512) :: path
   character(len=3) :: compound_low

   ! Construct the filepath.
   write(year_char, "(I4)") year  ! Convert integer to string.
   compound_low = LoCase(compound)

   filename = "/bg" // compound_low // "c" // trim(adjustl(year_char)) // ".ops"
   path = trim(directory) // trim(filename)

   ! Try to read the file.
   call ReadAps(trim(path), trim(filename), grid, error, zero_dxy_allowed=.true.)

   if (error%haserror) then
      write(*,*) trim(path), trim(error%message)
      call ErrorCall("read_bg_map", error)
      return
   endif

   call convert_to_ppm(compound_low, grid, error)

   if (error%haserror) then
      call ErrorCall("read_bg_map", error)
      return
   endif

   ! Subroutine `ops_read_bg` converts NOx to NO2, so we need to do the same for `grid_direct`.
   ! HACK:
   ! maps also convert NO2 to NOx (since only NO2 is calibrated) at some point, so this step might
   ! be removed entirely in the future.
   if (compound_low .eq. "nox") call convert_nox_to_no2(grid)

   call SetAverage(grid=grid)
end subroutine read_bg_map

subroutine convert_to_ppm(compound, grid, error)
   ! Convert background map grid units to PPM.
   use m_aps, only: TApsGridReal, ReadAps, SetAverage
   use m_string, only: LoCase
   use m_error, only: TError, ErrorCall, ErrorParam, SetError

   implicit none

   character(len=3), intent(in) :: compound

   type(TApsGridReal), intent(inout) :: grid
   type(TError), intent(inout) :: error

   real :: factor
   real, dimension(3) :: MW_CORRECTION

   data MW_CORRECTION/17., 46., 64./

   select case (compound)
      case ("nh3")
         factor = MW_CORRECTION(1)
      case ("nox")
         factor = MW_CORRECTION(2)
      case ("so2")
         factor = MW_CORRECTION(3)
      case default
         call SetError("Cannot convert compound to PPM.", error)
         call ErrorParam("Compound", compound, error)
         call ErrorCall("convert_to_ppm", error)
         return
   end select

   call SetAverage(24. / factor, grid)

end subroutine convert_to_ppm

end module m_ops_read_bg
