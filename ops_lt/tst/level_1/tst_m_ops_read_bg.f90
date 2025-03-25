
module m_tst_ops_read_bg

implicit none

contains

subroutine tst_ops_read_bg

! Test is only for the file names; reading of data is not (yet) tested
! The files at ./level_1/resources/Chem... consist only of a header line and and a small 6 element grid.

use no_pfunit
use m_ops_read_bg
use m_aps
use m_error
use m_ops_vchem
use m_commonfile, only: IOB_STDOUT, datadir
use m_commonconst_lt, only: FIRSTYEAR, FUTUREYEAR, CNAME

implicit none

INTEGER, PARAMETER                               :: ENDYEAR = 2030             ! end year for tests


! SUBROUTINE ARGUMENTS - INPUT
INTEGER                                          :: icm                        ! substance index
INTEGER                                          :: iopt_vchem                 ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
INTEGER                                          :: nsubsec                    ! number of sub-secondary species
INTEGER                                          :: year                       ! year under consideration
logical                                          :: chem_meteo_prognosis       ! use meteo prognosis in chemistry maps
INTEGER                                          :: nemcat_road                ! number of road emission categories (for vdHout NO2/NOx ratio)
logical											:: road_chem 

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TApsGridReal)                              :: so2bggrid                  ! grid with SO2 background concentration [ppb]
TYPE (TApsGridReal)                              :: no2bggrid                  ! grid with NO2 background concentration [ppb]
TYPE (TApsGridReal)                              :: nh3bggrid                  ! grid with NH3 background concentration [ppb]
TYPE (TApsGridReal)                              :: gwgrid                     ! grid with gamma water values
TYPE (TApsGridReal)                              :: o3bggrid                   ! grids with O3 background concentration per wind sector [ug/m3]
TYPE (TApsGridReal)                              :: f_subsec_grid              ! grids of fractions for sub-secondary species, HNO3/NO3_total, NO3_C/NO3_total, NO3_F/NO3_total [-]
TYPE (Tvchem)                                    :: vchem2                     !
CHARACTER(512)                                   :: dir_chem                   ! directory with chemistry files
CHARACTER(LEN=512)                               :: fnames_used_chem           ! file names usedfor chemistry maps
character(len=512) :: dir_bg_actual
TYPE (TError)                                    :: error                      ! error handling record

INTEGER                                          :: year_start                 ! start year
INTEGER                                          :: lu_ref = 11                ! logical unit number file with reference data
CHARACTER(LEN=512)                               :: line_test                  ! line with test data (file names which have been read)
CHARACTER(LEN=512)                               :: line_ref                   ! line with reference data (file names which have been read)
INTEGER                                          :: iline                      ! line number in file with reference data
REAL                                             :: grid(3,2,1)                ! help grid, same as read from mockup files.
REAL                                             :: grid_o3(3,2,12)            ! help grid for ozone (12 wind sectors), same as read from mockup files.
REAL                                             :: grid_no2(3,2,1)            ! help grid for NO2
REAL                                             :: cf                         ! calibration factor
REAL                                             :: tf                         ! trend factor
INTEGER                                          :: ichem                      ! index for loop over chem_meteo_prognosis
CHARACTER(LEN = 200)                             :: msg                        ! message for assertEqual
! LOGICAL                                          :: no_ozone                   ! no ozone data available
CHARACTER(LEN = 20)                              :: chem_meteo_type            ! 'chem_meteo_actual' or 'chem_meteo_prognosis'
CHARACTER(LEN = 14)                              :: chem_type                  ! 'EMEP' or 'OPS_background'
CHARACTER(LEN = 100)                             :: fnam_ref                   ! filw with reference file names
INTEGER                                          :: icm2                       ! substance index
INTEGER                                          :: iopt_vchem2                ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
REAL, PARAMETER                                  :: tol = 1.0e-6               ! absolute tolerance in assert

CHARACTER*512, PARAMETER                         :: ROUTINENAAM = 'tst_ops_read_bg'

! Set help grid with data present in mock-up data files (see level_1/resources):
grid(:,1,1)     = (/   1.0,   2.0,   5.0 /); grid(:,2,1)     = (/   4.0,   5.0,   6.0 /)
grid_o3(:,1, 1) = (/   1.0,   2.0,   5.0 /); grid_o3(:,2, 1) = (/   4.0,   5.0,   6.0 /)
grid_o3(:,1, 2) = (/  11.0,  12.0,  15.0 /); grid_o3(:,2, 2) = (/  14.0,  15.0,  16.0 /)
grid_o3(:,1, 3) = (/  21.0,  22.0,  25.0 /); grid_o3(:,2, 3) = (/  24.0,  25.0,  26.0 /)
grid_o3(:,1, 4) = (/  31.0,  32.0,  35.0 /); grid_o3(:,2, 4) = (/  34.0,  35.0,  36.0 /)
grid_o3(:,1, 5) = (/  41.0,  42.0,  45.0 /); grid_o3(:,2, 5) = (/  44.0,  45.0,  46.0 /)
grid_o3(:,1, 6) = (/  51.0,  52.0,  55.0 /); grid_o3(:,2, 6) = (/  54.0,  55.0,  56.0 /)
grid_o3(:,1, 7) = (/  61.0,  62.0,  65.0 /); grid_o3(:,2, 7) = (/  64.0,  65.0,  66.0 /)
grid_o3(:,1, 8) = (/  71.0,  72.0,  75.0 /); grid_o3(:,2, 8) = (/  74.0,  75.0,  76.0 /)
grid_o3(:,1, 9) = (/  81.0,  82.0,  85.0 /); grid_o3(:,2, 9) = (/  84.0,  85.0,  86.0 /)
grid_o3(:,1,10) = (/  91.0,  92.0,  95.0 /); grid_o3(:,2,10) = (/  94.0,  95.0,  96.0 /)
grid_o3(:,1,11) = (/ 110.0, 120.0, 150.0 /); grid_o3(:,2,11) = (/ 140.0, 150.0, 160.0 /)
grid_o3(:,1,12) = (/ 210.0, 220.0, 250.0 /); grid_o3(:,2,12) = (/ 240.0, 250.0, 260.0 /)

! Define directory for data files:
!datadir = '/OPS/Data/Configuraties/Oper/GCN2021/Data/'
datadir = './level_1/resources/'

! Open reference file:
fnam_ref = './level_1/resources/tst_m_ops_read_bg.ref'
open(lu_ref, file = fnam_ref)
rewind(lu_ref)
iline = 0

! Number of road emission categories must be larger than 0 to get things going:
nemcat_road = 1

do ichem = 1,2
   ! First check actual meteo, then prognosis meteo:
   chem_meteo_prognosis = (ichem .eq. 2)

   if (chem_meteo_prognosis) then
      chem_meteo_type = 'chem_meteo_prognosis'
   else
      chem_meteo_type = 'chem_meteo_actual'
   endif


   do iopt_vchem = 0,1

      if (iopt_vchem .eq. 1) then
         chem_type = 'EMEP'
      else
         chem_type = 'OPS_background'
      endif

      ! Set start year for test (from this year files are available):
      call tst_m_ops_read_bg_year_start(chem_meteo_prognosis,iopt_vchem,year_start)

      do year = FIRSTYEAR, ENDYEAR
      ! do year = 2014,2015

         ! Loop over components:
         do icm = 1,3

            ! Determine number of sub secondary species (see ops_read_ctr):
            if (icm .eq. 2) then
               if (iopt_vchem .eq. 0) then
                  nsubsec = 2
               else
                  nsubsec = 4
               endif
            else
               nsubsec = 0
            endif

            ! Define file names and read files with chemical maps:

            !!------------------------------------------------------------------
            !
            !
            !no_ozone = (icm .eq. 2 .and. (year .lt. 2015 .or. year .gt. 2020))
            !if (no_ozone) then
            !   ! write(*,*) '(1) ozone data for year < 2015 or year > 2020 not yet available: ',year 
            !   if (year < year_start) call SetError('year before start year',error)
            !else
            !   call ops_read_bg(icm, iopt_vchem, nsubsec, year, chem_meteo_prognosis, so2bggrid, no2bggrid, nh3bggrid, o3bggrid, &
            !                    f_subsec_grid, vchem2, error, dir_chem, fnames_used_chem)
            !endif
            !
            !! Check for error:
            !if (year < year_start .or. no_ozone) then
            !   if (year < year_start) then
            !      ! Expected error if year is before start year (reset error in this case):
            !      write(msg,'(a,i4)') 'test expected error for year ',year
            !      call assertEqual(.true.,error%haserror,msg,__LINE__,__FILE__)
            !      error%haserror = .false.
            !   else
            !      ! No ozone data available; skip line from reference file, do not check anything:
            !      read(lu_ref,'(a)') line_ref
            !   endif
            !!------------------------------------------------------------------

            !------------------------------------------------------------------
            ! OZONE DATA AVAILABLE
            call ops_read_bg( &
               icm, iopt_vchem, nsubsec, year, chem_meteo_prognosis, nemcat_road, road_chem, dir_chem, &
               so2bggrid, no2bggrid, nh3bggrid, o3bggrid, gwgrid, &
               f_subsec_grid, vchem2, error, dir_chem, fnames_used_chem, dir_bg_actual)


            if (year < year_start) then
               ! Expected error if year is before start year (reset error in this case):
               write(msg,'(a,i4)') 'test expected error for year ',year
               call assertEqual(.true.,error%haserror,msg,__LINE__,__FILE__)
               error%haserror = .false.
            else
               ! Unexpected error:
               if (error%haserror) then
                  !CALL ErrorCall(ROUTINENAAM, error)
                  call ErrorParam('YEAR',year,error)
                  call WriteError(IOB_STDOUT,error)
                  call assertEqual(.false.,error%haserror,'unexpected error',__LINE__,__FILE__)
				  stop 1
               endif
               ! No error; check directory:
               if (year .ge. FUTUREYEAR .or. chem_meteo_prognosis) then
                  call assertEqual(trim(datadir)//'Chem_meteo_prognosis/',dir_chem,'directory for chemistry maps',__LINE__,__FILE__)
                  write(msg,'(a,i4,a)') 'test maps for year ',year,'; chem_meteo_prognosis ====== See HELP above ======; '
               else
                  call assertEqual(trim(datadir)//'Chem_meteo_actual/',dir_chem,'directory for chemistry maps',__LINE__,__FILE__)
                  write(msg,'(a,i4,a)') 'test maps for year ',year,'; chem_meteo_actual ====== See HELP above ======; '
               endif

               if (year .ge. FUTUREYEAR .and. .not. chem_meteo_prognosis) then
                  ! year >= FUTUREYEAR and chem_meteo_actual -> chem-file not present -> no check needed
                  continue
               else
                  ! Following line can be used to generate a reference file; the contents have to be checked of course!
                  ! write(*,'(a20,1x,a14,1x,i5,3(1x,a))') trim(chem_meteo_type),trim(chem_type),year, trim(CNAME(icm,1)),' ',trim(fnames_used_chem)

                  ! Read from reference file and check file names used:
                  ! background maps for SO2: SO2, NH3
                  ! background maps for NOx: NOx, NH3, O3
                  ! background maps for NH3: SO2, NOx, NH3
                  read(lu_ref,'(a)') line_ref
                  iline = iline + 1
                  write(line_test,'(a20,1x,a14,1x,i5,3(1x,a))') trim(chem_meteo_type),trim(chem_type),year, trim(CNAME(icm,1))! ,' ',trim(fnames_used_chem) 

                  ! Provide help what to do if FUTUREYEAR has been changed:
                  if (line_test .ne. line_ref) then
                     write(*,'(/,a)')   '====== HELP ======================================================================================== '
                     write(*,'(a,a,a)') ' If an error occurs in ', trim(ROUTINENAAM),', this may be due to a new FUTUREYEAR in OPS.'
                     write(*,'(a,a,a,i4,a1,/)') ' If this is the case, add the following 3 lines with file names in the reference file ',trim(fnam_ref),' after line ',iline-1,':'
                     do iopt_vchem2 = 0,1
                        if (iopt_vchem2 .eq. 1) then
                           chem_type = 'EMEP'
                        else
                           chem_type = 'OPS_background'
                        endif
                        do icm2 = 1,3
                           ! Determine number of sub secondary species (see ops_read_ctr):
                           if (icm2 .eq. 2) then
                              if (iopt_vchem2 .eq. 0) then
                                 nsubsec = 2
                              else
                                 nsubsec = 4
                              endif
                           else
                              nsubsec = 0
                           endif
                           ! Get file names to read from:
                           call ops_read_bg( &
                              icm2, iopt_vchem2, nsubsec, year, chem_meteo_prognosis, nemcat_road, road_chem, dir_chem, &
                              so2bggrid, no2bggrid, nh3bggrid, o3bggrid, gwgrid, &
                              f_subsec_grid, vchem2, error, dir_chem, fnames_used_chem, dir_bg_actual)


                           write(*,'(a20,1x,a14,1x,i5,3(1x,a))') trim(chem_meteo_type),trim(chem_type),year, trim(CNAME(icm2,1)),' ',trim(fnames_used_chem)
                        enddo
                        if (iopt_vchem2 .eq. 0) then
                           write(*,'(/,a,/)') ' and another 3 lines further on in this file:'
                        endif
                     enddo
                     write(*,'(/,a)') ' Please check if these lines are as expected!'
                     write(*,'(a,/)') '==================================================================================================== '
                  endif
                  call assertEqual(line_ref,line_test,msg,__LINE__,__FILE__)

                  ! Check some grid values:
                  if (icm .eq. 3 .and. year .eq. 2013) then
                     cf= 0.72; tf = 0.98 ! see m_commonconst_lt, so2
                     call assertEqual(24./64.*cf*tf*grid,so2bggrid%value,tol,'test so2 background',__LINE__,__FILE__)

                     cf= 0.94; tf = 0.94 ! see m_commonconst_lt, nox
                     ! NOx-NO2 conversion
                     call nox_no2_conversion(grid,grid_no2)
                     call assertEqual(24./46.*cf*tf*grid_no2,no2bggrid%value,tol,'test no2 background',__LINE__,__FILE__)
                     cf= 1.12; tf = 0.97 ! see m_commonconst_lt, nh3
                     call assertEqual(24./17.*cf*tf*grid,nh3bggrid%value,tol,'test nh3 background',__LINE__,__FILE__)
                  endif

                  ! Ozone
                  if (icm .eq. 2 .and. year .eq. 2019) then
                     call assertEqual(grid_o3,o3bggrid%value,tol,'test o3 background',__LINE__,__FILE__)
                  endif

                  if (icm .eq. 3 .and. year .eq. 2023) then
                     cf= 0.95; tf = 1.0 ! see m_commonconst_lt, so2
                     call assertEqual(24./64.*cf*tf*grid,so2bggrid%value,tol,'test so2 background',__LINE__,__FILE__)

                     cf= 0.98; tf = 1.0 ! see m_commonconst_lt, no2
                     call nox_no2_conversion(grid,grid_no2)
                     call assertEqual(24./46.*cf*tf*grid_no2,no2bggrid%value,tol,'test no2 background',__LINE__,__FILE__)

                     cf= 0.99; tf = 1.0 ! see m_commonconst_lt, nh3
                     call assertEqual(24./17.*cf*tf*grid,nh3bggrid%value,tol,'test nh3 background',__LINE__,__FILE__)
                     if (iopt_vchem .eq. 1) then
                        call assertEqual(grid,vchem2%mass_prec_grid%value,tol,'test mass_prec',__LINE__,__FILE__)
                        call assertEqual(grid,vchem2%mass_conv_dtfac_grid%value,tol,'test mass_conv_dtfac',__LINE__,__FILE__)
                     endif
                  endif
               endif
            endif
         enddo ! loop over iopt_vchem
      enddo ! loop over years
   enddo  ! loop over iopt_vchem
enddo ! loop over chem_meteo_prognosis

return



end subroutine tst_ops_read_bg

!--------------------------------------------------------------------------
subroutine tst_m_ops_read_bg_year_start(chem_meteo_prognosis,iopt_vchem,year_start)

use m_commonconst_lt, only: FIRSTYEAR

implicit none

LOGICAL, INTENT(IN)  :: chem_meteo_prognosis       ! use meteo prognosis in chemistry maps
INTEGER, INTENT(IN)  :: iopt_vchem                 ! option for chemical conversion rate (0 = old OPS, 1 = EMEP)
INTEGER, INTENT(OUT) :: year_start                 ! start year for test (files are available from year_start)

! Set starting year for test:
if (chem_meteo_prognosis) then
   year_start = 2018
else
   if (iopt_vchem .eq. 1) then
      year_start = 2014 ! EMEP; for years < 2014 -> error; file not found
   else
      year_start = FIRSTYEAR
   endif
endif

end subroutine tst_m_ops_read_bg_year_start

subroutine tst_convert_no2_nox_conversion
   use m_ops_read_bg, only: convert_nox_to_no2, convert_no2_to_nox
   use m_aps, only: TApsGridReal
   use no_pfunit

   type(TApsGridReal) :: grid
   type(TApsGridReal) :: grid_ref

   grid%gridheader%xul_corner = 0.0
   grid%gridheader%yul_corner = 0.0
   grid%gridheader%nrcol = 3
   grid%gridheader%nrrow = 1
   grid%gridheader%grixl = 1.0
   grid%gridheader%griyl = 1.0

   allocate(grid%average(1))
   allocate(grid%value(3,1,1))
   grid%value(:,1,1) = (/ 1.0, 10.0, 50.0 /)

   allocate(grid_ref%average(1))
   allocate(grid_ref%value(3,1,1))
   grid_ref%value(:,1,1) = (/ 1.0, 10.0, 50.0 /)
   grid_ref%gridheader = grid%gridheader

   call convert_nox_to_no2(grid)
   call convert_no2_to_nox(grid)

   call assertEqual(grid%value(1,1,1),grid_ref%value(1,1,1),1e-5,'Values differ.',__LINE__,__FILE__)
   call assertEqual(grid%value(2,1,1),grid_ref%value(2,1,1),1e-5,'Values differ.',__LINE__,__FILE__)
   call assertEqual(grid%value(3,1,1),grid_ref%value(3,1,1),1e-5,'Values differ.',__LINE__,__FILE__)
end subroutine

subroutine compare_aps_grid_real(old, new, corr_fact, mol_scale)
   use m_aps, only: TApsGridReal, TGridHeader

   use no_pfunit

   type(TApsGridReal), intent(in) :: old, new
   real, intent(in) :: corr_fact, mol_scale

   integer :: idx, jdx, kdx  ! Iteration indices.
   type(TGridHeader) :: header_old, header_new
   logical :: values_differ
   real :: new_value, old_value

   character*(*), parameter :: header_msg = "Headers are different."

   header_old = old%gridheader
   header_new = new%gridheader

   ! Compare headers.
   call assertEqual(header_old%xul_corner, header_new%xul_corner, header_msg, __LINE__, __FILE__)
   call assertEqual(header_old%yul_corner, header_new%yul_corner, header_msg, __LINE__, __FILE__)

   call assertEqual(header_old%nrcol, header_new%nrcol, header_msg, __LINE__, __FILE__)
   call assertEqual(header_old%nrrow, header_new%nrrow, header_msg, __LINE__, __FILE__)

   call assertEqual(header_old%grixl, header_new%grixl, header_msg, __LINE__, __FILE__)
   call assertEqual(header_old%griyl, header_new%griyl, header_msg, __LINE__, __FILE__)

   ! Compare average.
   ! call assertEqual(size(old%average), size(new%average), "", __LINE__, __FILE__)

   ! do idx = 1,size(old%average)
   !    call assertEqual(old%average(idx), new%average(idx), "Averages are not equal.", __LINE__, __FILE__)
   ! enddo

   ! Compare values.
   call assertEqual(size(new%value), size(new%value), "Sizes are not equal", __LINE__, __FILE__)

   values_differ = .false.
   data idx, jdx, kdx /1, 1, 1/

   do idx = 1,size(new%value, 1)
      do jdx = 1,size(new%value, 2)
         do kdx = 1,size(new%value, 3)
            new_value = new%value(idx,jdx,kdx)
            old_value = mol_scale * corr_fact * old%value(idx,jdx,kdx)
            if (new_value / old_value - 1 > 1e-5) then
               values_differ = .true.
               write(*,*) "Found difference at ", idx, jdx, kdx, ":", new_value, old_value
               exit
            endif
         enddo

         if (values_differ .eqv. .true.) exit
      enddo

      if (values_differ .eqv. .true.) exit
   enddo

   ! TODO:
   call assertFalse(values_differ, "All values should be equal.", __LINE__, __FILE__)
end subroutine

! NOTE:
! Chem_meteo_actual and Chem_meteo_prognosis. Old maps are no longer
! available for comparison.
! subroutine tst_compare_corrected_maps
!    use m_commonconst_lt, only: cf_so2, cf_nh3
!    use m_ops_read_bg, only: read_bg_map, read_bg_file
!
!    use m_aps, only: TApsGridReal, SetAverage
!    use m_error, only: TError
!
!    use no_pfunit
!
!    type(TApsGridReal) :: new_grid, old_grid
!    type(TError) :: error
!    character*256 :: fnames_used_chem = " "
!
!    ! Read original 2005 background map and compare to corrected 2005 map.
!    ! call read_bg_file(2005, "SO2", "background concentration", "/OPS/Data/Configuraties/Oper/GCN2022/Data/Chem_meteo_actual/", "bgso2c2005.ops", fnames_used_chem, old_grid, error)
!    call read_bg_file( &
!       2005, "SO2", "background concentration", &
!       "../data/GCN2022/Data/Chem_meteo_actual/", "bgso2c2005.ops", &
!       fnames_used_chem, old_grid, error)
!    call assertFalse(error%haserror, "Error when reading old map.", __LINE__, __FILE__)
!    if (error%haserror) write(*,*) trim(error%message)
!    call SetAverage(grid=old_grid)
!
!    call read_bg_map("SO2", 2005, "../data/GCN2022/Data/Chem_meteo_corrected", new_grid, error)
!    call assertFalse(error%haserror, "Error when reading new map.", __LINE__, __FILE__)
!    if (error%haserror) write(*,*) trim(error%message)
!
!    call compare_aps_grid_real(old_grid, new_grid, cf_so2(3), 24./64.)
!
!    ! NOx,
!    ! call read_bg_file(2005, "NOx", "background concentration", "/OPS/Data/Configuraties/Oper/GCN2022/Data/Chem_meteo_actual/", "bgnoxc2005.ops", fnames_used_chem, old_grid, error)
!    ! call assertFalse(error%haserror, "Error when reading old map.", __LINE__, __FILE__)
!    ! call SetAverage(grid=old_grid)
!
!    ! call read_bg_map("NOx", 2005, "./data/bg_maps/", new_grid, error)
!    ! call assertFalse(error%haserror, "Error when reading new map.", __LINE__, __FILE__)
!
!    ! call compare_aps_grid_real(old_grid, new_grid, cf_nox(3), 24./46.)
!
!    ! NH3
!    call read_bg_file(2005, "NH3", "background concentration", "../data/GCN2022/Data/Chem_meteo_actual/", "bgnh3c2005.ops", fnames_used_chem, old_grid, error)
!    call assertFalse(error%haserror, "Error when reading old map.", __LINE__, __FILE__)
!    if (error%haserror) write(*,*) trim(error%message)
!    call SetAverage(grid=old_grid)
!
!    call read_bg_map("NH3", 2005, "../data/GCN2022/Data/Chem_meteo_corrected/", new_grid, error)
!    call assertFalse(error%haserror, "Error when reading new map.", __LINE__, __FILE__)
!    if (error%haserror) write(*,*) trim(error%message)
!
!    call compare_aps_grid_real(old_grid, new_grid, cf_nh3(3), 24./17.)
! end subroutine

subroutine tst_manual_dir_bg
   use m_ops_read_bg, only: read_bg_map
   use m_ops_read_ctr, only: TCtrLayers, ops_read_ctr

   use m_aps, only: TApsGridReal, SetAverage
   use m_commonfile, only: ctrnam
   use m_error, only: TError

   use no_pfunit

   type(TCtrLayers) :: ctr
   type(TApsGridReal) :: test_grid, ref_grid
   type(TError) :: error

   ! Read CTR arguments from file.
   ctrnam = "./level_1/resources/tst_m_ops_read_bg/tst_manual_dir_bg.ctr"
   call ops_read_Ctr(ctr, error)
   call assertFalse(error%haserror, "Error when reading ctr file.", __LINE__, __FILE__)
   if (error%haserror) then
      write(*,*) trim(error%message)
      return
   endif

   call assertEqual( &
      trim(ctr%substance%dir_bg), &
      "./level_1/resources/tst_m_ops_read_bg/", &
      "Ctr file does not contain correct path.", &
      __LINE__, __FILE__)

   ! Read bg map from a manual dir_bg.
   ! NOTE: The file that is read by `read_bg_map` is actually the SO2 map from
   ! 2012, but renamed to `bgnh3c2005.ops` to make sure the function doesn't
   ! attempt to get the file from the default directory.
   call read_bg_map("nh3", 2005, ctr%substance%dir_bg, test_grid, error)
   call assertFalse(error%haserror, "Error when reading custom bg map file.", __LINE__, __FILE__)
   if (error%haserror) then
      write(*,*) trim(error%message)
      return
   endif

   call read_bg_map("so2", 2012, "../data/GCN2022/Data/Chem_meteo_actual", ref_grid, error)
   call assertFalse(error%haserror, "Error when reading legacy bg map file.", __LINE__, __FILE__)
   if (error%haserror) then
      write(*,*) trim(error%message)
      return
   endif

   call compare_aps_grid_real(test_grid, ref_grid, 1., 17. / 64.)
end subroutine

subroutine tst_convert_to_ppm()
   use m_ops_read_bg, only: read_bg_map, convert_to_ppm
   use m_ops_read_ctr, only: TCtrLayers, ops_read_ctr

   use m_aps, only: TApsGridReal, SetAverage
   use m_commonfile, only: ctrnam
   use m_error, only: TError

   use no_pfunit

   type(TApsGridReal) :: test_grid, ref_grid
   type(TError) :: error

   call read_bg_map("nh3", 2005, "./level_1/resources/tst_m_ops_read_bg/", test_grid, error)
   call read_bg_map("nh3", 2005, "./level_1/resources/tst_m_ops_read_bg/", ref_grid, error)

   call SetAverage(24. / 17., ref_grid)
   call convert_to_ppm("nh3", test_grid, error)
   call compare_aps_grid_real(ref_grid, test_grid, 1.0, 1.0)

   call SetAverage(24. / 46., ref_grid)
   call convert_to_ppm("nox", test_grid, error)
   call compare_aps_grid_real(ref_grid, test_grid, 1.0, 1.0)

   call SetAverage(24. / 64., ref_grid)
   call convert_to_ppm("so2", test_grid, error)
   call compare_aps_grid_real(ref_grid, test_grid, 1.0, 1.0)

   error%haserror = .false.
   call convert_to_ppm("bla", test_grid, error)
   call AssertTrue(error%haserror)
end subroutine

subroutine nox_no2_conversion(grid_nox,grid_no2)

! Input/output:
REAL, INTENT(IN)                                 :: grid_nox(3,2,1)            ! help grid for NOx
REAL, INTENT(OUT)                                :: grid_no2(3,2,1)            ! help grid for NO2

! Local variables
REAL                                             :: nox_threshold
REAL                                             :: nox_no2_beta(2)
REAL                                             :: alpha
INTEGER                                          :: i, j

DATA nox_no2_beta  /8.6, -12.4/

! NOx-NO2 conversion, see ops_read_bg
! Note that at the start of these loops, no2bggrid contains the NOx-concentration
nox_threshold = exp(1-(nox_no2_beta(2)/nox_no2_beta(1)))
alpha = nox_no2_beta(1)/nox_threshold
DO i=1,size(grid_nox,1)
  DO j=1,size(grid_nox,2)
    IF(grid_nox(i,j,1) .GT. nox_threshold) THEN
      grid_no2(i,j,1) = nox_no2_beta(1)*log(grid_nox(i,j,1)) + nox_no2_beta(2)
    ELSE
      grid_no2(i,j,1) = alpha*grid_nox(i,j,1)
    ENDIF
  ENDDO
ENDDO

end subroutine nox_no2_conversion

end module m_tst_ops_read_bg

program p_tst_ops_read_bg

use m_tst_ops_read_bg
use no_pfunit

implicit none

! call tst_ops_read_bg
call tst_convert_no2_nox_conversion
! call tst_compare_corrected_maps
call tst_manual_dir_bg
call tst_convert_to_ppm
call conclusion
! write(*,*) "AV - test disabled since finding filenames has changed significantly."

end program p_tst_ops_read_bg
