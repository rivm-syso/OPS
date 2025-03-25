module m_tst_m_depac
contains

   subroutine tst_rc_lai
   use no_pfunit
   use m_depac_private
   implicit none

      integer                  :: day_of_year      ! day of year
      real                     :: lat              ! latitude (degrees)
      integer                  :: lu               ! landuse class
      integer, parameter       :: NDAY = 365       ! number of days in a normal year
      real                     :: lai_(NDAY)       ! leaf area indices for a whole year
      real                     :: sai_(NDAY)       ! surface area indices for a whole year
      real                     :: lai_ref(NDAY)    ! leaf area indices for a whole year, reference
      real                     :: sai_ref(NDAY)    ! surface area indices for a whole year, reference
      real, parameter          :: tol_abs = 5.0e-3 ! absolute tolerance
      integer                  :: luin = 11        ! logical unit number input file

      ! Define latitude:
      lat = 52.0

      ! Open file with reference data, relative to toplevel directory:
      open(luin, file = './level_1/resources/tst_m_depac.ref', status = 'old');
      rewind(luin)

      ! Compute one-sided leaf area index:
      do lu = 1,NLU
         ! Read reference data:
         read(luin,'(a)') ! header line
         read(luin,*) lai_ref
         read(luin,*) sai_ref

         ! Loop over days and compute lai, sai:
         do day_of_year = 1,NDAY
            call rc_lai(day_of_year,lat,lu,lai_(day_of_year),sai_(day_of_year))
         enddo

         ! Compare:
         call assertEqual(lai_ref,lai_,tol_abs,message='Leaf area index;')
         call assertEqual(sai_ref,sai_,tol_abs,message='Surface area index;')
      enddo

      close(luin)
   end subroutine tst_rc_lai

   
   !! Get component specific parameters:
   ! call rc_getpar(comp_id,diffc,rsoil_wet,rsoil_frozen,ipar_snow,rsoil)
   
   
   
   !! Set Rc (i.e. rc_tot) in special cases:
   !call rc_special(comp_id,lu,t,ipar_snow,nwet,rc_tot,ready,ccomp_tot)

   subroutine tst_rc_gw
   use no_pfunit
   use m_depac_private
   use m_commonconst_lib, only: i_HNO3, i_NO, i_NO2, i_O3, i_SO2, i_NH3
   implicit none

      ! Test external conductance

      integer           :: comp_id ! component id
      integer, parameter:: iratns=2! index for NH3/SO2 ratio;
                                   ! iratns = 1: low NH3/SO2
                                   ! iratns = 2: high NH3/SO2
                                   ! iratns = 3: very low NH3/SO2
      real              :: t       ! temperature (C)
      real              :: rh      ! relative humidity (%)
      integer           :: nwet    ! wetness indicator; nwet=0 -> dry; nwet=1 -> wet; nwet=9 -> snow
      real              :: sai     ! one-sided leaf area index (-)

      ! Output variables:
      real              :: gw     ! external leaf conductance (m/s)

      real, parameter   :: tol_rel = 1.0e-3 ! relative tolerance
      logical :: SAI_present

      ! Compute external conductance:
      comp_id = i_NH3; t = 12.0; rh = 70; nwet = 0; sai = 3.0;  SAI_present = .true.
      call rc_gw(comp_id,iratns,t,rh,nwet,sai,gw, SAI_present)
      call assertRelativelyEqual(3.5179287E-02,gw,tol_rel,'external conductance [m/s];')

      comp_id = i_NH3; t = 12.0; rh = 90; nwet = 1; sai = 3.0;  SAI_present = .true.
      call rc_gw(comp_id,iratns,t,rh,nwet,sai,gw,SAI_present)
      call assertRelativelyEqual(0.1862564,gw,tol_rel,'external conductance [m/s];')

      comp_id = i_NH3; t = -10.0; rh = 90; nwet = 1; sai = 1.0;  SAI_present = .true.
      call rc_gw(comp_id,iratns,t,rh,nwet,sai,gw,SAI_present)
      call assertRelativelyEqual(0.005,gw,tol_rel,'external conductance [m/s];')

      comp_id = i_SO2; t = 15.0; rh = 70; nwet = 0; sai = 3.0;  SAI_present = .true. 
      call rc_gw(comp_id,iratns,t,rh,nwet,sai,gw,SAI_present)
      !write(*,*) 'gw = ',gw, ', Rw = ',1.0/gw
      call assertRelativelyEqual(0.5115E-02,gw,tol_rel,'external conductance [m/s]; SO2')
      
      comp_id = i_SO2; t = 15.0; rh = 90; nwet = 0; sai = 3.0;  SAI_present = .true. 
      call rc_gw(comp_id,iratns,t,rh,nwet,sai,gw,SAI_present)
      !write(*,*) 'gw = ',gw, ', Rw = ',1.0/gw
      call assertRelativelyEqual(0.5588E-01,gw,tol_rel,'external conductance [m/s]; SO2')
      
      comp_id = i_NO2; t = 15.0; rh = 90; nwet = 0; sai = 3.0;  SAI_present = .true. 
      call rc_gw(comp_id,iratns,t,rh,nwet,sai,gw,SAI_present)
      !write(*,*) 'gw = ',gw, ', Rw = ',1.0/gw
      call assertRelativelyEqual(1.0/2000.,gw,tol_rel,'external conductance [m/s]; NO2')
      
      comp_id = i_NO; t = 15.0; rh = 90; nwet = 0; sai = 3.0;  SAI_present = .true. 
      call rc_gw(comp_id,iratns,t,rh,nwet,sai,gw,SAI_present)
      !write(*,*) 'gw = ',gw, ', Rw = ',1.0/gw
      call assertRelativelyEqual(-999.,gw,tol_rel,'external conductance [m/s]; NO')
      
      comp_id = i_O3; t = 15.0; rh = 90; nwet = 0; sai = 3.0;  SAI_present = .true. 
      call rc_gw(comp_id,iratns,t,rh,nwet,sai,gw,SAI_present)         
      !write(*,*) 'gw = ',gw, ', Rw = ',1.0/gw
      call assertRelativelyEqual(1./1000.,gw,tol_rel,'external conductance [m/s]; O3')

   end subroutine tst_rc_gw

   subroutine tst_rc_gstom
   use no_pfunit
   use m_depac_private
   use m_commonconst_lib, only: i_HNO3, i_NO, i_NO2, i_O3, i_SO2, i_NH3
   implicit none

      integer            :: comp_id   ! component id
      integer            :: lu        ! land use type , lu = 1,...,nlu
      real               :: lai       ! one-sided leaf area index
      real               :: glrad     ! global radiation (W/m2)
      real               :: sinphi    ! sin of solar elevation angle
      real               :: t         ! temperature (C)
      real               :: rh        ! relative humidity (%)
      real               :: diffc     ! diffusion coefficient of the gas involved

      real               :: gstom_(nlu)     ! stomatal conductances for all land use classes (m/s)

      real, parameter   :: tol_abs = 1.0e-6 ! absolute tolerance
      real              :: gstom_ref(nlu)   ! reference data for gstom (run 2020-10-06)
      integer           :: ipar_snow                         ! not used
      real              :: rsoil_wet,rsoil_frozen,rsoil(nlu) ! not used
      logical :: LAI_present
      ! variables from module
      ! LAI_present: vegetation is present
      ! dwat: diffusion coefficient of water vapour
      ! dO3 : diffusion coefficient of ozone


      ! Stomatal conductance:
      comp_id = i_NH3; lai = 2.0; glrad = 100; sinphi = 0.8; t = 12.0; rh = 90;
      call rc_getpar(comp_id,diffc,rsoil_wet,rsoil_frozen,ipar_snow,rsoil)

                 !          grass          arable           perm.          conif.          decid.           water          urban            other       desert
                 !                           land           crops          forest          forest
      gstom_ref = (/   0.11995E-03,  0.13327E-03,  0.13327E-03,  0.41658E-02,  0.41513E-02,  0.00000E+00,  0.00000E+00,  0.11995E-03,  0.00000E+00 /)

      do lu = 1, NLU
         ! No leaves for water, urban, desert:
         LAI_present = .not. any(lu == (/ 6, 7, 9 /))
         call rc_gstom(comp_id,lu,lai,glrad,sinphi,t,rh,diffc,gstom_(lu), LAI_present)
      enddo
      call assertEqual(gstom_ref,gstom_,tol_abs,'stomatal conductance [m/s];')


      comp_id = i_NH3; lai = 2.0; glrad = 100; sinphi = 0.5; t = 12.0; rh = 90;
      call rc_getpar(comp_id,diffc,rsoil_wet,rsoil_frozen,ipar_snow,rsoil)

                 !          grass          arable           perm.          conif.          decid.           water          urban            other       desert
                 !                           land           crops          forest          forest
      gstom_ref = (/   0.11731E-03,  0.13034E-03,  0.13034E-03,  0.40595E-02,  0.40454E-02,  0.00000E+00,  0.00000E+00,  0.11731E-03,  0.00000E+00 /)

      do lu = 1, NLU
         ! No leaves for water, urban, desert:
         LAI_present = .not. any(lu == (/ 6, 7, 9 /))
         call rc_gstom(comp_id,lu,lai,glrad,sinphi,t,rh,diffc,gstom_(lu), LAI_present)
      enddo
      call assertEqual(gstom_ref,gstom_,tol_abs,'stomatal conductance [m/s];')

   end subroutine tst_rc_gstom

   ! test water temperature
   subroutine tst_rc_temp_water
   use no_pfunit
   use m_depac_private
   implicit none

      integer           :: day_of_year  ! day of year, 1 ... 365 (366)

      real              :: tk(12)       ! water temperature (K)

      real    :: tol_abs = 0.1   ! absolute tolerance [K]
      integer :: mnt             ! index of month
      real, parameter :: Tref(12) = &    ! reference temperature [K]
             (/ 277.9361,  278.3873,  280.8721,  284.7424,  288.9887,  292.5036,  294.3703,  294.1018,  291.7683,  287.9783,  283.7204,  280.1051 /) ! correct data
      ! Reference temperature from S:\OPS\Users\sauterf\doc\matlab\tst_sea_temperature.m:
      ! Tref = (/ 293.6366,  290.9890,  280.2408,  279.5726,  290.1146,  294.0351,  284.7026,  277.9029,  285.1378,  294.1694,  289.7208,  279.3168 /) ! old data with bug

      ! Loop over months and compute water temperature:
      do mnt = 1, 12
         day_of_year = mnt*30-15 
         call rc_temp_water(day_of_year,tk(mnt))
      enddo

      call assertEqual(Tref,tk,tol_abs,' water temperature [K];')

   end subroutine tst_rc_temp_water

   subroutine tst_rw_constant
   use no_pfunit
   use m_depac_private
   use m_ops_utils, only: is_missing
   implicit none

      real :: rw_val ! constant value of Rw
      real :: gw     ! wernal leaf conductance (m/s)
      logical :: SAI_present
      rw_val = 2000. ! NO2

      SAI_present = .FALSE.
      call rw_constant(rw_val, gw, SAI_present)
      call assertEqual(0.,gw,'External leaf cond. no vegetation')

      SAI_present = .TRUE.
      call rw_constant(rw_val, gw, SAI_present)
      call assertEqual(1./rw_val,gw,'External leaf cond. with vegetation')

      ! missing data
      rw_val = -999.
      call rw_constant(rw_val, gw, SAI_present)
      call assertTrue(is_missing(gw), 'External leaf cond., missing data')

   end subroutine tst_rw_constant

   subroutine tst_rc_get_vpd
   use no_pfunit
   use m_depac_private
   implicit none

      ! Input/output variables:
      real :: temp          ! temperature (C)
      real :: relh          ! relative humidity (%)
      real :: vpd           ! vapour pressure deficit (kPa)
      real :: tol = 1.0e-5  ! tolerance for equality of reals

      !--------------------------------------
      ! Reference value 2021-06-23: 0.5285050
      !--------------------------------------
      temp = 22.; relh = 80.
      call rc_get_vpd(temp, relh, vpd)
      call assertEqual(0.5285050, vpd, tol, 'Test 1, vpd:')
      !--------------------------------------
      ! Reference value 2021-06-23: 0.4102946
      !--------------------------------------
      temp = 8.5; relh = 63.
      call rc_get_vpd(temp, relh, vpd)
      call assertEqual(0.4102946, vpd, tol, 'Test 2, vpd:')

   end subroutine tst_rc_get_vpd

   SUBROUTINE tst_rc_gstom_emb
   use no_pfunit
   use m_depac_private
   use binas, only: pi
   implicit none

      ! Input/output variables:
      integer :: lu        ! land use type, lu = 1,...,nlu
      real    :: glrad     ! global radiation (W/m2)
      real    :: T         ! temperature (C)
      real    :: relh      ! relative humidity (%)
      real    :: vpd       ! vapour pressure deficit (kPa)
      real    :: lai       ! one-sided leaf area index
      real    :: sinphi    ! sin of solar elevation angle
      real    :: gstom     ! stomatal conductance (m/s)
      real    :: gstom2    ! stomatal conductance (m/s), copy for testing
      real    :: tol = 1.0e-5  ! tolerance for equality of reals
      logical :: LAI_present
      !--------------------------------------
      ! No LAI -> 0.
      !--------------------------------------
      LAI_present = .FALSE.; lu = 1; glrad = 340.
      T = 15.; relh = 75.; lai = 0.; sinphi = SIN(pi/3.)
      call rc_get_vpd(T,relh,vpd)
      call rc_gstom_emb(lu,glrad,t,vpd,lai,sinphi,gstom, LAI_present)
      call assertEqual(0., gstom, tol, 'Test 1, gstom:')

      !--------------------------------------
      ! Reference value 2021-06-23: +0.4725924E-02
      !--------------------------------------
      LAI_present = .TRUE.; lu = 1; glrad = 340.
      T = 15.; relh = 75.; lai = 2.; sinphi = SIN(pi/3.)
      call rc_get_vpd(T,relh,vpd)
      call rc_gstom_emb(lu,glrad,t,vpd,lai,sinphi,gstom, LAI_present)
      call assertEqual(0.4725924E-02, gstom, tol, 'Test 2, gstom:')

      !--------------------------------------
      ! Negative sin(phi), then should be close to value of phi=0
      !--------------------------------------
      LAI_present = .TRUE.; lu = 1; glrad = 340.
      T = 15.; relh = 75.; lai = 2.; sinphi = SIN(-pi/3.)
      call rc_get_vpd(T,relh,vpd)
      call rc_gstom_emb(lu,glrad,t,vpd,lai,sinphi,gstom, LAI_present)
      call rc_gstom_emb(lu,glrad,t,vpd,lai,0.,gstom2, LAI_present)
      call assertEqual(gstom2, gstom, tol, 'Test 3, small solar elev., gstom:')

      !--------------------------------------
      ! glrad > 200 and LAI > 2.5
      ! Reference value 2021-06-23: +0.7257645E-02
      !--------------------------------------
      LAI_present = .TRUE.; lu = 1; glrad = 340.
      T = 15.; relh = 75.; lai = 8.3; sinphi = SIN(pi/3.)
      call rc_get_vpd(T,relh,vpd)
      call rc_gstom_emb(lu,glrad,t,vpd,lai,sinphi,gstom, LAI_present)
      call assertEqual(0.7257645E-02, gstom, tol, 'Test 4, gstom:')

   END SUBROUTINE tst_rc_gstom_emb

   subroutine tst_rc_gsoil_eff
   use no_pfunit
   use m_depac_private
   use m_commonconst_lib, only: i_HNO3, i_NO, i_NO2, i_O3, i_SO2, i_NH3
   implicit none

      ! Input/output variables:
      real, parameter :: sai = 3.5 ! surface area index
      real, parameter :: ust = 0.2 ! friction velocity (m/s)

      integer :: lu             ! land use type, lu = 1,...,nlu
      integer :: nwet           ! index for wetness
                                ! nwet=0 -> dry; nwet=1 -> wet; nwet=9 -> snow
                                ! N.B. this routine cannot be called with nwet = 9,
                                ! nwet = 9 should be handled outside this routine.
      real    :: t              ! temperature (C)
      real    :: rsoil(nlu)     ! soil resistance, dry soil (s/m)
      real    :: rsoil_wet      ! soil resistance, wet soil  (s/m)
      real    :: rsoil_frozen   ! soil resistance, frozen soil  (s/m)
      real    :: gsoil_eff      ! effective soil conductance (m/s)
      integer :: comp_id       ! component id
      real    :: diffc         ! diffusion coefficient of the gas involved
      integer :: ipar_snow     ! parameterisation in case of snow:
      real    :: tol = 1.0e-5  ! tolerance for equality of reals

      !------------------------------------------------
      ! Test 1: dry, value 2021-06-23: +0.2898551E-02
      !------------------------------------------------

      comp_id = i_NH3; lu = 2; nwet = 0; t = 22.5
      call rc_getpar(comp_id,diffc,rsoil_wet,rsoil_frozen,ipar_snow,rsoil)
      call rc_gsoil_eff(lu,sai,ust,nwet,t,rsoil,rsoil_wet,rsoil_frozen,gsoil_eff)
      call assertEqual(0.2898551E-02,gsoil_eff,tol,'Test 1, gsoil_eff:')
      
      !------------------------------------------------
      ! Test 2: lu = 1 (grass) -> 0.0
      !------------------------------------------------
      comp_id = i_NH3; lu = 1; nwet = 0; t = 22.5
      call rc_getpar(comp_id,diffc,rsoil_wet,rsoil_frozen,ipar_snow,rsoil)
      call rc_gsoil_eff(lu,sai,ust,nwet,t,rsoil,rsoil_wet,rsoil_frozen,gsoil_eff)
      call assertEqual(0.,gsoil_eff,tol,'Test 2, grass, gsoil_eff=0:')
      
      !------------------------------------------------
      ! Test 3: wet, value 2021-06-23: +0.3921569E-02
      !------------------------------------------------
      comp_id = i_NH3; lu = 2; nwet = 1; t = 22.5
      call rc_getpar(comp_id,diffc,rsoil_wet,rsoil_frozen,ipar_snow,rsoil)
      call rc_gsoil_eff(lu,sai,ust,nwet,t,rsoil,rsoil_wet,rsoil_frozen,gsoil_eff)
      call assertEqual(0.3921569E-02,gsoil_eff,tol,'Test 3, gsoil_eff:')
      
      !------------------------------------------------
      ! Test 4: frozen, value 2021-06-23: +0.8032129E-03
      !------------------------------------------------
      comp_id = i_NH3; lu = 2; nwet = 1; t = -8.5
      call rc_getpar(comp_id,diffc,rsoil_wet,rsoil_frozen,ipar_snow,rsoil)
      call rc_gsoil_eff(lu,sai,ust,nwet,t,rsoil,rsoil_wet,rsoil_frozen,gsoil_eff)
      call assertEqual(0.8032129E-03,gsoil_eff,tol,'Test 4, gsoil_eff:')
      
      !------------------------------------------------
      ! Test 6: dry, NO, water
      !------------------------------------------------
      comp_id = i_NO; lu = 6; nwet = 0; t = 22.5
      call rc_getpar(comp_id,diffc,rsoil_wet,rsoil_frozen,ipar_snow,rsoil)
      call rc_gsoil_eff(lu,sai,ust,nwet,t,rsoil,rsoil_wet,rsoil_frozen,gsoil_eff)
      call assertEqual(0.50E-03,gsoil_eff,tol,'Test 6, gsoil_eff:')

      !------------------------------------------------
      ! Test 7: dry, NO2
      !------------------------------------------------
      comp_id = i_NO2; lu = 2; nwet = 0; t = 22.5
      call rc_getpar(comp_id,diffc,rsoil_wet,rsoil_frozen,ipar_snow,rsoil)
      call rc_gsoil_eff(lu,sai,ust,nwet,t,rsoil,rsoil_wet,rsoil_frozen,gsoil_eff)
      call assertEqual(0.8032E-03,gsoil_eff,tol,'Test 7, gsoil_eff:')
      
      !------------------------------------------------
      ! Test 8: wet, NO2
      !------------------------------------------------
      comp_id = i_NO2; lu = 2; nwet = 1; t = 22.5
      call rc_getpar(comp_id,diffc,rsoil_wet,rsoil_frozen,ipar_snow,rsoil)
      call rc_gsoil_eff(lu,sai,ust,nwet,t,rsoil,rsoil_wet,rsoil_frozen,gsoil_eff)
      call assertEqual(0.4454E-03,gsoil_eff,tol,'Test 8, gsoil_eff:')
      
      !------------------------------------------------
      ! Test 9: dry, O3
      !------------------------------------------------
      comp_id = i_O3; lu = 2; nwet = 0; t = 22.5
      call rc_getpar(comp_id,diffc,rsoil_wet,rsoil_frozen,ipar_snow,rsoil)
      call rc_gsoil_eff(lu,sai,ust,nwet,t,rsoil,rsoil_wet,rsoil_frozen,gsoil_eff)
      call assertEqual(0.2247E-02,gsoil_eff,tol,'Test 9, gsoil_eff:')
      
      !------------------------------------------------
      ! Test 10: wet, O3
      !------------------------------------------------
      comp_id = i_O3; lu = 2; nwet = 1; t = 22.5
      call rc_getpar(comp_id,diffc,rsoil_wet,rsoil_frozen,ipar_snow,rsoil)
      call rc_gsoil_eff(lu,sai,ust,nwet,t,rsoil,rsoil_wet,rsoil_frozen,gsoil_eff)
      call assertEqual(0.4454E-03,gsoil_eff,tol,'Test 10, gsoil_eff:')
      
      !------------------------------------------------
      ! Test 11: dry, SO2
      !------------------------------------------------
      comp_id = i_SO2; lu = 2; nwet = 0; t = 22.5
      call rc_getpar(comp_id,diffc,rsoil_wet,rsoil_frozen,ipar_snow,rsoil)
      call rc_gsoil_eff(lu,sai,ust,nwet,t,rsoil,rsoil_wet,rsoil_frozen,gsoil_eff)
      call assertEqual(0.8032E-03,gsoil_eff,tol,'Test 11, gsoil_eff:')
      
      !------------------------------------------------
      ! Test 12: wet, SO2
      !------------------------------------------------
      comp_id = i_SO2; lu = 2; nwet = 1; t = 22.5
      call rc_getpar(comp_id,diffc,rsoil_wet,rsoil_frozen,ipar_snow,rsoil)
      call rc_gsoil_eff(lu,sai,ust,nwet,t,rsoil,rsoil_wet,rsoil_frozen,gsoil_eff)
      call assertEqual(0.3922E-02,gsoil_eff,tol,'Test 12, gsoil_eff:')

   end subroutine tst_rc_gsoil_eff

   subroutine tst_rc_rinc
   use no_pfunit
   use m_depac_private
   use m_ops_utils, only: is_missing
   implicit none

      ! Input/output variables:
      real, parameter :: sai = 3.5 ! surface area index
      integer :: lu          ! land use class, lu = 1, ..., nlu
      real    :: ust         ! friction velocity (m/s)
      real    :: rinc        ! in canopy resistance (s/m)
      real    :: rinc2        ! in canopy resistance (s/m), second copy for testing
      real    :: tol = 1.0e-5  ! tolerance for equality of reals

      !------------------------------------------------
      ! Test 1: b(lu)>0, ust>0, value 2021-06-23: 245.
      !------------------------------------------------
      lu = 2; ust = 0.2
      call rc_rinc(lu,sai,ust,rinc)
      call assertEqual(245.,rinc,tol,'Test 1, rinc:')
      !------------------------------------------------
      ! Test 2: b(lu)>0, ust=0 -> 1000.
      !------------------------------------------------
      lu = 2; ust = 0.
      call rc_rinc(lu,sai,ust,rinc)
      call assertEqual(1000.,rinc,tol,'Test 2, rinc:')
      !------------------------------------------------
      ! Test 3: b(lu)<0, lu==1 or lu==8 -> missing
      !------------------------------------------------
      ust = 0.
      call rc_rinc(1,sai,ust,rinc)
      call rc_rinc(8,sai,ust,rinc2)
      call assertEqual(rinc2,rinc,tol,'Test 3, rinc missing for lu=1,8:')
      call assertTrue(is_missing(rinc),'Test 3, rinc missing for lu=1:')
      !------------------------------------------------
      ! Test 4: b(lu)<0, lu!=1 or 8 -> 0.0
      !------------------------------------------------
      ust = 0.
      call rc_rinc(6,sai,ust,rinc)
      call assertEqual(0.,rinc,tol,'Test 4, rinc=0:')

   end subroutine tst_rc_rinc

   subroutine tst_rc_rctot
   use no_pfunit
   use m_depac_private
   use m_ops_utils, only: is_missing
   implicit none

      ! Input/output variables:
      real :: gstom        ! stomatal conductance (s/m)
      real :: gsoil_eff    ! effective soil conductance (s/m)
      real :: gw           ! external leaf conductance (s/m)
      real :: gc_tot       ! total canopy conductance (m/s)
      real :: rc_tot       ! total canopy resistance Rc (s/m)
      real    :: tol = 1.0e-5  ! tolerance for equality of reals

      !------------------------------------------------
      ! Test 1: normal case
      !------------------------------------------------
      gstom = 0.5e-2; gsoil_eff = 0.3e-2; gw = 0.2
      call rc_rctot(gstom,gsoil_eff,gw,gc_tot,rc_tot)
      call assertEqual(1./(gstom+gsoil_eff+gw),rc_tot,tol,'Test 1, rc_tot:')
      call assertEqual(gstom+gsoil_eff+gw,gc_tot,tol,'Test 1, gc_tot:')

      !------------------------------------------------
      ! Test 2: gc_tot negative
      !------------------------------------------------
      gstom = -0.3; gsoil_eff = 0.3e-2; gw = 0.2
      call rc_rctot(gstom,gsoil_eff,gw,gc_tot,rc_tot)
      call assertEqual(gstom+gsoil_eff+gw,gc_tot,tol,'Test 2, gc_tot:')
      call assertEqual(-9999., rc_tot, tol,'Test 2, rc_tot missing')

      !------------------------------------------------
      ! Test 3: gw negative, gc_tot positive
      !------------------------------------------------
      gstom = 0.5e-2; gsoil_eff = 0.3e-2; gw = -0.1e-2
      call rc_rctot(gstom,gsoil_eff,gw,gc_tot,rc_tot)
      call assertGreaterThan(gc_tot,0.0,message='Test 3, gc_tot should be positive, so that test 3 != test 2')
      call assertEqual(gstom+gsoil_eff+gw,gc_tot,tol,'Test 3, gc_tot:')
      call assertEqual(-9999., rc_tot, tol,'Test 3, rc_tot missing')

   end subroutine tst_rc_rctot

   SUBROUTINE tst_par_dir_diff
   use no_pfunit
   use m_depac_private
   use binas, only: pi
   implicit none
      real :: glrad         ! global radiation (W m-2)
      real :: sinphi        ! sine of the solar elevation
      real :: pres          ! actual pressure (to correct for height) (Pa)
      real :: pres_0        ! pressure at sea level (Pa)
      ! outputs
      real :: par_dir       ! PAR direct : visible (photoactive) direct beam radiation (W m-2)
      real :: par_diff      ! PAR diffuse: visible (photoactive) diffuse radiation (W m-2)

      real :: tol = 1.0e-4  ! tolerance for equality of reals

      !-----------------------------------------------
      ! Test 1, average, 2020-06-23: par_dir = 9.870549; par_diff = 127.9842
      !-----------------------------------------------
      glrad = 300.; sinphi = SIN(pi/3.); pres = 0.95e5; pres_0 = 1e5
      call par_dir_diff(glrad,sinphi,pres,pres_0,par_dir,par_diff)
      call assertGreaterThan(glrad, par_dir+par_diff, message='Test 1: sum of both < total')
      call assertEqual(9.870074,par_dir,tol,'Test 1, par_dir:')
      call assertEqual(127.9847,par_diff,tol,'Test 1, par_diff:')

      !-----------------------------------------------
      ! Test 2, sunny, 2020-06-23: par_dir = 452.9669; par_diff = 34.81323
      !-----------------------------------------------
      glrad = 1100.; sinphi = SIN(pi/2.); pres = 0.95e5; pres_0 = 1e5
      call par_dir_diff(glrad,sinphi,pres,pres_0,par_dir,par_diff)
      call assertGreaterThan(glrad, par_dir+par_diff, message='Test 2: sum of both < total')
      call assertEqual(452.9669,par_dir,tol,'Test 2, par_dir:')
      call assertEqual(34.81323,par_diff,tol,'Test 2, par_diff:')

      !----------------------------------------------------------------------------
      ! Test 3, night time, sinphi = 0.0001 in rc_gstom_emb (otherwise if 0 -> NaN)
      !----------------------------------------------------------------------------
      glrad = 0.; sinphi = 0.0001; pres = 0.95e5; pres_0 = 1e5
      call par_dir_diff(glrad,sinphi,pres,pres_0,par_dir,par_diff)
      call assertEqual(0.,par_dir,tol,'Test 3, par_dir:')
      call assertEqual(0.,par_diff,tol,'Test 3, par_diff:')

   END SUBROUTINE tst_par_dir_diff

   subroutine tst_rc_snow
   use no_pfunit
   use m_depac_private
   implicit none

      ! Input/output variables:
      integer :: ipar_snow  ! choice of parameterisation in case of snow
                            ! ipar_snow = 1 : constant parameterisation (=rssnow)
                            ! ipar_snow = 2 : temperature dependent parameterisation
      real    :: t          ! temperature (C)
      real    :: rc_tot     ! total canopy resistance Rc (s/m) = output

      real :: tol = 1.0e-5  ! tolerance for equality of reals

      !----------------------------------------------------------------------------
      ! Test 1, constant parameterisation, rssnow = 2000.
      !----------------------------------------------------------------------------
      ipar_snow=1; t=-5.0
      call rc_snow(ipar_snow,t,rc_tot)
      call assertEqual(2000.,rc_tot,tol,'Test 1, rc_tot:')

      !----------------------------------------------------------------------------
      ! Test 2, temperature dependent, T < -1, rc_tot = 500.
      !----------------------------------------------------------------------------
      ipar_snow=2; t=-5.0
      call rc_snow(ipar_snow,t,rc_tot)
      call assertEqual(500.,rc_tot,tol,'Test 2, rc_tot:')

      !----------------------------------------------------------------------------
      ! Test 3, temperature dependent, T > 1, rc_tot = 70.
      !----------------------------------------------------------------------------
      ipar_snow=2; t=5.0
      call rc_snow(ipar_snow,t,rc_tot)
      call assertEqual(70.,rc_tot,tol,'Test 3, rc_tot:')

      !----------------------------------------------------------------------------
      ! Test 4, temperature dependent, -1 <= T <= 1, rc_tot = 70.*(2.-t)
      !----------------------------------------------------------------------------
      ipar_snow=2
      call rc_snow(ipar_snow,0.0,rc_tot)
      call assertEqual(140.,rc_tot,tol,'Test 4.1, rc_tot:')
      call rc_snow(ipar_snow,0.5,rc_tot)
      call assertEqual(105.,rc_tot,tol,'Test 4.2, rc_tot:')
      call rc_snow(ipar_snow,-0.5,rc_tot)
      call assertEqual(175.,rc_tot,tol,'Test 4.3, rc_tot:')

   end subroutine tst_rc_snow


   subroutine tst_rw_so2
   use no_pfunit
   use m_depac_private
   implicit none

      ! Input/output variables:
      real    t      ! temperature (C)
      integer nwet   ! wetness indicator; nwet=0 -> dry; nwet=1 -> wet; nwet=9 -> snow
      real    rh     ! relative humidity (%)
      integer iratns ! index for NH3/SO2 ratio;
                     ! iratns = 1: low NH3/SO2
                     ! iratns = 2: high NH3/SO2
                     ! iratns = 3: very low NH3/SO2
      real    gw     ! external leaf conductance (m/s) = output

      real :: tol = 1.0e-5  ! tolerance for equality of reals
      logical :: SAI_present
      !----------------------------------------------------------------------------
      ! Test 1: dry, 2021-06-23, gw = 0.7232787E-02
      !----------------------------------------------------------------------------
      SAI_present = .TRUE.; t = 15.; nwet = 0; rh = 75.; iratns = 1
      call rw_so2(t,nwet,rh,iratns,gw, SAI_present)
      call assertEqual(0.7232787E-02, gw, tol, 'Test 1: gw:')

      !----------------------------------------------------------------------------
      ! Test 2: test 1 for very low NH3/SO2, use same ref value as test 1, rw+50.
      !----------------------------------------------------------------------------
      SAI_present = .TRUE.; t = 15.; nwet = 0; rh = 75.; iratns = 3
      call rw_so2(t,nwet,rh,iratns,gw, SAI_present)
      call assertEqual(1./(1./0.7232787E-02+50.), gw, tol, 'Test 2: gw:')

      !----------------------------------------------------------------------------
      ! Test 3: wet / snow, so rw = 10.
      !----------------------------------------------------------------------------
      SAI_present = .TRUE.; t = 15.; rh = 75.; iratns = 1
      call rw_so2(t,1,rh,iratns,gw, SAI_present)
      call assertEqual(0.1, gw, tol, 'Test 3: gw (wet):')
      call rw_so2(t,9,rh,iratns,gw, SAI_present)
      call assertEqual(0.1, gw, tol, 'Test 3: gw (snow):')

      !----------------------------------------------------------------------------
      ! Test 4: dry, humid, 2021-06-23, gw = 0.2398121E-01
      !----------------------------------------------------------------------------
      SAI_present = .TRUE.; t = 15.; nwet = 0; rh = 85.; iratns = 1
      call rw_so2(t,nwet,rh,iratns,gw, SAI_present)
      call assertEqual(0.2398121E-01, gw, tol, 'Test 4: gw:')

      !----------------------------------------------------------------------------
      ! Test 5: dry, freezing (T < -1 C)
      !----------------------------------------------------------------------------
      SAI_present = .TRUE.; nwet = 0; rh = 85.; iratns = 1
      call rw_so2(-1.5,nwet,rh,iratns,gw, SAI_present)
      call assertEqual(1./200., gw, tol, 'Test 5: gw(T=-1.5):')
      call rw_so2(-8.,nwet,rh,iratns,gw, SAI_present)
      call assertEqual(1./500., gw, tol, 'Test 5: gw(T=-8):')

      !----------------------------------------------------------------------------
      ! Test 6: no vegetation
      !----------------------------------------------------------------------------
      SAI_present = .FALSE.; t = 15.; nwet = 0; rh = 85.; iratns = 1
      call rw_so2(t,nwet,rh,iratns,gw, SAI_present)
      call assertEqual(0., gw, tol, 'Test 6: gw, no vegetation:')

   end subroutine tst_rw_so2

   subroutine tst_rw_nh3_sutton
   use no_pfunit
   use m_depac_private
   implicit none

      ! Input/output variables:
      real :: tsurf     ! surface temperature (C)
      real :: rh        ! relative humidity (%)
      real :: gw        ! external leaf conductance (m/s)

      real :: tol = 1.0e-5  ! tolerance for equality of reals
      logical :: SAI_present
      !----------------------------------------------------------------------------
      ! Test 1: no vegetation
      !----------------------------------------------------------------------------
      SAI_present = .FALSE.; tsurf = 15.; rh = 85.
      call rw_nh3_sutton(tsurf,rh,gw, SAI_present)
      call assertEqual(0., gw, tol, 'Test 1: gw, no vegetation:')

      !----------------------------------------------------------------------------
      ! Test 2: frozen soil
      !----------------------------------------------------------------------------
      SAI_present = .TRUE.; tsurf = -5.; rh = 85.
      call rw_nh3_sutton(tsurf,rh,gw, SAI_present)
      call assertEqual(1./200., gw, tol, 'Test 2: gw, frozen:')

      !----------------------------------------------------------------------------
      ! Test 3: ref 2021-06-23: 0.4092926E-01
      !----------------------------------------------------------------------------
      SAI_present = .TRUE.; tsurf = 15.; rh = 85.
      call rw_nh3_sutton(tsurf,rh,gw, SAI_present)
      call assertEqual(0.4092926E-01, gw, tol, 'Test 3: gw:')

   end subroutine tst_rw_nh3_sutton

   subroutine tst_rc_comp_point
   use no_pfunit
   use m_depac_private
   use m_commonconst_lib, only: i_HNO3, i_NO, i_NO2, i_O3, i_SO2, i_NH3
   implicit none

      ! Input/output variables:
      integer :: comp_id ! component id
      integer :: lu           ! land use type, lu = 1,...,9
      integer :: day_of_year  ! day of year
      real :: t            ! temperature (C)
      real :: catm         ! actual atmospheric concentration (ug/m3)
      real :: c_ave_prev_nh3   ! air concentration averaged over a previous
                                                        ! period (e.g. previous year or month) (ug/m3)
      real :: c_ave_prev_so2   ! air concentration averaged over a previous
                                                        ! period (e.g. previous year or month) (ug/m3)
      real, parameter :: gamma_soil_water_fac = 430. ! user supplied value for gamma_soil (as in LT version)
      real :: gw           ! external leaf conductance (m/s)
      real :: gstom        ! stomatal conductance (m/s)
      real :: gsoil_eff    ! effective soil conductance (m/s)
      real :: gc_tot       ! total canopy conductance (m/s)
      real :: ccomp_tot    ! total compensation point (weighed average of
                                                    ! separate compensation points) (ug/m3)

      ! Variables from module:
      ! gamma_stom_c_fac: factor in linear relation between gamma_stom and NH3 air concentration.
      ! LAI_present or SAI_present: vegetation is present

      real, parameter :: tol = 1.0e-5  ! tolerance for equality of reals
      logical :: LAI_present, SAI_present
      !----------------------------------------------------------------------------
      ! Test 1: for compnam != NH3, ccomp_tot = 0
      !----------------------------------------------------------------------------
       LAI_present= .false.; SAI_present = .false.; 
      lu = 1; day_of_year = 231; t = 22.5; catm = 5.3; c_ave_prev_nh3 = 7.9;
      c_ave_prev_so2 = 0.9; gw = 0.05; gstom = 0.05; gsoil_eff = 0.03; gc_tot = gstom+gsoil_eff+gw
      call rc_comp_point(i_NO,lu,day_of_year,t,catm,c_ave_prev_nh3,c_ave_prev_so2,gamma_soil_water_fac,gw,gstom,gsoil_eff,gc_tot,ccomp_tot, LAI_present, SAI_present)
      call assertEqual(0., ccomp_tot, tol, 'Test 1: ccomp_tot for NO = 0:')
      call rc_comp_point(i_NO2,lu,day_of_year,t,catm,c_ave_prev_nh3,c_ave_prev_so2,gamma_soil_water_fac,gw,gstom,gsoil_eff,gc_tot,ccomp_tot, LAI_present, SAI_present)
      call assertEqual(0., ccomp_tot, tol, 'Test 1: ccomp_tot for NO2 = 0:')
      call rc_comp_point(i_O3,lu,day_of_year,t,catm,c_ave_prev_nh3,c_ave_prev_so2,gamma_soil_water_fac,gw,gstom,gsoil_eff,gc_tot,ccomp_tot, LAI_present, SAI_present)
      call assertEqual(0., ccomp_tot, tol, 'Test 1: ccomp_tot for O3 = 0:')
      call rc_comp_point(i_SO2,lu,day_of_year,t,catm,c_ave_prev_nh3,c_ave_prev_so2,gamma_soil_water_fac,gw,gstom,gsoil_eff,gc_tot,ccomp_tot, LAI_present, SAI_present)
      call assertEqual(0., ccomp_tot, tol, 'Test 1: ccomp_tot for SO2 = 0:')
      
      !----------------------------------------------------------------------------
      ! Test 2: decid. forest, 2021-06-23 = 14.27670
      !----------------------------------------------------------------------------
      lu = 5; LAI_present= .TRUE.; SAI_present = .TRUE.; 
      comp_id = i_NH3; day_of_year = 231; t = 22.5; catm = 15.3; c_ave_prev_nh3 = 17.;
      c_ave_prev_so2 = 0.1; gw = 0.05; gstom = 0.05; gsoil_eff = 0.03; gc_tot = gstom+gsoil_eff+gw
      call rc_comp_point(comp_id,lu,day_of_year,t,catm,c_ave_prev_nh3,c_ave_prev_so2,gamma_soil_water_fac,gw,gstom,gsoil_eff,gc_tot,ccomp_tot, LAI_present, SAI_present)
      call assertRelativelyEqual(14.27670, ccomp_tot, tol, 'Test 2: ccomp_tot:')
      
      !----------------------------------------------------------------------------
      ! Test 3: no prev. concentrations -> 0
      !----------------------------------------------------------------------------
      lu = 5; LAI_present = .TRUE.; SAI_present = .TRUE.; 
      comp_id = i_NH3; day_of_year = 231; t = 22.5; catm = 5.3; c_ave_prev_nh3 = 0.;
      c_ave_prev_so2 = 0.; gw = 0.05; gstom = 0.05; gsoil_eff = 0.03; gc_tot = gstom+gsoil_eff+gw
      call rc_comp_point(comp_id,lu,day_of_year,t,catm,c_ave_prev_nh3,c_ave_prev_so2,gamma_soil_water_fac,gw,gstom,gsoil_eff,gc_tot,ccomp_tot,LAI_present, SAI_present)
      call assertEqual(0., ccomp_tot, tol, 'Test 3: ccomp_tot:')
      
      !----------------------------------------------------------------------------
      ! Test 4: water,
      !                2021-07-13 = 0.3932707 (bugfix water temperature)
      !----------------------------------------------------------------------------
      lu = 6; LAI_present = .FALSE.; SAI_present = .FALSE.; 
      comp_id = i_NH3; day_of_year = 231; t = 22.5; catm = 5.3; c_ave_prev_nh3 = 7.9;
      c_ave_prev_so2 = 0.9; gw = 0.05; gstom = 0.05; gsoil_eff = 0.03; gc_tot = gstom+gsoil_eff+gw
      call rc_comp_point(comp_id,lu,day_of_year,t,catm,c_ave_prev_nh3,c_ave_prev_so2,gamma_soil_water_fac,gw,gstom,gsoil_eff,gc_tot,ccomp_tot,LAI_present, SAI_present)
      call assertEqual(.3932707, ccomp_tot, tol, 'Test 4: ccomp_tot:')

      ! temperature should be set in rc_temp_water -> no change
      call rc_temp_water(day_of_year,t)
      call assertGreaterThan(ABS(t-22.5), 0.0, message='Test 4: sea temperature different from 22.5C')
      call rc_comp_point(comp_id,lu,day_of_year,t,catm,c_ave_prev_nh3,c_ave_prev_so2,gamma_soil_water_fac,gw,gstom,gsoil_eff,gc_tot,ccomp_tot,LAI_present, SAI_present)
      call assertEqual(.3932707, ccomp_tot, tol, message='Test 4: ccomp_tot different T:')
      
      ! if gamma_soil_water_fac is negative but the same value, there should be a
      ! change because c_ave_prev > 0
      call rc_comp_point(comp_id,lu,day_of_year,t,catm,c_ave_prev_nh3,c_ave_prev_so2,-gamma_soil_water_fac,gw,gstom,gsoil_eff,gc_tot,ccomp_tot,LAI_present, SAI_present)
      call assertRelativelyEqual(3.106839, ccomp_tot, tol, message='Test 4: gamma_soil_water_fac < 0:')
      
      !----------------------------------------------------------------------------
      ! Test 5: decid. forest, prev. SO2 = 0., 2021-07-13 = 0.3885208
      !----------------------------------------------------------------------------
      comp_id = i_NH3; day_of_year = 180; t = 22.5; catm = 15.3; c_ave_prev_nh3 = 17.;
      c_ave_prev_so2 = 0.0; gw = 0.05; gstom = 0.05; gsoil_eff = 0.03; gc_tot = gstom+gsoil_eff+gw
      call rc_comp_point(comp_id,lu,day_of_year,t,catm,c_ave_prev_nh3,c_ave_prev_so2,gamma_soil_water_fac,gw,gstom,gsoil_eff,gc_tot,ccomp_tot,LAI_present, SAI_present)
      call assertEqual(0.3885208, ccomp_tot, tol, message='Test 5: ccomp_tot:')
      
      !----------------------------------------------------------------------------
      ! Test 6: gamma_soil_water_fac = 0 (no re-emission from water)
      !----------------------------------------------------------------------------
      lu = 6; LAI_present = .FALSE.; SAI_present = .FALSE.; 
      comp_id = i_NH3; day_of_year = 231; t = 22.5; catm = 5.3; c_ave_prev_nh3 = 7.9;
      c_ave_prev_so2 = 0.9; gw = 0.05; gstom = 0.05; gsoil_eff = 0.03; gc_tot = gstom+gsoil_eff+gw
      call rc_comp_point(comp_id,lu,day_of_year,t,catm,c_ave_prev_nh3,c_ave_prev_so2,0*gamma_soil_water_fac,gw,gstom,gsoil_eff,gc_tot,ccomp_tot,LAI_present, SAI_present)
      call assertEqual(0.0, ccomp_tot, tol, message='Test 6: ccomp_tot:')

   end subroutine tst_rc_comp_point

   subroutine tst_rc_comp_point_rc_eff
   use no_pfunit
   use m_depac_private
   use m_ops_utils, only: is_missing
   implicit none

      ! Input/output variables:
      real :: ccomp_tot  ! total compensation point (weighed average of separate compensation points) (ug/m3)
      real :: catm       ! atmospheric concentration (ug/m3)
      real :: ra         ! aerodynamic resistance (s/m)
      real :: rb         ! boundary layer resistance (s/m)
      real :: rc_tot     ! total canopy resistance (s/m)
      real :: rc_eff     ! effective total canopy resistance (s/m)

      real :: tol = 1.0e-4  ! tolerance for equality of reals

      !----------------------------------------------------------------------------
      ! Test 1: 2021-06-24: rc_eff = 187.4281
      !----------------------------------------------------------------------------
      ccomp_tot = 5.3; catm = 8.5; ra = 0.1; rb = 0.8; rc_tot = 70.
      call rc_comp_point_rc_eff(ccomp_tot,catm,ra,rb,rc_tot,rc_eff)
      call assertEqual(187.4281, rc_eff, tol, 'Test 1: rc_eff:')

      !----------------------------------------------------------------------------
      ! Test 2: catm == ccomp_tot -> missing
      !----------------------------------------------------------------------------
      ccomp_tot = 5.3; catm = 5.3; ra = 0.1; rb = 0.8; rc_tot = 70.
      call rc_comp_point_rc_eff(ccomp_tot,catm,ra,rb,rc_tot,rc_eff)
      call assertEqual(-9999., rc_eff,'Test 2, catm == ccomp_tot -> undefined')

   end subroutine tst_rc_comp_point_rc_eff


   subroutine tst_depac
   use no_pfunit
   use m_depac_private
   use m_depac
   use binas, only: pi
   use m_commonconst_lib, only: i_HNO3, i_NO, i_NO2, i_O3, i_SO2, i_NH3
   implicit none

      integer :: comp_id ! component id
                         !  1      2    3    4     5      6
                         ! 'HNO3','NO','NO2','O3','SO2','NH3'
      real    :: rc_tot       ! total canopy resistance Rc (s/m) [output]

      integer, parameter :: day_of_year =213 ! day of year, 1 ... 365 (366)
      real, parameter :: lat = 52.1          ! latitude Northern hemisphere (degrees) (DEPAC cannot be used for S. hemisphere)
      real, parameter :: t = 21.3            ! temperature (C)
                                             ! NB discussion issue is temp T_2m or T_surf of T_leaf?
      real, parameter :: ust =0.5            ! friction velocity (m/s)
      real, parameter :: glrad = 400.0       ! global radiation (W/m2)
      real, parameter :: sinphi =  SIN(pi/3) ! sin of solar elevation angle
      real, parameter :: rh = 83.7           ! relative humidity (%)
      integer, parameter :: nwet = 0         ! wetness indicator; nwet=0 -> dry; nwet=1 -> wet; nwet=9 -> snow
      integer :: lu                          ! land use type, lu = 1,...,nlu
      integer, parameter :: iratns = 2       ! index for NH3/SO2 ratio;
                                             ! iratns = 1: low NH3/SO2
                                             ! iratns = 2: high NH3/SO2
                                             ! iratns = 3: very low NH3/SO2

      ! optional arguments needed only if compensation points are computed
      real, parameter :: c_ave_prev_nh3=8.9  ! air concentration averaged over a previous
                                             ! period (e.g. previous year or month) (ug/m3)
      real, parameter :: c_ave_prev_so2=2.1  ! air concentration averaged over a previous
                                             ! period (e.g. previous year or month) (ug/m3)
      real, parameter :: catm = 7.3          ! actual atmospheric concentration (ug/m3)
      real    :: ccomp_tot    ! total compensation point (ug/m3) [output]

      ! optional arguments needed only if an effective Rc (based on compensation points) is computed;
      ! in this case, also the previous three optional arguments are needed
      real, parameter :: ra = 0.1            ! aerodynamic resistance (s/m)
      real, parameter :: rb = 0.8            ! boundary layer resistance (s/m)
      real    :: rc_eff       ! effective total canopy resistance (s/m) [output]

      real, parameter :: tol = 1.0e-4  ! tolerance for equality of reals
      real, parameter :: gamma_soil_water_fac = 430. ! user supplied value for gamma_soil (as in LT version)
      
      !----------------------------------------------------------------------------
      ! Test 1: with compensation points & Rc ; 2021-06-24: rc_tot = 4.254499,
      !                                                     ccomp_tot = 2.703362,
      !                                                     rc_eff = 7.285950
      !----------------------------------------------------------------------------
      comp_id = i_NH3; lu = 4
      call depac(comp_id, day_of_year, lat, t, ust, glrad, sinphi, rh, nwet, lu, iratns, rc_tot, c_ave_prev_nh3, c_ave_prev_so2, catm, gamma_soil_water_fac, ccomp_tot, ra, rb, rc_eff)
      call assertEqual(4.254499, rc_tot, tol, 'Test 3: rc_tot:')
      call assertEqual(2.703362, ccomp_tot, tol, 'Test 3: ccomp_tot:')
      call assertEqual(7.285950, rc_eff, tol, 'Test 3: rc_eff:')
      
      !----------------------------------------------------------------------------
      ! Test 2: with compensation points & Rc ; 2021-06-24: rc_tot = 10.0,
      !
      !                                                     ccomp_tot = 1.846130,
      !                                                     rc_eff = 10.43043
      !         lu = 6 (water) -> no LAI / SAI
      !----------------------------------------------------------------------------
      comp_id = i_NH3; lu = 6
      call depac(comp_id, day_of_year, lat, t, ust, glrad, sinphi, rh, nwet, lu, iratns, rc_tot, c_ave_prev_nh3, c_ave_prev_so2, catm, gamma_soil_water_fac, ccomp_tot, ra, rb, rc_eff)
      call assertEqual(10.0, rc_tot, tol, 'Test 4: rc_tot:')
      call assertEqual(1.846130, ccomp_tot, tol, 'Test 4: ccomp_tot:')
      call assertEqual(13.68964, rc_eff, tol, 'Test 4: rc_eff:')
      
      !---------------------------------------------------------
      ! Test 3: for SO2
      !---------------------------------------------------------
      comp_id = i_SO2; lu = 2
      call depac(comp_id, day_of_year, lat, t, ust, glrad, sinphi, rh, nwet, lu, iratns, rc_tot, c_ave_prev_nh3, c_ave_prev_so2, catm, gamma_soil_water_fac, ccomp_tot, ra, rb, rc_eff)
      call assertEqual(31.935, rc_tot, tol, 'Test 5: rc_tot:')
      
      !---------------------------------------------------------
      ! Test 4: for NO2
      !---------------------------------------------------------
      comp_id = i_NO2; lu = 2
      call depac(comp_id, day_of_year, lat, t, ust, glrad, sinphi, rh, nwet, lu, iratns, rc_tot, c_ave_prev_nh3, c_ave_prev_so2, catm, gamma_soil_water_fac, ccomp_tot, ra, rb, rc_eff)
      call assertEqual(62.3103, rc_tot, tol, 'Test 6: rc_tot:')
      
      !---------------------------------------------------------
      ! Test 5: for NO
      !---------------------------------------------------------
      comp_id = i_NO; lu = 2
      call depac(comp_id, day_of_year, lat, t, ust, glrad, sinphi, rh, nwet, lu, iratns, rc_tot, c_ave_prev_nh3, c_ave_prev_so2, catm, gamma_soil_water_fac, ccomp_tot, ra, rb, rc_eff)
      call assertEqual(-9999., rc_tot, tol, 'Test 7: rc_tot:')
      
      !---------------------------------------------------------
      ! Test 6: for O3
      !---------------------------------------------------------
      comp_id = i_O3; lu = 2
      call depac(comp_id, day_of_year, lat, t, ust, glrad, sinphi, rh, nwet, lu, iratns, rc_tot, c_ave_prev_nh3, c_ave_prev_so2, catm, gamma_soil_water_fac, ccomp_tot, ra, rb, rc_eff)
      call assertEqual(52.95541, rc_tot, tol, 'Test 8: rc_tot:')
      
      !---------------------------------------------------------
      ! Test 7: for HNO3
      !---------------------------------------------------------
      comp_id = i_HNO3; lu = 2
      call depac(comp_id, day_of_year, lat, t, ust, glrad, sinphi, rh, nwet, lu, iratns, rc_tot, c_ave_prev_nh3, c_ave_prev_so2, catm, gamma_soil_water_fac, ccomp_tot, ra, rb, rc_eff)
      call assertEqual(10., rc_tot, tol, 'Test 9: rc_tot:')

   end subroutine tst_depac
end module m_tst_m_depac


program tst_m_depac
use m_tst_m_depac
use no_pfunit, only: conclusion
implicit none
   call tst_rc_lai()
   call tst_rc_gw()
   call tst_rc_gstom()
   call tst_rc_temp_water()
   call tst_rw_constant()
   call tst_rc_get_vpd()
   call tst_rc_gstom_emb()
   call tst_rc_gsoil_eff()
   call tst_rc_rinc()
   call tst_rc_rctot()
   call tst_par_dir_diff()
   call tst_rc_snow()
   call tst_rw_so2()
   call tst_rw_nh3_sutton()
   call tst_rc_comp_point()
   call tst_rc_comp_point_rc_eff()
   call tst_depac()

   call conclusion()
end program tst_m_depac
