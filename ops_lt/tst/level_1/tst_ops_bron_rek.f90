module m_test_ops_bron_rek
implicit none
contains

   subroutine setup_scratch(mm, x, y, qob, qww, hbron, diameter, sigz0, D_stack, V_stack, Ts_stack, &
      emis_horizontal, ibtg, ibroncat, iland, idgr, building_length, building_width, building_height, building_orientation)
    use m_commonfile, only: fu_scratch
    integer, intent(in) :: mm, ibtg, ibroncat, idgr, iland
    real, intent(in) :: x, y, qob, qww, hbron, diameter, sigz0, D_stack, V_stack, Ts_stack
    real, intent(in) :: building_length, building_width, building_height, building_orientation
    logical, intent(in) :: emis_horizontal
    logical :: exist
    inquire(unit=fu_scratch, exist=exist)
    if (exist) close(fu_scratch)  
    OPEN(fu_scratch, STATUS = 'SCRATCH', FORM = 'UNFORMATTED')
    WRITE (fu_scratch) mm, x, y, qob, qww, hbron, diameter, sigz0, D_stack, V_stack, Ts_stack, &
       emis_horizontal, ibtg, ibroncat, iland, idgr, building_length, building_width, building_height, building_orientation
    REWIND (fu_scratch)
   end subroutine

   subroutine test_ops_bron_rek()
   use no_pfunit_ops_lt
   use m_ops_building, only: TBuildingEffect
   use m_ops_building, only: TBuilding
   use m_ops_bron_rek
   use m_error, only: TError
        integer :: i
        real, parameter :: tol = 1e-5
        real, parameter :: emtrend =   1.00000000E+00
        type(TBuildingEffect) :: buildingEffect
        integer :: landmax
        integer, parameter :: in_landmax = 1
        real, parameter :: in_emis(6,50) = reshape( (/   5.28000000E+02,   0.00000000E+00,   1.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   1.00000000E+00, (0.00000000E+0, i=1, 6*49) /), &
          (/6,50/) )
        real :: emis(6,50)
        integer :: nsbuf
        integer, parameter :: ref_nsbuf = 1
        integer, parameter :: ref_bnr = 1
        integer, parameter :: ref_bx = 155000
        integer, parameter :: ref_by = 385000
        real, parameter :: ref_bdiam =  0.00000000E+00
        real, parameter :: ref_bsterkte =  1.00000000E+00
        real, parameter :: ref_bwarmte =  0.00000000E+00
        real, parameter :: ref_bhoogte =  5.00000000E+00
        real, parameter :: ref_bsigmaz =  0.00000000E+00
        real, parameter :: ref_bD_stack = -9.99000000E+02
        real, parameter :: ref_bV_stack = -9.99000000E+02
        real, parameter :: ref_bTs_stack = -9.99000000E+02
        logical, parameter :: ref_bemis_horizontal = .false.
        integer, parameter :: ref_btgedr = 0
        integer, parameter :: ref_bdegr = 0
        real, parameter :: ref_bqrv =  0.00000000E+00
        real, parameter :: ref_bqtr =  0.00000000E+00
        integer, parameter :: ref_bcatnr =  1
        integer, parameter :: ref_blandnr = 528
        logical :: eof
        logical, parameter :: ref_eof = .true.
        type(TError) :: error
        type(TError) :: ref_error
        type(TBuilding), allocatable :: bbuilding(:), ref_bbuilding(:)
        integer, allocatable :: bnr(:), bx(:), by(:)
        real, allocatable :: bdiam(:), bsterkte(:), bwarmte(:), bhoogte(:), bsigmaz(:), bD_stack(:), bV_stack(:), bTs_stack(:), bqrv(:), bqtr(:)
        logical, allocatable :: bemis_horizontal(:)
        integer, allocatable :: btgedr(:), bdegr(:), bcatnr(:), blandnr(:)
  
        allocate( bbuilding(4000), ref_bbuilding(4000), &
                  bnr(4000), bx(4000), by(4000), &
                  bdiam(4000), bsterkte(4000), bwarmte(4000), bhoogte(4000), bsigmaz(4000), bD_stack(4000), bV_stack(4000), bTs_stack(4000), bqrv(4000), bqtr(4000), &
                  bemis_horizontal(4000), &
                  btgedr(4000), bdegr(4000), bcatnr(4000), blandnr(4000))

        ref_bbuilding(:)%length =   0.00000000E+00
        ref_bbuilding(:)%width =   0.00000000E+00
        ref_bbuilding(:)%height =   0.00000000E+00
        ref_bbuilding(:)%orientation =   0.00000000E+00
        ref_bbuilding(:)%x =   0.00000000E+00
        ref_bbuilding(:)%y =   0.00000000E+00
        ref_bbuilding(:)%type =            0
        buildingEffect%nParam =            0
        buildingEffect%nClass =  [(0.00000000E+00, i=1, 9)]
        buildingEffect%minClass =  [(0.00000000E+00, i=1, 9)]
        buildingEffect%maxClass =  [(0.00000000E+00, i=1, 9)]
        landmax = in_landmax
        emis = in_emis
        ref_error%haserror = .false.
        ref_error%message = ""

        call setup_scratch(1, 155000.0, 385000.0, 1.000000, 0.0000000E+00, 5.000000, 0.0000000E+00, &
                           0.0000000E+00, -999.0000, -999.0000, -999.0000, .false., 0, 1, 528, 0, &
                           -999.0000, -999.0000, -999.0000, -999.0000)
        call ops_bron_rek( emtrend, buildingEffect, landmax, emis, nsbuf, bnr, bx, by, bdiam, bsterkte, bwarmte, bhoogte, &
            bsigmaz, bD_stack, bV_stack, bTs_stack, bemis_horizontal, bbuilding, btgedr, bdegr, bqrv, bqtr, bcatnr, &
            blandnr, eof, error)
         
        call assertEqual( ref_nsbuf, nsbuf, "nsbuf", __LINE__, __FILE__)
        call assertEqual( ref_bnr, bnr(1), "bnr", __LINE__, __FILE__)
        call assertEqual( ref_bx, bx(1), "bx", __LINE__, __FILE__)
        call assertEqual( ref_by, by(1), "by", __LINE__, __FILE__)
        call assertEqual( ref_bdiam, bdiam(1), tol, "bdiam", __LINE__, __FILE__)
        call assertEqual( ref_bsterkte, bsterkte(1), tol, "bsterkte", __LINE__, __FILE__)
        call assertEqual( ref_bwarmte, bwarmte(1), tol, "bwarmte", __LINE__, __FILE__)
        call assertEqual( ref_bhoogte, bhoogte(1), tol, "bhoogte", __LINE__, __FILE__)
        call assertEqual( ref_bsigmaz, bsigmaz(1), tol, "bsigmaz", __LINE__, __FILE__)
        call assertEqual( ref_bD_stack, bD_stack(1), tol, "bD_stack", __LINE__, __FILE__)
        call assertEqual( ref_bV_stack, bV_stack(1), tol, "bV_stack", __LINE__, __FILE__)
        call assertEqual( ref_bTs_stack, bTs_stack(1), tol, "bTs_stack", __LINE__, __FILE__)
        call assertEqual( ref_bemis_horizontal, bemis_horizontal(1), "bemis_horizontal", __LINE__, __FILE__)
        call assertEqual( ref_bbuilding, bbuilding, tol, "bbuilding", __LINE__, __FILE__)
        call assertEqual( ref_btgedr, btgedr(1), "btgedr", __LINE__, __FILE__)
        call assertEqual( ref_bdegr, bdegr(1), "bdegr", __LINE__, __FILE__)
        call assertEqual( ref_bqrv, bqrv(1), tol, "bqrv", __LINE__, __FILE__)
        call assertEqual( ref_bqtr, bqtr(1), tol, "bqtr", __LINE__, __FILE__)
        call assertEqual( ref_bcatnr, bcatnr(1), "bcatnr", __LINE__, __FILE__)
        call assertEqual( ref_blandnr, blandnr(1), "blandnr", __LINE__, __FILE__)
        call assertEqual( ref_eof, eof, "eof", __LINE__, __FILE__)
        call assertEqual( ref_error, error, "ops_bron_rek", __LINE__, __FILE__)
   end subroutine test_ops_bron_rek

   subroutine test_ops_bron_rek_space_heating()
      use no_pfunit_ops_lt
      use m_ops_building, only: TBuildingEffect
      use m_ops_building, only: TBuilding
      use m_ops_bron_rek
      use m_error, only: TError
      
      integer :: i
      real, parameter :: tol = 1e-5
      real, parameter :: emtrend =   1.00000000E+00
      type(TBuildingEffect) :: buildingEffect
      integer :: landmax
      integer, parameter :: in_landmax = 1
      real, parameter :: in_emis(6,50) = reshape( (/   5.28000000E+02,   0.00000000E+00,   1.00000000E+00, &
           0.00000000E+00,   0.00000000E+00,   1.00000000E+00, (0.00000000E+0, i=1, 6*49) /), &
        (/6,50/) )
      real :: emis(6,50)
      integer :: nsbuf
      integer, parameter :: ref_nsbuf = 1
      integer, parameter :: ref_bnr = 1
     integer, parameter :: ref_bx = 155000
      integer, parameter :: ref_by = 385000
     real, parameter :: ref_bdiam =  0.00000000E+00
     real, parameter :: ref_bsterkte =  0.00000000E+00
      real, parameter :: ref_bwarmte =  0.00000000E+00
      real, parameter :: ref_bhoogte =  5.00000000E+00
      real, parameter :: ref_bsigmaz =  0.00000000E+00
      real, parameter :: ref_bD_stack = -9.99000000E+02
      real, parameter :: ref_bV_stack = -9.99000000E+02
      real, parameter :: ref_bTs_stack = -9.99000000E+02
      logical, parameter :: ref_bemis_horizontal = .false.
      integer, parameter :: ref_btgedr = 2
      integer, parameter :: ref_bdegr = 0
      real, parameter :: ref_bqrv =  1.00000000E+00
      real, parameter :: ref_bqtr =  0.00000000E+00
      integer, parameter :: ref_bcatnr = 1
      integer, parameter :: ref_blandnr = 528
      logical :: eof
      logical, parameter :: ref_eof = .true.
      type(TError) :: error
      type(TError) :: ref_error
      type(TBuilding), allocatable :: bbuilding(:), ref_bbuilding(:)
      integer, allocatable :: bnr(:), bx(:), by(:)
      real, allocatable :: bdiam(:), bsterkte(:), bwarmte(:), bhoogte(:), bsigmaz(:), bD_stack(:), bV_stack(:), bTs_stack(:), bqrv(:), bqtr(:)
      logical, allocatable :: bemis_horizontal(:)
      integer, allocatable :: btgedr(:), bdegr(:), bcatnr(:), blandnr(:)

      allocate( bbuilding(4000), ref_bbuilding(4000), &
                bnr(4000), bx(4000), by(4000), &
                bdiam(4000), bsterkte(4000), bwarmte(4000), bhoogte(4000), bsigmaz(4000), bD_stack(4000), bV_stack(4000), bTs_stack(4000), bqrv(4000), bqtr(4000), &
                bemis_horizontal(4000), &
                btgedr(4000), bdegr(4000), bcatnr(4000), blandnr(4000))
      ref_bbuilding(:)%length =   0.00000000E+00
      ref_bbuilding(:)%width =   0.00000000E+00
      ref_bbuilding(:)%height =   0.00000000E+00
      ref_bbuilding(:)%orientation =   0.00000000E+00
      ref_bbuilding(:)%x =   0.00000000E+00
      ref_bbuilding(:)%y =   0.00000000E+00
      ref_bbuilding(:)%type =            0
      buildingEffect%nParam =            0
      buildingEffect%nClass =  [(0.00000000E+00, i=1, 9)]
      buildingEffect%minClass =  [(0.00000000E+00, i=1, 9)]
      buildingEffect%maxClass =  [(0.00000000E+00, i=1, 9)]
      buildingEffect%nClass =  [(0.00000000E+00, i=1, 9)]
      buildingEffect%minClass =  [(0.00000000E+00, i=1, 9)]
      buildingEffect%maxClass =  [(0.00000000E+00, i=1, 9)]
       
      landmax = in_landmax
      emis = in_emis
      ref_error%haserror = .false.
      ref_error%message = ""
   
      call setup_scratch(1, 155000.0, 385000.0, 1.000000, 0.0000000E+00, 5.000000, 0.0000000E+00, &
                      0.0000000E+00, -999.0000, -999.0000, -999.0000, .false., 2, 1, 528, 0, &
                      -999.0000, -999.0000, -999.0000, -999.0000)
      call ops_bron_rek( emtrend, buildingEffect, landmax, emis, nsbuf, bnr, bx, by, bdiam, bsterkte, bwarmte, bhoogte, &
          bsigmaz, bD_stack, bV_stack, bTs_stack, bemis_horizontal, bbuilding, btgedr, bdegr, bqrv, bqtr, bcatnr, &
          blandnr, eof, error)
       
      call assertEqual( ref_nsbuf, nsbuf, "nsbuf", __LINE__, __FILE__)
      call assertEqual( ref_bnr, bnr(1), "bnr", __LINE__, __FILE__)
      call assertEqual( ref_bx, bx(1), "bx", __LINE__, __FILE__)
      call assertEqual( ref_by, by(1), "by", __LINE__, __FILE__)
      call assertEqual( ref_bdiam, bdiam(1), tol, "bdiam", __LINE__, __FILE__)
      call assertEqual( ref_bsterkte, bsterkte(1), tol, "bsterkte", __LINE__, __FILE__)
      call assertEqual( ref_bwarmte, bwarmte(1), tol, "bwarmte", __LINE__, __FILE__)
      call assertEqual( ref_bhoogte, bhoogte(1), tol, "bhoogte", __LINE__, __FILE__)
      call assertEqual( ref_bsigmaz, bsigmaz(1), tol, "bsigmaz", __LINE__, __FILE__)
      call assertEqual( ref_bD_stack, bD_stack(1), tol, "bD_stack", __LINE__, __FILE__)
      call assertEqual( ref_bV_stack, bV_stack(1), tol, "bV_stack", __LINE__, __FILE__)
      call assertEqual( ref_bTs_stack, bTs_stack(1), tol, "bTs_stack", __LINE__, __FILE__)
      call assertEqual( ref_bemis_horizontal, bemis_horizontal(1), "bemis_horizontal", __LINE__, __FILE__)
      call assertEqual( ref_bbuilding, bbuilding, tol, "bbuilding", __LINE__, __FILE__)
      call assertEqual( ref_btgedr, btgedr(1), "btgedr", __LINE__, __FILE__)
      call assertEqual( ref_bdegr, bdegr(1), "bdegr", __LINE__, __FILE__)
      call assertEqual( ref_bqrv, bqrv(1), tol, "bqrv", __LINE__, __FILE__)
      call assertEqual( ref_bqtr, bqtr(1), tol, "bqtr", __LINE__, __FILE__)
      call assertEqual( ref_bcatnr, bcatnr(1), "bcatnr", __LINE__, __FILE__)
      call assertEqual( ref_blandnr, blandnr(1), "blandnr", __LINE__, __FILE__)
      call assertEqual( ref_eof, eof, "eof", __LINE__, __FILE__)
      call assertEqual( ref_error, error, "ops_bron_rek", __LINE__, __FILE__)
   end subroutine test_ops_bron_rek_space_heating

   subroutine test_ops_bron_rek_traffic()
      use no_pfunit_ops_lt
      use m_ops_building, only: TBuildingEffect
      use m_ops_building, only: TBuilding
      use m_ops_bron_rek
      use m_error, only: TError
      
      integer :: i
      real, parameter :: tol = 1e-5
      real, parameter :: emtrend =   1.00000000E+00
      type(TBuildingEffect) :: buildingEffect
      integer :: landmax
      integer, parameter :: in_landmax = 1
      real, parameter :: in_emis(6,50) = reshape( (/   5.28000000E+02,   0.00000000E+00,   1.00000000E+00, &
           0.00000000E+00,   0.00000000E+00,   1.00000000E+00, (0.00000000E+0, i=1, 6*49) /), &
        (/6,50/) )
      real, parameter :: ref_emis(6,50) = reshape( (/   5.28000000E+02,   0.00000000E+00,   1.00000000E+00, &
           0.00000000E+00,   1.00000000E+00,   2.00000000E+00, (0.00000000E+0, i=1, 6*49) /), &
         (/6,50/) )
      real :: emis(6,50)
      integer :: nsbuf
      integer, parameter :: ref_nsbuf = 1
      integer, parameter :: ref_bnr = 1
      integer, parameter :: ref_bx = 155000
      integer, parameter :: ref_by = 385000
      real, parameter :: ref_bdiam =  0.00000000E+00
      real, parameter :: ref_bsterkte = 0.00000000E+00
      real, parameter :: ref_bwarmte =  0.00000000E+00
      real, parameter :: ref_bhoogte = 5.00000000E+00
      real, parameter :: ref_bsigmaz =  0.00000000E+00
      real, parameter :: ref_bD_stack = -9.99000000E+02
      real, parameter :: ref_bV_stack = -9.99000000E+02
      real, parameter :: ref_bTs_stack = -9.99000000E+02
      logical, parameter :: ref_bemis_horizontal =  .false.
      integer, parameter :: ref_btgedr = 3
      integer, parameter :: ref_bdegr = 0
      real, parameter :: ref_bqrv = 0.00000000E+00
      real, parameter :: ref_bqtr = 1.00000000E+00
      integer, parameter :: ref_bcatnr = 1
      integer, parameter :: ref_blandnr = 528
      logical :: eof
      logical, parameter :: ref_eof = .true.
      type(TError) :: error
      type(TError) :: ref_error
      type(TBuilding), allocatable :: bbuilding(:), ref_bbuilding(:)
      integer, allocatable :: bnr(:), bx(:), by(:)
      real, allocatable :: bdiam(:), bsterkte(:), bwarmte(:), bhoogte(:), bsigmaz(:), bD_stack(:), bV_stack(:), bTs_stack(:), bqrv(:), bqtr(:)
      logical, allocatable :: bemis_horizontal(:)
      integer, allocatable :: btgedr(:), bdegr(:), bcatnr(:), blandnr(:)

      allocate( bbuilding(4000), ref_bbuilding(4000), &
                bnr(4000), bx(4000), by(4000), &
                bdiam(4000), bsterkte(4000), bwarmte(4000), bhoogte(4000), bsigmaz(4000), bD_stack(4000), bV_stack(4000), bTs_stack(4000), bqrv(4000), bqtr(4000), &
                bemis_horizontal(4000), &
                btgedr(4000), bdegr(4000), bcatnr(4000), blandnr(4000))

      ref_bbuilding(:)%length =   0.00000000E+00
      ref_bbuilding(:)%width =   0.00000000E+00
      ref_bbuilding(:)%height =   0.00000000E+00
      ref_bbuilding(:)%orientation =   0.00000000E+00
      ref_bbuilding(:)%x =   0.00000000E+00
      ref_bbuilding(:)%y =   0.00000000E+00
      ref_bbuilding(:)%type =            0
      buildingEffect%nParam =            0
      buildingEffect%nClass =  [(0.00000000E+00, i=1, 9)]
      buildingEffect%minClass =  [(0.00000000E+00, i=1, 9)]
      buildingEffect%maxClass =  [(0.00000000E+00, i=1, 9)]
      buildingEffect%nClass =  [(0.00000000E+00, i=1, 9)]
      buildingEffect%minClass =  [(0.00000000E+00, i=1, 9)]
      buildingEffect%maxClass =  [(0.00000000E+00, i=1, 9)]
       
      landmax = in_landmax
      emis = in_emis
      ref_error%haserror = .false.
      ref_error%message = ""
   
      call setup_scratch(1, 155000.0, 385000.0, 1.000000, 0.0000000E+00, 5.000000, 0.0000000E+00, &
                      0.0000000E+00, -999.0000, -999.0000, -999.0000, .false., 3, 1, 528, 0, &
                      -999.0000, -999.0000, -999.0000, -999.0000)
      call ops_bron_rek( emtrend, buildingEffect, landmax, emis, nsbuf, bnr, bx, by, bdiam, bsterkte, bwarmte, bhoogte, &
          bsigmaz, bD_stack, bV_stack, bTs_stack, bemis_horizontal, bbuilding, btgedr, bdegr, bqrv, bqtr, bcatnr, &
          blandnr, eof, error)
       
      call assertEqual( ref_emis, emis, tol, "emis", __LINE__, __FILE__)    
      call assertEqual( ref_nsbuf, nsbuf, "nsbuf", __LINE__, __FILE__)
      call assertEqual( ref_bnr, bnr(1), "bnr", __LINE__, __FILE__)
      call assertEqual( ref_bx, bx(1), "bx", __LINE__, __FILE__)
      call assertEqual( ref_by, by(1), "by", __LINE__, __FILE__)
      call assertEqual( ref_bdiam, bdiam(1), tol, "bdiam", __LINE__, __FILE__)
      call assertEqual( ref_bsterkte, bsterkte(1), tol, "bsterkte", __LINE__, __FILE__)
      call assertEqual( ref_bwarmte, bwarmte(1), tol, "bwarmte", __LINE__, __FILE__)
      call assertEqual( ref_bhoogte, bhoogte(1), tol, "bhoogte", __LINE__, __FILE__)
      call assertEqual( ref_bsigmaz, bsigmaz(1), tol, "bsigmaz", __LINE__, __FILE__)
      call assertEqual( ref_bD_stack, bD_stack(1), tol, "bD_stack", __LINE__, __FILE__)
      call assertEqual( ref_bV_stack, bV_stack(1), tol, "bV_stack", __LINE__, __FILE__)
      call assertEqual( ref_bTs_stack, bTs_stack(1), tol, "bTs_stack", __LINE__, __FILE__)
      call assertEqual( ref_bemis_horizontal, bemis_horizontal(1), "bemis_horizontal", __LINE__, __FILE__)
      call assertEqual( ref_bbuilding, bbuilding, tol, "bbuilding", __LINE__, __FILE__)
      call assertEqual( ref_btgedr, btgedr(1), "btgedr", __LINE__, __FILE__)
      call assertEqual( ref_bdegr, bdegr(1), "bdegr", __LINE__, __FILE__)
      call assertEqual( ref_bqrv, bqrv(1), tol, "bqrv", __LINE__, __FILE__)
      call assertEqual( ref_bqtr, bqtr(1), tol, "bqtr", __LINE__, __FILE__)
      call assertEqual( ref_bcatnr, bcatnr(1), "bcatnr", __LINE__, __FILE__)
      call assertEqual( ref_blandnr, blandnr(1), "blandnr", __LINE__, __FILE__)
      call assertEqual( ref_eof, eof, "eof", __LINE__, __FILE__)
      call assertEqual( ref_error, error, "ops_bron_rek", __LINE__, __FILE__)
   end subroutine test_ops_bron_rek_traffic
end module m_test_ops_bron_rek
 
program p_test_ops_bron_rek
use m_test_ops_bron_rek
use no_pfunit
implicit none
   call test_ops_bron_rek()
   call test_ops_bron_rek_space_heating()
   call test_ops_bron_rek_traffic()
   call conclusion()
end program p_test_ops_bron_rek
