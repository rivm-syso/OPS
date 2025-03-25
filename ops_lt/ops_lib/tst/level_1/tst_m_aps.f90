module m_tst_m_aps
contains

   SUBROUTINE one_read_aps_test(corner, values, dxy, x, y, gridvalues)
   
   ! Write and read APS file and check contents
   
   use no_pfunit
   USE m_aps,   only: WriteAps, TApsGridInt, Dealloc, GridValue, ReadAps, write_aps_header, TApsGridReal
   USE m_error, only: TError, ErrorCall, write_error
   USE m_commonfile, only: IOB_STDOUT
   USE m_fileutils, only: fix_slashes, sysclose
   
   implicit none
       real,             intent(in)   :: corner(2)
       integer*2,target, intent(in)   :: values(:,:,:)
       real,             intent(in)   :: dxy(2)
       real,             intent(in)   :: x(2), y(2)
       integer,          intent(in)   :: gridvalues(:)
   
       integer :: nx, ny, nfields
   
       TYPE (TApsGridInt)  :: intgrid1, intgrid2
       TYPE (TApsGridReal) :: floatgrid
       TYPE (TError)       :: error
       real, parameter     :: tol = 1.0e-5 
       integer             :: ifield
       logical             :: iscell
       integer             :: gvalue
       character(len=100)  :: filename
   
       ! Get grid dimensions:
       nx = size(values,1)
       ny = size(values,2)
       nfields = size(values,3)
   
       ! Fill ingrid with corners and fill ingrid%values:
       intgrid1%gridheader%xul_corner = corner(1)
       intgrid1%gridheader%yul_corner = corner(2)
       intgrid1%value => values

       ! Test writing APS-file with illegal header (zero dx, dy): 
       filename = './level_1/resources/illegal_header.ops'
       call fix_slashes(filename)
       intgrid1%gridheader%grixl = 0.0
       intgrid1%gridheader%griyl = 0.0
       call write_aps_header(88, filename, filename, intgrid1%gridheader, error)
       call assertTrue(error%haserror,'writing file for illegal aps header', __LINE__, __FILE__)
       error%haserror = .false. ! reset haserror
       CALL sysclose(88, filename, error)
       error%haserror=.false.

       ! Write APS-file, but allow zero dx, dy:
       call write_aps_header(88, filename, filename, intgrid1%gridheader, error, zero_dxy_allowed=.true.)
       call assertFalse(error%haserror,'writing file for illegal aps header', __LINE__, __FILE__)
       CALL sysclose(88, filename, error)
       call assertFalse(error%haserror,'closing file for illegal aps header',__LINE__, __FILE__)

       ! Reading illegal header (zero dx, dy) from integer APS file:
       call ReadAps(  filename, &
            gridtitle = 'reading illegal aps file', &
            intgrid = intgrid2, &
            error = error)
       call assertTrue(error%haserror,'reading illegal aps file',__LINE__, __FILE__)
       call write_error(IOB_STDOUT,error)
       CALL sysclose(88, filename, error)
       error%haserror = .false.

       ! Reading illegal header (zero dx, dy) from real APS file:
       call ReadAps(  filename, &
            gridtitle = 'reading illegal aps file', &
            floatgrid = floatgrid, &
            error = error)
       call assertTrue(error%haserror,'reading illegal aps file',__LINE__, __FILE__)
       call write_error(IOB_STDOUT,error)
       CALL sysclose(88, filename, error)
       error%haserror = .false.

       ! Legal values for dx, dy:
       intgrid1%gridheader%grixl = dxy(1)
       intgrid1%gridheader%griyl = dxy(2)
 
       ! Test writing illegal header (nrcol, nrrow < 0):
       intgrid1%gridheader%nrcol = -10
       intgrid1%gridheader%nrrow = -10
       call write_aps_header(88, filename, filename, intgrid1%gridheader, error)
       call assertFalse(error%haserror,'writing illegal aps header file', __LINE__, __FILE__)
       CALL sysclose(88, filename, error)
       
       ! Test writing illegal header, but allow for nrcol, nrrow < 0:
       call write_aps_header(88, filename, filename, intgrid1%gridheader, error, zero_dxy_allowed=.true.)
       call assertFalse(error%haserror,'writing illegal aps header file',__LINE__, __FILE__)
       CALL sysclose(88, filename, error)
       call assertFalse(error%haserror,'closing illegal aps header file',__LINE__, __FILE__)

       ! Read integer APS-file with illegal header (nrcol, nrrow < 0):
       call ReadAps(  filename, &
            gridtitle = 'reading illegal aps file', &
            intgrid = intgrid2, &
            error = error)
       call assertTrue(error%haserror,'reading illegal aps file',__LINE__, __FILE__)
       CALL sysclose(88, filename, error)
       error%haserror = .false.

       ! Read real APS-file with illegal header (nrcol, nrrow < 0):
       call ReadAps(  filename, &
            gridtitle = 'reading illegal aps file', &
            floatgrid = floatgrid, &
            error = error)
       call assertTrue(error%haserror,'reading illegal aps file',__LINE__, __FILE__)
       call write_error(IOB_STDOUT,error)
       CALL sysclose(88, filename, error)
       error%haserror = .false.

       ! Set legal values for nrcol, nrrow:
       intgrid1%gridheader%nrcol = nx
       intgrid1%gridheader%nrrow = ny

       ! Test writing integer APS-file with legal header:   
       filename = './level_1/resources/template_int.ops'
       call fix_slashes(filename)
       print *,'writing file "'//trim(filename)//'"'
       call WriteAps(  filename, &
            gridtitle = 'writing', &
            intgrid = intgrid1, &
            error = error)
       call assertFalse(error%haserror,'writing aps file',__LINE__, __FILE__)
       if (error%haserror) goto 9999
   
       ! Test reading integer APS-file with legal header:   
       call ReadAps(  filename, &
            gridtitle = 'reading', &
            intgrid = intgrid2, &
            error = error)
       call assertFalse(error%haserror,'reading aps file',__LINE__, __FILE__)
       if (error%haserror) goto 9999
   
       ! Check header values read:
       call assertEqual(corner,(/intgrid2%gridheader%xul_corner,intgrid2%gridheader%yul_corner/),tol,'corner',__LINE__, __FILE__)
       call assertEqual(shape(intgrid2%value), (/intgrid2%gridheader%nrcol,intgrid2%gridheader%nrrow,nfields/),'dimensions',__LINE__, __FILE__)
       call assertEqual(dxy,(/intgrid2%gridheader%grixl,intgrid2%gridheader%griyl/),tol,'dxy',__LINE__, __FILE__)

       ! Check grid values read:
       do ifield = 1,nfields
           call assertEqual(values(:,:,ifield), intgrid2%value(:,:,ifield), 'values',__LINE__, __FILE__)
           call GridValue(x(1), y(1), intgrid2, gvalue, iscell, ifield)
           call assertTrue(iscell, 'iscell(1)',__LINE__, __FILE__)
           call assertEqual(gvalue, gridvalues(ifield), 'gridvalues(1)',__LINE__, __FILE__)
           call GridValue(x(2), y(2), intgrid2, gvalue, iscell, ifield)
           call assertFalse(iscell, 'iscell(2)',__LINE__, __FILE__)
           call assertEqual(gvalue, 0, 'gridvalues(1)',__LINE__, __FILE__)
       end do
   
       call Dealloc(intgrid2)
       
       return
   
       ! Error handling   
  9999 continue
       CALL ErrorCall('tst_read_aps_int', error)
       call write_error(IOB_STDOUT,error)
   
   end subroutine one_read_aps_test
   
   
   SUBROUTINE tst_read_1field
   
   ! Test reading/writing APS file with one field
   
   implicit none
       real,    parameter   :: corner(2) = (/-235.000000     ,   865.000000 /)
       real,    parameter   :: dxy(2)    = (/ 1.,   1. /)
                            
       real,    parameter   :: x(2) = (/ -234.5, -236.0 /),  y(2) = (/ 864.6, 846.6 /)
                            
       integer, parameter   :: ivalues(3,2,1) = reshape((/ 1, 2, 5, 4, 5, 6 /), (/3,2,1/))
       integer, parameter   :: gridvalues(1) = (/ 1 /)
       integer*2, parameter :: values(3,2,1) = int(ivalues,kind=2)
   
       call one_read_aps_test(corner, values, dxy, x, y, gridvalues)
   
   end subroutine tst_read_1field
       
   
   SUBROUTINE tst_read_3fields()

   ! Test reading/writing APS file with three fields

   implicit none
       real,    parameter :: corner(2) = (/-235.000000     ,   865.000000 /)
       real,    parameter :: dxy(2)    = (/ 1.,   1. /)
       real,    parameter :: x(2) = (/ -234.5, -236.0 /),  y(2) = (/ 864.6, 864.6 /)
       integer,parameter :: ivalues(3,2,3) = reshape((/ &
                1,  2,  5,  4,  5,  6, &
               11, 12, 15, 14, 15, 16, &
               21, 22, 25, 24, 25, 26 /), (/3,2,3/))
       integer,  parameter :: gridvalues(3) = (/ 1, 11, 21 /)
       integer*2, parameter :: values(3,2,3) = int(ivalues,kind=2)
   
       call one_read_aps_test(corner, values, dxy, x, y, gridvalues)
   
   end subroutine tst_read_3fields

   !-----------------------------------------------------------
   SUBROUTINE tst_GridValue()

   ! Test subroutine GridValue that gets a grid value from a grid for a given test point

   use no_pfunit
   use m_aps, only: GridValue, TApsGridInt
   
   implicit none
       real,    parameter :: corner(2) = (/-235.000000     ,   865.000000 /)
       real,    parameter :: dxy(2)    = (/ 1.,   1. /)
       real,    parameter :: x(21) = (/ -235.00010000, -235.00001000, -235.00000100, -235.00000000, -234.999999, -234.99999, -234.9999, &
                                        -234.00010000, -234.00001000, -234.00000100, -234.00000000, -233.999999, -233.99999, -233.9999, &
                                        -232.00010000, -232.00001000, -232.00000100, -232.00000000, -231.999999, -231.99999, -231.9999  /)
       real,    parameter :: y(5) = (/ 864.6, 863.4, 865.000000, 864.000000, 863.000000 /)
       integer,parameter :: ivalues(3,2,3) = reshape((/ &
                1,  2,  3,  4,  5,  6, &
               11, 12, 13, 14, 15, 16, &
               21, 22, 23, 24, 25, 26 /), (/3,2,3/))
               
       ! Index ix for 21 values of x (0 -> outside grid):
       
       integer,  parameter :: ix_ref(21) = (/ 0, 0, 1, 1, 1, 1, 1, &
                                              1, 1, 2, 2, 2, 2, 2, &
                                              3, 3, 3, 3, 3, 0, 0 /)
       ! Index iy for 5 values of y (all inside grid):
       integer,  parameter :: iy_ref(5)  = (/ 1, 2, 1, 2, 2 /)
       
       integer*2, target :: values(3,2,3) = int(ivalues,kind=2)

       integer             :: nfields            ! number of fields in APS-grid
       integer             :: ifield             ! index of fields in APS-grid
       integer             :: gvalue             ! value retrieved from grid
       integer             :: gvalue_ref1        ! reference value 
       logical             :: iscell             ! coordinates (x,y) are within grid
       integer             :: ix                 ! index for x-coordinates of test points
       integer             :: iy                 ! index for y-coordinate of test points
       TYPE (TApsGridInt)  :: intgrid1           ! APS grid (integer)
       character(len = 200):: msg                ! message for assert

       ! Fill APS-grid:
       intgrid1%gridheader%xul_corner = corner(1)
       intgrid1%gridheader%yul_corner = corner(2)
       intgrid1%gridheader%grixl = dxy(1)
       intgrid1%gridheader%griyl = dxy(2)
       intgrid1%gridheader%nrcol = size(values,1)
       intgrid1%gridheader%nrrow = size(values,2)
       nfields = size(values,3)
       intgrid1%value => values
   
       ! Check grid value retrieved for coordinates (x(i),y(i)):
       do ifield = 1,nfields
          do iy = 1,size(y)
             do ix = 1,size(x)
                
                ! Get reference value:
                if (ix_ref(ix) == 0) then
                   gvalue_ref1 = 0
                else
                   gvalue_ref1 = ix_ref(ix) + (ifield-1)*10 + (iy_ref(iy)-1)*3
                endif
                
                ! Get grid value from grid and check:
                gvalue = -999.0
                call GridValue(x(ix), y(iy), intgrid1, gvalue, iscell, ifield)
                write(msg,'(a,3(1x,i2),2(1x,f14.8),1x,L1,1x,i4)') &
                   'Error in GridValue; ifield,ix,iy,x,y,iscell,gridvalue: ', &
                                        ifield,ix,iy,x(ix),y(iy),iscell,gvalue
                call assertEqual(gvalue, gvalue_ref1, msg,__LINE__, __FILE__)
             enddo
          enddo
       enddo
   
   end subroutine tst_GridValue
   
   !-----------------------------------------------------------
   SUBROUTINE tst_set_average

   USE no_pfunit
   USE m_commonconst_lib                                                            ! EPS_DELTA only
   USE m_aps
   
   implicit none
   
   ! SUBROUTINE ARGUMENTS - INPUT
   REAL                              :: factor                     ! multiplication factor for the whole grid
   
   ! SUBROUTINE ARGUMENTS - I/O
   TYPE (TApsGridReal)               :: grid                       ! real APS grid
   INTEGER                           :: fieldnumber                ! fieldnumber to retreive data from
   
   ! LOCAL VARIABLES
   INTEGER                           :: nfields                     ! number of fields
   REAL                              :: tol_abs = 1.0e-5            ! absolute tolerance in testing
   
   !-------------------------------------------------------------------------------------------------------------------------------
   
   grid%gridheader%nrcol = 4
   grid%gridheader%nrrow = 1
   nfields = 1
   fieldnumber = 1
   allocate(grid%value(grid%gridheader%nrcol,grid%gridheader%nrrow,nfields))
   allocate(grid%average(nfields))
   
   grid%value(:,1,1) = (/ 1.0, 2.0, 3.0, 4.0 /)
   factor = 1.0
   call SetAverage(factor, grid, fieldnumber)
   call assertEqual(2.5,grid%average(fieldnumber),tol_abs,'Test 1: average of [1, 2, 3, 4]', __LINE__, __FILE__)
   
   grid%value(:,1,1) = (/ 1.0, 2.0, 3.0, 4.0 /)
   factor = 2.0
   call SetAverage(factor, grid, fieldnumber)
   call assertEqual((/ 2.0, 4.0, 6.0, 8.0 /),grid%value(:,1,1),tol_abs,'Test 2a: 2 x [1, 2, 3, 4]', __LINE__, __FILE__)
   call assertEqual(5.0,grid%average(fieldnumber),tol_abs,'Test 2b: average of 2 x [1, 2, 3, 4]')
   
   grid%value(:,1,1) = (/ -999.0, 2.0, 3.0, -999.0 /)
   factor = 2.0
   call SetAverage(factor, grid, fieldnumber)
   call assertEqual((/ -999.0, 4.0, 6.0, -999.0 /),grid%value(:,1,1),tol_abs,'Test 3a: 2 x [-999, 2, 3, -999]', __LINE__, __FILE__)
   call assertEqual(5.0,grid%average(fieldnumber),tol_abs,'Test 3b: average of 2 x [-999, 2, 3, -999]', __LINE__, __FILE__)
   
   grid%value(:,1,1) = (/ 0.0, -999.0, -2.0, -999.0 /)
   factor = 1.0
   call SetAverage(factor, grid, fieldnumber)
   call assertEqual(-500.0,grid%average(fieldnumber),tol_abs,'Test 4: average of [0, -999, -2, -999]', __LINE__, __FILE__)
   
   END SUBROUTINE tst_set_average
    
end module m_tst_m_aps

!------------------------------------------
! Program to test writing/reading APS files
!------------------------------------------
program p_tst_m_aps
use no_pfunit
use m_tst_m_aps
implicit none
    call tst_read_1field() 
    call tst_read_3fields() 
    call tst_GridValue()
    call tst_set_average()
    call conclusion()
end program p_tst_m_aps

