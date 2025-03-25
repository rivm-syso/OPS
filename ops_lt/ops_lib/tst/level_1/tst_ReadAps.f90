module m_tst_ReadAps

implicit none

contains

SUBROUTINE one_read_aps_test(filename_in, corner, nx, ny, values, dxy, nfields, x, y, gridvalues)
use no_pfunit
USE m_aps,   only: ReadAps, TApsGridReal, set_average, Dealloc, GridValue, WriteAps

USE m_error, only: TError, ErrorCall, write_error
USE m_commonfile, only: IOB_STDOUT
USE m_fileutils, only: fix_slashes
implicit none
    CHARACTER(len=*), intent(in) :: filename_in
    real,             intent(in) :: corner(2)
    integer,          intent(in) :: nx, ny
    integer,          intent(in) :: nfields
    real,             intent(in) :: values(nx,ny,nfields)
    real,             intent(in) :: dxy(2)
    real,             intent(in) :: x(2), y(2), gridvalues(nfields)


    character(len=200) :: filename
    TYPE (TApsGridReal) :: floatgrid
    TYPE (TError)       :: error
    real, parameter     :: tol = 1.0e-5 
    integer             :: ifield, nvalues, value_shape(3)
    real, allocatable   :: values_out(:), values_ref(:)
    logical             :: iscell
    real                :: gvalue
    integer :: i
    do i = 1,2
       if (i==1) then
          filename = filename_in
       else
          filename = './level_1/tmp.grid'
       end if
     
       call fix_slashes(filename)
       print *,'reading file "'//trim(filename)//'"'
       call ReadAps(  filename, &
            gridtitle = 'reading', &
            floatgrid = floatgrid, &
            error = error)
       call assertFalse(error%haserror,"Error reading file",__LINE__,__FILE__)
       if (error%haserror) goto 9999

       if (i==1) then
          filename = './level_1/tmp.grid'
          call WriteAps(filename, 'writing', floatgrid, error)
          call assertFalse(error%haserror,"Error reading file",__LINE__,__FILE__)
          if (error%haserror) goto 9999
       end if

       do ifield = 1,nfields
           call set_average(grid = floatgrid, fieldnumber=ifield)
       end do
     
       print *,'    header:'
       print *,'        xyul_corner = (',floatgrid%gridheader%xul_corner,',', floatgrid%gridheader%yul_corner,')'
       print *,'         dimensions = (',floatgrid%gridheader%nrcol,',', floatgrid%gridheader%nrrow,')'
       print *,'                dxy = (',floatgrid%gridheader%grixl,',', floatgrid%gridheader%griyl,')'
       print *,'    average: ',floatgrid%average
       print *,'      value: ',floatgrid%value
     
       call assertEqual(corner,(/floatgrid%gridheader%xul_corner,floatgrid%gridheader%yul_corner/),tol,'corner',__LINE__,__FILE__)
       call assertEqual(shape(floatgrid%value), (/floatgrid%gridheader%nrcol,floatgrid%gridheader%nrrow,nfields/),'dimensions',__LINE__,__FILE__)
       call assertEqual(dxy,(/floatgrid%gridheader%grixl,floatgrid%gridheader%griyl/),tol,'dxy',__LINE__,__FILE__)
     
       value_shape = shape(values)
       nvalues = value_shape(1)*value_shape(2)
       allocate(values_out(nvalues), values_ref(nvalues))
       do ifield = 1,nfields
           call assertEqual(sum(values(:,:,ifield))/size(values,1)/size(values,2),floatgrid%average(ifield),tol,'average',__LINE__,__FILE__)
           values_out(1:nvalues) = pack(values(:,:,ifield),.true.)
           values_ref(1:nvalues) = pack(floatgrid%value(:,:,ifield),.true.)
           call assertEqual(values_out, values_out, tol, 'values',__LINE__,__FILE__)
           call GridValue(x(1), y(1), floatgrid, gvalue, iscell, ifield)
           call assertTrue(iscell, 'iscell(1)',__LINE__,__FILE__)
           call assertEqual(gvalue, gridvalues(ifield), tol, 'gridvalues(1)',__LINE__,__FILE__)
           call GridValue(x(2), y(2), floatgrid, gvalue, iscell, ifield)
           call assertFalse(iscell, 'iscell(2)',__LINE__,__FILE__)
           call assertEqual(gvalue, floatgrid%average(ifield), tol, 'gridvalues(2)',__LINE__,__FILE__)
       end do
       deallocate(values_out, values_ref)
     
       call Dealloc(floatgrid)
    end do
    print *
    print *,'Test was successful'
    print *
    return
9999 continue

    CALL ErrorCall('tst_read_aps', error)
    call write_error(IOB_STDOUT,error)

end subroutine one_read_aps_test


SUBROUTINE tst_read_so2
implicit none
    real,    parameter :: corner(2) = (/-235.000000     ,   865.000000 /)
    real,    parameter :: values(3,2,1) = reshape((/ 1., 2., 5., 4., 5., 6. /), (/3,2,1/))
    real,    parameter :: dxy(2)    = (/ 1.,   1. /)
    integer, parameter :: nx = size(values,1), ny=size(values,2), nfields   = size(values,3)
    real,    parameter :: x(2) = (/ -234.5, -236.0 /),  y(2) = (/ 864.6, 846.6 /)
    real,    parameter :: gridvalues(1) = (/ 1.0 /)
    character(len=*), parameter :: filename = '.\level_1/resources\/SO2_mass_prec_2021.ops'
    call one_read_aps_test(filename, corner, nx, ny, values, dxy, nfields, x, y, gridvalues)

end subroutine tst_read_so2
    
SUBROUTINE tst_read_template
implicit none
    real,    parameter :: corner(2) = (/-235.000000     ,   865.000000 /)
    real,    parameter :: values(3,2,3) = reshape((/ &
             1.0,  2.0,  5.0,  4.0,  5.0,  6.0, &
            11.0, 12.0, 15.0, 14.0, 15.0, 16.0, &
            21.0, 22.0, 25.0, 24.0, 25.0, 26.0 /), (/3,2,3/))
    real,    parameter :: dxy(2)    = (/ 1.,   1. /)
    integer, parameter :: nx = size(values,1), ny=size(values,2), nfields   = size(values,3)
    real,    parameter :: x(2) = (/ -234.5, -236.0 /),  y(2) = (/ 864.6, 864.6 /)
    real,    parameter :: gridvalues(nfields) = (/ 1.0, 11., 21. /)
    character(len=*), parameter :: filename = './level_1/resources\template3x2_nf3.ops'

    call one_read_aps_test(filename, corner, nx, ny, values, dxy, nfields, x, y, gridvalues)

end subroutine tst_read_template
    
end module m_tst_ReadAps

program p_tst_ReadAps

use no_pfunit
use m_tst_ReadAps

implicit none

call tst_read_so2
call tst_read_template
call conclusion
    
end program p_tst_ReadAps    
