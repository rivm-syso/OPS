module no_pfunit_ops_lib
use no_pfunit, only: assertRelativelyEqual,  assertGreaterThan, assertLessThan,  conclusion, &
        assertEqual_array_string, assertEqual_scalar_string, assertEqual_scalar_bool, assertEqual_scalar_int, &
        assertEqual_scalar_double_notol, assertEqual_scalar_dble, assertEqual_scalar, assertEqual_scalar_notol, &
        assertEqual_array, assertEqual_array_bool, assertEqual_array_int, assertEqual_array_int16, &
        assertEqual_array_4d, assertEqual_array_5d, assertEqual_array_2d, &
        assertEqual_array_2d_notol, assertEqual_array_int16_2d, assertTrue, assertFalse
implicit none
    interface assertEqual
       module procedure  assertEqual_array_string
       module procedure  assertEqual_scalar_string
       module procedure  assertEqual_scalar_bool
       module procedure  assertEqual_scalar_int
       module procedure  assertEqual_scalar_double_notol
       module procedure  assertEqual_scalar_dble
       module procedure  assertEqual_scalar 
       module procedure  assertEqual_scalar_notol
       module procedure  assertEqual_array
       module procedure  assertEqual_array_int
       module procedure  assertEqual_array_int16
       module procedure  assertEqual_array_bool
       module procedure  assertEqual_array_5d
       module procedure  assertEqual_array_4d
       module procedure  assertEqual_array_2d
       module procedure  assertEqual_array_2d_notol
       module procedure  assertEqual_array_int16_2d
       module procedure  assertEqual_tApsGridInt
       module procedure  assertEqual_tApsGridReal
       module procedure  assertEqual_tError
    end interface assertEqual

    interface initAps
       module procedure initAps_real
       module procedure initAps_int 
    end interface initAps

contains
    subroutine InitAps_int(fname, name, grid, lineno, filename)
    use m_commonfile
    use m_error
    use m_aps
    implicit none
       character(len=*),  intent(in) :: fname, name
       type(tApsGridInt), intent(out) :: grid
       integer, optional, intent(in) :: lineno
       character(len=*), optional, intent(in) :: filename
       type(tError) :: error
       call ReadAps(fname, name, grid, error, zero_dxy_allowed=.true.)
       if (error%haserror) call write_error(IOB_STDOUT, error)
       call assertFalse(error%haserror, 'reading "'//fname//'"', lineno, filename)
    end subroutine InitAps_int

    subroutine InitAps_real(fname, name, grid, lineno, filename)
    use m_commonfile
    use m_error
    use m_aps
    implicit none
       character(len=*), intent(in) :: fname, name
       type(tApsGridReal), intent(out) :: grid
       integer, optional, intent(in) :: lineno
       character(len=*), optional, intent(in) :: filename
       type(tError) :: error

       call ReadAps(fname, name, grid, error, zero_dxy_allowed=.true.)
       if (error%haserror) call write_error(IOB_STDOUT, error)
       call assertFalse(error%haserror, 'reading "'//fname//'"', lineno, filename)
    end subroutine InitAps_real

    subroutine assertEqual_tApsGridReal(grid, ref_grid, tol, message, lineno, filename)
    use m_aps
    implicit none
       type(TApsGridReal), intent(in) :: grid, ref_grid
       real,               intent(in) :: tol
       character(len=*),   intent(in) :: message
       integer, optional,  intent(in) :: lineno
       character(len=*), optional, intent(in) :: filename

       character(len=100) :: msg
       integer :: ifield

       call assertEqual( (/ref_grid%gridheader%xul_corner,ref_grid%gridheader%yul_corner/), &
                         (/grid%gridheader%xul_corner,grid%gridheader%yul_corner/), tol, &
                         message // ' corner', lineno, filename)

       call assertEqual( (/ref_grid%gridheader%nrcol,ref_grid%gridheader%nrrow/), &
                         (/grid%gridheader%nrcol,grid%gridheader%nrrow/), &
                         message // ' dimensions', lineno, filename)

       call assertEqual( (/ref_grid%gridheader%grixl,ref_grid%gridheader%griyl/), &
                         (/grid%gridheader%grixl,grid%gridheader%griyl/),tol, &
                         message // ' dxy', lineno, filename)
     
       do ifield = 1,size(ref_grid%value,3)
           write(msg,'(a,i0,a)') message // ' values(',ifield,')'
           call assertEqual( ref_grid%value(:,:,ifield), &
                             grid%value(:,:,ifield), tol, msg, lineno, filename)
       end do
    end subroutine assertEqual_tApsGridReal

    subroutine assertEqual_tApsGridInt(grid, ref_grid, tol, message, lineno, filename)
    use m_aps
    implicit none
       type(TApsGridInt),  intent(in) :: grid, ref_grid
       real,               intent(in) :: tol
       character(len=*),   intent(in) :: message
       integer, optional,  intent(in) :: lineno
       character(len=*), optional, intent(in) :: filename

       character(len=100) :: msg
       integer :: ifield
       call assertEqual( (/ref_grid%gridheader%xul_corner,ref_grid%gridheader%yul_corner/), &
                         (/grid%gridheader%xul_corner,grid%gridheader%yul_corner/), tol, &
                         message // ' corner', lineno, filename)

       call assertEqual( (/ref_grid%gridheader%nrcol,ref_grid%gridheader%nrrow/), &
                         (/grid%gridheader%nrcol,grid%gridheader%nrrow/), &
                         message // ' dimensions', lineno, filename)

       call assertEqual( (/ref_grid%gridheader%grixl,ref_grid%gridheader%griyl/), &
                         (/grid%gridheader%grixl,grid%gridheader%griyl/),tol, &
                         message // ' dxy', lineno, filename)
     
       do ifield = 1,size(ref_grid%value,3)
           write(msg,'(a,i0,a)') message // ' values(',ifield,')'
           call assertEqual( ref_grid%value(:,:,ifield), &
                             grid%value(:,:,ifield), msg, lineno, filename)
       end do
    end subroutine assertEqual_tApsGridInt

    subroutine assertEqual_tError(error,ref_error,message,lineno, fname)
    use m_error
    use m_commonfile
    implicit none
       type(TError),      intent(in) :: error, ref_error
       character(len=*),  intent(in) :: message
       integer, optional, intent(in) :: lineno
       character(len=*), optional, intent(in) :: fname
       if (ref_error%haserror) then
          call assertTrue( error%haserror, 'expected error from '//message//' did not occur', lineno, fname)
       else
          if (error%haserror) &
             call write_error(IOB_STDOUT,error)
          call assertFalse( error%haserror, message, __LINE__, __FILE__)
       endif
    end subroutine assertEqual_tError

end module no_pfunit_ops_lib

