module m_test_bgcon
implicit none
contains

    SUBROUTINE one_bgcon_test(filename_in, xr, yr, xb, yb, gridvalues, trajectory_values)
    use no_pfunit
    USE m_aps,   only: ReadAps, TApsGridReal, set_average, Dealloc, GridValue
    
    USE m_error, only: TError, ErrorCall, write_error
    USE m_commonfile, only: IOB_STDOUT
    USE m_ops_bgcon_tra
    USE m_ops_bgcon
    USE m_fileutils, only: fix_slashes
    implicit none
        CHARACTER(len=*), intent(in) :: filename_in
        real,             intent(in) :: xr(2), yr(2), xb(2), yb(2)
        real,             intent(in) :: gridvalues(:), trajectory_values(0:1,1:2)
    
        integer             :: nfields
        character(len=200)  :: filename
        TYPE (TApsGridReal) :: floatgrid
        TYPE (TError)       :: error
        real, parameter     :: tol = 1.0e-5 
        integer             :: ifield
        real                :: gvalue
        integer :: in_trajectory, is_zero, jscell

        filename = filename_in
        call fix_slashes(filename)
    
        print *,'reading file "'//trim(filename)//'"'
        call ReadAps(  filename, &
             gridtitle = 'reading', &
             floatgrid = floatgrid, &
             error = error)
        call assertFalse(error%haserror,"Error reading file")
        if (error%haserror) goto 9999
        nfields = size(floatgrid%value,3)
        print *,'nfields = ',nfields
        do ifield = 1,nfields
            call set_average(grid = floatgrid, fieldnumber=ifield)
        end do
    
        do is_zero=0,1
            if (is_zero==1) then
                floatgrid%value = 0
            end if
            do jscell=1,2

                call ops_bgcon_tra(xr(jscell), yr(jscell), xb(jscell), yb(jscell), bgdata=floatgrid, bgcon=gvalue, error=error)
                call assertFalse(error%haserror,"Error from ops_bgcon")
                if (error%haserror) goto 9999

                call assertEqual(gvalue, trajectory_values(is_zero, jscell), tol, 'trajectory_values')

                do ifield = 0,nfields
                    do in_trajectory=0,1
                        if (ifield==0) then
                            call ops_bgcon( xr(jscell)*1000, yr(jscell)*1000, in_trajectory=in_trajectory==1, &
                                            bgdata=floatgrid, bgcon=gvalue, error=error)
                        else
                            call ops_bgcon( xr(jscell)*1000, yr(jscell)*1000, in_trajectory=in_trajectory==1, &
                                            bgdata=floatgrid, bgcon=gvalue, error=error, fieldnumber=ifield)
                        end if
                        call assertFalse(error%haserror,"Error from ops_bgcon")
                        if (error%haserror) goto 9999
                        print *,'xr,yr=',xr(jscell),yr(jscell)
                        print *,'gvalue=',gvalue
                        if (jscell==1 .and. is_zero==0) then
                            call assertEqual(gvalue, gridvalues(max(1,ifield)), tol, 'gridvalues(1)')
                        else
                            call assertEqual(gvalue, floatgrid%average(max(1,ifield)), tol, 'gridvalues(2)')
                        end if
                    end do
                end do
            end do
        end do
    
        call Dealloc(floatgrid)
        return
    9999 continue
    
        CALL ErrorCall('one_bgcon_test', error)
        call write_error(IOB_STDOUT,error)
    
    end subroutine one_bgcon_test


    SUBROUTINE tst_read_so2()
    implicit none
        real,    parameter :: xr(2) = (/ -234.5, -236.0 /),  yr(2) = (/ 864.6, 846.6 /)
        real,    parameter :: xb(2) = (/ -231.0, -233.0 /),  yb(2) = (/ 863.6, 845.6 /)
        real,    parameter :: gridvalues(1) = (/ 1.0 /)
        real,    parameter :: trajectory_values(0:1,1:2) = reshape( &
                 (/ 3.83333325386, 3.83333325386, 3.83333325386, 3.83333325386 /), (/2, 2/))
        character(len=*), parameter :: filename = '../ops_lib/tst/level_1/resources/SO2_mass_prec_2021.ops'
        call one_bgcon_test(filename, xr, yr, xb, yb, gridvalues, trajectory_values)
    end subroutine tst_read_so2
        
    SUBROUTINE tst_read_template()
    implicit none
        real,    parameter :: xr(2) = (/ -234.5, -236.0 /),  yr(2) = (/ 864.6, 864.6 /)
        real,    parameter :: xb(2) = (/ -231.0, -233.0 /),  yb(2) = (/ 863.6, 845.6 /)
        real,    parameter :: gridvalues(3) = (/ 1.0, 11., 21. /)
        real,    parameter :: trajectory_values(0:1,1:2) = reshape( &
                 (/ 3.83333325386, 3.83333325386, 3.83333325386, 3.83333325386 /), (/2, 2/))
        character(len=*), parameter :: filename = '../ops_lib/tst/level_1/resources/template3x2_nf3.ops'
        call one_bgcon_test(filename, xr, yr, xb, yb, gridvalues, trajectory_values)
    end subroutine tst_read_template
    
end module m_test_bgcon


program p_test_bgcon
use no_pfunit
use m_test_bgcon
implicit none
    call tst_read_so2()
    call tst_read_template()
    call conclusion()
end program p_test_bgcon
