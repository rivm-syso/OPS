module no_pfunit_ops_lt
use no_pfunit_ops_lib, only: assertRelativelyEqual,  assertGreaterThan, assertLessThan, &
        assertFalse, assertTrue, conclusion, &
        assertEqual_array_string, assertEqual_scalar_string, assertEqual_scalar_bool, assertEqual_scalar_int, &
        assertEqual_scalar_double_notol, assertEqual_scalar_dble, assertEqual_scalar, assertEqual_scalar_notol, &
        assertEqual_array, assertEqual_array_bool, assertEqual_array_int, assertEqual_array_int16, &
        assertEqual_array_5d, assertEqual_array_4d, assertEqual_array_2d, &
        assertEqual_array_2d_notol, assertEqual_array_int16_2d, assertEqual_tApsGridInt, &
        assertEqual_tApsGridReal, assertEqual_tError, initAps, initAps_real, initAps_int
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
       module procedure  assertEqual_tvChem
       module procedure  assertEqual_tdo_proc
       module procedure  assertEqual_tBuilding
       module procedure  assertEqual_tBuilding_1d
    end interface assertEqual

contains

    subroutine assertEqual_tvChem(chem, ref_chem, tol, message, lineno, filename)
    use m_ops_vchem
    implicit none
       type(tvChem), intent(in) :: chem, ref_chem
       real,               intent(in) :: tol
       character(len=*),   intent(in) :: message
       integer, optional,  intent(in) :: lineno
       character(len=*), optional, intent(in) :: filename

       call assertEqual_tApsGridReal( ref_chem%mass_prec_grid,  chem%mass_prec_grid, tol, &
                         message // ' mass_prec_grid', lineno, filename)
       call assertEqual( ref_chem%mass_conv_dtfac_grid,  chem%mass_conv_dtfac_grid, tol, &
                         message // ' mass_conv_dtfac_grid', lineno, filename)
    end subroutine assertEqual_tvChem

    subroutine assertEqual_tdo_proc(do_proc, ref_do_proc, tol, message, lineno, filename)
    use m_ops_tdo_proc
    implicit none
       type(tdo_proc), intent(in) :: do_proc, ref_do_proc
       real,               intent(in) :: tol
       character(len=*),   intent(in) :: message
       integer, optional,  intent(in) :: lineno
       character(len=*), optional, intent(in) :: filename

       call assertEqual( ref_do_proc%chem,  do_proc%chem, &
                         message // ' chem', lineno, filename)
       call assertEqual( ref_do_proc%depl_drydep,  do_proc%depl_drydep, &
                         message // ' depl_drydep', lineno, filename)
       call assertEqual( ref_do_proc%depl_wetdep,  do_proc%depl_wetdep, &
                         message // ' depl_wetdep', lineno, filename)
       call assertEqual( ref_do_proc%grad_drydep,  do_proc%grad_drydep, &
                         message // ' grad_drydep', lineno, filename)
    end subroutine assertEqual_tdo_proc

    subroutine assertEqual_tBuilding_1d(ref_building, building, tol, message, lineno, filename)
        use m_ops_building
        use no_pfunit, only: compare_dimensions
        implicit none
          type(TBuilding),    intent(in) :: building(:), ref_building(:)
          real,               intent(in) :: tol
          character(len=*),   intent(in) :: message
          integer, optional,  intent(in) :: lineno
          character(len=*), optional, intent(in) :: filename
   
          integer :: i
          logical :: success
          
          success = compare_dimensions(shape(building),shape(ref_building))
          if (.not. success .or. size(building)==0) return

          do i = 1, size(building)
            call assertEqual_tBuilding(ref_building(i), building(i), tol, message, lineno, filename)
          end do
    end subroutine assertEqual_tBuilding_1d
    
    subroutine assertEqual_tBuilding(ref_building, building, tol, message, lineno, filename)
        use m_ops_building
        implicit none
          type(TBuilding),    intent(in) :: building, ref_building
          real,               intent(in) :: tol
          character(len=*),   intent(in) :: message
          integer, optional,  intent(in) :: lineno
          character(len=*), optional, intent(in) :: filename
   
          call assertEqual( ref_building%length, building%length, tol, &
                            message // ' length', lineno, filename)
   
          call assertEqual( ref_building%width, building%width, tol, &
                            message // ' width', lineno, filename)
   
          call assertEqual( ref_building%height, building%height, tol, &
                            message // ' height', lineno, filename)
   
          call assertEqual( ref_building%orientation, building%orientation, tol, &
                            message // ' orientation', lineno, filename)
   
          call assertEqual( ref_building%x, building%x, tol, &
                            message // ' x', lineno, filename)
   
          call assertEqual( ref_building%y, building%y, tol, &
                            message // ' y', lineno, filename)
          
          if (allocated(building%buildingFactFunction) .and. &
              allocated(ref_building%buildingFactFunction)) then
             call assertEqual( ref_building%buildingFactFunction, building%buildingFactFunction, tol, &
                            message // ' buildingFactFunction', lineno, filename)
          elseif (allocated(ref_building%buildingFactFunction)) then
             call assertTrue( allocated(building%buildingFactFunction), &
                  'expected building%buildingFactFunction to be allocated', lineno, filename)
          elseif (allocated(building%buildingFactFunction)) then
             call assertFalse( allocated(ref_building%buildingFactFunction), &
                   'expected building%buildingFactFunction not to be allocated', lineno, filename)
          end if
   
          call assertEqual( ref_building%type, building%type, message // ' type', lineno, filename)
 
     end subroutine assertEqual_tBuilding

end module no_pfunit_ops_lt


