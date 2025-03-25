module m_test_building
implicit none
contains
    subroutine test_ops_building_alloc_zero()
    use m_ops_building
    use m_commonfile
    use m_error
    use no_pfunit
        real, parameter :: tol = 1e-5
        type(tbuilding) :: building                 ! structure containing data for building
        type(tbuildingEffect) :: buildingEffect     ! structure containing data for building effect
        type(Terror)    :: error                    ! error handling record

        call ops_building_alloc_zero(building, error)
        if (error%haserror) call write_error(IOB_STDOUT, error)
        call assertFalse( error%haserror, "ops_building_alloc_zero", __LINE__, __FILE__)
        call assertTrue(allocated(building%buildingFactFunction), "allocated", __LINE__, __FILE__)
        call assertEqual( (/0,0/), shape(building%buildingFactFunction), "shape(0,0)", __LINE__, __FILE__)

        call ops_building_effect_alloc_zero(buildingEffect, error)
        call assertFalse( error%haserror, "ops_building_effect_alloc_zero", __LINE__, __FILE__)
        call assertTrue(allocated(buildingEffect%classdefinitionArray), "allocated", __LINE__, __FILE__)
        call assertTrue(allocated(buildingEffect%buildingFactArray), "allocated", __LINE__, __FILE__)
        call assertTrue(allocated(buildingEffect%buildingFactAngleSRxaxis), "allocated", __LINE__, __FILE__)
        call assertTrue(allocated(buildingEffect%buildingFactDistances), "allocated", __LINE__, __FILE__)
        call assertEqual( (/0/), shape(buildingEffect%classdefinitionArray), "shape(0,0)", __LINE__, __FILE__)
        call assertEqual( (/0/), shape(buildingEffect%buildingFactArray), "shape(0,0)", __LINE__, __FILE__)
        call assertEqual( (/0/), shape(buildingEffect%buildingFactAngleSRxaxis), "shape(0,0)", __LINE__, __FILE__)
        call assertEqual( (/0/), shape(buildingEffect%buildingFactDistances), "shape(0,0)", __LINE__, __FILE__)
    end subroutine test_ops_building_alloc_zero

    subroutine test_ops_building_get_factor()
    use m_ops_building
    use m_commonfile
    use m_error
    use no_pfunit
        real, parameter :: tol = 1e-5
        integer, parameter :: buildingType = 1
        real, parameter :: angle_SR_xaxis(3) = (/ 0., 0., 3.29999847E+02/) 
        real, parameter :: dist(3)           = (/ 500., 600.,  19499.5703 /)
        real, parameter :: buildingFactDistances(8) = (/   7.00000000E+01,   9.00000000E+01,   1.30000000E+02, &
             2.10000000E+02,   3.70000000E+02,   6.90000000E+02,   1.33000000E+03,   3.00000000E+03/)
        real, parameter :: buildingFactAngleSRxaxis(9) = (/   0.00000000E+00,   4.50000000E+01,   9.00000000E+01, &
             1.35000000E+02,   1.80000000E+02,   2.25000000E+02,   2.70000000E+02,   3.15000000E+02,   3.60000000E+02/)
        real, parameter :: buildingFactFunction(9,8) = reshape( (/   1.54377818E+00,   1.57741714E+00, &
             1.68123484E+00,   1.76957023E+00,   1.88710666E+00,   1.65668344E+00,   1.84550810E+00,   1.63974583E+00, &
             1.54377818E+00,   1.46823788E+00,   1.49874067E+00,   1.57835102E+00,   1.65640104E+00,   1.74576449E+00, &
             1.57175374E+00,   1.71376848E+00,   1.55522335E+00,   1.46823788E+00,   1.35723639E+00,   1.37921560E+00, &
             1.43098211E+00,   1.49309754E+00,   1.55026674E+00,   1.43490875E+00,   1.52993333E+00,   1.42304444E+00, &
             1.35723639E+00,   1.24897563E+00,   1.26376581E+00,   1.29533517E+00,   1.33527303E+00,   1.36528587E+00, &
             1.30029249E+00,   1.35596395E+00,   1.29287815E+00,   1.24897563E+00,   1.17060494E+00,   1.17954445E+00, &
             1.19833553E+00,   1.22388315E+00,   1.23704290E+00,   1.20503449E+00,   1.23454261E+00,   1.19797909E+00, &
             1.17060494E+00,   1.11929405E+00,   1.12367225E+00,   1.13471615E+00,   1.15163088E+00,   1.15470695E+00, &
             1.14309037E+00,   1.15876389E+00,   1.13580680E+00,   1.11929405E+00,   1.08940601E+00,   1.08954787E+00, &
             1.09838676E+00,   1.10884416E+00,   1.10815740E+00,   1.10757768E+00,   1.11554980E+00,   1.10013604E+00, &
             1.08940601E+00,   1.06534719E+00,   1.06332636E+00,   1.07177258E+00,   1.07758367E+00,   1.07519138E+00, &
             1.08052170E+00,   1.08438277E+00,   1.07259154E+00,   1.06534719E+00/), &
          (/9,8/) )
        real :: buildingFact
        real, parameter :: ref_buildingFact(3) = (/  1.14975989, 1.13372517, 1.0 /)
        integer :: i
        do i = 1,size(ref_buildingFact,1) 
           call ops_building_get_factor( buildingType, angle_SR_xaxis(i), dist(i), &
                   buildingFactAngleSRxaxis, buildingFactDistances,  &
                   buildingFactFunction, buildingFact)
           call assertEqual( ref_buildingFact(i), buildingFact, tol, "buildingFact", __LINE__, __FILE__)
        end do
    end subroutine test_ops_building_get_factor

    subroutine test_ops_building_get_function()
    use m_commonfile
    use m_fileutils
    use m_ops_building
    use m_error
    use no_pfunit
    use m_commonfile
    use m_error
    use no_pfunit
        type(TError) :: error
        real, parameter :: tol = 1e-5
        integer, parameter :: nParam = 9
        integer, parameter :: nClass(9) = (/ 10, 4, 4, 9, 3, 3, 5, 9, 8/)
        real, allocatable :: buildingFactArray(:)
        real, parameter :: classdefinitionArray(55) = (/   0.00000000E+00,   4.00000000E+00,   6.00000000E+00, &
             8.00000000E+00,   1.00000000E+01,   1.20000000E+01,   1.40000000E+01,   1.60000000E+01,   1.80000000E+01, &
             2.00000000E+01,   4.00000006E-01,   1.50000000E+00,   4.40000010E+00,   8.39999962E+00,   5.00000000E-01, &
             1.50000000E+00,   2.50000000E+00,   5.00000000E+00,   4.00000000E+00,   6.00000000E+00,   8.00000000E+00, &
             1.00000000E+01,   1.20000000E+01,   1.40000000E+01,   1.60000000E+01,   1.80000000E+01,   2.00000000E+01, &
             3.00000000E+01,   7.50000000E+01,   1.00000000E+02,   2.50000000E-01,   5.00000000E-01,   1.00000000E+00, &
             0.00000000E+00,   4.50000000E+01,   9.00000000E+01,   1.35000000E+02,   1.80000000E+02,   0.00000000E+00, &
             4.50000000E+01,   9.00000000E+01,   1.35000000E+02,   1.80000000E+02,   2.25000000E+02,   2.70000000E+02, &
             3.15000000E+02,   3.60000000E+02,   7.00000000E+01,   9.00000000E+01,   1.30000000E+02,   2.10000000E+02, &
             3.70000000E+02,   6.90000000E+02,   1.33000000E+03,   3.00000000E+03/)
        real, parameter :: buildingFactAngleSRxaxis(9) = (/   0.00000000E+00,   4.50000000E+01,   9.00000000E+01, &
             1.35000000E+02,   1.80000000E+02,   2.25000000E+02,   2.70000000E+02,   3.15000000E+02,   3.60000000E+02/)
        real, parameter :: buildingFactDistances(8) = (/   7.00000000E+01,   9.00000000E+01,   1.30000000E+02, &
             2.10000000E+02,   3.70000000E+02,   6.90000000E+02,   1.33000000E+03,   3.00000000E+03/)
        real :: valueArray(9)
        real, parameter :: refin_valueArray(9) = (/   5.00000000E+00,   4.00000006E-01,   5.00000000E-01,   5.00000000E+00, &
             4.00000000E+01,   5.00000000E-01,   0.00000000E+00,   0.00000000E+00,   7.00000000E+01/)
        real, parameter :: ref_valueArray(9) = (/   5.00000000E+00,   4.00000006E-01,   5.00000000E-01, &
             5.00000000E+00,   4.00000000E+01,   5.00000000E-01,   0.00000000E+00,   3.60000000E+02,   3.00000000E+03/)
        real, allocatable :: buildingFactFunction(:,:)
        real, parameter :: ref_buildingFactFunction(9,8) = reshape( (/   1.54377818E+00,   1.57741714E+00, &
             1.68123484E+00,   1.76957023E+00,   1.88710666E+00,   1.65668344E+00,   1.84550810E+00,   1.63974583E+00, &
             1.54377818E+00,   1.46823788E+00,   1.49874067E+00,   1.57835102E+00,   1.65640104E+00,   1.74576449E+00, &
             1.57175374E+00,   1.71376848E+00,   1.55522335E+00,   1.46823788E+00,   1.35723639E+00,   1.37921560E+00, &
             1.43098211E+00,   1.49309754E+00,   1.55026674E+00,   1.43490875E+00,   1.52993333E+00,   1.42304444E+00, &
             1.35723639E+00,   1.24897563E+00,   1.26376581E+00,   1.29533517E+00,   1.33527303E+00,   1.36528587E+00, &
             1.30029249E+00,   1.35596395E+00,   1.29287815E+00,   1.24897563E+00,   1.17060494E+00,   1.17954445E+00, &
             1.19833553E+00,   1.22388315E+00,   1.23704290E+00,   1.20503449E+00,   1.23454261E+00,   1.19797909E+00, &
             1.17060494E+00,   1.11929405E+00,   1.12367225E+00,   1.13471615E+00,   1.15163088E+00,   1.15470695E+00, &
             1.14309037E+00,   1.15876389E+00,   1.13580680E+00,   1.11929405E+00,   1.08940601E+00,   1.08954787E+00, &
             1.09838676E+00,   1.10884416E+00,   1.10815740E+00,   1.10757768E+00,   1.11554980E+00,   1.10013604E+00, &
             1.08940601E+00,   1.06534719E+00,   1.06332636E+00,   1.07177258E+00,   1.07758367E+00,   1.07519138E+00, &
             1.08052170E+00,   1.08438277E+00,   1.07259154E+00,   1.06534719E+00/), &
          (/9,8/) )
        integer, parameter :: nClassProd = 4665600

        datadir = '.\level_1/resources/'
        call fix_slashes(datadir)
        call ops_building_file_names(error)
        if (error%haserror) call write_error(IOB_STDOUT, error)
        call assertFalse( error%haserror, "ops_building_file_names", __LINE__, __FILE__)
        call ops_building_read_building_factors( nClassProd, buildingFactArray, error)
        if (error%haserror) call write_error(IOB_STDOUT, error)
        call assertFalse( error%haserror, "ops_building_read_building_factors", __LINE__, __FILE__)
        valueArray = refin_valueArray
        call ops_building_get_function( nParam, valueArray, nClass, classdefinitionArray, buildingFactAngleSRxaxis, &
            buildingFactDistances, buildingFactArray, buildingFactFunction, error)
        if (error%haserror) call write_error(IOB_STDOUT, error)
        call assertFalse( error%haserror, "ops_building_get_function", __LINE__, __FILE__)
        call assertEqual( ref_valueArray, valueArray, tol, "valueArray", __LINE__, __FILE__)
        call assertEqual( ref_buildingFactFunction, buildingFactFunction, tol, "buildingFactFunction", __LINE__, __FILE__)

    end subroutine test_ops_building_get_function

    subroutine test_ops_building_read_building_factors()
    use m_fileutils
    use m_commonfile
    use m_ops_building
    use m_error
    use no_pfunit
        type(TError) :: error
        real, parameter :: tol = 1e-5
        integer, parameter :: nParam = 9
        integer, parameter :: nClassProd = 4665600
        integer, parameter :: nClass(9) = (/ 10, 4, 4, 9, 3, 3, 5, 9, 8/)
        real, allocatable :: buildingFactArray(:)
        real, parameter :: sumFacts=   8328344.78463, sumFacts2 =   56546736.5833

        datadir = '.\level_1/resources/'
        call fix_slashes(datadir)
        call ops_building_file_names(error)
        if (error%haserror) call write_error(IOB_STDOUT, error)
        call assertFalse( error%haserror, "ops_building_file_names", __LINE__, __FILE__)

        call ops_building_read_building_factors( nClassProd, buildingFactArray, error)
        if (error%haserror) call write_error(IOB_STDOUT, error)
        call assertFalse( error%haserror, "ops_building_read_building_factors", __LINE__, __FILE__)

        call assertRelativelyEqual( sum(dble(buildingFactArray)), dble(sumFacts), tol, "buildingFactArray", __LINE__, __FILE__)
        call assertRelativelyEqual( sum(dble(buildingFactArray)**2), dble(sumFacts2), tol, "buildingFactArray**2", __LINE__, __FILE__)
    end subroutine test_ops_building_read_building_factors

    subroutine test_ops_building_read_classes()
    use m_commonfile
    use m_error
    use m_fileutils
    use m_ops_building
    use no_pfunit
        type(TError) :: error
        real, parameter :: tol = 1e-5
        integer, parameter :: mParam1 = 9
        integer, parameter :: mClass1 = 100
        real, allocatable :: classdefinitionArray(:)
        real, parameter :: ref_classdefinitionArray(55) = (/   0.00000000E+00,   4.00000000E+00,   6.00000000E+00, &
             8.00000000E+00,   1.00000000E+01,   1.20000000E+01,   1.40000000E+01,   1.60000000E+01,   1.80000000E+01, &
             2.00000000E+01,   4.00000006E-01,   1.50000000E+00,   4.40000010E+00,   8.39999962E+00,   5.00000000E-01, &
             1.50000000E+00,   2.50000000E+00,   5.00000000E+00,   4.00000000E+00,   6.00000000E+00,   8.00000000E+00, &
             1.00000000E+01,   1.20000000E+01,   1.40000000E+01,   1.60000000E+01,   1.80000000E+01,   2.00000000E+01, &
             3.00000000E+01,   7.50000000E+01,   1.00000000E+02,   2.50000000E-01,   5.00000000E-01,   1.00000000E+00, &
             0.00000000E+00,   4.50000000E+01,   9.00000000E+01,   1.35000000E+02,   1.80000000E+02,   0.00000000E+00, &
             4.50000000E+01,   9.00000000E+01,   1.35000000E+02,   1.80000000E+02,   2.25000000E+02,   2.70000000E+02, &
             3.15000000E+02,   3.60000000E+02,   7.00000000E+01,   9.00000000E+01,   1.30000000E+02,   2.10000000E+02, &
             3.70000000E+02,   6.90000000E+02,   1.33000000E+03,   3.00000000E+03/)
        real, allocatable :: buildingFactAngleSRxaxis(:)
        real, parameter :: ref_buildingFactAngleSRxaxis(9) = (/   0.00000000E+00,   4.50000000E+01, &
             9.00000000E+01,   1.35000000E+02,   1.80000000E+02,   2.25000000E+02,   2.70000000E+02,   3.15000000E+02, &
             3.60000000E+02/)
        real, allocatable :: buildingFactDistances(:)
        real, parameter :: ref_buildingFactDistances(8) = (/   7.00000000E+01,   9.00000000E+01,   1.30000000E+02, &
             2.10000000E+02,   3.70000000E+02,   6.90000000E+02,   1.33000000E+03,   3.00000000E+03/)
        integer :: nParam
        integer, parameter :: ref_nParam = 9
        integer :: nClass(9)
        integer, parameter :: ref_nClass(9) = (/ 10, 4, 4, 9, 3, 3, 5, 9, 8/)
        real :: minClass(9)
        real, parameter :: ref_minClass(9) = (/   0.00000000E+00,   4.00000006E-01,   5.00000000E-01, &
             4.00000000E+00,   3.00000000E+01,   2.50000000E-01,   0.00000000E+00,   0.00000000E+00,   7.00000000E+01 &
          /)
        real :: maxClass(9)
        real, parameter :: ref_maxClass(9) = (/   2.00000000E+01,   8.39999962E+00,   5.00000000E+00, &
             2.00000000E+01,   1.00000000E+02,   1.00000000E+00,   1.80000000E+02,   3.60000000E+02,   3.00000000E+03 &
          /)
        integer :: nClassProd
        integer, parameter :: ref_nClassProd = 4665600

        datadir = '.\level_1/resources/'
        call fix_slashes(datadir)
        call ops_building_file_names(error)
        call ops_building_read_classes( mParam, mClass, classdefinitionArray, buildingFactAngleSRxaxis, &
            buildingFactDistances, nParam, nClass, minClass, maxClass, nClassProd, error)
        if (error%haserror) call write_error(IOB_STDOUT, error)
        call assertFalse( error%haserror, "ops_building_read_classes", __LINE__, __FILE__)
        call assertEqual( ref_classdefinitionArray, classdefinitionArray, tol, "classdefinitionArray", __LINE__, __FILE__)
        call assertEqual( ref_buildingFactAngleSRxaxis, buildingFactAngleSRxaxis, tol, "buildingFactAngleSRxaxis", __LINE__, __FILE__)
        call assertEqual( ref_buildingFactDistances, buildingFactDistances, tol, "buildingFactDistances", __LINE__, __FILE__)
        call assertEqual( ref_nParam, nParam, "nParam", __LINE__, __FILE__)
        call assertEqual( ref_nClass, nClass, "nClass", __LINE__, __FILE__)
        call assertEqual( ref_minClass, minClass, tol, "minClass", __LINE__, __FILE__)
        call assertEqual( ref_maxClass, maxClass, tol, "maxClass", __LINE__, __FILE__)
        call assertEqual( ref_nClassProd, nClassProd, "nClassProd", __LINE__, __FILE__)
    end subroutine test_ops_building_read_classes

    subroutine test_ops_building_read_tables()
    use m_commonfile
    use m_error
    use m_fileutils
    use m_ops_building
    use no_pfunit
        type(tbuildingEffect) :: buildingEffect
        type(TError) :: error
        real, parameter :: tol = 1e-5
        integer, parameter :: mParam1 = 9
        integer, parameter :: mClass1 = 100
        real, parameter :: ref_classdefinitionArray(55) = (/   0.00000000E+00,   4.00000000E+00,   6.00000000E+00, &
             8.00000000E+00,   1.00000000E+01,   1.20000000E+01,   1.40000000E+01,   1.60000000E+01,   1.80000000E+01, &
             2.00000000E+01,   4.00000006E-01,   1.50000000E+00,   4.40000010E+00,   8.39999962E+00,   5.00000000E-01, &
             1.50000000E+00,   2.50000000E+00,   5.00000000E+00,   4.00000000E+00,   6.00000000E+00,   8.00000000E+00, &
             1.00000000E+01,   1.20000000E+01,   1.40000000E+01,   1.60000000E+01,   1.80000000E+01,   2.00000000E+01, &
             3.00000000E+01,   7.50000000E+01,   1.00000000E+02,   2.50000000E-01,   5.00000000E-01,   1.00000000E+00, &
             0.00000000E+00,   4.50000000E+01,   9.00000000E+01,   1.35000000E+02,   1.80000000E+02,   0.00000000E+00, &
             4.50000000E+01,   9.00000000E+01,   1.35000000E+02,   1.80000000E+02,   2.25000000E+02,   2.70000000E+02, &
             3.15000000E+02,   3.60000000E+02,   7.00000000E+01,   9.00000000E+01,   1.30000000E+02,   2.10000000E+02, &
             3.70000000E+02,   6.90000000E+02,   1.33000000E+03,   3.00000000E+03/)
        real, parameter :: ref_buildingFactAngleSRxaxis(9) = (/   0.00000000E+00,   4.50000000E+01, &
             9.00000000E+01,   1.35000000E+02,   1.80000000E+02,   2.25000000E+02,   2.70000000E+02,   3.15000000E+02, &
             3.60000000E+02/)
        real, parameter :: ref_buildingFactDistances(8) = (/   7.00000000E+01,   9.00000000E+01,   1.30000000E+02, &
             2.10000000E+02,   3.70000000E+02,   6.90000000E+02,   1.33000000E+03,   3.00000000E+03/)
        integer, parameter :: ref_nParam = 9
        integer, parameter :: ref_nClass(9) = (/ 10, 4, 4, 9, 3, 3, 5, 9, 8/)
        real, parameter :: ref_minClass(9) = (/   0.00000000E+00,   4.00000006E-01,   5.00000000E-01, &
             4.00000000E+00,   3.00000000E+01,   2.50000000E-01,   0.00000000E+00,   0.00000000E+00,   7.00000000E+01 &
          /)
        real, parameter :: ref_maxClass(9) = (/   2.00000000E+01,   8.39999962E+00,   5.00000000E+00, &
             2.00000000E+01,   1.00000000E+02,   1.00000000E+00,   1.80000000E+02,   3.60000000E+02,   3.00000000E+03 &
          /)
        integer, parameter :: ref_nClassProd = 4665600
        real, parameter :: sumFacts=   8328344.78463, sumFacts2 =   56546736.5833

        datadir = '.\level_1/resources/'
        call fix_slashes(datadir)
        call ops_building_file_names(error)
        call ops_building_read_tables(buildingEffect, error)
        if (error%haserror) call write_error(IOB_STDOUT, error)
        call assertFalse( error%haserror, "ops_building_read_tables", __LINE__, __FILE__)
        call assertEqual( ref_classdefinitionArray, buildingEffect%classdefinitionArray, tol, "classdefinitionArray", __LINE__, __FILE__)
        call assertEqual( ref_buildingFactAngleSRxaxis, buildingEffect%buildingFactAngleSRxaxis, tol, "buildingFactAngleSRxaxis", __LINE__, __FILE__)
        call assertEqual( ref_buildingFactDistances, buildingEffect%buildingFactDistances, tol, "buildingFactDistances", __LINE__, __FILE__)
        call assertEqual( ref_nParam, buildingEffect%nParam, "nParam", __LINE__, __FILE__)
        call assertEqual( ref_nClass, buildingEffect%nClass, "nClass", __LINE__, __FILE__)
        call assertEqual( ref_minClass, buildingEffect%minClass, tol, "minClass", __LINE__, __FILE__)
        call assertEqual( ref_maxClass, buildingEffect%maxClass, tol, "maxClass", __LINE__, __FILE__)
        call assertRelativelyEqual( sum(dble(buildingEffect%buildingFactArray)), dble(sumFacts), tol, "buildingFactArray", __LINE__, __FILE__)
        call assertRelativelyEqual( sum(dble(buildingEffect%buildingFactArray)**2), dble(sumFacts2), tol, "buildingFactArray**2", __LINE__, __FILE__)
   end subroutine test_ops_building_read_tables
end module m_test_building

program test_building
use m_test_building
use no_pfunit
   call test_ops_building_alloc_zero
   call test_ops_building_get_factor()
   call test_ops_building_get_function()
   call test_ops_building_read_building_factors()
   call test_ops_building_read_classes()
   call test_ops_building_read_tables()
   call conclusion()
end program test_building


