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
module m_ops_building

implicit none

! module for simulating building effect by means of a multidimensional table with building effect factors.

!  General setup up of multi dimensional lookup table
!
!  1) Class definition file:
!  A file with n parameters at n rows; each row contains a parameter with a generic names of the parameter in the first column,
!  followed by a number of columns with representative parameter values for each class. Note that each parameter can have a different number of classes.
!  Example file:
!  p1  5.0  9.0  16.0  25  50   75   100
!  p2  5.0  9.0  20.0
!  **
!  pn  10  20  30  40  50  60
!
!  Last two parameters must be (source-receptor angle, source-receptor distance)
!
!  2) Lookup table:
!  Table with n + 1 columns containing the class indices for n parameters and the associated building effect factor.
!  Example lookup table
!  1. last parameter varies first, then last but one, ... THIS IS ESSENTIAL FOR CORRECT READING OF THE DATA!
!  2. !  Last two parameters must be (source-receptor angle, source-receptor distance
!    p1     p2   ***    pn  buildingFact
!     1      1           1          2.20
!     1      1           2          2.10
!     1      1           3          1.90
!     1      1           4          1.85
!     1      1           5          1.20
!     1      1           6          1.00
!     1      2           1          2.30
!     1      2           2          2.15
!     .......
!     3      2           5          1.26
!     3      2           6          1.05
!
!  Note that class i of a parameter corresponds to column i+1 for this parameter in the class definition file.

private
public mParam, mClass
public Tbuilding, TbuildingEffect
public ops_building_file_names, ops_building_read_tables, ops_building_read_classes, ops_building_read_building_factors, ops_building_get_function, ops_building_get_factor

integer, parameter         :: mParam = 9                       ! maximal number of parameters
integer, parameter         :: mClass = 100                     ! maximal number of classes for any parameter

! Define parameter names - these must be the same as the parameters as filled into valueArray (see ops_bron_rek) - distance must be last parameter
!character(len=200)         :: buildingParamNames(3) = (/'hEmis', 'angleSRxaxis', 'distance' /)  ! 3 parameters, simple test 
integer, parameter         :: mBparms = 9
character(len=200)         :: CbuildingParamNames(mBparms) =  &
                                 (/'hEmis              ', &
                                   'V_stack            ', &
                                   'D_stack            ', &
                                   'buildingHeight     ', &
                                   'buildingLength     ', &
                                   'buildingWLRatio    ', &
                                   'buildingOrientation', &
                                   'angleSRxaxis       ', &
                                   'distance           ' /)  ! 9 parameters
! character(len=200)         :: buildingParamNames(7) = (/'hEmis', 'V_stack', 'D_stack', 'buildingHeight', 'buildingLength', 'buildingWLRatio', 'distance' /)  ! 7 parameters
! character(len=200)         :: buildingParamNames(4) = (/'V_stack', 'buildingHeight', 'hEmis', 'distance' /)  ! simple test with 4 parameters

Type Tbuilding
   real      :: length                                    ! building length [m]
   real      :: width                                     ! building width [m]
   real      :: height                                    ! building height [m]
   real      :: orientation                               ! building orientation (degrees w.r.t. North)
   real, allocatable :: buildingFactFunction(:,:)         ! building effect function (function of source receptor angle, source receptor distance)
   integer   :: type                                      ! building type for determining distance function for building effect [-]; type = 0 -> no building effect
End Type Tbuilding

type TbuildingEffect
    integer           :: nParam                           ! number of building parameters (read from file)
    real, allocatable :: classdefinitionArray(:)          ! array with representative class values for each parameter
                                                          ! (stored in one-dimensional array: [nClass(1) values for p1, nClass(2) values for p2, ...])
    integer           :: nClass(mParam)                   ! number of classes for each parameter
    real              :: minClass(mParam)                 ! minimum of class values for each parameter
    real              :: maxClass(mParam)                 ! maximum of class values for each parameter
    real, allocatable :: buildingFactArray(:)             ! building effect factors for each parameter/class, stored in a one-dimensional array
    real, allocatable :: buildingFactAngleSRxaxis(:)      ! source receptor angles (w.r.t. x-axis) where to evaluate 2D function of building effect
    real, allocatable :: buildingFactDistances(:)         ! distances where to evaluate 2D function of building effect
end type TbuildingEffect

contains

!-----------------------------------------------------------------------------------
subroutine ops_building_file_names(error)

! Set standard file names for building effect tables

USE m_error
USE m_fileutils
USE m_commonfile

CHARACTER*512, PARAMETER   :: ROUTINENAAM = 'ops_building_file_names'

type(Terror),          intent(out) :: error                    ! error handling record

! Set standard file names for building effect tables:
CALL MakeCommonPath(BUILDINGCLASSFILE, buildingClassFilename, error)
CALL MakeCommonPath(BUILDINGFACTFILE, buildingFactFilename, error)
if (error%haserror) goto 9999

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)

end subroutine ops_building_file_names

!-----------------------------------------------------------------------------------
subroutine ops_building_read_tables(buildingEffect, error)

! Read class definition of building parameters and factors for building effect

use m_error

CHARACTER*512, PARAMETER   :: ROUTINENAAM = 'ops_building_read_tables'

! Output:
type(tbuildingEffect), intent(out) :: buildingEffect           ! structure containing data for building effect
type(Terror),          intent(out) :: error                    ! error handling record

! Local:
integer               :: nClassProd                            ! product of number of classes for each parameter

! Read classes for building parameters:
call ops_building_read_classes(mParam, mClass, buildingEffect%classdefinitionArray, buildingEffect%buildingFactAngleSRxaxis, buildingEffect%buildingFactDistances, &
                               buildingEffect%nParam, buildingEffect%nClass, buildingEffect%minClass, buildingEffect%maxClass, nClassProd, error)
if (error%haserror) goto 9999

! Read building factors:
call ops_building_read_building_factors(mClass, buildingEffect%nParam, nClassProd, buildingEffect%nClass, buildingEffect%buildingFactArray, error)
if (error%haserror) goto 9999

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)

end subroutine ops_building_read_tables

!-----------------------------------------------------------------------------------
subroutine ops_building_read_classes(mParam, mClass, &
           classdefinitionArray, buildingFactAngleSRxaxis, buildingFactDistances, nParam, nClass, minClass, maxClass, nClassProd, error)

use m_commonfile, only: buildingClassFilename, fu_tmp
use m_error
use m_fileutils

    CHARACTER*512, PARAMETER   :: ROUTINENAAM = 'ops_building_read_classes'

    ! Input:
    integer,  intent(in)       :: mParam                           ! maximal number of parameters
    integer,  intent(in)       :: mClass                           ! maximal number of classes for any parameter

    ! Output:
    real, allocatable, intent(out) :: classdefinitionArray(:)      ! array with representative class values for each parameter
                                                                   ! (stored in one-dimensional array: [nClass(1) values for p1, nClass(2) values for p2, ...])
    real, allocatable, intent(out) :: buildingFactAngleSRxaxis(:)  ! source rceptor angles (w.r.t. x-axis) where to evaluate 2D function of building effect
    real, allocatable, intent(out) :: buildingFactDistances(:)     ! distances where to evaluate 2D function of building effect
    integer, intent(out)       :: nParam                           ! actual number of parameters (read from file)
    integer, intent(out)       :: nClass(mParam)                   ! number of classes for each parameter
    real   , intent(out)       :: minClass(mParam)                 ! minimum of class values for each parameter
    real   , intent(out)       :: maxClass(mParam)                 ! maximum of class values for each parameter
    integer, intent(out)       :: nClassProd                       ! product of number of classes for each parameter
    type(Terror), intent(out)  :: error                            ! error handling record

    ! Local:
    real                       :: classdefinitionArrayTemp(mClass*mParam)  ! temporary array for reading classdefinitionArray
    integer                    :: iParam                           ! index of parameter
    character(1000)            :: line                             ! line read from file
    integer                    :: ilast                            ! index of last value in classdefinitionArrayTemp (during reading)
    character(100)             :: pName                            ! parameter name
    real                       :: pVals( mClass )                  ! representative parameter values for classes for one parameter
    integer                    :: n                                ! number of values read from file
    character(100)             :: paramNames(mParam)               ! parameter names
    integer                    :: nClassSum                        ! sum of number of classes for each parameter
character(len=200) :: buildingParamNames(mBparms)
integer :: j
    do j = 1, mBparms
       buildingParamNames(j) = trim( CbuildingParamNames(j) )
    enddo


    ! Initialisation:
    iParam = 0
    ilast  = 0  ! index of last value in classdefinitionArrayTemp

    ! Open file:
    IF (.NOT. sysopen(fu_tmp, buildingClassFilename, 'r', 'class definition file for building effect', error)) GOTO 9999

    ! Loop over lines in file:
    do
        ! Read line from file and split into name and values:
        read( fu_tmp, "(a)", end=500 ) line
        call split1( mClass, line, pName, pVals, n )
        if ( n == 0 ) then
           call SetError('No parameter values found in file ',error)
           goto 9998
        else
            ! New parameter has been read:
            !write(*,'(a30,99(1x,f8.3))') pName, pVals(1:n)
            iParam = iParam + 1
            if (iParam .gt. mParam) then
               call SetError('Too many parameters in file ',error)
               call ErrorParam('maximal number of parameters allowed', mParam, error)
               goto 9998
            endif

            ! Set number of classes for this parameter and fill paramNames and classdefinitionArrayTemp:
            nClass(iParam)     = n
            paramNames(iParam) = pName
            classdefinitionArrayTemp(ilast+1:ilast+n) = pVals(1:n)   !  note that pVals has to be sorted .. check?
            minClass(iParam) = minval(pVals(1:n))
            maxClass(iParam) = maxval(pVals(1:n))
            ilast = ilast + n
        endif
    enddo
500 continue
    close( fu_tmp )

    ! Now we know the number of parameters and the number of classes:
    nParam     = iParam
    nClassSum  = sum(nClass(1:nParam))
    nClassProd = product(nClass(1:nParam))

    ! Check:
    if (ilast .ne. nClassSum) then
       write(*,*) 'Internal programming error in ', ROUTINENAAM
       write(*,*) 'ilast = ',ilast, ' nClassSum = ',nClassSum
       write(*,*) 'ilast must be nClassSum  '
       stop
    endif

    ! Check parameter names:
    if (any(paramNames(1:nParam) .ne. buildingParamNames)) then
        call SetError('Error in parameter names ',error)
        call ErrorParam('parameter names in file ', paramNames(1:nParam), error)
        call ErrorParam('expected parameter names', buildingParamNames, error)
        goto 9999
    endif

    ! **** Allocate memory and fill class definition table *****

    allocate(classdefinitionArray(nClassSum))
    classdefinitionArray = classdefinitionArrayTemp(1:nClassSum)

    ! Allocate and fill array with source rceptor angles (w.r.t. x-axis) used to evaluate building factors
    ! (one but last parameter in classdefinitionArray):
    allocate(buildingFactAngleSRxaxis(nClass(nParam-1)))
    buildingFactAngleSRxaxis = classdefinitionArray(nClassSum - nClass(nParam) - nClass(nParam-1) + 1 : nClassSum - nClass(nParam))

    ! Allocate and fill array with distances used to evaluate building factors
    ! (last parameter in classdefinitionArray):
    allocate(buildingFactDistances(nClass(nParam)))
    buildingFactDistances = classdefinitionArray(nClassSum - nClass(nParam) + 1 : nClassSum)

    !write(*,*) 'ops_building_read_classes/buildingFactDistances:',buildingFactDistances
    !write(*,*) 'ops_building_read_classes/buildingFactAngleSRxaxis:',buildingFactAngleSRxaxis

    RETURN

9998 CALL ErrorParam('line read from file', trim(line), error)

9999 CALL ErrorParam('file name', buildingClassFilename, error)
CALL ErrorCall(ROUTINENAAM, error)

end subroutine ops_building_read_classes

!-----------------------------------------------------------------------------------------
subroutine ops_building_read_building_factors(mClass, nParam, nClassProd, nClass, buildingFactArray, error)

use m_commonfile, only: buildingFactFilename, fu_tmp
use m_error
use m_fileutils

   ! **** Read factors for building effects table from file *****

    CHARACTER*512, PARAMETER   :: ROUTINENAAM = 'ops_building_read_building_factors'

    ! Input:
    integer,  intent(in)       :: mClass                           ! maximal number of classes for any parameter
    integer,  intent(in)       :: nParam                           ! actual number of parameters (read from file)
    integer,  intent(in)       :: nClassProd                       ! product of number of classes for each parameter
    integer,  intent(in)       :: nClass(:)                        ! number of classes for each parameter


    ! Output:
    real, allocatable, intent(out) :: buildingFactArray(:)         ! building effect factors for each parameter/class, stored in a one-dimensional array
    type(Terror), intent(out)      :: error                        ! error handling record

    ! Local:
    character(1000)            :: line                             ! line read from file
    integer                    :: iLine                            ! index of line read (includes header line)
    integer                    :: iParam                           ! index of parameter
    character(100)             :: colNames(nParam+1)               ! column names in building effect table
    integer                    :: iClassRead(nParam)               ! class indices read from file
    real                       :: buildingFactInput                ! buiding effect factor read from input file
    character(len = 100)       :: fmt                              ! format for writeing output to screen
    integer                    :: i                                ! index (debug)
    integer                    :: iClassExpected(nParam)           ! class indices as expected by the order in which SILUPM wants it (last index fastest, then last but one, ...)
    logical                    :: shiftNext                        ! shift next parameter index (counting from last to first parameter)
    logical                    :: read_unformatted = .true.        ! read unformatted file (is much faster than formatted file)
character(len=200) :: buildingParamNames(mBparms)
integer :: j
    do j = 1, mBparms
       buildingParamNames(j) = trim( CbuildingParamNames(j) )
    enddo

    ! Allocate memory for  building effects table:
    allocate(buildingFactArray(nClassProd))

    if (read_unformatted) then
       ! Open file, read array with building factors and close file:
       IF (.NOT. sysopen(fu_tmp, buildingFactFilename, 'rb', 'file with building effect factors', error)) GOTO 9999
       read(fu_tmp) buildingFactArray
       close(fu_tmp)

    else
       !------------------------------------------------------------------------------------------------------
       ! This part of the subroutine is not used anymore in OPS; there is a separate program to convert
       ! the ASCII table into an unformatted file which read musch faster. This separate program uses
       ! the code below.
       !------------------------------------------------------------------------------------------------------

       ! Construct format for write to screen
       fmt = '(i6,": ",  (1x,i4),1x,f8.3)'
       write(fmt(10:11),'(i2)') nParam

       ! Open file:
       IF (.NOT. sysopen(fu_tmp, buildingFactFilename, 'r', 'file with building effect factors', error)) GOTO 9999

       ! Initialise:
       iClassExpected = 1

       ! Read file until end-of-file:
       iLine = 0
       do
           read( fu_tmp, "(a)", end=510 ) line
           ! print *,line

           ! Skip empty line:
           if (len_trim(line) > 0) then
              iLine = iLine + 1
              if (iLine .eq. 1) then
                 ! Header line
                 read( line, *) colNames(1:nParam+1 )
                 !write(*,*) 'Table '
                 !write(*,'(99(1x,a))') colNames(1:nParam+1 )

                    ! Check parameter names:
                    if (any(colNames(1:nParam) .ne. buildingParamNames)) then
                        call SetError('Error in parameter names ',error)
                        call ErrorParam('parameter names in file ', colNames(1:nParam), error)
                        call ErrorParam('expected parameter names', buildingParamNames, error)
                        goto 9999
                    endif

              else
                ! Check number of lines read:
                if (iLine-1 .gt. nClassProd) then
                   call SetError('number of lines read from file larger than expected ',error)
                   call ErrorParam('line number ', iLine, error)
                   call ErrorParam('number of lines expected', nClassProd+1, error)   ! including header line
                   goto 9998
                endif

                ! Split line into nParam integer class indices and (last value) buiding effect factor:
                call split2( line, nParam, iClassRead, buildingFactInput)

                ! Check class indices read from file:
                if (any(iClassExpected .ne. iClassRead)) then
                   call SetError('Incorrect set of class indices.','Last index must vary fastest, then last but one, ...',error)
                   call ErrorParam('line number ', iLine, error)
                   call ErrorParam('expected class indices', iClassExpected, error)
                   goto 9998
                endif

                ! Shift to next set of class indices ((must be in order for routine SILUPM: last index varies fast, then last but one, ...):
                shiftNext = .true.
                iParam = nParam
                do while (shiftNext .and. iParam .ge. 1)
                   iClassExpected(iParam) = iClassExpected(iParam) + 1

                   ! If this parameter exceeds the number of classes -> reset to 1 and shift to next parameter:
                   if (iClassExpected(iParam) .gt. nClass(iParam)) then
                      iClassExpected(iParam) = 1
                      shiftNext = .true.
                   else
                      shiftNext = .false.
                   endif
                   iParam = iParam - 1
                enddo

                ! Fill building effect factor into buildingFactArray;
                ! order of lines is essential here and has been checked above (iClassRead = iClassExpected). See definition SILUPM for 2D array ((y(x1(i), x2(j)), j=1:NTAB(2)), i=1:NTAB(1))
                ! if (iLine .le. 3) write(*,fmt) iLine-1,iClassRead(1:nParam),buildingFactInput
                buildingFactArray(iLine-1) = buildingFactInput
              endif ! iLine .eq. 1
           endif ! len_trim(line) > 0
       enddo
510    continue
       close( fu_tmp )

       ! write(*,'(a)') '............................'
       ! write(*,fmt) iLine-1,iClassRead(1:nparam),buildingFactInput

       ! Check number of lines read:
       if (iLine-1 .ne. nClassProd) then
          call SetError('number of lines read from file smaller than expected ',error)
          call ErrorParam('line number ', iLine, error)
          call ErrorParam('number of lines expected', nClassProd+1, error)  ! including header line
          goto 9999
       endif
    endif  ! read_unformatted

    ! **** Printing/checking building effects table *****
    if (.FALSE.) then
       do i = 1,nClassProd
          print *, i, buildingFactArray(i)
       enddo
    endif

    RETURN

9998 CALL ErrorParam('line read from file', trim(line), error)

9999 CALL ErrorParam('file name', buildingFactFilename, error)
CALL ErrorCall(ROUTINENAAM, error)

end subroutine ops_building_read_building_factors

!-----------------------------------------------------------------------------------------
! subroutine ops_building_get_function(nParam, valueArray, nClass, classdefinitionArray, buildingFactArray, buildingFactFunction, NTAB, NDEG, LUP, IOPT, EOPT)
subroutine ops_building_get_function(nParam, valueArray, nClass, classdefinitionArray, buildingFactAngleSRxaxis, buildingFactDistances, buildingFactArray, buildingFactFunction,error)

  ! Get 2D building effect function (function of source-receptor angle and distance to source) for a specific set of building parameter values in valueArray;
  ! interpolate this factor from factors in buildingFactArray, based on the location of valueArray within the table classdefinitionArray.

  use m_error

  CHARACTER*512, PARAMETER   :: ROUTINENAAM = 'ops_building_get_function'

  ! Input:
  integer,  intent(IN)    :: nParam                                  ! number of parameters
  integer,  intent(IN)    :: nClass(nParam)                          ! number of classes for each parameter
  real,     intent(IN)    :: classdefinitionArray(:)                 ! array with representative class values for each parameter
  real,     intent(IN)    :: buildingFactAngleSRxaxis(:)             ! source-receptor angles (w.r.t. x-axis) where to evaluate 2D building effect function
  real,     intent(IN)    :: buildingFactDistances(:)                ! distances where to evaluate 2D building effect function
  real,     intent(IN)    :: buildingFactArray(:)                    ! building effect factors for each parameter/class.

  ! Input/output:
  real,     intent(INOUT) :: valueArray(nParam)                      ! array with set of parameter values for specific building (output: values outside table are moved to boundaries of table)

  ! Output:
  real, allocatable, intent(OUT) :: buildingFactFunction(:,:)        ! 2D buiding effect function for specific building (function of angle, distance)
  type(Terror),      intent(out) :: error                            ! error handling record

  ! Arguments for SILUPM
  ! Local variables for SILUPM
  integer                 :: NTAB(2*nParam+1)
  integer                 :: NDEG(nParam)
  integer                 :: LUP(nParam)
  integer                 :: IOPT(3)            ! options used for output of SILUPM
  real                    :: EOPT(6*nParam)     ! error estimate

  ! Local:
  integer                 :: iParam             ! parameter index
  integer                 :: ix, iy             ! loop indices

   ! print *, "Building effects table from within subroutine getbuildingEffect"
   ! do ix = 1,size(buildingFactArray)
   !    print *, ix, buildingFactArray(ix)
   ! enddo

!  ! Interpolate multidimensional table:
!  ! CALL SILUPM(NDIM, X, Y, NTAB, XT, YT, NDEG, LUP, IOPT, EOPT)
!  ! NDIM = nParam
!  ! X = ValueArray (input for a specific building)
!  ! Y = buildingFactFunction(ix,iy) (output)
!  ! NTAB = number of values (classes) for each parameter + extra spzce needed for SILUPM
!  ! XT   = class parameter values, stored in a one-dimensional array (NTAB(1) values for p1, NTAB(2) values for p2, ...) = classdefinitionArray
!  ! YT   = building factors for each parameter/class, stored in a one-dimensional array ; see doc SILUPM  = buildingFactArray
!  ! NDEG = degree of polynomial used for interpolation (= 1 -> linear interpolation).
!  ! LUP  = type of lookup method (binary search or sequntial search, see doc SILUPM)
!  ! IOPT = options used for output


  NTAB(1:nParam) = nClass(1:nParam)
  NTAB(nParam+1) = 0   ! as required by SILUPM
  NDEG = 1
  LUP  = 1 ! binary search

  ! Set IOPT:
  IOPT(1) = 1
  !IOPT(2) = 0
  !IOPT(3) = 0
  IOPT(2) = 6*nParam; ! size(EOPT)
  IOPT(3) = 0

  ! Loop over distances for building effect function:
  allocate(buildingFactFunction(size(buildingFactAngleSRxaxis),size(buildingFactDistances)))

  ! write(*,*) '==================================================================================='
  ! write(*,*) 'ops_building_get_function/valueArray = ',ValueArray
  ! write(*,*) 'ops_building_get_function/buildingFactAngleSRxaxis: ',     buildingFactAngleSRxaxis
  ! write(*,*) 'ops_building_get_function/buildingFactDistances: ',     buildingFactDistances
  ! write(*,*) 'ops_building_get_function/classdefinitionArray: ',classdefinitionArray
  ! write(*,*) 'ops_building_get_function/nParam:',nParam
  ! write(*,*) 'ops_building_get_function/buildingFactArray:',buildingFactArray  ! can be very large

  ! Loop over angles and distances for building effect function:
  do iy = 1,size(buildingFactDistances)
     do ix = 1,size(buildingFactAngleSRxaxis)

        ! Put current angle, distance as last two values in valueArray:
        valueArray(nParam-1) = buildingFactAngleSRxaxis(ix)
        valueArray(nParam)   = buildingFactDistances(iy)

        ! Look up building factor in table and put factor into buildingFactFunction(ix,iy):
        CALL SILUPM(nParam, ValueArray, buildingFactFunction(ix,iy), NTAB, classdefinitionArray, buildingFactArray, NDEG, LUP, IOPT, EOPT)
     enddo
  enddo

  ! do ix = 1,size(buildingFactAngleSRxaxis)
  !    write(*,*) 'ops_building_get_function/buildingFactFunction for angle ',buildingFactAngleSRxaxis(ix),'degrees : ', buildingFactFunction(ix,:)
  ! enddo
  ! write(*,*) '==================================================================================='

  if (IOPT(1) .ne. 0) then
     if (IOPT(1) .eq. 1) then
        call SetError('Error in look up in table of building factors; parameter values outside domain of the table ',error)
     else
        call ErrorParam('error status (see documentation netlib/SILUPM) ', IOPT(1), error)
     endif
     call ErrorParam('parameter names  ', CbuildingParamNames, error)
     call ErrorParam('parameter values ', valueArray, error) 
     goto 9999
  endif

  RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)

end subroutine ops_building_get_function

!-------------------------------------------------------------------------------------------------------------------------------
!                       Copyright by
!   National Institute of Public Health and Environment
!           Laboratory for Air Research (RIVM/LLO)
!                      The Netherlands
!   No part of this software may be used, copied or distributed without permission of RIVM/LLO (2002)
!
! SUBROUTINE
! NAME                : %M%
! SCCS (SOURCE)       : %P%
! RELEASE - LEVEL     : %R% - %L%
! BRANCH - SEQUENCE   : %B% - %S%
! DATE - TIME         : %E% - %U%
! WHAT                : %W%:%E%
! AUTHOR              : OPS-support
! FIRM/INSTITUTE      : RIVM/LLO
! LANGUAGE            : FORTRAN-77/90
! DESCRIPTION         : Returns closest and interpolated building effect based on "buildingEffectTable",
! DESCRIPTION         : given a source catergory and a distance from source to receptor.
! EXIT CODES          :
! FILES I/O DEVICES   :
! SYSTEM DEPENDENCIES : HP Fortran
! CALLED FUNCTIONS    :
! UPDATE HISTORY      :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_building_get_factor(buildingType, angle_SR_xaxis, dist, buildingFactAngleSRxaxis, buildingFactDistances, buildingFactFunction, buildingFact)

IMPLICIT NONE

! Get building effect factor for a specified distance from source, given a building effect function (function of distance).
! Note the cut-off value of 50 m from the source.

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER    (ROUTINENAAM = 'ops_building_get_factor')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER, INTENT(IN)         :: buildingType                                    ! = 0 -> no building effect (factor = 1)
REAL*4, INTENT(IN)          :: angle_SR_xaxis                                  ! angle between source-receptor vector and x-axis (needed for building effect) [degrees]
REAL*4, INTENT(IN)          :: dist                                            ! distance between source and receptor
REAL*4, INTENT(IN)          :: buildingFactDistances(:)                        ! distances for which building effect function has been computed
REAL*4, INTENT(IN)          :: buildingFactAngleSRxaxis(:)                     ! source receptor angles (w.r.t. x-axis) for which building effect function has been computed
REAL*4, INTENT(IN)          :: buildingFactFunction(:,:)                       ! 2D building effect function (function of angle, distance)

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4, INTENT(OUT) :: buildingFact                                            ! building effect factor interpolated between (angle, distance) values in buildingFactFunction

! LOCAL VARIABLES
REAL    :: distcor                                                             ! corrected distance for distances below cut-off distance; for these distances take the effect at the cut-off distance

!-------------------------------------------------------------------------------------------------------------------------------
IF (buildingType .eq. 0) THEN
   buildingFact = 1.0
ELSE

   ! Use first distance in table as cut-off distance; below this value the building factor is constant:
   distcor = max(dist,buildingFactDistances(1))

   ! If source receptor distance larger than largest distance in table -> no building effect; else interpolate building factor from 2d table:

   if (distcor > buildingFactDistances(size(buildingFactDistances))) then
      buildingFact = 1.0
   else
      buildingFact = interpol_2d(buildingFactAngleSRxaxis,buildingFactDistances,buildingFactFunction,size(buildingFactAngleSRxaxis),size(buildingFactDistances),angle_SR_xaxis,distcor)
   endif
   ! write(*,*) 'interpolation: ',angle_SR_xaxis, dist,distcor,buildingFact
END IF

END SUBROUTINE ops_building_get_factor

!---------------------------------------------------------
real function interpol_2d(tabx,taby,f,nx,ny,x,y)

! 2D (bilinear) interpolation

implicit none

integer, intent(in) :: nx            ! length of array tabx
integer, intent(in) :: ny            ! length of array taby
real   , intent(in) :: tabx(nx)      ! array of table entries, x dimension
real   , intent(in) :: taby(ny)      ! array of table entries, y dimension
real   , intent(in) :: f(nx,ny)      ! function values
real   , intent(in) :: x             ! x value where to interpolate
real   , intent(in) :: y             ! y value where to interpolate

integer ::  i,ix,iy                  ! array indices
real    ::  x_intp,y_intp            ! 1D interpolation factors

! Check if outside tabel boundaries (normally this should not occur, because values have been shifted
! inside table boundaries before call -> normal error handling not needed):
if (x < tabx(1) .or. x > tabx(nx)) then
   write(*,*) ' '
   write(*,*) ' error: x index outside table'
   write(*,*) ' boundaries: ',tabx(1), tabx(nx)
   write(*,*) ' value     : ',x
   stop
endif
if (y < taby(1) .or. y > taby(ny)) then
   write(*,*) ' '
   write(*,*) ' error: y index outside table'
   write(*,*) ' boundaries: ',taby(1), taby(ny)
   write(*,*) ' value     : ',y
   stop
endif

! Find index ix, such that tabx(ix) < x <= tabx(ix+1)
! Note: first interval includes left boundary: tabx(1) <= x <= tabx(2)
do i = 1,nx-1
   if (x <= tabx(i+1)) then
      ix = i
      exit
   endif
enddo

! Find index iy, such that taby(iy) < y <= taby(iy+1)
! Note: first interval includes left boundary: taby(1) <= y <= taby(2)
do i = 1,ny-1
   if (y <= taby(i+1)) then
      iy = i
      exit
   endif
enddo

! Interpolation factors in x- and y-direction:
x_intp = (x-tabx(ix))/(tabx(ix+1)-tabx(ix))
y_intp = (y-taby(iy))/(taby(iy+1)-taby(iy))

! Interpolate between four corner values:
interpol_2d = (1-x_intp)*(1-y_intp)*f(ix,iy) + x_intp*(1-y_intp)*f(ix+1,iy) + x_intp*y_intp*f(ix+1,iy+1) + (1-x_intp)*y_intp*f(ix,iy+1)

end function interpol_2d

!-------------------------------------------------------------------------------------------
subroutine split1( mClass, line, pName , pVals, n )

    implicit none

    ! Input:
    integer,      intent(IN) :: mClass      ! maximal number of classes for any parameter
    character(*), intent(in) :: line        ! line read from file with parameter names and parameter values

    ! Output:
    character(100), intent(out) :: pName     ! parameter name
    real,           intent(out) :: pVals(*)  ! parameter values
    integer,        intent(out) :: n         ! number of parameter values read

    ! Local
    character*100              :: cbuf( mClass )
    integer :: m

    ! Read word for word from line:
    n = 1
    do
        read( line, *, end=100) cbuf( 1 : n)   !  !! (See Appendix for why buf is used here)
        read(cbuf(1),*) pName
        do m = 2,n
          read(cbuf(m),*) pVals(m-1)
          !print *, pVals(m)
        enddo
        n = n + 1
    enddo
100 continue
    n = n - 1 ! length of cbuf
    n = n - 1 ! number of reals behind first column with parameter name
end subroutine split1

!-------------------------------------------------------------------------------------------
subroutine split2( line, nParam, iClassRead, buildingFactInput)

    implicit none

    ! Input:
    character(*), intent(in) :: line                ! line read from file with class indices for each parameter and corresponding building effect factor
    integer, intent(in)      :: nParam              ! number of parameters

    ! Output:
    integer, intent(out)     :: iClassRead(nParam)      ! class indices for each parameter
    real,    intent(out)     :: buildingFactInput   ! buiding effect factor, read from input

    ! Local variables:
    character*8              :: cbuf( nParam+1 )
    integer                  :: iParam

    ! Read data from line:
    read( line, *) (iClassRead(iParam), iParam = 1,nParam), buildingFactInput

 end subroutine split2

 end module m_ops_building
