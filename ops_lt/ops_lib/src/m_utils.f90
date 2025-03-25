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
!-------------------------------------------------------------------------------------------------------------------------------
! DESCRIPTION          : General utilities
! IMPLEMENTS           : - Alloc: allocation of memory
!                        - DeAlloc: deallocation of memory
!                        - AllocError: a less important procedure.
!                        - Jaartal: converts year to four digit year.
!                        - GetNumber: retrieves number from a string.
!                        - GetOS: retrieves operating system.
!                        - GetCLArg: Retrieves command line arguments.
!                        - WisselBytes: Byte swapping.
!                        - StartFormat: Initial assignment to a format string.
!                        - AppendFormat: Appends a format string.
!                        - PrependFormat: Put format at start of format string.
!                        - GetIndex: Determines index for a numerically coded identity.
!                        - SortMatrix: Sorts rows of a matrix
!-------------------------------------------------------------------------------------------------------------------------------
MODULE m_utils

USE m_error
USE m_string
#ifndef __GFORTRAN__
USE IFPORT
#endif

IMPLICIT NONE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : Alloc
! DESCRIPTION : Allocation of memory and initialisation at a specific value.
! INPUTS      : dimension  (integer) The dimension of the array to be allocated. Dimensions are overloaded; it is possible to
!                          specify more than one dimension (currently up to five for real, double and integer arrays).
!               inival     (generic, could be real, integer) Initial value for all elements in the array. Optional for integers
!                          and reals, default is actually 0.
!                          Not for strings. Or multiple dimension real arrays.
! OUTPUTS     : array      (generic, array of integer or real type, which could have one or more dimensions). The array that is
!                          allocated / initialised.
!               error      (type TError) Whether error (no memory) occurred during allocation.
! REMARK      : Before allocating the dimensions are checked. If any dimension is equal to 0, allocate anyway to prevent runtime
!               error with the new ifort version: "forrtl: severe (408): fort: (8): 
!               Attempt to fetch from allocatable variable X when it is not allocated".
! REMARK      : The string allocation (allocstring) has problems under UNIX because the UNIX compiler will not accept
!               character*(*).
!               Creation of generic functions with different string lengths is also not possible. Therefore this string
!               allocation is currently not used, but left here until UNIX does accept the character*(*). For this reason the
!               AllocError is also made public.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE Alloc
  MODULE PROCEDURE allocreal0                                                  ! allocation of real array (def=0.0)
  MODULE PROCEDURE allocreal                                                   ! allocation of real array (pointer)
  MODULE PROCEDURE allocreala                                                  ! allocation of real array (allocatable)
  MODULE PROCEDURE allocreal2                                                  ! allocation of 2-dimensional real array (pointer)
  MODULE PROCEDURE allocreal2a                                                 ! allocation of 2-dimensional real array (allocatable)
  MODULE PROCEDURE allocreal3                                                  ! allocation of 3-dimensional real array (pointer)
  MODULE PROCEDURE allocreal3a                                                 ! allocation of 3-dimensional real array (allocatable)
  MODULE PROCEDURE allocreal4                                                  ! allocation of 4-dimensional real array (pointer)
  MODULE PROCEDURE allocreal4a                                                 ! allocation of 4-dimensional real array (allocatable)
  MODULE PROCEDURE allocreal5                                                  ! allocation of 5-dimensional real array (pointer)
  MODULE PROCEDURE allocreal5a                                                 ! allocation of 5-dimensional real array (allocatable)
  MODULE PROCEDURE allocdouble0                                                ! allocation of double array (def = 0.0)
  MODULE PROCEDURE allocdouble                                                 ! allocation of double array
  MODULE PROCEDURE allocdouble2                                                ! allocation of 2-dimensional double array
  MODULE PROCEDURE allocdouble3                                                ! allocation of 3-dimensional double array
  MODULE PROCEDURE allocdouble4                                                ! allocation of 4-dimensional double array
  MODULE PROCEDURE allocdouble5                                                ! allocation of 5-dimensional double array
  MODULE PROCEDURE allocinteger0                                               ! allocation of integer array
  MODULE PROCEDURE allocinteger                                                ! allocation of integer array
  MODULE PROCEDURE allocinteger2                                               ! allocation of 2-dimensional integer array
  MODULE PROCEDURE allocinteger3                                               ! allocation of 3-dimensional integer array
  MODULE PROCEDURE allocinteger4                                               ! allocation of 4-dimensional integer array
  MODULE PROCEDURE allocinteger5                                               ! allocation of 5-dimensional integer array
  MODULE PROCEDURE allocstring                                                 ! allocation of string array
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : AllocError
! PURPOSE     : Writing errors occurred during allocation.
! REMARK      : Is actually a private procedure, but because the string allocation does not work under UNIX it is useful to make
!               this function public, for the time being.
! INPUT/OUTPUT: See procedure implementation. Not a main item as this procedure should be considered as being private.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE AllocError
  MODULE PROCEDURE AllocError
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : Dealloc
! PURPOSE     : De-allocation of memory.
! DESCRIPTION : Memory is de-allocated only if it was allocated before.
! INPUT/OUTPUT: array      (generic, array of integer or real type). The array that is allocated / initialised.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE Dealloc
  MODULE PROCEDURE deallocreal                                                 ! deallocation of real array
  MODULE PROCEDURE deallocreal2                                                ! deallocation of 2-dimensional real array
  MODULE PROCEDURE deallocreal3                                                ! deallocation of 3-dimensional real array
  MODULE PROCEDURE deallocdouble                                               ! deallocation of double array
  MODULE PROCEDURE deallocdouble2                                              ! deallocation of 2-dimensional double array
  MODULE PROCEDURE deallocinteger                                              ! deallocation of integer array
  MODULE PROCEDURE deallocstring                                               ! deallocation of string array
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION    : Jaartal
! DESCRIPTION : Simple conversion of input number (which could be below 100 or above 1900) to a year above 1900.
! INPUT       : number     (integer) The input number to be converted.
! RESULT      : The year.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE Jaartal
  MODULE PROCEDURE Jaartal
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : GetNumber
! DESCRIPTION : Retrieves a number (integer or real) from a string.
!               Passes an error if the string is not that correct type of number. Overloaded for different types of values
!               returned (i.e. integer or real). If no number was located (empty string), a 0 is returned.
! INPUTS      : string     (character*(*)) The string which should represent the number to be extracted.
! OUTPUTS     : number     (generic, real or integer). The value read or 0 if not present
!               isdefault  (logical) Returns .TRUE. if no value was read because no value was defined 
!               error      (type TError). Is assigned when an error occurred in the number defined in the string.
! After call: 
! if (error) then
!    -> stop program
! else
!    if (isdefault) then
!       -> default value 0 is returned; check if this is ok or not !
!    else
!       -> continue with value read 
!    endif
! endif
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE GetNumber
  MODULE PROCEDURE getreal                                                     ! retrieves real value
  MODULE PROCEDURE getint                                                      ! retrieves integer value
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : GetOS
! DESCRIPTION : Retrieves operating system.
! OUTPUTS     : os         (integer) 1: WinNT, 0: UNIX
!               slash      (character), optional. character used as directory separator. / = UNIX, \ = WinNT.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE GetOS
  MODULE PROCEDURE GetOS
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : GetCLArg
! DESCRIPTION : Retrieves command line arguments, irrespective of operating system.
! OUTPUTS     : progpath   (character*512) Full path of the program.
!               nrarg      (integer) Number of arguments except for progpath.
!               arg        (character*(512), dimension nrarg) dynamically allocated array which contains all the arguments. The
!                          array should be de-allocated by then caller.
!               error      (type TError) Assigned when error is encountered e.g. in memory allocation.
! REMARK      : Implementation inquires the operating system (GetOS), so the user does not have to worry about that.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE GetCLArg
  MODULE PROCEDURE GetCLArg
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION    : WisselBytes
! DESCRIPTION : Converts integer*2 internal notation from HP fortran to Microsoft Fortran and visa versa
! AUTHOR      : HvJ/Franka Loeve (Cap Volmac)
! INPUTS      : string     (character*(*)) The string which should represent the number to be extracted.
! OUTPUTS     : default    (logical) Returns .TRUE. if no value was read because no value was defined following the = sign.
!               error      (type TError). Is assigned when an error occurred in the number defined in the string.
! RESULT      : Generic. The integer or real value that is assigned in the call of the function.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE WisselBytes
  MODULE PROCEDURE byteswap1
  MODULE PROCEDURE byteswap2
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : StartFormat
! PURPOSE     : Initial assignment to a format string.
! DESCRIPTION : To be called when starting the creation of a format string. The format string is then extended through
!               AppendFormat and PrependFormat.
! AUTHOR      : Martien de Haan (ARIS).
! OUTPUTS     : formatstring (character*(*)) The formnat string to be created.
!               error      (type TError). Is assigned when an error occurred in the assignment FormatString.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE StartFormat
  MODULE PROCEDURE StartFormat
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : AppendFormat
! PURPOSE     : Appends a format string.
! DESCRIPTION : With appendformat it is possible to append a format descriptor to a format string. The string should have been
!               initialised with StartFormat. After each call to
!               AppendFormat the result is always a correct format string, which can be used in reading or writing of data.
! REMARK      : AppendFormat checks first whether an error has occurred. If so nothing happens. This is handy, because the
!               calling procedure only has to check the error status once after all append and prepend procedures have been
!               called.
! AUTHOR      : Martien de Haan (ARIS).
! INPUTS      : nrelts     (integer,   optional) Assigns how many descriptor fields are present (that is number of integers,
!                          floats or whatever in the format string).
!               descriptor (character*(*)) The descriptor appended, such as 'I6', or 'F7.3' or 'X, I3'. This descriptor is
!                          appended for nrelts times.
! INPUT/OUTPUT: formatstring (character*(*)) The formnat string to be appended.
! OUTPUTS     : error      (type TError). Is assigned when an error occurred in the appending of the format string.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE AppendFormat
  MODULE PROCEDURE append_format_string1                                       ! without nrelts
  MODULE PROCEDURE append_format_string                                        ! with nrelts as parameter
END INTERFACE


!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : GetIndex
! DESCRIPTION : Determines index for a numerically coded identity.
! INPUT       : ... (integer) numerical code
! OUTPUT      : index (integer) index
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE GetIndex
  MODULE PROCEDURE GetIndex
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : SortMatrix
! DESCRIPTION : sorts rows of a matrix
! INPUTS      : nobs (integer) number of rows column (integer) column to be used as key
! INPUT/OUTPUT: matrix (real) matrix to be sorted
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE SortMatrix
  MODULE PROCEDURE SortMatrix
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! Private functions
!-------------------------------------------------------------------------------------------------------------------------------
! PRIVATE allocerror          ! for making error message in alloc procedures
PRIVATE extractint                                                             ! for GetNumber
PRIVATE byteswap                                                               ! for WisselBytes

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: get_version_utils
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE get_version_utils(dll_version, dll_date)

! !DEC$ ATTRIBUTES DLLEXPORT:: get_version_utils

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'get_version_utils')

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*(*), INTENT(OUT)                       :: dll_version                ! 
CHARACTER*(*), INTENT(OUT)                       :: dll_date                   ! 

!-------------------------------------------------------------------------------------------------------------------------------
!
dll_version="1.0.0"
dll_date="28 jun 2012"

END SUBROUTINE get_version_utils

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocreal
! INTERFACE            : Alloc
! PURPOSE              : Allocation of real array. The array is initialised at 0.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocreal0(dim, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocreal0

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim                        ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT), DIMENSION(:), POINTER    :: arr                        ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocreal0')

!-------------------------------------------------------------------------------------------------------------------------------
CALL Alloc(dim, 0., arr, error)

END SUBROUTINE allocreal0

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocreal
! INTERFACE            : Alloc
! PURPOSE              : Allocation and initialisation of real array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocreal(dim, defvalue, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocreal

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim                        ! 
REAL,      INTENT(IN)                            :: defvalue                   ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT), DIMENSION(:), POINTER    :: arr                        ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocreal')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim) = defvalue
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim, 'real', error)
  ENDIF
ENDIF

END SUBROUTINE allocreal
!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocreala
! INTERFACE            : Alloc
! PURPOSE              : Allocation of 1-dimensional real allocatable array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocreala(dim, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocreal2a

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                             :: dim                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT), DIMENSION(:), ALLOCATABLE :: arr                        ! 
TYPE (TError), INTENT(INOUT)                      :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                           :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                     :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocreala')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim) = 0.0
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim, '1-dimensional real allocatable', error)
  ENDIF
ENDIF

END SUBROUTINE allocreala

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocdouble0
! INTERFACE            : Alloc
! PURPOSE              : Allocation of double array. The array is initialised at 0.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocdouble0(dim, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocdouble0

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim                        ! 

! SUBROUTINE ARGUMENTS - OUTPUT
DOUBLE PRECISION,    INTENT(OUT), DIMENSION(:), POINTER    :: arr                        ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocdouble0')

!-------------------------------------------------------------------------------------------------------------------------------
CALL Alloc(dim, 0., arr, error)

END SUBROUTINE allocdouble0

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocdouble
! INTERFACE            : Alloc
! PURPOSE              : Allocation and initialisation of double precision array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocdouble(dim, defvalue, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocdouble

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim                        ! 
REAL,      INTENT(IN)                            :: defvalue                   ! 

! SUBROUTINE ARGUMENTS - OUTPUT
DOUBLE PRECISION,    INTENT(OUT), DIMENSION(:), POINTER    :: arr                        ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocdouble')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim) = defvalue
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim, 'real', error)
  ENDIF
ENDIF

END SUBROUTINE allocdouble

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocreal2
! INTERFACE            : Alloc
! PURPOSE              : Allocation of 2-dimensional real array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocreal2(dim1, dim2, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocreal2

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim1                       ! 
INTEGER,   INTENT(IN)                            :: dim2                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT), DIMENSION(:,:), POINTER  :: arr                        ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocreal2')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim1, dim2), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim1, :dim2) = 0.0
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim1, '2-dimensional real', error)
    CALL ErrorParam('second dimension', dim2, error)
  ENDIF
ENDIF

END SUBROUTINE allocreal2
!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocreal2
! INTERFACE            : Alloc
! PURPOSE              : Allocation of 2-dimensional real array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocreal2a(dim1, dim2, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocreal2a

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim1                       ! 
INTEGER,   INTENT(IN)                            :: dim2                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: arr                        ! 
TYPE (TError), INTENT(INOUT)                        :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocreal2')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim1, dim2), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim1, :dim2) = 0.0
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim1, '2-dimensional real', error)
    CALL ErrorParam('second dimension', dim2, error)
  ENDIF
ENDIF

END SUBROUTINE allocreal2a

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocdouble2
! INTERFACE            : Alloc
! PURPOSE              : Allocation of 2-dimensional double array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocdouble2(dim1, dim2, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocdouble2

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim1                       ! 
INTEGER,   INTENT(IN)                            :: dim2                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
DOUBLE PRECISION,    INTENT(OUT), DIMENSION(:,:), POINTER  :: arr                        ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocdouble2')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim1, dim2), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim1, :dim2) = 0.0
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim1, '2-dimensional double', error)
    CALL ErrorParam('second dimension', dim2, error)
  ENDIF
ENDIF

END SUBROUTINE allocdouble2

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocreal3
! INTERFACE            : Alloc
! PURPOSE              : Allocation of 3-dimensional real array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocreal3(dim1, dim2, dim3, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocreal3

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim1                       ! 
INTEGER,   INTENT(IN)                            :: dim2                       ! 
INTEGER,   INTENT(IN)                            :: dim3                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT), DIMENSION(:,:,:), POINTER :: arr                       ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocreal3')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim1, dim2, dim3), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim1, :dim2, :dim3) = 0.0
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim1, '3-dimensional real', error)
    CALL ErrorParam('second dimension', dim2, error)
    CALL ErrorParam('third dimension', dim3, error)
  ENDIF
ENDIF

END SUBROUTINE allocreal3

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocreal3a
! INTERFACE            : Alloc
! PURPOSE              : Allocation of 3-dimensional real array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocreal3a(dim1, dim2, dim3, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocreal3a

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim1                       ! 
INTEGER,   INTENT(IN)                            :: dim2                       ! 
INTEGER,   INTENT(IN)                            :: dim3                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,    INTENT(OUT), DIMENSION(:,:,:), ALLOCATABLE :: arr                        ! 
TYPE (TError), INTENT(INOUT)                        :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocreal3a')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim1, dim2, dim3), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim1, :dim2, :dim3) = 0.0
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim1, '3-dimensional real', error)
    CALL ErrorParam('second dimension', dim2, error)
    CALL ErrorParam('third dimension', dim3, error)
  ENDIF
ENDIF

END SUBROUTINE allocreal3a

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocdouble3
! INTERFACE            : Alloc
! PURPOSE              : Allocation of 3-dimensional double array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocdouble3(dim1, dim2, dim3, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocdouble3

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim1                       ! 
INTEGER,   INTENT(IN)                            :: dim2                       ! 
INTEGER,   INTENT(IN)                            :: dim3                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:,:), POINTER  :: arr                        ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocdouble3')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim1, dim2, dim3), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim1, :dim2, :dim3) = 0.0
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim1, '3-dimensional double', error)
    CALL ErrorParam('second dimension', dim2, error)
    CALL ErrorParam('third dimension', dim3, error)
  ENDIF
ENDIF

END SUBROUTINE allocdouble3

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocreal4
! INTERFACE            : Alloc
! PURPOSE              : Allocation of 4-dimensional real array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocreal4(dim1, dim2, dim3, dim4, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocreal4

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim1                       ! 
INTEGER,   INTENT(IN)                            :: dim2                       ! 
INTEGER,   INTENT(IN)                            :: dim3                       ! 
INTEGER,   INTENT(IN)                            :: dim4                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,   INTENT(OUT), DIMENSION(:,:,:,:), POINTER :: arr                       ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocreal4')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim1, dim2, dim3, dim4), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim1, :dim2, :dim3, :dim4) = 0.0
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim1, '4-dimensional real', error)
    CALL ErrorParam('second dimension', dim2, error)
    CALL ErrorParam('third dimension', dim3, error)
    CALL ErrorParam('fourth dimension', dim4, error)
  ENDIF
ENDIF

END SUBROUTINE allocreal4

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocreal4a
! INTERFACE            : Alloc
! PURPOSE              : Allocation of 4-dimensional real array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocreal4a(dim1, dim2, dim3, dim4, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocreal4a

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim1                       ! 
INTEGER,   INTENT(IN)                            :: dim2                       ! 
INTEGER,   INTENT(IN)                            :: dim3                       ! 
INTEGER,   INTENT(IN)                            :: dim4                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,   INTENT(OUT), DIMENSION(:,:,:,:), ALLOCATABLE :: arr                        ! 
TYPE (TError), INTENT(INOUT)                        :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocreal4a')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim1, dim2, dim3, dim4), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim1, :dim2, :dim3, :dim4) = 0.0
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim1, '4-dimensional real', error)
    CALL ErrorParam('second dimension', dim2, error)
    CALL ErrorParam('third dimension', dim3, error)
    CALL ErrorParam('fourth dimension', dim4, error)
  ENDIF
ENDIF

END SUBROUTINE allocreal4a

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocdouble4
! INTERFACE            : Alloc
! PURPOSE              : Allocation of 4-dimensional double array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocdouble4(dim1, dim2, dim3, dim4, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocdouble4

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim1                       ! 
INTEGER,   INTENT(IN)                            :: dim2                       ! 
INTEGER,   INTENT(IN)                            :: dim3                       ! 
INTEGER,   INTENT(IN)                            :: dim4                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:,:,:), POINTER  :: arr                        ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocdouble4')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim1, dim2, dim3, dim4), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim1, :dim2, :dim3, :dim4) = 0.0
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim1, '4-dimensional double', error)
    CALL ErrorParam('second dimension', dim2, error)
    CALL ErrorParam('third dimension', dim3, error)
    CALL ErrorParam('fourth dimension', dim4, error)
  ENDIF
ENDIF

END SUBROUTINE allocdouble4

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocreal5
! INTERFACE            : Alloc
! PURPOSE              : Allocation of 5-dimensional real array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocreal5(dim1, dim2, dim3, dim4, dim5, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocreal5

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim1                       ! 
INTEGER,   INTENT(IN)                            :: dim2                       ! 
INTEGER,   INTENT(IN)                            :: dim3                       ! 
INTEGER,   INTENT(IN)                            :: dim4                       ! 
INTEGER,   INTENT(IN)                            :: dim5                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,   INTENT(OUT), DIMENSION(:,:,:,:,:), POINTER :: arr                       ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocreal5')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim1, dim2, dim3, dim4, dim5), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim1, :dim2, :dim3, :dim4, :dim5) = 0.0
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim1, '5-dimensional real', error)
    CALL ErrorParam('second dimension', dim2, error)
    CALL ErrorParam('third dimension', dim3, error)
    CALL ErrorParam('fourth dimension', dim4, error)
    CALL ErrorParam('fifth dimension', dim5, error)
  ENDIF
ENDIF

END SUBROUTINE allocreal5

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocreal5a
! INTERFACE            : Alloc
! PURPOSE              : Allocation of 5-dimensional real array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocreal5a(dim1, dim2, dim3, dim4, dim5, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocreal5a

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim1                       ! 
INTEGER,   INTENT(IN)                            :: dim2                       ! 
INTEGER,   INTENT(IN)                            :: dim3                       ! 
INTEGER,   INTENT(IN)                            :: dim4                       ! 
INTEGER,   INTENT(IN)                            :: dim5                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,   INTENT(OUT), DIMENSION(:,:,:,:,:), ALLOCATABLE :: arr                        ! 
TYPE (TError), INTENT(INOUT)                        :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocreal5a')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim1, dim2, dim3, dim4, dim5), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim1, :dim2, :dim3, :dim4, :dim5) = 0.0
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim1, '5-dimensional real', error)
    CALL ErrorParam('second dimension', dim2, error)
    CALL ErrorParam('third dimension', dim3, error)
    CALL ErrorParam('fourth dimension', dim4, error)
    CALL ErrorParam('fifth dimension', dim5, error)
  ENDIF
ENDIF

END SUBROUTINE allocreal5a

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocdouble5
! INTERFACE            : Alloc
! PURPOSE              : Allocation of 5-dimensional double array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocdouble5(dim1, dim2, dim3, dim4, dim5, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocdouble5

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim1                       ! 
INTEGER,   INTENT(IN)                            :: dim2                       ! 
INTEGER,   INTENT(IN)                            :: dim3                       ! 
INTEGER,   INTENT(IN)                            :: dim4                       ! 
INTEGER,   INTENT(IN)                            :: dim5                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:,:,:,:), POINTER  :: arr                        ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocdouble5')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim1, dim2, dim3, dim4, dim5), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim1, :dim2, :dim3, :dim4, :dim5) = 0.0
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim1, '5-dimensional double', error)
    CALL ErrorParam('second dimension', dim2, error)
    CALL ErrorParam('third dimension', dim3, error)
    CALL ErrorParam('fourth dimension', dim4, error)
    CALL ErrorParam('fifth dimension', dim5, error)
  ENDIF
ENDIF

END SUBROUTINE allocdouble5

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocinteger0
! INTERFACE            : Alloc
! PURPOSE              : Allocation of integer array. The array is initialised at 0.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocinteger0(dim, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocinteger0

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim                        ! 

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER,   INTENT(OUT), DIMENSION(:), POINTER    :: arr                        ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocinteger0')

!-------------------------------------------------------------------------------------------------------------------------------
CALL alloc(dim, 0, arr, error)

END SUBROUTINE allocinteger0

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocinteger
! INTERFACE            : Alloc
! PURPOSE              : Allocation and initialisation of real array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocinteger(dim, defvalue, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocinteger

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim                        ! 
INTEGER,   INTENT(IN)                            :: defvalue                   ! 

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER,   INTENT(OUT), DIMENSION(:), POINTER    :: arr                        ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocinteger')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim) = defvalue
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim, 'integer', error)
  ENDIF
ENDIF

END SUBROUTINE allocinteger

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocinteger2
! INTERFACE            : Alloc
! PURPOSE              : Allocation of 2-dimensional integer array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocinteger2(dim1, dim2, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocinteger2

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim1                       ! 
INTEGER,   INTENT(IN)                            :: dim2                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER,    INTENT(OUT), DIMENSION(:,:), POINTER  :: arr                        ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocinteger2')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim1, dim2), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim1, :dim2) = 0
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim1, '2-dimensional integer', error)
    CALL ErrorParam('second dimension', dim2, error)
  ENDIF
ENDIF

END SUBROUTINE allocinteger2

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocinteger3
! INTERFACE            : Alloc
! PURPOSE              : Allocation of 3-dimensional integer array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocinteger3(dim1, dim2, dim3, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocinteger3

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim1                       ! 
INTEGER,   INTENT(IN)                            :: dim2                       ! 
INTEGER,   INTENT(IN)                            :: dim3                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER, INTENT(OUT), DIMENSION(:,:,:), POINTER  :: arr                        ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocinteger3')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim1, dim2, dim3), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim1, :dim2, :dim3) = 0
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim1, '3-dimensional integer', error)
    CALL ErrorParam('second dimension', dim2, error)
    CALL ErrorParam('third dimension', dim3, error)
  ENDIF
ENDIF

END SUBROUTINE allocinteger3

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocinteger4
! INTERFACE            : Alloc
! PURPOSE              : Allocation of 4-dimensional integer array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocinteger4(dim1, dim2, dim3, dim4, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocinteger4

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim1                       ! 
INTEGER,   INTENT(IN)                            :: dim2                       ! 
INTEGER,   INTENT(IN)                            :: dim3                       ! 
INTEGER,   INTENT(IN)                            :: dim4                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER, INTENT(OUT), DIMENSION(:,:,:,:), POINTER  :: arr                        ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocinteger4')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim1, dim2, dim3, dim4), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim1, :dim2, :dim3, :dim4) = 0
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim1, '4-dimensional integer', error)
    CALL ErrorParam('second dimension', dim2, error)
    CALL ErrorParam('third dimension', dim3, error)
    CALL ErrorParam('fourth dimension', dim4, error)
  ENDIF
ENDIF

END SUBROUTINE allocinteger4

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocinteger5
! INTERFACE            : Alloc
! PURPOSE              : Allocation of 5-dimensional integer array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocinteger5(dim1, dim2, dim3, dim4, dim5, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocinteger5

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim1                       ! 
INTEGER,   INTENT(IN)                            :: dim2                       ! 
INTEGER,   INTENT(IN)                            :: dim3                       ! 
INTEGER,   INTENT(IN)                            :: dim4                       ! 
INTEGER,   INTENT(IN)                            :: dim5                       ! 

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER, INTENT(OUT), DIMENSION(:,:,:,:,:), POINTER  :: arr                        ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocinteger5')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim1, dim2, dim3, dim4, dim5), stat=ierr)

  IF (ierr == 0) THEN
    arr(:dim1, :dim2, :dim3, :dim4, :dim5) = 0
  ELSE
    CALL AllocError(ierr, ROUTINENAAM, dim1, '5-dimensional integer', error)
    CALL ErrorParam('second dimension', dim2, error)
    CALL ErrorParam('third dimension', dim3, error)
    CALL ErrorParam('fourth dimension', dim4, error)
    CALL ErrorParam('fifth dimension', dim5, error)
  ENDIF
ENDIF

END SUBROUTINE allocinteger5

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : allocstring
! INTERFACE            : Alloc
! PURPOSE              : Allocation of string array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE allocstring(dim, arr, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: allocstring

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim                        ! 

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*(*), INTENT(OUT), DIMENSION(:), POINTER :: arr                       ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'allocstring')

!-------------------------------------------------------------------------------------------------------------------------------
IF (.NOT. error%haserror) THEN
  ALLOCATE(arr(dim), stat=ierr)
  CALL AllocError(ierr, ROUTINENAAM, dim, 'string', error)
ENDIF

END SUBROUTINE allocstring

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : AllocError (private)
! PURPOSE              : Writes allocation error message.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE AllocError(ierr, routinenaam, dim, arraytype, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: AllocError

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: ierr                       ! error index
CHARACTER*(*), INTENT(IN)                        :: routinenaam                ! name of allocation subroutine where error occurred
INTEGER,   INTENT(IN)                            :: dim                        ! dimension of array to be allocated
CHARACTER*(*), INTENT(IN)                        :: arraytype                  ! type of elements in array to be allocated

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record
!
! LOCAL VARIABLES
CHARACTER*512                                    :: message                    ! the error message

!-------------------------------------------------------------------------------------------------------------------------------

IF (ierr /= 0) THEN
!
! Build message using simple copy commands (if these fail at least something looking like this error message should be written.
!
  CALL simplecopy('No memory for allocation of ', message)
  CALL simpleappend(arraytype(1:LEN_TRIM(arraytype)), message)
  CALL simpleappend(' array', message)
!
! Set the error message parameters and call stack.
!
  CALL SetError(message, error)
  CALL ErrorParam('error index', ierr, error)
  CALL ErrorParam('array dimension', dim, error)
  CALL ErrorCall(routinenaam, error)
ENDIF

END SUBROUTINE AllocError

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : deallocreal
! INTERFACE            : Dealloc
! PURPOSE              : Deallocation of real array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE deallocreal(arr)

! !DEC$ ATTRIBUTES DLLEXPORT:: deallocreal

! SUBROUTINE ARGUMENTS - I/O
REAL,      INTENT(INOUT), DIMENSION(:), POINTER  :: arr                        ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'deallocreal')

!-------------------------------------------------------------------------------------------------------------------------------
IF (ASSOCIATED(arr)) DEALLOCATE(arr)
END SUBROUTINE deallocreal

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : deallocdouble
! INTERFACE            : Dealloc
! PURPOSE              : Deallocation of double precision array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE deallocdouble(arr)

! !DEC$ ATTRIBUTES DLLEXPORT:: deallocdouble
! SUBROUTINE ARGUMENTS - I/O
DOUBLE PRECISION,    INTENT(INOUT), DIMENSION(:), POINTER  :: arr              ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'deallocdouble')

!-------------------------------------------------------------------------------------------------------------------------------
IF (ASSOCIATED(arr)) DEALLOCATE(arr)
END SUBROUTINE deallocdouble

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : deallocreal2
! INTERFACE            : Dealloc
! PURPOSE              : Deallocation of 2-dimensional real array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE deallocreal2(arr)

! !DEC$ ATTRIBUTES DLLEXPORT:: deallocreal2

! SUBROUTINE ARGUMENTS - I/O
REAL,      INTENT(INOUT), DIMENSION(:,:), POINTER :: arr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'deallocreal2')

!-------------------------------------------------------------------------------------------------------------------------------
IF (ASSOCIATED(arr)) DEALLOCATE(arr)
END SUBROUTINE deallocreal2

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : deallocdouble2
! INTERFACE            : Dealloc
! PURPOSE              : Deallocation of 2-dimensional double array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE deallocdouble2(arr)

! !DEC$ ATTRIBUTES DLLEXPORT:: deallocdouble2

! SUBROUTINE ARGUMENTS - I/O
DOUBLE PRECISION,    INTENT(INOUT), DIMENSION(:,:), POINTER :: arr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'deallocdouble2')

!-------------------------------------------------------------------------------------------------------------------------------
IF (ASSOCIATED(arr)) DEALLOCATE(arr)
END SUBROUTINE deallocdouble2

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : deallocreal3
! INTERFACE            : Dealloc
! PURPOSE              : Deallocation of 3-dimensional real array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE deallocreal3(arr)

! !DEC$ ATTRIBUTES DLLEXPORT:: deallocreal3

! SUBROUTINE ARGUMENTS - I/O
REAL,      INTENT(INOUT), DIMENSION(:,:,:), POINTER :: arr                     ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'deallocreal3')

!-------------------------------------------------------------------------------------------------------------------------------
IF (ASSOCIATED(arr)) DEALLOCATE(arr)
END SUBROUTINE deallocreal3

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : deallocinteger
! INTERFACE            : Dealloc
! PURPOSE              : Deallocation of integer array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE deallocinteger(arr)

! !DEC$ ATTRIBUTES DLLEXPORT:: deallocinteger
! SUBROUTINE ARGUMENTS - I/O
INTEGER,   INTENT(INOUT), DIMENSION(:), POINTER  :: arr                        ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'deallocinteger')

!-------------------------------------------------------------------------------------------------------------------------------
IF (ASSOCIATED(arr)) DEALLOCATE(arr)
END SUBROUTINE deallocinteger

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : deallocstring
! INTERFACE            : Dealloc
! PURPOSE              : Deallocation of string array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE deallocstring(arr)

! !DEC$ ATTRIBUTES DLLEXPORT:: deallocstring

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT), DIMENSION(:), POINTER :: arr                     ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'deallocstring')

!-------------------------------------------------------------------------------------------------------------------------------
IF (ASSOCIATED(arr)) DEALLOCATE(arr)
END SUBROUTINE deallocstring

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION             : Jaartal
! INTERFACE            : Jaartal
! PURPOSE              : Conversion of year to number above 1900. Inputs could be below 100 and that would otherwise give
!                        problems
! CALLED FUNCTIONS     : none
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION Jaartal(number)

! !DEC$ ATTRIBUTES DLLEXPORT:: Jaartal

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: number                     ! 

! FUNCTION RESULT
INTEGER                                          :: Jaartal                    ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'Jaartal')

!-------------------------------------------------------------------------------------------------------------------------------
IF (number < 50) THEN
  Jaartal = number+2000
ELSEIF (number > 1900) THEN
  Jaartal = number
ELSE
  Jaartal = number+1900
ENDIF

END FUNCTION Jaartal

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : getreal
! PURPOSE              : Extracting a real number from a string. If no number was located (empty string), a 0 is returned.
! CALLED FUNCTIONS     : getint, getrealerror
!
! REMARK               : WILL NOT YET GIVE ERROR MESSAGE IF STRING IS LIKE:
!                        2.+3 or something like that.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE getreal  (string, value, nopart, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: getreal

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: string                     ! String with real number, starting at pos. 1.

! SUBROUTINE ARGUMENTS - OUTPUT
REAL,      INTENT(OUT)                           :: value                      ! 
LOGICAL,   INTENT(OUT)                           :: nopart                     ! TRUE als er geen real is gelezen
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: beyondpos                  ! First position beyond integer in string
INTEGER                                          :: morepos                    ! First position beyond integer in substring
INTEGER                                          :: intpart                    ! Extracted integer from string
REAL                                             :: decpart                    ! Extracted decimal part from string
LOGICAL                                          :: nodecpart                  ! TRUE als er geen real is gelezen
LOGICAL                                          :: negative                   ! Of getal negatief is
CHARACTER                                        :: testchar                   ! Character looked at

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'getreal')

!-------------------------------------------------------------------------------------------------------------------------------
!
! Retrieve the intpart from the string. Check whether this part is correct and whether it contains any more relevant characters.
!
nopart = .NOT.extractint(string, intpart, beyondpos, error)
IF (error%haserror) GOTO 999
value = REAL(intpart)

IF (beyondpos.NE.0) THEN
!
! We may have more characters. These should be decimal point or e power.
! The next condition tests for situations where we are finished with the right value.
!
  testchar = string(beyondpos:beyondpos)
  IF ((.NOT.nopart .OR. string(beyondpos:beyondpos).NE.'                       ! ') .AND. testchar.NE.' ') THEN
    negative = string(1:1).EQ.'-'
!
!   Expecting a decimal point. If no decimal point we can still have the e-power provided we already had a number.
!
    IF (nopart .AND. testchar.NE.'.') GOTO 100                                 ! expecting a . here
    nopart = .FALSE.
    IF (testchar.EQ.'.') THEN
!
!     Retrieve the decimal part.
!
      beyondpos = beyondpos + 1
      nodecpart = .NOT. extractint(string(beyondpos:), intpart, morepos, error)
      IF (error%haserror) GOTO 999
      IF (nopart .AND. nodecpart) GOTO 100
      nodecpart = morepos.EQ.0                                                 ! help for end-of-line
      IF (nodecpart) THEN
        morepos = LEN_TRIM(string)+2-beyondpos
      ELSE
!
!       No + or - sign was allowed in decimal part. So check at this point.
!
        testchar = string(beyondpos:beyondpos)
        IF (testchar.EQ.'-' .OR. testchar.EQ.'+') GOTO 100
      ENDIF
      decpart = REAL(intpart)
      DO WHILE (morepos.GT.1)
        beyondpos = beyondpos + 1
        morepos = morepos - 1
        decpart = decpart / 10.
      ENDDO
      IF (negative) THEN
        value = value - decpart
      ELSE
        value = value + decpart
      ENDIF
      IF (nodecpart) beyondpos = 0
    ENDIF
!
! Check for e-power.
!
    IF (beyondpos.NE.0) THEN
      testchar = string(beyondpos:beyondpos)
      IF (testchar.NE.' ') THEN
        IF (testchar.NE.'E' .AND. testchar.NE.'e') GOTO 100
!
!       Retrieve the exponent, called intpart
!
        beyondpos = beyondpos + 1
        IF (.NOT.extractint(string(beyondpos:), intpart, morepos, error)) GOTO 100
        DO WHILE (intpart.GT.0)
          value = value * 10.
          intpart = intpart - 1
        ENDDO
        DO WHILE (intpart.LT.0)
          value = value / 10.
          intpart = intpart + 1
        ENDDO
      ENDIF
    ENDIF
  ENDIF
ENDIF
RETURN

100 CALL SetError('Not a correct real number', error)
CALL ErrorParam('real value', string, .TRUE., error)

999 CALL ErrorCall(ROUTINENAAM, error)
RETURN

END SUBROUTINE getreal

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE           : getint
! AUTHOR               : Martien de Haan, okt 2001
! PURPOSE              : Extraheren van integer waarde uit een string. Geeft terug of er een waarde was, welke positie, etc.
!                        If no number was located (empty string), a 0 is returned.
! CALLED FUNCTIONS     : extractint
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE getint      (string, value, isdefault, error, beyondpos_out)

! !DEC$ ATTRIBUTES DLLEXPORT:: getint 

USE m_error

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: string                     ! String met geheel getal, beginnend op pos. 1.

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER,   INTENT(OUT)                           :: value                      ! 
LOGICAL,   INTENT(OUT)                           :: isdefault                  ! Whether nothing is extracted
TYPE (TError), INTENT(INOUT)                     :: error                      ! 
INTEGER,   INTENT(OUT), optional                 :: beyondpos_out              ! First position beyond integer in string; 0 if end of line

! LOCAL PARAMETERS
INTEGER                                          :: beyondpos                  ! First position beyond integer in string; 0 if end of line
INTEGER                                          :: intvalue                   ! Value extracted

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'getint')

!-------------------------------------------------------------------------------------------------------------------------------
!
! Extract the integer value.
!
isdefault = .NOT. extractint(string, intvalue, beyondpos, error)

IF (isdefault) THEN
!
! No integer extracted. We should have either no text or commentary(!).
!
  IF (beyondpos.NE.0) THEN
    IF (string(beyondpos:beyondpos).NE.'                                       ! ') GOTO 100 
  ENDIF
  value = 0
ELSE
!
! Integer extracted. The next character should be blank or end of line.
!
  IF (beyondpos.NE.0) THEN
    IF (string(beyondpos:beyondpos).NE.' ') GOTO 100
  ENDIF
  value = intvalue
ENDIF

if (present(beyondpos_out)) beyondpos_out = beyondpos

RETURN
!
! Errorlabel 100: something went wrong.
!
100 CALL SetError('Number is not an integer', error)
CALL ErrorParam('number', string, .TRUE., error)
CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE getint

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : byteswap1
! DESCRIPTION : Converts integer*2 internal notation from HP fortran to Microsoft Fortran and visa versa in 1-dimensional array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE byteswap1(ishort, shortdim)

! !DEC$ ATTRIBUTES DLLEXPORT:: byteswap1 

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: shortdim                   ! number of elements in ishort

! SUBROUTINE ARGUMENTS - I/O
INTEGER*2, INTENT(INOUT)                         :: ishort(shortdim)           ! te converteren integers

! LOCAL VARIABLES
INTEGER                                          :: i                          ! tellers

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'byteswap1')

!-------------------------------------------------------------------------------------------------------------------------------
DO i = 1, shortdim
  CALL byteswap(ishort(i))
ENDDO

RETURN
END SUBROUTINE byteswap1

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : byteswap2
! DESCRIPTION : Converts integer*2 internal notation from HP fortran to Microsoft Fortran and visa versa in 2-dimensional array.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE byteswap2(ishort, dim1, dim2)

! !DEC$ ATTRIBUTES DLLEXPORT:: byteswap2

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: dim1                       ! first dimension number of elements in ishort
INTEGER,   INTENT(IN)                            :: dim2                       ! second dimension number of elements in ishort

! SUBROUTINE ARGUMENTS - I/O
INTEGER*2, INTENT(INOUT)                         :: ishort(dim1, dim2)         ! te converteren integers

! LOCAL VARIABLES
INTEGER                                          :: i, j                       ! tellers

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'byteswap2')

!-------------------------------------------------------------------------------------------------------------------------------
DO i = 1, dim1
  DO j = 1, dim2
    CALL byteswap(ishort(i,j))
  ENDDO
ENDDO

RETURN
END SUBROUTINE byteswap2

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : byteswap
! DESCRIPTION : Converts integer*2 internal notation from HP fortran to Microsoft Fortran and visa versa.
! AUTHOR      : HvJ/Franka Loeve (Cap Volmac)
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE byteswap(ishort)

! SUBROUTINE ARGUMENTS - I/O
INTEGER*2, INTENT(INOUT)                         :: ishort                     ! te converteren integer

! LOCAL VARIABLES
INTEGER                                          :: j                          ! tellers
INTEGER*2                                        :: iflg                       ! 128 als ishort(i) < 0; 0 als ishort(i) >= 0
INTEGER*2                                        :: k1                         ! hulpvariabele bij conversie
INTEGER*2                                        :: k2                         ! hulpvariabele bij conversie
INTEGER*2                                        :: maxint2                    ! maximum integerwaarde bij INT2

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'byteswap')

!-------------------------------------------------------------------------------------------------------------------------------
maxint2 = 256

IF (ishort .LT. 0) THEN
  ishort = ishort + 32768
  iflg = 128
ELSE
  iflg = 0
ENDIF
k1 = mod(ishort, maxint2)
k2 = ishort/maxint2
IF (k1 > 128) THEN ! 32768/maxint2 = 128
   j  = (k1-256)*maxint2 + k2 + iflg ! 256*maxint2 = 65536
ELSE
   j  = k1*maxint2 + k2 + iflg
   IF ( j .GT. 32768 ) THEN
      j = j - 65536
   ENDIF
ENDIF

ishort = j

RETURN
END SUBROUTINE byteswap

!-------------------------------------------------------------------------------------------------------------------------------
! FUNCTION             : extractint
! PURPOSE              : Extraction of integer value from a string. Returns whether an integer was extracted; intvalue is 0 if empty string!
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION extractint   (string, intvalue, beyondpos, error)

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: string                     ! String met geheel getal, beginned op pos. 1.

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER,   INTENT(OUT)                           :: intvalue                   ! First position beyond integer read. 0 if end of line.
INTEGER,   INTENT(OUT)                           :: beyondpos                  ! First position beyond integer read. 0 if end of line.
TYPE (TError), INTENT(INOUT)                     :: error                      ! 

! RESULT
LOGICAL                                          :: extractint                 ! 

! LOCAL VARIABLES
INTEGER                                          :: zerochar,lengte            ! 
LOGICAL                                          :: negative,doorgaan          ! 
integer(kind=8)  :: longvalue

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'extractint')

!-------------------------------------------------------------------------------------------------------------------------------
!
! Initialisaties
!
longvalue=0
intvalue=0
negative=.false.
zerochar=ICHAR('0')
beyondpos=0
!
! Determine length of string. If no length no integer.
!
lengte=LEN_TRIM(string)
IF (lengte.EQ.0) GOTO 100                                                      ! No integer located
!
! Determine sign of integer number.
!
beyondpos=1
IF (string(1:1).EQ.'+'.or.string(1:1).EQ.'-') THEN
  IF (lengte.EQ.1) GOTO 200                                                    ! Hier had integer moeten staan
  IF (string(1:1).EQ.'-') negative=.true.
  beyondpos=2
ENDIF
!
! The first character must be a digit. Otherwise no number whatsoever.
!
IF (string(beyondpos:beyondpos).lt.'0'.or. string(beyondpos:beyondpos).gt.'9') GOTO 100
!
! Now extract your integer number.
!
doorgaan=.true.
DO WHILE (doorgaan)
!
! Bepaal bijdrage van dit karakter
!
  longvalue=longvalue*10+(ichar(string(beyondpos:beyondpos)) -zerochar)
  intvalue = int(longvalue,kind=4)
  if (int(intvalue,kind=8) /= longvalue) then
     CALL SetError('Number is out of bounds', error)
     CALL ErrorParam('number', string, .TRUE., error)
     goto 100
  end if
!
! Check next digit, if it is a digit. If it is a blank we finished reading the integer. If no digit, no blank we have an error.
!
  beyondpos=beyondpos+1
  IF (beyondpos.GT.lengte) THEN
    beyondpos = 0
    doorgaan=.false.
  ELSEIF (string(beyondpos:beyondpos).LT.'0'.or. string(beyondpos:beyondpos).GT.'9') THEN
    doorgaan=.false.
  ENDIF
ENDDO
!
! Bepaal en retourneer de waarde voor getint
!
IF (negative) intvalue = -intvalue
extractint = .TRUE.
RETURN
!
! Errorlabel 200: incorrect number located. And pass just the number parsed, which is the first word in string.
!
200 CALL SetError('Number is not an integer', error)
CALL ErrorParam('number', string, .TRUE., error)
!
! Label 100: no number located. This is actually no error, just a default.
!
100 extractint = .FALSE.
CALL ErrorCall(ROUTINENAAM, error)

END FUNCTION extractint

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : GetOS
! PURPOSE            : Returns whether the operating system is UNIX (0 returned) or WinNT (returns 1). Second parameter is the
!                      directory separator for that system (\ or /)
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE GetOS(os, slash)

! !DEC$ ATTRIBUTES DLLEXPORT:: GetOS

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER,   INTENT(OUT)                           :: os                         ! 
CHARACTER, INTENT(OUT), OPTIONAL                 :: slash                      ! 
!
! Local variables:
!
INTEGER                                          :: rtc                        ! 
INTEGER                                          :: colonpos                   ! 
!
! De declaratie van GETCWD is uitgecommentarieerd omdat CWD in IFPORT is opgenomen. IFPORT declareert GETCWD. Als het hier toch
! wordt gedeclareerd neemt de compiler aan, dat dit een externe functie is. Bij het linken wordt die dan niet gevonden.
!
! INTEGER       :: GETCWD
CHARACTER*512                                    :: directory                  ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'GetOS')

rtc = GETCWD(directory)

colonpos = INDEX(directory,':')

IF (ANY(colonpos == (/2,3/)) .AND. directory(colonpos+1:colonpos+1) == '\') THEN
   os = 1                                                                      ! WinNT
   IF (PRESENT(slash)) slash = '\'
ELSE
   os = 0                                                                      ! Unix
   IF (PRESENT(slash)) slash = '/'
ENDIF

END SUBROUTINE GetOS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : GetCLArg
! PURPOSE     : Returns command line arguments.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE GetCLArg(progpath, nrarg, arg, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: GetCLArg

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*512, INTENT(OUT)                       :: progpath                   ! 
INTEGER,   INTENT(OUT)                           :: nrarg                      ! 
CHARACTER*512, INTENT(OUT), DIMENSION(:), POINTER :: arg                       ! 
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record
!
! External FUNCTIONS implemented on both HP-UX and WINNT.
!
INTEGER                                          :: iargc                      ! 
!
! Local variables.
!
INTEGER                                          :: i                          ! argument number counter
INTEGER                                          :: ierr                       ! error status in allocation

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'GetCLArg')

!-------------------------------------------------------------------------------------------------------------------------------
! Retrieve number of arguments. The function iargc() exists on both HP-UX and WINDOWS compilers as an extension and works
! identical. The same holds, starting from HPUX 11.0, for the subroutine getarg.
!
nrarg = iargc()
!
! Allocate memory in arg. The alloc call is switched off because the string allocation has problems under UNIX. Instead the full
! allocate and error handling code was implemented (for the time being).
!
!  CALL alloc(nrarg, arg, error)
ALLOCATE(arg(nrarg), stat = ierr)
CALL AllocError(ierr, ROUTINENAAM, nrarg, 'string', error)
IF (error%haserror) GOTO 9999
!
! Retrieve progpath, the full path of the program. Fill up with blanks.
!
progpath(1:LEN(progpath)) = ' '
CALL getarg(0, progpath)
!
! Retrieve and assign arg. Fill each argument beyond its length with blanks.
!
DO i = 1, nrarg
  arg(i)(1:LEN(arg(i))) = ' '
  CALL getarg(i , arg(i))
ENDDO
RETURN

9999 CALL ErrorCall(ROUTINENAAM, Error)
RETURN

END SUBROUTINE GetCLArg

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : startformat
! PURPOSE            : Starting a format string.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE StartFormat(formatstring, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: StartFormat

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*(*), INTENT(OUT)                       :: formatstring               ! resulting string used in format
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'startformat')

!-------------------------------------------------------------------------------------------------------------------------------
!
! Start the format string with the standard (
!
CALL StringCopy('()', formatstring, error)
IF (error%haserror) THEN
  CALL ErrorCall(ROUTINENAAM, error)
ENDIF
END SUBROUTINE StartFormat

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : appendformat
! PURPOSE            : Appends a format string. Type without nrelts as parameter.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE append_format_string1(typestring, formatstring, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: append_format_string1

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: typestring                 ! type applied to format

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: formatstring               ! resulting string used in format

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER     (ROUTINENAAM = 'append_format_string1')

!-------------------------------------------------------------------------------------------------------------------------------
CALL AppendFormat(1, typestring, formatstring, error)

RETURN
END SUBROUTINE append_format_string1

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : appendformat
! PURPOSE            : Appends a format string.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE append_format_string(nrelts, typestring, formatstring, error)

! !DEC$ ATTRIBUTES DLLEXPORT:: append_format_string

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: nrelts                     ! number of elements in this format string
CHARACTER*(*), INTENT(IN)                        :: typestring                 ! type applied to format

! SUBROUTINE ARGUMENTS - I/O
CHARACTER*(*), INTENT(INOUT)                     :: formatstring               ! resulting string used in format

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                          :: lastpos                    ! Last position in the string (=text lengte)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'append_format_string')

!-------------------------------------------------------------------------------------------------------------------------------
!
! Nothing happens when we have 0 elements.
!
IF (.not. error%haserror .and. nrelts > 0 ) THEN
!
!  Replace the last ) character by a , character. Check whether we have indeed a string to append to, otherwise make an error
!  message. N.B. when this is the first parameter the ) is just removed.
!
   lastpos = LEN_TRIM(formatstring)
   IF (lastpos <= 1) GOTO 1000
   IF (lastpos == 2) THEN                                                      ! First parameter
     formatstring(lastpos:lastpos) = ' '
   ELSE                                                                        ! Not first parameter
     formatstring(lastpos:lastpos) = ','
   ENDIF
!
!  Append the number to the string, if larger than 1.
!
   IF (nrelts > 1) THEN
     CALL appendinteger(formatstring, nrelts, error)
     IF (error%haserror) GOTO 2000
   ENDIF
!
!  Append the opening bracket (
!
   CALL StringAppend(formatstring, '(', error)
   IF (error%haserror) GOTO 2000
!
!  Append the typestring
!
   CALL StringAppend(formatstring, typestring, error)
   IF (error%haserror) GOTO 2000
!
!  Append two closing brackets, one for the typestring and one for the formatstring.
!
   CALL StringAppend(formatstring, '))', error)
   IF (error%haserror) GOTO 2000

ENDIF
RETURN

1000 CALL SetError('Formatting not started with startformat', error)
CALL ErrorParam('Format string', formatstring, error)
CALL ErrorParam('Appended string', typestring, error)

2000 CALL ErrorParam('Number elements', nrelts, error)
CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE append_format_string


!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : SortMatrix
! PURPOSE            : Sorts rows of matrix by one of the columns
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE SortMatrix (matrix, nobs, column)

! !DEC$ ATTRIBUTES DLLEXPORT:: SortMatrix

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: nobs                       ! 
INTEGER,   INTENT(IN)                            :: column                     ! 

! SUBROUTINE ARGUMENTS - I/O
REAL,      INTENT(INOUT)                         :: matrix(:,:)                ! 

! LOCAL VARIABLES
INTEGER                                          :: i                          ! 
INTEGER                                          :: j                          ! 
INTEGER                                          :: ctr                        ! 
INTEGER                                          :: isize                      ! 
REAL,      ALLOCATABLE                           :: tmp(:)                     ! 

isize=SIZE(matrix,DIM=1)
ALLOCATE(tmp(isize))

j=nobs-1
10 ctr=0

DO 20 i=1, j
  IF (matrix(column,i) <= matrix(column,i+1)) GOTO 20
  tmp(:) = matrix(:,i)
  matrix(:,i)=matrix(:,i+1)
  matrix(:,i+1)=tmp(:)
  ctr=1
20 CONTINUE
j=j-1
IF (ctr == 1) GOTO 10

END SUBROUTINE SortMatrix

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : GetIndex
! PURPOSE            : Determines index for a numerically coded identity
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE GetIndex(code, index)

! !DEC$ ATTRIBUTES DLLEXPORT:: GetIndex

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: code                       ! numerical code

! SUBROUTINE ARGUMENTS - I/O
INTEGER,   INTENT(INOUT)                         :: index                      ! index for numerical code

! LOCAL VARIABLES
INTEGER                                          :: idents (50)                ! identities that are already known (max 50) 
INTEGER                                          :: nidents                    ! number of identities that are already known
INTEGER                                          :: i                          ! do loop index
LOGICAL                                          :: found                      ! true if identity has been encountered earlier
LOGICAL                                          :: repeated                   ! true if subroutine is called after the first time
SAVE      nidents, idents, repeated
DATA      repeated / .false. /
!
! Initialization
!
found = .FALSE.
IF (.NOT. repeated) THEN                                                       ! only at first time execution
    idents  = -999
  nidents = 0
ENDIF

DO i=1,nidents
  IF (idents(i) == code) THEN
    index = i
    found = .TRUE.
  EXIT
  ENDIF
ENDDO

IF (.NOT. found) THEN
  nidents=nidents + 1
  idents(nidents) = code
  index = nidents
ENDIF

repeated = .TRUE.

RETURN
END SUBROUTINE GetIndex
!-------------------------------------------------------------------------------------------------------------------------------

END MODULE m_utils
