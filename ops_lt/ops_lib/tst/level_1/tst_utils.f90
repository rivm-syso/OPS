
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
!
!                        - GetNumber: retrieves number from a string
!                        - Alloc: allocation of memory 
!                        - DeAlloc: deallocation of memory
!                        - AllocError: a less important procedure
!                        - Jaartal: converts year to four digit year
!                        - GetOS: retrieves operating system 
!                        - GetCLArg: Retrieves command line arguments
!                        - WisselBytes: Byte swapping 
!                        - StartFormat: Initial assignment to a format string 
!                        - AppendFormat: Appends a format string
!                        - PrependFormat: Put format at start of format string
!                        - GetIndex: Determines index for a numerically coded identity
!                        - SortMatrix: Sorts rows of a matrix
!-------------------------------------------------------------------------------------------------------------------------------
module m_tst_utils

implicit none

contains
 
subroutine tst_utils
use no_pfunit
use m_error, only: TError
use m_utils
implicit none
   real,   pointer :: array1p(:), array2p(:,:), array3p(:,:,:)
   real,   allocatable :: array1a(:), array2a(:,:)
   double precision,   pointer :: darray1p(:), darray2p(:,:)
   integer,   pointer :: iarray1p(:), iarray2p(:,:)
   character(len=12), pointer :: strarray(:)
   integer, parameter :: dims(3) = (/ 4, 7 , 9 /)
   real, parameter :: defvalue = 2.0
   type(TError) :: error
   integer, parameter :: jaartal_inputs(6) = (/0, 22, 76, 1987, 2050, -12 /)
   integer, parameter :: jaartal_outputs(6) = (/2000, 2022, 1976, 1987, 2050, 1988 /)
   character(len=14), parameter :: intstrings(8) = (/ &
           '17            ', '-1       abc  ', '   2          ', ' - 2          ', &
           '-             ', ' twelve       ', '18 twelve     ', '99999999999999' /)
   integer,   parameter :: stringints(8)     = (/ 17,      -1,     0,       0,     0, 0,      18,      0 /)
   integer,   parameter :: sint_beyondpos(8) = (/ 0,       3,      2,       0,     0, 0,      3,       0 /)
   logical,         parameter :: sint_error(8)     = (/.false., .false., .true., .true., .true., .true., .false., .true. /)
   logical,         parameter :: sint_rerror(8)    = (/.false., .false., .false., .false., .true., .false., .false., .true. /)
   logical,         parameter :: sint_default(8)   = (/.false., .false., .true., .true., .true., .true., .false., .true. /)
   character(len=14), parameter :: realstrings(4) = (/ &
           '              ', &
           '2.1e4         ', &
           '.234375e-1    ', &
           '2.1d4         ' /)
   logical,       parameter :: sreal_part(4)    = (/.false.,  .true., .true.,     .true./)
   logical,       parameter :: sreal_rerror(4)  = (/.false., .false., .false.,    .true. /)
   real,          parameter :: stringreals(4)   = (/ 0.,      2.1e4,  .234375e-1,  0.    /)

   integer,   parameter :: swap1_in(6) = (/ 10, -50, 4000, -4000, 0, -1 /)
   integer,   parameter :: swap1_out(6) = (/ 2560, -12545, -24561, 24816, 0, -1 /)
   integer*2          :: swap1_inout(6) , swap2_inout(3,2)

   real   :: matrix_inout(4,3), matrix_ref(4,3)
   real,   parameter :: matrix_in(4,3) = reshape( (/ &
          1.25,  3.75,  12.0, 7.25, &
          0.125, 1.25,  8.25, 6.25, &
          5.25,  3.25,  4.0,  6.5  /), (/4,3/))
   integer,   parameter :: order(4,3) = reshape( (/ &
         123, 123, 123, 123, &
         213, 213, 213, 213, &
         213, 231, 321, 231 /), (/4,3/) )

   integer   :: beyondpos, ivalue
   logical :: isdefault, nopart
   integer :: i,j, k, nrarg
   real   :: rvalue
   logical, parameter :: verbose_sorting = .false.
   character(len=512) :: formatstring, progpath
   character(len=512), pointer :: arg(:)
   character(len=1) :: short_string

   call GetCLArg(progpath, nrarg, arg, error)
   call assertFalse(error%haserror,'error from GetCLarg',__LINE__,__FILE__)
   call assertGreaterThan(index(progpath,'tst_utils'), 0, 'name of this test program',__LINE__,__FILE__)
   print *,'nrarg = ',nrarg
   do i = 1,nrarg
      print *,'  argument ',i,' = "'//trim(arg(i))//'"'
      call assertEqual('--verbose', arg(i), 'command line option should be "--verbose" (or nothing)',__LINE__,__FILE__)
   end do
   print *,'progpath = "'//trim(progpath)//'"'

   print *,'Function AppendFormat'
   formatstring = 'original contents will be erased'
   call StartFormat(short_string, error)
   call assertTrue(error%haserror,'intentional error from StartPos',__LINE__,__FILE__)
   error%haserror=.false. ! reset haserror
   call StartFormat(formatstring, error)
   call assertFalse(error%haserror,'error from StartPos',__LINE__,__FILE__)
   error%haserror=.false. ! reset haserror
   call assertEqual('()',formatstring, 'StartFormat',__LINE__,__FILE__)
   call AppendFormat('g20.10', formatstring, error)
   call assertFalse(error%haserror,'error from AppendFormat',__LINE__,__FILE__)
   call assertEqual('((g20.10))',formatstring, 'StartFormat',__LINE__,__FILE__)
   call AppendFormat(7,'i8', formatstring, error)
   call assertFalse(error%haserror,'error from AppendFormat',__LINE__,__FILE__)
   call assertEqual('((g20.10),7(i8))',formatstring, 'StartFormat',__LINE__,__FILE__)
   formatstring = 'a'
   call AppendFormat(7,'i8', formatstring, error)
   call assertTrue(error%haserror,'intentional error from AppendFormat',__LINE__,__FILE__)
   error%haserror = .false.

   print *,'Function SortMatrix'
   matrix_inout = matrix_in
   if (verbose_sorting) then
      print *,'input:'
      do k = 1,3
         print '(g20.4,10(g10.4))', matrix_inout(:,k)
      end do
   end if
   do i = 1,4
      do j = 1,3
         matrix_inout = matrix_in
         call SortMatrix(matrix_inout, j, i)
         if (verbose_sorting) then
            print *
            print *,'sorted (',i,',',j,')'
            do k = 1,3
               print '(g20.4,10(g10.4))', matrix_inout(:,k)
            end do
         endif
         matrix_ref(:,1) = matrix_in(:,mod(order(i,j)/100,10))
         matrix_ref(:,2) = matrix_in(:,mod(order(i,j)/10, 10))
         matrix_ref(:,3) = matrix_in(:,mod(order(i,j)/1,  10))
         if (verbose_sorting) then
            print *
            print *,' = '
            do k = 1,3
               print '(g30.4,10(g10.4))', matrix_ref(:,k)
            end do
         end if
         call assertEqual(matrix_ref,matrix_inout, 'function SortMatrix',__LINE__,__FILE__)
      end do
   end do

   print *,'Function GetIndex'
   Call GetIndex(18,i)
   call assertEqual(i,1, 'function GetIndex(18,i)',__LINE__,__FILE__)
   Call GetIndex(12,i)
   call assertEqual(i,2, 'function GetIndex(12,i)',__LINE__,__FILE__)
   Call GetIndex(-7,i)
   call assertEqual(i,3, 'function GetIndex(-7,i)',__LINE__,__FILE__)
   Call GetIndex(18,i)
   call assertEqual(i,1, 'function GetIndex(18,i)',__LINE__,__FILE__)
   Call GetIndex(-7,i)
   call assertEqual(i,3, 'function GetIndex(-7,i)',__LINE__,__FILE__)
   Call GetIndex(12,i)
   call assertEqual(i,2, 'function GetIndex(12,i)',__LINE__,__FILE__)

   
   ! Function swapbytes
   swap1_inout = int(swap1_in, kind=2)
   call wisselbytes(swap1_inout, size(swap1_in))
   call assertEqual(int(swap1_out,kind=2), swap1_inout, 'function swapbytes1',__LINE__,__FILE__)

   swap2_inout = reshape(int(swap1_in, kind=2), (/3,2/))
   call wisselbytes(swap2_inout, 3, 2)
   call assertEqual(int(swap1_out,kind=2), swap1_inout, 'function swapbytes1',__LINE__,__FILE__)

   ! Function GetNumber
   do i = 1,size(stringints)
      print *,'testing conversion of ',intstrings(i)
      call getNumber(intstrings(i), ivalue, isdefault, error, beyondpos)
      call assertEqual(error%haserror, sint_error(i), 'function GetInt: error',__LINE__,__FILE__)
      call assertEqual(isdefault, sint_default(i), 'function GetInt: isdefault',__LINE__,__FILE__)
      if (.not. error%haserror) then
         call assertEqual(beyondpos, sint_beyondpos(i), 'function GetInt: beyond-pos',__LINE__,__FILE__)
         call assertEqual(ivalue, stringints(i), 'function GetInt: result',__LINE__,__FILE__)
      end if
      error%haserror = .false.
      call getNumber(intstrings(i), rvalue, isdefault, error)
      call assertEqual(error%haserror, sint_rerror(i), 'function GetReal: error',__LINE__,__FILE__)
      if (.not. error%haserror) then
         call assertEqual(isdefault, sint_default(i), 'function GetReal: isdefault',__LINE__,__FILE__)
         call assertEqual(rvalue, real(stringints(i)), 'function GetReal: result',__LINE__,__FILE__)
      end if
      error%haserror = .false.
   end do

   ! Function GetNumber
   do i = 1,size(stringreals)
      print *,'testing real conversion of ',realstrings(i)
      call getNumber(realstrings(i), rvalue, nopart, error)
      call assertEqual(error%haserror, sreal_rerror(i), 'function GetReal: error',__LINE__,__FILE__)
      if (.not. error%haserror) then
         call assertEqual(.not. nopart, sreal_part(i), 'function GetReal: real was read',__LINE__,__FILE__)
         call assertEqual(rvalue, stringreals(i), 1e-5,'function GetReal: result',__LINE__,__FILE__)
      end if
      error%haserror = .false.
   end do

   print *
   print *,'testing (de)allocation'

   call AllocError(ierr=0, routinenaam='No error', dim=187, arraytype='real', error=error)
   call assertFalse(error%haserror,'AllocError with zero error code',__LINE__,__FILE__)

   call AllocError(ierr=12, routinenaam='No error', dim=187, arraytype='real', error=error)
   call assertTrue(error%haserror,'intentional error from AllocError',__LINE__,__FILE__)
   call assertEqual(error%message, "No memory for allocation ofreal array", 'message from AllocError',__LINE__,__FILE__)
   error%haserror = .false.

   ! single precision arrays
   call alloc(dims(1), array1p, error)
   call assertFalse(error%haserror,'allocation 1d pointer array',__LINE__,__FILE__)
   call assertEqual(dims(1:1), shape(array1p), 'shape 1d pointer array',__LINE__,__FILE__)
   call assertEqual(0.0, sum(abs(array1p)), 'value 1d pointer array',__LINE__,__FILE__)
   call dealloc(array1p)

   call alloc(dims(1), defvalue, array1p, error)
   call assertFalse(error%haserror,'allocation 1d pointer array',__LINE__,__FILE__)
   call assertEqual(dims(1:1), shape(array1p), 'shape 1d pointer array',__LINE__,__FILE__)
   call assertEqual(dims(1)*defvalue, sum(abs(array1p)), 'nonzero value 1d pointer array',__LINE__,__FILE__)
   call dealloc(array1p)

   call alloc(dims(1), dims(2), array2p, error)
   call assertFalse(error%haserror,'allocation 2d pointer array',__LINE__,__FILE__)
   call assertEqual(dims(1:2), shape(array2p), 'shape 2d pointer array',__LINE__,__FILE__)
   call dealloc(array2p)

   call alloc(dims(1), dims(2), dims(3), array3p, error)
   call assertFalse(error%haserror,'allocation 3d pointer array',__LINE__,__FILE__)
   call assertEqual(dims(1:3), shape(array3p), 'shape 3d pointer array',__LINE__,__FILE__)
   call dealloc(array3p)

   call alloc(dims(1), array1a, error)
   call assertFalse(error%haserror,'allocation 1d allocatable array',__LINE__,__FILE__)
   call assertEqual(dims(1:1), shape(array1a), 'shape 1d allocatable array',__LINE__,__FILE__)

   call alloc(dims(1), dims(2), array2a, error)
   call assertFalse(error%haserror,'allocation 2d allocatable array',__LINE__,__FILE__)
   call assertEqual(dims(1:2), shape(array2a), 'shape 2d allocatable array',__LINE__,__FILE__)


   ! double precision arrays
   call alloc(dims(1), darray1p, error)
   call assertFalse(error%haserror,'allocation 1d pointer double array',__LINE__,__FILE__)
   call assertEqual(dims(1:1), shape(darray1p), 'shape 1d pointer double array',__LINE__,__FILE__)
   call assertEqual(dble(0.0), sum(abs(darray1p)), 'value 1d pointer double array',__LINE__,__FILE__)
   call dealloc(darray1p)

   call alloc(dims(1), defvalue, darray1p, error)
   call assertFalse(error%haserror,'allocation 1d pointer double array',__LINE__,__FILE__)
   call assertEqual(dims(1:1), shape(darray1p), 'shape 1d pointer double array',__LINE__,__FILE__)
   call assertEqual(dble(dims(1)*defvalue), sum(abs(darray1p)), 'nonzero value 1d pointer double array',__LINE__,__FILE__)
   call dealloc(darray1p)

   call alloc(dims(1), dims(2), darray2p, error)
   call assertFalse(error%haserror,'allocation 2d pointer double array',__LINE__,__FILE__)
   call assertEqual(dims(1:2), shape(darray2p), 'shape 2d pointer double array',__LINE__,__FILE__)
   call dealloc(darray2p)


   ! Integer arrays
   call alloc(dims(1), iarray1p, error)
   call assertFalse(error%haserror,'allocation 1d pointer integer array',__LINE__,__FILE__)
   call assertEqual(dims(1:1), shape(iarray1p), 'shape 1d pointer integer array',__LINE__,__FILE__)
   call assertEqual(0, sum(abs(iarray1p)), 'value 1d pointer integer array',__LINE__,__FILE__)
   call dealloc(iarray1p)

   call alloc(dims(1), int(defvalue), iarray1p, error)
   call assertFalse(error%haserror,'allocation 1d pointer integer array',__LINE__,__FILE__)
   call assertEqual(dims(1:1), shape(iarray1p), 'shape 1d pointer integer array',__LINE__,__FILE__)
   call assertEqual(int(dims(1)*defvalue), sum(abs(iarray1p)), 'nonzero value 1d pointer integer array',__LINE__,__FILE__)
   call dealloc(iarray1p)

   call alloc(dims(1), dims(2), iarray2p, error)
   call assertFalse(error%haserror,'allocation 2d pointer integer array',__LINE__,__FILE__)
   call assertEqual(dims(1:2), shape(iarray2p), 'shape 2d pointer integer array',__LINE__,__FILE__)
   !call dealloc(iarray2p)

   ! string arrays
   call alloc(dims(1), strarray, error)
   call assertFalse(error%haserror,'allocation 1d pointer string array',__LINE__,__FILE__)
   call assertEqual(dims(1:1), shape(strarray), 'shape 1d pointer string array',__LINE__,__FILE__)
   call dealloc(strarray)

   ! Function Jaartal
   do i = 1,size(jaartal_inputs)
      print *,'testing jaartal(',jaartal_inputs(i),')'
      call assertEqual(Jaartal(jaartal_inputs(i)), jaartal_outputs(i), 'function jaartal',__LINE__,__FILE__)
   end do

end subroutine tst_utils


subroutine tst_Alloc

   use no_pfunit
   use m_utils
   use m_error

   implicit none

   TYPE (TError)         :: error                      ! Error handling record
   INTEGER   :: dim1, dim2, dim3, dim4, dim5

   REAL,   DIMENSION(:), POINTER    :: arr_real                        ! 
   REAL,   DIMENSION(:), ALLOCATABLE    :: arr_reala                        ! 
   REAL,   DIMENSION(:,:), POINTER    :: arr_real2                        ! 
   REAL,   DIMENSION(:,:), ALLOCATABLE    :: arr_real2a                        ! 
   REAL,   DIMENSION(:,:,:), POINTER    :: arr_real3                        ! 
   REAL,   DIMENSION(:,:,:), ALLOCATABLE    :: arr_real3a                        ! 
   REAL,   DIMENSION(:,:,:,:), POINTER    :: arr_real4                        ! 
   REAL,   DIMENSION(:,:,:,:), ALLOCATABLE    :: arr_real4a                        ! 
   REAL,   DIMENSION(:,:,:,:,:), POINTER    :: arr_real5                        ! 
   REAL,   DIMENSION(:,:,:,:,:), ALLOCATABLE    :: arr_real5a                        ! 

   DOUBLE PRECISION, DIMENSION(:), POINTER    :: arr_double                        ! 
   DOUBLE PRECISION, DIMENSION(:,:), POINTER    :: arr_double2                        ! 
   DOUBLE PRECISION, DIMENSION(:,:,:), POINTER    :: arr_double3                        ! 
   DOUBLE PRECISION, DIMENSION(:,:,:,:), POINTER    :: arr_double4                        ! 
   DOUBLE PRECISION, DIMENSION(:,:,:,:,:), POINTER    :: arr_double5                        ! 

   INTEGER,   DIMENSION(:), POINTER    :: arr_integer                        ! 
   INTEGER,   DIMENSION(:,:), POINTER    :: arr_integer2                        ! 
   INTEGER,   DIMENSION(:,:,:), POINTER    :: arr_integer3                        ! 
   INTEGER,   DIMENSION(:,:,:,:), POINTER    :: arr_integer4                        ! 
   INTEGER,   DIMENSION(:,:,:,:,:), POINTER    :: arr_integer5                        ! 

   ! common dimensions for tests
   dim1 = 5
   dim2 = 1
   dim3 = 2
   dim4 = 3
   dim5 = 4

   ! test allocreal0
   call Alloc(dim1, arr_real, error)
   call assertEqual(.true.,ASSOCIATED(arr_real),'=== arr_real0 - allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_real0 - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0.0,arr_real(dim1-1),'=== arr_real0 - init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1 /), SHAPE(arr_real),'=== arr_real0 - shape ===',__LINE__,__FILE__)
   call Dealloc(arr_real)
   call assertEqual(.false.,ASSOCIATED(arr_real),'=== arr_real0 - deallocated ===',__LINE__,__FILE__)

   ! test allocreal
   call Alloc(dim1, 9.0, arr_real, error)
   call assertEqual(.true.,ASSOCIATED(arr_real),'=== arr_real - allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_real - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(9.0,arr_real(dim1-1),'=== arr_real - init 9 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1 /), SHAPE(arr_real),'=== arr_real - shape ===',__LINE__,__FILE__)
   call Dealloc(arr_real)
   call assertEqual(.false.,ASSOCIATED(arr_real),'=== arr_real - deallocated ===',__LINE__,__FILE__)

   ! test allocreala
   call Alloc(dim1, arr_reala, error)
   call assertEqual(.true.,ALLOCATED(arr_reala),'=== arr_reala - allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_reala - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0.0,arr_reala(dim1-1),'=== arr_reala - init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1 /), SHAPE(arr_reala),'=== arr_reala - shape ===',__LINE__,__FILE__)

   ! test allocreal2
   call Alloc(dim1, dim2, arr_real2, error)
   call assertEqual(.true.,ASSOCIATED(arr_real2),'=== arr_real2- allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_real2 - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0.0,arr_real2(dim1-1,dim2),'=== arr_real2- init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1,dim2 /), SHAPE(arr_real2),'=== arr_real2 - shape ===',__LINE__,__FILE__)
   call Dealloc(arr_real2)
   call assertEqual(.false.,ASSOCIATED(arr_real2),'=== arr_real2- deallocated ===',__LINE__,__FILE__)

   ! test allocreal2a
   call Alloc(dim1, dim2, arr_real2a, error)
   call assertEqual(.true.,ALLOCATED(arr_real2a),'=== arr_real2a - allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_real2a - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0.0,arr_real2a(dim1-1, dim2),'=== arr_real2a - init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1,dim2 /), SHAPE(arr_real2a),'=== arr_real2a - shape ===',__LINE__,__FILE__)

   ! test allocreal3
   call Alloc(dim1, dim2, dim3, arr_real3, error)
   call assertEqual(.true.,ASSOCIATED(arr_real3),'=== arr_real3- allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_real3 - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0.0,arr_real3(dim1-1,dim2,dim3-1),'=== arr_real3- init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1,dim2,dim3 /), SHAPE(arr_real3),'=== arr_real3 - shape ===',__LINE__,__FILE__)
   call Dealloc(arr_real3)
   call assertEqual(.false.,ASSOCIATED(arr_real3),'=== arr_real3- deallocated ===',__LINE__,__FILE__)

   ! test allocreal3a
   call Alloc(dim1, dim2, dim3, arr_real3a, error)
   call assertEqual(.true.,ALLOCATED(arr_real3a),'=== arr_real3a - allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_real3a - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0.0,arr_real3a(dim1-1, dim2, dim3),'=== arr_real3a - init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1,dim2,dim3 /), SHAPE(arr_real3a),'=== arr_real3a - shape ===',__LINE__,__FILE__)

   ! test allocreal4
   call Alloc(dim1, dim2, dim3, dim4, arr_real4, error)
   call assertEqual(.true.,ASSOCIATED(arr_real4),'=== arr_real4- allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_real4 - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0.0,arr_real4(dim1-1,dim2,dim3-1,dim4-1),'=== arr_real4- init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1,dim2,dim3,dim4 /), SHAPE(arr_real4),'=== arr_real4 - shape ===',__LINE__,__FILE__)
   DEALLOCATE(arr_real4)
   call assertEqual(.false.,ASSOCIATED(arr_real4),'=== arr_real4- deallocated ===',__LINE__,__FILE__)

   ! test allocreal4a
   call Alloc(dim1, dim2, dim3, dim4, arr_real4a, error)
   call assertEqual(.true.,ALLOCATED(arr_real4a),'=== arr_real4a - allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_real4a - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0.0,arr_real4a(dim1-1, dim2, dim3, dim4-1),'=== arr_real4a - init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1,dim2,dim3,dim4 /), SHAPE(arr_real4a),'=== arr_real4a - shape ===',__LINE__,__FILE__)

   ! test allocreal5
   call Alloc(dim1, dim2, dim3, dim4, dim5, arr_real5, error)
   call assertEqual(.true.,ASSOCIATED(arr_real5),'=== arr_real5- allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_real5 - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0.0,arr_real5(dim1-1,dim2,dim3-1,dim4-1,dim5),'=== arr_real5- init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1,dim2,dim3,dim4,dim5 /), SHAPE(arr_real5),'=== arr_real5 - shape ===',__LINE__,__FILE__)
   DEALLOCATE(arr_real5)
   call assertEqual(.false.,ASSOCIATED(arr_real5),'=== arr_real5- deallocated ===',__LINE__,__FILE__)

   ! test allocreal5a
   call Alloc(dim1, dim2, dim3, dim4, dim5, arr_real5a, error)
   call assertEqual(.true.,ALLOCATED(arr_real5a),'=== arr_real5a - allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_real5a - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0.0,arr_real5a(dim1-1, dim2, dim3, dim4-1, dim5),'=== arr_real5a - init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1,dim2,dim3,dim4,dim5 /), SHAPE(arr_real5a),'=== arr_real5a - shape ===',__LINE__,__FILE__)

   ! test allocdouble0
   call Alloc(dim1, arr_double, error)
   call assertEqual(.true.,ASSOCIATED(arr_double),'=== arr_double0 - allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_double0 - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0.d0,arr_double(dim1-1),'=== arr_double0 - init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1 /), SHAPE(arr_double),'=== arr_double0 - shape ===',__LINE__,__FILE__)
   call Dealloc(arr_double)
   call assertEqual(.false.,ASSOCIATED(arr_double),'=== arr_double0 - deallocated ===',__LINE__,__FILE__)

   ! test allocdouble
   call Alloc(dim1, 9.0, arr_double, error)
   call assertEqual(.true.,ASSOCIATED(arr_double),'=== arr_double - allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_double - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(9.d0,arr_double(dim1-1),'=== arr_double - init 9 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1 /), SHAPE(arr_double),'=== arr_double - shape ===',__LINE__,__FILE__)
   call Dealloc(arr_double)
   call assertEqual(.false.,ASSOCIATED(arr_double),'=== arr_double - deallocated ===',__LINE__,__FILE__)

   ! test allocdouble2
   call Alloc(dim1, dim2, arr_double2, error)
   call assertEqual(.true.,ASSOCIATED(arr_double2),'=== arr_double2- allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_double2 - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0.d0,arr_double2(dim1-1,dim2),'=== arr_double2- init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1,dim2 /), SHAPE(arr_double2),'=== arr_double2 - shape ===',__LINE__,__FILE__)
   call Dealloc(arr_double2)
   call assertEqual(.false.,ASSOCIATED(arr_double2),'=== arr_double2- deallocated ===',__LINE__,__FILE__)

   ! test allocdouble3
   call Alloc(dim1, dim2, dim3, arr_double3, error)
   call assertEqual(.true.,ASSOCIATED(arr_double3),'=== arr_double3- allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_double3 - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0.d0,arr_double3(dim1-1,dim2,dim3-1),'=== arr_double3- init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1,dim2,dim3 /), SHAPE(arr_double3),'=== arr_double3 - shape ===',__LINE__,__FILE__)
   DEALLOCATE(arr_double3)
   call assertEqual(.false.,ASSOCIATED(arr_double3),'=== arr_double3- deallocated ===',__LINE__,__FILE__)

   ! test allocdouble4
   call Alloc(dim1, dim2, dim3, dim4, arr_double4, error)
   call assertEqual(.true.,ASSOCIATED(arr_double4),'=== arr_double4- allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_double4 - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0.d0,arr_double4(dim1-1,dim2,dim3-1,dim4-1),'=== arr_double4- init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1,dim2,dim3,dim4 /), SHAPE(arr_double4),'=== arr_double4 - shape ===',__LINE__,__FILE__)
   DEALLOCATE(arr_double4)
   call assertEqual(.false.,ASSOCIATED(arr_double4),'=== arr_double4- deallocated ===',__LINE__,__FILE__)

   ! test allocdouble5
   call Alloc(dim1, dim2, dim3, dim4, dim5, arr_double5, error)
   call assertEqual(.true.,ASSOCIATED(arr_double5),'=== arr_double5- allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_double5 - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0.d0,arr_double5(dim1-1,dim2,dim3-1,dim4-1,dim5),'=== arr_double5- init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1,dim2,dim3,dim4,dim5 /), SHAPE(arr_double5),'=== arr_double5 - shape ===',__LINE__,__FILE__)
   DEALLOCATE(arr_double5)
   call assertEqual(.false.,ASSOCIATED(arr_double5),'=== arr_double5- deallocated ===',__LINE__,__FILE__)

   ! test allocinteger0
   call Alloc(dim1, arr_integer, error)
   call assertEqual(.true.,ASSOCIATED(arr_integer),'=== arr_integer0 - allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_integer0 - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0,arr_integer(dim1-1),'=== arr_integer0 - init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1 /), SHAPE(arr_integer),'=== arr_integer0 - shape ===',__LINE__,__FILE__)
   call Dealloc(arr_integer)
   call assertEqual(.false.,ASSOCIATED(arr_integer),'=== arr_integer0 - deallocated ===',__LINE__,__FILE__)

   ! test allocinteger
   call Alloc(dim1, 9, arr_integer, error)
   call assertEqual(.true.,ASSOCIATED(arr_integer),'=== arr_integer - allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_integer - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(9,arr_integer(dim1-1),'=== arr_integer - init 9 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1 /), SHAPE(arr_integer),'=== arr_integer - shape ===',__LINE__,__FILE__)
   call Dealloc(arr_integer)
   call assertEqual(.false.,ASSOCIATED(arr_integer),'=== arr_integer - deallocated ===',__LINE__,__FILE__)

   ! test allocinteger2
   call Alloc(dim1, dim2, arr_integer2, error)
   call assertEqual(.true.,ASSOCIATED(arr_integer2),'=== arr_integer2- allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_integer2 - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0,arr_integer2(dim1-1,dim2),'=== arr_integer2- init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1,dim2 /), SHAPE(arr_integer2),'=== arr_integer2 - shape ===',__LINE__,__FILE__)
   DEALLOCATE(arr_integer2)
   call assertEqual(.false.,ASSOCIATED(arr_integer2),'=== arr_integer2- deallocated ===',__LINE__,__FILE__)

   ! test allocinteger3
   call Alloc(dim1, dim2, dim3, arr_integer3, error)
   call assertEqual(.true.,ASSOCIATED(arr_integer3),'=== arr_integer3- allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_integer3 - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0,arr_integer3(dim1-1,dim2,dim3-1),'=== arr_integer3- init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1,dim2,dim3 /), SHAPE(arr_integer3),'=== arr_integer3 - shape ===',__LINE__,__FILE__)
   DEALLOCATE(arr_integer3)
   call assertEqual(.false.,ASSOCIATED(arr_integer3),'=== arr_integer3- deallocated ===',__LINE__,__FILE__)

   ! test allocinteger4
   call Alloc(dim1, dim2, dim3, dim4, arr_integer4, error)
   call assertEqual(.true.,ASSOCIATED(arr_integer4),'=== arr_integer4- allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_integer4 - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0,arr_integer4(dim1-1,dim2,dim3-1,dim4-1),'=== arr_integer4- init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1,dim2,dim3,dim4 /), SHAPE(arr_integer4),'=== arr_integer4 - shape ===',__LINE__,__FILE__)
   DEALLOCATE(arr_integer4)
   call assertEqual(.false.,ASSOCIATED(arr_integer4),'=== arr_integer4- deallocated ===',__LINE__,__FILE__)

   ! test allocinteger5
   call Alloc(dim1, dim2, dim3, dim4, dim5, arr_integer5, error)
   call assertEqual(.true.,ASSOCIATED(arr_integer5),'=== arr_integer5- allocated ===',__LINE__,__FILE__)
   call assertEqual(.false.,error%haserror,'=== arr_integer5 - error%haserror ===',__LINE__,__FILE__)
   call assertEqual(0,arr_integer5(dim1-1,dim2,dim3-1,dim4-1,dim5),'=== arr_integer5- init 0 ===',__LINE__,__FILE__)
   call assertEqual((/ dim1,dim2,dim3,dim4,dim5 /), SHAPE(arr_integer5),'=== arr_integer5 - shape ===',__LINE__,__FILE__)
   DEALLOCATE(arr_integer5)
   call assertEqual(.false.,ASSOCIATED(arr_integer5),'=== arr_integer5- deallocated ===',__LINE__,__FILE__)

end subroutine tst_Alloc

!--------------------------------------------------------------------------------------
subroutine tst_GetNumber

USE no_pfunit
USE m_utils
USE m_error
use m_commonfile, only: IOB_STDOUT

IMPLICIT NONE

integer, parameter :: mtest = 100                ! maximal number of tests
integer       :: i                               ! index of test
integer       :: i_end                           ! index of last test
real          :: rval                            ! real value read from string
integer       :: ival                            ! integer value read from string
integer       :: beyondpos                       ! first position beyond integer in string; 0 if end of line
logical       :: isdefault                       ! if nothing is extracted (use deafult value in this case)
type(Terror)  :: error                           ! error structure

! Reference data for each test:
character(len = 20) :: str_(mtest)               ! input string
real          :: rval_ref(mtest)                 ! real value read from string
integer       :: ival_ref(mtest)                 ! integer value read from string
integer       :: beyondpos_ref(mtest)            ! first position beyond integer in string; 0 if end of line
logical       :: isdefault_ref(mtest)            ! if nothing is extracted (use deafult value in this case)
logical       :: error_ref(mtest)                ! error has occurred
character(len=40) :: msg                         ! message for AssertEqual

! Define strings where to read numbers from and set reference data. -999 implies "this value is not read".
! Note: at least one of ival_ref or rval_ref must be set (not -999)
i = 1;     str_(i) = '1'        ; ival_ref(i) = 1   ; rval_ref(i) = -999.; isdefault_ref(i) = .false.; beyondpos_ref(i) = -999; error_ref(i) = .false.
i = i + 1; str_(i) = ''         ; ival_ref(i) = 0   ; rval_ref(i) = -999.; isdefault_ref(i) = .true. ; beyondpos_ref(i) = -999; error_ref(i) = .false.
i = i + 1; str_(i) = ''         ; ival_ref(i) = -999; rval_ref(i) =  0.0 ; isdefault_ref(i) = .true. ; beyondpos_ref(i) = -999; error_ref(i) = .false.
i = i + 1; str_(i) = '2.0'      ; ival_ref(i) = -999; rval_ref(i) =  2.0 ; isdefault_ref(i) = .false.; beyondpos_ref(i) = -999; error_ref(i) = .false.
i = i + 1; str_(i) = '1'        ; ival_ref(i) = 1   ; rval_ref(i) = -999.; isdefault_ref(i) = .false.; beyondpos_ref(i) = 0   ; error_ref(i) = .false.
i = i + 1; str_(i) = '1 hallo'  ; ival_ref(i) = 1   ; rval_ref(i) = -999.; isdefault_ref(i) = .false.; beyondpos_ref(i) = 2   ; error_ref(i) = .false.
i = i + 1; str_(i) = '1 !hallo' ; ival_ref(i) = 1   ; rval_ref(i) = -999.; isdefault_ref(i) = .false.; beyondpos_ref(i) = 2   ; error_ref(i) = .false.
i = i + 1; str_(i) = '123 hallo'; ival_ref(i) = 123 ; rval_ref(i) = -999.; isdefault_ref(i) = .false.; beyondpos_ref(i) = 4   ; error_ref(i) = .false.
i = i + 1; str_(i) = 'hallo 234'; ival_ref(i) = 0   ; rval_ref(i) = -999.; isdefault_ref(i) = .true. ; beyondpos_ref(i) = 4   ; error_ref(i) = .true.
i = i + 1; str_(i) = '34x'      ; ival_ref(i) = 0   ; rval_ref(i) = -999.; isdefault_ref(i) = .false.; beyondpos_ref(i) = 4   ; error_ref(i) = .true.
i = i + 1; str_(i) = ' '        ; ival_ref(i) = 0   ; rval_ref(i) = -999.; isdefault_ref(i) = .true. ; beyondpos_ref(i) = 0   ; error_ref(i) = .false.

! Output of tests. 
!             str                            ival     rval   isdefault   beyondpos error%haserror
! === test 01  "1                   "           1  -888.0000     F        -888        F
! === test 02  "                    "           0  -888.0000     T        -888        F
! === test 03  "                    "        -888  0.0000000E+00 T        -888        F
! === test 04  "2.0                 "        -888   2.000000     F        -888        F
! === test 05  "1                   "           1  -888.0000     F           0        F
! === test 06  "1 hallo             "           1  -888.0000     F           2        F
! === test 07  "1 !hallo            "           1  -888.0000     F           2        F
! === test 08  "123 hallo           "         123  -888.0000     F           4        F
! === test 09  "hallo 234           "        -888  -888.0000     T        -888        T
! === test 10  "34x                 "        -888  -888.0000     F        -888        T
! === test 11  "                    "           0  -888.0000     T           0        F

! After call: 
! if (error) then
!    -> stop program
! else
!    if (isdefault) then
!       -> default value 0 is returned; check if this is ok or not !
!    else
!       -> value read is returned
!    endif
! endif

! Last test:
i_end = i

write(*,*) ' '

! Read number and check result:
do i = 1,i_end

   ! Initial values:
   rval = -888.
   ival = -888
   beyondpos = -888
   error%haserror = .false.
   
   ! Read number:
   if (ival_ref(i) .ne. -999 .and. beyondpos_ref(i) .ne. -999) then
      call GetNumber(trim(str_(i)), ival, isdefault, error, beyondpos)
   elseif (ival_ref(i) .ne. -999) then
      call GetNumber(trim(str_(i)), ival, isdefault, error)
   elseif (rval_ref(i) .ne. -999.) then
      call GetNumber(trim(str_(i)), rval, isdefault, error)
   endif

   write(msg,'(a,i2.2)') '=== test ',i

   ! Output to screen:
   ! write(*,*) trim(msg),'  "',str_(i),'"',ival,rval,isdefault,beyondpos,error%haserror
   
   ! Check if error has been trapped:
   if (error_ref(i)) then
      ! Expected error:
      call assertEqual(.true.,error%haserror,trim(msg)//' - error%haserror ===',__LINE__,__FILE__)

   else
      ! Unexpected error:
      if (error%haserror) goto 9999   
      
      ! Check results:
      if (ival_ref(i) .ne. -999) then
         call assertEqual(ival_ref(i),ival,trim(msg)//' - ival ===',__LINE__,__FILE__)
      endif
      if (rval_ref(i) .ne. -999.0) then
         call assertEqual(rval_ref(i),rval,trim(msg)//' - rval ===',__LINE__,__FILE__)
      endif
      call assertEqual(isdefault_ref(i),isdefault,trim(msg)//' - isdefault ===',__LINE__,__FILE__)
      if (beyondpos_ref(i) .ne. -999.0) then
         call assertEqual(beyondpos_ref(i),beyondpos,trim(msg)//' - beyondpos ===',__LINE__,__FILE__)
      endif
   endif
enddo

return

9999 CALL ErrorCall('tst_GetNumber', error)
call write_error(IOB_STDOUT,error)

end subroutine tst_GetNumber

end module m_tst_utils

!--------------------------------------------------------------------
program p_tst_utils

use no_pfunit
use m_tst_utils

call tst_utils
call tst_Alloc
call tst_GetNumber
call conclusion

end program p_tst_utils
