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
! DESCRIPTION          : Handling of APS grid data.
!                      : APS grid related grid-types:
!                          - TGridHeader : header information of APS grid (coordinates upper-left corner, grid resolutions, grid dimensions)
!                          - TApsGridInt : integer APS grid
!                          - TApsGridReal: real APS grid 
!                        APS grid related (overloaded) subroutines:
!                          - ReadAps   : reads and allocates a real/integer grid
!                          - GetValue  : returns grid value at certain coordinates
!                          - SetAverage: sets average of a field in APS grid structure
!                          - Dealloc   : deallocates APS-grid
!                        Note: Missing values in an APS grid can be specified by a negative value in the 
!                              upper left grid cell (1,1), e.g. -999.0. If the upper left grid cell value is positive,
!                              then there are no missing values. 
!-------------------------------------------------------------------------------------------------------------------------------

MODULE m_aps

USE m_error
USE m_fileutils

IMPLICIT NONE

!-------------------------------------------------------------------------------------------------------------------------------
! Type       : TGridHeader
! Purpose    : Defines grid dimensions.
!-------------------------------------------------------------------------------------------------------------------------------
TYPE TGridHeader
   REAL*4                                        :: xul_corner                 ! x-coordinate of upper left corner of grid
   REAL*4                                        :: yul_corner                 ! y-coordinate of upper left corner of grid
   INTEGER*4                                     :: nrcol                      ! number of grid columns
   INTEGER*4                                     :: nrrow                      ! number of grid rows
   REAL*4                                        :: grixl                      ! horizontal size of grid cell
   REAL*4                                        :: griyl                      ! vertical size of grid cell
END TYPE TGridHeader

!-------------------------------------------------------------------------------------------------------------------------------
! Type       : TApsGridInt
! Purpose    : Definition of APS-grid with INTEGER grid values.
!-------------------------------------------------------------------------------------------------------------------------------
TYPE TApsGridInt
   TYPE (TGridHeader)                            :: gridheader                 ! grid header
   INTEGER*2, DIMENSION(:,:,:), POINTER          :: value                      ! 3D array with integer values
END TYPE TApsGridInt

!-------------------------------------------------------------------------------------------------------------------------------
! Type       : TApsGridReal
! Purpose    : Definition of APS-grid with FLOAT grid values.
!-------------------------------------------------------------------------------------------------------------------------------
TYPE TApsGridReal
   TYPE (TGridHeader)                            :: gridheader                 ! grid header
   REAL*4, DIMENSION(:), POINTER                 :: average                    ! average of all grid values
   REAL*4, DIMENSION(:,:,:), POINTER             :: value                      ! 3D array with real values
END TYPE TApsGridReal

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : ReadAps
! DESCRIPTION : Reading of aps file with grid data.
! INPUTS      : filename   (character*(*)). Name of the aps file 
!               gridtitle  (character*(*)). Description of grid shown in error messages.
! OUTPUTS     : gridvalues (type). Grid values read from grid file. Generic for different types of grid (float, integer).
!               error      (TError object). Assigned when an error occurred.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE ReadAps
   MODULE PROCEDURE read_aps_real
   MODULE PROCEDURE read_aps_integer
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : Dealloc
! DESCRIPTION : Deallocation of everything allocated in a grid.
! INPUTS      : grid       generic, either real or integer grid. The grid to be deallocated.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE Dealloc
   MODULE PROCEDURE dealloc_aps_real
   MODULE PROCEDURE dealloc_aps_integer
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : GridValue
! DESCRIPTION : Returns value of grid cell with input coordinates.
!               If coordinates outside grid, the average value (real grid) or 0 (integer) grid is returned. A flag, which
!               indicates whether coordinates were inside the grid, is also returned.
! INPUTS      : x          (real*4). RDM x-coordinate value (in km).
!               y          (real*4). RDM y-coordinate value (in km).
!               grid       (type TAPSGrid, generic) The aps grid definition.
! OUTPUTS     : value      (integer*4 or real*4, generic with grid type)
!                          The value in the grid cell or the default value (in case of location outside grid)
!               iscell     (logical) Whether value comes from a grid cell.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE GridValue
   MODULE PROCEDURE grid_value_real
   MODULE PROCEDURE grid_value_integer
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE  : SetAverage
! DESCRIPTION : Sets average field in aps grid structure. Average is calculated over all cells with a valid value (not missing). 
!               Note: Missing values in an APS grid can be specified by a negative value in the 
!                     upper left grid cell (1,1), e.g. -999.0. If the upper left grid cell value is positive, there are no missing values. 
!               It is possible to multiply all values by a certain factor first.
! INPUTS      : factor     (real*4, optional). Multiplication factor.
! INPUT/OUTPUTS: grid      (TApsGridReal). The field grid.average is adjusted.
!-------------------------------------------------------------------------------------------------------------------------------
INTERFACE SetAverage
   MODULE PROCEDURE set_average
END INTERFACE

!-------------------------------------------------------------------------------------------------------------------------------
! Private declarations
! APS grid related private routines: 
!   - read_aps_header: read APS grid header
!   - grid_cell_index: returns grid cell indices corresponding to given coordinates
!-------------------------------------------------------------------------------------------------------------------------------
PRIVATE read_aps_header                                                        ! called by ReadAps only
PRIVATE grid_cell_index                                                        ! called by GridValue only

!-------------------------------------------------------------------------------------------------------------------------------
! Implementation
!-------------------------------------------------------------------------------------------------------------------------------
CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   read_aps_real
! Purpose      Reading of aps file with real data.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE read_aps_real(filename, gridtitle, floatgrid, error)

!DEC$ ATTRIBUTES DLLEXPORT:: read_aps_real

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: filename                   ! name of the aps file 
CHARACTER*(*), INTENT(IN)                        :: gridtitle                  ! description of grid shown in error messages
! INPUTS      : filename   (character*(*)). 
!               gridtitle  (character*(*)). 
! OUTPUTS     : gridvalues (type). 
!               error      (TError object). .

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TApsGridReal), INTENT(OUT)                 :: floatgrid                  ! grid values read from grid file (float)
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record; assigned when an error occurred

! LOCAL VARIABLES
INTEGER*4                                        :: i                          ! row index
INTEGER*4                                        :: j                          ! column index
INTEGER*4                                        :: n                          ! field index
INTEGER*4                                        :: nfield                     ! number of grid fields
INTEGER*4                                        :: nrcol                      ! number of grid columns
INTEGER*4                                        :: nrrow                      ! number of grid rows
INTEGER*4                                        :: ierr                       ! error status (ierr != 0 => error)
CHARACTER*1                                      :: teststring                 ! helpvariable
REAL*4                                           :: r                          ! helpvariable
REAL*4, DIMENSION(:,:), ALLOCATABLE              :: helpgrid

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! name of subroutine
PARAMETER            (ROUTINENAAM = 'read_aps_real')

!-------------------------------------------------------------------------------------------------------------------------------
!
! Open aps file and read header.
!
nfield = 1
CALL read_aps_header(88, filename, gridtitle, floatgrid%gridheader, error)  
IF (error%haserror) GOTO 3000

! Allocate help grid:
nrcol = floatgrid%gridheader%nrcol
nrrow = floatgrid%gridheader%nrrow
! write(*,*)'read_aps1: ',trim(gridtitle),nrcol,nrrow
if (nrcol .le. 0 .or. nrrow .le. 0) then
   call SetError('need positive number of rows and columns in APS header', error)
   goto 2000
else
   ALLOCATE(helpgrid(nrcol,nrrow))
endif

!
! Determine the number of subgrids in the aps-file
!
nfield = 1
1 CONTINUE
DO i=1,nrrow
  READ(88, IOSTAT=ierr) r
ENDDO
READ (88, IOSTAT=ierr) j
IF (ierr.EQ.0) THEN
  nfield = nfield + 1
  GOTO 1
ENDIF
REWIND(88)
!
! Allocate dynamic memory for grid values.
!
nrcol = floatgrid%gridheader%nrcol
nrrow = floatgrid%gridheader%nrrow
! write(*,*)'read_aps2: ',nrcol,nrrow,nfield

ALLOCATE(floatgrid%value(nrcol,nrrow,nfield),STAT=ierr)

IF (ierr.NE.0) THEN
  CALL SetError('Memory allocation error in reading grid data', error)
  GOTO 1000
ENDIF

ALLOCATE(floatgrid%average(nfield),STAT=ierr)

IF (ierr.NE.0) THEN
  CALL SetError('Memory allocation error 2 in reading grid data', error)
  GOTO 1000
ENDIF

DO n = 1,nfield

  READ (88, IOSTAT=ierr) teststring
!  CALL read_aps_header(88, filename, gridtitle, floatgrid%gridheader, error)  
!
! Read float grid values.
!
  DO i=1,nrrow
    READ(88, IOSTAT=ierr) (floatgrid%value(j,i,n), j=1,nrcol)
    IF (ierr.NE.0) THEN
      CALL SetError('Error reading grid data', error)
      CALL ErrorParam('record number', i, error)
      GOTO 1000
    ENDIF
  ENDDO

ENDDO

CALL sysclose(88, filename, error)
IF (error%haserror) GOTO 2000

RETURN
!
! Error handling section, first when memory allocation or reading the gridfile failed
!
1000 CALL ErrorParam('error number', ierr, error)
!
! These parameters are also written when closing the file failed
!
2000 CALL ErrorParam('grid title', gridtitle, error)
CALL ErrorParam('grid dimension nrcol', nrcol, error)
CALL ErrorParam('grid dimension nrrow', nrrow, error)

3000 CALL ErrorParam('filename', filename, error)
CALL ErrorCall(ROUTINENAAM, error)
RETURN

END SUBROUTINE read_aps_real

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   read_aps_integer
! Purpose      Reading of aps file with integer data.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE read_aps_integer(filename, gridtitle, intgrid, error)

!DEC$ ATTRIBUTES DLLEXPORT:: read_aps_integer

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: filename                   ! name of the aps file 
CHARACTER*(*), INTENT(IN)                        :: gridtitle                  ! description of grid shown in error messages
! INPUTS      : filename   (character*(*)). 
!               gridtitle  (character*(*)). 
! OUTPUTS     : gridvalues (type). 
!               error      (TError object). .

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TApsGridInt), INTENT(OUT)                  :: intgrid                    ! grid values read from grid file (float)
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record; assigned when an error occurred

! LOCAL VARIABLES
INTEGER*4                                        :: i                          ! row index
INTEGER*4                                        :: j                          ! column index
INTEGER*4                                        :: n                          ! field index
INTEGER*4                                        :: nfield                     ! number of grid fields
INTEGER*4                                        :: nrcol                      ! number of grid columns
INTEGER*4                                        :: nrrow                      ! number of grid rows
INTEGER*4                                        :: ierr                       ! error status (ierr != 0 => error)
INTEGER*2, DIMENSION(:,:), ALLOCATABLE           :: helpgrid

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! name of subroutine
PARAMETER         (ROUTINENAAM = 'read_aps_integer')
! 
!-------------------------------------------------------------------------------------------------------------------------------
!
! Open aps file and read header.
!
CALL read_aps_header(88, filename, gridtitle, intgrid%gridheader, error)  
nrcol = intgrid%gridheader%nrcol
nrrow = intgrid%gridheader%nrrow
ALLOCATE(helpgrid(nrcol,nrrow))
IF (error%haserror) GOTO 3000
!
! Determine the number of subgrids in the aps-file
!
nfield = 1
1 CONTINUE
DO i=1,nrrow
  READ(88, IOSTAT=ierr) j
ENDDO
READ (88, IOSTAT=ierr) j
IF (ierr.EQ.0) THEN
  nfield = nfield + 1
  GOTO 1
ENDIF
REWIND(88)
!
! Allocate dynamic memory for grid values.
!
nrcol = intgrid%gridheader%nrcol
nrrow = intgrid%gridheader%nrrow

ALLOCATE(intgrid%value(nrcol,nrrow,nfield),STAT=ierr)

IF (ierr.NE.0) THEN
  CALL SetError('Memory allocation error in reading grid data', error)
  GOTO 1000
ENDIF

DO n = 1,nfield

  READ (88, IOSTAT=ierr) j
!  CALL read_aps_header(88, filename, gridtitle, intgrid%gridheader, error)   
!
! Read integer grid values.
!
  DO i=1,nrrow
    READ(88, IOSTAT=ierr) (intgrid%value(j,i,n), j=1,nrcol)
    IF (ierr.NE.0) THEN
      CALL SetError('Error reading grid data', error)
      CALL ErrorParam('record number', i, error)
      GOTO 1000
    ENDIF
  ENDDO

ENDDO

CALL sysclose(88, filename, error)
IF (error%haserror) GOTO 2000

RETURN
!
! Error handling section, first when memory allocation or reading the gridfile failed
!
1000 CALL ErrorParam('filename', filename, error)
CALL ErrorParam('error number', ierr, error)
!
! These parameters are also written when closing the file failed
!
2000 CALL ErrorParam('grid title', gridtitle, error)
CALL ErrorParam('grid dimension nrcol', nrcol, error)
CALL ErrorParam('grid dimension nrrow', nrrow, error)

3000 CALL ErrorCall(ROUTINENAAM, error)
RETURN

END SUBROUTINE read_aps_integer

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   dealloc_aps_real
! Purpose      Deallocation of real grid
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE dealloc_aps_real(realgrid)

!DEC$ ATTRIBUTES DLLEXPORT:: dealloc_aps_real

! SUBROUTINE ARGUMENTS - I/O
TYPE (TApsGridReal), INTENT(INOUT)               :: realgrid                   ! real APS grid

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! name of subroutine
PARAMETER         (ROUTINENAAM = 'dealloc_aps_real')
!-------------------------------------------------------------------------------------------------------------------------------
!
! When allocated this object is now deallocated.
!
IF (ASSOCIATED(realgrid%value)) DEALLOCATE(realgrid%value)
IF (ASSOCIATED(realgrid%average)) DEALLOCATE(realgrid%average)
RETURN

END SUBROUTINE dealloc_aps_real

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   dealloc_aps_integer
! Purpose      Deallocation of integer grid
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE dealloc_aps_integer(intgrid)

!DEC$ ATTRIBUTES DLLEXPORT:: dealloc_aps_integer

! SUBROUTINE ARGUMENTS - I/O
TYPE (TApsGridInt), INTENT(INOUT)                :: intgrid                    ! integer APS grid

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! name of subroutine
PARAMETER         (ROUTINENAAM = 'dealloc_aps_integer')
!-------------------------------------------------------------------------------------------------------------------------------
!
! When allocated this object is now deallocated.
!
IF (ASSOCIATED(intgrid%value)) DEALLOCATE(intgrid%value)
RETURN

END SUBROUTINE dealloc_aps_integer

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   read_aps_header
! Purpose      This routine opens an aps file and reads and assigns the aps header data.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE read_aps_header(fileunit, filename, gridtitle, gridheader, error)

USE m_commonconst_lib, only: EPS_DELTA 

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: fileunit                   ! file unit of APS file
CHARACTER*(*), INTENT(IN)                        :: filename                   ! name of APS file
CHARACTER*(*), INTENT(IN)                        :: gridtitle                  ! description of grid shown in error messages

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TGridHeader), INTENT(OUT)                  :: gridheader                 ! APS grid header
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
CHARACTER*22                                     :: comment                    ! comment in grid header
CHARACTER*10                                     :: kmpnm                      ! component name (parameter name of grid values) 
CHARACTER*10                                     :: eenheid                    ! unit of parameter
CHARACTER*10                                     :: oors                       ! origin of grid values
CHARACTER*6                                      :: form                       ! format which is used to read grid values (?? is this used?)
INTEGER*4                                        :: ij                         ! 
INTEGER*4                                        :: inu1                       ! 
INTEGER*4                                        :: inu2                       ! 
INTEGER*4                                        :: inu3                       ! 
INTEGER*4                                        :: kode                       ! 
INTEGER*4                                        :: ierr                       ! 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! name of subroutine
PARAMETER          (ROUTINENAAM = 'read_aps_header')
!-------------------------------------------------------------------------------------------------------------------------------
!
! Open the fileunit.
!
IF (.NOT. sysopen(fileunit, filename, 'rb', 'aps file', error)) GOTO 9999
!
! Read APS-header:
!
READ(fileunit, IOSTAT = ierr ) ij,inu1,inu2,inu3,kmpnm, eenheid, oors, comment, form, kode, gridheader%xul_corner, &
                &  gridheader%yul_corner, gridheader%nrcol, gridheader%nrrow, gridheader%grixl, gridheader%griyl
! write(*,*) 'APS header 1 ',ij,inu1,inu2,inu3
! write(*,*) 'APS header 2 ',kmpnm, eenheid, oors, comment, form, kode
! write(*,*) 'APS header 3 ',gridheader%xul_corner, gridheader%yul_corner, gridheader%nrcol, gridheader%nrrow, gridheader%grixl, gridheader%griyl

IF (ierr /= 0) THEN
  IF (ierr > 0) THEN
    CALL SetError('Error reading aps grid file header', error)
  ELSE
    CALL SetError('Aps grid file is empty', error)
  ENDIF
  CALL ErrorParam('error nr', ierr, error)
  GOTO 9999
ENDIF

! Check grid reolutions (note: grid dimensions are checked in calling routine):
IF (gridheader%grixl < EPS_DELTA .or. gridheader%griyl < EPS_DELTA) THEN
   CALL SetError('grid resolution must be a positive number;',error)
   CALL ErrorParam('grid resolution in x-direction', gridheader%grixl, error)
   CALL ErrorParam('grid resolution in y-direction', gridheader%griyl, error)
   goto 9999
ENDIF

RETURN

9999 CALL ErrorParam('grid', gridtitle, error)
CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE read_aps_header

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   set_average
! Purpose      See interface definition.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE set_average(factor, grid, fieldnumber)

!DEC$ ATTRIBUTES DLLEXPORT:: set_average

USE m_commonconst_lib                                                            ! EPS_DELTA only

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN), OPTIONAL                  :: factor                     ! multiplication factor for the whole grid

! SUBROUTINE ARGUMENTS - I/O
TYPE (TApsGridReal), INTENT(INOUT)               :: grid                       ! real APS grid

INTEGER, OPTIONAL, INTENT(IN)                    :: fieldnumber                ! fieldnumber to retreive data from

! LOCAL VARIABLES
INTEGER*4                                        :: nrcol                      ! number of grid columns
INTEGER*4                                        :: nrrow                      ! number of grid rows
INTEGER*4                                        :: fn                         ! index of fieldnumber (x,y,fn)
REAL*4                                           :: misval_aps                 ! missing value in APS file

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! name of subroutine
PARAMETER            (ROUTINENAAM = 'set_average')

!-------------------------------------------------------------------------------------------------------------------------------
nrcol = grid%gridheader%nrcol
nrrow = grid%gridheader%nrrow
fn=1
IF (PRESENT(fieldnumber)) fn=fieldnumber

! Get missing value (value of first grid cell if negative):
misval_aps = grid%value(1,1,fn)

IF (PRESENT(factor)) THEN
  IF (misval_aps .lt. 0.0) THEN
      ! data with missing values:
      WHERE (abs(grid%value(:nrcol, :nrrow, fn) - misval_aps) .gt. EPS_DELTA)
         grid%value(:,:,fn) = grid%value(:nrcol, :nrrow, fn) * factor
      ENDWHERE
   ELSE
      ! data without missing values:
      grid%value(:,:,fn) = grid%value(:nrcol, :nrrow, fn) * factor
   ENDIF
ENDIF

! Compute average over non-missing values (each field has its own average):
if (misval_aps .lt. 0.0) then
   grid%average(fn) = SUM(grid%value(:nrcol,:nrrow,fn), abs(grid%value(:nrcol, :nrrow,fn) - misval_aps) > EPS_DELTA) / &
                                                  COUNT(abs(grid%value(:nrcol, :nrrow,fn) - misval_aps) > EPS_DELTA)
ELSE
   grid%average(fn) = SUM(grid%value(:nrcol,:nrrow,fn)) / SIZE(grid%value(:nrcol, :nrrow,fn))
ENDIF

END SUBROUTINE set_average

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   grid_value_integer
! Purpose      See interface definition.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE grid_value_integer(x, y, grid, gridvalue, iscell, fieldnumber)

!DEC$ ATTRIBUTES DLLEXPORT:: grid_value_integer

! SUBROUTINE ARGUMENTS 
REAL*4,    INTENT(IN)                            :: x                          ! RDM x-coordinate value (in km)
REAL*4,    INTENT(IN)                            :: y                          ! RDM y-coordinate value (in km)
TYPE (TAPSGridInt), INTENT(IN)                   :: grid                       ! integer APS grid
INTEGER*4, INTENT(OUT)                           :: gridvalue                  ! the value in the grid cell or the default value 
                                                                               ! (in case of location outside grid)
LOGICAL,   INTENT(OUT)                           :: iscell                     ! whether value comes from a grid cell
INTEGER, OPTIONAL, INTENT(IN)                    :: fieldnumber                ! fieldnumber to retreive data from

! LOCAL VARIABLES
INTEGER*4                                        :: m                          ! grid-index in x-direction of (x,y)
INTEGER*4                                        :: n                          ! grid index in y-direction of (x,y)
INTEGER*4                                        :: fn                         ! index of fieldnumber (x,y,fn)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! name of subroutine
PARAMETER            (ROUTINENAAM = 'grid_value_integer')

!-------------------------------------------------------------------------------------------------------------------------------

fn=1
IF (PRESENT(fieldnumber)) fn=fieldnumber

CALL grid_cell_index(x, y, grid%gridheader, m, n, iscell)

IF (iscell) THEN
  gridvalue = grid%value(m,n,fn)
ELSE
  gridvalue = 0
ENDIF

RETURN

END SUBROUTINE grid_value_integer

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   grid_value_real
! Purpose      See interface definition.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE grid_value_real(x, y, grid, gridvalue, iscell, fieldnumber)

!DEC$ ATTRIBUTES DLLEXPORT:: grid_value_real

! SUBROUTINE ARGUMENTS
REAL*4,    INTENT(IN)                            :: x                          ! RDM x-coordinate value (in km)
REAL*4,    INTENT(IN)                            :: y                          ! RDM y-coordinate value (in km)
TYPE (TAPSGridReal), INTENT(IN)                  :: grid                       ! real APS grid
REAL*4,    INTENT(OUT)                           :: gridvalue                  ! the value in the grid cell or the default value 
                                                                               ! (in case of location outside grid)
LOGICAL,   INTENT(OUT)                           :: iscell                     ! whether value comes from a grid cell
INTEGER, OPTIONAL, INTENT(IN)                    :: fieldnumber                ! fieldnumber to retreive data from

! LOCAL VARIABLES
INTEGER*4                                        :: m                          ! grid-index in x-direction of (x,y)
INTEGER*4                                        :: n                          ! grid index in y-direction of (x,y)
INTEGER*4                                        :: fn                         ! index of fieldnumber (x,y,fn)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! name of subroutine
PARAMETER            (ROUTINENAAM = 'grid_value_real')

!-------------------------------------------------------------------------------------------------------------------------------

fn=1
IF (PRESENT(fieldnumber)) fn=fieldnumber

CALL grid_cell_index(x, y, grid%gridheader, m, n, iscell)

IF (iscell) THEN
  gridvalue = grid%value(m,n,fn)
ELSE
  gridvalue = grid%average(fn)
ENDIF

RETURN

END SUBROUTINE grid_value_real

!-------------------------------------------------------------------------------------------------------------------------------
! Subroutine   grid_cell_index
! Purpose      Returns grid cell indices (m,n) corresponding to given coordinates (x,y)
!              Also returns a flag which says whether the coordinates are inside the grid.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE grid_cell_index(x, y, gridheader, m, n, iscell)

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: x                          ! RDM x-coordinate [km]
REAL*4,    INTENT(IN)                            :: y                          ! RDM y-coordinate [km]
TYPE (TGridHeader), INTENT(IN)                   :: gridheader                 ! Header definition of grid

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: m                          ! x-index of cell
INTEGER*4, INTENT(OUT)                           :: n                          ! y-index of cell
LOGICAL,   INTENT(OUT)                           :: iscell                     ! whether (x,y) is inside grid 

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! name of subroutine
PARAMETER            (ROUTINENAAM = 'grid_cell_index')

!-------------------------------------------------------------------------------------------------------------------------------
! Note: performing a check that grixl > 0 and griyl > 0 here is too costly (routine is called many times);
! resolution > 0 is checked elsewhere.

m = INT(((x - gridheader%xul_corner)/gridheader%grixl) + 1)
n = INT(((gridheader%yul_corner - y)/gridheader%griyl) + 1)

iscell = m >= 1 .AND. n >= 1 .AND. m <= gridheader%nrcol .AND. n <= gridheader%nrrow

RETURN
END SUBROUTINE grid_cell_index

END MODULE m_aps
