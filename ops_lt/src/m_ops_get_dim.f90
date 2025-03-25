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
! DESCRIPTION        : Calculation of dimension of receptor point grids.
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_get_dim

implicit none

contains

SUBROUTINE ops_get_dim(spgrid, igrens, xc, yc, grid, nrcol, nrrow, nrrcp, xul_cell_centre, yul_cell_centre, masker, error)

use m_aps
USE m_fileutils
USE m_error
USE m_commonconst_lt                                                              ! EPS_DELTA only
USE m_commonfile

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_get_dim')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: spgrid                      
LOGICAL,   INTENT(IN)                            :: igrens                      
REAL,      INTENT(IN)                            :: xc                          
REAL,      INTENT(IN)                            :: yc                          
REAL,      INTENT(IN)                            :: grid                        

! SUBROUTINE ARGUMENTS - I/O
INTEGER,   INTENT(INOUT)                         :: nrcol                      ! number of colums in grid
INTEGER,   INTENT(INOUT)                         :: nrrow                      ! number of rows in grid

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER,   INTENT(OUT)                           :: nrrcp                      ! number of receptor points
REAL,      INTENT(OUT)                           :: xul_cell_centre            ! x-coordinate of centre of upper-left grid cell [m] 
REAL,      INTENT(OUT)                           :: yul_cell_centre            ! y-coordinate of centre of upper-left grid cell [m] 
TYPE (TApsGridReal), INTENT(OUT)                 :: masker                      
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES
REAL,      PARAMETER                             :: GRID_XSTART = 0.000        ! x-coordinate of upper-left corner point of NL grid [m]
REAL,      PARAMETER                             :: GRID_YSTART = 620000.000   ! y-coordinate of upper-left corner point of NL grid [m]
REAL,      PARAMETER                             :: NL_XLEFT    = 13562.623    !  
REAL,      PARAMETER                             :: NL_XRIGHT   = 278018.313   ! 
REAL,      PARAMETER                             :: NL_YUPPER   = 619122.750   ! 
REAL,      PARAMETER                             :: NL_YLOWER   = 306838.813   ! 

! LOCAL VARIABLES
INTEGER                                          :: i                          ! grid index                           
INTEGER                                          :: m                          ! column index                           
INTEGER                                          :: n                          ! row index
INTEGER                                          :: p                          ! receptor point number (dummy)
INTEGER                                          :: ierr                       ! error status
REAL                                             :: xmax                       ! maximum x coordinate of receptor points
REAL                                             :: xmin                       ! minimum x coordinate of receptor points
REAL                                             :: ymax                       ! maximum y coordinate of receptor points
REAL                                             :: ymin                       ! minimum y coordinate of receptor points
REAL                                             :: x_rcp                      ! x coordinate receptor point 
REAL                                             :: y_rcp                      ! y coordinate receptor point 
REAL                                             :: cellvalue                  ! value of masker grid cell at receptor point
LOGICAL                                          :: iscell                     ! whether point is inside masker grid
CHARACTER*12                                     :: namrp                      ! name of receptor point
REAL   :: rx, ry
integer :: nheader, iheader
character(len=8) :: line
line = ''
!
! Compute centre of the upper-left grid cell [m] and number of grid columns and rows for the NL grid.
!
IF (ANY(spgrid == (/0,1/))) THEN

   ! Standard NL grid
   IF (spgrid == 0) THEN

      ! Start from GRID_XSTART, move in steps of grid = grid resolution and get first index, such that x >= NL_XLEFT;
      ! xul_cell_centre is grid/2 to the left of this point.
      i=1
      DO WHILE (GRID_XSTART + i*grid < NL_XLEFT)
        i=i+1
      ENDDO
      
      
      xul_cell_centre = (i-1)*grid + 0.5*grid

      ! Start from GRID_YSTART, move in steps of grid = grid resolution and get first index, such that y <= NL_YUPPER;
      ! yul_cell_centre is grid/2 above this point.
      i=1
      DO WHILE (GRID_YSTART - i*grid > NL_YUPPER)
        i=i+1
      ENDDO
      yul_cell_centre = GRID_YSTART - (i-1)*grid -0.5*grid

      ! Compute number of columns to reach NL_XRIGHT
      nrcol=1
      DO WHILE ((xul_cell_centre - 0.5*grid) + nrcol*grid <= NL_XRIGHT)
        nrcol=nrcol+1
      ENDDO

      ! Compute number of rows to reach NL_YLOWER
      nrrow=1
      DO WHILE ((yul_cell_centre + 0.5*grid) - nrrow*grid >= NL_YLOWER)
        nrrow=nrrow+1
      ENDDO

   ! User defined grid; grid centre (xc,yc) nrcol and nrrow have been read from the control file.
   ! xul_cell_centre                  = x-coordinate of centre of upper-left grid cell [m]
   ! xul_cell_centre + (nrcol-1)*grid = x-coordinate of centre of upper-right grid cell [m]
   ! xc = (1/2)*(xul_cell_centre + xul_cell_centre + (nrcol-1)*grid) <=> xc = xul_cell_centre + (1/2)*(nrcol-1)*grid)
   ELSE IF (spgrid == 1) THEN
      xul_cell_centre = xc - (FLOAT(nrcol - 1))/2.*grid
      yul_cell_centre = yc + (FLOAT(nrrow - 1))/2.*grid
   ENDIF
!
!  Determine nrrcp (number of receptors) from nrcol and nrrow. Only points inside the possible mask are taken.
!
   IF (spgrid == 0 .AND. (.NOT. igrens)) THEN
!
!     In this case (grid with receptor points inside NL), a mask is generated with the current grid resolution, that fits NL;
!     "masker" is an APS-grid (type TApsGridReal) with the requested grid resolution, which contains the fraction of NL area
!     within a grid cell. 
!
      CALL gen_mask(grid, masker, error)
      IF (error%haserror) GOTO 9999

      ! count number of receptor points (grid centre) that are inside NL (area in NL > 0).
      ! Note: The call to GridValue needs coordinates in km.
      nrrcp = 0
      DO m = 1, nrcol
         DO n = 1, nrrow
           ! Note: (xul_cell_centre,yul_cell_centre) is centre of upper-left grid cell 
           x_rcp = xul_cell_centre + FLOAT(m - 1)*grid
           y_rcp = yul_cell_centre - FLOAT(n - 1)*grid
           CALL GridValue(x_rcp/1000, y_rcp/1000, masker, cellvalue, iscell)
           IF (iscell .AND. cellvalue > EPS_DELTA) THEN
             nrrcp = nrrcp + 1
           ENDIF
         ENDDO
      ENDDO
   ELSE
      ! Other rectangular grids
      nrrcp = nrcol * nrrow
   ENDIF

! User specified receptor points, spgrid = 2, 3
ELSE                                                                           
!
! --- Read receptor file for first time, just to see how many records it contains ---
!     It's data are assigned the second time, which is in ops_gen_rcp.
!
  IF (.NOT. sysopen(fu_recep, namrecept, 'r', 'receptor file', error)) GOTO 9999
!
! Initialise maximum and minimum of x and y coordinates of receptor points
!
  xmax = tiny(xmax)
  xmin = huge(xmin)
  ymax = tiny(ymax)
  ymin = huge(xmin)
!
! --- Skip header if present ---
!
  ierr = 1
  nheader = 0
  do 
    READ (fu_recep,*,IOSTAT=ierr) p,namrp,rx,ry
    if (ierr<=0) exit
    nheader = nheader + 1
  ENDDO
  CALL sysclose(fu_recep, 'receptor file', error)
  if (error%haserror) goto 9999

  IF (.NOT. sysopen(fu_recep, namrecept, 'r', 'receptor file', error)) GOTO 9999
  do iheader = 1,nheader
     read(fu_recep,'(a)') line
  end do
!
! Count number of receptor points nrrcp
!
  nrrcp = 0
  ierr = 0
  DO WHILE (ierr.EQ.0)
!
!   Read next line. If end-of-file the next round ierr will detect so.
!
    READ (fu_recep,*,IOSTAT=ierr) p,namrp,rx,ry
!
!   Update counter for number of receptorpoints and determine maximum and minimum of x and y coordinates of receptor points
!
    IF (ierr.EQ.0) THEN
      nrrcp = nrrcp + 1
      IF (rx > xmax) xmax = rx
      IF (rx < xmin) xmin = rx
      IF (ry > ymax) ymax = ry
      IF (ry < ymin) ymin = ry
    ENDIF

  ENDDO
!
! Check whether an error in reading the file occurred.
!
  IF (ierr .GT. 0) THEN
    CALL SetError('Incorrect data format while reading receptor file', error)
    CALL ErrorParam('filename', namrecept, error)
    CALL ErrorParam('error number', ierr, error)
    CALL ErrorParam('record number', nrrcp+1, error)
    GOTO 9999
  ENDIF
!
! Determine xul_cell_centre, yul_cell_centre, nrcol, nrow and grid if receptorpoints are on a raster.
! (xmin,ymin), (xmax,ymax), (xmin,ymax) and (xmax,ymin) are centres of corner grid cells, so (xul_cell_centre,yul_cell_centre) is centre of upper-left grid cell 
!
  IF (spgrid == 3) THEN
    xul_cell_centre  = xmin
    yul_cell_centre  = ymax
    nrcol = abs(xmax - xmin)/grid + 1
    nrrow = abs(ymax - ymin)/grid + 1

    IF (nrrow.GT.MAXROW) THEN
      CALL SetError('Error in number of rows of receptorgrid', error)
      CALL ErrorParam('MAXROW', MAXROW, error)
      CALL ErrorParam('nrrow', nrrow, error)
      GOTO 9999
    ENDIF
    IF (nrcol.GT.MAXCOL) THEN
      CALL SetError('Error in number of colums of receptorgrid', error)
      CALL ErrorParam('MAXCOL', MAXCOL, error)
      CALL ErrorParam('nrcol', nrcol, error)
      GOTO 9999
    ENDIF
  ENDIF

  CALL sysclose(fu_recep, namrecept, error)
  IF (error%haserror) GOTO 9999

ENDIF

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE  gen_mask(grid, maskergrid, error)
!
!     Generate a mask with the current grid resolution, that fits NL;
!     "masker" is an APS-grid (type TApsGridReal) with the requested grid resolution, that contains the percentage of NL area
!     within a grid cell.
!-------------------------------------------------------------------------------------------------------------------------------

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'gen_mask')

! SUBROUTINE ARGUMENTS - INPUT
REAL,      INTENT(IN)                            :: grid                       

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TApsGridReal), INTENT(OUT)                 :: maskergrid                 ! APS-grid with fraction of area inside NL for each grid cell
TYPE (TError), INTENT(INOUT)                     :: error                      ! error record

! LOCAL VARIABLES
TYPE (TApsGridInt)                               :: basisgrid                  ! base mask read from file;
                                                                               ! grid with a fixed resolution, with 0 (outside NL), 1 (inside NL)
INTEGER                                          :: ibasis                     ! column index of base grid
INTEGER                                          :: jbasis                     ! row index of output mask grid
INTEGER                                          :: imask                      ! column index of output mask grid
INTEGER                                          :: jmask                      ! row index of base grid
INTEGER                                          :: factor                     ! ratio (output mask grid resolution) : (base mask resolution)
INTEGER                                          :: land                       ! sum of 1's of base grid that lie inside a certain output mask grid cell
INTEGER                                          :: nrcol                      ! number of columns in output mask grid
INTEGER                                          :: nrrow                      ! number of rows in output mask grid
REAL                                             :: outputres                  ! resolution of output mask grid [km]
CHARACTER*1                                      :: gridname                   ! denotes direction 'x' or 'y' where error occurred when checking
                                                                               ! for grid resolution conformity 

!-------------------------------------------------------------------------------------------------------------------------------
!
! Read base mask for NL
!
CALL ReadAps(masknam, 'Basismasker NL', basisgrid, error)

IF (error%haserror) GOTO 9999
!
! Check whether the output grid resolution is an N times the resolution of the base grid (N integer). 
! If not, generate an error.
! Note: better to check this when reading the control file. 
!

! First convert output resolution to km 
outputres = grid /1000.

! Check ratio (output resolution) : (base grid resolution) and jump to error section if not ok
IF ( MOD(outputres,basisgrid%gridheader%grixl) /= 0 .or. MOD(outputres, basisgrid%gridheader%griyl) /= 0 ) GOTO 2000
!
! Check is oke; generate mask for the requested output resolution.
!
factor = outputres/basisgrid%gridheader%grixl

!
! Initialise maskergrid; current settings imply that maskergrid%gridheader%grixl = maskergrid%gridheader%griyl
!
maskergrid%gridheader       = basisgrid%gridheader
maskergrid%gridheader%grixl = outputres
maskergrid%gridheader%griyl = outputres

!
! Number of colums and rows of output mask.
!
IF (MOD(basisgrid%gridheader%nrcol,factor) /= 0) THEN
  nrcol = INT(basisgrid%gridheader%nrcol / factor) + 1
ELSE
  nrcol = INT(basisgrid%gridheader%nrcol / factor)
ENDIF
IF (MOD(basisgrid%gridheader%nrrow,factor) /= 0) THEN
  nrrow = INT(basisgrid%gridheader%nrrow / factor) + 1
ELSE
  nrrow = INT(basisgrid%gridheader%nrrow / factor)
ENDIF
maskergrid%gridheader%nrcol  = nrcol
maskergrid%gridheader%nrrow  = nrrow

!
! Allocate memory for maskergrid.
!
ALLOCATE (maskergrid%value(nrcol,nrrow,1))

! Loop over output mask grid and compute fraction of area NL inside grid cell, 
! i.e. the average of 0's and 1's of base grid that lie inside an output mask grid cell.
! Note that the accuracy of this area fraction depends on the ratio of the grid resolutions of
! the output mask grid and the base grid.
!
IF ( factor /= 1 ) THEN
  DO jmask = 1,nrrow
    DO imask = 1,nrcol
      land = 0
      DO jbasis = ((jmask-1)*factor)+1,factor*jmask
        DO ibasis = ((imask-1)*factor)+1,factor*imask
          IF (ibasis <= basisgrid%gridheader%nrcol .AND. jbasis <= basisgrid%gridheader%nrrow) THEN
            IF (basisgrid%value(ibasis,jbasis,1) == 1) land=land+1
          ENDIF
        ENDDO
      ENDDO
      maskergrid%value(imask,jmask,1)=FLOAT(land)/FLOAT(factor**2)
    ENDDO
  ENDDO
ELSE
  ! If output mask grid resolution = base grid resolution, the value of maskergrid = 0 or 1
  maskergrid%value = FLOAT(basisgrid%value)
ENDIF

DEALLOCATE (basisgrid%value)
RETURN

!
! Error section
!
2000 CALL SetError('Output resolution not a whole number multiple of', 'basegrid resolution', error)

IF ( MOD(outputres,basisgrid%gridheader%grixl) /= 0 ) THEN
  gridname = 'X'
ELSE
  gridname = 'Y'
ENDIF

CALL ErrorParam('error in grid:', gridname, error )
CALL ErrorParam('outputres', outputres, error )
CALL ErrorParam('base grixl', basisgrid%gridheader%grixl, error )
CALL ErrorParam('base griyl', basisgrid%gridheader%griyl, error )

9999 CALL ErrorCall(ROUTINENAAM, error)

RETURN

END SUBROUTINE gen_mask

END SUBROUTINE ops_get_dim

end module m_ops_get_dim
