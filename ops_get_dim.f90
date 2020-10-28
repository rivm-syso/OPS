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
!                       Copyright by
!   National Institute of Public Health and Environment
!           Laboratory for Air Research (RIVM/LLO)
!                      The Netherlands
!   No part of this software may be used, copied or distributed without permission of RIVM/LLO (2002)
!
! SUBROUTINE
! FILENAME           : %M%
! SCCS(SOURCE)       : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH -SEQUENCE   : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : OPS-support
! FIRM/INSTITUTE     : RIVM/LLO/IS
! LANGUAGE           : FORTRAN(HP-UX, HP-F77, HP-F90)
! DESCRIPTION        : Calculation of dimension of receptor point grids.
! EXIT CODES         :
! FILES AND OTHER
!    I/O DEVICES     :
! SYSTEM DEPENDENCIES:
! CALLED FUNCTIONS   :
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_get_dim(spgrid, igrens, xc, yc, grid, nrcol, nrrow, nrrcp, xorg, yorg, masker, error)

USE m_aps
USE m_fileutils
USE m_error
USE m_commonconst                                                              ! EPS_DELTA only
USE m_commonfile

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER    (ROUTINENAAM = 'ops_get_dim')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: spgrid
LOGICAL,   INTENT(IN)                            :: igrens
REAL*4,    INTENT(IN)                            :: xc
REAL*4,    INTENT(IN)                            :: yc
REAL*4,    INTENT(IN)                            :: grid

! SUBROUTINE ARGUMENTS - I/O
INTEGER*4, INTENT(INOUT)                         :: nrcol                      ! number of colums in grid
INTEGER*4, INTENT(INOUT)                         :: nrrow                      ! number of rows in grid

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: nrrcp                      ! number of receptor points
REAL*4,    INTENT(OUT)                           :: xorg
REAL*4,    INTENT(OUT)                           :: yorg
TYPE (TApsGridReal), INTENT(OUT)                 :: masker
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
REAL*4,    PARAMETER                             :: GRID_XSTART = 0.000        ! x-coordinate of left upper corner point of NL grid
REAL*4,    PARAMETER                             :: GRID_YSTART = 620000.000   ! y-coordinate of left upper corner point of NL grid
REAL*4,    PARAMETER                             :: NL_XLEFT    = 13562.623
REAL*4,    PARAMETER                             :: NL_XRIGHT   = 278018.313
REAL*4,    PARAMETER                             :: NL_YUPPER   = 619122.750
REAL*4,    PARAMETER                             :: NL_YLOWER   = 306838.813

! LOCAL VARIABLES
INTEGER*4                                        :: i                          ! grid index
INTEGER*4                                        :: m                          ! column index
INTEGER*4                                        :: n                          ! row index
real                                             :: ix                         ! x coordinate of receptor point (read from file)
real                                             :: iy                         ! y coordinate of receptor point (read from file)
INTEGER*4                                        :: p                          ! receptor point number (dummy)
INTEGER*4                                        :: ierr                       ! error status
REAL*4                                           :: lower
REAL*4                                           :: xmax                       ! maximum x coordinate of receptor points
REAL*4                                           :: xmax2
REAL*4                                           :: xmin                       ! minimum x coordinate of receptor points
REAL*4                                           :: ymax                       ! maximum y coordinate of receptor points
REAL*4                                           :: ymax2
REAL*4                                           :: ymin                       ! minimum y coordinate of receptor points
REAL*4                                           :: x_rcp                      ! x coordinate receptor point
REAL*4                                           :: y_rcp                      ! y coordinate receptor point
REAL*4                                           :: cellvalue                  ! value of masker grid cell at receptor point
LOGICAL                                          :: iscell                     ! whether point is inside masker grid
CHARACTER*12                                     :: namrp                      ! name of receptor point

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida
sccsida = '%W%:%E%'//char(0)

!
! Compute origin of grid and number of grid columns and rows for the NL grid.
! Note: grid origin is here defined as the centre of the left-upper grid cell.
!
IF (ANY(spgrid == (/0,1/))) THEN

   ! Standard NL grid
   IF (spgrid == 0) THEN

      ! Start from GRID_XSTART, move in steps of grid = grid resolution and get first index, such that x >= NL_XLEFT;
      ! xorg is grid/2 to the left of this point.
      i=1
      DO WHILE (GRID_XSTART + i*grid < NL_XLEFT)
        i=i+1
      ENDDO


      xorg = (i-1)*grid + 0.5*grid

      ! Start from GRID_YSTART, move in steps of grid = grid resolution and get first index, such that y <= NL_YUPPER;
      ! yorg is grid/2 above this point.
      i=1
      DO WHILE (GRID_YSTART - i*grid > NL_YUPPER)
        i=i+1
      ENDDO
      yorg= GRID_YSTART - (i-1)*grid -0.5*grid

      ! Compute number of columns to reach NL_XRIGHT
      nrcol=1
      DO WHILE ((xorg - 0.5*grid) + nrcol*grid <= NL_XRIGHT)
        nrcol=nrcol+1
      ENDDO

      ! Compute number of rows to reach NL_YLOWER
      nrrow=1
      DO WHILE ((yorg + 0.5*grid) - nrrow*grid >= NL_YLOWER)
        nrrow=nrrow+1
      ENDDO

   ! User defined grid; (xc,yc) nrcol and nrrow have been read from the control file
   ELSE IF (spgrid == 1) THEN
      xorg = xc - (FLOAT(nrcol - 1))/2.*grid
      yorg = yc + (FLOAT(nrrow - 1))/2.*grid
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
           x_rcp = xorg + FLOAT(m - 1)*grid
           y_rcp = yorg - FLOAT(n - 1)*grid
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
  DO WHILE (ierr.GT.0)
    READ (fu_recep,*,IOSTAT=ierr) p,namrp,ix,iy
  ENDDO
  BACKSPACE (fu_recep)
!
! Count number of receptor points nrrcp
!
  nrrcp = 0
  DO WHILE (ierr.EQ.0)
!
!   Read next line. If end-of-file the next round ierr will detect so.
!
    READ (fu_recep,*,IOSTAT=ierr) p,namrp,ix,iy
!
!   Update counter for number of receptorpoints and determine maximum and minimum of x and y coordinates of receptor points
!
    IF (ierr.EQ.0) THEN
      nrrcp= nrrcp + 1
      IF (ix > xmax) xmax = ix
      IF (ix < xmin) xmin = ix
      IF (iy > ymax) ymax = iy
      IF (iy < ymin) ymin = iy
    ENDIF

  ENDDO
!
! Check whether an error in reading the file occurred.
!
  IF (ierr .GT. 0) THEN
    CALL SetError('Error reading receptor file', error)
    CALL ErrorParam('filename', namrecept, error)
    CALL ErrorParam('error number', ierr, error)
    CALL ErrorParam('record number', nrrcp, error)
    GOTO 9999
  ENDIF
!
! Determine xorg, yorg, nrcol, nrow and grid if receptorpoints are on a raster.
!
  IF (spgrid == 3) THEN
    xorg  = xmin
    yorg  = ymax
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
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER    (ROUTINENAAM = 'gen_mask')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: grid

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TApsGridReal), INTENT(OUT)                 :: maskergrid                 ! APS-grid with fraction of area inside NL for each grid cell
TYPE (TError), INTENT(OUT)                       :: error                      ! error record

! LOCAL VARIABLES
TYPE (TApsGridInt)                               :: basisgrid                  ! base mask read from file;
                                                                               ! grid with a fixed resolution, with 0 (outside NL), 1 (inside NL)
INTEGER*4                                        :: ibasis                     ! column index of base grid
INTEGER*4                                        :: jbasis                     ! row index of output mask grid
INTEGER*4                                        :: imask                      ! column index of output mask grid
INTEGER*4                                        :: jmask                      ! row index of base grid
INTEGER*4                                        :: factor                     ! ratio (output mask grid resolution) : (base mask resolution)
INTEGER*4                                        :: land                       ! sum of 1's of base grid that lie inside a certain output mask grid cell
INTEGER*4                                        :: nrcol                      ! number of columns in output mask grid
INTEGER*4                                        :: nrrow                      ! number of rows in output mask grid
REAL*4                                           :: outputres                  ! resolution of output mask grid [km]
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
