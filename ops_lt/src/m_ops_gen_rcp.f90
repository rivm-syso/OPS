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

module m_ops_gen_rcp

IMPLICIT NONE

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! DESCRIPTION        : Generate coordinates of receptor points.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_gen_rcp(spgrid, igrens, masker, grid, nrcol, nrrow, nrrcp, xul_cell_centre, yul_cell_centre, jump, xm, ym, zm, frac, namrcp,      &
                     & lu_rcp_dom_all, z0_rcp_all, lu_rcp_per_user_all, varz, perc, error)

use m_fileutils
use m_aps
use m_error
use m_commonfile, only: fu_recep, namrecept
use m_commonconst_lt, only: EPS_DELTA
use m_commonconst_lib, only: NLU

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_gen_rcp')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: spgrid                      
LOGICAL,   INTENT(IN)                            :: igrens                      
TYPE (TApsGridReal), INTENT(IN)                  :: masker                      
REAL*4,    INTENT(IN)                            :: grid                        
INTEGER*4, INTENT(IN)                            :: nrcol                       
INTEGER*4, INTENT(IN)                            :: nrrow                       
INTEGER*4, INTENT(IN)                            :: nrrcp                      ! number of receptor points
REAL*4,    INTENT(IN)                            :: xul_cell_centre            ! x-coordinate of centre of upper-left grid cell [m] 
REAL*4,    INTENT(IN)                            :: yul_cell_centre            ! y-coordinate of centre of upper-left grid cell [m]  
LOGICAL,   INTENT(IN)                            :: varz                       ! option for variable receptor height                  
LOGICAL,   INTENT(IN)                            :: perc                       ! option for land use percentages from receptor file

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: jump(nrrcp+1)              ! number of grid points to jump to for next point in output            
INTEGER*4, INTENT(OUT)                           :: lu_rcp_dom_all(nrrcp)      ! dominant land use class for each receptor point
REAL*4,    INTENT(OUT)                           :: xm(nrrcp)                  ! x-coordinates of receptors (m RDM)
REAL*4,    INTENT(OUT)                           :: ym(nrrcp)                  ! y-coordinates of receptors (m RDM)
REAL*4,    INTENT(OUT)                           :: zm(nrrcp)                  ! z-coordinates of receptor points (m)
REAL*4,    INTENT(OUT)                           :: frac(nrrcp)                ! fraction of output cell on land surface
REAL*4,    INTENT(OUT)                           :: z0_rcp_all(nrrcp)          ! roughness lengths for all receptors; from z0-map or receptor file [m]
REAL*4,    INTENT(OUT)                           :: lu_rcp_per_user_all(nrrcp,NLU) ! percentage of landuse for all receptors, user defined in receptor file
CHARACTER*(*), INTENT(OUT)                       :: namrcp(nrrcp)              ! receptor names
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: m                          ! column index                           
INTEGER*4                                        :: n                          ! row index
INTEGER*4                                        :: i                          ! index of receptor point
INTEGER*4                                        :: h                          ! number of header lines
INTEGER*4                                        :: j                          ! index of receptor point
INTEGER*4                                        :: lu                         ! index of land use class
INTEGER*4                                        :: lu_dom                     ! dominant land use class at receptor 
INTEGER*4                                        :: nwords                     ! number of expected words in data record 
INTEGER*4                                        :: ix                         ! x coordinate of receptor point (read from file) [m RDM]                          
INTEGER*4                                        :: iy                         ! y coordinate of receptor point (read from file) [m RDM]
REAL*4                                           :: zrcp                       ! z coordinate of receptor point (read from file, default 4 m) [m]
REAL*4                                           :: x_rcp                      ! x coordinate receptor point 
REAL*4                                           :: y_rcp                      ! y coordinate receptor point 
REAL*4                                           :: cellvalue                  ! value of masker grid cell at receptor point
REAL*4                                           :: z0                         ! roughness length read from file [m]
REAL*4                                           :: lu_rcp_per_user(NLU)       ! percentages of landuse classes for this receptor
LOGICAL                                          :: iscell                     ! whether point is inside masker grid
CHARACTER*12                                     :: namrp                      ! name of receptor point
LOGICAL                                          :: eof                        ! end of file has been reached
LOGICAL                                          :: is_data                    ! line read is a data line (not a header line)


!-------------------------------------------------------------------------------------------------------------------------------
! By default calculation height in OPS = 4 m
!
lu_rcp_per_user_all = 0.0
zrcp = 4.0

IF (ANY(spgrid == (/0,1/))) THEN
   
   !-------------------------------------------------------------
   !  Regular grid of receptors, spgrid = 0 or 1
   !-------------------------------------------------------------
   !  Compute the coordinates of the centres of the grid cells. If receptors must be inside NL (igrens = 0), a receptor is only 
   !  accepted, if the area inside NL of the surrounding grid cell > 0.
   !  The area is stored in order to compute area averaged values.
   !
   !  Note: The call to GridValue needs coordinates in km.
   !
   !  Arrays xm, ym and jump are filled per row (nrrow in outer loop). 
   !  jump = number of grid points to jump to for next point in output.
   !  This way jump is assigned the right values for writing output per row, which happens in ops_print_grid.
   !  Note: jump is initialised at 1 in ops_main.
   !
   i = 1
   DO n = 1, nrrow
      DO m = 1, nrcol
         ! Note: (xul_cell_centre, yul_cell_centre) is centre of upper-left grid cell [m] 
         x_rcp = xul_cell_centre + FLOAT(m - 1)*grid
         y_rcp = yul_cell_centre - FLOAT(n - 1)*grid

         IF (spgrid == 0 .AND. (.NOT. igrens) ) THEN
            CALL GridValue(x_rcp/1000, y_rcp/1000, masker, cellvalue, iscell)

            IF (.NOT. iscell .OR. cellvalue <= EPS_DELTA) THEN
               ! point outside NL -> add 1 to jump = number of grid points to jump to for next point in output
               jump(i) = jump(i) + 1
            ELSE
               ! Inside NL -> store coordinates and fraction of area inside NL
               xm(i) = x_rcp
               ym(i) = y_rcp
               frac(i) = cellvalue
               i = i + 1
            ENDIF
         ELSE
            ! Store coordinates; fraction of area = 1
            xm(i) = x_rcp
            ym(i) = y_rcp
            frac(i) = 1.
            i = i + 1
         ENDIF
      ENDDO
   ENDDO
   zm = zrcp

ELSE 
   !-------------------------------------------------------------
   !  User specified receptor points, spgrid = 2, 3
   !-------------------------------------------------------------
   
   IF (.NOT. sysopen(fu_recep, namrecept, 'r', 'receptor file', error)) GOTO 9999

   ! --- Set number of words to read from input line:
   
   ! --- Default behavior; No user defined varz and percentages from standard z0- and lu-grid. (z = 4.0m):
   IF (.NOT.varz .AND. .NOT.perc)  nwords = 4
   
   ! --- user defined varz and percentages from standard z0- and lu-grid:
   IF (     varz .AND. .NOT.perc)  nwords = 5
   
   ! --- No user defined varz, but percentages from rcp-inputfile. (z = 4.0m):
   IF (.NOT.varz .AND.      perc)  nwords = 15
   
   ! --- user defined varz, but percentages from rcp-inputfile:
   IF (     varz .AND.      perc)  nwords = 16
   
   ! Inirialise i = receptor point index = number of data lines read and h = number of header lines:
   i = 0
   h = 0
   is_data = .false.
   eof     = .false.
   
   ! Default missing values:
   z0_rcp_all          = -999.0 
   lu_rcp_dom_all      = -999
   lu_rcp_per_user_all = -999.0
   
   ! Loop over lines in file:
   DO WHILE (.not. eof)
   
      ! Read one record from receptor file:
      call ops_recep_record1(nwords, i, h, is_data, namrp, ix, iy, zrcp, z0, lu_dom, lu_rcp_per_user, eof, error)
      if (error%haserror) goto 9999
      
      IF (.not. eof .and. is_data) THEN
           
         ! Add data for receptor point:
         namrcp(i) = namrp
         xm(i)     = ix
         ym(i)     = iy
         zm(i)     = zrcp
         IF (nwords == 15 .OR. nwords == 16) THEN
           z0_rcp_all(i)     = z0 
           lu_rcp_dom_all(i) = lu_dom
           DO lu = 1,NLU
             lu_rcp_per_user_all(i,lu) = lu_rcp_per_user(lu)
           ENDDO
         ENDIF
         frac(i) = 1.
      ENDIF
   ENDDO  ! Loop over lines in receptor file

   !-------------------------------------------------------------------------
   !  spgrid = 3 -> regular grid, but not necessarily rectangular.
   !-------------------------------------------------------------------------
   !
   ! Example for nrcol = 12, nrrow = 4, nrrcp = 27 (see ./tst/tst_spgrid3.f90)
   ! x = receptor (must be present in receptor file - filled into (xm,ym))
   ! o = point to be skipped
   !
   ! m =     123456789012  
   ! --------------------------------
   ! n = 1 | oooxxxxxxxxx  jump = 4111111111..
   ! n = 2 | ooooxxxxxxoo         5111111..
   ! n = 3 | ooxxxxxxoooo         5111111..
   ! n = 4 | xxxxxxoooooo         51111117
   !
   !  Compute array jump with number of grid points to jump to for next point in output;
   !  jump is filled per row (nrrow in outer loop); this way jump is assigned the right values for 
   !  writing output per row, which happens in ops_print_grid.
   !  Note: jump is initialised at 1 in ops_main.
   !
   IF (spgrid == 3) THEN
      zm = zrcp
      i = 1
      
      ! Loop over rows in a grid:
      DO n = 1, nrrow
      
         ! Loop over columns in this row:
         DO m = 1, nrcol
         
            ! Define grid point in a grid with resolution "grid" and (xul_cell_centre, yul_cell_centre) centre of upper-left grid cell [m]:
            x_rcp = xul_cell_centre + FLOAT(m - 1)*grid
            y_rcp = yul_cell_centre - FLOAT(n - 1)*grid
            
            ! Look for matching coordinates in arrays (xm,ym) which are read from the receptor file:
            DO j = 1, nrrcp
              IF (x_rcp == xm(j) .and. y_rcp == ym(j) ) THEN  
                 i = i + 1
                 goto 100 ! if match has been found -> do not update jump(i)
              ENDIF
            ENDDO
            ! Each time a non-matching coordinate has been found, jump increases with 1:
            jump(i) = jump(i) + 1
100      ENDDO ! loop over columns
         ! first matching coordinate (xm(j),ym(j)) in this row has been found;
         ! jump(i) = 1 + number of non-matching coordinates since last match = number of grid points to jump to for next point in output
      ENDDO ! loop over rows
   ENDIF
ENDIF

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE ops_gen_rcp

!-------------------------------------------------------------------------------------------
subroutine ops_recep_record1(nwords, i, h, is_data, namrp, ix, iy, zrcp, z0, lu_dom, lu_rcp_per_user, eof, error)

! Read one record from the receptor file

use m_commonconst_lib, only: NLU
use m_commonfile, only: fu_recep, namrecept
use m_string, only: string_count_words
use m_error

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4,     INTENT(IN)                        :: nwords                     ! number of expected words in data record 

! SUBROUTINE ARGUMENTS - INPUT/OUTPUT
INTEGER*4,     INTENT(INOUT)                     :: i                          ! index of receptor point = number of data records read
INTEGER*4,     INTENT(INOUT)                     :: h                          ! number of header lines read
LOGICAL,       INTENT(INOUT)                     :: is_data                    ! line read is a data line (not a header line)

! SUBROUTINE ARGUMENTS - OUTPUT
CHARACTER*12,  INTENT(OUT)                       :: namrp                      ! name of receptor point
INTEGER*4,     INTENT(OUT)                       :: ix                         ! x coordinate of receptor point (read from file) [m RDM]                          
INTEGER*4,     INTENT(OUT)                       :: iy                         ! y coordinate of receptor point (read from file) [m RDM]
REAL*4,        INTENT(OUT)                       :: zrcp                       ! z coordinate of receptor point (read from file, default 4 m) [m]
REAL*4,        INTENT(OUT)                       :: z0                         ! roughness length read from file [m]
INTEGER*4,     INTENT(OUT)                       :: lu_dom                     ! dominant land use class at receptor 
REAL*4,        INTENT(OUT)                       :: lu_rcp_per_user(NLU)       ! percentages of landuse classes for this receptor
LOGICAL,       INTENT(OUT)                       :: eof                        ! end of file has been reached
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: p                          ! receptor point number (dummy)
CHARACTER*512                                    :: line                       ! line read from file
INTEGER*4                                        :: ierr                       ! error status
INTEGER*4                                        :: ii                         ! help variable


! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_recep_record1')

! Initialisation:
z0           = -999.0
lu_dom       = -999

! Read line (skip empty lines):
line = ''
eof = .false.
DO WHILE (.not. eof .and. len_trim(line) <= 0) 
   READ (fu_recep,'(a)',IOSTAT=ierr) line
   eof = (ierr .lt. 0)
ENDDO ! {eof or len_trim(line) > 0}

IF (.not. eof) THEN

   ! Check number of words:
   IF (nwords .EQ. string_count_words(line)) THEN
   
      ! Read receptor data from input line:
      IF (nwords .EQ. 4) THEN
        READ (line,*,IOSTAT=ierr) p,namrp,ix,iy
      ELSEIF (nwords .EQ. 5) THEN
        READ (line,*,IOSTAT=ierr) p,namrp,ix,iy,zrcp
      ELSEIF (nwords .EQ. 15) THEN
        READ (line,*,IOSTAT=ierr) p,namrp,ix,iy,z0,lu_dom,(lu_rcp_per_user(ii),ii=1,NLU)
      ELSEIF (nwords .EQ. 16) THEN
        READ (line,*,IOSTAT=ierr) p,namrp,ix,iy,zrcp,z0,lu_dom,(lu_rcp_per_user(ii),ii=1,NLU)
      ENDIF

      ! If line read is is in the data has been successfully there was an error, the line is a header line:
      IF (is_data) THEN
         ! Previous line is a data line; generate error message if an error ocurred (header lines are not allowed in the data section):
         IF (ierr .ne. 0) THEN
            CALL SetError('Incorrect data format while reading receptor file', error)
            CALL ErrorParam('error number', ierr, error)
            CALL ErrorParam('record', line, error)
            GOTO 9999
         ENDIF
      ELSE
         ! Previous line was a header line; this line is a data line if data has been successfully read:
         is_data = (ierr .eq. 0)
      ENDIF
      
      ! Update counter i = index of receptor = number of data lines read so far and h = number of header lines:
      IF (is_data) THEN
        i = i + 1
      ELSE
        h = h + 1
      ENDIF
      
   ELSE
      ! Number of words is incorrect -> error:
      CALL SetError('Error reading receptor file', error)
      CALL ErrorParam('nwords read from rcp-file', string_count_words(line), error)
      CALL ErrorParam('nwords read from rcp-file should be', nwords, error)
      CALL ErrorParam('record number (excluding header)', i + 1, error)
      GOTO 9999
   ENDIF
   
   ! Check z0, land use data:
   IF (is_data .and. (nwords == 15 .or. nwords == 16)) THEN
   
       ! Check z0; see also ops_read_ctr for valid interval for z0:
       IF (z0 .le. 0.0 .or. z0 .gt. 3.0) THEN
          CALL SetError('z0 outside valid range', error)
          CALL ErrorParam('valid range [m]', (/ 0.0, 3.0 /), error)
          CALL ErrorParam('value read from receptor file [m]', z0, error)
          CALL ErrorParam('record number (excluding header)', i, error)
          goto 9999
       ENDIF
         
       ! Check if percentages sum up to ~100 on data line: 
       IF (sum(lu_rcp_per_user(1:NLU)) .lt. 99 .or. sum(lu_rcp_per_user(1:NLU)) .gt. 101) THEN
          CALL SetError('Incorrect input for land use percentages in receptorfile; sum of percentages should be between 99 and 101', error)
          CALL ErrorParam('sum of percentages land use', sum(lu_rcp_per_user(1:NLU)), error)
          CALL ErrorParam('record number (excluding header)', i, error)
          GOTO 9999
       ENDIF
   ENDIF
ENDIF ! end of file

RETURN

9999 CALL ErrorParam('record', line, error)
CALL ErrorParam('filename', namrecept, error)
CALL ErrorCall(ROUTINENAAM, error)
   
end subroutine ops_recep_record1

end module m_ops_gen_rcp
