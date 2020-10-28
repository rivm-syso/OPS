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
! DESCRIPTION        : Generate coordinates of receptor points.
! EXIT CODES         :
! FILES AND OTHER
!    I/O DEVICES     :
! SYSTEM DEPENDENCIES:
! CALLED FUNCTIONS   :
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_gen_rcp(spgrid, igrens, masker, grid, nrcol, nrrow, nrrcp, xorg, yorg, jump, xm, ym, zm, frac, namrcp,      &
                     & lu_rcp_dom_all, z0_rcp_all, lu_rcp_per_user_all, domlu, varz, perc, error)

USE m_fileutils
USE m_aps
USE m_error
USE m_commonfile
USE m_commonconst                                                              ! EPS_DELTA only

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
REAL*4,    INTENT(IN)                            :: xorg                        
REAL*4,    INTENT(IN)                            :: yorg                        
LOGICAL,   INTENT(IN)                            :: varz                      
LOGICAL,   INTENT(IN)                            :: perc
LOGICAL,   INTENT(IN)                            :: domlu                      

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: jump(nrrcp+1)              
INTEGER*4, INTENT(OUT)                           :: lu_rcp_dom_all(nrrcp)               ! 
REAL*4,    INTENT(OUT)                           :: xm(nrrcp)                  ! x-coordinates
REAL*4,    INTENT(OUT)                           :: ym(nrrcp)                  ! y-coordinates
REAL*4,    INTENT(OUT)                           :: zm(nrrcp)                  ! z-coordinates
REAL*4,    INTENT(OUT)                           :: frac(nrrcp)                ! fraction of output cell on land surface
REAL*4,    INTENT(OUT)                           :: z0_rcp_all(nrrcp)          ! roughness lengths for all receptors; from z0-map or receptor file [m]
INTEGER,   INTENT(OUT)                           :: lu_rcp_per_user_all(nrrcp,NLU) ! percentage of landuse for all receptors, used defined in receptor file
CHARACTER*(*), INTENT(OUT)                       :: namrcp(nrrcp)              ! receptor names
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: m                          ! column index                           
INTEGER*4                                        :: n                          ! row index
INTEGER*4                                        :: i                          ! index of receptor point
INTEGER*4                                        :: h                          ! number of header lines
INTEGER*4                                        :: ii                         ! help variable
INTEGER*4                                        :: j                          ! index of receptor point
INTEGER*4                                        :: lu                         ! index of land use class
INTEGER*4                                        :: lu_dom                     ! landuse
INTEGER*4                                        :: nwords                     ! number of words in string
INTEGER*4                                        :: check_nwords               ! number of words in string
INTEGER*4                                        :: ix                         ! x coordinate of receptor point (read from file)                          
INTEGER*4                                        :: iy                         ! y coordinate of receptor point (read from file) 
REAL*4                                           :: zrcp                       ! z coordinate of receptor point (read from file) 
INTEGER*4                                        :: p                          ! receptor point number (dummy)
INTEGER*4                                        :: ierr                       ! error status
REAL*4                                           :: x_rcp                      ! x coordinate receptor point 
REAL*4                                           :: y_rcp                      ! y coordinate receptor point 
REAL*4                                           :: cellvalue                  ! value of masker grid cell at receptor point
REAL*4                                           :: z0                         ! 
INTEGER                                          :: lu_rcp_per_user(NLU)       ! percentages of landuse classes for this receptor
LOGICAL                                          :: iscell                     ! whether point is inside masker grid
CHARACTER*12                                     :: namrp                      ! name of receptor point
CHARACTER*512                                    :: string                     ! 
CHARACTER*512                                    :: tmpstring                  ! 

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)

!-------------------------------------------------------------------------------------------------------------------------------
! By default calculation height in OPS = 4 m
!
lu_rcp_per_user_all = 0
zrcp = 4.0
!
IF (ANY(spgrid == (/0,1/))) THEN
!
!  Regular grid of receptors.
!  Compute the coordinates of the centres of the grid cells. If receptors must be inside NL (igrens = 0), a receptor is only 
!  accepted, if the area inside NL of the surrounding grid cell > 0.
!  The area is stored in order to compute area averaged values.
!
!  Note: The call to GridValue needs coordinates in km.
!
!  Arrays xm, ym and jump are filled per row (nrrow in outer loop). 
!  This way jump is assigned the right values for writing output per row, which happens in ops_print_grid
!
   i = 1
   DO n = 1, nrrow
      DO m = 1, nrcol
         x_rcp = xorg + FLOAT(m - 1)*grid
         y_rcp = yorg - FLOAT(n - 1)*grid

         IF (spgrid == 0 .AND. (.NOT. igrens) ) THEN
            CALL GridValue(x_rcp/1000, y_rcp/1000, masker, cellvalue, iscell)
            IF (.NOT. iscell .OR. cellvalue <= EPS_DELTA) THEN
               ! point outside NL -> add 1 to jump = number of successive points that are outside NL
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
! User specified receptor points, spgrid = 2, 3
ELSE 

  IF (.NOT. sysopen(fu_recep, namrecept, 'r', 'receptor file', error)) GOTO 9999
!
! --- Skip header if present ---
!
  ierr = 1
!
! --- What to read from each line:
!
! --- Default behavior:
! --- No user defined varz and percentages from standard z0- and lu-grid. (z = 4.0m)
  IF (.NOT.varz .AND. .NOT.perc)  nwords = 4
! --- user defined varz and percentages from standard z0- and lu-grid.
  IF (     varz .AND. .NOT.perc)  nwords = 5
! --- No user defined varz, but percentages from rcp-inputfile. (z = 4.0m).
  IF (.NOT.varz .AND.      perc)  nwords = 15
! --- user defined varz, but percentages from rcp-inputfile.
  IF (     varz .AND.      perc)  nwords = 16
!
  ! Inirialise number of data lines (i) and number of header lines (h):
  i = 0
  h = 0
  
  ! Loop over lines until an valid data line (ierr = 0) has been detected:
  DO WHILE (ierr.GT.0)
    z0=0
    lu_dom=0    
    check_nwords = 0
    
    ! Read line:
    READ (fu_recep,'(a)',IOSTAT=ierr) string

    ! Split line into words:
    IF (string(1:1) .ne. char(32) .and. string(1:1) .ne. char(9)) check_nwords = check_nwords + 1
    DO WHILE (len_trim(string) .ne. 1)
      DO WHILE (string(1:1) .ne. char(32) .and. string(1:1) .ne. char(9))
        tmpstring=string(2:len_trim(string))
        string=tmpstring  
        IF (len_trim(string) .eq. 0) goto 323
      ENDDO 
      DO WHILE (string(1:1) .eq. char(32) .or. string(1:1) .eq. char(9))
        tmpstring=string(2:len_trim(string))
        string=tmpstring
      IF (len_trim(string) .eq. 0) goto 323
      ENDDO
      check_nwords = check_nwords + 1   
    ENDDO
    
323 BACKSPACE(fu_recep)

    ! Check number of words and check whether we have a valid line with data:
    IF (nwords .EQ. check_nwords) THEN
      IF (nwords .EQ. 4) THEN
        READ (fu_recep,*,IOSTAT=ierr) p,namrp,ix,iy
      ELSEIF (nwords .EQ. 5) THEN
        READ (fu_recep,*,IOSTAT=ierr) p,namrp,ix,iy,zrcp
      ELSEIF (nwords .EQ. 15) THEN
        READ (fu_recep,*,IOSTAT=ierr) p,namrp,ix,iy,z0,lu_dom,(lu_rcp_per_user(ii),ii=1,NLU)
      ELSEIF (nwords .EQ. 16) THEN
        READ (fu_recep,*,IOSTAT=ierr) p,namrp,ix,iy,zrcp,z0,lu_dom,(lu_rcp_per_user(ii),ii=1,NLU)
      ENDIF
      
!     Update counter for number of data lines (i) and number of header lines (h):
      IF (ierr == 0) THEN
        i = i + 1
      ELSE
        h = h + 1
      ENDIF
      
    ELSE
      ! number of words in header line or first data line must be correct; if not -> error:
      CALL SetError('Error reading receptor file', error)
      CALL ErrorParam('filename', namrecept, error)
      CALL ErrorParam('record number', i + h, error)
      CALL ErrorParam('nwords read from rcp-file should be:', nwords, error)
      GOTO 9999
    ENDIF
    
    ! Check data on data line: 
    IF (ierr == 0) THEN
       IF (z0 > 0 .and. (nwords == 15 .or. nwords == 16) .and.  &
          ( sum(lu_rcp_per_user(1:NLU)) .lt. 99 .or. sum(lu_rcp_per_user(1:NLU)) .gt. 101)) THEN
          CALL SetError('INPUT ERROR: No correct input in receptorfile', error)
          CALL ErrorParam('filename', namrecept, error)
          CALL ErrorParam('record number', i + h, error)
          CALL ErrorParam('sum of percentages should be 99, 100 or 101, found: ', sum(lu_rcp_per_user(1:NLU)), error)
          GOTO 9999
       ENDIF
    ENDIF
  ENDDO
  
! Loop until end-of-file and read rest of data lines:
  DO WHILE (ierr.EQ.0)
!
!   Add next receptor point
!
    namrcp(i) = namrp
    xm(i) = ix
    ym(i) = iy
    zm(i) = zrcp
    IF (nwords == 15 .OR. nwords == 16) THEN
      z0_rcp_all(i)=z0 
      lu_rcp_dom_all(i)=lu_dom
      DO lu = 1,NLU
        lu_rcp_per_user_all(i,lu)=lu_rcp_per_user(lu)
      ENDDO
    ENDIF

    frac(i) = 1.
!
!   Read next line. If end-of-file the next round ierr will detect so.
!   There will be exactly nrrcp lines assigned.
!
    READ (fu_recep,'(a)',IOSTAT=ierr) string
    IF (ierr.EQ.0) THEN
!
! Get number of words in string
!
      z0=0
      lu_dom=0
      check_nwords = 0
      IF (string(1:1) .ne. char(32) .and. string(1:1) .ne. char(9)) check_nwords = check_nwords + 1
      DO WHILE (len_trim(string) .ne. 1)
        DO WHILE (string(1:1) .ne. char(32) .and. string(1:1) .ne. char(9))
          tmpstring=string(2:len_trim(string))
          string=tmpstring  
          IF (len_trim(string) .eq. 0) goto 321
        ENDDO 
        DO WHILE (string(1:1) .eq. char(32) .or. string(1:1) .eq. char(9))
          tmpstring=string(2:len_trim(string))
          string=tmpstring
          IF (len_trim(string) .eq. 0) goto 321
        ENDDO
        check_nwords = check_nwords + 1   
      ENDDO
321   BACKSPACE(fu_recep)

      ! Read data from data line:
      IF (nwords .EQ. check_nwords) THEN
        IF (nwords .EQ. 4) THEN
          READ (fu_recep,*,IOSTAT=ierr) p,namrp,ix,iy
        ELSEIF (nwords .EQ. 5) THEN
          READ (fu_recep,*,IOSTAT=ierr) p,namrp,ix,iy,zrcp
        ELSEIF (nwords .EQ. 15) THEN
          READ (fu_recep,*,IOSTAT=ierr) p,namrp,ix,iy,z0,lu_dom,(lu_rcp_per_user(ii),ii=1,NLU)
        ELSEIF (nwords .EQ. 16) THEN
          READ (fu_recep,*,IOSTAT=ierr) p,namrp,ix,iy,zrcp,z0,lu_dom,(lu_rcp_per_user(ii),ii=1,NLU)
        ENDIF
      ELSE
        CALL SetError('INPUT ERROR: No consistent input in receptorfile', error)
        CALL ErrorParam('filename', namrecept, error)
        CALL ErrorParam('record number', i + h, error)
        CALL ErrorParam('nwords read from rcp-file should be:', nwords, error)
        GOTO 9999
      ENDIF
      IF (z0 > 0.0 .AND. ierr == 0 .AND. (nwords == 15 .or. nwords == 16) .AND.  &
         ( sum(lu_rcp_per_user(1:NLU)) .lt. 99 .or. sum(lu_rcp_per_user(1:NLU)) .gt. 101)) THEN
        CALL SetError('INPUT ERROR: No correct input in receptorfile', error)
        CALL ErrorParam('filename', namrecept, error)
        CALL ErrorParam('record number', i + h, error)
        CALL ErrorParam('sum of percentages should be 99, 100 or 101, found: ', sum(lu_rcp_per_user(1:NLU)), error)
        GOTO 9999
      ENDIF
    ENDIF
   IF (ierr == 0) i = i + 1
  ENDDO
 
!  spgrid = 3 -> regular grid, but not necessarily rectangular.
!  Cordinates have already been read above, array jump (number of successive points that can be skipped for output purposes)
!  is filled per row (nrrow in outer loop). 
!  This way jump is assigned the right values for writing output per row, which happens in ops_print_grid
!
  IF (spgrid == 3) THEN
    zm = zrcp
    i = 1
    DO n = 1, nrrow
      DO m = 1, nrcol
        x_rcp = xorg + FLOAT(m - 1)*grid
        y_rcp = yorg - FLOAT(n - 1)*grid
        DO j = 1, nrrcp
          IF (x_rcp == xm(j) .and. y_rcp == ym(j) ) THEN  
             i = i + 1
             goto 100
          ENDIF
        ENDDO
        jump(i) = jump(i) + 1
100   ENDDO
    ENDDO
  ENDIF
ENDIF

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE ops_gen_rcp
