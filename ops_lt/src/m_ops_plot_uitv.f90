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
! DESCRIPTION        : Write results to plot-file (= PLTFILE in control file)
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_plot_uitv

implicit none

contains

SUBROUTINE ops_plot_uitv(spgrid, isec, coneh, nrrcp, nsubsec, jump, xul_cell_centre, yul_cell_centre, nrcol, nrrow, grid, idep, namco, nam_pri_sec, namsec,        &
                      & depeh, namrcp, xm, ym, cpri, csec, drydep, ddepri, wdepri, wetdep, cno2, cnox, icm, csubsec, nam_subsec, &
                      & nparout, parout_val, parout_name, parout_unit, parout_write, error)

use m_error
use m_commonfile
use m_fileutils
use m_commonconst_lt

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: spgrid                     ! 
LOGICAL,   INTENT(IN)                            :: isec                       ! 
CHARACTER*(*), INTENT(IN)                        :: coneh                      ! 
INTEGER*4, INTENT(IN)                            :: nrrcp                      ! 
INTEGER*4, INTENT(IN)                            :: nsubsec                    ! number of sub-secondary species
INTEGER*4, INTENT(IN)                            :: jump(nrrcp+1)              ! number of successive points that can be skipped for output purposes
REAL*4,    INTENT(IN)                            :: xul_cell_centre            ! x-coordinate of centre of upper-left grid cell [m]
REAL*4,    INTENT(IN)                            :: yul_cell_centre            ! y-coordinate of centre of upper-left grid cell [m]
INTEGER*4, INTENT(IN)                            :: nrcol                      ! number of columns in grid
INTEGER*4, INTENT(IN)                            :: nrrow                      ! number of row in grid
REAL*4,    INTENT(IN)                            :: grid                       ! 
LOGICAL,   INTENT(IN)                            :: idep                       ! 
CHARACTER*(*), INTENT(IN)                        :: namco                      ! 
CHARACTER*(*), INTENT(IN)                        :: nam_pri_sec                ! name of primary + secondary component (SOx, NOy, NHy)
CHARACTER*(*), INTENT(IN)                        :: namsec                     ! 
CHARACTER*(*), INTENT(IN)                        :: depeh                      ! 
CHARACTER*(*), INTENT(IN)                        :: namrcp(nrrcp)              ! 
REAL*4,    INTENT(IN)                            :: xm(nrrcp)                  ! x-coordinates of receptors (m RDM)
REAL*4,    INTENT(IN)                            :: ym(nrrcp)                  ! y-coordinates of receptors (m RDM)
REAL*4,    INTENT(IN)                            :: cpri(nrrcp)                ! 
REAL*4,    INTENT(IN)                            :: csec(nrrcp)                ! 
REAL*4,    INTENT(IN)                            :: drydep(nrrcp)              ! 
REAL*4,    INTENT(IN)                            :: ddepri(nrrcp)              ! 
REAL*4,    INTENT(IN)                            :: wdepri(nrrcp)              ! 
REAL*4,    INTENT(IN)                            :: wetdep(nrrcp)              ! 
REAL*4,    INTENT(IN)                            :: cno2(nrrcp)                ! NO2 concentration (derived from NOx and parameterised ratio NO2/NOx)
REAL*4,    INTENT(IN)                            :: cnox(nrrcp)                ! NOx concentration, saved from previous iteration
INTEGER*4, INTENT(IN)                            :: icm                        ! 
REAL*4,    INTENT(IN)                            :: csubsec(nrrcp,nsubsec)     ! concentration of sub-secondary species [ug/m3]
CHARACTER*(*), INTENT(IN)                        :: nam_subsec(nsubsec)        ! names of sub-secondary species
INTEGER*4, INTENT(IN)                            :: nparout                    ! number of extra output parameters (besides concentration, deposition)
REAL*4,    INTENT(IN)                            :: parout_val(nparout,nrrcp)  ! values for extra output parameters [nparout,nrrcp]
CHARACTER(len=*), INTENT(IN)                     :: parout_name(nparout)       ! names of extra output parameters                      
CHARACTER(len=*), INTENT(IN)                     :: parout_unit(nparout)       ! units of extra output parameters                      
LOGICAL                                          :: parout_write               ! write parout parameters to output

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: ierr                       ! 
INTEGER*4                                        :: ls                         ! lengte textstring nam_pri_sec
INTEGER*4                                        :: j                          ! 
REAL*4                                           :: xul_corner                 ! x-coordinate of upper left corner of grid [km]
REAL*4                                           :: yul_corner                 ! y-coordinate of upper left corner of grid [km]
REAL*4                                           :: totdep(nrrcp)              ! 
INTEGER*4                                        :: isubsec                    ! index of sub-secondary species
INTEGER*4                                        :: iparout                    ! index of extra output parameter
CHARACTER*12                                     :: str1                       ! help string for header
CHARACTER*12                                     :: str2                       ! help string for header
CHARACTER*12                                     :: str3                       ! help string for header
CHARACTER*12                                     :: str4                       ! help string for header
REAL*4                                           :: tmp_rcp(nrrcp)             ! temporary array [nrrcp]

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_plot_uitv')

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Open plot file
!
!IF (.NOT. sysopen(fu_plt, pltnam, 'w', 'plot file', error)) GOTO 9999

IF (spgrid .EQ. 2) THEN

!-------------------------------------------------------
! Tabulated output for non-gridded receptors
!-------------------------------------------------------

  IF (isec) THEN
      
      ! Acidifying components
      
      ! Define help strings for NO2 concentration:
      IF (icm .eq. 2) THEN
         str1 = 'conc.'
         str2 = 'NO2_ROADS'
         str3 = 'ug/m3'
         str4 = 'NOx_ROADS'
      ELSE
         str1 = ''
         str2 = ''
         str3 = ''
         str4 = ''
      ENDIF

      WRITE (fu_plt, '(a4,8x,a8,a8,15a12:)', IOSTAT = ierr) 'name', 'x-coord', 'y-coord', 'conc.', 'dry_dep.', 'wet_dep.',       &
          &  'tot_dep.', ('conc.', isubsec = 1,nsubsec+1), 'dry_dep.', 'dry_dep.', 'wet_dep.', 'wet_dep.',trim(str1),trim(str1)
      IF (ierr .GT. 0) GOTO 4200

      ls = LEN_TRIM(nam_pri_sec)
      WRITE (fu_plt, '(a4,8x,a8,a8,15a12:)', IOSTAT = ierr) '-', '-', '-', namco(:LEN_TRIM(namco)), nam_pri_sec(:ls), nam_pri_sec(:ls), nam_pri_sec(:ls),              &
            &  namsec(:LEN_TRIM(namsec)), (nam_subsec(isubsec)(:LEN_TRIM(nam_subsec(isubsec))), isubsec = 1,nsubsec), & 
            &  namco(:LEN_TRIM(namco)), namsec(:LEN_TRIM(namsec)), namco(:LEN_TRIM(namco)), namsec(:LEN_TRIM(namsec)),trim(str2),trim(str4)
      IF (ierr .GT. 0) GOTO 4200

      WRITE (fu_plt, '(a4,8x,a8,a8,15a12:)', IOSTAT = ierr) '-','m', 'm', coneh, depeh, depeh, depeh, ('ug/m3', isubsec = 1,nsubsec+1), & 
                                                            depeh, depeh, depeh, depeh, trim(str3),trim(str3)

      IF (ierr .GT. 0) GOTO 4200

     IF (icm .eq. 2) THEN
        ! NOx, NO2 included:
        DO j = 1, nrrcp
           WRITE (fu_plt, '(a12,2i8,20e12.4)', IOSTAT = ierr) namrcp(j), NINT(xm(j)), NINT(ym(j)), cpri(j), drydep(j),  &
                     &  wetdep(j), drydep(j) + wetdep(j), csec(j), (csubsec(j,isubsec), isubsec = 1,nsubsec),           &
                     &  ddepri(j), drydep(j)-ddepri(j), wdepri(j), wetdep(j)-wdepri(j), cno2(j), cnox(j)
           IF (ierr .GT. 0) GOTO 4200
        ENDDO
     ELSE
        ! Acidifying species, not NOx:
        DO j = 1, nrrcp
           WRITE (fu_plt, '(a12,2i8,13e12.4)', IOSTAT = ierr) namrcp(j), NINT(xm(j)), NINT(ym(j)), cpri(j), drydep(j),  &
                     &  wetdep(j), drydep(j) + wetdep(j), csec(j), (csubsec(j,isubsec), isubsec = 1,nsubsec),           &
                     &  ddepri(j), drydep(j)-ddepri(j), wdepri(j), wetdep(j)-wdepri(j)
           IF (ierr .GT. 0) GOTO 4200
        ENDDO
     ENDIF

  ELSE IF (idep) THEN
!
!   Non-acidifying substances, depositions (idep .and. .not. isec)
!
    WRITE (fu_plt, '(a4,8x,a8,a8,5a12)', IOSTAT = ierr) 'name', 'x-coord', 'y-coord', 'conc.', 'dry_dep.', 'wet_dep.',         &
          &  'tot_dep.'
    IF (ierr .GT. 0) GOTO 4200

    WRITE (fu_plt, '(a4,8x,a8,a8,5a12:)', IOSTAT = ierr) '-', '-', '-', namco(:5), nam_pri_sec(:5), nam_pri_sec(:5), nam_pri_sec(:5)
    IF (ierr .GT. 0) GOTO 4200

    WRITE (fu_plt, '(a4,8x,a8,a8,5a12:)', IOSTAT = ierr) '-', 'm', 'm', coneh, depeh, depeh, depeh
    IF (ierr .GT. 0) GOTO 4200

    DO j = 1, nrrcp
      WRITE (fu_plt, '(a12,2i8,5e12.4)', IOSTAT = ierr) namrcp(j), NINT(xm(j)), NINT(ym(j)), cpri(j), drydep(j), wetdep(j),    &
                  &  drydep(j) + wetdep(j)
      IF (ierr .GT. 0) GOTO 4200
    ENDDO

  ELSE
!
!   Non-acidifying substances AND no depositions (.not.idep .and. .not. isec)
!
    WRITE (fu_plt, '(a4,8x,a8,a8,a12)', IOSTAT = ierr) 'name', 'x-coord', 'y-coord', 'conc.'
    IF (ierr .GT. 0) GOTO 4200

    WRITE (fu_plt, '(a4,8x,a8,a8,a12:)', IOSTAT = ierr) '-', '-', '-', namco(:5)
    IF (ierr .GT. 0) GOTO 4200

     WRITE (fu_plt, '(a4,8x,a8,a8,a12:)', IOSTAT = ierr) '-', 'm', 'm', coneh
    IF (ierr .GT. 0) GOTO 4200

     DO j = 1, nrrcp
       WRITE (fu_plt, '(a12,2i8,e12.4)', IOSTAT = ierr) namrcp(j), NINT(xm(j)), NINT(ym(j)), cpri(j)
       IF (ierr .GT. 0) GOTO 4200
    ENDDO
  ENDIF
  WRITE (fu_plt, '(a1)', IOSTAT = ierr) ' '
ELSE
  
!-------------------------------------------------------
! Grid output (APS format) for gridded receptors
!-------------------------------------------------------


  ! Set (xul_corner,yul_corner) = coordinates of upper-left corner of grid, which are used in the definition of the APS-file [km].
  ! note: (xul_cell_centre,yul_cell_centre) is the centre of the upper-left grid cell [m]
  xul_corner = (xul_cell_centre - grid/2.)/1000.
  yul_corner = (yul_cell_centre + grid/2.)/1000.

  ! Primary concentration:
  CALL plot_mat(fu_plt, cpri, nrrcp, jump, nrcol, nrrow, 'concentration         ', namco, coneh, grid, xul_corner, yul_corner, error)
  IF (error%haserror) GOTO 9000

  ! Depositions (dry, wet, total):
  IF (idep) THEN
    CALL plot_mat(fu_plt, drydep, nrrcp, jump, nrcol, nrrow, 'dry_deposition        ', nam_pri_sec, depeh, grid, xul_corner, yul_corner, error)
    IF (error%haserror) GOTO 9000

    CALL plot_mat(fu_plt, wetdep, nrrcp, jump, nrcol, nrrow, 'wet_deposition        ', nam_pri_sec, depeh, grid, xul_corner, yul_corner, error)
    IF (error%haserror) GOTO 9000

    totdep(:nrrcp) = drydep(:) + wetdep(:)
    CALL plot_mat(fu_plt, totdep, nrrcp, jump, nrcol, nrrow, 'total_deposition      ', nam_pri_sec, depeh, grid, xul_corner, yul_corner, error)
    IF (error%haserror) GOTO 9000
  ENDIF

  ! If there is at least one secondary component:
  IF (isec) THEN

     ! Print secondary concentration of secondary component:
    CALL plot_mat(fu_plt, csec, nrrcp, jump, nrcol, nrrow, 'conctr._sec._component', namsec, 'ug/m3', grid, xul_corner, yul_corner, error)
    IF (error%haserror) GOTO 9000

    ! Sub-secondary concentration (NOx only, icm = 2):
    IF (icm == 2) THEN
       DO isubsec = 1,nsubsec
          CALL plot_mat(fu_plt, csubsec(:,isubsec), nrrcp, jump, nrcol, nrrow, trim(nam_subsec(isubsec))//'_concentration', nam_subsec(isubsec), 'ug/m3', grid, xul_corner, yul_corner,    &
                        &  error)
          IF (error%haserror) GOTO 9000
       ENDDO
    ENDIF

    ! Print dry and wet deposition for primary and secondary component:
    CALL plot_mat(fu_plt, ddepri,  nrrcp, jump, nrcol, nrrow, 'drydep._pri._component', namco , depeh  , grid, xul_corner, yul_corner, error)
    IF (error%haserror) GOTO 9000
    tmp_rcp = drydep-ddepri
    CALL plot_mat(fu_plt, tmp_rcp, nrrcp, jump, nrcol, nrrow, 'drydep._sec._component', namsec, depeh  , grid, xul_corner, yul_corner, error)
    IF (error%haserror) GOTO 9000
    CALL plot_mat(fu_plt, wdepri,  nrrcp, jump, nrcol, nrrow, 'wetdep._pri._component', namco , depeh  , grid, xul_corner, yul_corner, error)
    IF (error%haserror) GOTO 9000
    tmp_rcp = wetdep-wdepri
    CALL plot_mat(fu_plt, tmp_rcp, nrrcp, jump, nrcol, nrrow, 'wetdep._sec._component', namsec, depeh  , grid, xul_corner, yul_corner, error)
    IF (error%haserror) GOTO 9000
  ENDIF
  
  ! Write extra output parameters:
  IF (parout_write) THEN
     DO iparout = 1,nparout
        tmp_rcp = parout_val(iparout,:)
        CALL plot_mat(fu_plt, tmp_rcp, nrrcp, jump, nrcol, nrrow, parout_name(iparout), '-', parout_unit(iparout), grid, xul_corner, yul_corner, error)
        IF (error%haserror) GOTO 9000
     ENDDO
  ENDIF
ENDIF

!
! Close plot file
!
!CALL sysclose(fu_plt, pltnam,  error)

RETURN

4200 CALL SetError('Error writing plot output', error)
CALL ErrorParam('ierr', ierr, error)

9000 CALL ErrorParam('spgrid', spgrid, error)
CALL ErrorParam('isec', isec, error)
CALL ErrorParam('idep', idep, error)

9999 CALL ErrorCall(ROUTINENAAM, error)
CALL sysclose(fu_plt, pltnam,  error)
RETURN

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE : plot_mat
! DESCRIPTION: plotting of matrix
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE plot_mat(lun, value, nrrcp, jump, nrcol, nrrow, descco, compname, compunit, grid, xul_corner, yul_corner, error)

use m_commonconst_lt
use m_utils

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'plot_mat')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: lun                        ! number of unit to which values are written
REAL,      INTENT(IN)                            :: value(nrrcp)               ! values to be displayed in each receptor point
INTEGER,   INTENT(IN)                            :: nrrcp                      ! number of receptor points
INTEGER*4, INTENT(IN)                            :: jump(nrrcp+1)              ! number of successive points that can be skipped for output purposes
INTEGER,   INTENT(IN)                            :: nrcol                      ! number of elements displayed on row
INTEGER,   INTENT(IN)                            :: nrrow                      ! number of rows in grid
CHARACTER*(*), INTENT(IN)                        :: descco                     ! description of component
CHARACTER*(*), INTENT(IN)                        :: compname                   ! name of component
CHARACTER*(*), INTENT(IN)                        :: compunit                   ! component unit
REAL,          INTENT(IN)                        :: grid                       ! grid size in km
REAL*4,        INTENT(IN)                        :: xul_corner                 ! x-coordinate of upper left corner of grid [km]
REAL*4,        INTENT(IN)                        :: yul_corner                 ! y-coordinate of upper left corner of grid [km]

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! Local variables
INTEGER                                          :: j                          ! receptor counter
INTEGER                                          :: m                          ! do loop counter
INTEGER                                          :: ierr                       ! error number
INTEGER                                          :: pointto                    ! current receptor point index on line
REAL*4                                           :: line(nrcol)                ! value from each row
CHARACTER*80                                     :: formatstring               ! format string in writing
CHARACTER*10                                     :: OPSVERSIE                  ! format string in writing
! ---
450 FORMAT (4i3, 3(1x, a10), 1x, a22, 1x, a6, 1x, i2, 2(1x, f8.3), 2i3, 2(1x, f8.3))
460 FORMAT (4i3, 3(1x, a10), 1x, a22, 1x, a6, 1x, i2, 2(1x, f8.3), 2i4, 2(1x, f8.3))

#ifndef UNIX
  OPSVERSIE(1:2) = 'W-'
#else
  OPSVERSIE(1:2) = 'L-'
#endif
OPSVERSIE(3:10) = MODVERSIE(1:8)

IF (nrcol .LE. 999 .AND. nrrow .LE. 999) THEN
  WRITE (lun, 450, IOSTAT = ierr) 0, 0, 0, 0, compname, compunit, OPSVERSIE, descco, 'e12.4 ', 1, xul_corner, yul_corner, nrcol, nrrow,        &
               &  grid/1000, grid/1000
ELSE
  WRITE (lun, 460, IOSTAT = ierr) 0, 0, 0, 0, compname, compunit, OPSVERSIE, descco, 'e12.4 ', 1, xul_corner, yul_corner, nrcol, nrrow,        &
               &  grid/1000, grid/1000
ENDIF

IF (ierr .GT. 0) GOTO 1000
!
! Make format string for writing rows of nrcol columns
!
CALL startformat(formatstring, error)
! CALL appendformat(nrcol, 'es11.3', formatstring, error)
CALL appendformat(nrcol, 'e12.4', formatstring, error)
IF (error%haserror) GOTO 9999

pointto = 0
line(:) = -1.E+38                                                                 ! default value (indicating a missing value)
DO j = 1, nrrcp + 1
   pointto = pointto + jump(j)
   DO WHILE (pointto > nrcol)
!
!     This line is written and then reset to defaults. Also adjust pointto
!
      WRITE (lun, formatstring, IOSTAT = ierr) (line(m), m = 1, nrcol)
      IF (ierr .NE. 0) GOTO 1000

      line(:) = -1.E+38                                                            ! reset to default value
      pointto = pointto - nrcol
   ENDDO
!
!  The next receptor point value from matr is inserted into the line.
!
   IF ( j /= nrrcp + 1 ) THEN
      line(pointto) = value(j)
   ENDIF
ENDDO

RETURN

1000 CALL SetError('Error writing matrix to plot file', error)
CALL ErrorParam('ierr', ierr, error)

9999 CALL ErrorParam('Writing parameter', descco, error)
CALL ErrorCall(ROUTINENAAM, error)
RETURN

END SUBROUTINE plot_mat

END SUBROUTINE ops_plot_uitv

end module m_ops_plot_uitv
