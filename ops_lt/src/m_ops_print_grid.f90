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
!
! DESCRIPTION         : Print concentration, deposition and other gridded data to print file (= PRNFILE in control file)
!
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_print_grid 

implicit none

contains

SUBROUTINE ops_print_grid (nrrcp, nsubsec, jump, project, icm, gasv, idep, isec, igrid, verb, namco, namsec, nam_pri_sec, coneh, depeh, &
        &  conc_cf, amol21, ugmoldep, nrcol, nrrow, grid, xul_cell_centre, yul_cell_centre, precip, cpri, csec, drydep, wetdep, ddepri,                  &
        &  lu_rcp_dom_all, z0_rcp_all, gemcpri, gemcsec, ccr, gemddep, gemddpri, gemddsec, totddep, ddrpri, ddrsec, gemwdep,       &
        &  gemwdpri, gemwdsec, totwdep, wdrpri, wdrsec, gemprec, gemtdep, tottdep, csubsec, gem_subsec, nam_subsec, totdep,         &
        &  scale_con, scale_sec, scale_subsec, scale_dep, error)

use m_error
use m_commonfile
use m_commonconst_lt
use m_ops_print_kop

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_print_grid')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: nrrcp                      ! number of receptor points
INTEGER,   INTENT(IN)                            :: nsubsec                    ! number of sub-secondary species
INTEGER,   INTENT(IN)                            :: jump(nrrcp+1)              ! number of successive points that can be skipped for output purposes
CHARACTER*(*), INTENT(IN)                        :: project                    ! project name
INTEGER,   INTENT(IN)                            :: icm                        ! component number
LOGICAL,   INTENT(IN)                            :: gasv                       ! fase = gasvormig
LOGICAL,   INTENT(IN)                            :: isec                       ! sec. comp taken into account
LOGICAL,   INTENT(IN)                            :: verb                       ! extra calculations if true
CHARACTER*(*), INTENT(IN)                        :: namco                      ! component name
CHARACTER*(*), INTENT(IN)                        :: nam_pri_sec                     ! 
CHARACTER*(*), INTENT(IN)                        :: coneh                      ! concentration unit
CHARACTER*(*), INTENT(IN)                        :: depeh                      ! deposition unit
REAL,      INTENT(IN)                            :: conc_cf                    ! concentration correction factor
REAL,      INTENT(IN)                            :: amol21                     ! 
REAL,      INTENT(IN)                            :: ugmoldep                   ! 
INTEGER,   INTENT(IN)                            :: nrcol                      ! number of grid cells in X-dir
INTEGER,   INTENT(IN)                            :: nrrow                      ! number of grid cells in Y-dir
REAL,      INTENT(IN)                            :: grid                       ! grid cell dimension
REAL,      INTENT(IN)                            :: xul_cell_centre            ! x-coordinate of centre of upper-left grid cell [m] 
REAL,      INTENT(IN)                            :: yul_cell_centre            ! y-coordinate of centre of upper-left grid cell [m] 
REAL,      INTENT(IN)                            :: precip(nrrcp)              ! total precipitation per year [mm/year]
REAL,      INTENT(IN)                            :: cpri(nrrcp)                ! primary concentration
REAL,      INTENT(IN)                            :: csec(nrrcp)                ! secondary concentration
REAL,      INTENT(IN)                            :: drydep(nrrcp)              ! dry deposition
REAL,      INTENT(IN)                            :: wetdep(nrrcp)              ! wet deposition
REAL,      INTENT(IN)                            :: ddepri(nrrcp)              ! dry depo of primary comp.
INTEGER,   INTENT(IN)                            :: lu_rcp_dom_all(nrrcp)      ! dominant land use class for each receptor point
REAL,      INTENT(IN)                            :: z0_rcp_all(nrrcp)          ! roughness lengths for all receptors; from z0-map or receptor file [m]
REAL,      INTENT(IN)                            :: gemcpri                    ! grid mean for prim. concentration
REAL,      INTENT(IN)                            :: gemcsec                    ! grid mean for sec. concentration
REAL,      INTENT(IN)                            :: ccr                        ! eff. chemical conversion rate
REAL,      INTENT(IN)                            :: gemddep                    ! grid mean for dry deposition
REAL,      INTENT(IN)                            :: gemddpri                   ! grid mean for dry deposition (pri)
REAL,      INTENT(IN)                            :: gemddsec                   ! grid mean for dry deposition (sec)
REAL,      INTENT(IN)                            :: totddep                    ! grid total dry deposition (g/s)
REAL,      INTENT(IN)                            :: ddrpri                     ! eff. dry deposition rate (prim)
REAL,      INTENT(IN)                            :: ddrsec                     ! eff. dry deposition rate (sec)
REAL,      INTENT(IN)                            :: gemwdep                    ! grid mean for wet deposition (tot)
REAL,      INTENT(IN)                            :: gemwdpri                   ! grid mean for wet deposition (pri)
REAL,      INTENT(IN)                            :: gemwdsec                   ! grid mean for wet deposition (sec)
REAL,      INTENT(IN)                            :: totwdep                    ! grid total wet deposition (g/s)
REAL,      INTENT(IN)                            :: wdrpri                     ! effective wet deposition rate (primary component) [%/h]
REAL,      INTENT(IN)                            :: wdrsec                     ! effective wet deposition rate (secondary component) [%/h]
REAL,      INTENT(IN)                            :: gemprec                    ! grid mean annual precpitation from meteo
REAL,      INTENT(IN)                            :: gemtdep                    ! grid mean for total deposition
REAL,      INTENT(IN)                            :: tottdep                    ! grid total total deposition
REAL,      INTENT(IN)                            :: csubsec(nrrcp,nsubsec)     ! concentration of sub-secondary substance [ug/m3]
REAL,      INTENT(IN)                            :: gem_subsec(nsubsec)        ! grid mean for concentration of sub-secondary species [ug/m3]
CHARACTER*(*), INTENT(IN)                        :: nam_subsec(nsubsec)        ! names of sub-secondary species
REAL,      INTENT(IN)                            :: totdep(nrrcp)              ! total deposition
REAL,      INTENT(IN)                            :: scale_con                  ! scalefactor prim. concentration
REAL,      INTENT(IN)                            :: scale_sec                  ! scalefactor sec. concentration
REAL,      INTENT(IN)                            :: scale_subsec(nsubsec)      ! scaling factor for sub-secondary species
REAL,      INTENT(IN)                            :: scale_dep                  ! scalefactor deposition

! SUBROUTINE ARGUMENTS - I/O
LOGICAL,   INTENT(INOUT)                         :: idep                       ! deposition taken into account
LOGICAL,   INTENT(INOUT)                         :: igrid                      ! include receptor values in the Report output; is set with INCLUDE in ctr-file or -v option
CHARACTER*(*), INTENT(INOUT)                     :: namsec                     ! 

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TError), INTENT(INOUT)                     :: error                      ! whether an error occurred

! LOCAL VARIABLES
INTEGER                                          :: j                          ! counter through receptro points
REAL                                             :: tmp(nrrcp)                 ! tempory array with values to be written
INTEGER                                          :: isubsec                    ! index of sub-secondary species


!-------------------------------------------------------------------------------------------------------------------------------
!
! To avoid unlogical combinations
!
IF (verb) igrid = .TRUE.
IF (isec) idep  = .TRUE.
!
! (1) Concentration primary component
!
IF (icm /= icm_PM .or. namco == "PM2.5" ) THEN
  IF (namco == "PM2.5") THEN
    CALL ops_print_kop (project,"PM")
  ELSE
    CALL ops_print_kop (project,namco)
  ENDIF
ENDIF
IF (igrid) THEN
  WRITE (fu_prt, '('' concentration distribution of '', a, '': ('', 1p, e7.0, 1x, a10, '')'')') namco(:LEN_TRIM(namco)),       &
     &  1/scale_con, coneh
  CALL print_mat(fu_prt, cpri, scale_con, nrrcp, jump, nrcol, nrrow, grid, xul_cell_centre, yul_cell_centre, error)
ENDIF

WRITE (fu_prt, '(/,'' average '',a,'' concentration'', T50, '': '', e9.3, 1x, a10)') namco(:LEN_TRIM(namco)), gemcpri, coneh

IF (gasv.and.(icm.ne.0.or.idep)) THEN
  WRITE (fu_prt,'('' eff. chem. conv. rate'', T50, '': '', f9.3, '' %/h'')') ccr
ENDIF
IF (isec) THEN
!
! (1aa) Concentration of second secondary component (only for NOx, icm = icm_NOx)
!
  IF (icm == icm_NOx) THEN
    IF (igrid) THEN
      WRITE (fu_prt, '(a)') char(12)
      CALL ops_print_kop(project, namco)

      do isubsec = 1,nsubsec
         WRITE (fu_prt, '('' concentration distribution of '', a, '': ('', 1p, e7.0, 1x, a5, '')'')')                             &
         &  nam_subsec(isubsec)(:LEN_TRIM(nam_subsec(isubsec))), 1/scale_subsec(isubsec), 'ug/m3'
         CALL print_mat(fu_prt, csubsec(:,isubsec), scale_subsec(isubsec), nrrcp, jump, nrcol, nrrow, grid, xul_cell_centre, yul_cell_centre, error)
      enddo

    ENDIF

    do isubsec = 1,nsubsec
       WRITE (fu_prt, '(/,'' average '',a,'' concentration'', T50, '': '', e9.3, a6)') nam_subsec(isubsec)(:LEN_TRIM(nam_subsec(isubsec))),           &
        &  gem_subsec(isubsec), 'ug/m3'
     enddo
  ENDIF
!
! (1aa) Concentration of secondary component (non-NOx)
!
  IF (verb .or. icm /= icm_NOx) THEN
    IF (igrid) THEN
      WRITE (fu_prt, '(a)') char(12)
      CALL ops_print_kop(project, namco)

      WRITE (fu_prt, '('' concentration distribution of '', a, '': ('', 1p, e7.0, 1x, a5, '')'')')                             &
        &  namsec(:LEN_TRIM(namsec)), 1/scale_sec, 'ug/m3'
      CALL print_mat(fu_prt, csec, scale_sec, nrrcp, jump, nrcol, nrrow, grid, xul_cell_centre, yul_cell_centre, error)

    ENDIF

    WRITE (fu_prt, '(/,'' average '',a,'' concentration'', T50, '': '', e9.3, a6)') namsec(:LEN_TRIM(namsec)), gemcsec,        &
        &  'ug/m3'

    WRITE (fu_prt,'('' eff. '',a,'' > '',a,'' chem. conv. rate'', T50, '': '', f9.3, '' %/h'')') namco(:LEN_TRIM(namco)),      &
        &  namsec(:LEN_TRIM(namsec)), ccr

  ENDIF
ENDIF
!
! (2) Dry deposition
!
IF (idep) THEN
  IF (igrid) THEN
    WRITE (fu_prt, '(a)') char(12)
    CALL ops_print_kop(project, namco)
    WRITE (fu_prt, '( '' dry deposition distribution (as '', a, '')'', '': ('', 1p, e7.0, a10, '')'')')                        &
   &  namsec(:LEN_TRIM(namsec)), 1/scale_dep, depeh
    CALL print_mat(fu_prt, drydep, scale_dep, nrrcp, jump, nrcol, nrrow, grid, xul_cell_centre, yul_cell_centre, error)

  ENDIF
  IF (.NOT.isec) THEN
    WRITE (fu_prt, '(/,'' average dry deposition'', T50, '': '', e9.3, a10)') gemddep, depeh
    WRITE (fu_prt, '('' total dry deposition'', T50, '': '', e9.3, '' g/s'')') totddep
    WRITE (fu_prt, '('' effective dry deposition velocity'', T50, '': '', f9.3, '' cm/s'')') ddrpri
  ELSE
    WRITE (fu_prt, '(/, '' average dry '',a,'' deposition'', '' (as '', a, '')'', T50, '': '', e9.3, a10)')                    &
        &  nam_pri_sec(:LEN_TRIM(nam_pri_sec)), namsec(:LEN_TRIM(namsec)), gemddep, depeh
    WRITE (fu_prt,'('' average dry '',a,'' deposition'', '' (as '', a, '')'', T50, '': '', e9.3, a10)')                        &
        &  namco(:LEN_TRIM(namco)), namsec(:LEN_TRIM(namsec)), gemddpri, depeh
    WRITE (fu_prt,'('' average dry '',a,'' deposition '', ''(as '', a, '')'', T50, '': '', e9.3, a10)')                        &
        &  namsec(:LEN_TRIM(namsec)), namsec(:LEN_TRIM(namsec)), gemddsec, depeh
    WRITE (fu_prt, '('' total dry deposition (as '',a,'')'', T50, '': '', e9.3, '' g/s'')') namco(:LEN_TRIM(namco)), totddep
    WRITE (fu_prt, '('' effective dry deposition velocity '',a, T50, '': '', f9.3, '' cm/s'')') namco(:LEN_TRIM(namco)),       &
        &  ddrpri
    WRITE (fu_prt, '('' effective dry deposition velocity '',a, T50, '': '', f9.3, '' cm/s'')') namsec(:LEN_TRIM(namsec)),     &
        &  ddrsec
  ENDIF
ENDIF
!
! (3) Wet deposition
!
IF (idep) THEN
  IF (igrid) THEN
    WRITE (fu_prt, '(a)') char(12)
    CALL ops_print_kop(project, namco)
    WRITE (fu_prt, '( '' wet deposition distribution (as '', a, ''):'', ''('', 1p, e7.0, a10, '')'')')                         &
        &  namsec(:LEN_TRIM(namsec)), 1/scale_dep, depeh
    CALL print_mat(fu_prt, wetdep, scale_dep, nrrcp, jump, nrcol, nrrow, grid, xul_cell_centre, yul_cell_centre, error)
  ENDIF
  IF (.NOT.isec) THEN
    WRITE (fu_prt, '(/,'' average wet deposition'', T50, '': '', e9.3, a10)') gemwdep, depeh
    WRITE (fu_prt, '('' total wet deposition'', T50, '': '', e9.3, '' g/s'')') totwdep
    WRITE (fu_prt, '('' effective wet deposition rate'', T50, '': '', f9.3, '' %/h'')') wdrpri
    WRITE (fu_prt, '('' annual precipitation amount'', T50, '': '', i9, '' mm'')') NINT(gemprec)
  ELSE
    WRITE (fu_prt, '(/,'' average wet '',a,'' deposition'', '' (as '', a, '')'', T50, '': '', e9.3, a10)')                     &
        &  nam_pri_sec(:LEN_TRIM(nam_pri_sec)), namsec(:LEN_TRIM(namsec)), gemwdep, depeh
    WRITE (fu_prt, '('' average wet '',a,'' deposition'', '' (as '', a, '')'', T50, '': '', e9.3, a10)')                       &
        &  namco(:LEN_TRIM(namco)), namsec(:LEN_TRIM(namsec)), gemwdpri, depeh
    WRITE (fu_prt, '('' average wet '',a,'' deposition'', '' (as '', a, '')'', T50, '': '', e9.3, a10)')                       &
        &  namsec(:LEN_TRIM(namsec)), namsec(:LEN_TRIM(namsec)), gemwdsec, depeh
    WRITE (fu_prt, '('' total wet deposition (as '',a,'')'', T50, '': '', e9.3, '' g/s'')') namco(:LEN_TRIM(namco)), totwdep
    WRITE (fu_prt,'('' effective wet deposition rate '',a, T50, '': '', f9.3, '' %/h'')') namco(:LEN_TRIM(namco)), wdrpri
    WRITE (fu_prt,'('' effective wet deposition rate '',a, T50, '': '', f9.3, '' %/h'')') namsec(:LEN_TRIM(namsec)), wdrsec
    WRITE (fu_prt, '('' annual precipitation amount'', T50, '': '', i9, '' mm'')') NINT(gemprec)
  ENDIF
ENDIF
!
! (4) Total deposition
!
IF (idep) THEN
  IF (igrid) THEN
    WRITE (fu_prt, '(a)') char(12)
    CALL ops_print_kop(project, namco)
    WRITE (fu_prt, '( '' total deposition distribution (as '', a, '')'', '': ('', 1p, e7.0, a10, '')'')')                      &
    &  namsec(:LEN_TRIM(namsec)), 1/scale_dep, depeh
    CALL print_mat(fu_prt, totdep, scale_dep, nrrcp, jump, nrcol, nrrow, grid, xul_cell_centre, yul_cell_centre, error)
  ENDIF
  IF (.NOT.isec) THEN
    WRITE (fu_prt, '(/,'' average deposition'', T50, '': '', e9.3, a10)') gemtdep, depeh
    WRITE (fu_prt, '('' total deposition'', T50, '': '', e9.3, '' g/s'')') tottdep
  ELSE
    WRITE (fu_prt, '(/, '' average '',a,'' deposition'', '' (as '', a, '')'', T50, '': '', e9.3, a10)')                        &
        &  nam_pri_sec(:LEN_TRIM(nam_pri_sec)), namsec(:LEN_TRIM(namsec)), gemtdep, depeh
    WRITE (fu_prt, '('' total deposition     (as '',a,'')'', T50, '': '', e9.3, '' g/s'')') namco(:LEN_TRIM(namco)), tottdep
  ENDIF
ENDIF
IF (verb) THEN
!
! (6) dry deposition velocity (primary) vdpri:
!
  IF (idep) THEN
    WRITE (fu_prt, '(a)') char(12)
    CALL ops_print_kop(project, namco)
    WRITE (fu_prt, '(/,'' effective deposition velocity '', a, '' : ( 1/'', i4, '' cm/s)'')') namco(:LEN_TRIM(namco)), 1000
    tmp(1:nrrcp) = ddepri(:)/ugmoldep* 1.0e5/(cpri(:)/conc_cf*3600)/amol21
    CALL print_mat(fu_prt, tmp, 1., nrrcp, jump, nrcol, nrrow, grid, xul_cell_centre, yul_cell_centre, error)
  ENDIF
!
! (7) dry deposition velocity (secondary) vdsec:
!
  IF (isec) THEN                                                               ! isec --> idep=.TRUE.
    WRITE (fu_prt, '(a)') char(12)
    CALL ops_print_kop(project, namco)
    WRITE (fu_prt, '(/,'' effective deposition velocity '', a, '' : ( 1/'', i4, '' cm/s)'')') namsec(:LEN_TRIM(namsec)),       &
  &  1000
    DO j=1,nrrcp
      IF (csec(j).GT.0.) THEN
        tmp(j) = (drydep(j)-ddepri(j))/ugmoldep*1.0e5/ (csec(j)/conc_cf*3600)/amol21
      ELSE
        tmp(j) = -1.
      ENDIF
    ENDDO
    CALL print_mat(fu_prt, tmp, 1., nrrcp, jump, nrcol, nrrow, grid, xul_cell_centre, yul_cell_centre, error)
  ENDIF
!
! (8) precipitation:
!
  WRITE (fu_prt, '(a)') char(12)
  CALL ops_print_kop(project, namco)

  WRITE (fu_prt, '(/,'' calculated precipitation amount :'', '' (mm)'')')
  CALL print_mat(fu_prt, precip, 1., nrrcp, jump, nrcol, nrrow, grid, xul_cell_centre, yul_cell_centre, error)
!
! (9) roughness length z0:
!
  WRITE (fu_prt, '(a)') char(12)
  CALL ops_print_kop(project, namco)
  WRITE (fu_prt, '(/,'' z0 (1.E-03 m)'')')
  CALL print_mat(fu_prt, z0_rcp_all, 1.E3, nrrcp, jump, nrcol, nrrow, grid, xul_cell_centre, yul_cell_centre, error)
!
! (10) landuse:
!
  IF (isec) THEN
    WRITE (fu_prt, '(a)') char(12)
    CALL ops_print_kop(project, namco)
    WRITE (fu_prt, '(/,'' landuse '')')
    tmp(:)=REAL(lu_rcp_dom_all)
    CALL print_mat(fu_prt, tmp, 1., nrrcp, jump, nrcol, nrrow, grid, xul_cell_centre, yul_cell_centre, error)
  ENDIF
ENDIF                                                                          ! verb
IF (ANY(icm == (/icm_SO2,icm_NOx,icm_NH3/))) namsec=CNAME(icm,2)

RETURN

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE : print_mat
! DESCRIPTION: printing of matrix 
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE print_mat(lun, value, fact, nrrcp, jump, nrcol, nrrow, grid, xul_cell_centre, yul_cell_centre, error)
use m_utils

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'print_mat')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: lun                        ! number of unit to which values are written
REAL,      INTENT(IN)                            :: value(nrrcp)               ! values to be displayed in each receptor point
REAL,      INTENT(IN)                            :: fact                       ! multiplication factor in value displayed
INTEGER,   INTENT(IN)                            :: nrrcp                      ! number of receptor points
INTEGER,   INTENT(IN)                            :: jump(nrrcp+1)              ! number of successive points that can be skipped for output purposes
INTEGER,   INTENT(IN)                            :: nrcol                      ! number of elements displayed on row
INTEGER,   INTENT(IN)                            :: nrrow                      ! number of rows in grid
REAL,      INTENT(IN)                            :: grid                       ! grid size in km
REAL,      INTENT(IN)                            :: xul_cell_centre            ! x-coordinate of centre of upper-left grid cell [m] 
REAL,      INTENT(IN)                            :: yul_cell_centre            ! y-coordinate of centre of upper-left grid cell [m] 

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! Local variables
INTEGER                                          :: j                          ! receptor counter
INTEGER                                          :: m                          ! do loop counter
INTEGER                                          :: pointto                    ! current receptor point index on line
INTEGER                                          :: line(nrcol)                ! value from each row
CHARACTER*80                                     :: formatstring               ! format string in writing

! ---
250 FORMAT (/, ' grid cell dimension  :', f7.3, ' km   ', /,                                                                   &
            &  ' number of grid points:', i4, 'x', i4, /,                                                                      &
            &  ' centre of upper left grid cell :', f9.3, ',', f9.3, ' km') 
!
! Make format string
!
CALL startformat(formatstring, error)
IF (nrrow .LT. 26) THEN                                                        ! could have format with open line between the lines
  CALL appendformat('/', formatstring, error)
ENDIF
CALL appendformat(nrcol, 'i4', formatstring, error)
IF (error%haserror) GOTO 9000

pointto = 0
line(:) = -1                                                                   ! default value
DO j = 1, nrrcp + 1
   pointto = pointto + jump(j)
   DO WHILE (pointto > nrcol)
!
!     This line is written and then reset to defaults. Also adjust pointto
!
      WRITE (lun, formatstring) (line(m), m = 1, nrcol)

      line(:) = -1                                                             ! reset to default value
      pointto = pointto - nrcol
   ENDDO
!
!  The next receptor point value from matr is inserted into the line.
!
   IF (j /= nrrcp + 1) THEN
      line(pointto) = NINT(value(j)*fact)
   ENDIF
ENDDO

WRITE (lun, 250) grid/1000, nrcol, nrrow, xul_cell_centre/1000., yul_cell_centre/1000.

RETURN

9000 CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE print_mat

END SUBROUTINE ops_print_grid

end module m_ops_print_grid 
