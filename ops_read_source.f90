!-------------------------------------------------------------------------------------------------------------------------------
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
!                       Copyright (C) 2002 by
!   National Institute of Public Health and Environment
!           Laboratory for Air Research (RIVM/LLO)
!                      The Netherlands
!
! SUBROUTINE
!
! NAME                  : %M%
! SCCS (SOURCE)         : %P%
! RELEASE - LEVEL       : %R% - %L%
! BRANCH -SEQUENCE      : %B% - %S%
! DATE - TIME           : %E% - %U%
! WHAT                  : %W%:%E%
! AUTHOR                :
! FIRM/INSTITUTE        : RIVM/LLO
! LANGUAGE              : FORTRAN-77/90
! DESCRIPTION           : Read source file with emissions.
!                         Emissions are read from a source file and emissions for selected emission categories and countries 
!                         are then copied to a scratch file (line for line);
!                         emission parameters that lie outside a specified range are fixed at the lower or upper limit of this range.
!                         If this occurs, a warning is written to a log file.
! EXIT CODES            :
! FILES AND OTHER       :
!   I/O DEVICES
! SYSTEM DEPENDENCIES   : HP-Fortran
! CALLED FUNCTIONS      : flrs, ops_check, sysread
! UPDATE HISTORY        :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_read_source(icm, gasv, ncatsel, catsel, nlandsel, landsel, presentcode, spgrid, grid, numbron, error)

USE m_error
USE m_fileutils
USE m_geoutils
USE m_commonfile
USE m_commonconst                                                              ! EPS_DELTA only

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                 
PARAMETER    (ROUTINENAAM = 'ops_read_source')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: icm                        ! component nummer
LOGICAL,   INTENT(IN)                            :: gasv                       
INTEGER*4, INTENT(IN)                            :: ncatsel                    
INTEGER*4, INTENT(IN)                            :: catsel(*)                  
INTEGER*4, INTENT(IN)                            :: nlandsel                   
INTEGER*4, INTENT(IN)                            :: landsel(*)                 
LOGICAL,   INTENT(IN)                            :: presentcode(MAXDIST,4)     
INTEGER*4, INTENT(IN)                            :: spgrid                     
REAL,      INTENT(IN)                            :: grid                       

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: numbron                    ! number of (selected) sources
TYPE (TError), INTENT(OUT)                       :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: ibtg                       ! diurnal emission variation code read from emission record
INTEGER*4                                        :: nrec                       ! record number of source file
INTEGER*4                                        :: mm                         ! source identification number
INTEGER*4                                        :: iland                      ! country/area code read from emission record
INTEGER*4                                        :: idgr                       ! particle size distribution code read from emission record
INTEGER*4                                        :: cattel                     ! index for selected emission category
INTEGER*4                                        :: landtel                    ! index for selected country
INTEGER*4                                        :: ibroncat                   ! emission category code read from emission record
INTEGER*4                                        :: ierr                       ! error value
LOGICAL*4                                        :: end_of_file                ! end of file has been reached
LOGICAL*4                                        :: end_of_info                ! end of info has been reached
LOGICAL*4                                        :: new_brnfile                ! new version of emission file
REAL*4                                           :: qob                         
REAL*4                                           :: qww                         
REAL*4                                           :: hbron                       
REAL*4                                           :: diameter                    
REAL*4                                           :: szopp                       
REAL*4                                           :: brn_version                ! version of emission inputfile       
REAL*4                                           :: x                          ! x coordinate of source location (RDM [m])                 
REAL*4                                           :: y                          ! y coordinate of source location (RDM [m])
REAL*4                                           :: gl                         ! x coordinate of source location (longitude [degrees])                 
REAL*4                                           :: gb                         ! y coordinate of source location (latitude [degrees])                 
CHARACTER*180                                    :: cbuf                       ! character buffer, used to store an emission record
CHARACTER*180                                    :: word                       ! sting read from character buffer

!-------------------------------------------------------------------------------------------------------------------------------
50 FORMAT (i4, 2f9.0, es10.3, f7.3, f6.1, f8.0, f6.1, 4i4)
100 FORMAT (i4, 2f8.3, e10.3, f7.3, f6.1, f7.0, f6.1, 4i4)
150 FORMAT (i4, 2f8.0, e10.3, f7.3, f6.1, f7.0, f6.1, 4i4)

numbron     = 0
nrec        = 0
new_brnfile = .FALSE.
end_of_file = .FALSE.
end_of_info = .FALSE.
!
! First check the version of brnfile
!
CALL sysread(fu_bron, cbuf, end_of_info, error)
IF (error%haserror) GOTO 9999
!
! If first character is "!" and (on first line) BRN-VERSION = 1 we have a new-brnfile
!
IF (cbuf(1:1) .EQ. "!") THEN
  READ (cbuf(2:len_trim(cbuf)),*,end=33,err=33) word, brn_version
  IF (word .EQ. "BRN-VERSION" .AND. brn_version .EQ. 1) new_brnfile=.TRUE.
  33 continue
  DO WHILE (.NOT. end_of_info)
    CALL sysread(fu_bron, cbuf, end_of_info, error)
    IF (error%haserror) GOTO 9999
    IF (cbuf(1:1) .NE. "!") THEN
      end_of_info = .TRUE.
    ENDIF
  ENDDO
ENDIF
!
! This is the first real emission record so we backspace 1 line
!  
backspace(fu_bron)
!
! Read source file until end of file in order to check for errors.
!  
DO WHILE (.NOT. end_of_file)

  !
  ! Read string cbuf from file 
  !
  CALL sysread(fu_bron, cbuf, end_of_file, error)
  IF (error%haserror) GOTO 9999
  !
  IF (.NOT. end_of_file) THEN
    IF (new_brnfile) THEN
      idgr=-999
      READ (cbuf, *, IOSTAT = ierr) mm, x, y, qob, qww, hbron, diameter, szopp, ibtg, ibroncat, iland, idgr
      IF ( idgr .LT. 0 .OR. idgr .GT. MAXDIST ) THEN
        ierr = -99
      ELSE
        IF ( abs(y) .LT. 90 ) THEN
          gb = y
          gl = x
!
!       Convert lon-lat coordinates to RDM coordinates
!
          CALL geo2amc(gb, gl, x, y) ! (x,y) in km
          x = AINT(x*1000.) ! [m]
          y = AINT(y*1000.) ! [m]
        ENDIF
      ENDIF
    ELSE
     !
     ! If there is a dot at position 9, coordinates are assumed to be lon-lat
     ! or, in a new_brnfile if the value read for y is less or equal to 99
     !
     IF ( cbuf(9:9) .EQ. '.' ) THEN
!  
!     Read source record with lon-lat coordinates (gl,gb) 
!     "g" << geographical coordinates; "l" << lengtegraad = longitude, "b" << breedtegraad = latitude

!
      READ (cbuf, 100, IOSTAT = ierr) mm, gl, gb, qob, qww, hbron, diameter, szopp, ibtg, ibroncat, iland, idgr
!      
      IF (ierr == 0) THEN
!
!       Convert lon-lat coordinates to RDM coordinates
!
        CALL geo2amc(gb, gl, x, y) ! (x,y) in km
        x = AINT(x*1000.) ! [m]
        y = AINT(y*1000.) ! [m]
      ENDIF
     ELSE
      !
      ! Read source record with RDM coordinates
      
      !
      READ (cbuf, 150, IOSTAT = ierr) mm, x, y, qob, qww, hbron, diameter, szopp, ibtg, ibroncat, iland, idgr
     ENDIF
    ENDIF

    IF (ierr == 0) THEN

      nrec=nrec+1
!
!     Check source strength ("sterkte"), heat content ("warmte") and source height ("hoogte").
!     Note: check is only perfomed inside check_source if no error has occurred; 
!           therefore there is no need to check for error%haserror here each time.
!
! JA*  check is only needed if source is selected. 
      CALL check_source(nrec, '<sterkte>', 0., 99999., qob, error)
      CALL check_source(nrec, '<warmte>', 0., 999., qww, error)
      CALL check_source(nrec, '<hoogte>', 0., 9999., hbron, error)
!
!     Check whether the source diameter >= grid resolution
!     << not yet active >>
!
!     IF ( spgrid /= 2 ) THEN
!       CALL check_source(nrec, '<diameter>',grid, 999999.,       !i
! +                          diameter, error)                        !i/o
!        ELSE
        CALL check_source(nrec, '<diameter>',-999999., 999999., diameter, error)
!     ENDIF

!     
!     Check lower and upper boundary
!        deviation                     : 0 <= szopp <= hbron
!        diurnal variation             : -999 <= ibtg <= 999
!        emission category             : 1 <= ibroncat <= 9999
!        country (= 'land')            : 1 <= iland <= 9999
!        paricle size distribution code: -999 <= idgr <= 999
!
      CALL check_source(nrec, '<deviatie>', 0., hbron, szopp, error)
      CALL check_isource(nrec, '<variatie>', -999, 999, ibtg, error)
      CALL check_isource(nrec, '<categorie>', 1, 9999, ibroncat, error)
      CALL check_isource(nrec, '<land>', 1, 9999, iland, error)
      CALL check_isource(nrec, '<verdeling>', -999, MAXDIST, idgr, error)
!
!     Check whether ibtg and idgr distributions in this record have been read (using presentcodes array).
!     Check of ibtg is not for NH3 (icm=3) and NOx (icm=2) if a special diurnal variation (4 or 5) is used.
!     Check of particle size distribution is only for particles (.not. gasv).

      IF (.NOT.((icm == 2 .OR. icm == 3) .AND. (ibtg == 4 .OR. ibtg == 5)))  THEN
          CALL check_verdeling(ibtg, presentcode, 1, 3, 'ibtg', error)
      ENDIF
      IF (.NOT. gasv) THEN                                                     
        CALL check_verdeling(idgr, presentcode, 2, 4, 'idgr', error)
      ENDIF
      IF (error%haserror) GOTO 9999
!
!     Copy valid (emission > 0) and selected sources to scratch file
!
      IF (qob .GT. EPS_DELTA) THEN

        !
        ! Loop over selected emission categories and countries
        !
        DO cattel = 1, ncatsel
          DO landtel = 1, nlandsel
   
            ! Write emission record of each country in case landsel = 0;
            ! write emission record of each emission category in case catsel = 0
            IF (landsel(landtel) .EQ. 0 ) THEN
              IF (catsel(cattel) .EQ. 0 ) THEN
                WRITE (fu_scratch, 50) mm,x,y,qob,qww, hbron, diameter, szopp, ibtg, ibroncat, iland, idgr
                numbron=numbron+1
              ELSE
                
                ! write emission record if emission category has been selected
                IF (ibroncat .EQ. catsel(cattel)) THEN
                  WRITE (fu_scratch, 50) mm,x,y,qob,qww, hbron, diameter, szopp, ibtg, ibroncat, iland, idgr
                  numbron=numbron+1
                ENDIF
              ENDIF
            ELSE

              ! write emission record if country has been selected, ...
              IF (iland .EQ. landsel(landtel)) THEN
                
                ! ... for each emission category
                IF (catsel(cattel) .EQ. 0 ) THEN
                  WRITE (fu_scratch, 50) mm,x,y,qob,qww, hbron, diameter, szopp, ibtg, ibroncat, iland, idgr
                  numbron=numbron+1
                ELSE

                  ! ... for selected emission category
                  IF (ibroncat .EQ. catsel(cattel)) THEN
                    WRITE (fu_scratch, 50) mm,x,y,qob,qww,hbron, diameter, szopp, ibtg, ibroncat, iland, idgr
                    numbron=numbron+1
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDIF
    ENDIF
  ENDIF
ENDDO
REWIND (fu_scratch)

RETURN

9999 CALL ErrorParam('numbron', numbron, error)
CALL ErrorParam('nrec', nrec, error)
CALL ErrorCall(ROUTINENAAM, error)

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE NAME    : check_source
! DESCRIPTION        : check whether a source parameter lies within a specified range. If not, the paramater is fixed at either
!                      the lower or upper limit of the range. In this case, a warning is written to the log file;
!                      this warning includes the record number of the source. 
! CALLED FUNCTIONS   :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE check_source(nr, varnaam, onder, boven, varwaarde, error)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM               
PARAMETER    (ROUTINENAAM = 'check_source')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: nr                         ! record number of source file
CHARACTER*(*), INTENT(IN)                        :: varnaam                    ! variable to be checked
REAL*4,    INTENT(IN)                            :: onder                      ! lower limit
REAL*4,    INTENT(IN)                            :: boven                      ! upper limit

! SUBROUTINE ARGUMENTS - I/O
REAL*4,    INTENT(INOUT)                         :: varwaarde                  ! (adapted) value of variable
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: mlen                       ! length of variable name
LOGICAL*1                                        :: switch                     ! indicates weather WARNING has already been printed
LOGICAL                                          :: ops_openlog                ! function for opening log file

!-------------------------------------------------------------------------------------------------------------------------------
!
! Check and possibly open log file. From here there is always going to be something written to it, so the opening is allowed.
!
IF (error%haserror) GOTO 9999

switch = .FALSE.

!
! Check upper limit; if needed, write warning and fix variable at upper limit.
!
IF (varwaarde .GT. (boven + EPS_DELTA)) THEN                                   ! varwaarde too large

   IF (.NOT. switch) THEN
     IF (.NOT. ops_openlog(error)) GOTO 9000
     WRITE(fu_log,'("WARNING: OPS has detected a value outside", " its limits in routine ", A)')                               &
        &  ROUTINENAAM(:LEN_TRIM(ROUTINENAAM))
   ENDIF

   switch=.TRUE.

   mlen = LEN_TRIM(varnaam)
   WRITE(fu_log,'(''   Record number '',I6,'': Value of '', ''emission variable '', a, '' ('', G10.3,                          &
  &  '') is outside range '', ''('', G10.3, '' - '', G10.3, '')'')') nr, varnaam(:mlen), varwaarde, onder , boven
   WRITE(fu_log,'(25x,''and has been set to maximum value'')')

   varwaarde = boven

!
! Check lower limit; if needed, write warning and fix variable at lower limit.
!
ELSEIF (varwaarde .LT. (onder - EPS_DELTA)) THEN                               ! varwaarde too small

   IF (.NOT. switch) THEN
     IF (.NOT. ops_openlog(error)) GOTO 9000
     WRITE(fu_log,'("WARNING: OPS has detected a value outside", " its limits in routine ", A)')                               &
        &  ROUTINENAAM(:LEN_TRIM(ROUTINENAAM))
   ENDIF

   switch=.TRUE.

   mlen = LEN_TRIM(varnaam)
   WRITE(fu_log,'(''   Record number '',I6,'': Value of '', ''emission variable '', a, '' ('', G10.3,                          &
  &  '') is outside range '', ''('', G10.3, '' - '', G10.3, '')'')') nr, varnaam(:mlen), varwaarde, onder , boven

   IF (varnaam .EQ. '<sterkte>') THEN
     WRITE(fu_log,'(25x,''Record will be skipped'')')                          ! Zero emissions are meaningless
   ELSE
     WRITE(fu_log,'(25x,''and has been set to minimum value'')')
   ENDIF

   varwaarde = onder

ELSE
   CONTINUE
ENDIF
RETURN

9000 CALL ErrorCall(ROUTINENAAM, error)
9999 RETURN

END SUBROUTINE check_source

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE NAME    : check_isource
! DESCRIPTION        : check whether an integer source parameter lies within a specified range. If not, the paramater is fixed at either
!                      the lower or upper limit of the range. In this case, a warning is written to the log file;
!                      this warning includes the record number of the source. 
! CALLED FUNCTIONS   :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE check_isource(nr, varnaam, onder, boven, varwaarde, error)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                
PARAMETER    (ROUTINENAAM = 'check_source')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: nr                         ! record number of source file
CHARACTER*(*), INTENT(IN)                        :: varnaam                    ! variable to be checked
INTEGER*4, INTENT(IN)                            :: onder                      ! lower limit
INTEGER*4, INTENT(IN)                            :: boven                      ! upper limit

! SUBROUTINE ARGUMENTS - I/O
INTEGER*4, INTENT(INOUT)                         :: varwaarde                  ! (adapted) value of variable
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES
REAL*4                                           :: var                        ! help variable (= float(varwaarde)) 

var = FLOAT(varwaarde)
CALL check_source(nr, varnaam, FLOAT(onder), FLOAT(boven), var, error)
varwaarde = NINT(var)

END SUBROUTINE check_isource

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE NAME    : check_verdeling
! DESCRIPTION        : Check whether distribution (=verdeling) has been read.
! CALLED FUNCTIONS   :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE check_verdeling(icode, presentcode, stdclass, usdclass, parname, error)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                
PARAMETER    (ROUTINENAAM = 'check_verdeling')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: icode                      ! code that has to be checked; 
                                                                               ! if icode < 0 -> check whether a user defined distribution is present
                                                                               ! if icode > 0 -> check whether a standard distribution is present
                                                                               ! if icode = 0 -> do not check anything
LOGICAL,   INTENT(IN)                            :: presentcode(MAXDIST,4)     
INTEGER*4, INTENT(IN)                            :: stdclass                   ! index of standard distributions in 2nd dimension of presentcode
INTEGER*4, INTENT(IN)                            :: usdclass                   ! index of user defined distributions in 2nd dimension of presentcode
CHARACTER*(*), INTENT(IN)                        :: parname                    ! parameter name in error messages

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: klasse                     ! 2nd index into presentcode

!
! Check for user defined distributions, in case icode < 0,
! check for standard distributions, in case icode > 0
!
IF (.NOT.error%haserror .and. icode /= 0) THEN
  IF (icode < 0) THEN
    klasse = usdclass
  ELSE
    klasse = stdclass
  ENDIF
  IF (.NOT. presentcode(ABS(icode), klasse)) THEN
    CALL SetError('No distribution available for this code of', parname, error)
    CALL ErrorParam(parname, icode, error)
    CALL ErrorCall(ROUTINENAAM, error)
  ENDIF
ELSE
  IF (icode == 0 .and. parname == "idgr") THEN
    CALL SetError('It is not permitted to use code 0 for', parname, error)
    CALL ErrorParam(parname, icode, error)
    CALL ErrorCall(ROUTINENAAM, error)
  ENDIF
ENDIF

END SUBROUTINE check_verdeling

END SUBROUTINE ops_read_source
