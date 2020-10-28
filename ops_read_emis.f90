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
!
! NAME                  : %M%
! SCCS (SOURCE)         : %P%
! RELEASE - LEVEL       : %R% - %L%
! BRANCH -SEQUENCE      : %B% - %S%
! DATE - TIME           : %E% - %U%
! WHAT                  : %W%:%E%
! AUTHOR                : OPS-support 
! FIRM/INSTITUTE        : RIVM/LLO
! LANGUAGE              : FORTRAN-77/90
! DESCRIPTION           : Read source file with emissions and files with diurnal emission variations and particle size distributions.
!                         The source file is copied to a scratch file (line for line); variables that lie outside specified limits are fixed at
!                         the lower or upper limit. Data from user defined diurnal emission variations and particle size distributions are also
!                         checked and, if needed, adjusted.
! EXIT CODES            :
! FILES AND OTHER       :
!   I/O DEVICES
! SYSTEM DEPENDENCIES   : HP-Fortran
! CALLED FUNCTIONS      :
! UPDATE HISTORY        :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_read_emis(icm, gasv, ncatsel, catsel, nlandsel, landsel, numbron, dverl, usdverl, pmd, uspmd,     &
                      &  dv, usdv, presentcode, building_present1, error)

USE m_commonconst
USE m_commonfile
USE m_error
USE m_fileutils

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_read_emis')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER*4, INTENT(IN)                            :: icm                      
LOGICAL,   INTENT(IN)                            :: gasv                     
INTEGER*4, INTENT(IN)                            :: ncatsel                  
INTEGER*4, INTENT(IN)                            :: catsel(*)                
INTEGER*4, INTENT(IN)                            :: nlandsel                 
INTEGER*4, INTENT(IN)                            :: landsel(*)               


! SUBROUTINE ARGUMENTS - I/O
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: numbron                    ! number of selected sources
REAL*4,    INTENT(OUT)                           :: dverl(NHRBLOCKS,MAXDISTR)  ! standard diurnal emission variations distributions
REAL*4,    INTENT(OUT)                           :: usdverl(NHRBLOCKS,MAXDISTR)! user-defined diurnal emission variations distributions
REAL*4,    INTENT(OUT)                           :: pmd(NPARTCLASS,MAXDISTR)   ! standard particle size distributions
REAL*4,    INTENT(OUT)                           :: uspmd(NPARTCLASS,MAXDISTR) ! user-defined particle size distributions
INTEGER*4, INTENT(OUT)                           :: dv                         ! maximum code diurnal emission variation dverl
INTEGER*4, INTENT(OUT)                           :: usdv                       ! maximum code user specified diurnal emission variation usdverl
LOGICAL,   INTENT(OUT)                           :: presentcode(MAXDISTR,4)    ! which distribution codes are present
                                                                               ! presentcode(:,1): diurnal variations
                                                                               ! presentcode(:,2): particle size distributions
                                                                               ! presentcode(:,3): user-defined diurnal variation
                                                                               ! presentcode(:,4): user-defined particle size distributions
LOGICAL,   INTENT(OUT)                           :: building_present1          ! at least one building is present in the source file 


! LOCAL VARIABLES
INTEGER*4                                        :: ps                         ! maximum code pmd distribution (dummy)
INTEGER*4                                        :: usps                       ! maximum code uspmd distribution (dummy)

! SCCS-ID VARIABLE
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
!
! Read standard diurnal variations
!
CALL read_variation(dvnam, 'F6.0', NHRBLOCKS, 0, 'diurnal variations', .FALSE., dverl, dv, presentcode(:, 1), error)
IF (error%haserror) GOTO 9999
!
! Read user-defined diurnal variations (optionally)
!
IF (LEN_TRIM(usdvnam) /= 0) THEN
  CALL read_variation(usdvnam, 'F6.0', NHRBLOCKS, 1200, 'user-defined diurnal variation', .FALSE., usdverl, usdv,              &
                   &  presentcode(:,3), error)
  IF (error%haserror) GOTO 9999
ELSE
  usdv = 0
ENDIF

IF (.NOT.gasv) THEN

  ! Read standard particle size distributions 

  CALL read_variation(psdnam, 'F7.1', NPARTCLASS, 0, 'particle size distributions', .TRUE., pmd, ps, presentcode(:, 2),        &
                   &  error)
  IF (error%haserror) GOTO 9999

  ! Read user-defined particle size distributions (optionally)

  IF (LEN_TRIM(uspsdnam) /= 0) THEN
    CALL read_variation(uspsdnam, 'F7.1', NPARTCLASS, 100, 'user-defined particle size distributions', .TRUE., uspmd, usps,    &
                    &  presentcode(:,4), error)
    IF (error%haserror) GOTO 9999
  ENDIF
ENDIF
!
! Read brnam, the file with sources. Note that this file (and other files later on) are also closed if an error occurred during
! reading, therefore the error check is not before the close statement.
!
IF (.NOT.sysopen(fu_bron, brnam, 'r', 'emission file', error)) GOTO 9999
!
! Open scratch file
!
OPEN(fu_scratch, STATUS = 'SCRATCH')

!
! Read, select and check sources 
!
CALL ops_read_source(icm, gasv, ncatsel, catsel, nlandsel, landsel, presentcode, numbron, building_present1, error)

IF (error%haserror) THEN
  CALL ErrorParam('emission file', brnam, error)
ENDIF

CALL sysclose(fu_bron, brnam, error)
IF (error%haserror) GOTO 9999

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)

CONTAINS

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE NAME    : read_variation
! DESCRIPTION        : Reads variations file (e.g. diurnal emission variations or particle size distributions).
!
! Example of diurnal emission variations file:
!
! code   0-2   2-4   4-6   6-8  8-10 10-12 12-14 14-16 16-18 18-20 20-22 22-24 description 
! +000   100   100   100   100   100   100   100   100   100   100   100   100 Continuous emission
! +001    73    69    68   100   129   131   124   121   109    97    93    86 Average industrial activity
! +002    33    33    35    80   150   155   120   116   122   135   145    77 Average heating behaviour
! +003    24    16    23   150   175   121   127   154   190   112    60    48 Average traffic intensity

! Example of particle size distribution file: FS
! 
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE read_variation(distnam, fmt, nrclass, normalvalue, compdesc, fraction, distrib, maxcode, presentcode, error)

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'read_variation')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: distnam                    ! name of file with distributions
CHARACTER*(*), INTENT(IN)                        :: fmt                        ! format of the numbers in distributions file
INTEGER*4, INTENT(IN)                            :: nrclass                    ! number of distribution classes read each record
INTEGER*4, INTENT(IN)                            :: normalvalue                ! value used in normalisation, 0 if no normalisation
                                                                               ! normalisation means that the sum of the variation is set to 
                                                                               ! normalvalue (e.g. 100 for a set of percentages or 
                                                                               ! 1200 for a set of 2-hourly percentages in a day)
CHARACTER*(*), INTENT(IN)                        :: compdesc                   ! type of distributions (diurnal variation or particle size)
LOGICAL,   INTENT(IN)                            :: fraction                   ! whether conversion to fractions is required (instead of %)

! SUBROUTINE ARGUMENTS - OUTPUT
REAL*4,    INTENT(OUT)                           :: distrib(nrclass,MAXDISTR)   ! array with all distributions 
INTEGER*4, INTENT(OUT)                           :: maxcode                    ! maximum code used for distribution
LOGICAL,   INTENT(OUT)                           :: presentcode(MAXDISTR)       ! which distribution codes are present
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: distcode                   ! code used for distribution; 
                                                                               ! read from the first column of the distributions file.
                                                                               ! (|distcode| = index into 2nd dimension of distrib(nclass, MAXDISTR))

! LOCAL VARIABLES
INTEGER*4                                        :: i                          ! DO LOOP counter
INTEGER*4                                        :: numdist                    ! Number of distributions read
INTEGER*4                                        :: ierr                       ! value of IOSTAT
REAL*4                                           :: buffer(nrclass)            ! array with the last distrib values read
REAL*4                                           :: som                        ! sum of row values
REAL*4                                           :: normalfactor               ! normalisation factor
CHARACTER*80                                     :: readformat                 ! format used for reading
LOGICAL                                          :: ops_openlog                ! function for opening log file

!-------------------------------------------------------------------------------------------------------------------------------
numdist = 0
maxcode = 0
distcode = 0
!
! Open the file for reading.
!
IF (.NOT. sysopen(fu_dist,distnam,'r',compdesc,error)) GOTO 9999
!
! Create format for reading, e.g. '(I6,6(F6.0))'
!
CALL startformat(readformat, error)
CALL appendformat('I6', readformat, error)
CALL appendformat(nrclass, fmt, readformat, error)
IF (error%haserror) GOTO 9000
!
! Skip header and blank records, if present, and read the first record. (ierr == 0 and distcode == 0) is a blank line.
!
ierr = 1                                                                       ! any value > 0 is OK to enter loop
DO WHILE (ierr > 0 .OR. (ierr == 0 .AND. distcode == 0))
  READ(fu_dist, readformat, IOSTAT=ierr) distcode, (buffer(i), i=1, nrclass)
ENDDO
presentcode(:) = .FALSE.
!
! Check and assign previous record and read remaining records.
!
DO WHILE (ierr >= 0)
  IF (ierr > 0) GOTO 1000
  IF (distcode /= 0 ) THEN                                                     ! Ignores any blank lines
    distcode = ABS(distcode)
    numdist = numdist + 1
!
!   Check whether distcode is allowed.
!
    IF (distcode > MAXDISTR) THEN
      CALL SetError('Distribution code exceeds maximum', error)
      GOTO 2000
    ELSEIF (presentcode(distcode)) THEN
      CALL SetError('Distribution code occurs twice', error)
      GOTO 2000
    ENDIF
!
!   Assign code.
!
    presentcode(distcode) = .TRUE.
    IF (distcode > maxcode) THEN
      maxcode = distcode
    ENDIF
!
!   Assign values.
!
    distrib(:, distcode) = buffer(:nrclass)
  ENDIF

  ! Read next record
  READ(fu_dist, readformat, IOSTAT=ierr) distcode, (buffer(i), i=1, nrclass)
ENDDO
!
! Close file and write a log message.
!
CALL sysclose(fu_dist, distnam, error)
!
! Check whether any records were read. If not, generate an error message.
!
IF (numdist == 0) THEN
  CALL SetError('Distribution file contains no values', error)
  GOTO 2000
ENDIF
!
! Normalise any rows, where required.
! Normalisation means that the sum of the variation is set to normalvalue (e.g. 100 for a set of percentages or 
! 1200 for a set of 2-hourly percentages in a day)

IF (normalvalue > 0) THEN
  DO i = 1, maxcode
    IF (presentcode(i)) THEN
      som = SUM(distrib(:nrclass, i))
      IF (ABS(som) > EPS_DELTA) THEN
!
!     This is a record with values. So normalise it, if required.
!
        IF (NINT(som) /= normalvalue) THEN
          normalfactor = REAL(normalvalue) / som
          distrib(:, i) = distrib(:nrclass, i) * normalfactor

          IF (NINT(som) /= 100) THEN                                           ! message only if som != normalvalue and !=100
            IF (.NOT. ops_openlog(error)) GOTO 9999
            WRITE (fu_log, '("WARNING: OPS has detected an erraneous", " distribution in routine ", A)')                       &
                &  ROUTINENAAM(:LEN_TRIM(ROUTINENAAM))
            WRITE (fu_log,'(A," with code",i4, " has been normalized")') compdesc, SIGN(i, -1)
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDDO

ENDIF
!
! Set to fractions, if required (instead of %)
!
IF (fraction) THEN
  distrib(:,:maxcode) = distrib(:nrclass,:maxcode)/100.
ENDIF

RETURN
!
! Error during reading of file. Create error message.
!
1000 CALL SetError('Error reading variations file', error)
CALL ErrorParam('Error nr', ierr, error)

2000 CALL ErrorParam('Filename', distnam, error)
CALL ErrorParam('Reading', compdesc, error)
CALL ErrorParam('Code', distcode, error)
call ErrorParam('Numdist', numdist, error)

9000 CALL sysclose(fu_dist, distnam, error)
9999 CALL ErrorCall(ROUTINENAAM, error)
RETURN

END SUBROUTINE read_variation

END SUBROUTINE ops_read_emis
