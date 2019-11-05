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
! FILENAME           : %M%
! SCCS(SOURCE)       : %P%
! RELEASE - LEVEL    : %R% - %L%
! BRANCH - SEQUENCE  : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : Hans van Jaarsveld, Franka Loeve (Cap Volmac)
!                      Chris Twenh"ofel (Cap Gemini)
! FIRM/INSTITUTE     : RIVM/LLO/IS
! LANGUAGE           : FORTRAN-77/90
! DESCRIPTION        : Read source data from scratch file and fill source related variables into buffer-arrays of size LSBUF.
!                      See also ops_read_source 
! EXIT CODES         :
! FILES AND OTHER    :
!    I/O DEVICES
! SYSTEM DEPENDENCIES: HP-Fortran
! CALLED FUNCTIONS   :
! UPDATE HISTORY     :
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_bron_rek(emtrend, landmax, emis, nsbuf, bnr, bx, by, bdiam, bsterkte, bwarmte, bhoogte, bsigmaz, btgedr,        &
                     &  bdegr, bqrv, bqtr, bcatnr, blandnr, eof, error)

USE m_commonconst
USE m_commonfile
USE m_error
USE m_geoutils
USE m_fileutils

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_bron_rek')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: emtrend                     

! SUBROUTINE ARGUMENTS - I/O
INTEGER*4, INTENT(INOUT)                         :: landmax                     
REAL*4,    INTENT(INOUT)                         :: emis(6,NLANDMAX)           

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*4, INTENT(OUT)                           :: nsbuf                       
INTEGER*4, INTENT(OUT)                           :: bnr(LSBUF)                  
INTEGER*4, INTENT(OUT)                           :: bx(LSBUF)                   
INTEGER*4, INTENT(OUT)                           :: by(LSBUF)                   
REAL*4,    INTENT(OUT)                           :: bdiam(LSBUF)                
REAL*4,    INTENT(OUT)                           :: bsterkte(LSBUF)             
REAL*4,    INTENT(OUT)                           :: bwarmte(LSBUF)              
REAL*4,    INTENT(OUT)                           :: bhoogte(LSBUF)              
REAL*4,    INTENT(OUT)                           :: bsigmaz(LSBUF)              
INTEGER*4, INTENT(OUT)                           :: btgedr(LSBUF)               
INTEGER*4, INTENT(OUT)                           :: bdegr(LSBUF)                
REAL*4,    INTENT(OUT)                           :: bqrv(LSBUF)                 
REAL*4,    INTENT(OUT)                           :: bqtr(LSBUF)                 
INTEGER*4, INTENT(OUT)                           :: bcatnr(LSBUF)               
INTEGER*4, INTENT(OUT)                           :: blandnr(LSBUF)              
LOGICAL,   INTENT(OUT)                           :: eof                        ! end of file has been reached 
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: mm                         ! 
INTEGER*4                                        :: ibtg                       ! 
INTEGER*4                                        :: ierr                       ! 
INTEGER*4                                        :: ibroncat                   ! 
INTEGER*4                                        :: idgr                       ! 
INTEGER*4                                        :: iland                      ! country code
INTEGER*4                                        :: index                      ! index of country code iland, in list of country codes
REAL*4                                           :: gl                         ! 
REAL*4                                           :: gb                         ! 
REAL*4                                           :: qtr                        ! 
REAL*4                                           :: qob                        ! 
REAL*4                                           :: x                          ! 
REAL*4                                           :: y                          ! 
REAL*4                                           :: diameter                   ! 
REAL*4                                           :: qww                        ! 
REAL*4                                           :: hbron                      ! 
REAL*4                                           :: szopp                      ! 
REAL*4                                           :: qrv                        ! 
CHARACTER*80                                     :: cbuf                       ! 

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida                    ! 
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
100 FORMAT (i4, 2f8.3, e10.3, f7.3, f6.1, f7.0, f6.1, 4i4)
150 FORMAT (i4, 2f9.0, e10.3, f7.3, f6.1, f8.0, f6.1, 4i4)
!
! Initialise nsbuf = 0 (no sources in buffer arrays).
!
nsbuf = 0
!
! Read source data until nsbuf = LSBUF or end-of-file
!
DO WHILE (nsbuf /= LSBUF)
!
! Read source record cbuf from scratch file
!
  CALL sysread(fu_scratch, cbuf, eof, error)
  IF (error%haserror) GOTO 9999
!
! If end of file has been reached, nothing is left to do here
!
  IF (eof) RETURN
!
! If there is a dot at position 9, coordinates are assumed to be lon-lat
!
  IF (cbuf(9:9) == '.') THEN
!
!   Read source record with lon-lat coordinates (gl,gb) 
!   "g" << geographical coordinates; "l" << lengtegraad = longitude, "b" << breedtegraad = latitude

!
    READ (cbuf, 100, IOSTAT = ierr) mm, gl, gb, qob, qww, hbron, diameter, szopp, ibtg, ibroncat, iland, idgr
    IF (ierr == 0) THEN
!
!     Convert lon-lat coordinates to RDM coordinates [m]
!
      CALL geo2amc(gb, gl, x, y)
      x = AINT(x*1000.)
      y = AINT(y*1000.)
    ENDIF
  ELSE
!
!   Read source record with RDM coordinates

!
    READ (cbuf, 150, IOSTAT = ierr) mm, x, y, qob, qww, hbron, diameter, szopp, ibtg, ibroncat, iland, idgr
  ENDIF
!
! ierr > 0: error occurred while reading. Jump to error section.
!
  IF (ierr > 0) GOTO 1000
!
! ierr < 0: end-of-file. Nothing left to do here (return).
!
  IF (ierr < 0) then
    eof = .TRUE.
    RETURN
  ENDIF
!
! ierr = 0: no error or end-of-file; Store data in source buffer arrays.
!

  ! Default source strength of traffic and space heating = 0
  qtr = 0.
  qrv = 0.

  ! ibtg = 2: space heating emissions
  IF (ABS(ibtg) == 2) THEN
    qrv = qob
    qob = 0.

  ! ibtg = 3: traffic emissions
  ELSE IF (ABS(ibtg) == 3) THEN
    qtr   = qob
    qob   = 0.
  ELSE
    CONTINUE
  ENDIF

  ! Muliply emission with a trend factor for the current year
  qob = qob*emtrend
  qrv = qrv*emtrend
  qtr = qtr*emtrend

  ! Reset negative source height to 0.01 m
  IF (ABS(hbron) <= EPS_DELTA) THEN
    hbron = .01
  ENDIF

  ! Check whether the source has a positive emission:
  IF ((qob + qrv + qtr) > (0. + EPS_DELTA)) THEN 
!
!   Compute emission totals per land and for 4 categories (see ops_print_info).
!   1: country number, 2: industry/high, 3: industry/low, 4: space heating, 5:traffic, 6: total);
!
 
   ! Find the index of the country for this source record
   ! Note: a list of maximal 50 country codes is kept inside the routine GetIndex
   CALL GetIndex(iland, index)

  IF (index <= NLANDMAX ) THEN
      IF (index > landmax) landmax = index
      emis(1,index) = FLOAT(iland)                                             ! country number

      ! Split into high and low sources:
      IF (hbron > (35. + EPS_DELTA)) THEN
        emis(2,index) = emis(2,index) + qob                                    ! summation of high industrial sources
      ELSE
        emis(3,index) = emis(3,index) + qob                                    ! summation of low industrial sources
      ENDIF
      emis(4,index) = emis(4,index) + qrv                                      ! summation of space heating sources
      emis(5,index) = emis(5,index) + qtr                                      ! summation of traffic sources
      emis(6,index) = emis(6,index) + qob+qrv+qtr                              ! summation of all sources
    ENDIF
!
!   Store data for this source in buffer array
!
    nsbuf = nsbuf + 1
    IF (IGEO /= 1) THEN
      bnr(nsbuf) = mm
      bx(nsbuf)  = NINT(x)
      by(nsbuf)  = NINT(y)
    ENDIF

    bsterkte(nsbuf) = qob
    bwarmte(nsbuf)  = qww
    bhoogte(nsbuf)  = hbron
    bdiam(nsbuf)    = diameter
    bsigmaz(nsbuf)  = szopp
    btgedr(nsbuf)   = ibtg
    bdegr(nsbuf)    = idgr
    bqrv(nsbuf)     = qrv
    bqtr(nsbuf)     = qtr
    bcatnr(nsbuf)   = ibroncat
    blandnr(nsbuf)  = iland
  ENDIF
ENDDO

RETURN

1000 CALL SetError('Error reading sources', error)
CALL ErrorParam('error number', ierr, error)

9999 CALL ErrorParam('file', 'scratch', error)
CALL ErrorParam('source number', nsbuf, error)
CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE ops_bron_rek
