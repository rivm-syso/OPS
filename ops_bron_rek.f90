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
! BRANCH - SEQUENCE  : %B% - %S%
! DATE - TIME        : %E% - %U%
! WHAT               : %W%:%E%
! AUTHOR             : OPS-support
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
SUBROUTINE ops_bron_rek(emtrend, buildingEffect, landmax, emis, nsbuf, bnr, bx, by, bdiam, bsterkte, bwarmte, bhoogte, bsigmaz, bD_stack, bV_stack, bTs_stack, &
                        bemis_horizontal, bbuilding, btgedr, bdegr, bqrv, bqtr, bcatnr, blandnr, eof, error)

USE m_commonconst
USE m_commonfile
USE m_error
USE m_geoutils
USE m_fileutils
USE m_ops_building
use m_ops_utils, only: is_missing

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM
PARAMETER    (ROUTINENAAM = 'ops_bron_rek')

! SUBROUTINE ARGUMENTS - INPUT
REAL*4,    INTENT(IN)                            :: emtrend
type(TbuildingEffect)                            :: buildingEffect            ! structure with building effect tables

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
REAL*4,    INTENT(OUT)                           :: bD_stack(LSBUF)           ! diameter of the stack [m]
REAL*4,    INTENT(OUT)                           :: bV_stack(LSBUF)           ! exit velocity of plume at stack tip [m/s]
REAL*4,    INTENT(OUT)                           :: bTs_stack(LSBUF)          ! temperature of effluent from stack [K]
LOGICAL,   INTENT(OUT)                           :: bemis_horizontal(LSBUF)   ! horizontal outflow of emission
type(Tbuilding), INTENT(OUT)                     :: bbuilding(LSBUF)          ! array with structures with building parameters
INTEGER*4, INTENT(OUT)                           :: btgedr(LSBUF)
INTEGER*4, INTENT(OUT)                           :: bdegr(LSBUF)
REAL*4,    INTENT(OUT)                           :: bqrv(LSBUF)
REAL*4,    INTENT(OUT)                           :: bqtr(LSBUF)
INTEGER*4, INTENT(OUT)                           :: bcatnr(LSBUF)
INTEGER*4, INTENT(OUT)                           :: blandnr(LSBUF)
LOGICAL,   INTENT(OUT)                           :: eof                        ! end of file has been reached
TYPE (TError), INTENT(OUT)                       :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER*4                                        :: mm
INTEGER*4                                        :: ibtg
INTEGER*4                                        :: ibroncat
INTEGER*4                                        :: idgr
INTEGER*4                                        :: iland                      ! country code
INTEGER*4                                        :: index                      ! index of country code iland, in list of country codes
REAL*4                                           :: gl
REAL*4                                           :: gb
REAL*4                                           :: qtr
REAL*4                                           :: qob
REAL*4                                           :: x
REAL*4                                           :: y
REAL*4                                           :: diameter
REAL*4                                           :: qww
REAL*4                                           :: hbron
REAL*4                                           :: szopp
REAL*4                                           :: D_stack                    ! diameter of the stack [m]
REAL*4                                           :: V_stack                    ! exit velocity of plume at stack tip [m/s]
REAL*4                                           :: Ts_stack                   ! temperature of effluent from stack [K]
LOGICAL                                          :: emis_horizontal            ! horizontal outflow of emission
type(Tbuilding)                                  :: building                   ! structure with building paramaters
REAL*4                                           :: qrv
CHARACTER*512                                    :: cbuf                       ! character buffer
REAL                                             :: valueArray(buildingEffect%nParam)  ! array with parameters needed to compute building effect
INTEGER                                          :: iParam                     ! index of building parameter

! SCCS-ID VARIABLES
CHARACTER*81                                     :: sccsida
sccsida = '%W%:%E%'//char(0)
!-------------------------------------------------------------------------------------------------------------------------------
 50 FORMAT (i8, 2f9.0, es12.3, f9.3, f6.1, f8.0, f6.1, 3e12.5, l2, 4i4, 4f9.3) ! format for writing to scratch (RDM; includes D_stack, V_stack, Ts_stack, building parameters possibly -999). Also possible -999 for qw

! Initialise nsbuf = 0 (no sources in buffer arrays).
!
nsbuf = 0
!
! Read source data from scratch file in block of length LSBUF (or till end-of-file) and put data into buffer arrays of size LSBUF.
!

DO WHILE (nsbuf /= LSBUF)
!
! Read source record cbuf from scratch file
!
  CALL sysread(fu_scratch, cbuf, eof, error)
  IF (error%haserror) GOTO 9998
!
! If end of file has been reached, nothing is left to do here
!
  IF (eof) RETURN
!
! Read source record with RDM coordinates
!
  READ (cbuf, 50) mm, x, y, qob, qww, hbron, diameter, szopp, D_stack, V_stack, Ts_stack, emis_horizontal, ibtg, ibroncat, iland, idgr, building%length, building%width, building%height, building%orientation
  nsbuf = nsbuf + 1

  !write(*,'(a,i6,10(1x,e12.5),1x,l2,4(1x,i4),4(1x,e12.5))') 'ops_bron_rek a ',mm, x, y, qob, qww, hbron, diameter, szopp, D_stack, V_stack, Ts_stack, emis_horizontal, &
  !                                                           ibtg, ibroncat, iland, idgr, building%length, building%width, building%height, building%orientation

  ! Determine building factor function (function of source receptor angle and source receptor distance):
  if (is_missing(building%length) .or. is_missing(building%width) .or. is_missing(building%height) .or. is_missing(building%orientation)) then
     building%type = 0 ! no building effect
  else
     building%type = 1 ! building effect is present

     ! Fill array with parameters relevant for building effect (last two values (angle_SR_axis, distance) are filled in subroutine ops_building_get_function and are set to -999 here);
     ! parameters must correspond with buildingParamNames(9) = (/'hEmis', 'V_stack', 'D_stack', 'buildingHeight', 'buildingLength', 'buildingWLRatio', 'buildingOrientation', 'angleSRxaxis', 'distance' /)  in m_ops_building
     ! horizontal emission -> no momentum plume rise -> set valueArray(2) = 0 -> V_stack uses minimal value in table for building effect
     if (emis_horizontal) then
        valueArray = (/ hbron, 0.0    , D_stack, building%height, building%length, building%width/building%length, building%orientation, -999.0, -999.0 /)
        ! valueArray = (/ hbron, -999.0, -999.0 /)  ! TEST with three parameters
        ! valueArray = (/ 0.0, building%height, hbron, -999.0 /)  !  TEST with four parameters as in test6_fs2
     else
        valueArray = (/ hbron, V_stack, D_stack, building%height, building%length, building%width/building%length, building%orientation, -999.0, -999.0 /)
        ! valueArray = (/ hbron, -999.0, -999.0 /)   ! TEST with three parameters
        ! valueArray = (/ V_stack, building%height, hbron, -999.0 /)  ! TEST with four parameters as in test6_fs2
     endif

     ! Values outside the table input are moved to the boundary of the table ('constant extrapolation'):
     do iParam = 1,buildingEffect%nParam
        valueArray(iParam) = min(max(valueArray(iParam),buildingEffect%minClass(iParam)),buildingEffect%maxClass(iParam))
     enddo

     ! write(*,*) 'ops_bron_rek/valueArray: ',valueArray
     ! write(*,*) 'ops_bron_rek/classdefinitionArray: ',buildingEffect%classdefinitionArray
     ! write(*,*) 'ops_bron_rek/nParam = ',buildingEffect%nParam
     ! write(*,*) 'ops_bron_rek/nClass = ',buildingEffect%nClass(1:buildingEffect%nParam)
     ! write(*,*) 'ops_bron_rek/minClass = ',buildingEffect%minClass(1:buildingEffect%nParam)
     ! write(*,*) 'ops_bron_rek/maxClass = ',buildingEffect%maxClass(1:buildingEffect%nParam)
     ! write(*,*) 'ops_bron_rek/buildingFactArray(1:10): ',buildingEffect%buildingFactArray(1:10)

     call ops_building_get_function(buildingEffect%nParam, valueArray, buildingEffect%nClass, buildingEffect%classdefinitionArray,  &
                                    buildingEffect%buildingFactAngleSRxaxis, buildingEffect%buildingFactDistances, buildingEffect%buildingFactArray, building%buildingFactFunction, error)
     ! write(*,*) 'buildingFactFunction = ',building%buildingFactFunction
     if (error%haserror) goto 9999
  endif

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

  ! Multiply emission with a trend factor for the current year
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
    IF (IGEO /= 1) THEN
      bnr(nsbuf) = mm
      bx(nsbuf)  = NINT(x)
      by(nsbuf)  = NINT(y)
    ELSE
       write(*,*) 'IGEO in ops_bron_rek = ',IGEO
       stop
    ENDIF

    bsterkte(nsbuf)  = qob
    bwarmte(nsbuf)   = qww
    bhoogte(nsbuf)   = hbron
    bdiam(nsbuf)     = diameter
    bsigmaz(nsbuf)   = szopp
    bD_stack(nsbuf)  = D_stack
    bV_stack(nsbuf)  = V_stack
    bTs_stack(nsbuf) = Ts_stack
    bemis_horizontal(nsbuf) = emis_horizontal

    bbuilding(nsbuf) = building
    btgedr(nsbuf)    = ibtg
    bdegr(nsbuf)     = idgr
    bqrv(nsbuf)      = qrv
    bqtr(nsbuf)      = qtr
    bcatnr(nsbuf)    = ibroncat
    blandnr(nsbuf)   = iland
  ENDIF
ENDDO  ! Loop over nsbuf

RETURN

9998 CALL ErrorParam('file', 'scratch', error)
9999 CALL ErrorParam('source number', nsbuf, error)
CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE ops_bron_rek
