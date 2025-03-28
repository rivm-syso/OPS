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
! DESCRIPTION        : Read source data from scratch file and fill source related variables into buffer-arrays of size LSBUF.
!                      See also ops_read_source 
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_bron_rek

implicit none

contains

SUBROUTINE ops_bron_rek(emtrend, buildingEffect, landmax, emis, nsbuf, bnr, bx, by, bdiam, bsterkte, bwarmte, bhoogte, bsigmaz, bD_stack, bV_stack, bTs_stack, & 
                        bemis_horizontal, bbuilding, btgedr, bdegr, bqrv, bqtr, bcatnr, blandnr, eof, error)

use m_commonconst_lt
USE m_commonfile
USE m_error
USE m_geoutils
USE m_fileutils
USE m_ops_building
use m_ops_utils, only: is_missing

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_bron_rek')

! SUBROUTINE ARGUMENTS - INPUT
REAL,      INTENT(IN)                            :: emtrend  
type(TbuildingEffect)                            :: buildingEffect            ! structure with building effect tables                   

! SUBROUTINE ARGUMENTS - I/O
INTEGER,   INTENT(INOUT)                         :: landmax                     
REAL,      INTENT(INOUT)                         :: emis(6,NLANDMAX)           

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER,   INTENT(OUT)                           :: nsbuf                       
INTEGER,   INTENT(OUT)                           :: bnr(LSBUF)                  
INTEGER,   INTENT(OUT)                           :: bx(LSBUF)                   
INTEGER,   INTENT(OUT)                           :: by(LSBUF)                   
REAL,      INTENT(OUT)                           :: bdiam(LSBUF)                
REAL,      INTENT(OUT)                           :: bsterkte(LSBUF)             
REAL,      INTENT(OUT)                           :: bwarmte(LSBUF)              
REAL,      INTENT(OUT)                           :: bhoogte(LSBUF)              
REAL,      INTENT(OUT)                           :: bsigmaz(LSBUF)  
REAL,      INTENT(OUT)                           :: bD_stack(LSBUF)           ! diameter of the stack [m]
REAL,      INTENT(OUT)                           :: bV_stack(LSBUF)           ! exit velocity of plume at stack tip [m/s]
REAL,      INTENT(OUT)                           :: bTs_stack(LSBUF)          ! temperature of effluent from stack [K]            
LOGICAL,   INTENT(OUT)                           :: bemis_horizontal(LSBUF)   ! horizontal outflow of emission
type(Tbuilding), INTENT(OUT)                     :: bbuilding(LSBUF)          ! array with structures with building parameters
INTEGER,   INTENT(OUT)                           :: btgedr(LSBUF)
INTEGER,   INTENT(OUT)                           :: bdegr(LSBUF)                
REAL,      INTENT(OUT)                           :: bqrv(LSBUF)                 
REAL,      INTENT(OUT)                           :: bqtr(LSBUF)                 
INTEGER,   INTENT(OUT)                           :: bcatnr(LSBUF)               
INTEGER,   INTENT(OUT)                           :: blandnr(LSBUF)              
LOGICAL,   INTENT(OUT)                           :: eof                        ! end of file has been reached 
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER                                          :: mm                         ! 
INTEGER                                          :: ibtg                       ! 
INTEGER                                          :: ibroncat                   ! 
INTEGER                                          :: idgr                       ! 
INTEGER                                          :: iland                      ! country code
INTEGER                                          :: index                      ! index of country code iland, in list of country codes
REAL                                             :: qtr                        ! 
REAL                                             :: qob                        ! 
REAL                                             :: x                          ! 
REAL                                             :: y                          ! 
REAL                                             :: diameter                   ! 
REAL                                             :: qww                        ! 
REAL                                             :: hbron                      ! emission height at source (stack height), without plume rise [m]
REAL                                             :: sigz0                      ! initial vertical dispersion length [m]
REAL                                             :: D_stack                    ! diameter of the stack [m]
REAL                                             :: V_stack                    ! exit velocity of plume at stack tip [m/s]
REAL                                             :: Ts_stack                   ! temperature of effluent from stack [K]            
LOGICAL                                          :: emis_horizontal            ! horizontal outflow of emission
type(Tbuilding)                                  :: building                   ! structure with building paramaters
REAL                                             :: qrv                        ! 
REAL                                             :: valueArray(buildingEffect%nParam)  ! array with parameters needed to compute building effect
INTEGER                                          :: iParam                     ! index of building parameter
INTEGER                                          :: ierr                       ! error code

!-------------------------------------------------------------------------------------------------------------------------------
!
! Initialise nsbuf = 0 (no sources in buffer arrays).
!
nsbuf = 0
bcatnr = 0
by = 0
bx = 0
bnr = 0
blandnr = 0
btgedr = 0
bdegr = 0
!
! Read source data from scratch file in block of length LSBUF (or till end-of-file) and put data into buffer arrays of size LSBUF.
!

DO WHILE (nsbuf /= LSBUF)

  ! Read source record with RDM coordinates:
  READ (fu_scratch, iostat = ierr) mm, x, y, qob, qww, hbron, diameter, sigz0, D_stack, V_stack, Ts_stack, emis_horizontal, ibtg, ibroncat, iland, idgr, building%length, building%width, building%height, building%orientation
  IF (ierr < 0) THEN
     eof = .true.
     return ! If end of file has been reached, nothing is left to do here
  ELSE IF (ierr > 0) THEN
     CALL SetError('Error reading file', error)
     CALL ErrorParam('io-status', ierr, error)
     GOTO 9998
  ENDIF
  nsbuf = nsbuf + 1

  !write(*,'(a,i6,10(1x,e12.5),1x,l2,4(1x,i4),4(1x,e12.5))') 'ops_bron_rek a ',mm, x, y, qob, qww, hbron, diameter, sigz0, D_stack, V_stack, Ts_stack, emis_horizontal, & 
  !                                                           ibtg, ibroncat, iland, idgr, building%length, building%width, building%height, building%orientation

  ! Determine building factor function (function of source receptor angle and source receptor distance):
  if (is_missing(building%length) .or. is_missing(building%width) .or. is_missing(building%height) .or. is_missing(building%orientation)) then  
     building%type = 0 ! no building effect
     
     ! Allocate building arrays to zero size, even when there is no building (they're passed as subroutine arguments)
     call ops_building_alloc_zero(building, error) 
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
       write(*,*) 'IGEO in ops_bron_rek not supported = ',IGEO  
       stop 1
    ENDIF

    bsterkte(nsbuf)  = qob
    bwarmte(nsbuf)   = qww
    bhoogte(nsbuf)   = hbron
    bdiam(nsbuf)     = diameter
    bsigmaz(nsbuf)   = sigz0
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

end module m_ops_bron_rek
