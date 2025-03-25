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
! DESCRIPTION        : Read meteo statistics.
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_read_meteo

implicit none

contains

SUBROUTINE ops_read_meteo(intpol, varin_unc, jb, mb, idb, jt, mt, idt, uurtot, iseiz, zf, astat, trafst, gemre, z0_metreg_user, cs,       &
                       &  rainreg, z0_metreg, xreg, yreg, hourreg, error )

use m_error
USE m_commonconst_lt
USE m_commonfile
USE m_ops_varin, ONLY: Tvarin_unc

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                
PARAMETER    (ROUTINENAAM = 'ops_readmeteo')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: intpol                     !  = 0 interpolate between all meteo regions at specified location
                                                                               !  = 1 use meteo parameters from user specified meteo region
                                                                               !  = 2? use meteo parameters from user specified meteo file
TYPE(Tvarin_unc), INTENT(IN)                     :: varin_unc

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER,   INTENT(OUT)                           :: jb                          
INTEGER,   INTENT(OUT)                           :: mb                          
INTEGER,   INTENT(OUT)                           :: idb                         
INTEGER,   INTENT(OUT)                           :: jt                          
INTEGER,   INTENT(OUT)                           :: mt                          
INTEGER,   INTENT(OUT)                           :: idt                         
REAL,      INTENT(OUT)                           :: uurtot                     ! total number of hours from meteo statistics
INTEGER,   INTENT(OUT)                           :: iseiz                       
REAL,      INTENT(OUT)                           :: zf                          
REAL,      INTENT(OUT)                           :: astat(NTRAJ, NCOMP, NSTAB, NSEK)  
REAL,      INTENT(OUT)                           :: trafst(NTRAJ)               
REAL,      INTENT(OUT)                           :: gemre                       
REAL,      INTENT(OUT)                           :: z0_metreg_user             ! roughness length of user specified meteo region [m]
REAL,      INTENT(OUT)                           :: cs(NTRAJ, NCOMP, NSTAB, NSEK, NMETREG)  
REAL,      INTENT(OUT)                           :: rainreg(NMETREG)            
REAL,      INTENT(OUT)                           :: z0_metreg(NMETREG)         ! roughness lengths of NMETREG meteo regions; scale < 50 km [m]     
REAL,      INTENT(OUT)                           :: xreg(NMETREG)               
REAL,      INTENT(OUT)                           :: yreg(NMETREG)               
REAL,      INTENT(OUT)                           :: hourreg(NMETREG)            
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER                                          :: iyr                        ! year of time stamp of meteo file; currently not used
INTEGER                                          :: imon                       ! month of time stamp of meteo file; currently not used
INTEGER                                          :: iday                       ! day of time stamp of meteo file; currently not used
REAL                                             :: xpos                        
REAL                                             :: ypos                        
REAL                                             :: z0_metreg1                 ! rougness length of 1 meteo region [m]                        

!-------------------------------------------------------------------------------------------------------------------------------
!
! Get meteo parameters for every meteo class (distance class, wind direction sector, stability class).
! Note that astat is used to store the meteo parameters for one region (output of ops_readstexp);
! cs contains the meteo parameters for all regions (extra dimension NMETREG).
!
IF (intpol.EQ.0) THEN 
   ! Fill meteo parameters for every region (calls ops_readstexp NMETREG+1 times)
   CALL ops_statfil(varin_unc, jb, mb, idb,jt, mt, idt, uurtot, iseiz, zf, astat, trafst, cs, rainreg, z0_metreg, xreg, yreg,        &
                 &  hourreg, error)

   ! average precipitation amount [mm/h]:
   gemre = SUM(rainreg(:NMETREG))/NMETREG
ELSE
   ! Read meteo parameters for one region or from user specified file
   CALL ops_readstexp(kname, varin_unc, jb, mb, idb, gemre, iyr, imon, iday, xpos, ypos, z0_metreg1, jt, mt, idt, uurtot,     &
                   &  iseiz, zf, astat, trafst, error)
   z0_metreg_user = z0_metreg1
ENDIF

9999 IF (error%haserror) THEN
  CALL ErrorCall(ROUTINENAAM, error)
ENDIF

RETURN
END SUBROUTINE ops_read_meteo

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : ops_statfil
! DESCRIPTION        : Read meteo parameters for all meteo regions.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_statfil(varin_unc, jb, mb, idb, jt, mt, idt, uurtot, iseiz, zf, astat, trafst, cs, rainreg, z0_metreg, xreg, yreg,    &
                    &  hourreg, error)

USE m_commonconst_lt
USE m_commonfile
USE m_error
USE m_ops_varin, ONLY: Tvarin_unc

IMPLICIT NONE

! CONSTANTS
REAL                                             :: XP(NMETREG)                ! x-coordinate meteo regions in NL
REAL                                             :: YP(NMETREG)                ! y-coordinate meteo regions in NL
                                                                               ! (XP,YP)~ centre of circle that encompasses a meteo region.
                                                                               ! (XP,YP) are used for interpolation of meteo parameters in a 
                                                                               ! specific location
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_statfil')

! SUBROUTINE ARGUMENTS - INPUT
TYPE(Tvarin_unc), INTENT(IN)                     :: varin_unc

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER,   INTENT(OUT)                           :: jb                          
INTEGER,   INTENT(OUT)                           :: mb                          
INTEGER,   INTENT(OUT)                           :: idb                         
INTEGER,   INTENT(OUT)                           :: jt                          
INTEGER,   INTENT(OUT)                           :: mt                          
INTEGER,   INTENT(OUT)                           :: idt                         
REAL,      INTENT(OUT)                           :: uurtot                     ! total number of hours from meteo statistics
INTEGER,   INTENT(OUT)                           :: iseiz                       
REAL,      INTENT(OUT)                           :: zf                          
REAL,      INTENT(OUT)                           :: astat(NTRAJ, NCOMP, NSTAB, NSEK)  
REAL,      INTENT(OUT)                           :: trafst(NTRAJ)               
REAL,      INTENT(OUT)                           :: cs(NTRAJ, NCOMP, NSTAB, NSEK, NMETREG) 
REAL,      INTENT(OUT)                           :: rainreg(NMETREG)            
REAL,      INTENT(OUT)                           :: z0_metreg(NMETREG)         ! roughness lengths of NMETREG meteo regions; scale < 50 km [m]     
REAL,      INTENT(OUT)                           :: xreg(NMETREG)               
REAL,      INTENT(OUT)                           :: yreg(NMETREG)               
REAL,      INTENT(OUT)                           :: hourreg(NMETREG)            
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER                                          :: ireg                       ! index of meteo region
INTEGER                                          :: iday                       ! day of time stamp of meteo file; currently not used
INTEGER                                          :: imon                       ! month of time stamp of meteo file; currently not used
INTEGER                                          :: iyr                        ! year of time stamp of meteo file; currently not used
INTEGER                                          :: idx                        ! index of '.' in name of meteo statistics file
REAL                                             :: gemre                      ! average amount of precipitation (mm/h)
REAL                                             :: xpos                       
REAL                                             :: ypos                       
REAL                                             :: z0_metreg1                 ! roughness length of 1 meteo region [m]                      
CHARACTER*512                                    :: nfile                      ! filename for meteo statistics file

! DATA 
! (XP,YP) are locations of region (~ centre of circle that encompasses region);
! (XP,YP) are used for interpolation of meteo parameters in a specific location.

DATA XP/ 5.5 ,  4.6,  6.2,  3.8 ,  5.5,  6.35/
DATA YP/53.25, 52.0, 52.8, 51.65, 52.0, 51.75/
!-------------------------------------------------------------------------------------------------------------------------------
50 FORMAT(/, 2x, 'meteo statistics from period:', 3i3, ' to', 3i3, /)

! Get index of '.' in name of meteo statistics file
idx = INDEX(kname,'.',.TRUE.)

! Loop over meteo regions:
DO ireg = 1, NMETREG

   ! Construct filename for meteo statistics (e.g. a001101c.001)
   ! (see also check in ops_gen_fnames)
   nfile = kname
   WRITE (nfile,'(A,I3.3)') kname(1:idx), ireg

   ! Read meteo parameters for this meteo region
   CALL ops_readstexp(nfile, varin_unc, jb, mb, idb, gemre, iyr, imon, iday, xpos, ypos, z0_metreg1, jt, mt, idt, uurtot,     &
                   &  iseiz, zf, astat, trafst, error)

   IF (error%haserror) THEN
     CALL ErrorParam('Meteo region index', ireg, error)
     GOTO 9999
   ENDIF

!
!  if xpos < 0, set xpos,ypos to pre-defined meteo region locations (xp,yp)
!
   IF (ABS(xpos) .LE. EPS_DELTA) THEN
      xpos = XP(ireg)
      ypos = YP(ireg)
   ENDIF

   ! Fill cs and other meteo arrays for this region
   cs(:, :, :, :, ireg) = astat(:NTRAJ, :NCOMP, :NSTAB, :NSEK)
   rainreg(ireg)        = gemre
   z0_metreg(ireg)      = z0_metreg1
   xreg(ireg)           = xpos
   yreg(ireg)           = ypos
   hourreg(ireg)        = uurtot
ENDDO
!
! Fill astat() with meteo parameters of most central region in NL (region 5) 890309;
! these data are used in ops_init for the evlauation of diurnal emission variation
!
nfile( idx+3:idx+3 ) = '5'
CALL ops_readstexp(nfile, varin_unc, jb, mb, idb, gemre, iyr, imon, iday, xpos, ypos, z0_metreg1, jt, mt, idt, uurtot,        &
                &  iseiz, zf, astat, trafst, error)
RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE ops_statfil

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : ops_readstexp
! DESCRIPTION        : This routine reads the climatology (meteo statistics) file and fills the meteodata array. 
!                      Depending on the value of intpol, this routine is called only once, or for each region.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_readstexp(nfile, varin_unc, jb, mb, idb, gemre, iyr, imon, iday, xpos, ypos, z0_metreg1, jt, mt, idt,          &
                      &  uurtot, iseiz, zf, astat, trafst, error)

USE m_commonconst_lt
USE m_error
USE m_utils
USE m_ops_varin, ONLY: Tvarin_unc

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_readstexp')

INTEGER                                          :: ISCALE(NCOMP)              ! OPS scalefactors for the different components in the climatology.
                                                                               ! All meteo parameters are given as integers and have to be scaled 
                                                                               ! by 10, 100 or 1000 in order to get meaningful real values; 
                                                                               ! e.g. wind speed in 0.1 m/s -> m/s

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: nfile                      ! full file name of meteo statistics file
TYPE(Tvarin_unc), INTENT(IN)                    :: varin_unc

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER,   INTENT(OUT)                           :: jb                         ! start year (meteo statistics period) ("b" << begin = start)
INTEGER,   INTENT(OUT)                           :: mb                         ! start month (meteo statistics period) ("b" << begin = start)
INTEGER,   INTENT(OUT)                           :: idb                        ! start day (meteo statistics period) ("b" << begin = start)
REAL,      INTENT(OUT)                           :: gemre                      ! average precipitation amount [mm/h]
INTEGER,   INTENT(OUT)                           :: iyr                        ! year of time stamp of meteo file; currently not used
INTEGER,   INTENT(OUT)                           :: imon                       ! month of time stamp of meteo file; currently not used
INTEGER,   INTENT(OUT)                           :: iday                       ! day of time stamp of meteo file; currently not used
REAL,      INTENT(OUT)                           :: xpos                       
REAL,      INTENT(OUT)                           :: ypos                       
REAL,      INTENT(OUT)                           :: z0_metreg1                 ! rougness length of 1 meteo region [m]                        
INTEGER,   INTENT(OUT)                           :: jt                         ! end year (meteo statistics period) ("t" << tot = until)
INTEGER,   INTENT(OUT)                           :: mt                         ! end month (meteo statistics period) ("t" << tot = until)
INTEGER,   INTENT(OUT)                           :: idt                        ! end day (meteo statistics period) ("t" << tot = until)
REAL,      INTENT(OUT)                           :: uurtot                     ! total number of hours from meteo statistics
INTEGER,   INTENT(OUT)                           :: iseiz                      
REAL,      INTENT(OUT)                           :: zf                          
REAL,      INTENT(OUT)                           :: astat(NTRAJ, NCOMP, NSTAB, NSEK) 
REAL,      INTENT(OUT)                           :: trafst(NTRAJ)               
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER                                          :: jtl                        ! four digit end year
INTEGER                                          :: jbl                        ! four digit start year
INTEGER                                          :: icomp                      ! index of meteo parameters
INTEGER*2                                        :: ishort(NSTAB*NSEK)         ! meta data of meteo statistics file 

! DATA
! OPS scalefactors for the different components in the climatology.
! All meteo parameters are given as integers, and scaling is necessary, e.g. wind speed in 0.1 m/s -> m/s.
! Note: 3*1 means 1,1,1.
!
! The following parameters are scaled with a scaling factor other than 1:
!  3. wind speed (at 10 m height) [m/s]
!  7. ratio effective dry deposition velocity over transport distance and average dry deposition velocity over transport distance for low sources [-]
!  8. ratio effective dry deposition velocity over transport distance and average dry deposition velocity over transport distance for high sources [-]
! 10. degree day (= 19-T for T < 12 degree C) (domestic heating coefficient) [degree C] 
! 11. precipitation probability []
! 12. length of rainfall period []
! 13. rain intensity []
! 15. wind speed power law coefficient [-]
! 19. friction velocity u* [m/s]
! 23. sensible heat flux H0 [W/m2]
! number     1  2   3 4/5/6   7/8  9  10    11   12   13 14   15 16 17 18    19 20 21 22  23 24 25 26 27 
DATA ISCALE/ 1, 1, 10, 3*1, 2*100, 1, 10, 1000, 100, 100, 1, 100, 1, 1, 1, 1000, 1, 1, 1, 10, 1, 1, 1, 1/
!-------------------------------------------------------------------------------------------------------------------------------
! Read meta data into ishort and meteo parameters into astat 
CALL ops_readdata(nfile, ishort, astat, error)

IF (error%haserror) GOTO 9999

!
! Fill meta data of meteo file  
!

! Start year, month, day
jb        = ishort(1)
mb        = ishort(2)
idb       = ishort(3)

! End year, month, day
jt        = ishort(4)
mt        = ishort(5)
idt       = ishort(6)

! Total number of hours
uurtot    = FLOAT(ishort(7)*1000 + ishort(8))

! ishort(11): NSTAB = number of stability classes
! ishort(12): NSEK = number of wind direction sectors

! Distance classes for trajectories [m]; see METPRO -> trafst = 0, 100, 300, 1000 km
trafst(1) = FLOAT(ishort(13)*1000) ! convert from km -> m
trafst(2) = FLOAT(ishort(14)*1000)
trafst(3) = FLOAT(ishort(15)*1000)
trafst(4) = FLOAT(ishort(16)*1000)

! ishort(17): NTRAJ = number of distance classes for trajectories
! ishort(18): NCOMP = number of meteo components

! Time stamp of file (year, month, day); currently not used
iyr       = ishort(19)
imon      = ishort(20)
iday      = ishort(21)

! Position of meteo region
ypos      = FLOAT(ishort(22)) + FLOAT(ishort(23))/100.
xpos      = FLOAT(ishort(24)) + FLOAT(ishort(25))/100.

! Roughness lengths
z0_metreg1 = FLOAT(ishort(27))/1000.
!
!    Scale astat with ISCALE
!
DO icomp = 1, NCOMP
  astat(:, icomp, :, :) = astat(:, icomp, :, :)/ISCALE(icomp)
ENDDO

! Adjustment relevant for sensitivity analyses. Multiplication factor is 1.0 by default.
astat(:, 2, :, :) = varin_unc%unc_meteo%xl_fact * astat(:, 2, :, :)

!
! Compute average precipitation amount [mm/h] 
!
gemre   = SUM( astat(1, 1, :NSTAB, :NSEK) * astat(1, 11, :, :) * astat(1, 13, :, :)) / uurtot
!
! Convert two digit year (from meteostatistics file) to four digits
!
jtl = Jaartal(jt)
jbl = Jaartal(jb)

!
! Get time period (years, year, winter, summer or month)
! and set zf = interpolation factor between summer and winter (zf << "zomer fractie" = summer fraction)
!
   
! year_end > year_start + 1 -> multiple years, iseiz = 0 (long term)
IF (jtl .GT. (jbl + 1)) THEN
   iseiz = 0
   zf    = .5

! (year_end,month_end) = (year_start+1,month_start) & more than 5000 hours -> iseiz = 1 (year)
ELSE IF ((jtl .GT. jbl) .AND. (mt .EQ. mb)) THEN
   IF (uurtot .GT. (5000. + EPS_DELTA)) THEN
      iseiz = 1
      zf    = .5

! (year_end,month_end) = (year_start+1,month_start) & less than 5000 hours -> iseiz = 2 (winter period in calender year)
   ELSE
      iseiz = 2
      zf    = 0.
   ENDIF

! year_end = year_start+1 & month_end+5 < month_start & month_start /= 12 -> iseiz = 2 (winter)
ELSE IF ((jtl .GT. jbl) .AND. ((mt + 5) .LT. mb) .AND. (mb .NE. 12)) THEN
   iseiz = 2
   zf    = 0.

! year_end = year_start & month_end > month_start+5 -> iseiz = 3 (summer)
ELSE IF ((jtl .EQ. jbl) .AND. (mt .GT. (mb + 5))) THEN
   iseiz = 3
   zf    = 1.

! all other cases -> iseiz = 4,5 (month)
ELSE
  
   ! month in summer
   iseiz = 5
   IF ((mb .GT. 3) .AND. (mb .LT. 10)) THEN
      zf = 1.

   ! month in winter
   ELSE
      iseiz = 4
      zf    = 0.
   ENDIF
ENDIF

RETURN                                                                         ! normal end, no error
!
! Error section. No error handling at this point. The error is passed to a higher level routine (for the time being).
!
9999 END

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : ops_readdata
! DESCRIPTION        : Read meteo data from meteo statistics file.
!-------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE ops_readdata(nfile, ishort, astat, error)

USE m_commonconst_lt
USE m_commonfile
USE m_error
USE m_utils
USE m_fileutils
IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_readdata')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*(*), INTENT(IN)                        :: nfile                      ! filename for meteo statistics file

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER*2, INTENT(OUT)                           :: ishort(NSTAB*NSEK)         ! meta data of meteo statistics file
REAL,      INTENT(OUT)                           :: astat(NTRAJ, NCOMP, NSTAB, NSEK) 
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER                                          :: irec                       ! record number
INTEGER                                          :: iwissel                    ! 1 if integers have been converted; otherwise 0.
INTEGER                                          :: itraj                      ! index into distance classes of trajectories
INTEGER                                          :: icomp                      ! index into meteo components
INTEGER*2                                        :: iastat(NSTAB, NSEK)        ! integer meteo parameters, before scaling
INTEGER                                          :: ierr                       ! error status when opening or reading file
INTEGER                                          :: recl                       ! record length

!-------------------------------------------------------------------------------------------------------------------------------
!
! Read meta data from meteo statistics file 
!
recl = NSTAB*NSEK*2
!
! Open meteo statistics file as direct access file
!
IF (.NOT.sysopen(fu_klim,nfile,'d','meteo file',error,recl)) THEN
  CALL SetError('Error opening meteo file', error)
  CALL ErrorParam('error nr', ierr, error)
  GOTO 9999
ENDIF

! Read from direct access file 
irec = 1
READ (fu_klim, IOSTAT = ierr, REC = irec) ishort

IF (ierr .GT. 0) THEN
  CALL SetError('Error reading meteo data header line', error)
  CALL ErrorParam('error nr', ierr, error)
  GOTO 9999
ENDIF

! wisselbytes needed to account for differences between operations systems/compilers
! ishort(17) = NTRAJ how does this influence wisselbytes
IF ((ishort(17) .LT. 3) .OR. (ishort(17) .GT. 4)) THEN
  iwissel = 1
  CALL wisselbytes(ishort, NSTAB*NSEK)
ELSE
  iwissel = 0
ENDIF
!
! Check meteo gegevens in ishort met NSEK, NCOMP, NSTAB en NTRAJ.
!
IF (.NOT.ops_checkmeteo(NSTAB, ishort(11), 'stability classes', error)) GOTO 9999
IF (.NOT.ops_checkmeteo(NSEK, ishort(12), 'sectors', error)) GOTO 9999
IF (.NOT.ops_checkmeteo(NTRAJ, ishort(17), 'trajectories', error)) GOTO 9999
IF (.NOT.ops_checkmeteo(NCOMP, ishort(18), 'components', error)) GOTO 9999

! Loop over distance classes and meteo components:
DO itraj = 1, NTRAJ
  DO icomp = 1, NCOMP

    ! Read from direct access file into integer array iastat
    irec = irec + 1
    READ (fu_klim, IOSTAT = ierr, REC = irec) iastat
    IF (ierr .GT. 0) GOTO 1000

    IF (iwissel .EQ. 1) THEN
      CALL wisselbytes(iastat, NSTAB, NSEK)
    ENDIF

    ! Fill into real array astat:
    astat(itraj,icomp,:,:) = FLOAT(iastat(:NSTAB,:NSEK))

  ENDDO
ENDDO
!
! Close the file.
!
CALL sysclose(fu_klim, nfile, error)
IF (error%haserror) GOTO 9999
!
! Reading was succesfull. Return.
!
RETURN
!
! Error reading line in meteo file
!
1000 CALL SetError('Error reading meteo data line', error)
CALL ErrorParam('error nr', ierr, error)
CALL ErrorParam('record nr', irec, error)
CALL ErrorParam('trajectory', itraj, error)
CALL ErrorParam('component', icomp, error)
!
! Error. Tell the name of the file.
!
9999 CALL ErrorParam('meteo file', nfile, error)
CALL ErrorCall(ROUTINENAAM, error)

END SUBROUTINE ops_readdata

!-------------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE         : ops_checkmeteo
! DESCRIPTION        : Checks whether the value of a meteo parameter read is what it should be.
!                      Here ops_checkmeteo is called in order to check values read from file
!                      against values fixed as parameters in m_commonconst_lt.
!-------------------------------------------------------------------------------------------------------------------------------
FUNCTION ops_checkmeteo(value, valueread, paramname, error)

USE m_error

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_checkmeteo')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER,   INTENT(IN)                            :: value                      ! what the value should be
INTEGER*2, INTENT(IN)                            :: valueread                  ! the value read (which should equal value)
CHARACTER*(*), INTENT(IN)                        :: paramname                  ! name of parameter (in error message)

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TError), INTENT(INOUT)                     :: error                      ! error handling record

! LOCAL VARIABLES
INTEGER                                          :: readvalue                  ! valueread as integer

! RESULT
LOGICAL                                          :: ops_checkmeteo             ! values are equal

!-------------------------------------------------------------------------------------------------------------------------------
IF (value.EQ.valueread) THEN
  ops_checkmeteo = .TRUE.
ELSE
  readvalue = valueread
  CALL SetError( 'Meteo data dimension is not according to OPS dimension', error)
  CALL ErrorParam('parameter', paramname, error)
  CALL ErrorParam('meteo data dimension', readvalue, error)
  CALL ErrorCall(ROUTINENAAM, error)
  ops_checkmeteo = .FALSE.
ENDIF

RETURN
END FUNCTION ops_checkmeteo

end module m_ops_read_meteo
