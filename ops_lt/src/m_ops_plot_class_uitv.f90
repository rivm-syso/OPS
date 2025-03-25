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
! DESCRIPTION        : Write receptor results to csv-file (= based on name of PLTFILE in control file) and store output with class info.
!                      The format is in CSV, a sample file looks as follows:
! "name", "x-coord", "y-coord", "conc.", "freq.", "ps_class", "stab_class", "wind_sec_class", "dist_class", "wind_sec_low", "wind_sec_up", "dist_low", "dist_up"
! "", "", "", "NH3", "", "", "", "", "", "", "", "", ""
! "", "m", "m", "ug/m3", "", "", "", "", "", "deg", "deg", "m", "m"
! "NL10444", 95216, 479092, .5849580E-02, .7364839E-02, 1, 1, 1, 1, -15.00000, 15.00000, .000000, 100000.0
! "NL10444", 95216, 479092, .1254952E-02, .6176014E-02, 1, 1, 1, 2, -15.00000, 15.00000, 100000.0, 300000.0
! etc.
!-------------------------------------------------------------------------------------------------------------------------------
module m_ops_plot_class_uitv

implicit none

contains

! writes the csv file
SUBROUTINE ops_plot_class_uitv_rcp(spgrid, nrrcp, namco, coneh, namrcp, xm, ym, cpri_class, percvk_class, ps_index, trafst, error)

use m_error
use m_commonfile
use m_fileutils
use m_string
use m_commonconst_lt

IMPLICIT NONE

! SUBROUTINE ARGUMENTS - INPUT
INTEGER, INTENT(IN)                            :: spgrid                     ! currently only spgrid = 2 is supported: receptors at specific locations, read from file 
CHARACTER*(*), INTENT(IN)                      :: coneh                      ! concentration unit
INTEGER, INTENT(IN)                            :: nrrcp                      ! number of receptors
CHARACTER*(*), INTENT(IN)                      :: namco                      ! name of concentration variable
CHARACTER*(*), INTENT(IN)                      :: namrcp(nrrcp)              ! receptor names
REAL,      INTENT(IN)                          :: xm(nrrcp)                  ! x-coordinates of receptors (m RDM)
REAL,      INTENT(IN)                          :: ym(nrrcp)                  ! y-coordinates of receptors (m RDM)
REAL,      INTENT(IN)                          :: cpri_class(:,:,:,:)        ! (NSTAB,NSEK,NTRAJ,nrrcp) concentration of primary component at receptor points and height zm, per class [ug/m3]
DOUBLE PRECISION,    INTENT(IN)                :: percvk_class(:,:,:,:)      !(NSTAB,NSEK,NTRAJ,nrrcp) ! percvk of primary component at receptor points and height zm, per class [factor of occurrence] 
INTEGER, INTENT(IN)                            :: ps_index                   ! particle size class index (1-6)
REAL,      INTENT(IN)                          :: trafst(NTRAJ)              ! travel distances for each class


! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TError), INTENT(INOUT)                     :: error                      ! Error handling record

! LOCAL VARIABLES
INTEGER                                        :: ierr                       ! 
CHARACTER*512                                  :: csv_basename, csv_path
INTEGER                                        :: ircp, istab, isek, itraj         ! loop variables
REAL                                           :: trafst_bins(NTRAJ+1)       ! travel distances for each class
REAL                                           :: sec_low, sec_up            ! edges of wind sectors

! CONSTANTS
CHARACTER*512                                  :: ROUTINENAAM                ! 
PARAMETER    (ROUTINENAAM = 'ops_plot_class_uitv_rcp')

IF (spgrid .NE. 2) THEN
  ! only supported for receptor output (spgrid == 2)
  CALL SetError('Plot class output only available for receptors', error)
  GOTO 9999
ENDIF

! edges of trajectory classes, based on trafst
trafst_bins(:NTRAJ) = trafst
! last one is open ended on the right
! distances increase with ~factor 3, set right edge of bin to 3000 km
trafst_bins(NTRAJ+1) = trafst_bins(NTRAJ)*3

!-------------------------------------------------------------------------------------------------------------------------------

! determine file name from plot file: e.g. run001.plt -> run001_NH3_classoutput.csv
! 1. find directory path
call getdirectory(pltnam, csv_path, error)
! 2. find basename
call getbasename(pltnam, csv_basename, error)
! 3. merge directory and basename (without ext)
call mergestring(csv_path, csv_basename, csv_path, error)
! 4. append namco
call mergestring(csv_path, "_"//TRIM(namco), csv_path, error)
! 5. append "_classoutput.csv"
call mergestring(csv_path, "_classoutput.csv", csv_path, error)

!
! Open CSV file
!
IF (.NOT. sysopen(fu_classoutput, csv_path, 'w', 'csv classoutput file', error)) GOTO 9999

! Formatting for CSV
101 format((*(g0 : ", ")))

! Currently, only primary concentration is stored per class

! write header names in first row, quoted
WRITE (fu_classoutput, 101, IOSTAT = ierr) '"name"', '"x-coord"', '"y-coord"', '"conc."', '"freq."', &
                                           '"ps_class"', '"stab_class"', '"wind_sec_class"', '"dist_class"', &
                                           '"wind_sec_low"', '"wind_sec_up"', '"dist_low"', '"dist_up"'                                    
IF (ierr .GT. 0) GOTO 9777

! write substance name in second row, quoted
WRITE (fu_classoutput, 101, IOSTAT = ierr) '""', '""', '""', '"'//TRIM(namco)//'"', '""', &
                                           '""', '""', '""', '""', '""', '""', '""', '""'
IF (ierr .GT. 0) GOTO 9777

! write units in third row, quoted
WRITE (fu_classoutput, 101, IOSTAT = ierr) '""','"m"', '"m"', '"'//TRIM(coneh)//'"', '""', &
                                           '""', '""', '""', '""', &
                                           '"deg"', '"deg"', '"m"', '"m"'
IF (ierr .GT. 0) GOTO 9777

! loop over all indices nrrcp,NSTAB,NSEK,NTRAJ
! write name, location, concentration and classes
DO ircp = 1, nrrcp
  DO istab = 1, NSTAB
    DO isek = 1, NSEK
      ! edges of wind sectors, bin width = 360/NSEK
      sec_low = ((isek-1)-0.5)*360/NSEK
      sec_up = sec_low + 360/NSEK

      DO itraj = 1, NTRAJ
        IF ((cpri_class(istab, isek, itraj, ircp) > EPS_DELTA) &
              .and. percvk_class(istab, isek, itraj, ircp) > EPS_DELTA) THEN
            ! write output
            WRITE (fu_classoutput, 101, IOSTAT = ierr) '"'//TRIM(namrcp(ircp))//'"', NINT(xm(ircp)), NINT(ym(ircp)), &
                                                       cpri_class(istab, isek, itraj, ircp), percvk_class(istab, isek, itraj, ircp), &
                                                       ps_index, istab, isek, itraj, &
                                                       sec_low, sec_up, trafst_bins(itraj), trafst_bins(itraj+1)
            IF (ierr .GT. 0) GOTO 9777
        ENDIF
      ENDDO
    ENDDO
  ENDDO
ENDDO

! append empty line to end of the file
WRITE (fu_classoutput, '(a1)', IOSTAT = ierr) ' '

!
! Close plot file
!
CALL sysclose(fu_classoutput, csv_path, error)

RETURN

9777 CALL SetError('Error writing plot class output', error)
CALL ErrorParam('ierr', ierr, error)

9999 CALL ErrorCall(ROUTINENAAM, error)
CALL sysclose(fu_classoutput, csv_path, error)

END SUBROUTINE ops_plot_class_uitv_rcp

end module m_ops_plot_class_uitv
