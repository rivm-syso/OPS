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

module m_tst_ops_plot_class_uitv_rcp

contains

SUBROUTINE tst_ops_plot_class_uitv_rcp

use no_pfunit
use m_error
use m_fileutils
use m_commonconst_lt
use m_commonfile
use m_ops_plot_class_uitv

IMPLICIT NONE

! for this test, nrrcp = 3
! SUBROUTINE ARGUMENTS - INPUT
INTEGER                         :: spgrid                     ! currently only spgrid = 2 is supported: receptors at specific locations, read from file 
CHARACTER*128                   :: coneh                      ! concentration unit
INTEGER, PARAMETER              :: nrrcp = 3                  ! number of receptors
CHARACTER*128                   :: namco                      ! name of concentration variable
CHARACTER*128                   :: namrcp(nrrcp)              ! receptor names
REAL                            :: xm(nrrcp)                  ! x-coordinates of receptors (m RDM)
REAL                            :: ym(nrrcp)                  ! y-coordinates of receptors (m RDM)
REAL                            :: cpri_class(NSTAB,NSEK,NTRAJ,nrrcp) ! concentration of primary component at receptor points and height zm, per class [ug/m3]
DOUBLE PRECISION                :: percvk_class(NSTAB,NSEK,NTRAJ,nrrcp) ! percvk of primary component at receptor points and height zm, per class [factor of occurrence] 
INTEGER                         :: ps_index                   ! particle size class index (1-6)
REAL                            :: trafst(NTRAJ)              ! travel distances for each class

! SUBROUTINE ARGUMENTS - OUTPUT
TYPE (TError)                   :: error                      ! Error handling record

! local variables
INTEGER                         :: ircp, istab, isek, itraj   ! loop variables
CHARACTER*128                   :: csv_path                 ! classoutput filename (csv)
LOGICAL                         :: file_exists
CHARACTER*512                   :: header_line
CHARACTER*512                   :: header_line_ex
! record values
CHARACTER*8                     :: lineno_c
CHARACTER*128                   :: rnam
INTEGER                         :: rx, ry, rps, rstab, rsek, rtraj, lineno
REAL                            :: rconc, rfreq, rwlow, rwup, rdistlow, rdistup, tol
REAL                            :: trafst_bins(NTRAJ+1)       ! travel distances for each class
REAL                            :: sec_low, sec_up

! set test tolerance
tol = 1e-4

! initialize values
namco = 'NH3'
coneh = 'ug/m3'
namrcp(1) = "rcp1"
namrcp(2) = "rcp2"
namrcp(3) = "rcp3"
xm(1) = 155000
xm(2) = 150000
xm(3) = 160000
ym(1) = 385000
ym(2) = 380000
ym(3) = 390000
trafst(1) = 0
trafst(2) = 100*1000
trafst(3) = 300*1000
trafst(4) = 1000*1000
! edges of trajectory classes, based on trafst
trafst_bins(:NTRAJ) = trafst
! last one is open ended on the right
! distances increase with ~factor 3, set right edge of bin to 3000 km
trafst_bins(NTRAJ+1) = trafst_bins(NTRAJ)*3
ps_index = 1
spgrid = 1

! initialize arrays
! loop over all indices nrrcp,NSTAB,NSEK,NTRAJ
DO ircp = 1, nrrcp
  DO istab = 1, NSTAB
    DO isek = 1, NSEK
      DO itraj = 1, NTRAJ
        cpri_class(istab,isek,itraj,ircp) = REAL(ircp*istab*isek*itraj)
        percvk_class(istab,isek,itraj,ircp) = DBLE(ircp*istab*isek*itraj)/DBLE(nrrcp*NSTAB*NSEK*NTRAJ)
      ENDDO
    ENDDO
  ENDDO
ENDDO

! set pltnam
pltnam = './level_1/resources/tst_m_ops_plot_class_uitv.plt'
csv_path = './level_1/resources/tst_m_ops_plot_class_uitv_NH3_classoutput.csv'

! delete test file if it exists
file_exists = chkexist(csv_path, error)
IF (file_exists) THEN
    ! open and delete the file
    file_exists = sysopen(fu_classoutput, csv_path, 'w', 'csv classoutput file', error)
    call assertFalse(error%haserror, "Pre-test: unable to open existing test file for deletion: "//error%message,__LINE__,__FILE__)
    CLOSE (UNIT=fu_classoutput, STATUS='DELETE')
    file_exists = chkexist(csv_path, error)
END IF
call assertFalse(file_exists, "Pre-test: test file exists, unable to remove",__LINE__,__FILE__)

! test 0 - error for spgrid != 2
CALL ops_plot_class_uitv_rcp(spgrid, nrrcp, namco, coneh, namrcp, xm, ym, &
                             cpri_class, percvk_class, ps_index, trafst, error)
call assertTrue(error%haserror, "No error for spgrid != 2, test 0",__LINE__,__FILE__)
error%haserror = .false.
error%message = ""

! test 1 - spgrid = 2
spgrid = 2
! save the classoutput file
CALL ops_plot_class_uitv_rcp(spgrid, nrrcp, namco, coneh, namrcp, xm, ym, &
                             cpri_class, percvk_class, ps_index, trafst, error)
if (error%haserror) call write_error(IOB_STDOUT, error)
call assertFalse(error%haserror, "Error test 1: "//error%message,__LINE__,__FILE__)
file_exists = chkexist(csv_path, error)
call assertFalse(error%haserror, "Error test 1, error chkexist: "//error%message,__LINE__,__FILE__)
call assertTrue(file_exists, "Error test 1, file_exists",__LINE__,__FILE__)

file_exists = sysopen(fu_classoutput, csv_path, 'w', 'csv classoutput file', error)
call assertTrue(file_exists, "Error test 1, file can't be opened",__LINE__,__FILE__)

! read first header line
READ(fu_classoutput, '(a)') header_line

header_line_ex = '"name", "x-coord", "y-coord", "conc.", "freq.", ' // &
                 '"ps_class", "stab_class", "wind_sec_class", "dist_class", ' // &
                 '"wind_sec_low", "wind_sec_up", "dist_low", "dist_up"'
call assertEqual(header_line_ex, TRIM(header_line), "Test 1, incorrect first header line",__LINE__,__FILE__)

! read second header line
READ(fu_classoutput, '(a)') header_line

header_line_ex = '"", "", "", "NH3", "", "", "", "", "", "", "", "", ""'
call assertEqual(header_line_ex, TRIM(header_line), "Test 1, incorrect second header line",__LINE__,__FILE__)

! read third header line
READ(fu_classoutput, '(a)') header_line

header_line_ex = '"", "m", "m", "ug/m3", "", "", "", "", "", "deg", "deg", "m", "m"'
call assertEqual(header_line_ex, TRIM(header_line), "Test 1, incorrect third header line",__LINE__,__FILE__)

! loop over all indices nrrcp,NSTAB,NSEK,NTRAJ
lineno = 4
DO ircp = 1, nrrcp
  DO istab = 1, NSTAB
    DO isek = 1, NSEK
      ! edges of wind sectors, bin width = 360/NSEK
      sec_low = ((isek-1)-0.5)*360/NSEK
      sec_up = sec_low + 360/NSEK

      DO itraj = 1, NTRAJ
        ! read next line
        READ(fu_classoutput, *) rnam, rx, ry, rconc, rfreq, rps, rstab, rsek, rtraj, rwlow, rwup, rdistlow, rdistup

        ! store line number as character for error messages
        WRITE(lineno_c, '(I8)') lineno
        
        ! assertions
        call assertEqual(namrcp(ircp), rnam, "name on line "//lineno_c,__LINE__,__FILE__)
        call assertEqual(NINT(xm(ircp)), rx, "x-coord on line "//lineno_c,__LINE__,__FILE__)
        call assertEqual(NINT(ym(ircp)), ry, "y-coord on line "//lineno_c,__LINE__,__FILE__)
        call assertEqual(cpri_class(istab,isek,itraj,ircp), rconc, tol, "conc. on line "//lineno_c,__LINE__,__FILE__)
        call assertEqual(REAL(percvk_class(istab,isek,itraj,ircp)), rfreq, tol, "freq. on line "//lineno_c,__LINE__,__FILE__)
        call assertEqual(ps_index, rps, "ps_class on line "//lineno_c,__LINE__,__FILE__)
        call assertEqual(ps_index, rps, "ps_class on line "//lineno_c,__LINE__,__FILE__)
        call assertEqual(istab, rstab, "stab_class on line "//lineno_c,__LINE__,__FILE__)
        call assertEqual(isek, rsek, "wind_sec_class on line "//lineno_c,__LINE__,__FILE__)
        call assertEqual(itraj, rtraj, "dist_class on line "//lineno_c,__LINE__,__FILE__)
        call assertEqual(sec_low, rwlow, tol, "wind_sec_low on line "//lineno_c,__LINE__,__FILE__)
        call assertEqual(sec_up, rwup, tol, "wind_sec_up on line "//lineno_c,__LINE__,__FILE__)
        call assertEqual(trafst_bins(itraj), rdistlow, tol, "dist_low on line "//lineno_c,__LINE__,__FILE__)
        call assertEqual(trafst_bins(itraj+1), rdistup, tol, "dist_up on line "//lineno_c,__LINE__,__FILE__)

        ! increment line number counter
        lineno = lineno + 1
      ENDDO
    ENDDO
  ENDDO
ENDDO

CALL sysclose(fu_classoutput, csv_path, error)

RETURN

END SUBROUTINE tst_ops_plot_class_uitv_rcp

end module m_tst_ops_plot_class_uitv_rcp

program p_tst_ops_plot_class_uitv_rcp
use m_tst_ops_plot_class_uitv_rcp
use no_pfunit
implicit none

call tst_ops_plot_class_uitv_rcp
call conclusion

end program p_tst_ops_plot_class_uitv_rcp
