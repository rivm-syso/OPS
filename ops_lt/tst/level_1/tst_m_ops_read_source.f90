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
! DESCRIPTION           : Read source file with emissions.
!                         Emissions are read from a source file and emissions for selected emission categories and countries 
!                         are then copied to an unformatted scratch file (line for line);
!                         emission parameters that lie outside a specified range generate an error.
!-------------------------------------------------------------------------------------------------------------------------------
module m_tst_ops_read_source

implicit none

contains

SUBROUTINE tst_ops_read_source

use no_pfunit
use m_ops_read_source
use m_commonfile, only: fu_scratch, fu_bron
use m_commonconst_lib, only: MAXDISTR
use m_error
use m_fileutils

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                 
PARAMETER    (ROUTINENAAM = 'tst_ops_read_source')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER                                          :: icm                        ! component nummer
LOGICAL                                          :: gasv                       ! component is gasuous       
INTEGER                                          :: ncatsel                    ! number of selected emission categories
INTEGER,   ALLOCATABLE                           :: catsel(:)                  ! selected emission categories
INTEGER                                          :: nlandsel                   ! number of selected emission countries
INTEGER,   ALLOCATABLE                           :: landsel(:)                 ! selected emission countries
LOGICAL,   ALLOCATABLE                           :: presentcode(:,:)           ! which distribution codes are present 
                                                                               ! presentcode(:,1): diurnal variations
                                                                               ! presentcode(:,2): particle size distributions
                                                                               ! presentcode(:,3): user-defined diurnal variation
                                                                               ! presentcode(:,4): user-defined particle size distributions
LOGICAL                                          :: allow_sigz0_point_source   ! allow initial sigma for point sources                                                                               

! SUBROUTINE ARGUMENTS - OUTPUT
! Note: emission parameters are written to scratch file and are not part of the output arguments
INTEGER                                          :: numbron                    ! number of (selected) sources
LOGICAL                                          :: building_present1          ! at least one building is present in the source file   
TYPE (TError)                                    :: error                      ! Error handling record
allocate(presentcode(MAXDISTR,4))
!-------------------------------------------------------------------------------------------------------------------------------
! Read brnam, the file with sources. Note that this file (and other files later on) are also closed if an error occurred during
! reading, therefore the error check is not before the close statement.
IF (.NOT.sysopen(fu_bron, './level_1/resources/brn1.brn', 'r', 'emission file', error)) GOTO 9999

! Open scratch file:
OPEN(fu_scratch, STATUS = 'SCRATCH', FORM = 'UNFORMATTED')

! Set input parameters:
icm           = 2
gasv          = .true. 
ncatsel       = 2
allocate(catsel(ncatsel))
catsel        = (/ 1200, 3111 /)
nlandsel      = 3
allocate(landsel(nlandsel))
landsel       = (/ 528, 100, 222 /)
presentcode   = .true.  ! check for presentcode in unit test of m_ops_emis in library

! Call routine:
call ops_read_source(icm, gasv, ncatsel, catsel, nlandsel, landsel, presentcode, &
                     allow_sigz0_point_source, numbron, building_present1, error)

! Check output - see emission file brn1.brn at level_1/resources; 
! note that the scratch file content is not checked here, but in the library unit tests
call assertEqual(5,numbron,'numbron',__LINE__,__FILE__)
call assertEqual(.false.,building_present1, 'building_present1',__LINE__,__FILE__)
call assertEqual(.false.,error%haserror, 'error%haserror',__LINE__,__FILE__)

! Check error handling:
9999 CALL ErrorCall(ROUTINENAAM, error)
call assertFalse(error%haserror,'tst_ops_read_source: unexpected error has occurred: '//error%message,__LINE__,__FILE__)

END SUBROUTINE tst_ops_read_source

end module m_tst_ops_read_source

program p_tst_ops_read_source

use no_pfunit
use m_tst_ops_read_source

call tst_ops_read_source
call conclusion

end program p_tst_ops_read_source



