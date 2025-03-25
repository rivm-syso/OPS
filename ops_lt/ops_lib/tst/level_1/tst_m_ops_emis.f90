module m_tst_ops_emis

implicit none

contains

SUBROUTINE tst_check_building_param
use no_pfunit
use m_ops_emis_private

USE m_error
USE m_ops_utils, only: is_missing
USE m_commonconst_lib, only: EPS_DELTA
use m_ops_building
USE m_commonfile, only: fu_log, lognam
USE m_ops_logfile

implicit none

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                
PARAMETER    (ROUTINENAAM = 'check_building_param')

! Input:
type(Tbuilding) :: building       ! structure with building parameters
type(Tbuilding) :: building2      ! structure with building parameters
real :: hbron                     ! emission height at source (stack height), without plume rise [m]
real :: qww                       ! heat content [MW]
real :: D_stack                   ! stack diameter [m]
real :: D_stack_outer             ! outer stack diameter [m]
real :: V_stack                   ! exit velocity [m/s]
integer :: brn_version            ! version of brn file

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError) :: error            ! error handling record

! Open log file:
lognam = 'test.log'
open(unit = fu_log, file = lognam)

!----------------------------------------------------------
! Test 1: allowed value, no error
!----------------------------------------------------------
building%length = 80.0 ; building%width = 20.0; building%height = 12.0; building%orientation = 85.0
hbron = 13.0; qww = 1.9; V_stack = 1.2 ; D_stack = 4.3; D_stack_outer = -999.0
brn_version = 4
call check_building_param(brn_version, building, hbron, qww, D_stack, D_stack_outer, V_stack, error)
! Error: Could not open file for writing
call assertFalse(error%haserror, 'Test 1: allowed value',__LINE__,__FILE__)

!----------------------------------------------------------
! Test 2: allowed value, no error, BRN version 5
!----------------------------------------------------------
building%length = 80.0 ; building%width = 20.0; building%height = 12.0; building%orientation = 85.0
building%x = 123456 ; building%y = 3123456
hbron = 13.0; qww = 1.9; V_stack = 1.2 ; D_stack = 4.3; D_stack_outer = 5.0
brn_version = 5
call check_building_param(brn_version, building, hbron, qww, D_stack, D_stack_outer, V_stack, error)
! Error: Could not open file for writing
call assertFalse(error%haserror, 'Test 2: allowed value',__LINE__,__FILE__)

!----------------------------------------------------------
! Test 3: not allowed value, error, BRN version 5
!----------------------------------------------------------
building2%length = 80.0 ; building2%width = 20.0; building2%height = 12.0; building2%orientation = 85.0
building2%x = 123456 ; building2%y = -999
hbron = 13.0; qww = 1.9; V_stack = 1.2 ; D_stack = 4.3; D_stack_outer = 5.0
brn_version = 5
call check_building_param(brn_version, building2, hbron, qww, D_stack, D_stack_outer, V_stack, error)
! Error: Could not open file for writing
call assertEqual('If building is present in BRN version 5, then building location must be specified', error%message, 'Test 3: error message:',__LINE__,__FILE__)
call assertTrue(error%haserror, 'Test 3: not allowed value',__LINE__,__FILE__)
! reset error
error%haserror = .FALSE.
error%message = ''

!----------------------------------------------------------
! Test 4: not allowed value, error, BRN version 5
!----------------------------------------------------------
building2%length = 80.0 ; building2%width = 20.0; building2%height = 12.0; building2%orientation = 85.0
building2%x = 123456 ; building2%y = 234567
hbron = 13.0; qww = 1.9; V_stack = 1.2 ; D_stack = 4.3; D_stack_outer = -999.0
brn_version = 5
call check_building_param(brn_version, building2, hbron, qww, D_stack, D_stack_outer, V_stack, error)
! Error: Could not open file for writing
call assertEqual('If building is present in BRN version 5, then D_stack_outer must be specified', error%message, 'Test 3: error message:',__LINE__,__FILE__)
call assertTrue(error%haserror, 'Test 4: not allowed value',__LINE__,__FILE__)
! reset error
error%haserror = .FALSE.
error%message = ''

!----------------------------------------------------------
! Test 5: no building? Then D_stack_outer not required: BRN version 5
!----------------------------------------------------------
building2%length = -999 ; building2%width = -999 ; building2%height = -999 ; building2%orientation = -999
building2%x = -999 ; building2%y = -999
hbron = 13.0; qww = 1.9; V_stack = 1.2 ; D_stack = 4.3; D_stack_outer = -999.0
brn_version = 5
call check_building_param(brn_version, building2, hbron, qww, D_stack, D_stack_outer, V_stack, error)
call assertFalse(error%haserror, 'Test 5: allowed value',__LINE__,__FILE__)

! close the log file
call ops_closelog(error)
if (error%haserror) then
    CALL ErrorCall(ROUTINENAAM, error)
    call assertFalse(error%haserror,'ops_closelog: unexpected error has occurred: '//error%message,__LINE__,__FILE__)
endif

close(fu_log)

END SUBROUTINE tst_check_building_param

SUBROUTINE tst_check_stack_param

use no_pfunit
use m_ops_emis_private

USE m_error
USE m_ops_utils, only: is_missing
USE m_commonconst_lib, only: EPS_DELTA

implicit none

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                
PARAMETER    (ROUTINENAAM = 'tst_check_stack_param')

! SUBROUTINE ARGUMENTS - INPUT
real :: qww                        ! heat content[ MW]
logical :: VsDs_opt                ! read stack parameters Ds/Vs/Ts from source file
real :: D_stack                    ! diameter of the stack [m]
real :: D_stack_outer              ! outer diameter of the stack [m]
real :: V_stack                    ! exit velocity of plume at stack tip [m/s]
real :: Ts_stack_C                 ! temperature of effluent from stack [C]

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError) :: error                      ! error handling record

!----------------------------------------------------------
! Test 1: allowed value, no error; qww & Ts_stack_C
!----------------------------------------------------------
qww = 1.8; VsDs_opt = .FALSE.; D_stack = -999.; V_stack = -999.; Ts_stack_C = 45.
D_stack_outer = -999.
call check_stack_param(qww, VsDs_opt, D_stack, D_stack_outer, V_stack, Ts_stack_C, error)
call assertFalse(error%haserror, 'Test 1: allowed value',__LINE__,__FILE__)

!----------------------------------------------------------
! Test 2; allowed value, no error; VsDs_opt = .TRUE.
!----------------------------------------------------------
qww = -999.; VsDs_opt = .TRUE.; D_stack = 12.; V_stack = 0.8; Ts_stack_C = 45.
call check_stack_param(qww, VsDs_opt, D_stack, D_stack_outer, V_stack, Ts_stack_C, error)
call assertFalse(error%haserror, 'Test 2: allowed value',__LINE__,__FILE__)

!----------------------------------------------------------
! Test 3: missing qww
!----------------------------------------------------------
qww = -999.; VsDs_opt = .FALSE.; D_stack = -999.; V_stack = -999.; Ts_stack_C = 45.
call check_stack_param(qww, VsDs_opt, D_stack, D_stack_outer, V_stack, Ts_stack_C, error)
call assertTrue(error%haserror, 'Test 3: error:',__LINE__,__FILE__)
call assertEqual('Heat content (Qw) must be specified', error%message, 'Test 3: error message:',__LINE__,__FILE__)
! reset error
error%haserror = .FALSE.
error%message = ''

!----------------------------------------------------------
! Test 4; VsDs_opt = .TRUE., missing Ts_stack_C & qww
!----------------------------------------------------------
qww = -999.; VsDs_opt = .TRUE.; D_stack = 12.; V_stack = 0.8; Ts_stack_C = -999.
call check_stack_param(qww, VsDs_opt, D_stack, D_stack_outer, V_stack, Ts_stack_C, error)
call assertTrue(error%haserror, 'Test 4: error:',__LINE__,__FILE__)
call assertEqual('One of heat content (Qw) or temperature effluent gas (Ts_stack_C) must be specified, other must be -999.', error%message, 'Test 4: error message:',__LINE__,__FILE__)
! reset error
error%haserror = .FALSE.
error%message = ''

!----------------------------------------------------------
! Test 5; VsDs_opt = .TRUE., both Ts_stack_C & qww present
!----------------------------------------------------------
qww = 1.8; VsDs_opt = .TRUE.; D_stack = 12.; V_stack = 0.8; Ts_stack_C = 45.
call check_stack_param(qww, VsDs_opt, D_stack, D_stack_outer, V_stack, Ts_stack_C, error)
call assertTrue(error%haserror, 'Test 5: error:',__LINE__,__FILE__)
call assertEqual('One of heat content (Qw) or temperature effluent gas (Ts_stack_C) must be specified, other must be -999.', error%message, 'Test 5: error message:',__LINE__,__FILE__)
! reset error
error%haserror = .FALSE.
error%message = ''

!----------------------------------------------------------
! Test 6; V_stack == 0, Qw > 0 = error
!----------------------------------------------------------
qww = 1.8; VsDs_opt = .TRUE.; D_stack = 12.; V_stack = 0.; Ts_stack_C = -999.
call check_stack_param(qww, VsDs_opt, D_stack, D_stack_outer, V_stack, Ts_stack_C, error)
call assertTrue(error%haserror, 'Test 6: error:',__LINE__,__FILE__)
call assertEqual('If exit velocity (V_stack) is zero, then heat content (Qw) must be zero also. Use V_stack = -999. if you only want to specify Qw.', error%message, 'Test 6: error message:',__LINE__,__FILE__)
! reset error
error%haserror = .FALSE.
error%message = ''

!----------------------------------------------------------
! Test 7; D_stack_outer requires D_stack
!----------------------------------------------------------
qww = 1.8; VsDs_opt = .TRUE.; D_stack = 12.; V_stack = -999; Ts_stack_C = -999.
D_stack= -999. ; D_stack_outer = 5.0
call check_stack_param(qww, VsDs_opt, D_stack, D_stack_outer, V_stack, Ts_stack_C, error)
call assertTrue(error%haserror, 'Test 7: error: D_stack_outer should be used',__LINE__,__FILE__)
call assertEqual('D_stack is required to use D_stack_outer Specify D_stack', error%message, 'Test 7: error message:',__LINE__,__FILE__)
! reset error
error%haserror = .FALSE.
error%message = ''

!----------------------------------------------------------
! Test 8; D_stack_outer > D_stack
!----------------------------------------------------------
qww = 1.8; VsDs_opt = .TRUE.; D_stack = 12.; V_stack = -999; Ts_stack_C = -999.
D_stack_outer = 0.9 * D_stack
call check_stack_param(qww, VsDs_opt, D_stack, D_stack_outer, V_stack, Ts_stack_C, error)
call assertTrue(error%haserror, 'Test 8: error: D_stack_outer too small',__LINE__,__FILE__)
call assertEqual('<outer diameter stack [m]>  outside permitted range', error%message, 'Test 8: error message:',__LINE__,__FILE__)
! reset error
error%haserror = .FALSE.
error%message = ''

END SUBROUTINE tst_check_stack_param

SUBROUTINE tst_check_verdeling
use no_pfunit
use m_ops_emis_private

USE m_error
USE m_commonconst_lib, only: MAXDISTR

implicit none

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                
PARAMETER    (ROUTINENAAM = 'check_verdeling')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER   :: icode                      ! code that has to be checked; 
LOGICAL, allocatable :: presentcode(:,:)
INTEGER   :: stdclass                   ! index of standard distributions in 2nd dimension of presentcode
INTEGER   :: usdclass                   ! index of user defined distributions in 2nd dimension of presentcode
CHARACTER*48 :: parname                    ! parameter name in error messages

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError) :: error                      ! error handling record
allocate(presentcode(MAXDISTR,4))

! set all to .TRUE. so tests succeed
presentcode(:,:) = .TRUE.

!----------------------------------------------------------
! Test 1: allowed value, no error
!----------------------------------------------------------
icode = 1; stdclass = 2; usdclass = 4; parname = 'idgr'
CALL check_verdeling(icode, presentcode, stdclass, usdclass, parname, error)
call assertFalse(error%haserror, 'Test 1: allowed value',__LINE__,__FILE__)

!----------------------------------------------------------
! Test 2: idgr == 0, error
!----------------------------------------------------------
icode = 0; stdclass = 2; usdclass = 4; parname = 'idgr'
CALL check_verdeling(icode, presentcode, stdclass, usdclass, parname, error)
call assertTrue(error%haserror, 'Test 2: allowed value',__LINE__,__FILE__)
call assertEqual('It is not permitted to use code 0 for idgr', error%message, 'Test 2: error message:',__LINE__,__FILE__)
! reset error
error%haserror = .FALSE.
error%message = ''

!----------------------------------------------------------
! Test 3: presentcode(icode, klasse) = .FALSE. (icode > 0)
!----------------------------------------------------------
icode = 1; stdclass = 2; usdclass = 4; parname = 'idgr'
presentcode(ABS(icode), stdclass) = .FALSE.
CALL check_verdeling(icode, presentcode, stdclass, usdclass, parname, error)
call assertTrue(error%haserror, 'Test 3: invalid',__LINE__,__FILE__)
call assertEqual('No distribution available for this code of idgr. Note: use minus sign for user defined codes.', error%message, 'Test 3: error message:',__LINE__,__FILE__)
! reset error
error%haserror = .FALSE.
error%message = ''

!----------------------------------------------------------
! Test 4: presentcode(icode, klasse) = .FALSE. (icode < 0)
!----------------------------------------------------------
icode = -1; stdclass = 2; usdclass = 4; parname = 'idgr'
presentcode(:,:) = .TRUE. ! reset
presentcode(ABS(icode), usdclass) = .FALSE.
CALL check_verdeling(icode, presentcode, stdclass, usdclass, parname, error)
call assertTrue(error%haserror, 'Test 4: invalid',__LINE__,__FILE__)
call assertEqual('No distribution available for this code of idgr. Note: use minus sign for user defined codes.', error%message, 'Test 4: error message:',__LINE__,__FILE__)
! reset error
error%haserror = .FALSE.
error%message = ''

END SUBROUTINE tst_check_verdeling

SUBROUTINE tst_check_source3

use no_pfunit
use m_ops_emis_private

USE m_error
USE m_commonconst_lib, only: EPS_DELTA
USE m_commonfile, only: fu_log, lognam

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM               
PARAMETER    (ROUTINENAAM = 'tst_check_source3')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*48 :: warning1                   ! first part of warning
CHARACTER*48 :: varnaam                    ! variable to be checked
REAL   :: onder                      ! lower limit
REAL   :: boven                      ! upper limit
REAL   :: varwaarde                  ! value of variable

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError) :: error                      ! error handling record

! Open log file:
lognam = 'test.log'
open(unit = fu_log, file = lognam, access = 'append')

!----------------------------------------------------------
! Test 1: allowed value, no error
!----------------------------------------------------------
varnaam = '<inner diameter stack [m]>'; warning1 = 'test'
onder = 0.01; boven = 30.0; varwaarde = 5.6
CALL check_source3(warning1, varnaam, onder, boven, varwaarde, error)
call assertFalse(error%haserror, 'Test 1: allowed value',__LINE__,__FILE__)

!----------------------------------------------------------
! Test 2: should give an error
!----------------------------------------------------------
varnaam = '<inner diameter stack [m]>'; warning1 = 'test'
onder = 0.01; boven = 30.0; varwaarde = 38.0
CALL check_source3(warning1, varnaam, onder, boven, varwaarde, error)
if (error%haserror) then
    call assertFalse(error%haserror,'Test 2: error has occurred instead of warning: '//error%message,__LINE__,__FILE__)
else
   call assertEqual('test; <inner diameter stack [m]>  outside permitted range', error%message,'Test 2: warning message:',__LINE__,__FILE__)
endif
! reset error
error%haserror = .FALSE.
error%message = ''

!----------------------------------------------------------
! Test 3: should give an error
!----------------------------------------------------------
varnaam = '<inner diameter stack [m]>'; warning1 = 'test'
onder = 0.01; boven = 30.0; varwaarde = 0.005
CALL check_source3(warning1, varnaam, onder, boven, varwaarde, error)
if (error%haserror) then
    call assertFalse(error%haserror,'Test 3: error has occurred instead of warning: '//error%message,__LINE__,__FILE__)
else
   call assertEqual('test; <inner diameter stack [m]>  outside permitted range', error%message,'Test 3: warning message:',__LINE__,__FILE__)
endif
! reset error
error%haserror = .FALSE.
error%message = ''

close(fu_log)

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)
call assertFalse(error%haserror,'tst_check_source3: unexpected error has occurred: '//error%message,__LINE__,__FILE__)

END SUBROUTINE tst_check_source3

SUBROUTINE tst_check_source2

use no_pfunit
use m_ops_emis_private
USE m_error

implicit none

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM               
PARAMETER    (ROUTINENAAM = 'tst_check_source2')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*48 :: varnaam                    ! variable to be checked
REAL   :: onder                      ! lower limit
REAL   :: boven                      ! upper limit
REAL   :: varwaarde                  ! value of variable

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError) :: error                      ! error handling record

!----------------------------------------------------------
! Test 1: allowed value, no error
!----------------------------------------------------------
varnaam = '<inner diameter stack [m]>'
onder = 0.01; boven = 30.0; varwaarde = 5.6
CALL check_source2(varnaam, onder, boven, varwaarde, error)
call assertFalse(error%haserror, 'Test 1: allowed value',__LINE__,__FILE__)

!----------------------------------------------------------
! Test 2: should give an error
!----------------------------------------------------------
varnaam = '<inner diameter stack [m]>'
onder = 0.01; boven = 30.0; varwaarde = 38.0
CALL check_source2(varnaam, onder, boven, varwaarde, error)
if (error%haserror) then
   call assertEqual('<inner diameter stack [m]>  outside permitted range', error%message, 'Test 2: wrong error message:',__LINE__,__FILE__)
else
   call assertTrue(error%haserror,'Test 2: no error detected:',__LINE__,__FILE__)
endif
! reset error
error%haserror = .FALSE.
error%message = ''

!----------------------------------------------------------
! Test 3: should give an error
!----------------------------------------------------------
varnaam = '<inner diameter stack [m]>'
onder = 0.01; boven = 30.0; varwaarde = 0.005
CALL check_source2(varnaam, onder, boven, varwaarde, error)
if (error%haserror) then
   call assertEqual('<inner diameter stack [m]>  outside permitted range', error%message, 'Test 3: wrong error message:',__LINE__,__FILE__)
else
   call assertTrue(error%haserror,'Test 3: no error detected:',__LINE__,__FILE__)
endif
! reset error
error%haserror = .FALSE.
error%message = ''

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)
call assertFalse(error%haserror,'tst_check_source2: unexpected error has occurred: '//error%message,__LINE__,__FILE__)

END SUBROUTINE tst_check_source2

SUBROUTINE tst_check_isource2

use no_pfunit
use m_ops_emis_private
USE m_error

implicit none

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM               
PARAMETER    (ROUTINENAAM = 'tst_check_isource2')

! SUBROUTINE ARGUMENTS - INPUT
CHARACTER*48 :: varnaam                    ! variable to be checked
INTEGER   :: onder                      ! lower limit
INTEGER   :: boven                      ! upper limit
INTEGER   :: varwaarde                  ! value of variable

! SUBROUTINE ARGUMENTS - I/O
TYPE (TError) :: error                      ! error handling record

!----------------------------------------------------------
! Test 1: allowed value, no error
!----------------------------------------------------------
varnaam = '<inner diameter stack [m]>'
onder = 1; boven = 30; varwaarde = 6
CALL check_isource2(varnaam, onder, boven, varwaarde, error)
call assertFalse(error%haserror, 'Test 1: allowed value',__LINE__,__FILE__)

!----------------------------------------------------------
! Test 2: should give an error
!----------------------------------------------------------
varnaam = '<inner diameter stack [m]>'
onder = 1; boven = 30; varwaarde = 36
CALL check_isource2(varnaam, onder, boven, varwaarde, error)
if (error%haserror) then
   call assertEqual('<inner diameter stack [m]>  outside permitted range', error%message, 'Test 2: wrong error message:',__LINE__,__FILE__)
else
   call assertTrue(error%haserror,'Test 2: no error detected:',__LINE__,__FILE__)
endif
! reset error
error%haserror = .FALSE.
error%message = ''

!----------------------------------------------------------
! Test 3: should give an error
!----------------------------------------------------------
varnaam = '<inner diameter stack [m]>'
onder = 1; boven = 30; varwaarde = 0
CALL check_isource2(varnaam, onder, boven, varwaarde, error)
if (error%haserror) then
   call assertEqual('<inner diameter stack [m]>  outside permitted range', error%message, 'Test 3: wrong error message:',__LINE__,__FILE__)
else
   call assertTrue(error%haserror,'Test 3: no error detected:',__LINE__,__FILE__)
endif
! reset error
error%haserror = .FALSE.
error%message = ''

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)
call assertFalse(error%haserror,'tst_check_isource2: unexpected error has occurred: '//error%message,__LINE__,__FILE__)

END SUBROUTINE tst_check_isource2

SUBROUTINE tst_check_isource

use no_pfunit
use m_ops_emis_private
USE m_commonfile, only: fu_log, lognam
USE m_ops_logfile

USE m_error

implicit none

CHARACTER*512                                    :: ROUTINENAAM                
PARAMETER    (ROUTINENAAM = 'tst_check_isource')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER   :: nr                         ! record number of source file
CHARACTER*48 :: varnaam                    ! variable to be checked
INTEGER   :: onder                      ! lower limit
INTEGER   :: boven                      ! upper limit

! SUBROUTINE ARGUMENTS - I/O
INTEGER   :: varwaarde                  ! (adapted) value of variable
TYPE (TError) :: error                      ! error handling record

! Open log file:
lognam = 'test.log'
open(unit = fu_log, file = lognam, access = 'append')

!---------------------------------------------------------------
! Test 1: allowed value
!---------------------------------------------------------------
nr = 1; varnaam = '<emission strength [g/s]>'
onder = 0; boven = 99999; varwaarde = 133
CALL check_isource (nr, varnaam, onder, boven, varwaarde, error)
IF (error%haserror) GOTO 9999
call assertEqual(133, varwaarde,'Test 1: allowed:',__LINE__,__FILE__)

!---------------------------------------------------------------
! Test 2: too low
!---------------------------------------------------------------
nr = 1; varnaam = '<emission strength [g/s]>'
onder = 0; boven = 99999; varwaarde = -133
CALL check_isource (nr, varnaam, onder, boven, varwaarde, error)
IF (error%haserror) GOTO 9999
call assertEqual(0, varwaarde,'Test 2: too low:',__LINE__,__FILE__)

!---------------------------------------------------------------
! Test 3: too high
!---------------------------------------------------------------
nr = 1; varnaam = '<emission strength [g/s]>'
onder = 0; boven = 99999; varwaarde = 100000
CALL check_isource (nr, varnaam, onder, boven, varwaarde, error)
IF (error%haserror) GOTO 9999
call assertEqual(99999, varwaarde,'Test 3: too high:',__LINE__,__FILE__)

! close the log file
call ops_closelog(error)
if (error%haserror) then
    CALL ErrorCall(ROUTINENAAM, error)
    call assertFalse(error%haserror,'ops_closelog: unexpected error has occurred: '//error%message,__LINE__,__FILE__)
endif

close(fu_log)

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)
call assertFalse(error%haserror,'tst_check_isource: unexpected error has occurred: '//error%message,__LINE__,__FILE__)

END SUBROUTINE tst_check_isource

SUBROUTINE tst_check_source

use no_pfunit
use m_ops_emis_private
USE m_commonfile, only: fu_log, lognam
USE m_ops_logfile

USE m_error

implicit none

CHARACTER*512                                    :: ROUTINENAAM                
PARAMETER    (ROUTINENAAM = 'tst_check_source')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER   :: nr                         ! record number of source file
CHARACTER*48 :: varnaam                    ! variable to be checked
REAL   :: onder                      ! lower limit
REAL   :: boven                      ! upper limit

! SUBROUTINE ARGUMENTS - I/O
REAL   :: varwaarde                  ! (adapted) value of variable
TYPE (TError) :: error                      ! error handling record
real              :: tol = 1.0e-5               ! tolerance for equality of reals

! Open log file:
lognam = 'test.log'
open(unit = fu_log, file = lognam, access = 'append')

!---------------------------------------------------------------
! Test 1: allowed value
!---------------------------------------------------------------
nr = 1; varnaam = '<emission strength [g/s]>'
onder = 0.; boven = 99999.; varwaarde = 133.
CALL check_source (nr, varnaam, onder, boven, varwaarde, error)
IF (error%haserror) GOTO 9999
call assertEqual(133., varwaarde, tol, 'Test 1: allowed:',__LINE__,__FILE__)

!---------------------------------------------------------------
! Test 2: too low
!---------------------------------------------------------------
nr = 1; varnaam = '<emission strength [g/s]>'
onder = 0.; boven = 99999.; varwaarde = -133.
CALL check_source (nr, varnaam, onder, boven, varwaarde, error)
IF (error%haserror) GOTO 9999
call assertEqual(0., varwaarde, tol, 'Test 2: too low:',__LINE__,__FILE__)

!---------------------------------------------------------------
! Test 3: too high
!---------------------------------------------------------------
nr = 1; varnaam = '<emission strength [g/s]>'
onder = 0.; boven = 99999.; varwaarde = 100000.
CALL check_source (nr, varnaam, onder, boven, varwaarde, error)
IF (error%haserror) GOTO 9999
call assertEqual(99999., varwaarde, tol, 'Test 3: too high:',__LINE__,__FILE__)

! close the log file
call ops_closelog(error)
if (error%haserror) then
    CALL ErrorCall(ROUTINENAAM, error)
    call assertFalse(error%haserror,'ops_closelog: unexpected error has occurred: '//error%message,__LINE__,__FILE__)
endif

close(fu_log)

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)
call assertFalse(error%haserror,'tst_check_source: unexpected error has occurred: '//error%message,__LINE__,__FILE__)

END SUBROUTINE tst_check_source

subroutine tst_ops_emis_read_header

use m_ops_emis

implicit none

CHARACTER*512 :: brnfile                   ! Example .brn file
CHARACTER*512 :: error_msg

!-----------------------------------------------------------------------------
! Test 1: correct brn file, version = 1
!-----------------------------------------------------------------------------
brnfile = './level_1/resources/brn1.brn'
error_msg = ''
call chk_result_read_header(1, brnfile, 1, .FALSE., 2, 0, error_msg)

!-----------------------------------------------------------------------------
! Test 2: incorrect brn file, type 1
!-----------------------------------------------------------------------------
brnfile = './level_1/resources/brn_error1.brn'
error_msg = 'Error while reading BRN-VERSION version_number in first line of header'
call chk_result_read_header(2, brnfile, 0, .FALSE., 1, 0, error_msg)

!-----------------------------------------------------------------------------
! Test 3: incorrect brn file, type 2
!-----------------------------------------------------------------------------
brnfile = './level_1/resources/brn_error2.brn'
error_msg = 'Cannot find BRN-VERSION in first line of header'
call chk_result_read_header(3, brnfile, 0, .FALSE., 1, 0, error_msg)

!-----------------------------------------------------------------------------
! Test 4: brn version 0
!-----------------------------------------------------------------------------
brnfile = './level_1/resources/brn2.brn'
error_msg = ''
call chk_result_read_header(4, brnfile, 0, .FALSE., 1, 0, error_msg)

!-----------------------------------------------------------------------------
! Test 5: wrong BRN version
!-----------------------------------------------------------------------------
brnfile = './level_1/resources/brn_error3.brn'
error_msg = '<BRN-version>  outside permitted range'
call chk_result_read_header(6, brnfile, 6, .TRUE., 1, 0, error_msg)

end subroutine tst_ops_emis_read_header

!--------------------------------------------------------------------------------------------------
SUBROUTINE tst_ops_emis_read_annual1

use m_ops_emis
USE m_error
USE m_fileutils
USE m_geoutils
USE m_commonconst_lib, only: EPS_DELTA, HUMAX, MAXDISTR, ncolBuildingEffectTable
USE Binas, only: T0
use m_ops_utils, only: is_missing
use m_ops_building
USE m_commonfile, only: fu_bron

IMPLICIT NONE

! CONSTANTS
CHARACTER*512                                    :: ROUTINENAAM                 
PARAMETER    (ROUTINENAAM = 'tst_ops_emis_read_annual1')

! SUBROUTINE ARGUMENTS - INPUT
INTEGER :: icm                        ! component nummer
LOGICAL :: check_psd                  ! check whether particle size distribution has been read
LOGICAL, allocatable :: presentcode(:,:)

! SUBROUTINE ARGUMENTS - INPUT/OUTPUT
INTEGER :: nrec                       ! record number of source file
INTEGER :: numbron                    ! number of (selected) source
LOGICAL :: building_present1          ! at least one building is present in the source file   

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER :: mm                         ! source identification number [-]
REAL    :: x                          ! x coordinate of source location (RDM [m])                 
REAL    :: y                          ! y coordinate of source location (RDM [m])
REAL    :: qob                        ! emission strength [g/s]
REAL    :: qww                        ! heat content [MW]
REAL    :: hbron                      ! emission height at source (stack height), without plume rise [m]
REAL    :: diameter                   ! diameter area source (NOT stack diameter) [m]
REAL    :: szopp                      ! deviation emission height for area source = initial sigma_z [m]
real    :: D_stack                    ! diameter of the stack [m]
real    :: D_stack_outer              ! outer diameter of the stack [m]
real    :: V_stack                    ! exit velocity of plume at stack tip [m/s]
real    :: Ts_stack                   ! temperature of effluent from stack [K]
logical :: emis_horizontal            ! horizontal outflow of emission
type(Tbuilding) :: building                   ! structure with building parameters
INTEGER :: ibtg                       ! diurnal emission variation code [-]
INTEGER :: ibroncat                   ! emission category code [-]
INTEGER :: iland                      ! country/area code [-]
INTEGER :: idgr                       ! particle size distribution code [-]
LOGICAL :: end_of_file                ! end of file has been reached

CHARACTER*512 :: brnfile                   ! Example .brn file
CHARACTER*512 :: error_msg
allocate( presentcode(MAXDISTR,4))    ! which distribution codes are present

!-----------------------------------------------------------------------------
! Test 1: correct brn file, version = 1
!-----------------------------------------------------------------------------
brnfile = './level_1/resources/brn1.brn'
error_msg = ''
! PM10
icm = 24 ; check_psd = .TRUE. ; presentcode(:, :) = .TRUE.
! expected values
nrec = 3; building_present1 = .FALSE.; numbron = 0; mm = 1
x = 155000.; y = 385000.; qob = 1.; qww = 0.; hbron = 40.
diameter = 0.; szopp = 0.; D_stack = -999.; D_stack_outer = -999.; V_stack = -999.; Ts_stack = -999.
emis_horizontal = .FALSE.; ibtg = -1; ibroncat = 3111; iland = 528; idgr = 2
end_of_file = .FALSE.
building%length = -999.0
building%width  = -999.0
building%height = -999.0
building%orientation = -999.0
call chk_result_read_annual1(1, brnfile, icm, check_psd, presentcode, nrec, numbron, building_present1, mm, x, y, qob, qww, hbron, diameter, szopp, D_stack, D_stack_outer, V_stack, Ts_stack, emis_horizontal, building, ibtg, ibroncat, iland, idgr, end_of_file, error_msg)

!-----------------------------------------------------------------------------
! Test 2: same as in test 1, brn version 0
!-----------------------------------------------------------------------------
brnfile = './level_1/resources/brn2.brn'
error_msg = ''
! PM10
icm = 24 ; check_psd = .TRUE. ; presentcode(:, :) = .TRUE.
! expected values
nrec = 2; building_present1 = .FALSE.; numbron = 0; mm = 1
x = 155000.; y = 385000.; qob = 1.; qww = 0.; hbron = 40.
diameter = 0.; szopp = 0.; D_stack = -999.; D_stack_outer = -999.; V_stack = -999.; Ts_stack = -999.
emis_horizontal = .FALSE.; ibtg = -1; ibroncat = 3111; iland = 528; idgr = 2
end_of_file = .FALSE.
building%length = -999.0
building%width  = -999.0
building%height = -999.0
building%orientation = -999.0
call chk_result_read_annual1(2, brnfile, icm, check_psd, presentcode, nrec, numbron, building_present1, mm, x, y, qob, qww, hbron, diameter, szopp, D_stack, D_stack_outer, V_stack, Ts_stack, emis_horizontal, building, ibtg, ibroncat, iland, idgr, end_of_file, error_msg)

!-----------------------------------------------------------------------------
! Test 3: multiple sources
!-----------------------------------------------------------------------------
brnfile = './level_1/resources/brn3.brn'
error_msg = ''
! PM10
icm = 24 ; check_psd = .TRUE. ; presentcode(:, :) = .TRUE.
! expected values
nrec = 3; building_present1 = .FALSE.; numbron = 0; mm = 1
x = 155000.; y = 385000.; qob = 1.; qww = 0.; hbron = 40.
diameter = 0.; szopp = 0.; D_stack = -999.; D_stack_outer = -999.; V_stack = -999.; Ts_stack = -999.
emis_horizontal = .FALSE.; ibtg = -1; ibroncat = 3111; iland = 528; idgr = 2
end_of_file = .FALSE.
building%length = -999.0
building%width  = -999.0
building%height = -999.0
building%orientation = -999.0
call chk_result_read_annual1(3, brnfile, icm, check_psd, presentcode, nrec, numbron, building_present1, mm, x, y, qob, qww, hbron, diameter, szopp, D_stack, D_stack_outer, V_stack, Ts_stack, emis_horizontal, building, ibtg, ibroncat, iland, idgr, end_of_file, error_msg)

!-----------------------------------------------------------------------------
! Test 4: error in emission record
!-----------------------------------------------------------------------------
brnfile = './level_1/resources/brn_error4.brn'
error_msg = 'Error when reading record from emission file; see compiler manual for IO errors'
! Other settings the same as previous
call chk_result_read_annual1(4, brnfile, icm, check_psd, presentcode, nrec, numbron, building_present1, mm, x, y, qob, qww, hbron, diameter, szopp, D_stack, D_stack_outer, V_stack, Ts_stack, emis_horizontal, building, ibtg, ibroncat, iland, idgr, end_of_file, error_msg)

!-----------------------------------------------------------------------------
! Test 5: error in emission record for D_outer
!-----------------------------------------------------------------------------
brnfile = './level_1/resources/brn_error5.brn'
nrec = 3; building_present1 = .TRUE.; numbron = 0; mm = 1
x = 155000.; y = 385000.; qob = 1.; qww = 0.; hbron = 15.
diameter = 0.; szopp = 0.; D_stack = 1.5; D_stack_outer = 2.0; V_stack = -999.; Ts_stack = -999.
emis_horizontal = .FALSE.; ibtg = -1; ibroncat = 3111; iland = 528; idgr = 2
end_of_file = .FALSE.
building%length = 20.0
building%width  = 20.0
building%height = 10.0
building%orientation = 50.0
error_msg = 'If building is present in BRN version 5, then D_stack_outer must be specified'
call chk_result_read_annual1(5, brnfile, icm, check_psd, presentcode, nrec, numbron, building_present1, mm, x, y, qob, qww, hbron, diameter, szopp, D_stack, D_stack_outer, V_stack, Ts_stack, emis_horizontal, building, ibtg, ibroncat, iland, idgr, end_of_file, error_msg)

!-----------------------------------------------------------------------------
! Test 6: error in emission record for D_outer
!-----------------------------------------------------------------------------
brnfile = './level_1/resources/brn_error6.brn'
nrec = 3; building_present1 = .TRUE.; numbron = 0; mm = 1
x = 155000.; y = 385000.; qob = 1.; qww = 0.; hbron = 15.
diameter = 0.; szopp = 0.; D_stack = 1.5; D_stack_outer = 1.0; V_stack = -999.; Ts_stack = -999.
emis_horizontal = .FALSE.; ibtg = -1; ibroncat = 3111; iland = 528; idgr = 2
end_of_file = .FALSE.
building%length = 20.0
building%width  = 20.0
building%height = 10.0
building%orientation = 50.0
error_msg = '<outer diameter stack [m]>  outside permitted range'
call chk_result_read_annual1(6, brnfile, icm, check_psd, presentcode, nrec, numbron, building_present1, mm, x, y, qob, qww, hbron, diameter, szopp, D_stack, D_stack_outer, V_stack, Ts_stack, emis_horizontal, building, ibtg, ibroncat, iland, idgr, end_of_file, error_msg)

!-----------------------------------------------------------------------------
! Test 7: error in emission record sigz0 > 0 for point source
!-----------------------------------------------------------------------------
brnfile = './level_1/resources/brn_error7.brn'
nrec = 3; building_present1 = .FALSE.; numbron = 0; mm = 1
x = 155000.; y = 385000.; qob = 1.; qww = 0.; hbron = 15.
diameter = 0.; szopp = 0.; D_stack = -999.; D_stack_outer = -999.; V_stack = -999.; Ts_stack = -999.
emis_horizontal = .FALSE.; ibtg = -1; ibroncat = 3111; iland = 528; idgr = 2
end_of_file = .FALSE.

error_msg = '<initial sigma_z> must be zero for point sources (for backward compatibility); either set sigma_z = 0 m or use option -allow_sigz0_point_source on the command line'
call chk_result_read_annual1(7, brnfile, icm, check_psd, presentcode, nrec, numbron, building_present1, mm, x, y, qob, qww, hbron, diameter, szopp, D_stack, D_stack_outer, V_stack, Ts_stack, emis_horizontal, building, ibtg, ibroncat, iland, idgr, end_of_file, error_msg)
END SUBROUTINE tst_ops_emis_read_annual1

subroutine chk_result_read_header(itest, brnfile, brn_version_e, VsDs_opt_e, nrec_e, numbron_e, error_msg)

use no_pfunit
use m_ops_emis
use m_error

USE m_fileutils
USE m_commonfile, only: fu_bron

implicit none

CHARACTER*512                                    :: ROUTINENAAM                
PARAMETER    (ROUTINENAAM = 'tst_ops_emis_read_header')
character(len=*), intent(in)  :: error_msg              ! error message
character(len=*), intent(in)  :: brnfile                ! emission file
INTEGER, intent(in)           :: itest                  ! test number

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER                   :: brn_version                ! version of emission file
INTEGER, intent(in)       :: brn_version_e              ! version of emission file
LOGICAL                   :: VsDs_opt                   ! read stack parameters Ds/Vs/Ts from source file
LOGICAL, intent(in)       :: VsDs_opt_e                 ! read stack parameters Ds/Vs/Ts from source file
INTEGER                   :: nrec                       ! number of records read (= number of records = lines, in header)
INTEGER, intent(in)       :: nrec_e                     ! number of records read (= number of records = lines, in header)
INTEGER                   :: numbron                    ! number of (selected) sources (initial value = 0)
INTEGER, intent(in)       :: numbron_e                  ! number of (selected) sources (initial value = 0)
TYPE (TError)             :: error                      ! Error handling record

CHARACTER*8       :: testname

testname = 'Test    '; write(testname(6:8),'(i3)') itest

IF (.NOT.sysopen(fu_bron, brnfile, 'r', 'emission file', error)) GOTO 9999
call ops_emis_read_header(fu_bron, brn_version, VsDs_opt, nrec, numbron, error)
call assertEqual(brn_version_e, brn_version,testname // ': brn version',__LINE__,__FILE__)
call assertTrue(VsDs_opt_e .eqv. VsDs_opt, testname // ': Ds/Vs/Ts',__LINE__,__FILE__)
call assertEqual(nrec_e, nrec,testname // ': nrec:',__LINE__,__FILE__)
call assertEqual(numbron_e, numbron,testname // ': numbron:',__LINE__,__FILE__)

if (len_trim(error_msg) .gt. 0) then
    ! Check error message:
    if (error%haserror) then
       call assertEqual(error_msg,error%message, testname//': wrong error message:',__LINE__,__FILE__)
    else
       call assertTrue(error%haserror,testname//': error in input has not been trapped by subroutine ops_emis_read_header',__LINE__,__FILE__)
    endif
    ! reset error
    error%haserror = .FALSE.
    error%message = ''
endif

call sysclose(fu_bron, brnfile, error)
IF (error%haserror) GOTO 9999

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)
call assertFalse(error%haserror,testname//': unexpected error has occurred: '//error%message,__LINE__,__FILE__)

end subroutine chk_result_read_header

!-----------------------------------------------------------------------------------------------------------------
subroutine chk_result_read_annual1(itest, brnfile, icm, check_psd, presentcode, nrec_e, numbron_e, building_present1_e, mm_e, x_e, y_e, qob_e, qww_e, hbron_e, diameter_e, szopp_e, D_stack_e, D_stack_outer_e, V_stack_e, Ts_stack_e, emis_horizontal_e, building_e, ibtg_e, ibroncat_e, iland_e, idgr_e, end_of_file_e, error_msg)

! Expected values have the '_e' suffix

use no_pfunit
use m_ops_emis

USE m_error
USE m_fileutils
USE m_geoutils
USE m_commonconst_lib, only: EPS_DELTA, HUMAX, MAXDISTR, ncolBuildingEffectTable
USE Binas, only: T0
use m_ops_utils, only: is_missing
use m_ops_building
USE m_commonfile, only: fu_bron

implicit none

INTEGER :: icm                        ! component nummer
LOGICAL :: check_psd                  ! check whether particle size distribution has been read
LOGICAL :: presentcode(MAXDISTR,4)    ! which distribution codes are present
INTEGER :: brn_version                ! version of emission file
LOGICAL :: VsDs_opt                   ! read stack parameters Ds/Vs/Ts from source file
LOGICAL :: allow_sigz0_point_source   ! allow initial sigma for point sources

! SUBROUTINE ARGUMENTS - INPUT/OUTPUT
INTEGER :: nrec                       ! record number of source file
INTEGER :: numbron                    ! number of (selected) source
LOGICAL :: building_present1          ! at least one building is present in the source file   

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER :: mm                         ! source identification number [-]
REAL    :: x                          ! x coordinate of source location (RDM [m])                 
REAL    :: y                          ! y coordinate of source location (RDM [m])
REAL    :: qob                        ! emission strength [g/s]
REAL    :: qww                        ! heat content [MW]
REAL    :: hbron                      ! emission height at source (stack height), without plume rise [m]
REAL    :: diameter                   ! diameter area source (NOT stack diameter) [m]
REAL    :: szopp                      ! deviation emission height for area source = initial sigma_z [m]
real    :: D_stack                    ! diameter of the stack [m]
real    :: D_stack_outer              ! outer diameter of the stack [m]
real    :: V_stack                    ! exit velocity of plume at stack tip [m/s]
real    :: Ts_stack                   ! temperature of effluent from stack [K]
logical :: emis_horizontal            ! horizontal outflow of emission
type(Tbuilding) :: building           ! structure with building parameters
INTEGER :: ibtg                       ! diurnal emission variation code [-]
INTEGER :: ibroncat                   ! emission category code [-]
INTEGER :: iland                      ! country/area code [-]
INTEGER :: idgr                       ! particle size distribution code [-]
LOGICAL :: end_of_file                ! end of file has been reached

! SUBROUTINE ARGUMENTS - INPUT/OUTPUT
INTEGER :: nrec_e                     ! record number of source file
INTEGER :: numbron_e                  ! number of (selected) source
LOGICAL :: building_present1_e        ! at least one building is present in the source file   

! SUBROUTINE ARGUMENTS - OUTPUT
INTEGER :: mm_e                       ! source identification number [-]
REAL    :: x_e                        ! x coordinate of source location (RDM [m])                 
REAL    :: y_e                        ! y coordinate of source location (RDM [m])
REAL    :: qob_e                      ! emission strength [g/s]
REAL    :: qww_e                      ! heat content [MW]
REAL    :: hbron_e                    ! emission height at source (stack height), without plume rise [m]
REAL    :: diameter_e                 ! diameter area source (NOT stack diameter) [m]
REAL    :: szopp_e                    ! deviation emission height for area source = initial sigma_z [m]
real    :: D_stack_e                  ! diameter of the stack [m]
real    :: D_stack_outer_e            ! outer diameter of the stack [m]
real    :: V_stack_e                  ! exit velocity of plume at stack tip [m/s]
real    :: Ts_stack_e                 ! temperature of effluent from stack [K]
logical :: emis_horizontal_e          ! horizontal outflow of emission
type(Tbuilding) :: building_e                 ! structure with building parameters
INTEGER :: ibtg_e                     ! diurnal emission variation code [-]
INTEGER :: ibroncat_e                 ! emission category code [-]
INTEGER :: iland_e                    ! country/area code [-]
INTEGER :: idgr_e                     ! particle size distribution code [-]
LOGICAL :: end_of_file_e              ! end of file has been reached

TYPE (TError) :: error                ! Error handling record

real              :: tol = 1.0e-5     ! tolerance for equality of reals
CHARACTER*8       :: testname
CHARACTER*512     :: ROUTINENAAM                
PARAMETER    (ROUTINENAAM = 'tst_ops_emis_read_annual1')
character(len=*), intent(in)  :: error_msg
character(len=*), intent(in)  :: brnfile
INTEGER, intent(in)       :: itest ! test number

testname = 'Test    '; write(testname(6:8),'(i3)') itest

! First, read in header
IF (.NOT.sysopen(fu_bron, brnfile, 'r', 'emission file', error)) GOTO 9999
call ops_emis_read_header(fu_bron, brn_version, VsDs_opt, nrec, numbron, error)
IF (error%haserror) GOTO 9999

! Then, read in one emission line
building_present1 = .false.
allow_sigz0_point_source = .false.
call ops_emis_read_annual1(fu_bron, icm, check_psd, presentcode, brn_version, VsDs_opt, &
                           nrec, numbron, building_present1, mm, x, y, qob, qww, hbron, diameter, szopp, D_stack, D_stack_outer, V_stack, Ts_stack, emis_horizontal, building, ibtg, ibroncat, iland, idgr, end_of_file, error, allow_sigz0_point_source)
if (error%haserror) then
   if (len_trim(error_msg) .gt. 0) then
       ! Check error message:
       call assertEqual(error_msg,error%message,testname,__LINE__,__FILE__)

       ! reset error
       error%haserror = .FALSE.
       error%message = ''
   endif
else
    ! Check data read:
   call assertEqual(nrec_e, nrec,testname // ': nrec:',__LINE__,__FILE__)
   call assertEqual(numbron_e, numbron,testname // ': numbron:',__LINE__,__FILE__)
   call assertTrue(building_present1_e .eqv. building_present1, testname // ': building_present1:',__LINE__,__FILE__)
   call assertEqual(mm_e, mm,testname // ': mm:',__LINE__,__FILE__)
   call assertEqual(x_e, x, tol, testname // ': x:',__LINE__,__FILE__)
   call assertEqual(y_e, y, tol, testname // ': y:',__LINE__,__FILE__)
   call assertEqual(qob_e, qob, tol, testname // ': qob:',__LINE__,__FILE__)
   call assertEqual(qww_e, qww, tol, testname // ': qww:',__LINE__,__FILE__)
   call assertEqual(hbron_e, hbron, tol, testname // ': hbron:',__LINE__,__FILE__)
   call assertEqual(diameter_e, diameter, tol, testname // ': diameter:',__LINE__,__FILE__)
   call assertEqual(szopp_e, szopp, tol, testname // ': szopp:',__LINE__,__FILE__)
   call assertEqual(D_stack_e, D_stack, tol, testname // ': D_stack:',__LINE__,__FILE__)
   call assertEqual(D_stack_outer_e, D_stack_outer, tol, testname // ': D_stack_outer:',__LINE__,__FILE__)
   call assertEqual(V_stack_e, V_stack, tol, testname // ': V_stack:',__LINE__,__FILE__)
   call assertEqual(Ts_stack_e, Ts_stack, tol, testname // ': Ts_stack:',__LINE__,__FILE__)
   call assertTrue(emis_horizontal_e .eqv. emis_horizontal, testname // ': emis_horizontal:',__LINE__,__FILE__)
   call assertEqual(building_e%length, building%length, tol, testname // ': building%length:',__LINE__,__FILE__)
   call assertEqual(building_e%width, building%width, tol, testname // ': building%width:',__LINE__,__FILE__)
   call assertEqual(building_e%height, building%height, tol, testname // ': building%height:',__LINE__,__FILE__)
   call assertEqual(building_e%orientation, building%orientation, tol, testname // ': building%orientation:',__LINE__,__FILE__)
   call assertEqual(ibtg_e, ibtg,testname // ': ibtg:',__LINE__,__FILE__)
   call assertEqual(ibroncat_e, ibroncat,testname // ': ibroncat:',__LINE__,__FILE__)
   call assertEqual(iland_e, iland,testname // ': iland:',__LINE__,__FILE__)
   call assertEqual(idgr_e, idgr,testname // ': idgr:',__LINE__,__FILE__)
   call assertTrue(end_of_file_e .eqv. end_of_file, testname // ': end_of_file:',__LINE__,__FILE__)
   
   if (len_trim(error_msg) .gt. 0) then
      ! Check error message:
      print *,'error_msg=',trim(error_msg)
      call assertTrue(error%haserror,testname//': error in input has not been trapped by subroutine ops_emis_read_annual1',__LINE__,__FILE__)

      ! reset error
      error%haserror = .FALSE.
      error%message = ''
   endif

endif

call sysclose(fu_bron, brnfile, error)
IF (error%haserror) GOTO 9999

RETURN

9999 CALL ErrorCall(ROUTINENAAM, error)
call assertFalse(error%haserror,testname//': unexpected error has occurred: '//error%message,__LINE__,__FILE__)

end subroutine chk_result_read_annual1

end module m_tst_ops_emis

program p_tst_ops_emis

use no_pfunit
use m_tst_ops_emis

implicit none

call tst_check_building_param
call tst_check_stack_param
call tst_check_verdeling
call tst_check_source3
call tst_check_source2
call tst_check_isource2
call tst_check_isource
call tst_check_source
call tst_ops_emis_read_header
call tst_ops_emis_read_annual1
call conclusion

end program p_tst_ops_emis
