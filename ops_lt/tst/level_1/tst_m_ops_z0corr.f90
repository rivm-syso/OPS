module m_tst_ops_z0corr

implicit none

contains

subroutine tst_ops_z0corr

use no_pfunit
use m_error
use m_ops_z0corr
use m_ops_varin
use m_ops_meteo
use m_commonfile, only: fu_log

implicit none

! input
TYPE(Tvarin_meteo):: varin_meteo     ! input variables for meteo
REAL   :: z01                        ! standard roughness length [m]
REAL   :: uster1                     ! friction velocity at standard roughness length 
REAL   :: ol1                        ! Monin-Obukhov length at standard roughness length [m]
REAL   :: z02                        ! new roughness length [m]

! SUBROUTINE ARGUMENTS - OUTPUT
REAL   :: uster2                     ! friction velocity at new roughness length 
REAL   :: ol2                        ! Monin-Obukhov length at standard roughness length [m]
REAL   :: uster3, ol3

! SUBROUTINE ARGUMENTS - INPUT/OUTPUT
TYPE(Terror) :: error                      ! error handling structure

! Local variables:
real, parameter :: tol1 = 0.001     ! relative tolerance for outputs of ops_z0_c0rr
real, parameter :: tol2 = 0.01      ! relative tolerance for check whether u(50) is the same
real, parameter :: z_undb = 50      ! height where wind is undisturbed by roughness
real            :: u_undb1          ! wind speed at height z_undb from z01,uster1,ol1
real            :: u_undb2          ! wind speed at height z_undb from z02,uster2,ol2
character(len = 200) :: line        ! line read from log file

! First test, no change
z01 = 0.3; uster1 = 2.0; ol1 = 80.0; z02 = z01 
call ops_z0corr(varin_meteo, z01, uster1, ol1, z02, uster2, ol2, error)
call assertRelativelyEqual(uster1, uster2, tol1, 'Test 1, uster',__LINE__,__FILE__)
call assertRelativelyEqual(ol1, ol2, tol1, 'Test 1, ol',__LINE__,__FILE__)

! Second test, ol > 0; ! values from 2021-08-09. Also check that u(z = 50 m) is the same for the two regimes:
z01 = 0.3; uster1 = 2.0; ol1 = 80.0; z02 = 0.8
call ops_z0corr(varin_meteo, z01, uster1, ol1, z02, uster2, ol2, error)
call assertRelativelyEqual(2.943749, uster2, tol1, 'Test 2, uster',__LINE__,__FILE__)
call assertRelativelyEqual(188.3952, ol2, tol1, 'Test 2, ol',__LINE__,__FILE__)
call ops_wv_log_profile(z01, z_undb, uster1, ol1, varin_meteo, u_undb1)
call ops_wv_log_profile(z02, z_undb, uster2, ol2, varin_meteo, u_undb2)
call assertRelativelyEqual(u_undb1, u_undb2, tol2, 'Test 2, wind speed at 50 m',__LINE__,__FILE__)

! Third test, ol < 0; values from 2021-08-09. Also check that u(z = 50 m) is the same for the two regimes:
z01 = 0.3; uster1 = 2.0; ol1 = -100.0; z02 = 0.8
call ops_z0corr(varin_meteo, z01, uster1, ol1, z02, uster2, ol2, error)
call assertRelativelyEqual(2.424499, uster2, tol1, 'Test 3, uster',__LINE__,__FILE__)
call assertRelativelyEqual(-174.6149, ol2, tol1, 'Test 3, ol',__LINE__,__FILE__)
call ops_wv_log_profile(z01, z_undb, uster1, ol1, varin_meteo, u_undb1)
call ops_wv_log_profile(z02, z_undb, uster2, ol2, varin_meteo, u_undb2)
call assertRelativelyEqual(u_undb1, u_undb2, tol2, 'Test 3, wind speed at 50 m',__LINE__,__FILE__)

! Test 4, cut-off (2022-12-12); in this case the equality for u(z=50 m) is no longer valid
z01 = 0.3; uster1 = 0.01; ol1 = 1.0; z02 = 0.8
call ops_z0corr(varin_meteo, z01, uster1, ol1, z02, uster2, ol2, error)
call assertRelativelyEqual(0.06, uster2, tol1, 'Test 4, uster',__LINE__,__FILE__)
call assertRelativelyEqual(5.0, ol2, tol1, 'Test 4, ol',__LINE__,__FILE__)

! Test 5, correction + inverse correction = identity:
z01 = 0.3; uster1 = 2.0; ol1 = -100.0; z02 = 0.8
call ops_z0corr(varin_meteo, z01, uster1, ol1, z02, uster2, ol2, error)
CALL ops_z0corr(varin_meteo, z02, uster2, ol2, z01, uster3, ol3, error)
call assertRelativelyEqual(uster1,uster3,tol1,'Test 5, uster3 (inverse); ',__LINE__,__FILE__)
call assertRelativelyEqual(ol1,   ol3,   tol1,'Test 5, ol3 (inverse); ',__LINE__,__FILE__)

! Test 6, incorrect input; check message in log file:
z01 = 35577; ol1 = -100.0
open(fu_log, FILE='./level_1/resources/m_tst_ops_z0corr.log')
rewind(fu_log)
CALL ops_z0corr(varin_meteo, z01, uster1, ol1, z02, uster2, ol2, error)
rewind(fu_log)
read(fu_log,'(a)') line
close(fu_log)
call assertEqual(' negative u* in subr. z0corr', line, 'Test 6, log message',__LINE__,__FILE__)

end subroutine tst_ops_z0corr

end module m_tst_ops_z0corr

program p_tst_ops_z0corr

use no_pfunit
use m_tst_ops_z0corr

implicit none

call tst_ops_z0corr
call conclusion

end program p_tst_ops_z0corr
