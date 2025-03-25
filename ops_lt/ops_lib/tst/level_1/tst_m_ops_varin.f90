module m_tst_ops_read_varin

implicit none

contains

!----------------------------------------------------------------------
subroutine tst_ops_read_varin

use no_pfunit
use m_commonfile, only: IOB_STDOUT
use m_error
use m_ops_varin

implicit none

! SUBROUTINE ARGUMENTS - OUTPUT 
! character(len=*), intent(in) :: namvarin ! name of varin file

TYPE(Tvarin)                         :: varin
!TYPE(Tvarin_gen)                    :: varin_gen                  ! general_variables
!TYPE(Tvarin_meteo)                  :: varin_meteo                ! meteo_variables
!TYPE(Tvarin_disp)                   :: varin_disp                 ! dispersion_variables
!TYPE(Tvarin_emis)                   :: varin_emis                 ! emission_variables
!TYPE(Tvarin_drydep)                 :: varin_drydep               ! dry_deposition_variables
!TYPE(Tvarin_wetdep)                 :: varin_wetdep               ! wet_deposition_variables
!TYPE(Tvarin_chem)                   :: varin_chem                 ! chemistry_variables
 TYPE(TError)                        :: error                      ! error handling record

call ops_read_varin( './level_1/resources/ops_varin1.txt', varin, error) 
if (error%haserror) goto 9999
call assertEqual(0.5,varin%varin_meteo%uz_cutoff1,'ops_varin_read3 uz_cutoff1',__LINE__,__FILE__)
call assertEqual(0.75,varin%varin_meteo%uz_cutoff2,'ops_varin_read3 uz_cutoff2',__LINE__,__FILE__)

return

9999 CALL ErrorCall('tst_ops_read_varin', error)
call write_error(IOB_STDOUT,error)

end subroutine tst_ops_read_varin

!----------------------------------------------------------------------
subroutine tst_ops_varin_read2

use no_pfunit
use m_commonfile, only: IOB_STDOUT
use m_error
use m_ops_varin

implicit none

! SUBROUTINE ARGUMENTS - OUTPUT 
! character(len=*), intent(in) :: namvarin ! name of varin file

TYPE(Tvarin)                         :: varin
!TYPE(Tvarin_gen)                    :: varin_gen                  ! general_variables
!TYPE(Tvarin_meteo)                  :: varin_meteo                ! meteo_variables
!TYPE(Tvarin_disp)                   :: varin_disp                 ! dispersion_variables
!TYPE(Tvarin_emis)                   :: varin_emis                 ! emission_variables
!TYPE(Tvarin_drydep)                 :: varin_drydep               ! dry_deposition_variables
!TYPE(Tvarin_wetdep)                 :: varin_wetdep               ! wet_deposition_variables
!TYPE(Tvarin_chem)                   :: varin_chem                 ! chemistry_variables
 TYPE(TError)                        :: error                      ! error handling record

namelist / meteo / varin

! Read varin_meteo variables:
call ops_varin_read2( './level_1/resources/ops_varin2a.txt', varin, error) 
!write(*,*) '------------------ tst_ops_varin_read2 -------------'
!write(*,nml=meteo) 
!write(*,*) '----------------------------------------------------'

if (error%haserror) goto 9999

! Check values of varin_meteo variables (either read or default values):
call assertEqual(    1          ,varin%varin_meteo%ol_cutoff_iopt1    ,'ops_varin_read2 varin_meteo%ol_cutoff_iopt1    ',__LINE__,__FILE__)
call assertEqual(    2          ,varin%varin_meteo%ol_cutoff_iopt2    ,'ops_varin_read2 varin_meteo%ol_cutoff_iopt2    ',__LINE__,__FILE__)
call assertEqual(    0.0        ,varin%varin_meteo%ol_add_stable0     ,'ops_varin_read2 varin_meteo%ol_add_stable0     ',__LINE__,__FILE__)
call assertEqual(    5.000000   ,varin%varin_meteo%ol_add_stable1     ,'ops_varin_read2 varin_meteo%ol_add_stable1     ',__LINE__,__FILE__)
call assertEqual(    0.0        ,varin%varin_meteo%ol_max_unstable0   ,'ops_varin_read2 varin_meteo%ol_max_unstable0   ',__LINE__,__FILE__)
call assertEqual(   -5.000000   ,varin%varin_meteo%ol_max_unstable1   ,'ops_varin_read2 varin_meteo%ol_max_unstable1   ',__LINE__,__FILE__)
call assertEqual(   -7.000000   ,varin%varin_meteo%ol_max_unstable2   ,'ops_varin_read2 varin_meteo%ol_max_unstable2   ',__LINE__,__FILE__)
call assertEqual(    5.000000   ,varin%varin_meteo%ol_min_stable1     ,'ops_varin_read2 varin_meteo%ol_min_stable1     ',__LINE__,__FILE__)
call assertEqual(   10.00000    ,varin%varin_meteo%ol_min_stable2     ,'ops_varin_read2 varin_meteo%ol_min_stable2     ',__LINE__,__FILE__)
call assertEqual(   20.00000    ,varin%varin_meteo%ol_min_stable3     ,'ops_varin_read2 varin_meteo%ol_min_stable3     ',__LINE__,__FILE__)
call assertEqual(  100.0000     ,varin%varin_meteo%ol_z0_ratio_cutoff1,'ops_varin_read2 varin_meteo%ol_z0_ratio_cutoff1',__LINE__,__FILE__)
call assertEqual(    0.0000     ,varin%varin_meteo%ol_z0_ratio_cutoff2,'ops_varin_read2 varin_meteo%ol_z0_ratio_cutoff2',__LINE__,__FILE__)
call assertEqual(   0.1         ,varin%varin_meteo%uster_min1         ,'ops_varin_read2 varin_meteo%uster_min1         ',__LINE__,__FILE__)
call assertEqual(   0.2         ,varin%varin_meteo%uster_min2         ,'ops_varin_read2 varin_meteo%uster_min2         ',__LINE__,__FILE__)
call assertEqual(   0.3         ,varin%varin_meteo%uster_min3         ,'ops_varin_read2 varin_meteo%uster_min3         ',__LINE__,__FILE__)
call assertEqual(   0.75        ,varin%varin_meteo%uz_cutoff1         ,'ops_varin_read2 varin_meteo%uz_cutoff1         ',__LINE__,__FILE__)
call assertEqual(   0.1         ,varin%varin_meteo%uz_cutoff2         ,'ops_varin_read2 varin_meteo%uz_cutoff2         ',__LINE__,__FILE__)

! Test with unknown parameter:
call ops_varin_read2( './level_1/resources/ops_varin2b.txt', varin, error) 
call assertTrue(error%haserror,'ops_varin_read2 unknown parameter',__LINE__,__FILE__)
call assertEqual('unknown variable for input variables block',error%message,'ops_varin_read2 unknown parameter',__LINE__,__FILE__)
!call WriteError(IOB_STDOUT,error)
error%haserror = .false.

! Test with error in header:
call ops_varin_read2( './level_1/resources/ops_varin2c.txt', varin, error) 
call assertTrue(error%haserror,'ops_varin_read2 error in header',__LINE__,__FILE__)
call assertEqual('Undeclared parameter',error%message,'ops_varin_read2 error in header',__LINE__,__FILE__)
!call WriteError(IOB_STDOUT,error)
error%haserror = .false.

! Test with variable name without value (-> -9999 -> out of range):
call ops_varin_read2( './level_1/resources/ops_varin2d.txt', varin, error) 
call assertTrue(error%haserror,'ops_varin_read2 missing value',__LINE__,__FILE__)
call assertEqual('variable read out of range',error%message,'ops_varin_read2 missing value ',__LINE__,__FILE__)
!call WriteError(IOB_STDOUT,error)
error%haserror = .false.

! Test with END VARIN instead of END_VARIN (error message correct, but maybe not very clear)
call ops_varin_read2( './level_1/resources/ops_varin2e.txt', varin, error) 
call assertTrue(error%haserror,'ops_varin_read2 END VARIN instead of END_VARIN',__LINE__,__FILE__)
call assertEqual('Not a correct real number',error%message,'ops_varin_read2 END VARIN instead of END_VARIN ',__LINE__,__FILE__)
! call WriteError(IOB_STDOUT,error)
error%haserror = .false.

! Test with a single value assigned to `unc_sourcedepl`. This value should be
! assigned to all members of `unc_sourcedepl`.
call ops_varin_read2("./level_1/resources/ops_varin2f.txt", varin, error)
call assertEqual(3.14, varin%varin_unc%plrise_fact,'tst_ops_varin_read2',__LINE__,__FILE__)
call assertEqual( 2.0, varin%varin_unc%unc_sourcedepl%washout_pri_fact,'tst_ops_varin_read2',__LINE__,__FILE__)
call assertEqual( 2.0, varin%varin_unc%unc_sourcedepl%washout_sec_fact,'tst_ops_varin_read2',__LINE__,__FILE__)
call assertEqual( 2.0, varin%varin_unc%unc_sourcedepl%rainout_pri_fact,'tst_ops_varin_read2',__LINE__,__FILE__)
call assertEqual( 2.0, varin%varin_unc%unc_sourcedepl%rainout_sec_fact,'tst_ops_varin_read2',__LINE__,__FILE__)
call assertEqual( 2.0, varin%varin_unc%unc_sourcedepl%vd_drydep_pri_fact,'tst_ops_varin_read2',__LINE__,__FILE__)
call assertEqual( 2.0, varin%varin_unc%unc_sourcedepl%vd_drydep_sec_fact,'tst_ops_varin_read2',__LINE__,__FILE__)
call assertEqual( 2.0, varin%varin_unc%unc_sourcedepl%vchem_fact,'tst_ops_varin_read2',__LINE__,__FILE__)
varin%varin_unc%unc_sourcedepl%vd_drydep_pri_fact = 1.0  ! Reset this value for the next test.
varin%varin_unc%unc_sourcedepl%vd_drydep_sec_fact = 1.0  ! Reset this value for the next test.

! Test with assignments to two of the three members of `unc_sourcedepl`. The
! third member should be assigned the default value.
call ops_varin_read2("./level_1/resources/ops_varin2g.txt", varin, error)
call assertEqual( 2.0, varin%varin_unc%unc_sourcedepl%washout_pri_fact  ,'tst_ops_varin_read2',__LINE__,__FILE__)
call assertEqual( 2.0, varin%varin_unc%unc_sourcedepl%washout_sec_fact  ,'tst_ops_varin_read2',__LINE__,__FILE__)
call assertEqual( 3.0, varin%varin_unc%unc_sourcedepl%rainout_pri_fact,'tst_ops_varin_read2',__LINE__,__FILE__)
call assertEqual( 3.0, varin%varin_unc%unc_sourcedepl%rainout_sec_fact,'tst_ops_varin_read2',__LINE__,__FILE__)
write(*,*) "Drydep factor pri: ", varin%varin_unc%unc_sourcedepl%vd_drydep_pri_fact
call assertEqual( 1.0, varin%varin_unc%unc_sourcedepl%vd_drydep_pri_fact  ,'tst_ops_varin_read2',__LINE__,__FILE__)
write(*,*) "Drydep factor sec: ", varin%varin_unc%unc_sourcedepl%vd_drydep_sec_fact
call assertEqual( 1.0, varin%varin_unc%unc_sourcedepl%vd_drydep_sec_fact  ,'tst_ops_varin_read2',__LINE__,__FILE__)

return

9999 CALL ErrorCall('tst_ops_varin_read2', error)
call write_error(IOB_STDOUT,error)
call assertFalse(error%haserror,'ops_varin_read2: unexpected error',__LINE__,__FILE__)

end subroutine tst_ops_varin_read2

!----------------------------------------------------------------------
subroutine tst_ops_varin_read3

use no_pfunit
use m_commonfile, only: IOB_STDOUT
use m_error
use m_ops_varin

implicit none

! SUBROUTINE ARGUMENTS - OUTPUT 
! character(len=*), intent(in) :: namvarin ! name of varin file

!TYPE(Tvarin_gen)                    :: varin_gen                  ! general_variables
 TYPE(Tvarin_meteo)                  :: varin_meteo                ! meteo_variables
!TYPE(Tvarin_disp)                   :: varin_disp                 ! dispersion_variables
!TYPE(Tvarin_emis)                   :: varin_emis                 ! emission_variables
!TYPE(Tvarin_drydep)                 :: varin_drydep               ! dry_deposition_variables
!TYPE(Tvarin_wetdep)                 :: varin_wetdep               ! wet_deposition_variables
!TYPE(Tvarin_chem)                   :: varin_chem                 ! chemistry_variables
 TYPE(TError)                        :: error                      ! error handling record

namelist / meteo / varin_meteo

! Read varin_meteo variables:
call ops_varin_read3( './level_1/resources/ops_varin3.txt', varin_meteo, error) 
!write(*,*) '------------------ tst_ops_varin_read3 -------------'
!write(*,nml=meteo) 
!write(*,*) '----------------------------------------------------'

if (error%haserror) goto 9999

! Check values of varin_meteo variables (either read or default values):
call assertEqual(    1          ,varin_meteo%ol_cutoff_iopt1    ,'ops_varin_read3 varin_meteo%ol_cutoff_iopt1    ',__LINE__,__FILE__)
call assertEqual(  200          ,varin_meteo%ol_cutoff_iopt2    ,'ops_varin_read3 varin_meteo%ol_cutoff_iopt2    ',__LINE__,__FILE__)
call assertEqual(    0.0        ,varin_meteo%ol_add_stable0     ,'ops_varin_read3 varin_meteo%ol_add_stable0     ',__LINE__,__FILE__)
call assertEqual(    5.000000   ,varin_meteo%ol_add_stable1     ,'ops_varin_read3 varin_meteo%ol_add_stable1     ',__LINE__,__FILE__)
call assertEqual(    0.0        ,varin_meteo%ol_max_unstable0   ,'ops_varin_read3 varin_meteo%ol_max_unstable0   ',__LINE__,__FILE__)
call assertEqual(   -5.000000   ,varin_meteo%ol_max_unstable1   ,'ops_varin_read3 varin_meteo%ol_max_unstable1   ',__LINE__,__FILE__)
call assertEqual(   -7.000000   ,varin_meteo%ol_max_unstable2   ,'ops_varin_read3 varin_meteo%ol_max_unstable2   ',__LINE__,__FILE__)
call assertEqual(    5.000000   ,varin_meteo%ol_min_stable1     ,'ops_varin_read3 varin_meteo%ol_min_stable1     ',__LINE__,__FILE__)
call assertEqual(   10.00000    ,varin_meteo%ol_min_stable2     ,'ops_varin_read3 varin_meteo%ol_min_stable2     ',__LINE__,__FILE__)
call assertEqual(   20.00000    ,varin_meteo%ol_min_stable3     ,'ops_varin_read3 varin_meteo%ol_min_stable3     ',__LINE__,__FILE__)
call assertEqual(  100.0000     ,varin_meteo%ol_z0_ratio_cutoff1,'ops_varin_read3 varin_meteo%ol_z0_ratio_cutoff1',__LINE__,__FILE__)
call assertEqual(    0.0000     ,varin_meteo%ol_z0_ratio_cutoff2,'ops_varin_read3 varin_meteo%ol_z0_ratio_cutoff2',__LINE__,__FILE__)
call assertEqual(   91.00000    ,varin_meteo%uster_min1         ,'ops_varin_read3 varin_meteo%uster_min1         ',__LINE__,__FILE__)
call assertEqual(   92.00000    ,varin_meteo%uster_min2         ,'ops_varin_read3 varin_meteo%uster_min2         ',__LINE__,__FILE__)
call assertEqual(   93.00000    ,varin_meteo%uster_min3         ,'ops_varin_read3 varin_meteo%uster_min3         ',__LINE__,__FILE__)
call assertEqual(    7.500000   ,varin_meteo%uz_cutoff1         ,'ops_varin_read3 varin_meteo%uz_cutoff1         ',__LINE__,__FILE__)
call assertEqual(    0.01       ,varin_meteo%uz_cutoff2         ,'ops_varin_read3 varin_meteo%uz_cutoff2         ',__LINE__,__FILE__)

return

9999 CALL ErrorCall('tst_ops_varin_read3', error)
call write_error(IOB_STDOUT,error)
call assertFalse(error%haserror,'ops_varin_read3: unexpected error',__LINE__,__FILE__)
if (error%haserror) write(*,*) error%message

end subroutine tst_ops_varin_read3

end module m_tst_ops_read_varin

program p_tst_ops_read_varin
use no_pfunit
use m_tst_ops_read_varin

implicit none

call tst_ops_read_varin
call tst_ops_varin_read2
call tst_ops_varin_read3
call conclusion

end program p_tst_ops_read_varin
