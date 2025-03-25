module m_test_ops_stab_rek
implicit none
contains
   subroutine test_ops_stab_rek3()
   use m_ops_stab_rek
   use no_pfunit_ops_lt
   use m_error, only: TError
   use m_ops_varin, only: Tvarin
   
        type(Tvarin) :: varin
        real, parameter :: tol = 1e-5
        integer, parameter :: icm = 3
        real, parameter :: rb_ms =   1.18939507E+02
        real, parameter :: temp_C =   8.11497784E+00
        real, parameter :: h0 =  -4.26504898E+00
        real, parameter :: z0_metreg_rcp =   2.89999992E-01
        real, parameter :: disxx =   2.95000000E+04
        real, parameter :: disx = -999.0
        real, parameter :: z0_rcp =   2.00000003E-01
        real, parameter :: xl =   5.67004166E+01
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: qtr =   0.00000000E+00
        real, parameter :: qrv =   0.00000000E+00
        integer, parameter :: dv = 33
        real, parameter :: ecvl(6,4,36) = reshape( (/   1.20471072E+00,   1.20982134E+00,   9.50585306E-01, &
             9.77232754E-01,   8.31212103E-01,   8.49113882E-01,   1.23252738E+00,   1.20151615E+00,   9.36372101E-01, &
             9.84885693E-01,   9.44933355E-01,   8.50000024E-01,   1.23885250E+00,   1.18642557E+00,   9.91904795E-01, &
             9.42245603E-01,   9.53809619E-01,   8.42040837E-01,   1.25147545E+00,   1.19587779E+00,   1.03138340E+00, &
             9.32492554E-01,   9.58118677E-01,   8.46203148E-01,   1.27132213E+00,   1.28992844E+00,   9.59853649E-01, &
             9.84465420E-01,   7.45707035E-01,   7.83670843E-01,   1.30967033E+00,   1.29234648E+00,   9.84976649E-01, &
             9.85753298E-01,   1.20026648E+00,   8.19626212E-01,   1.28999984E+00,   1.28558230E+00,   1.13198411E+00, &
             9.30140316E-01,   1.11933339E+00,   8.12244892E-01,   1.31967211E+00,   1.28118312E+00,   1.03956521E+00, &
             1.01890218E+00,   9.50792015E-01,   9.00534749E-01,   1.48165286E+00,   1.46296406E+00,   9.46243882E-01, &
             9.68805015E-01,   5.65303028E-01,   6.40886068E-01,   1.43725264E+00,   1.46823108E+00,   9.11860526E-01, &
             1.01776266E+00,   9.38399971E-01,   6.31495357E-01,   1.40262282E+00,   1.51240957E+00,   1.09111106E+00, &
             9.22280610E-01,   1.04600000E+00,   5.93959153E-01,   1.42639339E+00,   1.48824418E+00,   1.15675902E+00, &
             9.65192854E-01,   1.04970300E+00,   5.73048115E-01,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   1.27132213E+00,   1.28992844E+00,   9.59853649E-01, &
             9.84465420E-01,   7.45707035E-01,   7.83670843E-01,   1.30967033E+00,   1.29234648E+00,   9.84976649E-01, &
             9.85753298E-01,   1.20026648E+00,   8.19626212E-01,   1.28999984E+00,   1.28558230E+00,   1.13198411E+00, &
             9.30140316E-01,   1.11933339E+00,   8.12244892E-01,   1.31967211E+00,   1.28118312E+00,   1.03956521E+00, &
             1.01890218E+00,   9.50792015E-01,   9.00534749E-01,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   1.41966939E+00,   1.53553557E+00,   9.39365804E-01, &
             9.64150906E-01,   5.39090931E-01,   6.18059039E-01,   1.44791222E+00,   1.51231039E+00,   9.14697647E-01, &
             1.00826478E+00,   9.37999964E-01,   6.11838043E-01,   1.49606550E+00,   1.53216875E+00,   1.10178566E+00, &
             9.19438481E-01,   1.03295255E+00,   5.68612218E-01,   1.47918034E+00,   1.52988541E+00,   1.17363644E+00, &
             9.51097965E-01,   1.04247522E+00,   5.60748696E-01,   1.61107445E+00,   1.56799984E+00,   8.90634179E-01, &
             9.23962295E-01,   5.22929251E-01,   5.78860760E-01,   1.63384604E+00,   1.54584837E+00,   7.89255857E-01, &
             9.92009103E-01,   6.28266633E-01,   5.50031126E-01,   1.65213108E+00,   1.51405621E+00,   8.81468177E-01, &
             8.90140295E-01,   7.31523871E-01,   5.19469321E-01,   1.66016400E+00,   1.54503810E+00,   1.10869563E+00, &
             7.62937725E-01,   8.89901042E-01,   4.53957170E-01,   1.55314052E+00,   1.52735710E+00,   9.29170728E-01, &
             9.43396151E-01,   5.24999976E-01,   5.97974658E-01,   1.53626370E+00,   1.51483750E+00,   8.48511577E-01, &
             1.01301360E+00,   8.13600063E-01,   5.81713319E-01,   1.54229510E+00,   1.52104414E+00,   9.95515943E-01, &
             9.06526268E-01,   8.97238076E-01,   5.43591797E-01,   1.53327858E+00,   1.52904582E+00,   1.15043473E+00, &
             8.53679538E-01,   9.79405820E-01,   5.14759302E-01,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00/), &
          (/6,4,36/) )
        real, parameter :: coef_space_heating =   3.73999977E+00
        integer, parameter :: ibtg = 0, ibtg1=1, ibtg_neg=-1, ibtg4=4, ibtg5=5
        real, parameter :: uster_metreg_rcp =   5.63751794E-02
        real, parameter :: hbron =   5.00000000E+00
        real, parameter :: qww =   0.00000000E+00
        real, parameter :: D_stack =   1.00000000E+00
        real, parameter :: V_stack =   3.00000000E+00
        real, parameter :: Ts_stack =  -9.99000000E+02
        logical, parameter :: emis_horizontal = .false.
        integer, parameter :: ircp = 1
        integer, parameter :: istab = 5
        integer, parameter :: itra = 1
        real, parameter :: qob =   1.00000000E+00
        real, parameter :: xloc =   3.89999962E+01
        real, parameter :: regenk =   1.73333343E-02
        real, parameter :: ra_ms_4 =   2.03518982E+02
        real, parameter :: z0_tra =   2.00000003E-01
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: in_ol_metreg_rcp =   6.88502073E+00
        real :: ol_metreg_rcp
        type(TError) :: error
        type(TError) :: ref_error
        real :: uster_rcp
        real :: ol_rcp
        real :: uster_src
        real :: ol_src
        real :: uster_tra
        real :: ol_tra
        real :: htot
        real :: htt
        real :: onder
        real :: uh
        real :: zu
        real :: qruim
        real :: qbron
        real :: dispg
        
        !! Reference values (with old version of cut-off for Obukhov length)
        !real, parameter :: ref_ol_metreg_rcp(2) = (/ 11.8850212, 10.0 /)
        !real, parameter :: ref_uster_rcp        =     5.99999987E-02
        !real, parameter :: ref_ol_rcp(2)        = (/  8.87143230, -5.72831678391 /)
        !real, parameter :: ref_uster_src        =     5.99999987E-02
        !real, parameter :: ref_ol_src(2)        = (/ 10.1107035, 8.87143516541 /)
        !real, parameter :: ref_uster_tra        =     5.99999987E-02
        !real, parameter :: ref_ol_tra(2)        = (/ 10.1107035, 8.87143516541 /)
        !real, parameter :: ref_htot(2)          = (/ 12.4498577,  12.0245342255 /)
        !real, parameter :: ref_htt(2)           = (/ 12.4498577,  12.0245342255 /)
        !real, parameter :: ref_onder            =     1.00000000E+00
        !real, parameter :: ref_uh(3)            = (/  2.14770889, 2.25960898399, 2.26710152626  /) 
        !real, parameter :: ref_zu(2)            = (/ 28.3502083, 28.1551399231/) 
        !real, parameter :: ref_qruim            =     0.00000000E+00
        !real, parameter :: ref_qbron(5)         = (/  1.0, 0.831212103367, 0.0, 0.104208059609, 0.864843189716 /) 
        !real, parameter :: ref_dispg(3)         = (/  0.0331908911,  0.03256081789732, 0.212278172374   /)
        
        ! Reference values (with curent version of cut-off for Obukhov length)
        real, parameter :: ref_ol_metreg_rcp(3) = (/ 11.8850212, 9.0, 10.0/)
        real, parameter :: ref_uster_rcp        =     5.99999987E-02
        real, parameter :: ref_ol_rcp(3)        = (/  8.87143230, -5.72831678391, 8.87143135071 /)
        real, parameter :: ref_uster_src        =     5.99999987E-02
        real, parameter :: ref_ol_src(3)        = (/ 10.1107035, 8.09578990936, 8.87143516541  /)
        real, parameter :: ref_uster_tra        =     5.99999987E-02
        real, parameter :: ref_ol_tra(3)        = (/ 10.1107035, 8.09578990936, 8.87143516541 /)
        real, parameter :: ref_htot(2)          = (/ 12.4498577,  11.7348155975 /)
        real, parameter :: ref_htt(2)           = (/ 12.4498577,  11.7348155975 /)
        real, parameter :: ref_onder            =     1.00000000E+00
        real, parameter :: ref_uh(4)            = (/  2.14770889, 2.33312582970, 2.3512756824, 2.259608983995  /) 
        real, parameter :: ref_zu(3)            = (/ 28.3502083, 27.8812236786, 28.1551399231/) 
        real, parameter :: ref_qruim            =     0.00000000E+00
        real, parameter :: ref_qbron(5)         = (/  1.0, 0.831212103367, 0.0, 0.104208059609, 0.864843189716 /) 
        real, parameter :: ref_dispg(4)         = (/  0.0331908911,  3.242139518261E-02, 0.210802391171,   3.256081789732E-02   /)

        real :: temp_C4
        
        ol_metreg_rcp = in_ol_metreg_rcp
        ref_error%haserror = .false.
        ref_error%message = ""
        dispg = ref_dispg(1)
        call ops_stab_rek( varin, icm, rb_ms, temp_C, h0, z0_metreg_rcp, disxx, disx, z0_rcp, xl, radius, qtr, qrv, &
            dv, ecvl, coef_space_heating, ibtg, uster_metreg_rcp, hbron, qww, D_stack, V_stack, Ts_stack, &
            emis_horizontal, ircp, istab, itra, qob, xloc, regenk, ra_ms_4, z0_tra, z0_src, ol_metreg_rcp, error, &
            uster_rcp, ol_rcp, uster_src, ol_src, uster_tra, ol_tra, htot, htt, onder, uh, zu, qruim, &
            qbron, dispg)
        call assertEqual( ref_ol_metreg_rcp(1), ol_metreg_rcp, tol, "ol_metreg_rcp", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_stab_rek", __LINE__, __FILE__)
        call assertEqual( ref_uster_rcp, uster_rcp, tol, "uster_rcp", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_rcp(1), ol_rcp, tol, "ol_rcp", __LINE__, __FILE__)
        call assertEqual( ref_uster_src, uster_src, tol, "uster_src", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_src(1), ol_src, tol, "ol_src", __LINE__, __FILE__)
        call assertEqual( ref_uster_tra, uster_tra, tol, "uster_tra", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_tra(1), ol_tra, tol, "ol_tra", __LINE__, __FILE__)
        call assertEqual( ref_htot(1), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_htt(1), htt, tol, "htt", __LINE__, __FILE__)
        call assertEqual( ref_onder, onder, tol, "onder", __LINE__, __FILE__)
        call assertEqual( ref_uh(1), uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu(1), zu, tol, "zu", __LINE__, __FILE__)
        call assertEqual( ref_qruim, qruim, tol, "qruim", __LINE__, __FILE__)
        call assertEqual( ref_qbron(1), qbron, tol, "qbron", __LINE__, __FILE__)
        call assertEqual( ref_dispg(1), dispg, tol, "dispg", __LINE__, __FILE__)

        ol_metreg_rcp = 4.0
        ref_error%haserror = .false.
        ref_error%message = ""
        dispg = ref_dispg(1)

        call ops_stab_rek( varin, icm, rb_ms, temp_C, h0, z0_metreg_rcp, disxx, disx, z0_rcp, xl, radius, qtr, qrv, &
            dv, ecvl, coef_space_heating, ibtg, uster_metreg_rcp, hbron, qww, D_stack, V_stack, Ts_stack, &
            emis_horizontal, ircp, istab, itra, qob, xloc, regenk, ra_ms_4, z0_tra, z0_src, ol_metreg_rcp, error, &
            uster_rcp, ol_rcp, uster_src, ol_src, uster_tra, ol_tra, htot, htt, onder, uh, zu, qruim, &
            qbron, dispg)
        call assertEqual( ref_ol_metreg_rcp(2), ol_metreg_rcp, tol, "ol_metreg_rcp", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_stab_rek", __LINE__, __FILE__)
        call assertEqual( ref_uster_rcp, uster_rcp, tol, "uster_rcp", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_rcp(1), ol_rcp, tol, "ol_rcp", __LINE__, __FILE__)
        call assertEqual( ref_uster_src, uster_src, tol, "uster_src", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_src(2), ol_src, tol, "ol_src", __LINE__, __FILE__)
        call assertEqual( ref_uster_tra, uster_tra, tol, "uster_tra", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_tra(2), ol_tra, tol, "ol_tra", __LINE__, __FILE__)
        call assertEqual( ref_htot(2), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_htt(2), htt, tol, "htt", __LINE__, __FILE__)
        call assertEqual( ref_onder, onder, tol, "onder", __LINE__, __FILE__)
        call assertEqual( ref_uh(2), uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu(2), zu, tol, "zu", __LINE__, __FILE__)
        call assertEqual( ref_qruim, qruim, tol, "qruim", __LINE__, __FILE__)
        call assertEqual( ref_qbron(1), qbron, tol, "qbron", __LINE__, __FILE__)
        call assertEqual( ref_dispg(2), dispg, tol, "dispg", __LINE__, __FILE__)

        ol_metreg_rcp = 5.0
        ref_error%haserror = .false.
        ref_error%message = ""
        dispg = ref_dispg(1)

        ! 2.6 OPS doc (v5.1.1.0) L = [-T rho_a cp (u*)^3 ] / [g Ho kappa] <=> 
        ! T = [-L g Ho kappa] / [rho_a cp (u*)^3 ] 
        ! u* = AMAX1(7.22/(rb_ms + 1),0.06) ! ops_stab_rek
        ! -273 -> K naar C
        temp_C4 = -ol_metreg_rcp*(0.4*9.8*h0)/AMAX1(7.22/(rb_ms + 1),0.06)**3/1.29/1005 - 273 

        call ops_stab_rek( varin, icm, rb_ms, temp_C4, h0, z0_metreg_rcp, disxx, disx, z0_rcp, xl, radius, qtr, qrv, &
            dv, ecvl, coef_space_heating, ibtg, uster_metreg_rcp, hbron, qww, D_stack, V_stack, Ts_stack, &
            emis_horizontal, ircp, istab, itra, qob, xloc, regenk, ra_ms_4, z0_tra, z0_src, ol_metreg_rcp, error, &
            uster_rcp, ol_rcp, uster_src, ol_src, uster_tra, ol_tra, htot, htt, onder, uh, zu, qruim, &
            qbron, dispg)
        call assertEqual( ref_ol_metreg_rcp(3), ol_metreg_rcp, tol, "ol_metreg_rcp", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_stab_rek", __LINE__, __FILE__)
        call assertEqual( ref_uster_rcp, uster_rcp, tol, "uster_rcp", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_rcp(3), ol_rcp, tol, "ol_rcp", __LINE__, __FILE__)
        call assertEqual( ref_uster_src, uster_src, tol, "uster_src", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_src(3), ol_src, tol, "ol_src", __LINE__, __FILE__)
        call assertEqual( ref_uster_tra, uster_tra, tol, "uster_tra", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_tra(3), ol_tra, tol, "ol_tra", __LINE__, __FILE__)
        call assertEqual( ref_uh(4), uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu(3), zu, tol, "zu", __LINE__, __FILE__)
        call assertEqual( ref_qruim, qruim, tol, "qruim", __LINE__, __FILE__)
        call assertEqual( ref_qbron(1), qbron, tol, "qbron", __LINE__, __FILE__)
        call assertEqual( ref_dispg(4), dispg, tol, "dispg", __LINE__, __FILE__)


        ol_metreg_rcp = 4.0
        ref_error%haserror = .false.
        ref_error%message = ""
        dispg = ref_dispg(1)

        call ops_stab_rek( varin, icm, rb_ms, temp_C, h0, z0_metreg_rcp, disxx, disx, z0_rcp, xl, radius, qtr, qrv, &
            dv, ecvl, coef_space_heating, ibtg1, uster_metreg_rcp, hbron, qww, D_stack, V_stack, Ts_stack, &
            emis_horizontal, ircp, istab, itra, qob, xloc, regenk, ra_ms_4, z0_tra, z0_src, ol_metreg_rcp, error, &
            uster_rcp, ol_rcp, uster_src, ol_src, uster_tra, ol_tra, htot, htt, onder, uh, zu, qruim, &
            qbron, dispg)
        call assertEqual( ref_ol_metreg_rcp(2), ol_metreg_rcp, tol, "ol_metreg_rcp", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_stab_rek", __LINE__, __FILE__)
        call assertEqual( ref_uster_rcp, uster_rcp, tol, "uster_rcp", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_rcp(1), ol_rcp, tol, "ol_rcp", __LINE__, __FILE__)
        call assertEqual( ref_uster_src, uster_src, tol, "uster_src", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_src(2), ol_src, tol, "ol_src", __LINE__, __FILE__)
        call assertEqual( ref_uster_tra, uster_tra, tol, "uster_tra", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_tra(2), ol_tra, tol, "ol_tra", __LINE__, __FILE__)
        call assertEqual( ref_htot(2), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_htt(2), htt, tol, "htt", __LINE__, __FILE__)
        call assertEqual( ref_onder, onder, tol, "onder", __LINE__, __FILE__)
        call assertEqual( ref_uh(2), uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu(2), zu, tol, "zu", __LINE__, __FILE__)
        call assertEqual( ref_qruim, qruim, tol, "qruim", __LINE__, __FILE__)
        call assertEqual( ref_qbron(2), qbron, tol, "qbron", __LINE__, __FILE__)
        call assertEqual( ref_dispg(2), dispg, tol, "dispg", __LINE__, __FILE__)

        ol_metreg_rcp = 4.0
        ref_error%haserror = .false.
        ref_error%message = ""
        dispg = ref_dispg(1)

        call ops_stab_rek( varin, icm, rb_ms, temp_C, h0, z0_metreg_rcp, disxx, disx, z0_rcp, xl, radius, qtr, qrv, &
            dv, ecvl, coef_space_heating, ibtg_neg, uster_metreg_rcp, hbron, qww, D_stack, V_stack, Ts_stack, &
            emis_horizontal, ircp, istab, itra, qob, xloc, regenk, ra_ms_4, z0_tra, z0_src, ol_metreg_rcp, error, &
            uster_rcp, ol_rcp, uster_src, ol_src, uster_tra, ol_tra, htot, htt, onder, uh, zu, qruim, &
            qbron, dispg)
        call assertEqual( ref_ol_metreg_rcp(2), ol_metreg_rcp, tol, "ol_metreg_rcp", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_stab_rek", __LINE__, __FILE__)
        call assertEqual( ref_uster_rcp, uster_rcp, tol, "uster_rcp", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_rcp(1), ol_rcp, tol, "ol_rcp", __LINE__, __FILE__)
        call assertEqual( ref_uster_src, uster_src, tol, "uster_src", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_src(2), ol_src, tol, "ol_src", __LINE__, __FILE__)
        call assertEqual( ref_uster_tra, uster_tra, tol, "uster_tra", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_tra(2), ol_tra, tol, "ol_tra", __LINE__, __FILE__)
        call assertEqual( ref_htot(2), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_htt(2), htt, tol, "htt", __LINE__, __FILE__)
        call assertEqual( ref_onder, onder, tol, "onder", __LINE__, __FILE__)
        call assertEqual( ref_uh(2), uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu(2), zu, tol, "zu", __LINE__, __FILE__)
        call assertEqual( ref_qruim, qruim, tol, "qruim", __LINE__, __FILE__)
        call assertEqual( ref_qbron(3), qbron, tol, "qbron", __LINE__, __FILE__)
        call assertEqual( ref_dispg(2), dispg, tol, "dispg", __LINE__, __FILE__)

        ol_metreg_rcp = 4.0
        ref_error%haserror = .false.
        ref_error%message = ""
        dispg = ref_dispg(1)

        call ops_stab_rek( varin, icm, rb_ms, temp_C, h0, z0_metreg_rcp, disxx, disx, z0_rcp, xl, radius, qtr, qrv, &
            dv, ecvl, coef_space_heating, ibtg4, uster_metreg_rcp, hbron, qww, D_stack, V_stack, Ts_stack, &
            emis_horizontal, ircp, istab, itra, qob, xloc, regenk, ra_ms_4, z0_tra, z0_src, ol_metreg_rcp, error, &
            uster_rcp, ol_rcp, uster_src, ol_src, uster_tra, ol_tra, htot, htt, onder, uh, zu, qruim, &
            qbron, dispg)
        call assertEqual( ref_ol_metreg_rcp(2), ol_metreg_rcp, tol, "ol_metreg_rcp", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_stab_rek", __LINE__, __FILE__)
        call assertEqual( ref_uster_rcp, uster_rcp, tol, "uster_rcp", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_rcp(1), ol_rcp, tol, "ol_rcp", __LINE__, __FILE__)
        call assertEqual( ref_uster_src, uster_src, tol, "uster_src", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_src(2), ol_src, tol, "ol_src", __LINE__, __FILE__)
        call assertEqual( ref_uster_tra, uster_tra, tol, "uster_tra", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_tra(2), ol_tra, tol, "ol_tra", __LINE__, __FILE__)
        call assertEqual( ref_htot(2), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_htt(2), htt, tol, "htt", __LINE__, __FILE__)
        call assertEqual( ref_onder, onder, tol, "onder", __LINE__, __FILE__)
        call assertEqual( ref_uh(2), uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu(2), zu, tol, "zu", __LINE__, __FILE__)
        call assertEqual( ref_qruim, qruim, tol, "qruim", __LINE__, __FILE__)
        call assertEqual( ref_qbron(5), qbron, tol, "qbron", __LINE__, __FILE__)
        call assertEqual( ref_dispg(2), dispg, tol, "dispg", __LINE__, __FILE__)

        ol_metreg_rcp = 4.0
        ref_error%haserror = .false.
        ref_error%message = ""
        dispg = ref_dispg(1)

        call ops_stab_rek( varin, icm, rb_ms, temp_C, h0, z0_metreg_rcp, disxx, disx, z0_rcp, xl, radius, qtr, qrv, &
            dv, ecvl, coef_space_heating, ibtg5, uster_metreg_rcp, hbron, qww, D_stack, V_stack, Ts_stack, &
            emis_horizontal, ircp, istab, itra, qob, xloc, regenk, ra_ms_4, z0_tra, z0_src, ol_metreg_rcp, error, &
            uster_rcp, ol_rcp, uster_src, ol_src, uster_tra, ol_tra, htot, htt, onder, uh, zu, qruim, &
            qbron, dispg)
        call assertEqual( ref_ol_metreg_rcp(2), ol_metreg_rcp, tol, "ol_metreg_rcp", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_stab_rek", __LINE__, __FILE__)
        call assertEqual( ref_uster_rcp, uster_rcp, tol, "uster_rcp", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_rcp(1), ol_rcp, tol, "ol_rcp", __LINE__, __FILE__)
        call assertEqual( ref_uster_src, uster_src, tol, "uster_src", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_src(2), ol_src, tol, "ol_src", __LINE__, __FILE__)
        call assertEqual( ref_uster_tra, uster_tra, tol, "uster_tra", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_tra(2), ol_tra, tol, "ol_tra", __LINE__, __FILE__)
        call assertEqual( ref_htot(2), htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_htt(2), htt, tol, "htt", __LINE__, __FILE__)
        call assertEqual( ref_onder, onder, tol, "onder", __LINE__, __FILE__)
        call assertEqual( ref_uh(2), uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu(2), zu, tol, "zu", __LINE__, __FILE__)
        call assertEqual( ref_qruim, qruim, tol, "qruim", __LINE__, __FILE__)
        call assertEqual( ref_qbron(4), qbron, tol, "qbron", __LINE__, __FILE__)
        call assertEqual( ref_dispg(2), dispg, tol, "dispg", __LINE__, __FILE__)
   end subroutine test_ops_stab_rek3

   subroutine test_ops_stab_rek2()
   use m_ops_stab_rek
   use no_pfunit_ops_lt
   use m_error, only: TError
   use m_ops_varin, only: Tvarin
   
        type(Tvarin) :: varin
        real, parameter :: tol = 1e-5 
        integer, parameter :: icm = 4
        real, parameter :: rb_ms =   2.06342964E+01
        real, parameter :: temp_C =   9.57713318E+00
        real, parameter :: h0 =  -3.09668713E+01
        real, parameter :: z0_metreg_rcp =   2.89999992E-01
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: disx = -999.0
        real, parameter :: z0_rcp =   2.00000003E-01
        real, parameter :: xl =   2.85942810E+02
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: qtr =   0.00000000E+00
        real, parameter :: qrv =   0.00000000E+00
        integer, parameter :: dv = 33
        real, parameter :: ecvl(6,4,36) = reshape( (/   1.20471072E+00,   1.20982134E+00,   9.50585306E-01, &
             9.77232754E-01,   8.31212103E-01,   8.49113882E-01,   1.23252738E+00,   1.20151615E+00,   9.36372101E-01, &
             9.84885693E-01,   9.44933355E-01,   8.50000024E-01,   1.23885250E+00,   1.18642557E+00,   9.91904795E-01, &
             9.42245603E-01,   9.53809619E-01,   8.42040837E-01,   1.25147545E+00,   1.19587779E+00,   1.03138340E+00, &
             9.32492554E-01,   9.58118677E-01,   8.46203148E-01,   1.27132213E+00,   1.28992844E+00,   9.59853649E-01, &
             9.84465420E-01,   7.45707035E-01,   7.83670843E-01,   1.30967033E+00,   1.29234648E+00,   9.84976649E-01, &
             9.85753298E-01,   1.20026648E+00,   8.19626212E-01,   1.28999984E+00,   1.28558230E+00,   1.13198411E+00, &
             9.30140316E-01,   1.11933339E+00,   8.12244892E-01,   1.31967211E+00,   1.28118312E+00,   1.03956521E+00, &
             1.01890218E+00,   9.50792015E-01,   9.00534749E-01,   1.48165286E+00,   1.46296406E+00,   9.46243882E-01, &
             9.68805015E-01,   5.65303028E-01,   6.40886068E-01,   1.43725264E+00,   1.46823108E+00,   9.11860526E-01, &
             1.01776266E+00,   9.38399971E-01,   6.31495357E-01,   1.40262282E+00,   1.51240957E+00,   1.09111106E+00, &
             9.22280610E-01,   1.04600000E+00,   5.93959153E-01,   1.42639339E+00,   1.48824418E+00,   1.15675902E+00, &
             9.65192854E-01,   1.04970300E+00,   5.73048115E-01,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   1.27132213E+00,   1.28992844E+00,   9.59853649E-01, &
             9.84465420E-01,   7.45707035E-01,   7.83670843E-01,   1.30967033E+00,   1.29234648E+00,   9.84976649E-01, &
             9.85753298E-01,   1.20026648E+00,   8.19626212E-01,   1.28999984E+00,   1.28558230E+00,   1.13198411E+00, &
             9.30140316E-01,   1.11933339E+00,   8.12244892E-01,   1.31967211E+00,   1.28118312E+00,   1.03956521E+00, &
             1.01890218E+00,   9.50792015E-01,   9.00534749E-01,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   1.41966939E+00,   1.53553557E+00,   9.39365804E-01, &
             9.64150906E-01,   5.39090931E-01,   6.18059039E-01,   1.44791222E+00,   1.51231039E+00,   9.14697647E-01, &
             1.00826478E+00,   9.37999964E-01,   6.11838043E-01,   1.49606550E+00,   1.53216875E+00,   1.10178566E+00, &
             9.19438481E-01,   1.03295255E+00,   5.68612218E-01,   1.47918034E+00,   1.52988541E+00,   1.17363644E+00, &
             9.51097965E-01,   1.04247522E+00,   5.60748696E-01,   1.61107445E+00,   1.56799984E+00,   8.90634179E-01, &
             9.23962295E-01,   5.22929251E-01,   5.78860760E-01,   1.63384604E+00,   1.54584837E+00,   7.89255857E-01, &
             9.92009103E-01,   6.28266633E-01,   5.50031126E-01,   1.65213108E+00,   1.51405621E+00,   8.81468177E-01, &
             8.90140295E-01,   7.31523871E-01,   5.19469321E-01,   1.66016400E+00,   1.54503810E+00,   1.10869563E+00, &
             7.62937725E-01,   8.89901042E-01,   4.53957170E-01,   1.55314052E+00,   1.52735710E+00,   9.29170728E-01, &
             9.43396151E-01,   5.24999976E-01,   5.97974658E-01,   1.53626370E+00,   1.51483750E+00,   8.48511577E-01, &
             1.01301360E+00,   8.13600063E-01,   5.81713319E-01,   1.54229510E+00,   1.52104414E+00,   9.95515943E-01, &
             9.06526268E-01,   8.97238076E-01,   5.43591797E-01,   1.53327858E+00,   1.52904582E+00,   1.15043473E+00, &
             8.53679538E-01,   9.79405820E-01,   5.14759302E-01,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00/), &
          (/6,4,36/) )
        real, parameter :: coef_space_heating =   7.67999935E+00
        integer, parameter :: ibtg = 0
        real, parameter :: uster_metreg_rcp =   3.55194211E-01
        real, parameter :: hbron =   5.00000000E+00
        real, parameter :: qww =   0.00000000E+00
        real, parameter :: D_stack =  -9.99000000E+02
        real, parameter :: V_stack =  -9.99000000E+02
        real, parameter :: Ts_stack =  -9.99000000E+02
        logical, parameter :: emis_horizontal = .false.
        integer, parameter :: ircp = 1
        integer, parameter :: istab = 3
        integer, parameter :: itra = 1
        real, parameter :: qob =   1.00000000E+00
        real, parameter :: xloc =   2.81999969E+02
        real, parameter :: regenk =   5.78000024E-02
        real, parameter :: ra_ms_4 =   1.92685966E+01
        real, parameter :: z0_tra =   2.00000003E-01
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: in_ol_metreg_rcp =   1.71194199E+02
        real :: ol_metreg_rcp
        real, parameter :: ref_ol_metreg_rcp =   1.76194199E+02
        type(TError) :: error
        type(TError) :: ref_error
        real :: uster_rcp
        real, parameter :: ref_uster_rcp =   2.95470983E-01
        real :: ol_rcp
        real, parameter :: ref_ol_rcp =   8.89519348E+01
        real :: uster_src
        real, parameter :: ref_uster_src =   3.21115643E-01
        real :: ol_src
        real, parameter :: ref_ol_src =   1.39790878E+02
        real :: uster_tra
        real, parameter :: ref_uster_tra =   3.21115643E-01
        real :: ol_tra
        real, parameter :: ref_ol_tra =   1.39790878E+02
        real :: htot
        real, parameter :: ref_htot =   5.00000000E+00
        real :: htt
        real, parameter :: ref_htt =   5.00000000E+00
        real :: onder
        real, parameter :: ref_onder =   1.00000000E+00
        real :: uh
        real, parameter :: ref_uh =   7.16747808E+00
        real :: zu
        real, parameter :: ref_zu =   8.82900620E+01
        real :: qruim
        real, parameter :: ref_qruim =   0.00000000E+00
        real :: qbron
        real, parameter :: ref_qbron =   1.00000000E+00
        real :: dispg
        real, parameter :: ref_dispg =   7.10568428E-02
        ol_metreg_rcp = in_ol_metreg_rcp
        ref_error%haserror = .false.
        ref_error%message = ""
        dispg = ref_dispg
        call ops_stab_rek( varin, icm, rb_ms, temp_C, h0, z0_metreg_rcp, disxx, disx, z0_rcp, xl, radius, qtr, qrv, &
            dv, ecvl, coef_space_heating, ibtg, uster_metreg_rcp, hbron, qww, D_stack, V_stack, Ts_stack, &
            emis_horizontal, ircp, istab, itra, qob, xloc, regenk, ra_ms_4, z0_tra, z0_src, ol_metreg_rcp, error, &
            uster_rcp, ol_rcp, uster_src, ol_src, uster_tra, ol_tra, htot, htt, onder, uh, zu, qruim, &
            qbron, dispg)
        call assertEqual( ref_ol_metreg_rcp, ol_metreg_rcp, tol, "ol_metreg_rcp", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_stab_rek", __LINE__, __FILE__)
        call assertEqual( ref_uster_rcp, uster_rcp, tol, "uster_rcp", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_rcp, ol_rcp, tol, "ol_rcp", __LINE__, __FILE__)
        call assertEqual( ref_uster_src, uster_src, tol, "uster_src", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_src, ol_src, tol, "ol_src", __LINE__, __FILE__)
        call assertEqual( ref_uster_tra, uster_tra, tol, "uster_tra", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_tra, ol_tra, tol, "ol_tra", __LINE__, __FILE__)
        call assertEqual( ref_htot, htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_htt, htt, tol, "htt", __LINE__, __FILE__)
        call assertEqual( ref_onder, onder, tol, "onder", __LINE__, __FILE__)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertEqual( ref_qruim, qruim, tol, "qruim", __LINE__, __FILE__)
        call assertEqual( ref_qbron, qbron, tol, "qbron", __LINE__, __FILE__)
        call assertEqual( ref_dispg, dispg, tol, "dispg", __LINE__, __FILE__)
   end subroutine test_ops_stab_rek2

   subroutine test_ops_stab_rek()
   use m_ops_stab_rek
   use no_pfunit_ops_lt
   use m_error, only: TError
   use m_ops_varin, only: Tvarin

        type(Tvarin) :: varin
        real, parameter :: tol = 1e-5
        integer, parameter :: icm = 4
        real, parameter :: rb_ms =   2.72685947E+01
        real, parameter :: temp_C =   1.25771332E+01
        real, parameter :: h0 =   5.04353943E+01
        real, parameter :: z0_metreg_rcp =   2.89999992E-01
        real, parameter :: disxx =   2.11424922E+04
        real, parameter :: disx = -999.0
        real, parameter :: z0_rcp =   2.00000003E-01
        real, parameter :: xl =   2.96321716E+02
        real, parameter :: radius =   0.00000000E+00
        real, parameter :: qtr =   0.00000000E+00
        real, parameter :: qrv =   0.00000000E+00
        integer, parameter :: dv = 33
        real, parameter :: ecvl(6,4,36) = reshape( (/   1.20471072E+00,   1.20982134E+00,   9.50585306E-01, &
             9.77232754E-01,   8.31212103E-01,   8.49113882E-01,   1.23252738E+00,   1.20151615E+00,   9.36372101E-01, &
             9.84885693E-01,   9.44933355E-01,   8.50000024E-01,   1.23885250E+00,   1.18642557E+00,   9.91904795E-01, &
             9.42245603E-01,   9.53809619E-01,   8.42040837E-01,   1.25147545E+00,   1.19587779E+00,   1.03138340E+00, &
             9.32492554E-01,   9.58118677E-01,   8.46203148E-01,   1.27132213E+00,   1.28992844E+00,   9.59853649E-01, &
             9.84465420E-01,   7.45707035E-01,   7.83670843E-01,   1.30967033E+00,   1.29234648E+00,   9.84976649E-01, &
             9.85753298E-01,   1.20026648E+00,   8.19626212E-01,   1.28999984E+00,   1.28558230E+00,   1.13198411E+00, &
             9.30140316E-01,   1.11933339E+00,   8.12244892E-01,   1.31967211E+00,   1.28118312E+00,   1.03956521E+00, &
             1.01890218E+00,   9.50792015E-01,   9.00534749E-01,   1.48165286E+00,   1.46296406E+00,   9.46243882E-01, &
             9.68805015E-01,   5.65303028E-01,   6.40886068E-01,   1.43725264E+00,   1.46823108E+00,   9.11860526E-01, &
             1.01776266E+00,   9.38399971E-01,   6.31495357E-01,   1.40262282E+00,   1.51240957E+00,   1.09111106E+00, &
             9.22280610E-01,   1.04600000E+00,   5.93959153E-01,   1.42639339E+00,   1.48824418E+00,   1.15675902E+00, &
             9.65192854E-01,   1.04970300E+00,   5.73048115E-01,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   1.27132213E+00,   1.28992844E+00,   9.59853649E-01, &
             9.84465420E-01,   7.45707035E-01,   7.83670843E-01,   1.30967033E+00,   1.29234648E+00,   9.84976649E-01, &
             9.85753298E-01,   1.20026648E+00,   8.19626212E-01,   1.28999984E+00,   1.28558230E+00,   1.13198411E+00, &
             9.30140316E-01,   1.11933339E+00,   8.12244892E-01,   1.31967211E+00,   1.28118312E+00,   1.03956521E+00, &
             1.01890218E+00,   9.50792015E-01,   9.00534749E-01,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   1.41966939E+00,   1.53553557E+00,   9.39365804E-01, &
             9.64150906E-01,   5.39090931E-01,   6.18059039E-01,   1.44791222E+00,   1.51231039E+00,   9.14697647E-01, &
             1.00826478E+00,   9.37999964E-01,   6.11838043E-01,   1.49606550E+00,   1.53216875E+00,   1.10178566E+00, &
             9.19438481E-01,   1.03295255E+00,   5.68612218E-01,   1.47918034E+00,   1.52988541E+00,   1.17363644E+00, &
             9.51097965E-01,   1.04247522E+00,   5.60748696E-01,   1.61107445E+00,   1.56799984E+00,   8.90634179E-01, &
             9.23962295E-01,   5.22929251E-01,   5.78860760E-01,   1.63384604E+00,   1.54584837E+00,   7.89255857E-01, &
             9.92009103E-01,   6.28266633E-01,   5.50031126E-01,   1.65213108E+00,   1.51405621E+00,   8.81468177E-01, &
             8.90140295E-01,   7.31523871E-01,   5.19469321E-01,   1.66016400E+00,   1.54503810E+00,   1.10869563E+00, &
             7.62937725E-01,   8.89901042E-01,   4.53957170E-01,   1.55314052E+00,   1.52735710E+00,   9.29170728E-01, &
             9.43396151E-01,   5.24999976E-01,   5.97974658E-01,   1.53626370E+00,   1.51483750E+00,   8.48511577E-01, &
             1.01301360E+00,   8.13600063E-01,   5.81713319E-01,   1.54229510E+00,   1.52104414E+00,   9.95515943E-01, &
             9.06526268E-01,   8.97238076E-01,   5.43591797E-01,   1.53327858E+00,   1.52904582E+00,   1.15043473E+00, &
             8.53679538E-01,   9.79405820E-01,   5.14759302E-01,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00,   0.00000000E+00, &
             0.00000000E+00,   0.00000000E+00,   0.00000000E+00/), &
          (/6,4,36/) )
        real, parameter :: coef_space_heating =   5.10666656E+00
        integer, parameter :: ibtg = 0
        real, parameter :: uster_metreg_rcp =   2.71183252E-01
        real, parameter :: hbron =   5.00000000E+00
        real, parameter :: qww =   0.00000000E+00
        real, parameter :: D_stack =  -9.99000000E+02
        real, parameter :: V_stack =  -9.99000000E+02
        real, parameter :: Ts_stack =  -9.99000000E+02
        logical, parameter :: emis_horizontal = .false.
        integer, parameter :: ircp = 1
        integer, parameter :: istab = 1
        integer, parameter :: itra = 1
        real, parameter :: qob =   1.00000000E+00
        real, parameter :: xloc =   2.08999985E+02
        real, parameter :: regenk =   1.70000009E-02
        real, parameter :: ra_ms_4 =   2.01143246E+01
        real, parameter :: z0_tra =   2.00000003E-01
        real, parameter :: z0_src =   2.00000003E-01
        real, parameter :: in_ol_metreg_rcp =  -4.03774261E+01
        real :: ol_metreg_rcp
        real, parameter :: ref_ol_metreg_rcp =  -4.03774261E+01
        type(TError) :: error
        type(TError) :: ref_error
        real :: uster_rcp
        real, parameter :: ref_uster_rcp =   2.39550680E-01
        real :: ol_rcp
        real, parameter :: ref_ol_rcp =  -2.59285927E+01
        real :: uster_src
        real, parameter :: ref_uster_src =   2.52821952E-01
        real :: ol_src
        real, parameter :: ref_ol_src =  -3.29010468E+01
        real :: uster_tra
        real, parameter :: ref_uster_tra =   2.52821952E-01
        real :: ol_tra
        real, parameter :: ref_ol_tra =  -3.29010468E+01
        real :: htot
        real, parameter :: ref_htot =   5.00000000E+00
        real :: htt
        real, parameter :: ref_htt =   5.00000000E+00
        real :: onder
        real, parameter :: ref_onder =   1.00000000E+00
        real :: uh
        real, parameter :: ref_uh =   2.93962026E+00
        real :: zu
        real, parameter :: ref_zu =   1.48160858E+02
        real :: qruim
        real, parameter :: ref_qruim =   0.00000000E+00
        real :: qbron
        real, parameter :: ref_qbron =   1.00000000E+00
        real :: dispg
        real, parameter :: ref_dispg(1) = (/   2.14678213E-01 /)
        ol_metreg_rcp = in_ol_metreg_rcp
        ref_error%haserror = .false.
        ref_error%message = ""
        dispg = ref_dispg(1)
        call ops_stab_rek( varin, icm, rb_ms, temp_C, h0, z0_metreg_rcp, disxx, disx, z0_rcp, xl, radius, qtr, qrv, &
            dv, ecvl, coef_space_heating, ibtg, uster_metreg_rcp, hbron, qww, D_stack, V_stack, Ts_stack, &
            emis_horizontal, ircp, istab, itra, qob, xloc, regenk, ra_ms_4, z0_tra, z0_src, ol_metreg_rcp, error, &
            uster_rcp, ol_rcp, uster_src, ol_src, uster_tra, ol_tra, htot, htt, onder, uh, zu, qruim, &
            qbron, dispg)
        call assertEqual( ref_ol_metreg_rcp, ol_metreg_rcp, tol, "ol_metreg_rcp", __LINE__, __FILE__)
        call assertEqual(error, ref_error, "ops_stab_rek", __LINE__, __FILE__)
        call assertEqual( ref_uster_rcp, uster_rcp, tol, "uster_rcp", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_rcp, ol_rcp, tol, "ol_rcp", __LINE__, __FILE__)
        call assertEqual( ref_uster_src, uster_src, tol, "uster_src", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_src, ol_src, tol, "ol_src", __LINE__, __FILE__)
        call assertEqual( ref_uster_tra, uster_tra, tol, "uster_tra", __LINE__, __FILE__)
        call assertRelativelyEqual( ref_ol_tra, ol_tra, tol, "ol_tra", __LINE__, __FILE__)
        call assertEqual( ref_htot, htot, tol, "htot", __LINE__, __FILE__)
        call assertEqual( ref_htt, htt, tol, "htt", __LINE__, __FILE__)
        call assertEqual( ref_onder, onder, tol, "onder", __LINE__, __FILE__)
        call assertEqual( ref_uh, uh, tol, "uh", __LINE__, __FILE__)
        call assertEqual( ref_zu, zu, tol, "zu", __LINE__, __FILE__)
        call assertEqual( ref_qruim, qruim, tol, "qruim", __LINE__, __FILE__)
        call assertEqual( ref_qbron, qbron, tol, "qbron", __LINE__, __FILE__)
        call assertEqual( ref_dispg(1), dispg, tol, "dispg", __LINE__, __FILE__)
   end subroutine test_ops_stab_rek
end module m_test_ops_stab_rek
 
program p_test_ops_stab_rek
use m_test_ops_stab_rek
use no_pfunit
implicit none
   call test_ops_stab_rek3()
   call test_ops_stab_rek2()
   call test_ops_stab_rek()
   call conclusion()
end program p_test_ops_stab_rek

