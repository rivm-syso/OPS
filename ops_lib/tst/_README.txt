test directory for subroutines in OPS-library
Use is made of the Fortran Unit test software pFUnit (see CMakeLists.txt)

tst_m_ops_plumerise.pf : plume rise
tst_m_ops_meteo.pf     : meteo routines (wind profiles)

=> more tests needed for ops_plumerise, independent tests/measurements  !!!!!!!!!!!!

--------------------------------------------------------------------------------------------------------------------------------------
Short manual pFUnit

1. The start of each test suite for pFUnit is a .pf file, which defines one or more tests. In our set-up, we have one .pf file for each module
   and one unit test for each routine in the module. A .pf file contains normal Fortran code, with a few pFUnit directives e.g. @test, @assert...
   Each test is indicated by @test. Example for tst_m_ops_meteo.pf: 
      
   @test
   subroutine tst_ops_wv_log_profile
   ...
   end subroutine tst_ops_wv_log_profile
   
   @test
   subroutine tst_ops_wv_powerlaw_metstat
   ...
   end subroutine tst_ops_wv_powerlaw_metstat
      
   There are only subroutines or functions in a .pf file, the main program is a standard driver program from pFUnit.
   
   A good way to start a .pf file is to copy the routine we want to test:
   cp ../m_ops_meteo.f90 tst_m_ops_meteo.pf
   
   - Insert @test above each subroutine we want to test 
   - prefix the subroutine name with tst_ 
   - add statement: USE funit
   - add use statements that are needed to call the routine that is to be tested
   - add statement: implicit none
   - keep the declarations of the input/output arguments, but remove the intent (all variables are local to the tst_ routine)
   - replace the original subroutine header with a call to that routine (and put it after the declarations)
   - skip the arguments in the tst_ routine (do not forget this, otherwise you will get a strange SEGMENTATION FAULT)
   - give the input arguments a value (before the call)
   - check the output arguments against a reference value (after the call) - see 2
   
   A skeleton test file looks like this:
   @test
   subroutine tst_sub1
   
   ! test sqrt implementation 
   
   use funit
   use m_my_sqrt
   
   implicit none
   
   ! Input
   real           :: x       ! input to sqrt
   
   ! Output:
   output         :: y      ! output y = sqrt(x) 
   
   ! Define input, call rouitne and check output:
   arg_in = 2
   call sub1(arg_in,arg_out)
   @assertEqual(1.4142135,arg_out,message=' water temperature [K];')
   
   end subroutine tst_rc_temp_water

   
   See as an example the routine ops_wv_log_profile in m_ops_meteo.f90 and the test in tst_m_ops_meteo.
      
2. Include assertions in the .pf file
   In a .pf file, we test the outcome of a subroutine against some reference values, using pFUnit functions, such as @assertEqual. 
   Testing equality of reals should be done using a tolerance. We can use an absolute tolerance, e.g. tol_abs = 1.0e-5.
   The following call fails if |real_expected - real_found| > tol_abs:
   @assertEqual(real_expected,real_found,tol_abs,message='message; ')  

   In some cases (most cases?) it is better to test equality with a relative tolerance, e.g. tol_rel = 0.001.
   The following call fails if  |real_expected - real_found|/|real_expected| > tol_rel:
   @assertRelativelyEqual(real_expected,real_found,tol_rel,message='message; ')  
   Note: the error mesage gives the absolute error, but the error is detected correctly.

3. Get a CMakeLists.txt from another tst-directory and adapt it to the current tst-directory.

4. > CMake uses CMakeLists.txt to generate a test program tst.exe in the following steps:
   a) preprocess .pf files -> .F90 files (containing information on line numbers)
   b) compile .F90 files -> .o files
   c) Link .o files, ../ops_lib_debug.a, library and driver program of pFUnit, into executable tst.exe

5. > tst.exe runs the test suite and gives either failures or gives OK if all tests have been without error. 
   Note that for pFUnit each unit test may contain several asserts. Example:
   test suite
      test A
        assert A1
        assert A2
        assert A3
      test B
         assert B1
         assert B2

   Note that if assert A2 fails, then there is a failure of test A and assert A3 is not tested anymore; pFUnit continues with test B. 
   pFUnit gives the number of failed unit tests (so not the number of failed asserts) and the .pf file and line number where the failure occurred. 
   If you want pFUnit to continue with assert A3 after a failure, you could use the -robust option, but this has not been installed yet.

6. Note that the test suite is coupled to the CMakeLists.txt in the parent directory. This means that each successfull compilation in the
   parent directory is automatically followed by running the test suite. 

