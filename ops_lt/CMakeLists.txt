cmake_minimum_required (VERSION 3.18)
enable_testing()
project (ops_lt Fortran)

# set(OPSLIB_DIR ${CMAKE_CURRENT_SOURCE_DIR}/ops_lib)
# list(APPEND CMAKE_MODULE_PATH  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/")
# list(APPEND CMAKE_MODULE_PATH  "${OPSLIB_DIR}/cmake/")

# Append ops_lib/cmake to module path:
list(APPEND CMAKE_MODULE_PATH  "${CMAKE_CURRENT_SOURCE_DIR}/ops_lib/cmake/")

# Configure cmake process:
include(OpsConfigure)
OpsConfigure()

# SetCompilerOptions()
# SetMath77Target()
# 
# ################################################################################
# ## FS pFUnit disabled
# ## pFUnit is unusually difficult to install properly:
# ## several hacks are needed to make it work.
# ## Moreover, pFUnit does not work with ifx or on Windows.
# ## As a consequence of all these difficulties, it was not reasonably possible to
# ## write a function to include pFUnit in the CMakeLists.  Instead, the script
# ## below does that (install pFUnit).
# ## It may be wise to switch all unit tests to nopfunit-tests and remove pFUnit
# ## from this project
# #set(PFUNIT_DIR "${OPSLIB_DIR}/pFUnit")
# #include(CompilePFUnit)
# ################################################################################
# 
# SetNoPfUnitTarget( DIR "${OPSLIB_DIR}/nopfunit")
# SetOpsTarget(      DIR "${OPSLIB_DIR}")
# SetOpsLtTarget()
# 
# # Define main = name of executable that runs the simulations and create target
# SetOpsLtMainTarget()
# 
# # The targets ctest (which creates ctest.xml) and coverage depend on
# # running the test suite, which depends on all libraries and tests to have been
# # compiled and run. The list of all targets that need to be built before running the
# # tests is kept in array ${test_dependencies}.
# # ${test_dependencies} is extended by functions CreateUnitTests and RegressionTests.
# set(test_dependencies ops nopfunit ${main} ${writer})
# 
# CreateUnitTests(DIR ${OPSLIB_DIR}/tst               PREFIX lib LIBS ops)
# CreateUnitTests(DIR ${CMAKE_CURRENT_SOURCE_DIR}/tst PREFIX lt  LIBS opslt)
# RegressionTests(LEVEL ${TEST_LEVEL})
# SetCtestTarget()
# 
# SetCoverageTarget()
# SetProfilingTarget(LEVEL ${TEST_LEVEL})
