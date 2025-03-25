
function(OpsConfigure)

   # The build directory must start with "build", (to prevent to start this at e.g. ops_lib):
   get_filename_component(build_dir ${CMAKE_CURRENT_BINARY_DIR} NAME)
   string(FIND ${build_dir} build ipos)
   if (NOT ${ipos} EQUAL 0)
      message("\n\nError")
      message("The build directory is ${build_dir}")
      message("The build directory (where you start cmake ..) must start with the string \"build\"\n")
   else()

      # Define OPSLIB_DIR and append cmake subdirectory to cmake-module-path:
      if (${PROJECT_NAME} STREQUAL "ops_lib")
         set(OPSLIB_DIR ${CMAKE_CURRENT_SOURCE_DIR})
      else()
         set(OPSLIB_DIR ${CMAKE_CURRENT_SOURCE_DIR}/ops_lib)
      endif()
      list(APPEND CMAKE_MODULE_PATH  "${OPSLIB_DIR}/cmake/")

      # Define functions:
      include(OpsFunctions)

      # Set default definitions into Cache:
      # set(CMAKE_BUILD_TYPE release CACHE STRING "")  # Is already set by SetCompilerOptions
      set(TEST_LEVEL 1 CACHE STRING "")
      set(CMAKE_Fortran_COMPILER ifort CACHE STRING "")
      set(CMAKE_Fortran_MODULE_DIRECTORY include CACHE STRING "")
      set(CMAKE_LIBRARY_OUTPUT_DIRECTORY lib CACHE STRING "")
      set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY lib CACHE STRING "")
      set(CMAKE_RUNTIME_OUTPUT_DIRECTORY bin CACHE STRING "")
      option(SKIP_OPENMP "Build without OpenMP.")

      # Set compiler option:
      SetCompilerOptions()

      # Install numdiff (only for ops_st) to compare files including tolerance:
      if (${PROJECT_NAME} STREQUAL "ops_st")
         InstallNumdiff()
      endif()

      # Set targets (define rules) and define main = name of executable that runs the simulations and create target
      SetMath77Target()
      SetNoPfUnitTarget( DIR "${OPSLIB_DIR}/nopfunit")
      SetOpsLibTarget(   DIR "${OPSLIB_DIR}")
      if (${PROJECT_NAME} STREQUAL "ops_lib")
         set(main "")
      else()
         SetOpsTarget()
         SetOpsMainTarget()
      endif()

      # The targets ctest (which creates ctest.xml) and coverage depend on
      # running the test suite, which depends on all libraries and tests to have been
      # compiled and run. The list of all targets that need to be built before running the
      # tests is kept in array ${test_dependencies}.
      # ${test_dependencies} is extended by functions CreateUnitTests and RegressionTests.
      set(test_dependencies ops_lib nopfunit ${main})

      # Create unit tests for ops_lib:
      CreateUnitTests(DIR ${OPSLIB_DIR}/tst/level_1 PREFIX lib LIBS ops_lib)

      # Create tests for ops_st or ops_lt:
      if (NOT ${PROJECT_NAME} STREQUAL "ops_lib")
         string(REPLACE "ops_" "" prefix ${PROJECT_NAME}) # prefix = lt or st
         CreateUnitTests(DIR ${CMAKE_CURRENT_SOURCE_DIR}/tst/level_1 PREFIX ${prefix} LIBS ${PROJECT_NAME})
         CreateRegressionTests(LEVEL ${TEST_LEVEL} NTHREADS ${NTHREADS})
      endif()

      # Set target (define rules) for ctest, coverage, profiling:
      SetCtestTarget()
      SetCoverageTarget()
      if (NOT ${PROJECT_NAME} STREQUAL "ops_lib")
         SetProfilingTarget(LEVEL ${TEST_LEVEL})
      endif()

      # Pass output variables to PARENT_SCOPE
      set(CMAKE_BUILD_TYPE               ${CMAKE_BUILD_TYPE}              PARENT_SCOPE)
      set(CMAKE_COVERAGE_POSTFIX         ${CMAKE_POSTFIX}                 PARENT_SCOPE)
      set(CMAKE_DEBUG_POSTFIX            ${CMAKE_POSTFIX}                 PARENT_SCOPE)
      set(CMAKE_Fortran_FLAGS            ${CMAKE_Fortran_FLAGS}           PARENT_SCOPE)
      set(CMAKE_Fortran_FLAGS_COVERAGE   ${CMAKE_Fortran_FLAGS_DEBUG}     PARENT_SCOPE)
      set(CMAKE_Fortran_FLAGS_DEBUG      ${CMAKE_Fortran_FLAGS_DEBUG}     PARENT_SCOPE)
      set(CMAKE_Fortran_FLAGS_PROFILING  ${CMAKE_Fortran_FLAGS_PROFILING} PARENT_SCOPE)
      set(CMAKE_Fortran_FLAGS_RELEASE    ${CMAKE_Fortran_FLAGS_RELEASE}   PARENT_SCOPE)
      set(CMAKE_POSTFIX                  ${CMAKE_POSTFIX}                 PARENT_SCOPE)
      set(CMAKE_RELEASE_POSTFIX          ${CMAKE_POSTFIX}                 PARENT_SCOPE)
      set(COVERAGE_OPTIONS               ${COVERAGE_OPTIONS}              PARENT_SCOPE)
      set(PROFILE_OPTIONS                ${PROFILE_OPTIONS}               PARENT_SCOPE)
      set(main "${main}" PARENT_SCOPE)
   endif()
endfunction()

