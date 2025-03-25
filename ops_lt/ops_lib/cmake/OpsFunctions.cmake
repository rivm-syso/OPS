
# Functions are put into four chapters:    FS make separate modules?
#   compilation
#   set targets
#   tests
#   utility fucnctions

######################################################################################
# Chapter compilation
#    SetCompilerOptions
######################################################################################

function(SetCompilerOptions)
#
# SetCompilerOptions()
#
# Set the linker and compiler flags.
# Also choose a postfix for compiled targets.
#
    start_section("compiler_options")

    if (CMAKE_CONFIGURATION_TYPES)
        set(CMAKE_CONFIGURATION_TYPES "Debug" "Release" "Coverage" "Profiling")
        set(CMAKE_EXE_LINKER_FLAGS_COVERAGE ${CMAKE_EXE_LINKER_FLAGS_DEBUG})
        list(APPEND CMAKE_EXE_LINKER_FLAGS_COVERAGE --coverage)
        set(CMAKE_EXE_LINKER_FLAGS_PROFILING ${CMAKE_EXE_LINKER_FLAGS_RELEASE})
    elseif (NOT CMAKE_BUILD_TYPE)
        set(CMAKE_BUILD_TYPE DEBUG CACHE STRING
           "Choose the type of build, options are: Debug, Release, Coverage."
           FORCE)
    endif()

    # Set CMAKE_*_POSTFIX, so targets will have indication of the compiler and
    # build type in their file names
    string(TOUPPER "${CMAKE_BUILD_TYPE}"  CMAKE_BUILD_TYPE)
    message("Fortran compiler = ${CMAKE_Fortran_COMPILER_ID}")
    message("CMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}")
    set(CMAKE_POSTFIX "_${CMAKE_BUILD_TYPE}_${CMAKE_Fortran_COMPILER_ID}")

    # Compile flags depend on the compiler:

    if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
        if(WIN32)
            set(PROFILE_OPTIONS "-fno-omit-frame-pointer")
            set(COVERAGE_OPTIONS "")
            set(CMAKE_Fortran_FLAGS "-fpp -assume:byterecl -extend-source -F10485760")
            set(CMAKE_Fortran_FLAGS_RELEASE "-nowarn -debug:full -Ofast")
            set(CMAKE_Fortran_FLAGS_DEBUG
               " -warn unused -diag-disable8291 -diag-disable7841 -check all,nouninit  -debug-parameters all -traceback -Od")
        else()
            set(PROFILE_OPTIONS "-fno-omit-frame-pointer -pg")
            set(COVERAGE_OPTIONS "-prof-gen=srcpos")
            set(CMAKE_Fortran_FLAGS "-DUNIX -fpp -assume byterecl -extend-source -g -heap-arrays 1")
            set(CMAKE_Fortran_FLAGS_RELEASE "-nowarn -O2" )
            #set(CMAKE_Fortran_FLAGS_DEBUG  " -warn unused -diag-disable8291 -diag-disable7841 -check all,nouninit  -debug-parameters all -traceback -O0 -double-size 128 -real-size 64")
            set(CMAKE_Fortran_FLAGS_DEBUG  " -warn unused -diag-disable8291 -diag-disable7841 -check all,nouninit  -debug-parameters all -traceback -O0  -fpe0 -init=snan")
        endif()
    elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
        set(PROFILE_OPTIONS "-fprofile-arcs")
        set(COVERAGE_OPTIONS "--coverage")
        set(CMAKE_Fortran_FLAGS         "-DUNIX -fpp -assume byterecl -extend-source")
        set(CMAKE_Fortran_FLAGS_RELEASE "-nowarn -O2 -fp-model=precise" )
        set(CMAKE_Fortran_FLAGS_DEBUG  " -warn unused -check all,nouninit  -debug-parameters all -traceback -O0 -g -fpe0 -init=snan" )
        if (CMAKE_BUILD_TYPE STREQUAL "COVERAGE")
             add_link_options(--coverage)
        endif()
    elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
        if(WIN32)
            set(PROFILE_OPTIONS "-fno-omit-frame-pointer -Og")
        else()
            set(PROFILE_OPTIONS "-fno-omit-frame-pointer -pg -g -Og")
        endif()
        set(COVERAGE_OPTIONS "--coverage")
        set(CMAKE_Fortran_FLAGS_RELEASE "-O2 -fno-fast-math" )
        set(CMAKE_Fortran_FLAGS_DEBUG   "-g -fbacktrace -O0 " )  #-pedantic
        set(warnings
            all error=shadow error=intrinsic-shadow uninitialized aggregate-return extra
            array-bounds unreachable-code conversion no-maybe-uninitialized no-aggregate-return
            no-unused-dummy-argument no-error=return-type  no-error=unused-function aliasing
            ampersand c-binding-type character-truncation conversion do-subscript
            function-elimination implicit-procedure intrinsic-shadow intrinsics-std
            line-truncation real-q-constant surprising underflow unused-parameter
            frontend-loop-interchange target-lifetime no-compare-reals )

        foreach( warning ${WARNINGS})
            set(CMAKE_Fortran_FLAGS_DEBUG   "${CMAKE_Fortran_FLAGS_DEBUG} -W${warning}" )
        endforeach(warning)
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-0 -ffixed-line-length-0 -cpp -fdec -DUNIX")
        if (CMAKE_BUILD_TYPE STREQUAL "COVERAGE")
            add_link_options(--coverage)
        endif()
    else ()
        message(FATAL_ERROR "Unknown CMAKE_Fortran_COMPILER_ID: ${CMAKE_Fortran_COMPILER_ID}")
    endif ()

    # Flags for OpenMP:
    if (${SKIP_OPENMP})
       message("Not including OpenMP.")
    else()
       find_package(OpenMP)
       set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
    endif()

    # Finish setting compiler optionss:
    set (CMAKE_Fortran_FLAGS_PROFILING "${CMAKE_Fortran_FLAGS_RELEASE} ${PROFILE_OPTIONS}")
    message("CMAKE_Fortran_FLAGS  = ${CMAKE_Fortran_FLAGS}")
    message("CMAKE_Fortran_FLAGS_${CMAKE_BUILD_TYPE}  = ${CMAKE_Fortran_FLAGS_${CMAKE_BUILD_TYPE}}")

    end_section("compiler_options")

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

endfunction()


######################################################################################
# Chapter set targets
#   SetMath77Target
#   InstallNumdiff
#   SetNoPfUnitTarget
#   SetOpsLibTarget
#   SetOpsTarget
#   SetOpsMainTarget
#   SetCoverageTarget
#   SetProfilingTarget
######################################################################################
function(SetMath77Target)
#
# SetMath77Target
#
# Download MATH77 and set the target math77.
# MATH77 is only downloaded when it is not yet available.
#
    set(math77_content_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/MATH77)

    if (NOT EXISTS ${math77_content_SOURCE_DIR})
        file(DOWNLOAD https://netlib.org/math/math77.tgz ${CMAKE_CURRENT_BINARY_DIR}/math77.tgz
             EXPECTED_HASH SHA256=71cff919c7aa809ef3711c38ed80fda44d65caeca770c875b57bc426ce62d7ba)
        execute_process(
            COMMAND ${CMAKE_COMMAND} -E tar xfz "${CMAKE_CURRENT_BINARY_DIR}/math77.tgz"
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
        execute_process(
            COMMAND ${CMAKE_COMMAND} -E rm "${CMAKE_CURRENT_BINARY_DIR}/math77.tgz"
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
    endif()

    file(GLOB MATH77_SRC_FILES
         ${math77_content_SOURCE_DIR}/mess.f
         ${math77_content_SOURCE_DIR}/smess.f
         ${math77_content_SOURCE_DIR}/amach.f
         ${math77_content_SOURCE_DIR}/optchk.f
         ${math77_content_SOURCE_DIR}/silup.f
         ${math77_content_SOURCE_DIR}/silupm.f
    )
    add_library(math77 STATIC ${MATH77_SRC_FILES})
endfunction()

######################################################################################
function(InstallNumdiff)
#
# InstallNumdiff
#
# Download numdiff and install numdiff:
#    numdiff: show differences between files; numerical differences above certain threshold.
# Numdiff is only downloaded and installed when it is not yet available.
#
    start_section("InstallNumdiff")
    set(numdiff_version 5.9.0)
    set(numdiff_dir ${CMAKE_CURRENT_SOURCE_DIR}/numdiff)
    set(numdiff_tgz numdiff-${numdiff_version}.tar.gz)

        # TEST
        #execute_process(
        #    COMMAND ${CMAKE_COMMAND} -E env bash /usr/bin/date
        #    ECHO_OUTPUT_VARIABLE
        #    ECHO_ERROR_VARIABLE
        #    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

    # Download, untar, configure, make, make install:
    if (NOT EXISTS ${numdiff_dir})
        message("Installing numdiff may take a while, but is only done once")
        message("download numdiff:")
        file(DOWNLOAD http://savannah.nongnu.org/download/numdiff/${numdiff_tgz} ${CMAKE_CURRENT_BINARY_DIR}/${numdiff_tgz})

        message("untar numdiff:")
        execute_process(
            COMMAND ${CMAKE_COMMAND} -E tar xfz "${CMAKE_CURRENT_BINARY_DIR}/${numdiff_tgz}"
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

        # Script for configure/make/make install:
        message("my_install from ${CMAKE_CURRENT_BINARY_DIR}/numdiff-${numdiff_version}")
        file(WRITE  "${CMAKE_CURRENT_BINARY_DIR}/numdiff-${numdiff_version}/my_install" "#!/bin/bash\n")
        file(APPEND  "${CMAKE_CURRENT_BINARY_DIR}/numdiff-${numdiff_version}/my_install" "./configure --prefix=${numdiff_dir}\n")
        file(APPEND "${CMAKE_CURRENT_BINARY_DIR}/numdiff-${numdiff_version}/my_install" "make\n")
        file(APPEND "${CMAKE_CURRENT_BINARY_DIR}/numdiff-${numdiff_version}/my_install" "make install\n")
        execute_process(
            COMMAND ${CMAKE_COMMAND} -E env bash ./my_install
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/numdiff-${numdiff_version})

        # Remove tgz-file:
        execute_process(
            COMMAND ${CMAKE_COMMAND} -E rm "${CMAKE_CURRENT_BINARY_DIR}/${numdiff_tgz}"
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    endif()
    end_section("InstallNumdiff")

endfunction()

######################################################################################
function(SetNoPfUnitTarget)
cmake_parse_arguments(PARSE_ARGV 0 NOPFUNIT "" "DIR" "")
#
# SetNoPFUnitTarget(DIR ${dir})
#
# Create the target nopfunit
#
# DIR: directory with nopfunit sources
#
    file(GLOB SRC_FILES ${NOPFUNIT_DIR}/*.f90)
    add_library(nopfunit STATIC ${SRC_FILES})
endfunction()

######################################################################################
function(SetOpsLibTarget)
cmake_parse_arguments(PARSE_ARGV 0 OPSLIB "" "DIR" "")
#
# SetOpsLibTarget(DIR ${dir})
#
# Create the target ops_lib
#
# DIR: directory with ops_lib sources
#
    file(GLOB SRC_FILES ${OPSLIB_DIR}/src/*.f90)
    add_library(ops_lib STATIC ${SRC_FILES})
    target_link_libraries(ops_lib PUBLIC math77 nopfunit)
    if (CMAKE_BUILD_TYPE STREQUAL "COVERAGE")
        target_compile_options(ops_lib PRIVATE ${COVERAGE_OPTIONS})
    endif()
endfunction()

######################################################################################
function(SetOpsTarget)
#
# SetOpsTarget()
#
# Create the target PROJECT_NAME = ops_lt or ops_st: a library of all the modules of OPS-LT or -ST
#
    set(SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src)
    file(GLOB SRC_FILES ${SRC_DIR}/*.f90 ${SRC_DIR}/*.f)
    # mian program is not in library, is compiled and linked further on:
    list(REMOVE_ITEM SRC_FILES "${PROJECT_NAME}_main.f90")  # PROJECT_NAME = ops_lt or ops_st
    add_library(${PROJECT_NAME} STATIC ${SRC_FILES})
    if(NOT WIN32)
        target_link_libraries(${PROJECT_NAME} PUBLIC ops_lib gcov)
    else()
        target_link_libraries(${PROJECT_NAME} PUBLIC ops_lib)
    endif()
endfunction()

######################################################################################
function(SetOpsMainTarget)
#
# SetOpsMainTarget()
#
# Create the target ops_lt_main... or ops_st_main... : the executable that runs the simulations
#
    set(SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src)
    set(main "${PROJECT_NAME}_main${CMAKE_POSTFIX}")
    message("Name of executable = ${main}")
    add_executable(${main} ${SRC_DIR}/${PROJECT_NAME}_main.f90) # PROJECT_NAME = ops_lt or ops_st
    if (${SKIP_OPENMP})
       target_link_libraries(${main} PUBLIC ${PROJECT_NAME})
    else()
       target_link_libraries(${main} PUBLIC ${PROJECT_NAME} OpenMP::OpenMP_Fortran)
    endif()
    if (NOT WIN32 AND CMAKE_BUILD_TYPE STREQUAL "COVERAGE")
        target_compile_options(${PROJECT_NAME} PRIVATE ${COVERAGE_OPTIONS})
        target_compile_options(${main} PRIVATE ${COVERAGE_OPTIONS})
        if(Fortran_COMPILER_NAME MATCHES "gfortran")
            target_link_libraries(${main} PUBLIC gcov)
        elseif(Fortran_COMPILER_NAME MATCHES "ifx")
            target_link_libraries(${main} PUBLIC gcov)
        endif()
    endif()

    # Pass output variables to PARENT_SCOPE
    set(main "${main}" PARENT_SCOPE)
endfunction()

######################################################################################
function(SetCoverageTarget)
#
# SetCoverageTarget()
#
# Creates the target coverage, which will create coverage info
#   + in XML-format (coverage.xml) and also
#   + in HTML-format (CODE_COVERAGE.HTML for ifort, coverage/coverage.html for gfortran)
#
    if (NOT WIN32 AND CMAKE_BUILD_TYPE STREQUAL "COVERAGE")
        start_section("create_coverage_target")
        if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
            add_custom_target(coverage
                              COMMAND profmerge
                              COMMAND codecov
                              COMMAND codecov -xmlbcvrg coverage.xml
                              DEPENDS ctest)
        elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
            add_custom_target(coverdir COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/coverage)
            add_custom_target(coverage
                              COMMAND gcovr --html-details ${CMAKE_CURRENT_BINARY_DIR}/coverage/coverage.html
                                            --html-title "${CMAKE_Fortran_COMPILER_ID} coverage report for OPS-${tst_prefix}"
                                            ${CMAKE_CURRENT_BINARY_DIR}
                              COMMAND gcovr --xml-pretty --xml ${CMAKE_CURRENT_BINARY_DIR}/coverage.xml --print-summary
                                            ${CMAKE_CURRENT_BINARY_DIR}
                              WORKING_DIRECTORY  ${CMAKE_CURRENT_SOURCE_DIR}
                              DEPENDS ctest coverdir)
        elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
            add_custom_target(coverdir COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/coverage)
            add_custom_target(coverage
                              COMMAND gcovr --html-details ${CMAKE_CURRENT_BINARY_DIR}/coverage/coverage.html
                                            --html-title "${CMAKE_Fortran_COMPILER_ID} coverage report for OPS-${tst_prefix}"
                                            ${CMAKE_CURRENT_BINARY_DIR}
                              COMMAND gcovr --xml-pretty --xml ${CMAKE_CURRENT_BINARY_DIR}/coverage.xml --print-summary
                                            ${CMAKE_CURRENT_BINARY_DIR}
                              WORKING_DIRECTORY  ${CMAKE_CURRENT_SOURCE_DIR}
                              DEPENDS ctest coverdir)
        else()
            message(FATAL_ERROR "Coverage not yet available for compiler with ID: ${CMAKE_Fortran_COMPILER_ID}")
        endif()
        end_section("create_coverage_target")
    endif()
endfunction()

######################################################################################
function(SetProfilingTarget)
cmake_parse_arguments(PARSE_ARGV 0 PROFILING "" "LEVEL" "")
#
# SetProfilingTarget( LEVEL ${level})
#
# Add the target profiling, which creates the files gprof-dot.svg that show the profiling information
# of the level 2 runs.
#
    if (PROFILING_LEVEL GREATER_EQUAL 2 AND CMAKE_BUILD_TYPE STREQUAL "PROFILING")
        set (main_full "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${main}")
        file(GLOB TST_DIRS "${CMAKE_CURRENT_BINARY_DIR}/tst/level_2/[A-Z]_test*/[A-Z][0-9]*")
        foreach(test_dir ${TST_DIRS})
            get_filename_component (tst_name ${test_dir} NAME)
            set(prof_dir "${CMAKE_CURRENT_BINARY_DIR}/profiling/level_2/${tst_name}")

            add_custom_target(prof-${tst_name}
                COMMAND mkdir -p ${prof_dir}
                COMMAND gprof ${main_full} ${test_dir}/run/gmon.out -A > ${prof_dir}/gprof-A.out
                COMMAND gprof ${main_full} ${test_dir}/run/gmon.out > ${prof_dir}/gprof.out
                COMMAND gprof ${main_full} ${test_dir}/run/gmon.out | gprof2dot -n 0.5 --color-nodes-by-selftime | dot -Tsvg > ${prof_dir}/gprof-dot.svg
                DEPENDS ctest)
            set(gmon ${gmon} prof-${tst_name})
        endforeach()

        # Add gprof summary, currently only for test F:      TODO: FS tests uitbreiden, ook voor level 3
        add_custom_target(prof-sum
                  COMMAND gprof -s ${main_full} ./tst/level_2/F_test*/F*/run/gmon.out
                  COMMAND gprof ${main_full} gmon.sum > profiling/gprof_Fsum.out
                  COMMAND gprof ${main_full} gmon.sum | gprof2dot --color-nodes-by-selftime --wrap -n 0.5 | dot -Tsvg > profiling/gprof_Fsum.svg
                  DEPENDS ctest)
         set(gmon ${gmon} prof-sum)

        # Add target profiling:
        add_custom_target(profiling DEPENDS ${gmon})
    endif()
endfunction()

######################################################################################
# Chapter tests
#    CreateUnitTests
#    CreateRegressionTests
#    SetCtestTarget
######################################################################################

function(CreateUnitTests)
cmake_parse_arguments(PARSE_ARGV 0 UNIT_TESTS "" "DIR;PREFIX" "LIBS")
#
# CreateUnitTests( DIR ${dir} PREFIX ${prefix} LIBS ${lib1} [${lib2} ...])
#
# Create unit tests using add_test
#
# DIR:    The following files are used to make unit tests:
#         ${UNIT_TESTS_DIR}/m_*.f90     fortran sources that contain a module, but no program
#         ${UNIT_TESTS_DIR}/t_*.f90     fortran sources that contain a (main) program
# PREFIX: The tests get the ${UNIT_TESTS_PREFIX}, so tests from different directories can be
#         distinguished in the output
# LIBS:   libraries to be linked into the test executables (library gcov is added
#         by this function when needed)
#
    start_section("${UNIT_TESTS_PREFIX}_unit_tests")
    if (NOT WIN32 AND CMAKE_BUILD_TYPE STREQUAL "COVERAGE"
        AND (CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM" OR
             CMAKE_Fortran_COMPILER_ID STREQUAL "GNU") )
        set(UNIT_TESTS_LIBS ${UNIT_TESTS_LIBS} gcov)
    endif()

    set(all_targets ${test_dependencies})
    set(UNIT_TESTS_LIBS nopfunit ${UNIT_TESTS_LIBS})

    # The files "m_*.f90" contain modules, and are linked into a test-library
    file(GLOB M_FILES ${UNIT_TESTS_DIR}/m_*.f90)
    if (M_FILES)
        add_library(ops_tst_${UNIT_TESTS_PREFIX} STATIC ${M_FILES})
        target_link_libraries(ops_tst_${UNIT_TESTS_PREFIX} ${UNIT_TESTS_LIBS})
        set(UNIT_TESTS_LIBS "ops_tst_${UNIT_TESTS_PREFIX}")
    endif()

    # Loop over files t*.f90 and add test:
    file(GLOB F_FILES ${UNIT_TESTS_DIR}/t*.f90)
    foreach( tst_source ${F_FILES})
        get_filename_component(tst_name ${tst_source} NAME_WLE)
        add_executable(${tst_name} ${tst_source})
        set_target_properties(${tst_name} PROPERTIES
            RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/tst/level_1/bin/"
        )
        target_link_libraries(${tst_name} ${UNIT_TESTS_LIBS})

        # Define arguments for testing command line arguments (ops_get_arg):
        if (${tst_name} STREQUAL tst_m_ops_get_arg)
           # Get argument list from the test routine (grep .*tst_arg)
           # FS the following does not work, because ${tst_args} is interpreted as one string
           # FS not as separate arguments
           # EXECUTE_PROCESS(
           #  COMMAND grep "set(tst_args" ${tst_source}
           #  OUTPUT_VARIABLE tst_args)
           #  string(REGEX REPLACE ".*tst_args" "" tst_args ${tst_args}) # skip " ! set(tst_args "
           #  string(REGEX REPLACE "\\)" "" tst_args ${tst_args})        # skip "}"
           #  message("Test arguments: " ${tst_args})
           set(tst_args  -i ./level_1/resources/tst_ctrl.in -nosub -domlu -varz -perc -mindist -maxdist -classoutput -varinfile tst.txt -v -nthreads 11 -allow_sigz0_point_source)
        else()
           set(tst_args  "" )
        endif()

        # Add test:
        # COMMAND ${tst_name} -i ./level_1/resources/tst_ctrl.in -varinfile tst.txt -varz
        add_test (NAME "${UNIT_TESTS_PREFIX}_${tst_name}"
                  COMMAND ${tst_name} ${tst_args}
                  WORKING_DIRECTORY ${UNIT_TESTS_DIR}/../)
        set(all_targets ${all_targets} ${tst_name})
    endforeach(tst_source)
    end_section("${UNIT_TESTS_PREFIX}_unit_tests")

    # Pass output variables to PARENT_SCOPE
    set(test_dependencies ${all_targets} PARENT_SCOPE)
endfunction()

######################################################################################
function (CreateRegressionTests)

   cmake_parse_arguments(PARSE_ARGV 0 REGRESSION_TEST "" "LEVEL;NTHREADS" "")

   #
   # CreateRegressionTests( LEVEL ${level})
   #
   # Prepare the input for the level 2 or level 3 tests by
   # + unpacking the tgz-file with meteo, land use maps, chemistry maps etc. ;
   # + running ops_inittests to copy ctr- or inv-files for all tests and set appropriate paths in these files;
   # + add tests using add_test to be run with ctest.
   #
   # Note: CMAKE_CURRENT_SOURCE_DIR = current top directory, e.g. path/to/ops_[lt|st]/develop (is tracked)
   # Note: CMAKE_CURRENT_BINARY_DIR = current build directory, e.g. path/to/ops_[lt|st]/develop/build_debug (is not tracked)

    # Start:
    start_section("CreateRegression_tests")

    # Default test level = 1 (unit tests), NTHREADS = 1:
    if ("${REGRESSION_TEST_LEVEL}" STREQUAL "")
        set(REGRESSION_TEST_LEVEL 1)
    endif()
    if ("${REGRESSION_TEST_NTHREADS}" STREQUAL "")
        set(REGRESSION_TEST_NTHREADS 1)
    endif()

    message("TEST_LEVEL = ${REGRESSION_TEST_LEVEL}")
    message("NTHREADS for OPS = ${REGRESSION_TEST_NTHREADS}")

    # Make tst directory under build-directory:
    execute_process(
        COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_CURRENT_BINARY_DIR}/tst"
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )

    # Unpack data (meteo, land use maps, chemical maps, emissions, ...) in .tgz-files for testsuites.
    # Note: the top level directory in the file qqq.tgz must be qqq, so that data in file qqq.tgz is unpacked to directory qqq.
    # Note: NAME_WLE = name without last extension
    # Loop over .tgz-files and unpack, if not already unpacked:
    file(GLOB FILES_TGZ "${CMAKE_CURRENT_SOURCE_DIR}/data/*.tgz")
    foreach(file_tgz ${FILES_TGZ})
       get_filename_component (name_wle ${file_tgz} NAME_WLE)
       if (NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/data/${name_wle})
          message("unpacking file ${file_tgz} ...")
          execute_process(
            COMMAND ${CMAKE_COMMAND} -E tar xfz "${file_tgz}"
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/data)
       endif()
    endforeach(file_tgz ${TST_DIRS})

    # ops_inittests copies input files for several tests from CMAKE_SOURCE_DIR/tst/level_x to CMAKE_BINARY_DIR/tst/level_x;
    # this is needed because we don't want the output in the repository but under CMAKE_BINARY_DIR, which is not tracked.
    message("Initialise tests")
    if (REGRESSION_TEST_LEVEL GREATER_EQUAL 2)

       # Define data directory for emissions: FS must be set in data structure of ./data directory?
       if (REGRESSION_TEST_LEVEL EQUAL 3)
          # Git-lfs directory (level-3 tests have large emission files):
          set(ops_data_emis "${CMAKE_CURRENT_SOURCE_DIR}/data/emis_diag_2021_GDN")
       else()
          # If empty, the emission file is found under the specific test at ${CMAKE_CURRENT_SOURCE_DIR}/tst:
          set(ops_data_emis "")
       endif()

       # Initialise tests using ops_inittests.

       # Check whether to pass option --st to ops_inittests and define data directory for environmental data:
       if (${PROJECT_NAME} STREQUAL "ops_st")
          set(option "--st")
          set(ops_data_env "${CMAKE_CURRENT_SOURCE_DIR}/data")
       else()
          set(option "")
          set(ops_data_env "${CMAKE_CURRENT_SOURCE_DIR}/data/GCN2022")
       endif()
       execute_process(
           # Note: level-3 tests not yet for OPS-ST
           # Note: OPS_LT_TESTSUITE_PATH is used in ops_inittests as directory where to copy input files from
           #       OPS_DATA_ENV is used to set DATADIR and directory to find other data files into the ctr-files for running OPS
           #       OPS_DATA_EMIS is used to set the directory for the emission files
           #       --replace|-r : Overwrite files if they exist (default: exit if they exist).
           #       --level|-l   : Level of test suite to use (default 2)
           #       --output|-o  : Path to toplevel output folder under which the testsuite will be initialized
           #       ops_inittests is run in WORKING_DIRECTORY
           COMMAND ${CMAKE_COMMAND} -E env
               OPS_LT_TESTSUITE_PATH=${CMAKE_CURRENT_SOURCE_DIR}/tst
               OPS_ST_TESTSUITE_PATH=${CMAKE_CURRENT_SOURCE_DIR}/tst
               OPS_DATA_ENV=${ops_data_env}
               OPS_DATA_EMIS=${ops_data_emis}
               bash ops_inittests ${option} -r -l ${REGRESSION_TEST_LEVEL} -o ${CMAKE_CURRENT_BINARY_DIR}/tst/level_${REGRESSION_TEST_LEVEL}
           ECHO_OUTPUT_VARIABLE
           ECHO_ERROR_VARIABLE
           WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/ops_script/ops_test
       )
    endif()

    # If test level > 2
    if (REGRESSION_TEST_LEVEL GREATER_EQUAL 2)

        if (${REGRESSION_TEST_NTHREADS} GREATER 1)
           message("OPS argument for all tests: -nthreads ${REGRESSION_TEST_NTHREADS}")
        endif()

        # Prepare for Python:
        set(Python_FIND_VIRTUALENV FIRST)
        find_package (Python COMPONENTS Interpreter REQUIRED )

        # Loop over all regression tests under directories A_*, B_*, ..., Z_*:
        file(GLOB TST_DIRS "${CMAKE_CURRENT_BINARY_DIR}/tst/level_${REGRESSION_TEST_LEVEL}/[A-Z]_*/[A-Z][0-9]*")
        foreach(test_dir ${TST_DIRS})
            get_filename_component (tst_name ${test_dir} NAME)
            get_filename_component (parent ${test_dir} DIRECTORY)
            get_filename_component (parent ${parent} NAME)
            #message("+++++++++++++++++++++")
            #message("${test_dir}")
            #message("${tst_name}")

            # Define location of reference data FS previous version for level_3, not yet measurements
            set(ref_dir ${CMAKE_CURRENT_SOURCE_DIR}/tst/level_${REGRESSION_TEST_LEVEL}/reference/${parent}/${tst_name}/)

            # Get OPS arguments from file OPS_ARGS in input directory of test:
            if (EXISTS ${test_dir}/input/OPS_ARGS)
               execute_process(COMMAND cat ${test_dir}/input/OPS_ARGS OUTPUT_VARIABLE ops_args)
               message("extra OPS arguments for test ${tst_name}: ${ops_args}")
            else()
               set(ops_args "")
            endif()

            # Add option for nthreads:
            if (${REGRESSION_TEST_NTHREADS} GREATER 1)
               string(PREPEND ops_args "-nthreads ${REGRESSION_TEST_NTHREADS} ") # Note: appending does not work (extra line feed)
            endif()

            # Set option for plotting: default plot-on-error;
            set(opt_plot "--plot-on-error")

            # level_3 tests -> --plot-always
            if (REGRESSION_TEST_LEVEL EQUAL 3)
               set(opt_plot "--plot-always")
            endif()

            #message("... reference data from ${ref_dir}")

            # Add test that runs a test and compares computed data with reference data in
            # ref_dir using Python function run_a_test.py:
            #
            # run_a_test.py arguments:
            # -h, --help            show this help message and exit
            # -x EXE, --exe EXE     name of the executable (default: )
            # -r REF_DIR, --ref-dir REF_DIR
            #           reference directory: contents must resemble those of
            #           test-dir (default: None)
            # -t TEST_DIR, --test-dir TEST_DIR
            #           test directory: contents must resemble those of ref-
            #           dir (default: None)
            # -v, --verbose         more standard output (default: False)
            # -pa, --plot-always    ALWAYS produce graphical output (default: False)
            # -pe, --plot-on-error  graphical output when differences are detected
            #           (default: False)
            # --abstol ABSTOL       tolerance for absolute errors (default: 1e-15)
            # --reltol RELTOL       tolerance for relative errors (default: 0.0001)
            # -o OPS_ARGS, --ops-args OPS_ARGS
            #           arguments for OPS. Important note: because OPS-options
            #           start with a dash (e.g. -varz) and python strips
            #           initial dashes, and replaces other dashes to
            #           underscores, you have to specify this option using =,
            #           e.g. -o='-varz -nthreads 10' (default: )
            # --numdiff full path name of numdiff executable (needed for OPS-ST)

            # WORKING_DIRECTORY = ${test_dir}/run , e.g. CMAKE_BINARY_DIR/tst/level_2/A_test_src1_rcp1_year1/A1_SO2_point_hlow/run
            # ${tst_name} = A1_SO2_point_hlow
            # -t ../../${tst_name} -> test directory CMAKE_SOURCE_DIR/tst/level_2/A_test_src1_rcp1_year1/A1_SO2_point_hlow/
            # -t ..                -> test directory CMAKE_SOURCE_DIR/tst/level_2/A_test_src1_rcp1_year1/A1_SO2_point_hlow/
            # but -t .. gives many more errors FS ????

            # numdiff only for OPS-ST (FS temporary solution because test_plt_flies/read_plt_st is not yet operational)
            if (${PROJECT_NAME} STREQUAL "ops_st")
               add_test (NAME "${tst_name}"
                   COMMAND
                        ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/ops_script/ops_test/run_a_test.py
                                -t ../../${tst_name}
                                -r ${ref_dir}
                                -o=${ops_args} ${opt_plot}
                                -x ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${main}
                                --numdiff ${CMAKE_CURRENT_SOURCE_DIR}/numdiff/bin/numdiff
                   WORKING_DIRECTORY ${test_dir}/run)
            else()
               add_test (NAME "${tst_name}"
                   COMMAND
                        ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/ops_script/ops_test/run_a_test.py
                                -t ../../${tst_name}
                                -r ${ref_dir}
                                -o=${ops_args} ${opt_plot}
                                -x ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${main}
                   WORKING_DIRECTORY ${test_dir}/run)
            endif()
        endforeach(test_dir)
    endif()
    end_section("CreateRegression_tests")

endfunction ()

######################################################################################
function(SetCtestTarget)
#
# SetCtestTarget()
#
# Add rules for target ctest.
#
# When building this target, using
#      cmake . --build --target ctest
# first all the targets will be compiled, and then the tests will be run in parallel.
# The result will be stored in the file ctest.xml
#
    start_section("SetCtestTarget")
    if(WIN32)
        set(CONFIGOPTION -C $<CONFIG>)
    endif()
    add_custom_command( OUTPUT ctest.xml
                        COMMAND ctest --output-on-failure  -j 10 --output-junit ctest.xml ${CONFIGOPTION}
                        DEPENDS ${test_dependencies})
    add_custom_target(ctest DEPENDS ctest.xml)
    end_section("SetCtestTarget")
endfunction()



######################################################################################
# Chapter utility functions
#    start_section
#    end_section
######################################################################################
function(start_section section)
#
# start_section(${section})
#
# print a message that will be interpreted as the start of a 'collapsible section'
# in GitLab's log-screen.
#
    string(TIMESTAMP seconds "%s")
    string(ASCII 27 escape)
    message("section_start:${seconds}:${section}\r${escape}[0K${section}")
endfunction()


######################################################################################
function(end_section section)
#
# end_section(${section})
#
# print a message that will be interpreted as the end of a 'collapsible section'
# in GitLab's log-screen.
#
    string(TIMESTAMP seconds "%s")
    string(ASCII 27 escape)
    message("section_end:${seconds}:${section}\r${escape}[0K")
endfunction()

