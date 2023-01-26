# CMake project file for OPS-LT
cmake_minimum_required (VERSION 3.18)
project (ops_lt Fortran)
enable_language (Fortran)

# Add our local modules (.cmake files) to the module path
# Note: CMAKE_SOURCE_DIR is the top-level directory (where .git is located)
#       CMAKE_MODULE_PATH specifies a search path for CMake modules to be loaded by include() or find_package()
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/")

# Directories should be specified as command line arguments (e.g. -D BLD_DIR=./build)
# The following should be set:
# BLD_DIR, BIN_DIR, SRC_DIR, MOD_DIR, TST_DIR, MOD_TST_DIR, COV_DIR
# NETCDF_DIR, OPSLIB_DIR, MATHLIB_DIR, PFUNIT_DIR.

# make sure that the default build type is DEBUG
set ( CMAKE_CONFIGURATION_TYPES "Debug" "Release" "Coverage" "Profiler" )
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE DEBUG CACHE STRING
      "Choose the type of build, options are: Debug, Release, Coverage, Profiler."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)
message (STATUS "CMAKE_BUILD_TYPE = ${CMAKE_BUILD_TYPE}")

# append build type to output files
set(CMAKE_DEBUG_POSTFIX "_debug")
set(CMAKE_RELEASE_POSTFIX "_optim")
set(CMAKE_COVERAGE_POSTFIX "_cov")
set(CMAKE_PROFILER_POSTFIX "_prof")

# Get name of Fortran compiler (without directory):
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

# Compile flags depend on the compiler:
if (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # ifort (default/preferred compiler)
  set (CMAKE_Fortran_FLAGS_RELEASE "-nowarn -DUNIX -fpp -assume byterecl -O2 -extend-source -module ${MOD_DIR}")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fpe0 -warn unused -diag-disable8291 -diag-disable7841 -DUNIX -fpp -assume byterecl -check -warn interfaces -debug-parameters all -traceback -O0 -g -extend-source -module ${MOD_DIR}") 
  set (CMAKE_Fortran_FLAGS_PROFILER   "-pg ${CMAKE_Fortran_FLAGS_RELEASE}") 
  set (CMAKE_Fortran_FLAGS_COVERAGE   "${CMAKE_Fortran_FLAGS_DEBUG} -prof-gen=srcpos -prof-dir=${COV_DIR}") 
else ()
  message (STATUS "CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message (STATUS "Fortran compiler: " ${Fortran_COMPILER_NAME})
  message (STATUS "No optimized Fortran compiler flags are known for compilers other than ifort, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
  set (CMAKE_Fortran_FLAGS_PROFILER   "-pg ${CMAKE_Fortran_FLAGS_RELEASE}") 
  set (CMAKE_Fortran_FLAGS_COVERAGE   "${CMAKE_Fortran_FLAGS_DEBUG} -prof-gen=srcpos -prof-dir=${COV_DIR}") 
endif (Fortran_COMPILER_NAME MATCHES "ifort.*")

# Rerun CMake if the library paths change
set_property (DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS "${CMAKE_SOURCE_DIR}/library_paths_config.txt")

# Double-check required library paths so we can give a clear error message
if ("${OPSLIB_DIR}" STREQUAL "")
    message(FATAL_ERROR "OPSLIB_DIR should be set in ${CMAKE_SOURCE_DIR}/library_paths_config.txt")
endif()
if ("${MATHLIB_DIR}" STREQUAL "")
    message(FATAL_ERROR "MATHLIB_DIR should be set in ${CMAKE_SOURCE_DIR}/library_paths_config.txt")
endif()
if ("${PFUNIT_DIR}" STREQUAL "")
    message(FATAL_ERROR "PFUNIT_DIR should be set in ${CMAKE_SOURCE_DIR}/library_paths_config.txt")
endif()

# Find ops lib and math77
find_package (OPSLIB REQUIRED)
find_package (MATH77 REQUIRED)

# OPS lt source files in src, put output files in build/mod directory; then process CMakeLists.txt in src:
add_subdirectory(${SRC_DIR} ${MOD_DIR})

# OPS lt tst files (.pf) in tst, put output files in build/mod_tst directory; then process CMakeLists.txt in tst:
add_subdirectory(${TST_DIR} ${MOD_TST_DIR})

