# - Find OPS lib
include(FindPackageHandleStandardArgs)

# Find path for OPS lib if .a file exists:
# check build type, first convert to lower case
string(TOLOWER "${CMAKE_BUILD_TYPE}" build_type_lower)
IF(build_type_lower STREQUAL "release")
    # use the optimized version
    find_library(OPSLIB_LIBRARIES NAMES "libops_optim.a" PATHS "${OPSLIB_DIR}")
    set(OPSLIB_CANDIDATE "${OPSLIB_DIR}/libops_optim.a")
ELSE()
    # use the debug version
    find_library(OPSLIB_LIBRARIES NAMES "libops_debug.a" PATHS "${OPSLIB_DIR}")
    set(OPSLIB_CANDIDATE "${OPSLIB_DIR}/libops_debug.a")
ENDIF(build_type_lower STREQUAL "release")

# set the include directory to the folder that contains the .mod files
set(OPSLIB_INCLUDE_DIRS "${OPSLIB_DIR}/mod")

# Set OPSLIB_FOUND; DEFAULT_MSG produces a standard error message if not found;
# report succes (first time only) if found :
find_package_handle_standard_args(OPSLIB
"OPS Lib could not be found: ${OPSLIB_CANDIDATE} doesn't exist"
 OPSLIB_INCLUDE_DIRS
 OPSLIB_LIBRARIES
)

# Set advanced state (for cmake-gui, so not necessary)
mark_as_advanced(OPSLIB_LIBRARIES OPSLIB_INCLUDE_DIRS)

# show status
message(STATUS "Using ${OPSLIB_LIBRARIES}")
