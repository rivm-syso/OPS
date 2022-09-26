# - Find MATH77 lib. Note that MATH77 has not been built with CMake.
include(FindPackageHandleStandardArgs)

# Find path for MATH77 library and if lib exists:
find_path(MATH77_INCLUDE_DIRS NAMES "libmath77_optim.a" PATHS "${MATHLIB_DIR}")
find_library(MATH77_LIBRARIES math77_optim PATHS "${MATHLIB_DIR}")

# Set MATH77_FOUND; DEFAULT_MSG produces a standard error message if not found;
# report succes (first time only) if found :
find_package_handle_standard_args(MATH77
 "MATH77 could not be found: ${MATHLIB_DIR}/libmath77_optim.a doesn't exist"
 MATH77_INCLUDE_DIRS
 MATH77_LIBRARIES
)

# Set advanced state (for cmake-gui, so not necessary)
mark_as_advanced(MATH77_LIBRARIES MATH77_INCLUDE_DIRS)
