cmake_minimum_required (VERSION 3.18)
enable_testing()
project (ops_lib Fortran)

# Append subdirectory cmake to module path:
list(APPEND CMAKE_MODULE_PATH  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/")

# Configure cmake process:
include(OpsConfigure)
OpsConfigure()
