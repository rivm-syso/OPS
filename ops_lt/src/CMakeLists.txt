# build library
# source files relative to current directory
file(GLOB SRC_FILES RELATIVE ${CMAKE_CURRENT_LIST_DIR} "*.f90")

# but remove the main file, that is only used for the executable and excluded from the tests
list(REMOVE_ITEM SRC_FILES "ops_main.f90")

# object library https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#object-libraries
# needed for the unit tests
add_library(opslt OBJECT ${SRC_FILES})
target_link_libraries(opslt ${MATH77_LIBRARIES} ${OPSLIB_LIBRARIES})
target_include_directories(opslt PUBLIC ${MATH77_INCLUDE_DIRS} PUBLIC ${OPSLIB_INCLUDE_DIRS})

# Executable, links the object library above
add_executable(ops_lt ops_main.f90)
target_link_libraries(ops_lt ${MATH77_LIBRARIES} ${OPSLIB_LIBRARIES} opslt)
target_include_directories(ops_lt PUBLIC ${MATH77_INCLUDE_DIRS} PUBLIC ${OPSLIB_INCLUDE_DIRS})

# Set suffix based on build type
# check build type, first convert to lower case
string(TOLOWER "${CMAKE_BUILD_TYPE}" build_type_lower)
IF(build_type_lower STREQUAL "release")
    set_target_properties(ops_lt PROPERTIES SUFFIX "_optim.exe")
ELSEIF(build_type_lower STREQUAL "profiler")
    set_target_properties(ops_lt PROPERTIES SUFFIX "_prof.exe")
ELSE()
    set_target_properties(ops_lt PROPERTIES SUFFIX "_debug.exe")
ENDIF(build_type_lower STREQUAL "release")


# After compilation, move module files to bin folder
install(TARGETS ops_lt DESTINATION ${BIN_DIR})
