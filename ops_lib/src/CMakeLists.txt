# build OPS library

# Make a list of .f90 source files:
file(GLOB SRC_FILES *.f90)

# Define ops library (static) 
add_library(ops STATIC ${SRC_FILES})

# Link MATH77 lib to ops lib:
target_link_libraries(ops PUBLIC ${MATH77_LIBRARIES})

# Set include directories to use when compiling target ops
target_include_directories(ops PUBLIC ${MATH77_INCLUDE_DIRS})

# After compilation, move .a (library archive) to ./bin
install(TARGETS ops DESTINATION ${BIN_DIR})

# After compilation, move .mod (module files) to ./bin/mod
# Trailing slash of MOD_DIR important, so that we can choose the name
# of the target directory
install(DIRECTORY "${MOD_DIR}/" DESTINATION "${BIN_DIR}/mod"
        FILES_MATCHING PATTERN "*.mod" # include files matching *.mod
        PATTERN "CMakeFiles" EXCLUDE) # exclude the CMakeFiles directory
