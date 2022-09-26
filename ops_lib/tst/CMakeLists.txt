# Find pFUnit installation
find_package(PFUNIT REQUIRED PATHS "${PFUNIT_DIR}")
enable_testing()

# find pFUnit files and assign it to TST_SRC_FILES
file(GLOB TST_SRC_FILES RELATIVE ${TST_DIR} *.pf)

# add the unit tests and define target tst.exe:

# Release options do not work with pFUnit:
set (CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_DEBUG})
message (STATUS "CMAKE_Fortran_FLAGS_RELEASE (adapted for pFUnit) = ${CMAKE_Fortran_FLAGS_RELEASE}")

# Add pFUnit tests:
add_pfunit_ctest (tst.exe
  TEST_SOURCES ${TST_SRC_FILES}
  LINK_LIBRARIES ops ${MATH77_LIBRARIES}
  )

# Add include directories (to use when compiling target) to target tst.exe:
target_include_directories(tst.exe PUBLIC ${MATH77_INCLUDE_DIRS} PUBLIC ${MOD_DIR})

# After compilation, move tst.exe to ops_lib/bin
install(TARGETS tst.exe DESTINATION ${BIN_DIR})

