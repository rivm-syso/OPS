# Find pFUnit installation
find_package(PFUNIT REQUIRED PATHS "${PFUNIT_DIR}")
enable_testing()

# find pFUnit files
file(GLOB TST_SRC_FILES RELATIVE ${TST_DIR} *.pf)

# Release options do not work with pFUnit:
set (CMAKE_Fortran_FLAGS_RELEASE ${CMAKE_Fortran_FLAGS_DEBUG})
message (STATUS "CMAKE_Fortran_FLAGS_RELEASE (adapted for pFUnit) = ${CMAKE_Fortran_FLAGS_RELEASE}")

# Add pFUnit tests:
add_pfunit_ctest (tst.exe
  TEST_SOURCES ${TST_SRC_FILES}
  LINK_LIBRARIES ${MATH77_LIBRARIES} ${OPSLIB_LIBRARIES} opslt
  )
target_include_directories(tst.exe PUBLIC ${MATH77_INCLUDE_DIRS} PUBLIC ${OPSLIB_INCLUDE_DIRS} PUBLIC ${MOD_DIR})

# After compilation, move module files to ops_lib/bin
install(TARGETS tst.exe DESTINATION ${BIN_DIR})
