# Compiling OPS

This is a short manual on how to compile the OPS model on a Linux machine.

## Dependencies

The following dependencies are required:

* `ifort`, `gfortran` or `ifx` Fortran compiler
* CMake (RIVM uses version 3.18.2 downloaded from [cmake](https://cmake.org/download/))

First, download and install/compile the above dependencies before proceeding.

## Configure

After the dependencies are installed, OPS can be compiled using CMake. The commands below configure the compilation process. It tells `cmake` to create a `debug` version of the OPS executable, with the `ifort` compiler. During the compilation, level 1 unit tests are performed.

```bash
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=debug -DCMAKE_Fortran_COMPILER=ifort
```

Other possible options are:

+ `CMAKE_BUILD_TYPE=debug`: use compiler options for error checking and debugging
+ `CMAKE_BUILD_TYPE=release`: use compiler options optimised for speed
+ `CMAKE_BUILD_TYPE=profiling`: profile shows the timings of separate subroutines
+ `CMAKE_BUILD_TYPE=coverage`: coverage shows the percentage of code that is covered by the tests
+ `CMAKE_Fortran_COMPILER`: can be `ifort`, `gfortran`, `ifx`. At RIVM, we currently use ifort.
+ `DSKIP_OPENMP=true`: by default, OPS is built using the OpenMP library to enable multithreading. However, for this to work OpenMP has to be available on your system. If you want to build OPS without this dependency, you can run the configure step with this option.

Notes:
+ The configure step only has to be repeated when new source files have been added to the project, or when targets are needed for a different CMAKE_Fortran_COMPILER, CMAKE_BUILD_TYPE, or TEST_LEVEL.
+ cmake keeps the values of CMAKE_Fortran_COMPILER, CMAKE_BUILD_TYPE, etc in a cache-file; this means that you don't need to specify these values the next time you use cmake.
+ The build directory is not tracked by git, this means that it is only available on the local computer. All files in the build directory can be generated anew with the configure, build and test steps.
+ If you want to build different executables, you can make different build directories, e.g. build_debug, build_release.
+ When the `CMakeLists.txt` or the files in the `cmake`-directories are changed, the configuration will be automatically run when compiling.


## Building the OPS executable

Build the project: compile all OPS subroutines and test-routines and link them to an OPS executable and many test-executables for the unit-tests.

```bash
cmake --build . -j 10   # in directory build
```

Here, `-j 10` indicates that CMake will parallel build the project, running 10 jobs simultaneously.
