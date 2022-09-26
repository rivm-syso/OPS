# Compiling OPS

This is a short manual on how to compile the OPS model on a Linux machine.

## Dependencies

The following dependencies are required:

* Intel `ifort` Fortran compiler (RIVM uses version 2021.3.0 downloaded from [Intel oneAPI HPC](https://www.intel.com/content/www/us/en/developer/tools/oneapi/hpc-toolkit-download.html))
* CMake (RIVM uses version 3.18.2 downloaded from [cmake](https://cmake.org/download/))
* math77 library (RIVM uses Release 6 downloaded from [netlib](https://netlib.org/math/))
* pFUnit (RIVM uses [commit 594a6e2](https://github.com/Goddard-Fortran-Ecosystem/pFUnit/commit/594a6e2d26df70d45ead4392da65cdec69a0b869))

First, download and install/compile the above dependencies before proceeding.

## Compiling

After the dependencies are installed, OPS can be compiled using CMake. The OPS model is split into two parts (a library and an executable) that should be compiled separately.

### Specifying environment variables

The following environment variables need to be specified:

The build type. Choose from `Debug` or `Release`. The `Debug` version turns off optimizations and turns on tracebacks, warnings and debugging information.
Use the `Debug` version during development. The `Release` version results in an optimized executable, which should typically be used.
The specific compiler flags can be found in the `./ops_lib/CMakeLists.txt` and `./ops_lt/CMakeLists.txt` files. 

```bash
export CMAKE_BUILD_TYPE="Release" 
```

The location of the Math77 and pFUnit libraries. Example paths should be changed to the appropriate location on your system.

```bash
export MATHLIB_DIR="/home/user/MATH77/"
export PFUNIT_DIR="/home/user/pfunit/src/build/"
```

### Compiling the OPS library

First, change to the `ops_lib` folder and set its path as our `CMAKE_TOPDIR`:

```bash
cd ops_lib
export CMAKE_TOPDIR=`realpath .`
```

Create a `build` directory for cmake and change into it:
```bash
mkdir build
cd build
```

Next, we can compile the OPS library using CMake. First, we need to generate the necessary CMake files as follows:

```bash
cmake -DCMAKE_BUILD_TYPE="${CMAKE_BUILD_TYPE}" \
      -DBIN_DIR="${CMAKE_TOPDIR}/bin" -DSRC_DIR="${CMAKE_TOPDIR}/src" \
      -DMOD_DIR="${CMAKE_TOPDIR}/build/mod" \
      -DTST_DIR="${CMAKE_TOPDIR}/tst" \
      -DMOD_TST_DIR="${CMAKE_TOPDIR}/build/mod_tst" \
      -DCOV_DIR="${CMAKE_TOPDIR}/build/codecov" \
      -DMATHLIB_DIR="${MATHLIB_DIR}" \
      -DPFUNIT_DIR="${PFUNIT_DIR}" "${CMAKE_TOPDIR}"
```

Then, we can build the library:

```bash
cmake --build .
```

In the final step, the library is installed into the `../bin` folder:

```bash
cmake --install .
```

Now, it is ready to be used by the main OPS executable. First, store the path of the `bin` folder:

```bash
export OPSLIB_DIR="${CMAKE_TOPDIR}/bin/"
```

Optionally, the unit tests can be run with:

```bash
cd ..
./bin/tst.exe
```

### Compiling the OPS executable

Then, change to the `ops_lt` folder and set its path as our `CMAKE_TOPDIR`:

```bash
cd "${CMAKE_TOPDIR}/../ops_lt"
export CMAKE_TOPDIR=`realpath .`
```

Create a `build` directory for cmake and change into it:
```bash
mkdir build
cd build
```

Next, we can compile the final OPS executable using CMake. First, we need to generate the necessary CMake files as follows:

```bash
cmake -DCMAKE_BUILD_TYPE="${CMAKE_BUILD_TYPE}" \
      -DBIN_DIR="${CMAKE_TOPDIR}/bin" -DSRC_DIR="${CMAKE_TOPDIR}/src" \
      -DMOD_DIR="${CMAKE_TOPDIR}/build/mod" \
      -DTST_DIR="${CMAKE_TOPDIR}/tst" \
      -DMOD_TST_DIR="${CMAKE_TOPDIR}/build/mod_tst" \
      -DCOV_DIR="${CMAKE_TOPDIR}/build/codecov" \
      -DOPSLIB_DIR="${OPSLIB_DIR}" -DMATHLIB_DIR="${MATHLIB_DIR}" \
      -DPFUNIT_DIR="${PFUNIT_DIR}" "${CMAKE_TOPDIR}"
```

Then, we can build the executable:

```bash
cmake --build .
```

In the final step, the executable is installed into the `../bin` folder:

```bash
cmake --install .
```

Optionally, the unit tests can be run with:

```bash
cd ..
./bin/tst.exe
```