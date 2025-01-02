Test Termination
----------------
The code in this subdirectory intentionally terminate to tests the following
procedures and interface from  prif.F90:
  - `prif_error_stop`
  - `prif_register_stop_callback`
  - `prif_stop_callback_interface`
  - `prif_stop`

Usage
-----
To build or rebuild and run the tests in this subdirectory, execute the following
commands:
```
fpm clean --all
cd ..
./install.sh
cd - 
mkdir build
cp ../build/run-fpm.sh build
../build/run-fpm.sh test
```
which should yield trailing output similar to the following:
```
 ERROR STOP 'USER_PROVIDED_STRING'
 callback invoked
 STOP
 STOP 'USER_PROVIDED_STRING'
 STOP           99
 STOP
<ERROR> Execution for object " error_stop_with_character_code " returned exit code  1
<ERROR> Execution for object " error_stop_with_integer_code " returned exit code  100
<ERROR> Execution for object " error_stop_with_no_code " returned exit code  1
<ERROR> Execution for object " stop_with_integer_code " returned exit code  99
<ERROR> *cmd_run*:stopping due to failed executions
STOP 1
```
The environment variables that might be relevant to reproducing the above behavior
include `FPM_FC`, `FPM_CC`, `FPM_FFLAGS`, `LC_RPATH`, and either `DYLD_LIBRARY_PATH`
on macOS or `LD_LIBRARY_PATH` on Linux.  On the macOS system tested, `FPM_FC`
and `FPM_CC` point to `flang-new` and `clang` (version information below), whereas
`LC_RPATH` and `DYLD_LIBRARY_PATH` are both set to the same path as that of `flang-new`
but with the trailing `bin` replaced by `lib`.

```
% flang-new --version
flang-new version 20.0.0git (git@github.com:ROCm/llvm-project 27e3c3a2c5716678cef303ba211ccea46a421b00)
Target: arm64-apple-darwin23.6.0
Thread model: posix
InstalledDir: /Users/rouson/Repositories/llvm-project/install/rocm/bin
Build config: +assertions
```

