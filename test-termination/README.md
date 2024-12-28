Test Termination
----------------
The code in this subdirectory intentionally terminate to test the following
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
