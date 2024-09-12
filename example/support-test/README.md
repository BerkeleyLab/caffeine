Test Support
------------
The programs in this directory intentionally terminate to support the `stop` and `error stop` 
unit tests, which use Fortran's `execute_command_line` to run the programs in this directory 
and to check for the expected non-zero stop codes.  Running the tests in this manner enables 
the tests to continue executing after the child process launched by `execute_command_line` 
terminates.
