program error_stop_with_integer_code
  use iso_c_binding, only: c_bool
  use prif, only : &
     prif_init &
    ,prif_stop &
    ,prif_error_stop
  use unit_test_parameters_m, only : expected_error_stop_code 
  implicit none

  integer init_exit_code
  logical(kind=c_bool), parameter :: true = .true._c_bool

  call prif_init(init_exit_code)
  call prif_error_stop(quiet=true, stop_code_int=expected_error_stop_code) ! a prif_error_stop unit test passes if this line executes error termination
  call prif_stop(quiet=true) ! a prif_error_stop unit tests fails if this line runs
end program
