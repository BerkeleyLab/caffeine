program error_stop_integer_code
  use iso_c_binding, only: c_bool
  use prif, only : prif_init, prif_error_stop, prif_stop
  use unit_test_parameters_m, only : unexpected_error_stop, expected_error_stop, unexpected_stop
  implicit none

  integer :: init_exit_code

  call prif_init(init_exit_code)
  if (init_exit_code /= 0) call prif_error_stop(.false._c_bool, unexpected_error_stop)
    ! prif_error_stop_test.f90 should report a test failure if the above line invoikes prif_error_stop

  call prif_error_stop(.false._c_bool, expected_error_stop)
    ! prif_error_stop_test.f90 should report a passing test if the above prif_error_stop call succeeds 

  call prif_stop(.false._c_bool, unexpected_stop)
    ! prif_error_stop_test.f90 should report a passing test if the above prif_stop call executes
end program
