program error_stop_integer_code
  use iso_c_binding, only: c_bool
  use prif, only : prif_init, prif_error_stop
  implicit none

  integer :: init_exit_code

  call prif_init(init_exit_code)
  if (init_exit_code /= 0) error stop "caffeinate returned a non-zero exit_code"

  call prif_error_stop(logical(.false., c_bool), 1)

  stop 0 ! caffeine/test/caf_error_stop_test.f90 reports a failure if this line runs
end program
