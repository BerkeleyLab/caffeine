program error_stop_character_code
  use prif, only : prif_init, prif_error_stop
  implicit none

  integer :: init_exit_code

  call prif_init(init_exit_code)
  if (init_exit_code /= 0) error stop "caffeinate returned a non-zero exit_code"

  call prif_error_stop(stop_code_char="Oh snap!")

  stop 0 ! ../../test/caf_error_stop_test.f90 will report a test failure if this line runs
end program 
