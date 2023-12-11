program error_stop_character_code
  use prif_m, only : prif_caffeinate, prif_error_stop
  implicit none

  if (prif_caffeinate() /= 0) error stop "caffeinate returned a non-zero exit_code"

  call prif_error_stop("Oh snap!")

  stop 0 ! ../../test/caf_error_stop_test.f90 will report a test failure if this line runs
end program 
