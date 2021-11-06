program error_stop_character_code
  use caffeine_m, only : caffeinate, decaffeinate, caf_error_stop
  implicit none

  if (caffeinate() /= 0) error stop "caffeinate returned a non-zero exit_code"

  call caf_error_stop("")

  stop 0 ! ../../test/caf_error_stop_test.f90 will report a test failure if this line runs
end program 
