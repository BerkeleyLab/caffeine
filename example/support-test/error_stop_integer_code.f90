program error_stop_integer_code
  use caffeine_m, only : caffeinate, decaffeinate, caf_error_stop 
  implicit none

  if (caffeinate() /= 0) error stop "caffeinate returned a non-zero exit_code"

  call caf_error_stop(1) 

  stop 0 ! caffeine/test/caf_error_stop_test.f90 reports a failure if this line runs
end program 
