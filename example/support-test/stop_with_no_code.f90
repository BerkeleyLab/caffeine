program stop_with_no_code
  use caffeine_m, only : caf_caffeinate, caf_stop
  implicit none

  if (caf_caffeinate() /= 0) error stop "caffeinate returned a non-zero exit_code"

  call caf_stop

  stop 1 ! caffeine/test/zzz_finalization_test.f90 reports a failure if this line runs
end program 
