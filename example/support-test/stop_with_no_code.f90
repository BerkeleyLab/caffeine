program stop_with_no_code
  use prif, only : prif_caffeinate, prif_stop
  implicit none

  if (prif_caffeinate() /= 0) error stop "caffeinate returned a non-zero exit_code"

  call prif_stop

  stop 1 ! caffeine/test/zzz_finalization_test.f90 reports a failure if this line runs
end program 
