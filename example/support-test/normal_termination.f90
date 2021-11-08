program normal_termination
  use caffeine_m, only : caffeinate, decaffeinate
  implicit none

  if (caffeinate() /= 0) error stop "caffeinate returned a non-zero exit_code"

  call decaffeinate(0)

  stop 1 ! caffeine/test/zzz_finalization_test.f90 reports a failure if this line runs
end program 
