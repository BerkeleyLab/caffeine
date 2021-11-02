program hello_world
  use caffeine_m, only : caffeinate, decaffeinate
  implicit none

  if (caffeinate() /= 0) error stop "caffeinate returned a non-zero exit code"

  call decaffeinate(exit_code=0)

  stop 0 ! not reached in most implementations
end program
