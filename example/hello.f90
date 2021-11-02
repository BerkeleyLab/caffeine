program hello_world
  use caffeine_m, only : caffeinate
  implicit none

  if (caffeinate() /= 0) error stop "caffeinate returned a non-zero exit code"

end program
