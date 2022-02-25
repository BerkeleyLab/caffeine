program sum_double_complex_scalar
  use caffeine_m, only : caf_co_sum, caf_num_images, caf_caffeinate, caf_decaffeinate
  implicit none

  integer, parameter :: dp = kind(1.D0)
  complex(dp), parameter :: input = (1.D0,1.0D0)
  complex(dp) scalar

  if (caf_caffeinate()/=0) error stop "caf_caffeinate() returned a non-zero exit code"

  scalar = input
  call caf_co_sum(scalar)
  print *,"scalar", scalar, " should be ", input*caf_num_images()

  call caf_decaffeinate(0)

end program
