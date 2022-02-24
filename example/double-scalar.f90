program double
  use caffeine_m, only : caf_co_sum, caf_num_images, caf_caffeinate, caf_decaffeinate
  use assert_m, only : assert

  implicit none

  integer, parameter :: dp = kind(1.D0)
  complex(dp), allocatable :: scalar
  complex(dp), parameter :: input = (1.D0,1.0D0)

  call assert(caf_caffeinate()==0,"caf_caffeinate()==0")

  scalar = input
  call caf_co_sum(scalar)
  print *,"scalar", scalar, " should be ", input*caf_num_images()

  call caf_decaffeinate(0)

end program
