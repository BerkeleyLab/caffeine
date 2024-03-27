! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) co_sum_s
  use utilities_m, only : optional_value
  use caffeine_h_m, only : caf_co_sum

  implicit none

contains

  module procedure prif_co_sum
    if (present(stat)) stat=0

    call caf_co_sum( &
        a, optional_value(result_image), int(product(shape(a)), c_size_t), current_team%gex_team)
  end procedure

end submodule co_sum_s
