submodule(prif:prif_private_s)  co_broadcast_s
  use utilities_m, only : get_c_ptr
  use caffeine_h_m, only : caf_co_broadcast

  implicit none

contains

  module procedure prif_co_broadcast
    type(c_ptr) stat_ptr

    stat_ptr = get_c_ptr(stat)

    call caf_co_broadcast(a, source_image, stat_ptr, product(shape(a)), current_team%gex_team)
      ! With a compliant Fortran 2018 compiler, pass in c_sizeof(a) as the final argument
      ! and eliminate the calculation of num_elements*sizeof(a) in caffeine.c.
  end procedure

end submodule co_broadcast_s
