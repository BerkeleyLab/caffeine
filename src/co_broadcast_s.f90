submodule(prif:prif_private_s)  co_broadcast_s

  implicit none

contains

  module procedure prif_co_broadcast
    if (present(stat)) stat=0
    call caf_co_broadcast(a, source_image, product(shape(a)), current_team%gex_team)
      ! With a compliant Fortran 2018 compiler, pass in c_sizeof(a) as the `Nelem` argument
      ! and eliminate the calculation of num_elements*sizeof(a) in caffeine.c.
  end procedure

end submodule co_broadcast_s
