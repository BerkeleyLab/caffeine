submodule(collective_subroutines_m)  co_broadcast_s
  use iso_c_binding, only : c_int, c_size_t, c_ptr, c_loc, c_sizeof, c_int64_t, c_double
  use utilities_m, only : get_c_ptr
  use caffeine_h_m, only : c_co_broadcast
  implicit none

contains

  module procedure caf_co_broadcast
    type(c_ptr) stat_ptr

    stat_ptr = get_c_ptr(stat)

    call c_co_broadcast(a, source_image, stat_ptr, product(shape(a)))  
      ! With a compliant Fortran 2018 compiler, pass in c_sizeof(a) as the final argument 
      ! and eliminate the calculation of num_elements*sizeof(a) in caffeine.c.
  end procedure

end submodule co_broadcast_s
