submodule(collective_subroutines_m)  co_broadcast_s
  use iso_c_binding, only : c_int, c_size_t, c_ptr, c_loc, c_sizeof, c_int64_t, c_double
  use utilities_m, only : get_c_ptr
  implicit none

contains

  module procedure caf_co_broadcast

    interface

      subroutine c_co_broadcast(a, source_image, stat, c_sizeof_a) bind(C)
        import c_int, c_ptr, c_size_t
        implicit none
        type(*) a(..)
        type(c_ptr), value :: stat
        integer(c_int), value :: source_image
        integer(c_size_t), value :: c_sizeof_a
      end subroutine

    end interface

    type(c_ptr) stat_ptr

    stat_ptr = get_c_ptr(stat)

    call c_co_broadcast(a, source_image, stat_ptr, int(product(shape(a)),c_size_t))  
      ! With a compliant Fortran 2018 compiler, pass in c_sizeof(a) as the final argument 
      ! and eliminate the calculation of num_elements*sizeof(a) in caffeine.c.

  end procedure

end submodule co_broadcast_s
