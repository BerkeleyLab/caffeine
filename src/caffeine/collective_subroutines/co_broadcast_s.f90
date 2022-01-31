submodule(collective_subroutines_m)  co_broadcast_s
  use iso_c_binding, only : c_int, c_size_t, c_ptr, c_loc, c_sizeof
  use utilities_m, only : get_c_ptr
  implicit none

contains

  module procedure caf_co_broadcast

    interface

      subroutine c_co_broadcast(c_loc_a, source_image, stat, nbytes) bind(C)
        !! void c_co_broadcast(void* c_loc_a, int source_image, int* stat, size_t nbytes)
        import c_int, c_size_t, c_ptr
        implicit none
        type(c_ptr), value :: c_loc_a, stat
        integer(c_int), value :: source_image
        integer(c_size_t), value :: nbytes
      end subroutine

    end interface

    type(c_ptr) stat_ptr

    stat_ptr = get_c_ptr(stat)

    select rank(a)
      rank(0)
        select type(a)
          type is(integer(c_int32_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, c_sizeof(a))
          class default
            error stop "caf_co_broadcst: unsupported type"
        end select
      rank default
       error stop "caf_co_broadcast rank not yet supported"
    end select

  end procedure

end submodule co_broadcast_s
