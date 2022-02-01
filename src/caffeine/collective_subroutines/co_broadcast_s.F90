submodule(collective_subroutines_m)  co_broadcast_s
  use iso_c_binding, only : c_int, c_size_t, c_ptr, c_loc, c_sizeof, c_int64_t, c_double
  use utilities_m, only : get_c_ptr
  implicit none

contains

  module procedure caf_co_broadcast

    interface

      subroutine c_co_broadcast(c_loc_a, source_image, stat, nbytes) bind(C)
        import c_int, c_size_t, c_ptr
        implicit none
        type(c_ptr), value :: c_loc_a, stat
        integer(c_int), value :: source_image
        integer(c_size_t), value :: nbytes
      end subroutine

    end interface

    type(c_ptr) stat_ptr

    stat_ptr = get_c_ptr(stat)

    ! Once Fortran 2018 assume-type arguments to c_sizeof is supported, the type 
    ! and rank guarding below might be replaceable by one line:
    !
    ! call c_co_broadcast(c_loc(a), source_image, stat_ptr, c_sizeof(a))

    select rank(a)
      rank(0)
        select type(a)
          type is(integer(c_int32_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, c_sizeof(a))
          type is(integer(c_int64_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, c_sizeof(a))
          type is(real(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, c_sizeof(a))
          type is(real(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, c_sizeof(a))
          type is(complex(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, c_sizeof(a))
          type is(complex(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, c_sizeof(a))
          type is(logical(c_bool))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, c_sizeof(a))
          type is(character(len=*,kind=c_char))
#ifndef __GFORTRAN__
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, len(a)*c_sizeof(a(1:1)))
#else
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, len(a)*c_sizeof([character(len=1,kind=c_char)::]))
#endif
          class default
            error stop "caf_co_broadcst: unsupported type"
        end select
      rank(1)
        select type(a)
          type is(integer(c_int32_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1)))
          type is(integer(c_int64_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1)))
          type is(real(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1)))
          type is(real(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1)))
          type is(complex(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1)))
          type is(complex(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1)))
          type is(logical(c_bool))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1)))
          type is(character(len=*, kind=c_char))
#ifndef __GFORTRAN__
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, len(a)*size(a)*c_sizeof(a(1)(1:1)))
#else
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, len(a)*size(a)*c_sizeof([character(len=1,kind=c_char)::]))
#endif
          class default
            error stop "caf_co_broadcst: unsupported type"
        end select
      rank(2)
        select type(a)
          type is(integer(c_int32_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1)))
          type is(integer(c_int64_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1)))
          type is(real(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1)))
          type is(real(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1)))
          type is(complex(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1)))
          type is(complex(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1)))
          type is(logical(c_bool))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1)))
          type is(character(len=*, kind=c_char))
#ifndef __GFORTRAN__
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, len(a)*size(a)*c_sizeof(a(1,1)(1:1)))
#else
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof([character(len=1,kind=c_char)::]))
#endif
          class default
            error stop "caf_co_broadcst: unsupported type"
        end select
      rank(3)
        select type(a)
          type is(integer(c_int32_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1)))
          type is(integer(c_int64_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1)))
          type is(real(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1)))
          type is(real(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1)))
          type is(complex(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1)))
          type is(complex(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1)))
          type is(logical(c_bool))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1)))
          type is(character(len=*, kind=c_char))
#ifndef __GFORTRAN__
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof(a(1,1,1)(1:1)))
#else
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof([character(len=1,kind=c_char)::]))
#endif
          class default
            error stop "caf_co_broadcst: unsupported type"
        end select
      rank(4)
        select type(a)
          type is(integer(c_int32_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1,1)))
          type is(integer(c_int64_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1,1)))
          type is(real(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1,1)))
          type is(real(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1,1)))
          type is(complex(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1,1)))
          type is(complex(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1,1)))
          type is(logical(c_bool))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1,1)))
          type is(character(len=*, kind=c_char))
#ifndef __GFORTRAN__
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof(a(1,1,1,1)(1:1)))
#else
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof([character(len=1,kind=c_char)::]))
#endif
          class default
            error stop "caf_co_broadcst: unsupported type"
        end select
      rank(5)
        select type(a)
          type is(integer(c_int32_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1,1,1)))
          type is(integer(c_int64_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1,1,1)))
          type is(real(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1,1,1)))
          type is(real(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1,1,1)))
          type is(complex(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1,1,1)))
          type is(complex(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1,1,1)))
          type is(logical(c_bool))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1,1,1)))
          type is(character(len=*, kind=c_char))
#ifndef __GFORTRAN__
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof(a(1,1,1,1,1)(1:1)))
#else
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof([character(len=1,kind=c_char)::]))
#endif
          class default
            error stop "caf_co_broadcst: unsupported type"
        end select
      rank(6)
        select type(a)
          type is(integer(c_int32_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1)))
          type is(integer(c_int64_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1)))
          type is(real(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1)))
          type is(real(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1)))
          type is(complex(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1)))
          type is(complex(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1)))
          type is(logical(c_bool))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1)))
          type is(character(len=*, kind=c_char))
#ifndef __GFORTRAN__
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof(a(1,1,1, 1,1,1)(1:1)))
#else
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof([character(len=1,kind=c_char)::]))
#endif
          class default
            error stop "caf_co_broadcst: unsupported type"
        end select
      rank(7)
        select type(a)
          type is(integer(c_int32_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1)))
          type is(integer(c_int64_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1)))
          type is(real(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1)))
          type is(real(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1)))
          type is(complex(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1)))
          type is(complex(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1)))
          type is(logical(c_bool))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1)))
          type is(character(len=*, kind=c_char))
#ifndef __GFORTRAN__
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof(a(1,1,1, 1,1,1, 1)(1:1)))
#else
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof([character(len=1,kind=c_char)::]))
#endif
          class default
            error stop "caf_co_broadcst: unsupported type"
        end select
      rank(8)
        select type(a)
          type is(integer(c_int32_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1)))
          type is(integer(c_int64_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1)))
          type is(real(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1)))
          type is(real(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1)))
          type is(complex(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1)))
          type is(complex(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1)))
          type is(logical(c_bool))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1)))
          type is(character(len=*, kind=c_char))
#ifndef __GFORTRAN__
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1)(1:1)))
#else
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof([character(len=1,kind=c_char)::]))
#endif
          class default
            error stop "caf_co_broadcst: unsupported type"
        end select
      rank(9)
        select type(a)
          type is(integer(c_int32_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1)))
          type is(integer(c_int64_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1)))
          type is(real(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1)))
          type is(real(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1)))
          type is(complex(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1)))
          type is(complex(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1)))
          type is(logical(c_bool))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1)))
          type is(character(len=*, kind=c_char))
#ifndef __GFORTRAN__
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1)(1:1)))
#else
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof([character(len=1,kind=c_char)::]))
#endif
          class default
            error stop "caf_co_broadcst: unsupported type"
        end select
      rank(10)
        select type(a)
          type is(integer(c_int32_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1)))
          type is(integer(c_int64_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1)))
          type is(real(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1)))
          type is(real(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1)))
          type is(complex(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1)))
          type is(complex(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1)))
          type is(logical(c_bool))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1)))
          type is(character(len=*, kind=c_char))
#ifndef __GFORTRAN__
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1)(1:1)))
#else
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof([character(len=1,kind=c_char)::]))
#endif
          class default
            error stop "caf_co_broadcst: unsupported type"
        end select
      rank(11)
        select type(a)
          type is(integer(c_int32_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1)))
          type is(integer(c_int64_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1)))
          type is(real(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1)))
          type is(real(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1)))
          type is(complex(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1)))
          type is(complex(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1)))
          type is(logical(c_bool))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1)))
          type is(character(len=*, kind=c_char))
#ifndef __GFORTRAN__
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1)(1:1)))
#else
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof([character(len=1,kind=c_char)::]))
#endif
          class default
            error stop "caf_co_broadcst: unsupported type"
        end select
      rank(12)
        select type(a)
          type is(integer(c_int32_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1)))
          type is(integer(c_int64_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1)))
          type is(real(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1)))
          type is(real(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1)))
          type is(complex(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1)))
          type is(complex(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1)))
          type is(logical(c_bool))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1)))
          type is(character(len=*, kind=c_char))
#ifndef __GFORTRAN__
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1)(1:1)))
#else
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof([character(len=1,kind=c_char)::]))
#endif
          class default
            error stop "caf_co_broadcst: unsupported type"
        end select
      rank(13)
        select type(a)
          type is(integer(c_int32_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1)))
          type is(integer(c_int64_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1)))
          type is(real(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1)))
          type is(real(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1)))
          type is(complex(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1)))
          type is(complex(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1)))
          type is(logical(c_bool))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1)))
          type is(character(len=*, kind=c_char))
#ifndef __GFORTRAN__
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1)(1:1)))
#else
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof([character(len=1,kind=c_char)::]))
#endif
          class default
            error stop "caf_co_broadcst: unsupported type"
        end select
      rank(14)
        select type(a)
          type is(integer(c_int32_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1,1)))
          type is(integer(c_int64_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1,1)))
          type is(real(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1,1)))
          type is(real(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1,1)))
          type is(complex(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1,1)))
          type is(complex(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1,1)))
          type is(logical(c_bool))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1,1)))
          type is(character(len=*, kind=c_char))
#ifndef __GFORTRAN__
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1,1)(1:1)))
#else
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof([character(len=1,kind=c_char)::]))
#endif
          class default
            error stop "caf_co_broadcst: unsupported type"
        end select
      rank(15)
        select type(a)
          type is(integer(c_int32_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1,1,1)))
          type is(integer(c_int64_t))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1,1,1)))
          type is(real(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1,1,1)))
          type is(real(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1,1,1)))
          type is(complex(c_float))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1,1,1)))
          type is(complex(c_double))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1,1,1)))
          type is(logical(c_bool))
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*c_sizeof(a(1,1,1, 1,1,1, 1,1,1, 1,1,1, 1,1,1)))
          type is(character(len=*, kind=c_char))
#ifndef __GFORTRAN__
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof(a(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)(1:1)))
#else
            call c_co_broadcast(c_loc(a), source_image, stat_ptr, size(a)*len(a)*c_sizeof([character(len=1,kind=c_char)::]))
#endif
          class default
            error stop "caf_co_broadcst: unsupported type"
        end select
      rank default
       error stop "caf_co_broadcast rank not yet supported"
    end select

  end procedure

end submodule co_broadcast_s
