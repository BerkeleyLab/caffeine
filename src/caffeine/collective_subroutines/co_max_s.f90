! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(collective_subroutines_m) co_max_s
  use iso_c_binding, only : c_int64_t, c_ptr, c_size_t, c_loc, c_double, c_null_ptr

  implicit none

contains

  module procedure caf_co_max

   interface

     !! void c_co_max_int32(void* c_loc_a, int Nelem)
     subroutine c_co_max_int32(c_loc_a, Nelem, c_loc_stat, c_loc_result_image) bind(C)
       import c_ptr, c_size_t
       type(c_ptr), value :: c_loc_a, c_loc_stat, c_loc_result_image
       integer(c_size_t), value :: Nelem
     end subroutine

     subroutine c_co_max_int64(c_loc_a, Nelem, c_loc_stat, c_loc_result_image) bind(C)
       import c_ptr, c_size_t
       type(c_ptr), value :: c_loc_a, c_loc_stat, c_loc_result_image
       integer(c_size_t), value :: Nelem
     end subroutine

     subroutine c_co_max_float(c_loc_a, Nelem, c_loc_stat, c_loc_result_image) bind(C)
       import c_ptr, c_size_t
       type(c_ptr), value :: c_loc_a, c_loc_stat, c_loc_result_image
       integer(c_size_t), value :: Nelem
     end subroutine

     subroutine c_co_max_double(c_loc_a, Nelem, c_loc_stat, c_loc_result_image) bind(C)
       import c_ptr, c_size_t
       type(c_ptr), value :: c_loc_a, c_loc_stat, c_loc_result_image
       integer(c_size_t), value :: Nelem
     end subroutine

    !subroutine c_co_max_char(c_loc_a, Nelem, c_loc_stat) bind(C)
    !  import c_ptr, c_size_t
    !  type(c_ptr), value :: c_loc_a, c_loc_stat
    !  integer(c_size_t), value :: Nelem
    !end subroutine

   end interface

   type(c_ptr) stat_ptr, result_image_ptr

   if (present(stat)) then
     stat_ptr = c_loc(stat)
   else
     stat_ptr = c_null_ptr
   end if

   if (present(result_image)) then
     result_image_ptr = c_loc(result_image)
   else
     result_image_ptr = c_null_ptr
   end if

   select rank(a)
     rank(0)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_max_int32(c_loc(a),  nelem=1_c_size_t, c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(integer(c_int64_t))
           call c_co_max_int64(c_loc(a),  nelem=1_c_size_t, c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_float))
           call c_co_max_float(c_loc(a),  nelem=1_c_size_t, c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_double))
           call c_co_max_double(c_loc(a), nelem=1_c_size_t, c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(character(len=*,kind=c_char))
           error stop  "caf_co_max: character type not yet supported"
        !  call c_co_max_char(c_loc(a), nelem=2_c_size_t, c_loc_stat=stat_ptr)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(1)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_max_int32(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(integer(c_int64_t))
           call c_co_max_int64(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_float))
           call c_co_max_float(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_double))
           call c_co_max_double(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(character(len=*,kind=c_char))
           error stop "caf_co_max: character type not yet supported"
        !  call c_co_max_char(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(2)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_max_int32(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(integer(c_int64_t))
           call c_co_max_int64(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_float))
           call c_co_max_float(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_double))
           call c_co_max_double(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(character(len=*,kind=c_char))
           error stop "caf_co_max: character type not yet supported"
        !  call c_co_max_char(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(3)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_max_int32(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(integer(c_int64_t))
           call c_co_max_int64(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_float))
           call c_co_max_float(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_double))
           call c_co_max_double(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(character(len=*,kind=c_char))
           error stop "caf_co_max: character type not yet supported"
        !  call c_co_max_char(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(4)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_max_int32(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(integer(c_int64_t))
           call c_co_max_int64(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_float))
           call c_co_max_float(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_double))
           call c_co_max_double(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(character(len=*,kind=c_char))
           error stop "caf_co_max: character type not yet supported"
        !  call c_co_max_char(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(5)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_max_int32(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(integer(c_int64_t))
           call c_co_max_int64(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_float))
           call c_co_max_float(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_double))
           call c_co_max_double(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(character(len=*,kind=c_char))
           error stop "caf_co_max: character type not yet supported"
        !  call c_co_max_char(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(6)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_max_int32(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(integer(c_int64_t))
           call c_co_max_int64(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_float))
           call c_co_max_float(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_double))
           call c_co_max_double(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(character(len=*,kind=c_char))
           error stop "caf_co_max: character type not yet supported"
        !  call c_co_max_char(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(7)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_max_int32(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(integer(c_int64_t))
           call c_co_max_int64(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_float))
           call c_co_max_float(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_double))
           call c_co_max_double(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(character(len=*,kind=c_char))
           error stop "caf_co_max: character type not yet supported"
        !  call c_co_max_char(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(8)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_max_int32(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(integer(c_int64_t))
           call c_co_max_int64(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_float))
           call c_co_max_float(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_double))
           call c_co_max_double(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(character(len=*,kind=c_char))
           error stop "caf_co_max: character type not yet supported"
        !  call c_co_max_char(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(9)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_max_int32(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(integer(c_int64_t))
           call c_co_max_int64(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_float))
           call c_co_max_float(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_double))
           call c_co_max_double(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(character(len=*,kind=c_char))
           error stop "caf_co_max: character type not yet supported"
        !  call c_co_max_char(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(10)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_max_int32(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(integer(c_int64_t))
           call c_co_max_int64(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_float))
           call c_co_max_float(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_double))
           call c_co_max_double(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(character(len=*,kind=c_char))
           error stop "caf_co_max: character type not yet supported"
        !  call c_co_max_char(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(11)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_max_int32(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(integer(c_int64_t))
           call c_co_max_int64(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_float))
           call c_co_max_float(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_double))
           call c_co_max_double(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(character(len=*,kind=c_char))
           error stop "caf_co_max: character type not yet supported"
        !  call c_co_max_char(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(12)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_max_int32(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(integer(c_int64_t))
           call c_co_max_int64(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_float))
           call c_co_max_float(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_double))
           call c_co_max_double(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(character(len=*,kind=c_char))
           error stop "caf_co_max: character type not yet supported"
        !  call c_co_max_char(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(13)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_max_int32(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(integer(c_int64_t))
           call c_co_max_int64(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_float))
           call c_co_max_float(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_double))
           call c_co_max_double(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(character(len=*,kind=c_char))
           error stop "caf_co_max: character type not yet supported"
        !  call c_co_max_char(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(14)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_max_int32(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(integer(c_int64_t))
           call c_co_max_int64(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_float))
           call c_co_max_float(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_double))
           call c_co_max_double(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(character(len=*,kind=c_char))
           error stop "caf_co_max: character type not yet supported"
        !  call c_co_max_char(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(15)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_max_int32(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(integer(c_int64_t))
           call c_co_max_int64(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_float))
           call c_co_max_float(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(real(c_double))
           call c_co_max_double(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr, c_loc_result_image=result_image_ptr)
         type is(character(len=*,kind=c_char))
           error stop "caf_co_max: character type not yet supported"
        !  call c_co_max_char(c_loc(a), nelem=size(a,kind=c_size_t), c_loc_stat=stat_ptr)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
   end select

  end procedure

end submodule co_max_s
