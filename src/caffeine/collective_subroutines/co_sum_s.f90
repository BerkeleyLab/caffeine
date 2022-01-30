! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(collective_subroutines_m) co_sum_s
  use iso_c_binding, only : c_int64_t, c_ptr, c_size_t, c_loc, c_double, c_null_ptr, c_int
  use utilities_m, only : get_c_ptr, optional_value

  implicit none

contains

  module procedure caf_co_sum

   interface

     subroutine c_co_sum_int32(c_loc_a, Nelem, c_loc_stat, result_image) bind(C)
       import c_ptr, c_size_t, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
     end subroutine

     subroutine c_co_sum_int64(c_loc_a, Nelem, c_loc_stat, result_image) bind(C)
       import c_ptr, c_size_t, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
     end subroutine

     subroutine c_co_sum_float(c_loc_a, Nelem, c_loc_stat, result_image) bind(C)
       import c_ptr, c_size_t, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
     end subroutine

     subroutine c_co_sum_double(c_loc_a, Nelem, c_loc_stat, result_image) bind(C)
       import c_ptr, c_size_t, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
     end subroutine

   end interface

   type(c_ptr) stat_ptr

   stat_ptr = get_c_ptr(stat)

   select rank(a)
     rank(0)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_int32(c_loc(a),  1_c_size_t, stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call c_co_sum_int64(c_loc(a),  1_c_size_t, stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call c_co_sum_float(c_loc(a),  1_c_size_t, stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call c_co_sum_double(c_loc(a), 1_c_size_t, stat_ptr, optional_value(result_image))
         type is(complex(c_float))
           call c_co_sum_float(c_loc(a),  2_c_size_t, stat_ptr, optional_value(result_image))
         type is(complex(c_double))
           call c_co_sum_double(c_loc(a), 2_c_size_t, stat_ptr, optional_value(result_image))
         class default
           error stop         "caf_co_sum: co_sum argument 'A' must be of type integer, real, or character."  // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and c_float & c_double complex data."
       end select
     rank(1)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_int32(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call c_co_sum_int64(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         class default
           error stop         "caf_co_sum: co_sum argument 'A' must be of type integer, real, or character."  // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and c_float & c_double complex data."
       end select
     rank(2)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_int32(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call c_co_sum_int64(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         class default
           error stop         "caf_co_sum: co_sum argument 'A' must be of type integer, real, or character."  // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and c_float & c_double complex data."
       end select
     rank(3)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_int32(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call c_co_sum_int64(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         class default
           error stop         "caf_co_sum: co_sum argument 'A' must be of type integer, real, or character."  // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and c_float & c_double complex data."
       end select
     rank(4)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_int32(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call c_co_sum_int64(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         class default
           error stop         "caf_co_sum: co_sum argument 'A' must be of type integer, real, or character."  // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and c_float & c_double complex data."
       end select
     rank(5)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_int32(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call c_co_sum_int64(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         class default
           error stop         "caf_co_sum: co_sum argument 'A' must be of type integer, real, or character."  // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and c_float & c_double complex data."
       end select
     rank(6)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_int32(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call c_co_sum_int64(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         class default
           error stop         "caf_co_sum: co_sum argument 'A' must be of type integer, real, or character."  // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and c_float & c_double complex data."
       end select
     rank(7)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_int32(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call c_co_sum_int64(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         class default
           error stop         "caf_co_sum: co_sum argument 'A' must be of type integer, real, or character."  // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and c_float & c_double complex data."
       end select
     rank(8)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_int32(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call c_co_sum_int64(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         class default
           error stop         "caf_co_sum: co_sum argument 'A' must be of type integer, real, or character."  // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and c_float & c_double complex data."
       end select
     rank(9)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_int32(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call c_co_sum_int64(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         class default
           error stop         "caf_co_sum: co_sum argument 'A' must be of type integer, real, or character."  // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and c_float & c_double complex data."
       end select
     rank(10)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_int32(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call c_co_sum_int64(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         class default
           error stop         "caf_co_sum: co_sum argument 'A' must be of type integer, real, or character."  // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and c_float & c_double complex data."
       end select
     rank(11)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_int32(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call c_co_sum_int64(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         class default
           error stop         "caf_co_sum: co_sum argument 'A' must be of type integer, real, or character."  // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and c_float & c_double complex data."
       end select
     rank(12)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_int32(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call c_co_sum_int64(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         class default
           error stop         "caf_co_sum: co_sum argument 'A' must be of type integer, real, or character."  // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and c_float & c_double complex data."
       end select
     rank(13)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_int32(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call c_co_sum_int64(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         class default
           error stop         "caf_co_sum: co_sum argument 'A' must be of type integer, real, or character."  // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and c_float & c_double complex data."
       end select
     rank(14)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_int32(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call c_co_sum_int64(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         class default
           error stop         "caf_co_sum: co_sum argument 'A' must be of type integer, real, or character."  // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and c_float & c_double complex data."
       end select
     rank(15)
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_int32(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call c_co_sum_int64(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_float))
           call c_co_sum_float(c_loc(a),  size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(complex(c_double))
           call c_co_sum_double(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         class default
           error stop         "caf_co_sum: co_sum argument 'A' must be of type integer, real, or character."  // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and c_float & c_double complex data."
       end select
   end select

  end procedure

end submodule co_sum_s
