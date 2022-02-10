! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(collective_subroutines_m) co_max_s
  use iso_c_binding, only : &
    c_int64_t, c_ptr, c_size_t, c_loc, c_double, c_null_ptr, c_int, c_funptr, c_associated, c_funloc, c_f_pointer
  use utilities_m, only : get_c_ptr, optional_value
  use assert_m, only : assert
  use intrinsic_array_m, only : intrinsic_array_t
  use caffeine_h_m, only : caf_c_co_max_int32, caf_c_co_max_int64, caf_c_co_max_float,  caf_c_co_max_double

  implicit none


contains

  module procedure caf_co_max

    procedure(c_char_operation), pointer :: op=>null()

   type(c_ptr) stat_ptr

   stat_ptr = get_c_ptr(stat)

   select rank(a)
     rank(0)
       select type(a)
         type is(integer(c_int32_t))
           call caf_c_co_max_int32(c_loc(a),  1_c_size_t, stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call caf_c_co_max_int64(c_loc(a),  1_c_size_t, stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call caf_c_co_max_float(c_loc(a),  1_c_size_t, stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call caf_c_co_max_double(c_loc(a), 1_c_size_t, stat_ptr, optional_value(result_image))
         type is(character(len=*,kind=c_char))
           op => reverse_alphabetize
           call caf_co_reduce(a, op, result_image, stat, errmsg)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(1)
       select type(a)
         type is(integer(c_int32_t))
           call caf_c_co_max_int32(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call caf_c_co_max_int64(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call caf_c_co_max_float(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call caf_c_co_max_double(c_loc(a), size(a,kind=c_size_t),stat_ptr, optional_value(result_image))
         type is(character(len=*,kind=c_char))
           op => reverse_alphabetize
           call caf_co_reduce(a, op, result_image, stat, errmsg)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(2)
       select type(a)
         type is(integer(c_int32_t))
           call caf_c_co_max_int32(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call caf_c_co_max_int64(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call caf_c_co_max_float(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call caf_c_co_max_double(c_loc(a), size(a,kind=c_size_t),stat_ptr, optional_value(result_image))
         type is(character(len=*,kind=c_char))
           op => reverse_alphabetize
           call caf_co_reduce(a, op, result_image, stat, errmsg)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(3)
       select type(a)
         type is(integer(c_int32_t))
           call caf_c_co_max_int32(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call caf_c_co_max_int64(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call caf_c_co_max_float(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call caf_c_co_max_double(c_loc(a), size(a,kind=c_size_t),stat_ptr, optional_value(result_image))
         type is(character(len=*,kind=c_char))
           op => reverse_alphabetize
           call caf_co_reduce(a, op, result_image, stat, errmsg)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(4)
       select type(a)
         type is(integer(c_int32_t))
           call caf_c_co_max_int32(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call caf_c_co_max_int64(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call caf_c_co_max_float(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call caf_c_co_max_double(c_loc(a), size(a,kind=c_size_t),stat_ptr, optional_value(result_image))
         type is(character(len=*,kind=c_char))
           op => reverse_alphabetize
           call caf_co_reduce(a, op, result_image, stat, errmsg)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(5)
       select type(a)
         type is(integer(c_int32_t))
           call caf_c_co_max_int32(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call caf_c_co_max_int64(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call caf_c_co_max_float(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call caf_c_co_max_double(c_loc(a), size(a,kind=c_size_t),stat_ptr, optional_value(result_image))
         type is(character(len=*,kind=c_char))
           op => reverse_alphabetize
           call caf_co_reduce(a, op, result_image, stat, errmsg)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(6)
       select type(a)
         type is(integer(c_int32_t))
           call caf_c_co_max_int32(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call caf_c_co_max_int64(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call caf_c_co_max_float(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call caf_c_co_max_double(c_loc(a), size(a,kind=c_size_t),stat_ptr, optional_value(result_image))
         type is(character(len=*,kind=c_char))
           op => reverse_alphabetize
           call caf_co_reduce(a, op, result_image, stat, errmsg)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(7)
       select type(a)
         type is(integer(c_int32_t))
           call caf_c_co_max_int32(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call caf_c_co_max_int64(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call caf_c_co_max_float(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call caf_c_co_max_double(c_loc(a), size(a,kind=c_size_t),stat_ptr, optional_value(result_image))
         type is(character(len=*,kind=c_char))
           op => reverse_alphabetize
           call caf_co_reduce(a, op, result_image, stat, errmsg)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(8)
       select type(a)
         type is(integer(c_int32_t))
           call caf_c_co_max_int32(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call caf_c_co_max_int64(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call caf_c_co_max_float(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call caf_c_co_max_double(c_loc(a), size(a,kind=c_size_t),stat_ptr, optional_value(result_image))
         type is(character(len=*,kind=c_char))
           op => reverse_alphabetize
           call caf_co_reduce(a, op, result_image, stat, errmsg)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(9)
       select type(a)
         type is(integer(c_int32_t))
           call caf_c_co_max_int32(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call caf_c_co_max_int64(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call caf_c_co_max_float(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call caf_c_co_max_double(c_loc(a), size(a,kind=c_size_t),stat_ptr, optional_value(result_image))
         type is(character(len=*,kind=c_char))
           op => reverse_alphabetize
           call caf_co_reduce(a, op, result_image, stat, errmsg)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(10)
       select type(a)
         type is(integer(c_int32_t))
           call caf_c_co_max_int32(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call caf_c_co_max_int64(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call caf_c_co_max_float(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call caf_c_co_max_double(c_loc(a), size(a,kind=c_size_t),stat_ptr, optional_value(result_image))
         type is(character(len=*,kind=c_char))
           op => reverse_alphabetize
           call caf_co_reduce(a, op, result_image, stat, errmsg)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(11)
       select type(a)
         type is(integer(c_int32_t))
           call caf_c_co_max_int32(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call caf_c_co_max_int64(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call caf_c_co_max_float(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call caf_c_co_max_double(c_loc(a), size(a,kind=c_size_t),stat_ptr, optional_value(result_image))
         type is(character(len=*,kind=c_char))
           op => reverse_alphabetize
           call caf_co_reduce(a, op, result_image, stat, errmsg)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(12)
       select type(a)
         type is(integer(c_int32_t))
           call caf_c_co_max_int32(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call caf_c_co_max_int64(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call caf_c_co_max_float(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call caf_c_co_max_double(c_loc(a), size(a,kind=c_size_t),stat_ptr, optional_value(result_image))
         type is(character(len=*,kind=c_char))
           op => reverse_alphabetize
           call caf_co_reduce(a, op, result_image, stat, errmsg)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(13)
       select type(a)
         type is(integer(c_int32_t))
           call caf_c_co_max_int32(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call caf_c_co_max_int64(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call caf_c_co_max_float(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call caf_c_co_max_double(c_loc(a), size(a,kind=c_size_t),stat_ptr, optional_value(result_image))
         type is(character(len=*,kind=c_char))
           op => reverse_alphabetize
           call caf_co_reduce(a, op, result_image, stat, errmsg)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(14)
       select type(a)
         type is(integer(c_int32_t))
           call caf_c_co_max_int32(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call caf_c_co_max_int64(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call caf_c_co_max_float(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call caf_c_co_max_double(c_loc(a), size(a,kind=c_size_t),stat_ptr, optional_value(result_image))
         type is(character(len=*,kind=c_char))
           op => reverse_alphabetize
           call caf_co_reduce(a, op, result_image, stat, errmsg)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
     rank(15)
       select type(a)
         type is(integer(c_int32_t))
           call caf_c_co_max_int32(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(integer(c_int64_t))
           call caf_c_co_max_int64(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_float))
           call caf_c_co_max_float(c_loc(a), size(a,kind=c_size_t), stat_ptr, optional_value(result_image))
         type is(real(c_double))
           call caf_c_co_max_double(c_loc(a), size(a,kind=c_size_t),stat_ptr, optional_value(result_image))
         type is(character(len=*,kind=c_char))
           op => reverse_alphabetize
           call caf_co_reduce(a, op, result_image, stat, errmsg)
         class default
           error stop         "caf_co_max: co_max argument 'A' must be of type integer, real, or character." // &
             new_line('a') // "Caffeine supports c_int_32_t & c_int_64_t integers; c_float & c_double reals;" // &
             new_line('a') // "and plans to support c_char characters."
       end select
   end select

   contains

    pure function reverse_alphabetize(lhs, rhs) result(last_alphabetically)
      character(len=*), intent(in) :: lhs, rhs 
      character(len=:), allocatable :: last_alphabetically
      call assert(len(lhs)==len(rhs), "co_reduce_s alphabetize: LHS/RHS length match", lhs//" , "//rhs)
      last_alphabetically = max(lhs,rhs)
    end function

  end procedure

end submodule co_max_s
