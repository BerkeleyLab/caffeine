! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(collective_subroutines_m) co_max_s
  use iso_c_binding, only : c_ptr, c_size_t, c_null_char, c_f_pointer
  use utilities_m, only : get_c_ptr, get_c_ptr_character, optional_value
  use caffeine_h_m, only : caf_c_co_max, caf_c_is_character
  use assert_m, only : assert
  implicit none

contains

  module procedure caf_co_max
    type(c_ptr) stat_c_ptr, errmsg_c_ptr
    character(len=:), allocatable :: c_string
    character(len=:), pointer :: errmsg_f_ptr

    procedure(c_char_operation), pointer :: op => null()

    stat_c_ptr = get_c_ptr(stat)
    c_string = errmsg // c_null_char
    errmsg_c_ptr = get_c_ptr_character(c_string)

    if (caf_c_is_character(a)) then
      op => reverse_alphabetize
      error stop "caf_co_max: character arguments not yet supported"
      !call caf_co_reduce(a, op, result_image, stat, errmsg)
    else
      call caf_c_co_max(a, optional_value(result_image), stat_c_ptr, errmsg_c_ptr, int(product(shape(a)), c_size_t))
      call c_f_pointer(errmsg_c_ptr, errmsg_f_ptr)
      errmsg = errmsg_f_ptr
    end if

  contains

    pure function reverse_alphabetize(lhs, rhs) result(last_alphabetically)
      character(len=*), intent(in) :: lhs, rhs 
      character(len=:), allocatable :: last_alphabetically
      call assert(len(lhs)==len(rhs), "co_reduce_s reverse_alphabetize: LHS/RHS length match", lhs//" , "//rhs)
      last_alphabetically = max(lhs,rhs)
    end function

  end procedure

end submodule co_max_s
