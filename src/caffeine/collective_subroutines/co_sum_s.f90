! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(collective_subroutines_m) co_sum_s
  use iso_c_binding, only : c_ptr, c_size_t, c_null_char, c_f_pointer
  use utilities_m, only : get_c_ptr, get_c_ptr_character, optional_value
  use caffeine_h_m, only : caf_c_co_sum
  implicit none

contains

  module procedure caf_co_sum
    type(c_ptr) stat_c_ptr, errmsg_c_ptr
    character(len=:), allocatable :: c_string
    character(len=:), pointer :: errmsg_f_ptr

    stat_c_ptr = get_c_ptr(stat)
    c_string = errmsg // c_null_char
    errmsg_c_ptr = get_c_ptr_character(c_string)

    call caf_c_co_sum(a, optional_value(result_image), stat_c_ptr, errmsg_c_ptr, int(product(shape(a)), c_size_t))
    call c_f_pointer(errmsg_c_ptr, errmsg_f_ptr)
    errmsg = errmsg_f_ptr
  end procedure

end submodule co_sum_s
