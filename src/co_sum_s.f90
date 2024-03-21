! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) co_sum_s
  use iso_c_binding, only : c_null_char, c_f_pointer, c_null_ptr
  use utilities_m, only : get_c_ptr, get_c_ptr_character, optional_value
  use caffeine_h_m, only : caf_co_sum

  implicit none

contains

  module procedure prif_co_sum
    type(c_ptr) stat_c_ptr, errmsg_c_ptr

    stat_c_ptr = c_null_ptr
    errmsg_c_ptr = c_null_ptr
    if (present(stat)) stat=0

    call caf_co_sum( &
        a, optional_value(result_image), stat_c_ptr, errmsg_c_ptr, int(product(shape(a)), c_size_t), current_team%gex_team)
  end procedure

end submodule co_sum_s
