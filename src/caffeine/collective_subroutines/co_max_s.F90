! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(prif:prif_private_s) co_max_s
  use iso_c_binding, only : c_funloc

  implicit none

contains

  module procedure prif_co_max
    if (present(stat)) stat=0

    if (caf_numeric_type(a)) then
      call caf_co_max( &
          a, optional_value(result_image), int(product(shape(a)), c_size_t), current_team%info%gex_team)
    else if (caf_is_f_string(a)) then
      call prif_co_reduce(a, c_funloc(reverse_alphabetize), optional_value(result_image), stat, errmsg, errmsg_alloc)
    else
      call prif_error_stop(.false._c_bool, stop_code_char="caf_co_max: unsupported type")
    end if

  contains

    function reverse_alphabetize(lhs, rhs) result(last_alphabetically)
      character(len=*), intent(in) :: lhs, rhs
      character(len=len(lhs)) :: last_alphabetically
      call_assert_diagnose(len(lhs)==len(rhs), "caf_co_max: LHS/RHS length match", lhs//" , "//rhs)
      last_alphabetically = max(lhs,rhs)
    end function

  end procedure

end submodule co_max_s
