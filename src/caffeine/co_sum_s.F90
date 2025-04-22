! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(prif:prif_private_s) co_sum_s
  ! DO NOT ADD USE STATEMENTS HERE
  ! All use statements belong in prif_private_s.F90
  implicit none

contains

  module procedure prif_co_sum
    if (present(result_image)) then
      call_assert(result_image >= 1 .and. result_image <= current_team%info%num_images)
    endif
    call contiguous_co_sum(a, result_image, stat, errmsg, errmsg_alloc)
  end procedure

  subroutine contiguous_co_sum(a, result_image, stat, errmsg, errmsg_alloc)
    type(*), intent(inout), target, contiguous :: a(..)
    integer(c_int), intent(in), optional :: result_image
    integer(c_int), intent(out), optional :: stat
    character(len=*), intent(inout), optional :: errmsg
    character(len=:), intent(inout), allocatable, optional :: errmsg_alloc

    if (present(stat)) stat=0

    call caf_co_sum( &
        a, optional_value(result_image), int(product(shape(a)), c_size_t), current_team%info%gex_team)
  end subroutine

end submodule co_sum_s
