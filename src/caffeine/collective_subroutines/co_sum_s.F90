! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) co_sum_s

  implicit none

contains

  module procedure prif_co_sum
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
