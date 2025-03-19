! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) co_min_s
  use iso_c_binding, only: c_loc, c_f_pointer
  implicit none

contains

  module procedure prif_co_min
    call contiguous_co_min(a, result_image, stat, errmsg, errmsg_alloc)
  end procedure

  subroutine contiguous_co_min(a, result_image, stat, errmsg, errmsg_alloc)
    implicit none
    type(*), intent(inout), target, contiguous :: a(..)
    integer(c_int), intent(in), optional :: result_image
    integer(c_int), intent(out), optional :: stat
    character(len=*), intent(inout), optional :: errmsg
    character(len=:), intent(inout), allocatable, optional :: errmsg_alloc

    if (present(stat)) stat=0

    call caf_co_min( &
          a, &
          optional_value(result_image), &
          int(product(shape(a)), c_size_t), &
          current_team%info%gex_team)
  end subroutine

  module procedure prif_co_min_character
    call unimplemented("prif_co_min_character")
    ! integer(c_size_t), target :: char_len
    ! procedure(prif_operation_wrapper_interface), pointer :: op

    ! char_len = len(a)
    ! op => char_min_wrapper
    ! call prif_co_reduce(a, op, c_loc(char_len), result_image, stat, errmsg, errmsg_alloc)
  end procedure

  ! subroutine char_min_wrapper(arg1, arg2_and_out, count, cdata) bind(C)
  !   type(c_ptr), intent(in), value :: arg1, arg2_and_out
  !   integer(c_size_t), intent(in), value :: count
  !   type(c_ptr), intent(in), value :: cdata

  !   integer(c_size_t), pointer :: char_len
  !   integer(c_size_t) :: i

  !   if (count == 0) return
  !   call c_f_pointer(cdata, char_len)
  !   block
  !     character(len=char_len,kind=c_char), pointer :: lhs(:), rhs_and_result(:)
  !     call c_f_pointer(arg1, lhs, [count])
  !     call c_f_pointer(arg2_and_out, rhs_and_result, [count])
  !     do i = 1, count
  !       if (lhs(i) <= rhs_and_result(i)) rhs_and_result(i) = lhs(i)
  !     end do
  !   end block
  ! end subroutine

end submodule co_min_s
