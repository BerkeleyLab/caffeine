! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) co_reduce_s
  use iso_c_binding, only: c_funloc
  implicit none
contains

  module subroutine prif_co_reduce(a, operation_wrapper, cdata, result_image, stat, errmsg, errmsg_alloc)
    type(*), intent(inout), target :: a(..)
    procedure(prif_operation_wrapper_interface), pointer, intent(in) :: operation_wrapper
    type(c_ptr), intent(in), value :: cdata
    integer(c_int), intent(in), optional :: result_image
    integer(c_int), intent(out), optional :: stat
    character(len=*), intent(inout), optional :: errmsg
    character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    call contiguous_co_reduce(a, operation_wrapper, cdata, result_image, stat, errmsg, errmsg_alloc)
  end subroutine

  subroutine contiguous_co_reduce(a, operation_wrapper, cdata, result_image, stat, errmsg, errmsg_alloc)
    type(*), intent(inout), target, contiguous :: a(..)
    procedure(prif_operation_wrapper_interface), pointer, intent(in) :: operation_wrapper
    type(c_ptr), intent(in), value :: cdata
    integer(c_int), intent(in), optional :: result_image
    integer(c_int), intent(out), optional :: stat
    character(len=*), intent(inout), optional :: errmsg
    character(len=:), intent(inout), allocatable, optional :: errmsg_alloc

    if (present(stat)) stat=0
    call assert(associated(operation_wrapper), "caf_co_reduce: associated(operation)")

    call caf_co_reduce( &
        a, &
        optional_value(result_image), &
        int(product(shape(a)), c_size_t), &
        c_funloc(operation_wrapper), &
        cdata, &
        current_team%info%gex_team)
  end subroutine

end submodule co_reduce_s
