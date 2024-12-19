! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) co_reduce_s
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
    call unimplemented("prif_co_reduce")
  end subroutine

end submodule co_reduce_s
