! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module alias_m
  use iso_c_binding, only: c_intmax_t
  use allocation_m, only: prif_coarray_handle

  implicit none
  private
  public :: prif_alias_create, prif_alias_destroy

  interface

     module subroutine prif_alias_create(source_handle, alias_co_lbounds, alias_co_ubounds, alias_handle)
       implicit none
       type(prif_coarray_handle), intent(in) :: source_handle
       integer(c_intmax_t), intent(in) :: alias_co_lbounds(:)
       integer(c_intmax_t), intent(in) :: alias_co_ubounds(:)
       type(prif_coarray_handle), intent(out) :: alias_handle
     end subroutine

     module subroutine prif_alias_destroy(alias_handle)
       implicit none
       type(prif_coarray_handle), intent(in) :: alias_handle
     end subroutine

  end interface

end module alias_m
