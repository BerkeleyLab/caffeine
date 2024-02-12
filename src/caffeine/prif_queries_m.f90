! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module prif_queries_m
  use iso_c_binding, only: c_ptr, c_intmax_t, c_intptr_t, c_size_t, c_int
  use allocation_m, only: prif_coarray_handle
  use teams_m, only: prif_team_type

  implicit none
  private
  public :: prif_set_context_data, prif_get_context_data, prif_base_pointer, prif_local_data_size

  interface

     module subroutine prif_set_context_data(coarray_handle, context_data)
       implicit none
       type(prif_coarray_handle), intent(in) :: coarray_handle
       type(c_ptr), intent(in) :: context_data
     end subroutine

     module subroutine prif_get_context_data(coarray_handle, context_data)
       implicit none
       type(prif_coarray_handle), intent(in) :: coarray_handle
       type(c_ptr), intent(out) :: context_data
     end subroutine

     module subroutine prif_base_pointer(coarray_handle, image_num, ptr)
       implicit none
       type(prif_coarray_handle), intent(in) :: coarray_handle
       integer(c_int), intent(in) :: image_num
       integer(c_intptr_t), intent(out) :: ptr
     end subroutine

     module subroutine prif_local_data_size(coarray_handle, data_size)
       implicit none
       type(prif_coarray_handle), intent(in) :: coarray_handle
       integer(c_size_t), intent(out) :: data_size
     end subroutine

  end interface

end module prif_queries_m
