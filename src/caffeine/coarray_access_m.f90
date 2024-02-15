! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module coarray_access_m
  use iso_c_binding, only: c_intptr_t, c_int, c_intmax_t, c_size_t, c_ptr, c_ptrdiff_t
  use allocation_m, only: prif_coarray_handle
  use teams_m, only: prif_team_type

  implicit none
  private
  public :: prif_put, prif_put_raw, prif_put_raw_strided, prif_get, prif_get_raw, prif_get_raw_strided

  interface

     module subroutine prif_put( &
         coarray_handle, coindices, value, first_element_addr, team, team_number, notify_ptr, stat, errmsg, errmsg_alloc)
       implicit none
       type(prif_coarray_handle), intent(in) :: coarray_handle
       integer(c_intmax_t), intent(in) :: coindices(:)
       type(*), intent(in), contiguous, target :: value(..)
       type(c_ptr), intent(in) :: first_element_addr
       type(prif_team_type), optional, intent(in) :: team
       integer(c_intmax_t), optional, intent(in) :: team_number
       integer(c_intptr_t), optional, intent(in) :: notify_ptr
       integer(c_int), intent(out), optional :: stat
       character(len=*), intent(inout), optional :: errmsg
       character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
     end subroutine

     module subroutine prif_put_raw(image_num, local_buffer, remote_ptr, notify_ptr, size, stat, errmsg, errmsg_alloc)
       implicit none
       integer(c_int), intent(in) :: image_num
       type(c_ptr), intent(in) :: local_buffer
       integer(c_intptr_t), intent(in) :: remote_ptr
       integer(c_intptr_t), optional, intent(in) :: notify_ptr
       integer(c_size_t), intent(in) :: size
       integer(c_int), intent(out), optional :: stat
       character(len=*), intent(inout), optional :: errmsg
       character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
     end subroutine

     module subroutine prif_put_raw_strided( &
         image_num, local_buffer, remote_ptr, element_size, extent, remote_ptr_stride, &
         local_buffer_stride, notify_ptr, stat, errmsg, errmsg_alloc)
       implicit none
       integer(c_int), intent(in) :: image_num
       type(c_ptr), intent(in) :: local_buffer
       integer(c_intptr_t), intent(in) :: remote_ptr
       integer(c_size_t), intent(in) :: element_size
       integer(c_size_t), intent(in) :: extent(:)
       integer(c_ptrdiff_t), intent(in) :: remote_ptr_stride(:)
       integer(c_ptrdiff_t), intent(in) :: local_buffer_stride(:)
       integer(c_intptr_t), optional, intent(in) :: notify_ptr
       integer(c_int), intent(out), optional :: stat
       character(len=*), intent(inout), optional :: errmsg
       character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
     end subroutine

     module subroutine prif_get( &
         coarray_handle, coindices, first_element_addr, value, team, team_number, stat, errmsg, errmsg_alloc)
       implicit none
       type(prif_coarray_handle), intent(in) :: coarray_handle
       integer(c_intmax_t), intent(in) :: coindices(:)
       type(c_ptr), intent(in) :: first_element_addr
       type(*), dimension(..), intent(inout), contiguous :: value
       type(prif_team_type), optional, intent(in) :: team
       integer(c_intmax_t), optional, intent(in) :: team_number
       integer(c_int), intent(out), optional :: stat
       character(len=*), intent(inout), optional :: errmsg
       character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
     end subroutine

     module subroutine prif_get_raw(image_num, local_buffer, remote_ptr, size, stat, errmsg, errmsg_alloc)
       implicit none
       integer(c_int), intent(in) :: image_num
       type(c_ptr), intent(in) :: local_buffer
       integer(c_intptr_t), intent(in) :: remote_ptr
       integer(c_size_t), intent(in) :: size
       integer(c_int), intent(out), optional :: stat
       character(len=*), intent(inout), optional :: errmsg
       character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
     end subroutine

     module subroutine prif_get_raw_strided( &
         image_num, local_buffer, remote_ptr, element_size, extent, remote_ptr_stride, local_buffer_stride, &
         stat, errmsg, errmsg_alloc)
       implicit none
       integer(c_int), intent(in) :: image_num
       type(c_ptr), intent(in) :: local_buffer
       integer(c_intptr_t), intent(in) :: remote_ptr
       integer(c_size_t), intent(in) :: element_size
       integer(c_size_t), intent(in) :: extent(:)
       integer(c_ptrdiff_t), intent(in) :: remote_ptr_stride(:)
       integer(c_ptrdiff_t), intent(in) :: local_buffer_stride(:)
       integer(c_int), intent(out), optional :: stat
       character(len=*), intent(inout), optional :: errmsg
       character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
     end subroutine

  end interface

end module coarray_access_m
