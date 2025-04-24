! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(prif:prif_private_s) coarray_access_s
  ! DO NOT ADD USE STATEMENTS HERE
  ! All use statements belong in prif_private_s.F90
  implicit none

contains

  ! _______________________ Contiguous Put RMA ____________________________
  module procedure prif_put
    integer(c_intptr_t) :: remote_base

    call_assert(offset >= 0)

    call base_pointer(coarray_handle, image_num, remote_base)
    call prif_put_indirect( &
        image_num = image_num, &
        remote_ptr = remote_base + offset, &
        current_image_buffer = current_image_buffer, &
        size_in_bytes = size_in_bytes, &
        stat = stat, errmsg = errmsg, errmsg_alloc = errmsg_alloc)
  end procedure

  module procedure prif_put_indirect
    call_assert_describe(image_num > 0 .and. image_num <= initial_team%num_images, "image_num not within valid range")

    call caf_put( &
        image = image_num, &
        dest = remote_ptr, &
        src = current_image_buffer, &
        size = size_in_bytes)

    if (present(stat)) stat = 0
  end procedure

  module procedure prif_put_with_notify
    integer(c_intptr_t) :: remote_base
    integer(c_intptr_t) :: notify_remote_base

    call_assert(offset >= 0)
    call_assert(notify_offset >= 0)

    call base_pointer(coarray_handle, image_num, remote_base)
    call base_pointer(notify_coarray_handle, image_num, notify_remote_base)

    call prif_put_indirect_with_notify_indirect( &
        image_num = image_num, &
        remote_ptr = remote_base + offset, &
        current_image_buffer = current_image_buffer, &
        size_in_bytes = size_in_bytes, &
        notify_ptr = notify_remote_base + notify_offset, &
        stat = stat, errmsg = errmsg, errmsg_alloc = errmsg_alloc)
  end procedure

  module procedure prif_put_with_notify_indirect
    integer(c_intptr_t) :: remote_base

    call_assert(offset >= 0)

    call base_pointer(coarray_handle, image_num, remote_base)
    call prif_put_indirect_with_notify_indirect( &
        image_num = image_num, &
        remote_ptr = remote_base + offset, &
        current_image_buffer = current_image_buffer, &
        size_in_bytes = size_in_bytes, &
        notify_ptr = notify_ptr, &
        stat = stat, errmsg = errmsg, errmsg_alloc = errmsg_alloc)
  end procedure

  module procedure prif_put_indirect_with_notify
    integer(c_intptr_t) :: notify_remote_base

    call_assert(notify_offset >= 0)

    call base_pointer(notify_coarray_handle, image_num, notify_remote_base)
    call prif_put_indirect_with_notify_indirect( &
        image_num = image_num, &
        remote_ptr = remote_ptr, &
        current_image_buffer = current_image_buffer, &
        size_in_bytes = size_in_bytes, &
        notify_ptr = notify_remote_base + notify_offset, &
        stat = stat, errmsg = errmsg, errmsg_alloc = errmsg_alloc)
  end procedure

  module procedure prif_put_indirect_with_notify_indirect
    call_assert_describe(image_num > 0 .and. image_num <= initial_team%num_images, "image_num not within valid range")

    call caf_put( &
        image = image_num, &
        dest = remote_ptr, &
        src = current_image_buffer, &
        size = size_in_bytes)
    call caf_event_post(image_num, notify_ptr, &
           segment_boundary=0, release_fence=1)

    if (present(stat)) stat = 0
  end procedure

  ! _______________________ Contiguous Get RMA ____________________________
  module procedure prif_get
    integer(c_intptr_t) :: remote_base

    call_assert(offset >= 0)

    call base_pointer(coarray_handle, image_num, remote_base)
    call prif_get_indirect( &
        image_num = image_num, &
        remote_ptr = remote_base + offset, &
        current_image_buffer = current_image_buffer, &
        size_in_bytes = size_in_bytes, &
        stat = stat, &
        errmsg = errmsg, &
        errmsg_alloc = errmsg_alloc)
  end procedure

  module procedure prif_get_indirect
    call_assert_describe(image_num > 0 .and. image_num <= initial_team%num_images, "image_num not within valid range")

    call caf_get( &
        image = image_num, &
        dest = current_image_buffer, &
        src = remote_ptr, &
        size = size_in_bytes)
  end procedure

  ! _______________________ Strided Get RMA ____________________________
  ! This helper ensures the metadata arrays are contiguous (RMA data may still be non-contiguous)
  subroutine get_strided_helper( &
        image_num, remote_ptr, remote_stride, current_image_buffer, current_image_stride, element_size, extent, &
        stat, errmsg, errmsg_alloc)
    implicit none
    integer(c_int), intent(in) :: image_num
    integer(c_intptr_t), intent(in) :: remote_ptr
    integer(c_ptrdiff_t), intent(in), target, contiguous :: remote_stride(:)
    type(c_ptr), intent(in) :: current_image_buffer
    integer(c_ptrdiff_t), intent(in), target, contiguous :: current_image_stride(:)
    integer(c_size_t), intent(in) :: element_size
    integer(c_size_t), intent(in), target, contiguous :: extent(:)
    integer(c_int), intent(out), optional :: stat
    character(len=*), intent(inout), optional :: errmsg
    character(len=:), intent(inout), allocatable, optional :: errmsg_alloc

    call_assert_describe(image_num > 0 .and. image_num <= initial_team%num_images, "image_num not within valid range")
    call_assert(size(remote_stride) == size(extent))
    call_assert(size(current_image_stride) == size(extent))

    call caf_get_strided( &
        dims = size(extent), &
        image_num = image_num, &
        remote_ptr = remote_ptr, &
        remote_stride = c_loc(remote_stride), &
        current_image_buffer = current_image_buffer, &
        current_image_stride = c_loc(current_image_stride), &
        element_size = element_size, &
        extent = c_loc(extent))

    if (present(stat)) stat = 0
  end subroutine

  module procedure prif_get_strided
    integer(c_intptr_t) :: remote_base

    call_assert(offset >= 0)

    call base_pointer(coarray_handle, image_num, remote_base)
    call prif_get_strided_indirect( &
        image_num = image_num, &
        remote_ptr = remote_base + offset, &
        remote_stride = remote_stride, &
        current_image_buffer = current_image_buffer, &
        current_image_stride = current_image_stride, &
        element_size = element_size, &
        extent = extent, &
        stat = stat, &
        errmsg = errmsg, &
        errmsg_alloc = errmsg_alloc)
  end procedure

  module procedure prif_get_strided_indirect
    call get_strided_helper( &
        image_num = image_num, &
        remote_ptr = remote_ptr, &
        remote_stride = remote_stride, &
        current_image_buffer = current_image_buffer, &
        current_image_stride = current_image_stride, &
        element_size = element_size, &
        extent = extent, &
        stat = stat, &
        errmsg = errmsg, &
        errmsg_alloc = errmsg_alloc)
  end procedure

  ! _______________________ Strided Put RMA ____________________________
  ! This helper ensures the metadata arrays are contiguous (RMA data may still be non-contiguous)
  subroutine put_strided_helper( &
        image_num, remote_ptr, remote_stride, current_image_buffer, current_image_stride, element_size, extent, &
        stat, errmsg, errmsg_alloc)
    implicit none
    integer(c_int), intent(in) :: image_num
    integer(c_intptr_t), intent(in) :: remote_ptr
    integer(c_ptrdiff_t), intent(in), target, contiguous :: remote_stride(:)
    type(c_ptr), intent(in) :: current_image_buffer
    integer(c_ptrdiff_t), intent(in), target, contiguous :: current_image_stride(:)
    integer(c_size_t), intent(in) :: element_size
    integer(c_size_t), intent(in), target, contiguous :: extent(:)
    integer(c_int), intent(out), optional :: stat
    character(len=*), intent(inout), optional :: errmsg
    character(len=:), intent(inout), allocatable, optional :: errmsg_alloc

    call_assert_describe(image_num > 0 .and. image_num <= initial_team%num_images, "image_num not within valid range")
    call_assert(size(remote_stride) == size(extent))
    call_assert(size(current_image_stride) == size(extent))

    call caf_put_strided( &
        dims = size(extent), &
        image_num = image_num, &
        remote_ptr = remote_ptr, &
        remote_stride = c_loc(remote_stride), &
        current_image_buffer = current_image_buffer, &
        current_image_stride = c_loc(current_image_stride), &
        element_size = element_size, &
        extent = c_loc(extent))

    if (present(stat)) stat = 0
  end subroutine

  module procedure prif_put_strided
    integer(c_intptr_t) :: remote_base

    call_assert(offset >= 0)

    call base_pointer(coarray_handle, image_num, remote_base)
    call prif_put_strided_indirect( &
        image_num = image_num, &
        remote_ptr = remote_base + offset, &
        remote_stride = remote_stride, &
        current_image_buffer = current_image_buffer, &
        current_image_stride = current_image_stride, &
        element_size = element_size, &
        extent = extent, &
        stat = stat, errmsg = errmsg, errmsg_alloc = errmsg_alloc)
  end procedure

  module procedure prif_put_strided_indirect
    call put_strided_helper( &
        image_num = image_num, &
        remote_ptr = remote_ptr, &
        remote_stride = remote_stride, &
        current_image_buffer = current_image_buffer, &
        current_image_stride = current_image_stride, &
        element_size = element_size, &
        extent = extent, &
        stat = stat, errmsg = errmsg, errmsg_alloc = errmsg_alloc)
  end procedure

  module procedure prif_put_strided_with_notify
    integer(c_intptr_t) :: remote_base
    integer(c_intptr_t) :: notify_remote_base

    call_assert(offset >= 0)
    call_assert(notify_offset >= 0)

    call base_pointer(coarray_handle, image_num, remote_base)
    call base_pointer(notify_coarray_handle, image_num, notify_remote_base)

    call prif_put_strided_indirect_with_notify_indirect( &
        image_num = image_num, &
        remote_ptr = remote_base + offset, &
        remote_stride = remote_stride, &
        current_image_buffer = current_image_buffer, &
        current_image_stride = current_image_stride, &
        element_size = element_size, &
        extent = extent, &
        notify_ptr = notify_remote_base + notify_offset, &
        stat = stat, errmsg = errmsg, errmsg_alloc = errmsg_alloc)
  end procedure

  module procedure prif_put_strided_with_notify_indirect
    integer(c_intptr_t) :: remote_base

    call_assert(offset >= 0)

    call base_pointer(coarray_handle, image_num, remote_base)

    call prif_put_strided_indirect_with_notify_indirect( &
        image_num = image_num, &
        remote_ptr = remote_base + offset, &
        remote_stride = remote_stride, &
        current_image_buffer = current_image_buffer, &
        current_image_stride = current_image_stride, &
        element_size = element_size, &
        extent = extent, &
        notify_ptr = notify_ptr, &
        stat = stat, errmsg = errmsg, errmsg_alloc = errmsg_alloc)
  end procedure

  module procedure prif_put_strided_indirect_with_notify
    integer(c_intptr_t) :: notify_remote_base

    call_assert(notify_offset >= 0)

    call base_pointer(notify_coarray_handle, image_num, notify_remote_base)

    call prif_put_strided_indirect_with_notify_indirect( &
        image_num = image_num, &
        remote_ptr = remote_ptr, &
        remote_stride = remote_stride, &
        current_image_buffer = current_image_buffer, &
        current_image_stride = current_image_stride, &
        element_size = element_size, &
        extent = extent, &
        notify_ptr = notify_remote_base + notify_offset, &
        stat = stat, errmsg = errmsg, errmsg_alloc = errmsg_alloc)
  end procedure

  module procedure prif_put_strided_indirect_with_notify_indirect
    call put_strided_helper( &
        image_num = image_num, &
        remote_ptr = remote_ptr, &
        remote_stride = remote_stride, &
        current_image_buffer = current_image_buffer, &
        current_image_stride = current_image_stride, &
        element_size = element_size, &
        extent = extent, &
        stat = stat, errmsg = errmsg, errmsg_alloc = errmsg_alloc)
    call caf_event_post(image_num, notify_ptr, &
           segment_boundary=0, release_fence=1)
  end procedure

end submodule coarray_access_s
