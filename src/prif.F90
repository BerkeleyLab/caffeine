! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module prif

  use iso_c_binding, only: &
      c_char, c_int, c_bool, c_intptr_t, c_ptr, &
      c_funptr, c_size_t, c_ptrdiff_t, c_null_ptr, c_int64_t

  implicit none

  private
  public :: prif_init
  public :: prif_register_stop_callback, prif_stop_callback_interface
  public :: prif_stop, prif_error_stop, prif_fail_image
  public :: prif_allocate_coarray, prif_allocate, prif_deallocate_coarray, prif_deallocate
  public :: prif_put, prif_put_indirect, prif_get, prif_get_indirect, prif_put_with_notify, prif_put_with_notify_indirect
  public :: prif_put_indirect_with_notify, prif_put_indirect_with_notify_indirect
  public :: prif_get_strided, prif_get_strided_indirect, prif_put_strided, prif_put_strided_indirect
  public :: prif_put_strided_with_notify, prif_put_strided_with_notify_indirect
  public :: prif_put_strided_indirect_with_notify, prif_put_strided_indirect_with_notify_indirect
  public :: prif_alias_create, prif_alias_destroy
  public :: prif_lcobound_with_dim, prif_lcobound_no_dim, prif_ucobound_with_dim, prif_ucobound_no_dim, prif_coshape
  public :: prif_image_index, prif_image_index_with_team, prif_image_index_with_team_number
  public :: prif_this_image_no_coarray, prif_this_image_with_coarray, prif_this_image_with_dim
  public :: prif_num_images, prif_num_images_with_team, prif_num_images_with_team_number
  public :: prif_failed_images, prif_stopped_images, prif_image_status
  public :: prif_local_data_pointer, prif_set_context_data, prif_get_context_data, prif_size_bytes
  public :: prif_co_sum, prif_co_max, prif_co_min, prif_co_reduce, prif_co_broadcast
  public :: prif_operation_wrapper_interface
  public :: prif_form_team, prif_change_team, prif_end_team, prif_get_team, prif_team_number
  public :: prif_sync_all, prif_sync_images, prif_sync_team, prif_sync_memory
  public :: prif_lock, prif_lock_indirect, prif_unlock, prif_unlock_indirect
  public :: prif_critical, prif_end_critical
  public :: prif_event_post, prif_event_post_indirect, prif_event_wait, prif_event_query
  public :: prif_notify_wait
  public :: prif_atomic_add, prif_atomic_add_indirect, prif_atomic_and, prif_atomic_and_indirect
  public :: prif_atomic_or, prif_atomic_or_indirect, prif_atomic_xor, prif_atomic_xor_indirect
  public :: prif_atomic_cas_int, prif_atomic_cas_int_indirect, prif_atomic_cas_logical, prif_atomic_cas_logical_indirect
  public :: prif_atomic_fetch_add, prif_atomic_fetch_add_indirect
  public :: prif_atomic_fetch_and, prif_atomic_fetch_and_indirect, prif_atomic_fetch_or, prif_atomic_fetch_or_indirect
  public :: prif_atomic_fetch_xor, prif_atomic_fetch_xor_indirect
  public :: prif_atomic_define_int, prif_atomic_define_int_indirect, prif_atomic_define_logical, prif_atomic_define_logical_indirect
  public :: prif_atomic_ref_int, prif_atomic_ref_int_indirect, prif_atomic_ref_logical, prif_atomic_ref_logical_indirect

  integer(c_int), parameter, public :: PRIF_VERSION_MAJOR = 0
  integer(c_int), parameter, public :: PRIF_VERSION_MINOR = 4

  integer(c_int), parameter, public :: PRIF_ATOMIC_INT_KIND = selected_int_kind(18)

#if HAVE_SELECTED_LOGICAL_KIND
  integer(c_int), parameter, public :: PRIF_ATOMIC_LOGICAL_KIND = selected_logical_kind(32)
#else
  integer(c_int), parameter, public :: PRIF_ATOMIC_LOGICAL_KIND = PRIF_ATOMIC_INT_KIND
#endif

  integer(c_int), parameter, public :: &
    PRIF_CURRENT_TEAM               = 101, &
    PRIF_INITIAL_TEAM               = 102, &
    PRIF_PARENT_TEAM                = 103, &
    PRIF_STAT_FAILED_IMAGE          = 201, &
    PRIF_STAT_LOCKED                = 202, &
    PRIF_STAT_LOCKED_OTHER_IMAGE    = 203, &
    PRIF_STAT_STOPPED_IMAGE         = 204, &
    PRIF_STAT_UNLOCKED              = 205, &
    PRIF_STAT_UNLOCKED_FAILED_IMAGE = 206, &
    PRIF_STAT_OUT_OF_MEMORY         = 301, &
    PRIF_STAT_ALREADY_INIT          = 302

  type, public :: prif_event_type
    private
    ! TODO: actual implementation
    integer :: unimplemented_feature_placeholder = 0
  end type

  type, public :: prif_lock_type
    private
    ! TODO: actual implementation
    integer :: unimplemented_feature_placeholder = 0
  end type

  type, public :: prif_critical_type
    private
    ! TODO: actual implementation
    integer :: unimplemented_feature_placeholder = 0
  end type

  type, public :: prif_notify_type
    private
    ! TODO: actual implementation
    integer :: unimplemented_feature_placeholder = 0
  end type

  type, public :: prif_coarray_handle
    private
    type(prif_coarray_descriptor), pointer :: info
  end type

  type, public :: prif_team_type
    private
    type(team_data), pointer :: info => null()
  end type

  abstract interface
    subroutine prif_stop_callback_interface( &
          is_error_stop, quiet, stop_code_int, stop_code_char)
      import :: c_bool, c_int
      implicit none
      logical(c_bool), intent(in) :: is_error_stop, quiet
      integer(c_int), intent(in), optional :: stop_code_int
      character(len=*), intent(in), optional :: stop_code_char
    end subroutine

    subroutine prif_operation_wrapper_interface(arg1, arg2_and_out, count, cdata) bind(C)
      import :: c_ptr, c_size_t
      implicit none
      type(c_ptr), intent(in), value :: arg1, arg2_and_out
      integer(c_size_t), intent(in), value :: count
      type(c_ptr), intent(in), value :: cdata
    end subroutine
  end interface

  interface

    module subroutine prif_init(stat)
      implicit none
      integer(c_int), intent(out) :: stat
    end subroutine

    module subroutine prif_register_stop_callback(callback)
      implicit none
      procedure(prif_stop_callback_interface), pointer, intent(in) :: callback
    end subroutine

    module subroutine prif_stop(quiet, stop_code_int, stop_code_char)
      implicit none
      logical(c_bool), intent(in) :: quiet
      integer(c_int), intent(in), optional :: stop_code_int
      character(len=*), intent(in), optional :: stop_code_char
    end subroutine

    module subroutine prif_error_stop(quiet, stop_code_int, stop_code_char)
      implicit none
      logical(c_bool), intent(in) :: quiet
      integer(c_int), intent(in), optional :: stop_code_int
      character(len=*), intent(in), optional :: stop_code_char
    end subroutine

    module subroutine prif_fail_image()
      implicit none
    end subroutine

    module subroutine prif_allocate_coarray( &
        lcobounds, ucobounds, size_in_bytes, final_func, coarray_handle, &
        allocated_memory, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int64_t), dimension(:), intent(in) :: lcobounds, ucobounds
      integer(c_size_t), intent(in) :: size_in_bytes
      type(c_funptr), intent(in) :: final_func
      type(prif_coarray_handle), intent(out) :: coarray_handle
      type(c_ptr), intent(out) :: allocated_memory
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_allocate(size_in_bytes, allocated_memory, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_size_t) :: size_in_bytes
      type(c_ptr), intent(out) :: allocated_memory
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_deallocate_coarray(coarray_handles, stat, errmsg, errmsg_alloc)
      implicit none
      type(prif_coarray_handle), intent(in) :: coarray_handles(:)
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_deallocate(mem, stat, errmsg, errmsg_alloc)
      implicit none
      type(c_ptr), intent(in) :: mem
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_put( &
        image_num, coarray_handle, offset, current_image_buffer, size_in_bytes, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      type(c_ptr), intent(in) :: current_image_buffer
      integer(c_size_t), intent(in) :: size_in_bytes
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_put_indirect( &
        image_num, remote_ptr, current_image_buffer, size_in_bytes, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: remote_ptr
      type(c_ptr), intent(in) :: current_image_buffer
      integer(c_size_t), intent(in) :: size_in_bytes
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_put_with_notify( &
        image_num, coarray_handle, offset, current_image_buffer, size_in_bytes, &
        notify_coarray_handle, notify_offset, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      type(c_ptr), intent(in) :: current_image_buffer
      integer(c_size_t), intent(in) :: size_in_bytes
      type(prif_coarray_handle), intent(in) :: notify_coarray_handle
      integer(c_size_t), intent(in) :: notify_offset
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_put_with_notify_indirect( &
        image_num, coarray_handle, offset, current_image_buffer, size_in_bytes, notify_ptr, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      type(c_ptr), intent(in) :: current_image_buffer
      integer(c_size_t), intent(in) :: size_in_bytes
      integer(c_intptr_t), intent(in) :: notify_ptr
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_put_indirect_with_notify( &
        image_num, remote_ptr, current_image_buffer, size_in_bytes, notify_coarray_handle, notify_offset, &
        stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: remote_ptr
      type(c_ptr), intent(in) :: current_image_buffer
      integer(c_size_t), intent(in) :: size_in_bytes
      type(prif_coarray_handle), intent(in) :: notify_coarray_handle
      integer(c_size_t), intent(in) :: notify_offset
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_put_indirect_with_notify_indirect( &
        image_num, remote_ptr, current_image_buffer, size_in_bytes, notify_ptr, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: remote_ptr
      type(c_ptr), intent(in) :: current_image_buffer
      integer(c_size_t), intent(in) :: size_in_bytes
      integer(c_intptr_t), intent(in) :: notify_ptr
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_get( &
        image_num, coarray_handle, offset, current_image_buffer, size_in_bytes, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      type(c_ptr), intent(in) :: current_image_buffer
      integer(c_size_t), intent(in) :: size_in_bytes
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_get_indirect(image_num, remote_ptr, current_image_buffer, size_in_bytes, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: remote_ptr
      type(c_ptr), intent(in) :: current_image_buffer
      integer(c_size_t), intent(in) :: size_in_bytes
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_get_strided( &
        image_num, coarray_handle, offset, remote_stride, current_image_buffer, current_image_stride, &
        element_size, extent, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      integer(c_ptrdiff_t), intent(in) :: remote_stride(:)
      type(c_ptr), intent(in) :: current_image_buffer
      integer(c_ptrdiff_t), intent(in) :: current_image_stride(:)
      integer(c_size_t), intent(in) :: element_size
      integer(c_size_t), intent(in) :: extent(:)
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_get_strided_indirect( &
        image_num, remote_ptr, remote_stride, current_image_buffer, current_image_stride, element_size, extent, &
        stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: remote_ptr
      integer(c_ptrdiff_t), intent(in) :: remote_stride(:)
      type(c_ptr), intent(in) :: current_image_buffer
      integer(c_ptrdiff_t), intent(in) :: current_image_stride(:)
      integer(c_size_t), intent(in) :: element_size
      integer(c_size_t), intent(in) :: extent(:)
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_put_strided( &
        image_num, coarray_handle, offset, remote_stride, current_image_buffer, current_image_stride, element_size, &
        extent, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      integer(c_ptrdiff_t), intent(in) :: remote_stride(:)
      type(c_ptr), intent(in) :: current_image_buffer
      integer(c_ptrdiff_t), intent(in) :: current_image_stride(:)
      integer(c_size_t), intent(in) :: element_size
      integer(c_size_t), intent(in) :: extent(:)
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_put_strided_indirect( &
        image_num, remote_ptr, remote_stride, current_image_buffer, current_image_stride, element_size, extent, &
        stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: remote_ptr
      integer(c_ptrdiff_t), intent(in) :: remote_stride(:)
      type(c_ptr), intent(in) :: current_image_buffer
      integer(c_ptrdiff_t), intent(in) :: current_image_stride(:)
      integer(c_size_t), intent(in) :: element_size
      integer(c_size_t), intent(in) :: extent(:)
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_put_strided_with_notify( &
        image_num, coarray_handle, offset, remote_stride, current_image_buffer, current_image_stride, element_size, &
        extent, notify_coarray_handle, notify_offset, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      integer(c_ptrdiff_t), intent(in) :: remote_stride(:)
      type(c_ptr), intent(in) :: current_image_buffer
      integer(c_ptrdiff_t), intent(in) :: current_image_stride(:)
      integer(c_size_t), intent(in) :: element_size
      integer(c_size_t), intent(in) :: extent(:)
      type(prif_coarray_handle), intent(in) :: notify_coarray_handle
      integer(c_size_t), intent(in) :: notify_offset
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_put_strided_with_notify_indirect( &
        image_num, coarray_handle, offset, remote_stride, current_image_buffer, current_image_stride, element_size, &
        extent, notify_ptr, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      integer(c_ptrdiff_t), intent(in) :: remote_stride(:)
      type(c_ptr), intent(in) :: current_image_buffer
      integer(c_ptrdiff_t), intent(in) :: current_image_stride(:)
      integer(c_size_t), intent(in) :: element_size
      integer(c_size_t), intent(in) :: extent(:)
      integer(c_intptr_t), intent(in) :: notify_ptr
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_put_strided_indirect_with_notify( &
        image_num, remote_ptr, remote_stride, current_image_buffer, current_image_stride, element_size, extent, &
        notify_coarray_handle, notify_offset, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: remote_ptr
      integer(c_ptrdiff_t), intent(in) :: remote_stride(:)
      type(c_ptr), intent(in) :: current_image_buffer
      integer(c_ptrdiff_t), intent(in) :: current_image_stride(:)
      integer(c_size_t), intent(in) :: element_size
      integer(c_size_t), intent(in) :: extent(:)
      type(prif_coarray_handle), intent(in) :: notify_coarray_handle
      integer(c_size_t), intent(in) :: notify_offset
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_put_strided_indirect_with_notify_indirect( &
        image_num, remote_ptr, remote_stride, current_image_buffer, current_image_stride, element_size, extent, &
        notify_ptr, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: remote_ptr
      integer(c_ptrdiff_t), intent(in) :: remote_stride(:)
      type(c_ptr), intent(in) :: current_image_buffer
      integer(c_ptrdiff_t), intent(in) :: current_image_stride(:)
      integer(c_size_t), intent(in) :: element_size
      integer(c_size_t), intent(in) :: extent(:)
      integer(c_intptr_t), intent(in) :: notify_ptr
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_alias_create(source_handle, alias_lcobounds, alias_ucobounds, alias_handle)
      implicit none
      type(prif_coarray_handle), intent(in) :: source_handle
      integer(c_int64_t), intent(in) :: alias_lcobounds(:)
      integer(c_int64_t), intent(in) :: alias_ucobounds(:)
      type(prif_coarray_handle), intent(out) :: alias_handle
    end subroutine

    module subroutine prif_alias_destroy(alias_handle)
      implicit none
      type(prif_coarray_handle), intent(in) :: alias_handle
    end subroutine

    module subroutine prif_lcobound_with_dim(coarray_handle, dim, lcobound)
      implicit none
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_int), intent(in) :: dim
      integer(c_int64_t), intent(out) :: lcobound
    end subroutine

    module subroutine prif_lcobound_no_dim(coarray_handle, lcobounds)
      implicit none
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_int64_t), intent(out) :: lcobounds(:)
    end subroutine

    module subroutine prif_ucobound_with_dim(coarray_handle, dim, ucobound)
      implicit none
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_int), intent(in) :: dim
      integer(c_int64_t), intent(out) :: ucobound
    end subroutine

    module subroutine prif_ucobound_no_dim(coarray_handle, ucobounds)
      implicit none
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_int64_t), intent(out) :: ucobounds(:)
    end subroutine

    module subroutine prif_coshape(coarray_handle, sizes)
      implicit none
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(out) :: sizes(:)
    end subroutine

    module subroutine prif_image_index(coarray_handle, sub, image_index)
      implicit none
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_int64_t), intent(in) :: sub(:)
      integer(c_int), intent(out) :: image_index
    end subroutine

    module subroutine prif_image_index_with_team(coarray_handle, sub, team, image_index)
      implicit none
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_int64_t), intent(in) :: sub(:)
      type(prif_team_type), intent(in) :: team
      integer(c_int), intent(out) :: image_index
    end subroutine

    module subroutine prif_image_index_with_team_number(coarray_handle, sub, team_number, image_index)
      implicit none
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_int64_t), intent(in) :: sub(:)
      integer(c_int), intent(in) :: team_number
      integer(c_int), intent(out) :: image_index
    end subroutine

    module subroutine prif_num_images(num_images)
      implicit none
      integer(c_int), intent(out) :: num_images
    end subroutine

    module subroutine prif_num_images_with_team(team, num_images)
      implicit none
      type(prif_team_type), intent(in) :: team
      integer(c_int), intent(out) :: num_images
    end subroutine

    module subroutine prif_num_images_with_team_number(team_number, num_images)
      implicit none
      integer(c_int64_t), intent(in) :: team_number
      integer(c_int), intent(out) :: num_images
    end subroutine

    module subroutine prif_this_image_no_coarray(team, this_image)
      implicit none
      type(prif_team_type), intent(in), optional :: team
      integer(c_int), intent(out) :: this_image
    end subroutine

    module subroutine prif_this_image_with_coarray(coarray_handle, team, cosubscripts)
      implicit none
      type(prif_coarray_handle), intent(in) :: coarray_handle
      type(prif_team_type), intent(in), optional :: team
      integer(c_int64_t), intent(out) :: cosubscripts(:)
    end subroutine

    module subroutine prif_this_image_with_dim(coarray_handle, dim, team, cosubscript)
      implicit none
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_int), intent(in) :: dim
      type(prif_team_type), intent(in), optional :: team
      integer(c_int64_t), intent(out) :: cosubscript
    end subroutine

    module subroutine prif_failed_images(team, failed_images)
      implicit none
      type(prif_team_type), intent(in), optional :: team
      integer(c_int), allocatable, intent(out) :: failed_images(:)
    end subroutine

    module subroutine prif_stopped_images(team, stopped_images)
      implicit none
      type(prif_team_type), intent(in), optional :: team
      integer(c_int), allocatable, intent(out) :: stopped_images(:)
    end subroutine

    module impure elemental subroutine prif_image_status(image, team, image_status)
      implicit none
      integer(c_int), intent(in) :: image
      type(prif_team_type), intent(in), optional :: team
      integer(c_int), intent(out) :: image_status
    end subroutine

    module subroutine prif_local_data_pointer(coarray_handle, local_data)
      type(prif_coarray_handle), intent(in) :: coarray_handle
      type(c_ptr), intent(out) :: local_data
    end subroutine

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

    module subroutine prif_size_bytes(coarray_handle, data_size)
      implicit none
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(out) :: data_size
    end subroutine

    module subroutine prif_co_sum(a, result_image, stat, errmsg, errmsg_alloc)
      implicit none
      type(*), intent(inout), target :: a(..)
      integer(c_int), intent(in), optional :: result_image
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_co_max(a, result_image, stat, errmsg, errmsg_alloc)
      implicit none
      type(*), intent(inout), target :: a(..)
      integer(c_int), intent(in), optional :: result_image
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_co_max_character(a, result_image, stat, errmsg, errmsg_alloc)
      implicit none
      character(len=*, kind=c_char), intent(inout), target :: a(..)
      integer(c_int), intent(in), optional :: result_image
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_co_min(a, result_image, stat, errmsg, errmsg_alloc)
      implicit none
      type(*), intent(inout), target :: a(..)
      integer(c_int), intent(in), optional :: result_image
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_co_min_character(a, result_image, stat, errmsg, errmsg_alloc)
      implicit none
      character(len=*, kind=c_char), intent(inout), target :: a(..)
      integer(c_int), intent(in), optional :: result_image
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_co_reduce(a, operation_wrapper, cdata, result_image, stat, errmsg, errmsg_alloc)
      implicit none
      type(*), intent(inout), target :: a(..)
      procedure(prif_operation_wrapper_interface), pointer, intent(in) :: operation_wrapper
      type(c_ptr), intent(in), value :: cdata
      integer(c_int), intent(in), optional :: result_image
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_co_broadcast(a, source_image, stat, errmsg, errmsg_alloc)
      implicit none
      type(*), intent(inout), target :: a(..)
      integer(c_int), intent(in) :: source_image
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_form_team(team_number, team, new_index, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int64_t), intent(in) :: team_number
      type(prif_team_type), intent(out) :: team
      integer(c_int), intent(in), optional :: new_index
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_change_team(team, stat, errmsg, errmsg_alloc)
      implicit none
      type(prif_team_type), intent(in) :: team
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_end_team(stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_get_team(level, team)
      implicit none
      integer(c_int), intent(in), optional :: level
      type(prif_team_type), intent(out) :: team
    end subroutine

    module subroutine prif_team_number(team, team_number)
      implicit none
      type(prif_team_type), intent(in), optional :: team
      integer(c_int64_t), intent(out) :: team_number
    end subroutine

    module subroutine prif_sync_all(stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_sync_images(image_set, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in), optional :: image_set(:)
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_sync_team(team, stat, errmsg, errmsg_alloc)
      implicit none
      type(prif_team_type), intent(in) :: team
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_sync_memory(stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_lock(image_num, coarray_handle, offset, acquired_lock, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      logical(c_bool), intent(out), optional :: acquired_lock
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_lock_indirect(image_num, lock_var_ptr, acquired_lock, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: lock_var_ptr
      logical(c_bool), intent(out), optional :: acquired_lock
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_unlock(image_num, coarray_handle, offset, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_unlock_indirect(image_num, lock_var_ptr, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: lock_var_ptr
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_critical(critical_coarray, stat, errmsg, errmsg_alloc)
      implicit none
      type(prif_coarray_handle), intent(in) :: critical_coarray
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_end_critical(critical_coarray)
      implicit none
      type(prif_coarray_handle), intent(in) :: critical_coarray
    end subroutine

    module subroutine prif_event_post(image_num, coarray_handle, offset, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_event_post_indirect(image_num, event_var_ptr, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: event_var_ptr
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_event_wait(event_var_ptr, until_count, stat, errmsg, errmsg_alloc)
      implicit none
      type(c_ptr), intent(in) :: event_var_ptr
      integer(c_int64_t), intent(in), optional :: until_count
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_event_query(event_var_ptr, count, stat)
      implicit none
      type(c_ptr), intent(in) :: event_var_ptr
      integer(c_int64_t), intent(out) :: count
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_notify_wait(notify_var_ptr, until_count, stat, errmsg, errmsg_alloc)
      implicit none
      type(c_ptr), intent(in) :: notify_var_ptr
      integer(c_int64_t), intent(in), optional :: until_count
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_atomic_add(image_num, coarray_handle, offset, value, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_add_indirect(image_num, atom_remote_ptr, value, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_and(image_num, coarray_handle, offset, value, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_and_indirect(image_num, atom_remote_ptr, value, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_or(image_num, coarray_handle, offset, value, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_or_indirect(image_num, atom_remote_ptr, value, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_xor(image_num, coarray_handle, offset, value, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_xor_indirect(image_num, atom_remote_ptr, value, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_cas_int(image_num, coarray_handle, offset, old, compare, new, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      integer(PRIF_ATOMIC_INT_KIND), intent(out) :: old
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: compare
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: new
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_cas_int_indirect(image_num, atom_remote_ptr, old, compare, new, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(PRIF_ATOMIC_INT_KIND), intent(out) :: old
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: compare
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: new
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_cas_logical(image_num, coarray_handle, offset, old, compare, new, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      logical(PRIF_ATOMIC_LOGICAL_KIND), intent(out) :: old
      logical(PRIF_ATOMIC_LOGICAL_KIND), intent(in) :: compare
      logical(PRIF_ATOMIC_LOGICAL_KIND), intent(in) :: new
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_cas_logical_indirect(image_num, atom_remote_ptr, old, compare, new, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      logical(PRIF_ATOMIC_LOGICAL_KIND), intent(out) :: old
      logical(PRIF_ATOMIC_LOGICAL_KIND), intent(in) :: compare
      logical(PRIF_ATOMIC_LOGICAL_KIND), intent(in) :: new
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_fetch_add(image_num, coarray_handle, offset, value, old, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: value
      integer(PRIF_ATOMIC_INT_KIND), intent(out) :: old
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_fetch_add_indirect(image_num, atom_remote_ptr, value, old, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: value
      integer(PRIF_ATOMIC_INT_KIND), intent(out) :: old
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_fetch_and(image_num, coarray_handle, offset, value, old, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: value
      integer(PRIF_ATOMIC_INT_KIND), intent(out) :: old
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_fetch_and_indirect(image_num, atom_remote_ptr, value, old, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: value
      integer(PRIF_ATOMIC_INT_KIND), intent(out) :: old
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_fetch_or(image_num, coarray_handle, offset, value, old, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: value
      integer(PRIF_ATOMIC_INT_KIND), intent(out) :: old
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_fetch_or_indirect(image_num, atom_remote_ptr, value, old, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: value
      integer(PRIF_ATOMIC_INT_KIND), intent(out) :: old
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_fetch_xor(image_num, coarray_handle, offset, value, old, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: value
      integer(PRIF_ATOMIC_INT_KIND), intent(out) :: old
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_fetch_xor_indirect(image_num, atom_remote_ptr, value, old, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: value
      integer(PRIF_ATOMIC_INT_KIND), intent(out) :: old
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_define_int(image_num, coarray_handle, offset, value, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_define_int_indirect(image_num, atom_remote_ptr, value, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(PRIF_ATOMIC_INT_KIND), intent(in) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_define_logical(image_num, coarray_handle, offset, value, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      logical(PRIF_ATOMIC_LOGICAL_KIND), intent(in) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_define_logical_indirect(image_num, atom_remote_ptr, value, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      logical(PRIF_ATOMIC_LOGICAL_KIND), intent(in) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_ref_int(image_num, coarray_handle, offset, value, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      integer(PRIF_ATOMIC_INT_KIND), intent(out) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_ref_int_indirect(image_num, atom_remote_ptr, value, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(PRIF_ATOMIC_INT_KIND), intent(out) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_ref_logical(image_num, coarray_handle, offset, value, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_size_t), intent(in) :: offset
      logical(PRIF_ATOMIC_LOGICAL_KIND), intent(out) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_ref_logical_indirect(image_num, atom_remote_ptr, value, stat)
      implicit none
      integer(c_int), intent(in) :: image_num
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      logical(PRIF_ATOMIC_LOGICAL_KIND), intent(out) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

  end interface

! Type definitions only relevant to Caffeine internals

  type, private, bind(C) :: prif_coarray_descriptor
    private
    type(c_ptr) :: coarray_data
    integer(c_int) :: corank
    integer(c_size_t) :: coarray_size
    type(c_funptr) :: final_func
    type(c_ptr) :: previous_handle = c_null_ptr, next_handle = c_null_ptr
    integer(c_int64_t) :: lcobounds(15), ucobounds(15)
  end type

  type, private :: team_data
    type(c_ptr) :: gex_team
    type(c_ptr) :: heap_mspace
    integer(c_intptr_t) :: heap_start
    integer(c_size_t) :: heap_size
    type(team_data), pointer :: parent_team => null()
    type(prif_coarray_descriptor), pointer :: coarrays => null()
    type(child_team_info), pointer :: child_heap_info => null()
  end type

  type :: child_team_info
    type(c_ptr) :: allocated_memory
    integer(c_ptrdiff_t) :: offset
    integer(c_size_t) :: size
  end type
end module prif
