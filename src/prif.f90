! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module prif

  use iso_c_binding, only: c_int, c_bool, c_intptr_t, c_intmax_t, c_ptr
  use iso_fortran_env, only: atomic_int_kind, atomic_logical_kind

  use allocation_m, only: &
    prif_coarray_handle, prif_allocate, prif_allocate_non_symmetric, prif_deallocate, prif_deallocate_non_symmetric
  use coarray_access_m, only: &
    prif_put, prif_put_raw, prif_put_raw_strided, prif_get, prif_get_raw, prif_get_raw_strided
  use alias_m, only: prif_alias_create, prif_alias_destroy
  use coarray_queries_m, only: prif_lcobound, prif_ucobound, prif_coshape, prif_image_index
  use image_queries_m, only : prif_this_image, prif_num_images, prif_failed_images, prif_stopped_images, prif_image_status
  use prif_queries_m, only: prif_set_context_data, prif_get_context_data, prif_base_pointer, prif_local_data_size
  use collective_subroutines_m, only : prif_co_sum, prif_co_max, prif_co_min, prif_co_reduce, prif_co_broadcast
  use teams_m, only: prif_form_team, prif_change_team, prif_end_team, prif_team_type, prif_get_team, prif_team_number
  use synchronization_m, only : prif_sync_all, prif_sync_images, prif_sync_team, prif_sync_memory
  use locks_m, only: prif_lock_type, prif_lock, prif_unlock
  use critical_m, only: prif_critical_type, prif_critical, prif_end_critical

  implicit none

  private
  public :: prif_init
  public :: prif_stop, prif_error_stop, prif_fail_image
  public :: prif_coarray_handle, prif_allocate, prif_allocate_non_symmetric, prif_deallocate, prif_deallocate_non_symmetric
  public :: prif_put, prif_put_raw, prif_put_raw_strided, prif_get, prif_get_raw, prif_get_raw_strided
  public :: prif_alias_create, prif_alias_destroy
  public :: prif_lcobound, prif_ucobound, prif_coshape, prif_image_index
  public :: prif_this_image, prif_num_images, prif_failed_images, prif_stopped_images, prif_image_status
  public :: prif_set_context_data, prif_get_context_data, prif_base_pointer, prif_local_data_size
  public :: prif_co_sum, prif_co_max, prif_co_min, prif_co_reduce, prif_co_broadcast
  public :: prif_form_team, prif_change_team, prif_end_team, prif_team_type, prif_get_team, prif_team_number
  public :: prif_sync_all, prif_sync_images, prif_sync_team, prif_sync_memory
  public :: prif_lock_type, prif_lock, prif_unlock
  public :: prif_critical_type, prif_critical, prif_end_critical
  public :: prif_event_post, prif_event_wait, prif_event_query
  public :: prif_notify_wait
  public :: prif_atomic_add, prif_atomic_and, prif_atomic_or, prif_atomic_xor, prif_atomic_cas, prif_atomic_fetch_add
  public :: prif_atomic_fetch_and, prif_atomic_fetch_or, prif_atomic_fetch_xor, prif_atomic_define, prif_atomic_ref

  interface prif_atomic_cas
     module procedure prif_atomic_cas_int
     module procedure prif_atomic_cas_logical
  end interface

  interface prif_atomic_define
     module procedure prif_atomic_define_int
     module procedure prif_atomic_define_logical
  end interface

  interface prif_atomic_ref
     module procedure prif_atomic_ref_int
     module procedure prif_atomic_ref_logical
  end interface

  interface

    module subroutine prif_init(exit_code)
      implicit none
      integer(c_int), intent(out) :: exit_code
    end subroutine

    module subroutine prif_stop(quiet, stop_code_int, stop_code_char)
      implicit none
      logical(c_bool), intent(in) :: quiet
      integer(c_int), intent(in), optional :: stop_code_int
      character(len=*), intent(in), optional :: stop_code_char
    end subroutine

    module pure subroutine prif_error_stop(quiet, stop_code_int, stop_code_char)
      logical(c_bool), intent(in) :: quiet
      integer(c_int), intent(in), optional :: stop_code_int
      character(len=*), intent(in), optional :: stop_code_char
    end subroutine

    module subroutine prif_fail_image()
      implicit none
    end subroutine

    module subroutine prif_event_post(image_num, event_var_ptr, stat, errmsg, errmsg_alloc)
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
      integer(c_intmax_t), intent(in), optional :: until_count
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_event_query(event_var_ptr, count, stat)
      implicit none
      type(c_ptr), intent(in) :: event_var_ptr
      integer(c_intmax_t), intent(out) :: count
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_notify_wait(notify_var_ptr, until_count, stat, errmsg, errmsg_alloc)
      implicit none
      type(c_ptr), intent(in) :: notify_var_ptr
      integer(c_intmax_t), intent(in), optional :: until_count
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_atomic_add(atom_remote_ptr, image_num, value, stat)
      implicit none
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(c_int), intent(in) :: image_num
      integer(atomic_int_kind), intent(in) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_and(atom_remote_ptr, image_num, value, stat)
      implicit none
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(c_int), intent(in) :: image_num
      integer(atomic_int_kind), intent(in) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_or(atom_remote_ptr, image_num, value, stat)
      implicit none
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(c_int), intent(in) :: image_num
      integer(atomic_int_kind), intent(in) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_xor(atom_remote_ptr, image_num, value, stat)
      implicit none
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(c_int), intent(in) :: image_num
      integer(atomic_int_kind), intent(in) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_cas_int(atom_remote_ptr, image_num, old, compare, new, stat)
      implicit none
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(c_int), intent(in) :: image_num
      integer(atomic_int_kind), intent(out) :: old
      integer(atomic_int_kind), intent(in) :: compare
      integer(atomic_int_kind), intent(in) :: new
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_cas_logical(atom_remote_ptr, image_num, old, compare, new, stat)
      implicit none
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(c_int), intent(in) :: image_num
      logical(atomic_logical_kind), intent(out) :: old
      logical(atomic_logical_kind), intent(in) :: compare
      logical(atomic_logical_kind), intent(in) :: new
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_fetch_add(atom_remote_ptr, image_num, value, old, stat)
      implicit none
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(c_int), intent(in) :: image_num
      integer(atomic_int_kind), intent(in) :: value
      integer(atomic_int_kind), intent(out) :: old
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_fetch_and(atom_remote_ptr, image_num, value, old, stat)
      implicit none
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(c_int), intent(in) :: image_num
      integer(atomic_int_kind), intent(in) :: value
      integer(atomic_int_kind), intent(out) :: old
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_fetch_or(atom_remote_ptr, image_num, value, old, stat)
      implicit none
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(c_int), intent(in) :: image_num
      integer(atomic_int_kind), intent(in) :: value
      integer(atomic_int_kind), intent(out) :: old
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_fetch_xor(atom_remote_ptr, image_num, value, old, stat)
      implicit none
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(c_int), intent(in) :: image_num
      integer(atomic_int_kind), intent(in) :: value
      integer(atomic_int_kind), intent(out) :: old
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_define_int(atom_remote_ptr, image_num, value, stat)
      implicit none
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(c_int), intent(in) :: image_num
      integer(atomic_int_kind), intent(in) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_define_logical(atom_remote_ptr, image_num, value, stat)
      implicit none
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(c_int), intent(in) :: image_num
      logical(atomic_logical_kind), intent(in) :: value
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_ref_int(value, atom_remote_ptr, image_num, stat)
      implicit none
      integer(atomic_int_kind), intent(out) :: value
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(c_int), intent(in) :: image_num
      integer(c_int), intent(out), optional :: stat
    end subroutine

    module subroutine prif_atomic_ref_logical(value, atom_remote_ptr, image_num, stat)
      implicit none
      logical(atomic_logical_kind), intent(out) :: value
      integer(c_intptr_t), intent(in) :: atom_remote_ptr
      integer(c_int), intent(in) :: image_num
      integer(c_int), intent(out), optional :: stat
    end subroutine

  end interface

end module prif
