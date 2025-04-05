! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(prif:prif_private_s) allocation_s
  use iso_c_binding, only: &
      c_sizeof, &
      c_f_pointer, &
      c_f_procpointer, &
      c_loc, &
      c_null_funptr

  implicit none

contains

  module procedure prif_allocate_coarray
    ! TODO: determining the size of the handle and where the coarray begins
    !       becomes a bit more complicated if we don't allocate space for
    !       15 cobounds
    integer :: me
    type(c_ptr) :: whole_block
    integer(c_ptrdiff_t) :: block_offset
    integer(c_size_t) :: descriptor_size, total_size
    type(prif_coarray_descriptor) :: unused
    type(prif_coarray_descriptor), pointer :: unused2(:)

    call_assert(size(lcobounds) == size(ucobounds))
    call_assert(product(ucobounds - lcobounds + 1) >= current_team%info%num_images)

    me = current_team%info%this_image
    if (caf_have_child_teams()) then
      ! Free the child team space to make sure we have space to allocate the coarray
      if (me == 1) then
        call caf_deallocate(current_team%info%heap_mspace, current_team%info%child_heap_info%allocated_memory)
      end if
    end if
    if (me == 1) then
      descriptor_size = c_sizeof(unused)
      total_size = descriptor_size + size_in_bytes
      whole_block = caf_allocate(current_team%info%heap_mspace, total_size)
      block_offset = as_int(whole_block) - current_team%info%heap_start
    else
      block_offset = 0
    end if
    call prif_co_sum(block_offset)
    if (me /= 1) whole_block = as_c_ptr(current_team%info%heap_start + block_offset)

    call c_f_pointer(whole_block, coarray_handle%info)
    call c_f_pointer(whole_block, unused2, [2])

    coarray_handle%info%coarray_data = c_loc(unused2(2))
    coarray_handle%info%corank = size(lcobounds)
    coarray_handle%info%coarray_size = size_in_bytes
    coarray_handle%info%final_func = final_func
    coarray_handle%info%lcobounds(1:size(lcobounds)) = lcobounds
    coarray_handle%info%ucobounds(1:size(ucobounds)) = ucobounds
    call add_to_team_list(coarray_handle)
    coarray_handle%info%reserved = c_null_ptr
    coarray_handle%info%p_context_data = c_loc(coarray_handle%info%reserved)

    allocated_memory = coarray_handle%info%coarray_data
    if (caf_have_child_teams()) then
      call caf_establish_child_heap
    end if

    call_assert(coarray_handle_check(coarray_handle))
  end procedure

  module procedure prif_allocate
    allocated_memory = caf_allocate(non_symmetric_heap_mspace, size_in_bytes)
  end procedure

  module procedure prif_deallocate_coarray
    ! gfortran is yelling that this isn't valid for bind(C)
    ! https://gcc.gnu.org/bugzilla/show_bug.cgi?id=113338
    ! abstract interface
    !   subroutine coarray_cleanup_i(handle, stat, errmsg) bind(C)
    !     import c_int, prif_coarray_handle
    !     implicit none
    !     type(prif_coarray_handle), pointer, intent(in) :: handle
    !     integer(c_int), intent(out) :: stat
    !     character(len=:), intent(out), allocatable :: errmsg
    !   end subroutine
    ! end interface
    integer :: i, num_handles
    !integer(c_int) :: local_stat
    !character(len=:), allocatable :: local_errmsg
    ! procedure(coarray_cleanup_i), pointer :: coarray_cleanup
    character(len=*), parameter :: unallocated_message = "Attempted to deallocate unallocated coarray"

    call prif_sync_all ! Need to ensure we don't deallocate anything till everyone gets here
    num_handles = size(coarray_handles)
    if (.not. all([(associated(coarray_handles(i)%info), i = 1, num_handles)])) then
      if (present(stat)) then
        stat = 1 ! TODO: decide what our stat codes should be
        if (present(errmsg)) then
          errmsg = unallocated_message
        else if (present(errmsg_alloc)) then
          errmsg_alloc = unallocated_message
        end if
        return
      else
        call prif_error_stop(.false._c_bool, stop_code_char=unallocated_message)
      end if
    end if
    call_assert(all(coarray_handle_check(coarray_handles)))

    ! TODO: invoke finalizers from coarray_handles(:)%info%final_func
    ! do i = 1, num_handles
    !   if (coarray_handles(i)%info%final_func /= c_null_funptr) then
    !     call c_f_procpointer(coarray_handles(i)%info%final_func, coarray_cleanup)
    !     call coarray_cleanup(coarray_handles(i), local_stat, local_errmsg)
    !     call prif_co_sum(local_stat) ! Need to be sure it didn't fail on any images
    !     if (local_stat /= 0) then
    !       if (present(stat)) then
    !         stat = local_stat
    !         if (present(errmsg)) then
    !           errmsg = local_errmsg
    !         else if (present(errmsg_alloc)) then
    !           call move_alloc(local_errmsg, errmsg_alloc)
    !         end if
    !         return ! NOTE: We no longer have guarantees that coarrays are in consistent state
    !       else
    !         call prif_error_stop(.false._c_bool, stop_code_char=local_errmsg)
    !       end if
    !     end if
    !   end if
    ! end do
    do i = 1, num_handles
      call remove_from_team_list(coarray_handles(i))
      if (current_team%info%this_image == 1) &
        call caf_deallocate(current_team%info%heap_mspace, c_loc(coarray_handles(i)%info))
    end do
    if (present(stat)) stat = 0
    if (caf_have_child_teams()) then
      ! reclaim any free space possible for the child teams to use
      if (current_team%info%this_image == 1) then
        call caf_deallocate(current_team%info%heap_mspace, current_team%info%child_heap_info%allocated_memory)
      end if
      call caf_establish_child_heap
    end if
  end procedure

  module procedure prif_deallocate
    call caf_deallocate(non_symmetric_heap_mspace, mem)
  end procedure

  subroutine add_to_team_list(coarray_handle)
    type(prif_coarray_handle), intent(in) :: coarray_handle

    if (associated(current_team%info%coarrays)) then
      current_team%info%coarrays%previous_handle = c_loc(coarray_handle%info)
      coarray_handle%info%next_handle = c_loc(current_team%info%coarrays)
    end if
    current_team%info%coarrays => coarray_handle%info
  end subroutine

  subroutine remove_from_team_list(coarray_handle)
    type(prif_coarray_handle), intent(in) :: coarray_handle

    type(prif_coarray_descriptor), pointer :: tmp_data

    if (&
        .not.c_associated(coarray_handle%info%previous_handle) &
        .and. .not.c_associated(coarray_handle%info%next_handle)) then
      nullify(current_team%info%coarrays)
      return
    end if
    if (c_associated(coarray_handle%info%previous_handle)) then
      call c_f_pointer(coarray_handle%info%previous_handle, tmp_data)
      tmp_data%next_handle = coarray_handle%info%next_handle
    else
      call c_f_pointer(coarray_handle%info%next_handle, current_team%info%coarrays)
    end if
    if (c_associated(coarray_handle%info%next_handle)) then
      call c_f_pointer(coarray_handle%info%next_handle, tmp_data)
      tmp_data%previous_handle = coarray_handle%info%previous_handle
    end if
  end subroutine

end submodule allocation_s
