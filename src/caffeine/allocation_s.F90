! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"
#include "version.h"
#include "language-support.F90"

submodule(prif:prif_private_s) allocation_s
  ! DO NOT ADD USE STATEMENTS HERE
  ! All use statements belong in prif_private_s.F90
  implicit none

contains

  module procedure prif_allocate_coarray
    ! TODO: determining the size of the handle and where the coarray begins
    !       becomes a bit more complicated if we don't allocate space for
    !       15 cobounds
    integer :: me
    type(c_ptr) :: whole_block
    integer(c_ptrdiff_t) :: block_offset
    integer(c_int) :: corank
    type(prif_coarray_descriptor), pointer :: dp

    call_assert(team_check(current_team))

    corank = size(lcobounds)
    call_assert(corank > 0)
    if (size(ucobounds) == corank) then
      call_assert(all(lcobounds <= ucobounds))
      call_assert(product(ucobounds - lcobounds + 1) >= current_team%info%num_images)
    else
      call_assert(size(ucobounds) == corank - 1)
      call_assert(all(lcobounds(1:corank-1) <= ucobounds))
    end if

    me = current_team%info%this_image
    if (caf_have_child_teams()) then
      ! Free the child team space to make sure we have space to allocate the coarray
      if (me == 1) then
        call caf_deallocate(current_team%info%heap_mspace, current_team%info%child_heap_info%allocated_memory)
      end if
    end if
    if (me == 1) then
    block
      type(prif_coarray_descriptor) :: unused
      integer(c_size_t), parameter :: descriptor_size = c_sizeof(unused)
      integer(c_size_t) :: total_size
      total_size = descriptor_size + size_in_bytes
      whole_block = caf_allocate(current_team%info%heap_mspace, total_size)
      if (.not. c_associated(whole_block)) then
        block_offset = -1 ! out of memory
      else
        block_offset = as_int(whole_block) - current_team%info%heap_start
      end if
    end block
    else
      block_offset = 0
    end if
    call prif_sync_memory ! end the current segment
    ! Use a co_sum to aggregate broadcasing the information from image 1
    ! together with the team barrier spec-required by coarray allocation
    call prif_co_sum(block_offset)
    if (block_offset == -1) then ! out of memory - abort allocation attempt
      call report_error(PRIF_STAT_OUT_OF_MEMORY, out_of_memory_message(size_in_bytes, .true.), &
                        stat, errmsg, errmsg_alloc)
      if (caf_have_child_teams()) then ! unroll state change above before return
        call caf_establish_child_heap
      end if
      return
    end if
    if (me /= 1) whole_block = as_c_ptr(current_team%info%heap_start + block_offset)

    coarray_handle%info = whole_block ! descriptor comes first in memory
    dp => handle_to_dp(coarray_handle)
    block
      type(prif_coarray_descriptor), pointer :: unused2(:)
      call c_f_pointer(whole_block, unused2, [2])
      dp%coarray_data = c_loc(unused2(2)) ! element data comes after descriptor
    end block
    dp%corank = corank
    dp%coarray_size = size_in_bytes
    if (associated(final_func)) then
      dp%final_func = c_funloc(final_func)
    else
      dp%final_func = c_null_funptr
    end if
    dp%lcobounds(1:corank) = lcobounds
    dp%ucobounds(1:corank-1) = ucobounds(1:corank-1)
    call compute_coshape_epp(lcobounds, ucobounds, dp%coshape_epp(1:corank))
#   if ASSERTIONS
      ! The following entries are dead, but initialize them to help detect defects
      dp%lcobounds(corank+1:15) = huge(0_c_int64_t)
      dp%ucobounds(corank:14) = -huge(0_c_int64_t)
      dp%coshape_epp(corank+1:15) = 0
#   endif
    dp%previous_handle = c_null_ptr
    dp%next_handle = c_null_ptr
    call add_to_team_list(coarray_handle)
    dp%reserved = c_null_ptr ! reserved holds the value of the context data
    dp%p_context_data = c_loc(dp%reserved)

    allocated_memory = dp%coarray_data
    if (caf_have_child_teams()) then
      call caf_establish_child_heap
    end if

    call_assert(coarray_handle_check(coarray_handle))
    call_assert(team_check(current_team))
  end procedure

  module procedure prif_allocate
    type(c_ptr) :: mem

    mem = caf_allocate_non_symmetric(size_in_bytes)
    if (.not. c_associated(mem)) then
      call report_error(PRIF_STAT_OUT_OF_MEMORY, out_of_memory_message(size_in_bytes, .false.), &
                        stat, errmsg, errmsg_alloc)
    else
      allocated_memory = mem
    end if
  end procedure

  function out_of_memory_message(size_in_bytes, symmetric) result(message)
    integer(c_size_t), intent(in) :: size_in_bytes
    logical, intent(in) :: symmetric
    character(len=:), allocatable :: mem_type
    character(len=:), allocatable :: message

    message = "Fortran shared heap is out of memory"
    if (symmetric) then
      mem_type = "coarray"
    else
      message = message // " on image " // num_to_str(initial_team%this_image)
      mem_type = "non-coarray"
    end if
    message = message // new_line('') &
       // "  while allocating " // num_to_str(size_in_bytes, .true.) // " of additional " &
       // mem_type // " memory." // new_line('') &
       // new_line('') &
       // "  Shared heap size information:" // new_line('') &
       // "    Total shared heap:          " // pad(num_to_str(total_heap_size, .true.)) &
       // "    (CAF_HEAP_SIZE)" // new_line('') &
       // "    Total non-coarray heap:     " // pad(num_to_str(non_symmetric_heap_size, .true.)) &
       // "    (CAF_COMP_FRAC * CAF_HEAP_SIZE)" // new_line('') &
       // "    Current team coarray heap:  " // pad(num_to_str(current_team%info%heap_size, .true.)) // new_line('') &
       // new_line('') &
       // "  Consider setting the CAF_HEAP_SIZE environment variable to request a larger heap."
  contains
    function pad(str) result(s)
      character(len=*), intent(in) :: str
      character(len=:), allocatable :: s
      s = str
      s = repeat(' ',max(0, 10 - len(str))) // s
    end function
  end function

#if CAF_PRIF_VERSION <= 6
  module procedure prif_deallocate_coarray
#else
  module procedure prif_deallocate_coarray
    call prif_deallocate_coarrays([coarray_handle], stat, errmsg, errmsg_alloc)
  end procedure
  module procedure prif_deallocate_coarrays
#endif
    integer :: i, num_handles
    type(prif_coarray_handle), target :: coarray_handle
    type(prif_coarray_descriptor), pointer :: dp
    procedure(prif_coarray_cleanup_interface), pointer :: coarray_cleanup
    integer(c_int) :: local_stat
    character(len=:), allocatable :: local_errmsg

    call prif_sync_all ! Need to ensure we don't deallocate anything till everyone gets here
    num_handles = size(coarray_handles)
    if (.not. all([(c_associated(coarray_handles(i)%info), i = 1, num_handles)])) then
      call report_error(CAF_STAT_INVALID_ARGUMENT, "Attempted to deallocate unallocated coarray", &
                        stat, errmsg, errmsg_alloc)
      return
    end if
    call_assert(all(coarray_handle_check(coarray_handles)))
    call_assert(team_check(current_team))

    ! invoke finalizers from coarray_handles(:)%final_func
    do i = 1, num_handles
      coarray_handle = coarray_handles(i) ! Add target attribute
      dp => handle_to_dp(coarray_handle)
      if (c_associated(dp%final_func)) then
        call c_f_procpointer(dp%final_func, coarray_cleanup)
        call coarray_cleanup(coarray_handle, local_stat, local_errmsg)
        call prif_co_max(local_stat) ! Need to be sure it didn't fail on any images
        if (local_stat /= 0) then
          if (.not. allocated(local_errmsg)) then ! provide a default errmsg
            local_errmsg = "coarray_cleanup finalization callback failed"
          end if
          call report_error(local_stat, local_errmsg, &
                            stat, errmsg, errmsg_alloc)
          return ! NOTE: We no longer have guarantees that coarrays are in consistent state
        end if
      end if
    end do

    do i = 1, num_handles
      call remove_from_team_list(coarray_handles(i))
      if (current_team%info%this_image == 1) &
        call caf_deallocate(current_team%info%heap_mspace, coarray_handles(i)%info)
    end do
    if (present(stat)) stat = 0
    if (caf_have_child_teams()) then
      ! reclaim any free space possible for the child teams to use
      if (current_team%info%this_image == 1) then
        call caf_deallocate(current_team%info%heap_mspace, current_team%info%child_heap_info%allocated_memory)
      end if
      call caf_establish_child_heap
    end if
    call_assert(team_check(current_team))
  end procedure

  module procedure prif_deallocate
    call caf_deallocate_non_symmetric(mem)
    if (present(stat)) stat = 0
  end procedure

  subroutine add_to_team_list(coarray_handle)
    type(prif_coarray_handle), intent(in) :: coarray_handle
    type(prif_coarray_descriptor), pointer :: dp

    dp => handle_to_dp(coarray_handle)

    call_assert(.not.c_associated(dp%previous_handle))
    call_assert(.not.c_associated(dp%next_handle))

    if (associated(current_team%info%coarrays)) then
      current_team%info%coarrays%previous_handle = coarray_handle%info
      dp%next_handle = c_loc(current_team%info%coarrays)
    end if
    current_team%info%coarrays => dp
  end subroutine

  subroutine remove_from_team_list(coarray_handle)
    type(prif_coarray_handle), intent(in) :: coarray_handle

    type(prif_coarray_descriptor), pointer :: nbr_dp, dp

    call_assert(associated(current_team%info%coarrays))
    dp => handle_to_dp(coarray_handle)

    if (c_associated(dp%previous_handle)) then ! have a predecessor
      call c_f_pointer(dp%previous_handle, nbr_dp)
      nbr_dp%next_handle = dp%next_handle
    else ! head of list
      call_assert(associated(current_team%info%coarrays, dp))
      if (c_associated(dp%next_handle)) then ! have a successor
        call c_f_pointer(dp%next_handle, current_team%info%coarrays)
      else ! sole element
        nullify(current_team%info%coarrays)
        return
      end if
    end if
    if (c_associated(dp%next_handle)) then ! have a successor
      call c_f_pointer(dp%next_handle, nbr_dp)
      nbr_dp%previous_handle = dp%previous_handle
    end if
  end subroutine

end submodule allocation_s
