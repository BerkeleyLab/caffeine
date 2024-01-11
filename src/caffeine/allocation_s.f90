! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(allocation_m) allocation_s
  use iso_c_binding, only: &
      c_sizeof, &
      c_f_pointer, &
      c_f_procpointer, &
      c_loc, &
      c_associated, &
      c_null_ptr, &
      c_null_funptr, &
      c_bool
  use caffeine_h_m, only: caf_allocate, caf_deallocate
  use collective_subroutines_m, only: prif_co_sum
  use program_termination_m, only: prif_error_stop
  use synchronization_m, only: prif_sync_all
  use teams_m, only: prif_team_type, current_team

  implicit none

contains

  module procedure prif_allocate
    ! TODO: determining the size of the handle and where the coarray begins
    !       becomes a bit more complicated if we don't allocate space for
    !       15 cobounds
    type(c_ptr) :: whole_block
    integer(c_size_t) :: handle_size, coarray_size, total_size
    type(handle_data) :: unused
    type(handle_data), pointer :: unused2(:)

    coarray_size = product(ubounds-lbounds+1)*element_length
    handle_size = c_sizeof(unused)
    total_size = handle_size + coarray_size

    ! TODO: have only "team leader" perform allocation
    !       and "broadcast" offset as part of synchronization
    whole_block = caf_allocate(current_team%heap, total_size)
    call prif_sync_all

    call c_f_pointer(whole_block, coarray_handle%info)
    call c_f_pointer(whole_block, unused2, [2])

    coarray_handle%info%coarray_data = c_loc(unused2(2))
    coarray_handle%info%corank = size(lcobounds)
    coarray_handle%info%coarray_size = coarray_size
    coarray_handle%info%final_func = final_func
    coarray_handle%info%cobounds(1:size(lcobounds))%lcobound = lcobounds
    coarray_handle%info%cobounds(1:size(lcobounds))%ucobound = ucobounds
    call add_to_team_list(current_team, coarray_handle)

    allocated_memory = coarray_handle%info%coarray_data
  end procedure

  module procedure prif_allocate_non_symmetric
  end procedure

  module procedure prif_deallocate
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
    integer(c_int) :: local_stat
    character(len=:), allocatable :: local_errmsg
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
      ! TODO: only team leader should perform deallocation
      call caf_deallocate(current_team%heap, c_loc(coarray_handles(i)%info))
      nullify(coarray_handles(i)%info)
    end do
    if (present(stat)) stat = 0
  end procedure

  module procedure prif_deallocate_non_symmetric
  end procedure

  subroutine add_to_team_list(current_team, coarray_handle)
    type(prif_team_type), intent(inout) :: current_team
    type(prif_coarray_handle), intent(inout) :: coarray_handle

    if (associated(current_team%coarrays)) then
      current_team%coarrays%previous_handle = c_loc(coarray_handle%info)
      coarray_handle%info%next_handle = c_loc(current_team%coarrays)
      coarray_handle%info%previous_handle = c_null_ptr
      current_team%coarrays => coarray_handle%info
    else
      current_team%coarrays => coarray_handle%info
      coarray_handle%info%next_handle = c_null_ptr
      coarray_handle%info%previous_handle = c_null_ptr
    end if
  end subroutine

  subroutine remove_from_team_list(coarray_handle)
    type(prif_coarray_handle), intent(inout) :: coarray_handle

    type(handle_data), pointer :: tmp_data

    if (c_associated(coarray_handle%info%previous_handle)) then
      call c_f_pointer(coarray_handle%info%previous_handle, tmp_data)
      tmp_data%next_handle = coarray_handle%info%next_handle
    end if
    if (c_associated(coarray_handle%info%next_handle)) then
      call c_f_pointer(coarray_handle%info%next_handle, tmp_data)
      tmp_data%previous_handle = coarray_handle%info%previous_handle
    end if
  end subroutine

end submodule allocation_s
