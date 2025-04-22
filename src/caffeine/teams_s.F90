! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) teams_s
  ! DO NOT ADD USE STATEMENTS HERE
  ! All use statements belong in prif_private_s.F90
  implicit none
contains

  module procedure prif_change_team
    team%info%heap_start = current_team%info%child_heap_info%offset + current_team%info%heap_start
    team%info%heap_size = current_team%info%child_heap_info%size
    if (caf_this_image(team%info%gex_team) == 1) then ! need to setup the heap for the team
      call caf_establish_mspace( &
          team%info%heap_mspace, &
          as_c_ptr(team%info%heap_start), &
          current_team%info%child_heap_info%size)
    end if
    current_team = team
    if (caf_have_child_teams()) then ! need to establish heap for child teams
      call caf_establish_child_heap
    end if
    call prif_sync_all ! child team sync required by F23 11.1.5.2
  end procedure

  module procedure prif_end_team
    type(prif_coarray_handle), allocatable :: teams_coarrays(:)
    integer :: num_coarrays_in_team, i
    type(prif_coarray_descriptor), pointer :: tmp_data

    ! deallocate the teams coarrays
    ! Currently we work to batch together all the deallocations into a single call
    ! to prif_deallocate_coarray(), in the hope it can amortize some costs
    num_coarrays_in_team = 0
    tmp_data => current_team%info%coarrays
    do while (associated(tmp_data))
      num_coarrays_in_team = num_coarrays_in_team + 1
      call c_f_pointer(tmp_data%next_handle, tmp_data)
    end do
    if (num_coarrays_in_team > 0) then
      allocate(teams_coarrays(num_coarrays_in_team))
      tmp_data => current_team%info%coarrays
      do i = 1, num_coarrays_in_team
        teams_coarrays(i)%info => tmp_data
        call c_f_pointer(tmp_data%next_handle, tmp_data)
      end do
      call prif_deallocate_coarray(teams_coarrays, stat, errmsg, errmsg_alloc)
      nullify(current_team%info%coarrays)
    else
      ! child team sync required by F23 11.1.5.2,
      ! because we skipped the prif_deallocate_coarray call above that includes same
      call prif_sync_all
    end if

    ! set the current team back to the parent team
    current_team%info => current_team%info%parent_team
  end procedure

  module procedure prif_form_team
    ! indicates this is the first time we're creating a child team
    if (.not.caf_have_child_teams()) then
      allocate(current_team%info%child_heap_info)
      call caf_establish_child_heap
    end if

    block
      integer(c_int) :: new_index_
      if (present(new_index)) then
        new_index_ = new_index
      else
        new_index_ = 1
      end if

! DOB: The two allocates in this procedure do not have a corresponding deallocate, 
! because Fortran lacks a destroy team operation. We consider this to represent
! a defect in the Fortran design of teams.
! As such, team-specific state such as these data structures and the corresponding 
! team-related data structures in GASNet can never be reclaimed.
      allocate(team%info)
      team%info%parent_team => current_team%info
      call caf_form_team(current_team%info%gex_team, team%info%gex_team, team_number, new_index_)
      team%info%team_number = team_number
      team%info%this_image = caf_this_image(team%info%gex_team)
      team%info%num_images = caf_num_images(team%info%gex_team)
    end block
  end procedure

  module procedure prif_get_team
    if (.not. present(level)) then
      team = current_team
    else if (level == PRIF_CURRENT_TEAM) then
      team = current_team
    else if (level == PRIF_PARENT_TEAM) then
      team = prif_team_type(current_team%info%parent_team)
    else if (level == PRIF_INITIAL_TEAM) then
      team = prif_team_type(initial_team)
    else
      call prif_error_stop(.false._c_bool, stop_code_char="prif_get_team: invalid level")
    endif
  end procedure

  module procedure prif_team_number
    if (present(team)) then
      team_number = team%info%team_number
    else
      team_number = current_team%info%team_number
    endif
  end procedure

end submodule
