! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) teams_s
  use iso_c_binding, only: c_null_funptr, c_f_pointer, c_loc

  implicit none
contains

  module procedure prif_change_team
    if (caf_this_image(team%info%gex_team) == 1) then ! need to setup the heap for the team
      team%info%heap_start = current_team%info%child_heap_info%offset + current_team%info%heap_start
      team%info%heap_size = current_team%info%child_heap_info%size
      call caf_establish_mspace( &
          team%info%heap_mspace, &
          as_c_ptr(team%info%heap_start), &
          current_team%info%child_heap_info%size)
    end if
    current_team = team
    if (caf_have_child_teams()) then ! need to establish heap for child teams
      call caf_establish_child_heap
    end if
  end procedure

  module procedure prif_end_team
    call unimplemented("prif_end_team")
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

      team%info%parent_team => current_team%info
      call caf_form_team(current_team%info%gex_team, team%info%gex_team, team_number, new_index)
    end block
  end procedure

  module procedure prif_get_team
    call unimplemented("prif_get_team")
  end procedure

  module procedure prif_team_number
    call unimplemented("prif_team_number")
  end procedure

end submodule
