! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) teams_s
  use iso_c_binding, only: c_null_funptr, c_f_pointer

  implicit none
contains

  module procedure prif_change_team
    call unimplemented("prif_change_team")
  end procedure

  module procedure prif_end_team
    call unimplemented("prif_end_team")
  end procedure

  module procedure prif_form_team
    if (.not.associated(current_team%info%child_heap_info)) then
      block
        type(c_ptr) :: allocated_memory
        integer :: num_imgs
        type(child_team_info) :: dummy_element

        call prif_num_images(num_images = num_imgs)
        call prif_allocate_coarray( &
            lcobounds = [1_c_intmax_t], &
            ucobounds = [int(num_imgs, c_intmax_t)], &
            lbounds = [integer(c_intmax_t)::], &
            ubounds = [integer(c_intmax_t)::], &
            element_size = int(storage_size(dummy_element)/8, c_size_t), &
            final_func = c_null_funptr, &
            coarray_handle = current_team%info%child_team_handle, &
            allocated_memory = allocated_memory)
        call c_f_pointer(allocated_memory, current_team%info%child_heap_info)

        if (caf_this_image(current_team%info%gex_team) == 1) then
          ! TODO: Take up remaining space and adjust during (de)allocations
          current_team%info%child_heap_info%size = int(current_team%info%heap_size * 0.1d0, c_size_t)
          current_team%info%child_heap_info%offset = &
              as_int(caf_allocate( &
                  current_team%info%heap_mspace, current_team%info%child_heap_info%size)) &
              - current_team%info%heap_start
        end if
      end block
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
