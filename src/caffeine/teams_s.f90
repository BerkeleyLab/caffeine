! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(teams_m) teams_s
  use allocation_m, only: prif_allocate
  use caffeine_h_m, only: caf_this_image, caf_allocate, as_int, caf_form_team
  use image_queries_m, only: prif_num_images
  use iso_c_binding, only: c_null_funptr, c_f_pointer

  implicit none

contains

  module procedure prif_change_team
  end procedure

  module procedure prif_end_team
  end procedure

  module procedure prif_form_team
    if (.not.associated(current_team%child_heap_info)) then
      block
        type(c_ptr) :: allocated_memory
        integer :: num_imgs
        type(child_team_info) :: dummy_element

        call prif_num_images(image_count = num_imgs)
        call prif_allocate( &
            lcobounds = [1_c_intmax_t], &
            ucobounds = [int(num_imgs, c_intmax_t)], &
            lbounds = [integer(c_intmax_t)::], &
            ubounds = [integer(c_intmax_t)::], &
            element_length = int(storage_size(dummy_element)/8, c_size_t), &
            final_func = c_null_funptr, &
            coarray_handle = current_team%child_team_handle, &
            allocated_memory = allocated_memory)
        call c_f_pointer(allocated_memory, current_team%child_heap_info)

        if (caf_this_image(current_team%gex_team) == 1) then
          ! TODO: Take up remaining space and adjust during (de)allocations
          current_team%child_heap_info%size = int(current_team%heap_size * 0.1, c_size_t)
          current_team%child_heap_info%offset = &
              as_int(caf_allocate( &
                  current_team%heap_mspace, current_team%child_heap_info%size)) &
              - current_team%heap_start
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

      team%parent_team => current_team
      nullify(team%coarrays)
      nullify(team%child_heap_info)
      call caf_form_team(current_team%gex_team, team%gex_team, team_number, new_index)
    end block
  end procedure

  module procedure prif_get_team
  end procedure

  module procedure prif_team_number
  end procedure

end submodule
