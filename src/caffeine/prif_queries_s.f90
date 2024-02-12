! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif_queries_m) prif_queries_s
  use caffeine_assert_m, only: assert
  use caffeine_h_m, only: caf_convert_base_addr
  use image_queries_m, only: prif_num_images
  use iso_c_binding, only: c_null_ptr
  use teams_m, only: current_team

  implicit none

contains

  module procedure prif_set_context_data
  end procedure

  module procedure prif_get_context_data
  end procedure

  module procedure prif_base_pointer
    integer(c_int) :: num_img

    call prif_num_images(image_count=num_img)
    call assert(image_num .gt. 0 .or. image_num .lt. num_img, "prif_base_pointer: image_num not within valid range")
    if (image_num .eq. 0) then
      ptr = 0
    else
      ! TODO: after more team work, replace last arg with initial_team%gex_team
      ptr = caf_convert_base_addr(coarray_handle%info%coarray_data, image_num, current_team%gex_team)
    end if
  end procedure

  module procedure prif_local_data_size
  end procedure

end submodule prif_queries_s
