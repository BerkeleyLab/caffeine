! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) prif_queries_s

  implicit none

contains

  module procedure prif_set_context_data
    call unimplemented("prif_set_context_data")
  end procedure

  module procedure prif_get_context_data
    call unimplemented("prif_get_context_data")
  end procedure

  module procedure prif_base_pointer
    integer(c_int) :: num_img

    call prif_num_images(num_images=num_img)
    call assert(image_num .ge. 0 .and. image_num .le. num_img, "prif_base_pointer: image_num not within valid range")
    if (image_num .eq. 0) then
      ptr = 0
    else
      ptr = caf_convert_base_addr(coarray_handle%info%coarray_data, image_num)
    end if
  end procedure

  module procedure prif_size_bytes
    call unimplemented("prif_size_bytes")
  end procedure

end submodule prif_queries_s
