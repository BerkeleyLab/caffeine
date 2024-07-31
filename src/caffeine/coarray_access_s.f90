! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) coarray_access_s
  use iso_c_binding, only: c_loc

  implicit none

contains

  module procedure prif_put
    integer(c_intptr_t) :: remote_base

    call base_pointer(coarray_handle, image_num, remote_base)
    call prif_put_indirect( &
        image_num = image_num, &
        remote_ptr = remote_base + offset, &
        current_image_buffer = current_image_buffer, &
        size_in_bytes = size_in_bytes)
  end procedure

  module procedure prif_put_indirect
    call caf_put( &
        image = image_num, &
        dest = remote_ptr, &
        src = current_image_buffer, &
        size = size_in_bytes)
  end procedure

  module procedure prif_put_with_notify
    call unimplemented("prif_put_with_notify")
  end procedure

  module procedure prif_put_with_notify_indirect
    call unimplemented("prif_put_with_notify_indirect")
  end procedure

  module procedure prif_put_indirect_with_notify
    call unimplemented("prif_put_indirect_with_notify")
  end procedure

  module procedure prif_put_indirect_with_notify_indirect
    call unimplemented("prif_put_indirect_with_notify_indirect")
  end procedure

  module procedure prif_get
    integer(c_intptr_t) :: remote_base

    call base_pointer(coarray_handle, image_num, remote_base)
    call prif_get_indirect( &
        image_num = image_num, &
        remote_ptr = remote_base + offset, &
        current_image_buffer = current_image_buffer, &
        size_in_bytes = size_in_bytes, &
        stat = stat, &
        errmsg = errmsg, &
        errmsg_alloc = errmsg_alloc)
  end procedure

  module procedure prif_get_indirect
    call caf_get( &
        image = image_num, &
        dest = current_image_buffer, &
        src = remote_ptr, &
        size = size_in_bytes)
  end procedure

  module procedure prif_get_strided
    call unimplemented("prif_get_strided")
  end procedure

  module procedure prif_get_strided_indirect
    call unimplemented("prif_get_strided_indirect")
  end procedure

end submodule coarray_access_s
