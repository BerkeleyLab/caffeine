! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(coarray_access_m) coarray_access_s
  use caffeine_h_m, only: caf_put, caf_get
  use coarray_queries_m, only: prif_image_index
  use teams_m, only: current_team

  implicit none

contains

  module procedure prif_put
    integer(c_int) :: image

    call prif_image_index(coarray_handle, coindices, image_index=image)
    call caf_put( &
        team = current_team%gex_team, &
        image = image, &
        dest = first_element_addr, &
        src = value)
  end procedure

  module procedure prif_put_raw
  end procedure

  module procedure prif_put_raw_strided
  end procedure

  module procedure prif_get
    integer(c_int) :: image

    call prif_image_index(coarray_handle, coindices, image_index=image)
    call caf_get( &
        team = current_team%gex_team, &
        image = image, &
        dest = value, &
        src = first_element_addr)
  end procedure

  module procedure prif_get_raw
  end procedure

  module procedure prif_get_raw_strided
  end procedure

end submodule coarray_access_s
