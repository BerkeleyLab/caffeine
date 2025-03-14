! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(prif:prif_private_s) image_queries_s

  implicit none

contains

  module procedure prif_num_images
    num_images = current_team%info%num_images
  end procedure

  module procedure prif_num_images_with_team
    num_images = team%info%num_images
  end procedure

  module procedure prif_num_images_with_team_number
    call unimplemented("prif_num_images_with_team_number")
  end procedure

  module procedure prif_this_image_no_coarray
    if (present(team)) then
      this_image = team%info%this_image
    else
      this_image = current_team%info%this_image
    endif
  end procedure

  module procedure prif_this_image_with_coarray
    call_assert(coarray_handle_check(coarray_handle))

    call unimplemented("prif_this_image_with_coarray")
  end procedure

  module procedure prif_this_image_with_dim
    call_assert(coarray_handle_check(coarray_handle))

    call unimplemented("prif_this_image_with_dim")
  end procedure

  module procedure prif_failed_images
    ! no current support for detecting image failure
    allocate(failed_images(0))
  end procedure

  module procedure prif_stopped_images
    ! no current support for detecting image stops
    allocate(stopped_images(0))
  end procedure

  module procedure prif_image_status
    ! no current support for detecting image failure/stops
    image_status = 0
  end procedure

end submodule image_queries_s
