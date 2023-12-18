! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(image_queries_m) image_queries_s
  use caffeine_h_m, only : caf_num_images, caf_this_image
  implicit none

contains

  module procedure prif_num_images
    ! TODO: handle optional args `team` and `team_number`
    image_count = caf_num_images()
  end procedure

  module procedure prif_this_image_no_coarray
    ! TODO: handle optional arg `team`
    image_index = caf_this_image()
  end procedure

  module procedure prif_this_image_with_coarray
  end procedure

  module procedure prif_this_image_with_dim
  end procedure

  module procedure prif_failed_images
  end procedure

  module procedure prif_stopped_images
  end procedure

end submodule image_queries_s
