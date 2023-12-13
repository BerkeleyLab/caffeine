! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(image_enumeration_m) image_enumeration_s
  use caffeine_h_m, only : caf_num_images, caf_this_image
  implicit none

contains

  module procedure num_images_team
    image_count = caf_num_images()
  end procedure

  module procedure num_images_team_number
  end procedure

  module procedure prif_this_image_no_coarray
    ! TODO: handle optional arg `team`
    image_index = caf_this_image()
  end procedure

  module procedure prif_this_image_with_coarray
  end procedure

  module procedure prif_this_image_with_dim
  end procedure

end submodule image_enumeration_s
