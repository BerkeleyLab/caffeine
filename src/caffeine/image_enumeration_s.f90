! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(image_enumeration_m) image_enumeration_s
  use caffeine_h_m, only : caf_c_num_images, caf_c_this_image
  implicit none
 
contains

  module procedure num_images_team
    image_count = caf_c_num_images() 
  end procedure

  module procedure num_images_team_number
  end procedure

  module procedure this_image_team
    image_number = caf_c_this_image()
  end procedure

  module procedure this_image_coarray_team
  end procedure

  module procedure this_image_coarray_dim_team
  end procedure

end submodule image_enumeration_s
