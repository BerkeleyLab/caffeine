! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) image_queries_s

  implicit none

contains

  module procedure prif_num_images
    ! TODO: handle optional args `team` and `team_number`
    image_count = caf_num_images(current_team%gex_team)
  end procedure

  module procedure prif_this_image_no_coarray
    ! TODO: handle optional arg `team`
    image_index = caf_this_image(current_team%gex_team)
  end procedure

  module procedure prif_this_image_with_coarray
    call unimplemented("prif_this_image_with_coarray")
  end procedure

  module procedure prif_this_image_with_dim
    call unimplemented("prif_this_image_with_dim")
  end procedure

  module procedure prif_failed_images
    call unimplemented("prif_failed_images")
  end procedure

  module procedure prif_stopped_images
    call unimplemented("prif_stopped_images")
  end procedure

  module procedure prif_image_status
    call unimplemented("prif_image_status")
  end procedure

end submodule image_queries_s
