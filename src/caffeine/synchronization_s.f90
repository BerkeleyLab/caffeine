! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) sychronization_s
  use caffeine_h_m, only : caf_sync_all
  implicit none

contains

  module procedure prif_sync_all
    !TODO: handle optional args stat, errmsg, errmsg_alloc
    call caf_sync_all
  end procedure

  module procedure prif_sync_images
  end procedure

  module procedure prif_sync_team
  end procedure

  module procedure prif_sync_memory
  end procedure

end submodule
