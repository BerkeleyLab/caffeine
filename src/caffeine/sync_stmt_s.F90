! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) sync_stmt_s

  implicit none

contains

  module procedure prif_sync_all
    call caf_sync_team(current_team%info%gex_team)
    if (present(stat)) stat = 0
  end procedure

  module procedure prif_sync_images
    call unimplemented("prif_sync_images")
  end procedure

  module procedure prif_sync_team
    call caf_sync_team(team%info%gex_team)
    if (present(stat)) stat = 0
  end procedure

  module procedure prif_sync_memory
    call caf_sync_memory
    if (present(stat)) stat = 0
  end procedure

end submodule
