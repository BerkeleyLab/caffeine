! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) program_startup_s
  use caffeine_h_m, only : caf_caffeinate

  implicit none
contains

  module procedure prif_init
    allocate(current_team)
    call caf_caffeinate(current_team%heap_mspace, current_team%heap_start, non_symmetric_heap_mspace, current_team%gex_team)
    nullify(current_team%parent_team)
    nullify(current_team%coarrays)
    exit_code = 0
  end procedure

end submodule program_startup_s
