! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(program_startup_m) program_startup_s
  use allocation_m, only: non_symmetric_heap_mspace
  use caffeine_h_m, only : caf_caffeinate
  use teams_m, only: current_team

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
