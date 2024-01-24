! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(program_startup_m) program_startup_s
  use allocation_m, only: non_symmetric_heap_mspace
  use caffeine_h_m, only : caf_caffeinate
  use teams_m, only: initial_team, current_team

  implicit none
contains

  module procedure prif_init
    call caf_caffeinate( &
        initial_team%heap_mspace, &
        initial_team%heap_start, &
        initial_team%heap_size, &
        non_symmetric_heap_mspace, &
        initial_team%gex_team)
    nullify(initial_team%parent_team)
    nullify(initial_team%coarrays)
    current_team => initial_team
    exit_code = 0
  end procedure

end submodule program_startup_s
