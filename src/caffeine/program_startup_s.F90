! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) program_startup_s
  implicit none
contains

  module procedure prif_init
    logical, save :: prif_init_called_previously = .false.

    if (prif_init_called_previously) then
       stat = PRIF_STAT_ALREADY_INIT
    else
       call caf_caffeinate( &
          initial_team%heap_mspace, &
          initial_team%heap_start, &
          non_symmetric_heap_mspace, &
          initial_team%gex_team)
       current_team%info => initial_team
       prif_init_called_previously = .true.
       stat = 0
    end if
  end procedure

end submodule program_startup_s
