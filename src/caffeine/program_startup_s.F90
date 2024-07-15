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
       allocate(current_team)
       call caf_caffeinate(current_team%heap_mspace, current_team%heap_start, non_symmetric_heap_mspace, current_team%gex_team)
       nullify(current_team%parent_team)
       nullify(current_team%coarrays)
       prif_init_called_previously = .true.
       stat = 0
    end if
  end procedure

end submodule program_startup_s
