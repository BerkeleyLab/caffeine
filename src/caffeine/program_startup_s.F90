! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(program_startup_m) program_startup_s
  use caffeine_h_m, only : caf_caffeinate
  use system_state_m, only: symmetric_heap

  implicit none
contains

  module procedure prif_init
    call caf_caffeinate(symmetric_heap)
    exit_code = 0
  end procedure

end submodule program_startup_s
