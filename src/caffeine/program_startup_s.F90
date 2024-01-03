! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(program_startup_m) program_startup_s
  use iso_c_binding, only : c_loc, c_char, c_null_char, c_bool
  use synchronization_m, only : prif_sync_all
  use caffeine_h_m, only : caf_caffeinate, caf_decaffeinate
  use program_termination_m, only: prif_error_stop
  implicit none

contains

  module procedure prif_init
    call caf_caffeinate()
  end procedure

end submodule program_startup_s
