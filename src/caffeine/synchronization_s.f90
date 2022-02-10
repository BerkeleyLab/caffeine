! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(synchronization_m) sychronization_s
  use caffeine_h_m, only : caf_c_sync_all
  implicit none

contains

  module procedure caf_sync_all

    call caf_c_sync_all

  end procedure 

end submodule
