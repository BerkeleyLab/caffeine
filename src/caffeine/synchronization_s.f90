! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(synchronization_m) sychronization_s
  implicit none

contains

  module procedure caf_sync_all

    interface 

      subroutine c_sync_all() bind(C)
      end subroutine

    end interface

    call c_sync_all

  end procedure 

end submodule
