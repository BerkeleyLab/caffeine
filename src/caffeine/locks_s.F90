! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) locks_s
  ! DO NOT ADD USE STATEMENTS HERE
  ! All use statements belong in prif_private_s.F90
  implicit none

contains

  module procedure prif_lock
    call unimplemented("prif_lock")
  end procedure

  module procedure prif_lock_indirect
    call unimplemented("prif_lock_indirect")
  end procedure

  module procedure prif_unlock
    call unimplemented("prif_unlock")
  end procedure

  module procedure prif_unlock_indirect
    call unimplemented("prif_unlock_indirect")
  end procedure

end submodule locks_s
