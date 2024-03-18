! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) atomic_s

  implicit none

contains

  module procedure prif_atomic_add
    call unimplemented("prif_atomic_add")
  end procedure

  module procedure prif_atomic_and
    call unimplemented("prif_atomic_and")
  end procedure

  module procedure prif_atomic_or
    call unimplemented("prif_atomic_or")
  end procedure

  module procedure prif_atomic_xor
    call unimplemented("prif_atomic_xor")
  end procedure

  module procedure prif_atomic_cas_int
    call unimplemented("prif_atomic_cas_int")
  end procedure

  module procedure prif_atomic_cas_logical
    call unimplemented("prif_atomic_cas_logical")
  end procedure

  module procedure prif_atomic_fetch_add
    call unimplemented("prif_atomic_fetch_add")
  end procedure

  module procedure prif_atomic_fetch_and
    call unimplemented("prif_atomic_fetch_and")
  end procedure

  module procedure prif_atomic_fetch_or
    call unimplemented("prif_atomic_fetch_or")
  end procedure

  module procedure prif_atomic_fetch_xor
    call unimplemented("prif_atomic_fetch_xor")
  end procedure

  module procedure prif_atomic_define_int
    call unimplemented("prif_atomic_define_int")
  end procedure

  module procedure prif_atomic_define_logical
    call unimplemented("prif_atomic_define_logical")
  end procedure

  module procedure prif_atomic_ref_int
    call unimplemented("prif_atomic_ref_int")
  end procedure

  module procedure prif_atomic_ref_logical
    call unimplemented("prif_atomic_ref_logical")
  end procedure

end submodule atomic_s
