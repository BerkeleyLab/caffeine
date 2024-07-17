! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) atomic_s

  implicit none

contains

  module procedure prif_atomic_add_indirect
    call unimplemented("prif_atomic_add_indirect")
  end procedure

  module procedure prif_atomic_and_indirect
    call unimplemented("prif_atomic_and_indirect")
  end procedure

  module procedure prif_atomic_or_indirect
    call unimplemented("prif_atomic_or_indirect")
  end procedure

  module procedure prif_atomic_xor_indirect
    call unimplemented("prif_atomic_xor_indirect")
  end procedure

  module procedure prif_atomic_cas_int_indirect
    call unimplemented("prif_atomic_cas_int_indirect")
  end procedure

  module procedure prif_atomic_cas_logical_indirect
    call unimplemented("prif_atomic_cas_logical_indirect")
  end procedure

  module procedure prif_atomic_fetch_add_indirect
    call unimplemented("prif_atomic_fetch_add_indirect")
  end procedure

  module procedure prif_atomic_fetch_and_indirect
    call unimplemented("prif_atomic_fetch_and_indirect")
  end procedure

  module procedure prif_atomic_fetch_or_indirect
    call unimplemented("prif_atomic_fetch_or_indirect")
  end procedure

  module procedure prif_atomic_fetch_xor_indirect
    call unimplemented("prif_atomic_fetch_xor_indirect")
  end procedure

  module procedure prif_atomic_define_int_indirect
    call unimplemented("prif_atomic_define_int_indirect")
  end procedure

  module procedure prif_atomic_define_logical_indirect
    call unimplemented("prif_atomic_define_logical_indirect")
  end procedure

  module procedure prif_atomic_ref_int_indirect
    call unimplemented("prif_atomic_ref_int_indirect")
  end procedure

  module procedure prif_atomic_ref_logical_indirect
    call unimplemented("prif_atomic_ref_logical_indirect")
  end procedure

end submodule atomic_s
