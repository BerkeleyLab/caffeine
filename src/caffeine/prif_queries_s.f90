! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) prif_queries_s

  implicit none

contains

  module procedure prif_set_context_data
    call unimplemented("prif_set_context_data")
  end procedure

  module procedure prif_get_context_data
    call unimplemented("prif_get_context_data")
  end procedure

  module procedure prif_size_bytes
    call unimplemented("prif_size_bytes")
  end procedure

end submodule prif_queries_s
