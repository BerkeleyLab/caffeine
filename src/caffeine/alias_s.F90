! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) alias_s

  implicit none

contains

  module procedure prif_alias_create
    call unimplemented("prif_alias_create")
  end procedure

  module procedure prif_alias_destroy
    call unimplemented("prif_alias_destroy")
  end procedure

end submodule alias_s
