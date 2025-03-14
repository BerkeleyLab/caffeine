! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(prif:prif_private_s) alias_s

  implicit none

contains

  module procedure prif_alias_create
    call_assert(coarray_handle_check(source_handle))

    call unimplemented("prif_alias_create")
  end procedure

  module procedure prif_alias_destroy
    call_assert(coarray_handle_check(alias_handle))

    call unimplemented("prif_alias_destroy")
  end procedure

end submodule alias_s
