! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) co_min_s
  use iso_c_binding, only : c_funloc

  implicit none

contains

  module procedure prif_co_min
    call unimplemented("prif_co_min")
  end procedure

end submodule co_min_s
