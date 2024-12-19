! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) co_sum_s

  implicit none

contains

  module procedure prif_co_sum
    call unimplemented("prif_co_sum")
  end procedure

end submodule co_sum_s
