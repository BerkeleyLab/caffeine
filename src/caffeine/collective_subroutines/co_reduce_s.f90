! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) co_reduce_s
  use iso_c_binding, only : &
    c_loc, c_funloc, c_associated, c_f_pointer, c_f_procpointer, c_char, c_double, &
    c_float, c_int32_t

  implicit none

contains

  module procedure prif_co_reduce
    call unimplemented("prif_co_reduce")
  end procedure

end submodule co_reduce_s
