! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) critical_s

  implicit none

contains

  module procedure prif_critical
    call unimplemented("prif_critical")
  end procedure

  module procedure prif_end_critical
    call unimplemented("prif_end_critical")
  end procedure

end submodule critical_s
