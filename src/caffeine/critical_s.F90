! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) critical_s
  ! DO NOT ADD USE STATEMENTS HERE
  ! All use statements belong in prif_private_s.F90
  implicit none

contains

  module procedure prif_critical
    call unimplemented("prif_critical")
    
    if (present(stat)) stat = 0
  end procedure

  module procedure prif_end_critical
    call unimplemented("prif_end_critical")
  end procedure

end submodule critical_s
