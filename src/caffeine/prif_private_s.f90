! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif) prif_private_s
  implicit none

  type(prif_team_type), target :: default_team
  type(prif_team_type), pointer :: current_team => null()
  type(c_ptr) :: non_symmetric_heap_mspace

end submodule prif_private_s
