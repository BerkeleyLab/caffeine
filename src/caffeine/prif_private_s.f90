! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif) prif_private_s
  implicit none

  type(prif_team_type), target :: default_team
  type(prif_team_type), pointer :: current_team => null()
  type(c_ptr) :: non_symmetric_heap_mspace

contains

  subroutine unimplemented(proc_name)
    character(len=*), intent(in) ::  proc_name
    error stop proc_name // " is not yet implemented"
  end subroutine

end submodule prif_private_s
