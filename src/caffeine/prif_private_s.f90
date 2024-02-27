! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif) prif_private_s
  implicit none

  type(prif_team_type), target :: default_team
  type(prif_team_type), pointer :: current_team => null()
  type(c_ptr) :: non_symmetric_heap_mspace

  ! TODO: Do these types need to be public? They are not used as a type of any of the args
  ! for the public procedures

  type :: prif_notify_type
  end type

  type :: prif_lock_type
  end type

  type :: prif_critical_type
  end type

end submodule prif_private_s
