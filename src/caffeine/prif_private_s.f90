! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif) prif_private_s
  use teams_m, only: prif_team_type
  implicit none

  type(prif_team_type), target :: default_team

  ! TODO: Do these types need to be public? They are not used as a type of any of the args
  ! for the public procedures

  type :: prif_notify_type
  end type

  type :: prif_lock_type
  end type

  type :: prif_critical_type
  end type

end submodule prif_private_s
