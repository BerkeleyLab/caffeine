! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif) prif_private_s
  use teams_m, only: prif_team_type
  implicit none

  type(prif_team_type), target :: default_team

end submodule prif_private_s
