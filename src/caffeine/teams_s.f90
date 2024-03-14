! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) teams_s
  implicit none

contains

  module procedure prif_change_team
    call unimplemented("prif_change_team")
  end procedure

  module procedure prif_end_team
    call unimplemented("prif_end_team")
  end procedure

  module procedure prif_form_team
    call unimplemented("prif_form_team")
  end procedure

  module procedure prif_get_team
    call unimplemented("prif_get_team")
  end procedure

  module procedure prif_team_number
    call unimplemented("prif_team_number")
  end procedure

end submodule
