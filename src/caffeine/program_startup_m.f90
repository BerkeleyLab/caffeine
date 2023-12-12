! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module program_startup_m
  use team_type_m, only: prif_team_type
  implicit none

  private
  public :: prif_init, default_team

  type(prif_team_type), target :: default_team

  interface

    module function prif_init() result(exit_code)
      implicit none 
      integer exit_code
    end function

  end interface

end module program_startup_m
