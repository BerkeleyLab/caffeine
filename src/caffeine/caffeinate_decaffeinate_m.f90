! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module caffeinate_decaffeinate_m
  use team_type_m, only: team_type
  implicit none

  private
  public :: caf_caffeinate, caf_decaffeinate, default_team

  type(team_type), target :: default_team

  interface

    module function caf_caffeinate() result(exit_code)
      implicit none 
      integer exit_code
    end function

    module subroutine caf_decaffeinate(exit_code)
      implicit none
      integer, intent(in) :: exit_code
    end subroutine

  end interface

end module caffeinate_decaffeinate_m
