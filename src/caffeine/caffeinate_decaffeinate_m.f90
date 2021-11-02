module caffeinate_decaffeinate_m
  use team_type_m, only: team_type
  implicit none

  private
  public :: caffeinate, decaffeinate, default_team
  interface

    module function caffeinate() result(exit_code)
      implicit none 
      integer exit_code
    end function

    module subroutine decaffeinate()
      implicit none
    end subroutine

  end interface

  type(team_type), target :: default_team

end module caffeinate_decaffeinate_m
