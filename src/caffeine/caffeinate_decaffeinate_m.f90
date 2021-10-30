module caffeinate_decaffeinate_m
  use team_type_m, only: team_type
  implicit none

  private
  public :: caffeinate, decaffeinate, default_team
  interface

    module subroutine caffeinate()
    end subroutine

    module subroutine decaffeinate()
    end subroutine

  end interface

  type(team_type), target :: default_team

end module caffeinate_decaffeinate_m
