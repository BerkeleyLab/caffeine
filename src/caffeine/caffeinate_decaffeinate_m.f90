module caffeinate_decaffeinate_m
  use iso_c_binding, only : c_int, c_ptr
  use team_type_m, only: team_type
  implicit none

  private
  public :: caffeinate, decaffeinate, default_team
  interface

    module subroutine caffeinate(argc, argv)
      implicit none 
      integer(c_int), value :: argc
      type(c_ptr) argv(*)
    end subroutine

    module subroutine decaffeinate()
      implicit none
    end subroutine

  end interface

  type(team_type), target :: default_team

end module caffeinate_decaffeinate_m
