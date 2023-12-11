! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module team_type_m
  use iso_c_binding, only: c_ptr

  implicit none

  private
  public :: prif_team_type, prif_form_team, current_team, prif_end_team, prif_change_team

  type :: prif_team_type
    type(c_ptr) :: team_ptr
  end type

  type(prif_team_type), pointer :: current_team => null()

  interface
    module subroutine prif_form_team (num, team, new_index, stat, errmsg)
      integer,          intent(in)  :: num
      type(prif_team_type),  intent(out) :: team
      integer,          intent(in),    optional :: new_index
      integer,          intent(out),   optional :: stat
      character(len=*), intent(inout), optional :: errmsg
    end subroutine

    module subroutine prif_change_team(team)
      type(prif_team_type), target, intent(in) :: team
    end subroutine

    module subroutine prif_end_team()
    end subroutine

  end interface


end module team_type_m
