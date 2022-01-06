! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module team_type_m
  implicit none

  private
  public :: team_type, caf_form_team, current_team, caf_end_team, caf_change_team

  type team_type
  end type

  type(team_type), pointer :: current_team => null()

  interface
    module subroutine caf_form_team (num, team, new_index, stat, errmsg) 
      integer,          intent(in)  :: num
      type(team_type),  intent(out) :: team
      integer,          intent(in),    optional :: new_index
      integer,          intent(out),   optional :: stat
      character(len=*), intent(inout), optional :: errmsg
    end subroutine

    module subroutine caf_change_team(team) 
      type(team_type), target, intent(in) :: team
    end subroutine

    module subroutine caf_end_team() 
    end subroutine

  end interface


end module team_type_m

