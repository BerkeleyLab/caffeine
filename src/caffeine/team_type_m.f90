! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module team_type_m
  use iso_c_binding, only: c_ptr, c_int, c_intmax_t

  implicit none

  private
  public :: prif_team_type, prif_form_team, current_team, prif_end_team, prif_change_team

  type :: prif_team_type
    type(c_ptr) :: team_ptr
  end type

  type(prif_team_type), pointer :: current_team => null()

  interface

    module subroutine prif_form_team(team_number, team, new_index, stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_intmax_t), intent(in) :: team_number
      type(prif_team_type), intent(out) :: team
      integer(c_int), intent(in), optional :: new_index
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_change_team(team, stat, errmsg, errmsg_alloc)
      implicit none
      type(prif_team_type), intent(in) :: team
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

    module subroutine prif_end_team(stat, errmsg, errmsg_alloc)
      implicit none
      integer(c_int), intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
      character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
    end subroutine

  end interface


end module team_type_m
