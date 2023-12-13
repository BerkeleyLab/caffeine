! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module program_startup_m
  use iso_c_binding, only : c_int
  use team_type_m, only: prif_team_type
  implicit none

  private
  public :: prif_init, default_team

  type(prif_team_type), target :: default_team

  interface

    module subroutine prif_init(exit_code)
      implicit none
      integer(c_int), intent(out) :: exit_code
    end subroutine

  end interface

end module program_startup_m
