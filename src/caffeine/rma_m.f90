! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module rma_m
  use team_type_m, only: team_type
  implicit none

  private
  public :: caf_put, caf_get

  type caf_co_handle
  end type

  interface

   module subroutine caf_put(coarray, coindices, target, value, team, team_number, stat)
      implicit none
      type(caf_co_handle), intent(in) :: coarray
      integer, intent(in) :: coindices(:)
      type(*), dimension(..), intent(in) :: target, value
      type(team_type), optional, intent(in) :: team
      integer, optional, intent(in) :: team_number
      integer, optional, intent(out) :: stat
    end subroutine

    module subroutine caf_get(coarray, coindices, source, value, team, team_number, stat)
      implicit none
      type(caf_co_handle), intent(in) :: coarray
      integer, intent(in) :: coindices(:)
      type(*), dimension(..), intent(in) :: source
!     type(*), dimension(..), intent(out) :: value ! causes gfortran error: Assumed-type variable value at (1) may not have the INTENT(OUT) attribute
      type(*), dimension(..), intent(inout) :: value
      type(team_type), optional, intent(in) :: team
      integer, optional, intent(in) :: team_number
      integer, optional, intent(out) :: stat
    end subroutine

  end interface

end module rma_m
