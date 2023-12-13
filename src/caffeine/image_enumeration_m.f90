! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module image_enumeration_m
  use iso_c_binding, only: c_int, c_intmax_t
  use team_type_m, only : prif_team_type
  use allocation_m, only: prif_coarray_handle
  implicit none

  private
  public :: prif_num_images
  public :: prif_this_image

  interface

     module subroutine prif_num_images(team, team_number, image_count)
       implicit none
       type(prif_team_type), intent(in), optional :: team
       integer(c_intmax_t), intent(in), optional :: team_number
       integer(c_int), intent(out) :: image_count
     end subroutine prif_num_images

  end interface

  interface prif_this_image

    pure module subroutine prif_this_image_no_coarray(team, image_index)
      implicit none
      type(prif_team_type), intent(in), optional :: team
      integer(c_int), intent(out) :: image_index
    end subroutine

    module subroutine prif_this_image_with_coarray(coarray_handle, team, cosubscripts)
      implicit none
      type(prif_coarray_handle), intent(in) :: coarray_handle
      type(prif_team_type), intent(in), optional :: team
      integer(c_intmax_t), intent(out) :: cosubscripts(:)
    end subroutine

    module subroutine prif_this_image_with_dim(coarray_handle, dim, team, cosubscript)
      implicit none
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_int), intent(in) :: dim
      type(prif_team_type), intent(in), optional :: team
      integer(c_intmax_t), intent(out) :: cosubscript
    end subroutine

  end interface

end module image_enumeration_m
