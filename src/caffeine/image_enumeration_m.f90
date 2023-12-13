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

  interface prif_num_images

    module function num_images_team(team) result(image_count)
      implicit none
      type(prif_team_type), intent(in), optional :: team
      integer image_count
    end function

    module function num_images_team_number(team_number) result(image_count)
      implicit none
      integer, intent(in) :: team_number
      integer image_count
    end function

  end interface

  interface prif_this_image
    pure module subroutine prif_this_image_no_coarray(team, image_index)
      implicit none
      type(prif_team_type), intent(in), optional :: team
      integer(c_int), intent(out) :: image_index
    end subroutine

    module subroutine prif_this_image_with_coarray(team, coarray_handle, cosubscripts)
      implicit none
      type(prif_team_type), intent(in), optional :: team
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_intmax_t), intent(out) :: cosubscripts(:)
    end subroutine

    module subroutine prif_this_image_with_dim(team, coarray_handle, dim, cosubscript)
      implicit none
      type(prif_team_type), intent(in), optional :: team
      type(prif_coarray_handle), intent(in) :: coarray_handle
      integer(c_int), intent(in) :: dim
      integer(c_intmax_t), intent(out) :: cosubscript
    end subroutine
  end interface

end module image_enumeration_m
