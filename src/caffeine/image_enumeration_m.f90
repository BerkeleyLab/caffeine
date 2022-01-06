! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module image_enumeration_m
  use team_type_m, only : team_type
  implicit none

  private
  public :: caf_num_images
  public :: caf_this_image
 
  interface caf_num_images

    module function num_images_team(team) result(image_count) 
      implicit none
      type(team_type), intent(in), optional :: team
      integer image_count
    end function

    module function num_images_team_number(team_number) result(image_count) 
      implicit none
      integer, intent(in) :: team_number
      integer image_count
    end function

  end interface

  interface caf_this_image

    module function this_image_team(team) result(image_number)
      implicit none
      type(team_type), intent(in), optional :: team
      integer image_number
    end function

    module function this_image_coarray_team(coarray, team) result(image_number)
      implicit none
      type(team_type), intent(in), optional :: team
      class(*), intent(in) :: coarray(..)
      integer image_number
    end function

    module function this_image_coarray_dim_team(coarray, dim, team) result(image_number)
      implicit none
      class(*), intent(in) :: coarray(..)
      integer, intent(in) :: dim
      type(team_type), intent(in), optional :: team
      integer image_number
    end function

  end interface

end module image_enumeration_m
