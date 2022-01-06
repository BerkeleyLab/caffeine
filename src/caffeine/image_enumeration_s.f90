! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(image_enumeration_m) image_enumeration_s
  use iso_c_binding, only : c_int
  implicit none
 
contains

  module procedure num_images_team
    
    interface
      integer(c_int) function c_num_images() bind(C)
        import c_int
      end function
    end interface
    
    image_count = c_num_images() 
  end procedure

  module procedure num_images_team_number
  end procedure

  module procedure this_image_team
    
    interface
      integer(c_int) function c_this_image() bind(C)
        import c_int
      end function
    end interface
    
    image_number = c_this_image()
  end procedure

  module procedure this_image_coarray_team
  end procedure

  module procedure this_image_coarray_dim_team
  end procedure

end submodule image_enumeration_s
