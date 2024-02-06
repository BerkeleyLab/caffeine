! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(coarray_queries_m) coarray_queries_s

  implicit none

contains

  module procedure prif_lcobound_with_dim
  end procedure

  module procedure prif_lcobound_no_dim
  end procedure

  module procedure prif_ucobound_with_dim
  end procedure

  module procedure prif_ucobound_no_dim
  end procedure prif_ucobound_no_dim

  module procedure prif_coshape
  end procedure

  module procedure prif_image_index
    integer :: dim
    integer(c_int) :: prior_size

    image_index = 1 + sub(1) - coarray_handle%info%lcobounds(1)
    prior_size = 1
    do dim = 2, size(sub)
      prior_size = prior_size * (coarray_handle%info%ucobounds(dim-1) - coarray_handle%info%lcobounds(dim-1) + 1)
      image_index = image_index + (sub(dim) - coarray_handle%info%lcobounds(dim)) * prior_size
    end do
  end procedure

end submodule coarray_queries_s
