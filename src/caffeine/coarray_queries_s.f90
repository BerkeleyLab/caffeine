! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(coarray_queries_m) coarray_queries_s
  use image_queries_m, only: prif_num_images

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
    integer :: dim, i
    integer(c_int) :: prior_size, num_img
    logical :: invalid_cosubscripts

    invalid_cosubscripts = .false.

    check_subscripts: do i = 1, size(sub)
       if (sub(i) .lt. coarray_handle%info%lcobounds(i) .or. sub(i) .gt. coarray_handle%info%ucobounds(i)) then
          invalid_cosubscripts = .true.
          exit check_subscripts
       end if
    end do check_subscripts

    if (.not. invalid_cosubscripts) then
      image_index = 1 + sub(1) - coarray_handle%info%lcobounds(1)
      prior_size = 1
      do dim = 2, size(sub)
        prior_size = prior_size * (coarray_handle%info%ucobounds(dim-1) - coarray_handle%info%lcobounds(dim-1) + 1)
        image_index = image_index + (sub(dim) - coarray_handle%info%lcobounds(dim)) * prior_size
       end do
    end if

    call prif_num_images(image_count=num_img)
    if (invalid_cosubscripts .or. image_index .gt. num_img) then
       image_index = 0
    end if
  end procedure

end submodule coarray_queries_s
