! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(prif:prif_private_s) coarray_queries_s
  ! DO NOT ADD USE STATEMENTS HERE
  ! All use statements belong in prif_private_s.F90
  implicit none

contains

  module procedure prif_lcobound_with_dim
    call_assert(coarray_handle_check(coarray_handle))
    call_assert(dim >= 1 .and. dim <= coarray_handle%info%corank)

    lcobound = coarray_handle%info%lcobounds(dim)
  end procedure

  module procedure prif_lcobound_no_dim
    call_assert(coarray_handle_check(coarray_handle))

    lcobounds = coarray_handle%info%lcobounds(1:coarray_handle%info%corank)
  end procedure

  module procedure prif_ucobound_with_dim
    call_assert(coarray_handle_check(coarray_handle))
    call_assert(dim >= 1 .and. dim <= coarray_handle%info%corank)

    ucobound = coarray_handle%info%ucobounds(dim)
  end procedure

  module procedure prif_ucobound_no_dim
    call_assert(coarray_handle_check(coarray_handle))

    ucobounds = coarray_handle%info%ucobounds(1:coarray_handle%info%corank)
  end procedure

  module procedure prif_coshape

    call_assert(coarray_handle_check(coarray_handle))

    associate(info => coarray_handle%info)
      sizes = info%ucobounds(1:info%corank) - info%lcobounds(1:info%corank) + 1
    end associate
  end procedure

  module procedure prif_image_index
    integer :: dim, i
    integer(c_int) :: prior_size, num_img
    logical :: invalid_cosubscripts

    call_assert(coarray_handle_check(coarray_handle))

    invalid_cosubscripts = .false.

    check_subscripts: do i = 1, size(sub)
       if (sub(i) .lt. coarray_handle%info%lcobounds(i) .or. sub(i) .gt. coarray_handle%info%ucobounds(i)) then
          invalid_cosubscripts = .true.
          exit check_subscripts
       end if
    end do check_subscripts

    if (.not. invalid_cosubscripts) then
      image_index = 1 + INT(sub(1) - coarray_handle%info%lcobounds(1), c_int)
      prior_size = 1
      ! Future work: values of prior_size are invariant across calls w/ the same coarray_handle
      !  We could store them in the coarray metadata at allocation rather than redundantly
      ! computing them here, which would accelerate calls with corank > 1 by removing
      ! corank multiply/add operations and the loop-carried dependence
      do dim = 2, size(sub)
        prior_size = prior_size * INT(coarray_handle%info%ucobounds(dim-1) - coarray_handle%info%lcobounds(dim-1) + 1, c_int)
        image_index = image_index + INT(sub(dim) - coarray_handle%info%lcobounds(dim), c_int) * prior_size
       end do
    end if

    call prif_num_images(num_images=num_img)
    if (invalid_cosubscripts .or. image_index .gt. num_img) then
       image_index = 0
    end if
  end procedure

  module procedure prif_image_index_with_team
    call_assert(coarray_handle_check(coarray_handle))

    call unimplemented("prif_image_index_with_team")
  end procedure

  module procedure prif_image_index_with_team_number
    call_assert(coarray_handle_check(coarray_handle))

    call unimplemented("prif_image_index_with_team_number")
  end procedure

  module procedure prif_local_data_pointer
    call_assert(coarray_handle_check(coarray_handle))

    local_data = coarray_handle%info%coarray_data
  end procedure

  module procedure prif_set_context_data
    type(c_ptr), pointer :: array_context_data
    call_assert(coarray_handle_check(coarray_handle))

    call c_f_pointer(coarray_handle%info%p_context_data, array_context_data)
    array_context_data = context_data
  end procedure

  module procedure prif_get_context_data
    type(c_ptr), pointer :: array_context_data
    call_assert(coarray_handle_check(coarray_handle))

    call c_f_pointer(coarray_handle%info%p_context_data, array_context_data)
    context_data = array_context_data
  end procedure

  module procedure prif_size_bytes
    call_assert(coarray_handle_check(coarray_handle))

    data_size = coarray_handle%info%coarray_size
  end procedure

end submodule coarray_queries_s
