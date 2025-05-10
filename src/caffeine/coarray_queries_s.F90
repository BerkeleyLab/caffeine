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

  subroutine image_index_helper(coarray_handle, sub, num_images, image_index)
    implicit none
    type(prif_coarray_handle), intent(in) :: coarray_handle
    integer(c_int64_t), intent(in) :: sub(:)
    integer(c_int), intent(in) :: num_images
    integer(c_int), intent(out) :: image_index

    integer :: dim
    integer(c_int) :: prior_size

    call_assert(coarray_handle_check(coarray_handle))

    associate (info => coarray_handle%info) 
      call_assert(size(sub) == info%corank)
      if (sub(1) .lt. info%lcobounds(1) .or. sub(1) .gt. info%ucobounds(1)) then
        image_index = 0
        return
      end if
      image_index = 1 + INT(sub(1) - info%lcobounds(1), c_int)
      prior_size = 1
      ! Future work: values of prior_size are invariant across calls w/ the same coarray_handle
      !  We could store them in the coarray metadata at allocation rather than redundantly
      ! computing them here, which would accelerate calls with corank > 1 by removing
      ! corank multiply/add operations and the loop-carried dependence
      do dim = 2, size(sub)
        prior_size = prior_size * INT(info%ucobounds(dim-1) - info%lcobounds(dim-1) + 1, c_int)
        if (sub(dim) .lt. info%lcobounds(dim) .or. sub(dim) .gt. info%ucobounds(dim)) then
          image_index = 0
          return
        end if
        image_index = image_index + INT(sub(dim) - info%lcobounds(dim), c_int) * prior_size
       end do
    end associate

    if (image_index .gt. num_images) then
       image_index = 0
    end if
  end subroutine

  module procedure prif_image_index
    call image_index_helper(coarray_handle, sub, current_team%info%num_images, image_index)
  end procedure

  module procedure prif_image_index_with_team
    call image_index_helper(coarray_handle, sub, team%info%num_images, image_index)
  end procedure

  module procedure prif_image_index_with_team_number
    if (team_number == -1) then
      call image_index_helper(coarray_handle, sub, initial_team%num_images, image_index)
    else if (team_number == current_team%info%team_number) then
      call image_index_helper(coarray_handle, sub, current_team%info%num_images, image_index)
    else
      call unimplemented("prif_image_index_with_team_number: no support for sibling teams")
    end if 
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
