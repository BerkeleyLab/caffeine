! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(prif:prif_private_s) image_queries_s
  ! DO NOT ADD USE STATEMENTS HERE
  ! All use statements belong in prif_private_s.F90
  implicit none

contains

  module procedure prif_num_images
    num_images = current_team%info%num_images
  end procedure

  module procedure prif_num_images_with_team
    num_images = team%info%num_images
  end procedure

  module procedure prif_num_images_with_team_number
    if (team_number == -1) then
      num_images = initial_team%num_images 
    else if (team_number == current_team%info%team_number) then
      num_images = current_team%info%num_images
    else
      call unimplemented("prif_num_images_with_team_number: no support for sibling teams")
    end if
  end procedure

  module procedure prif_this_image_no_coarray
    if (present(team)) then
      this_image = team%info%this_image
    else
      this_image = current_team%info%this_image
    endif
  end procedure

  module procedure prif_this_image_with_coarray
    integer(c_int) :: offset, doff, dsz
    integer :: dim

    call_assert(coarray_handle_check(coarray_handle))

    if (present(team)) then
      offset = team%info%this_image - 1
    else
      offset = current_team%info%this_image - 1
    endif

    associate (info => coarray_handle%info)
      call_assert(size(cosubscripts) == info%corank)
      do dim = 1, info%corank-1
        dsz = INT(info%ucobounds(dim) - info%lcobounds(dim) + 1, c_int)
        doff = mod(offset, dsz)
        cosubscripts(dim) = doff + info%lcobounds(dim)
        call_assert(cosubscripts(dim) <= info%ucobounds(dim))
        offset = offset / dsz
      end do
      cosubscripts(info%corank) = offset + info%lcobounds(info%corank)
      call_assert(cosubscripts(info%corank) <= info%ucobounds(info%corank))
    end associate

#   if ASSERTIONS
      block ! sanity check
        integer(c_int) :: image_index
        if (present(team)) then
          call prif_image_index_with_team(coarray_handle, cosubscripts, team, image_index)
          call_assert(image_index == team%info%this_image)
        else
          call prif_image_index(coarray_handle, cosubscripts, image_index)
          call_assert(image_index == current_team%info%this_image)
        end if
      end block
#   endif
  end procedure

  module procedure prif_this_image_with_dim
    call_assert(coarray_handle_check(coarray_handle))

    block
      integer(c_int64_t) :: cosubscripts(coarray_handle%info%corank)

      call_assert(dim >= 1 .and. dim <= coarray_handle%info%corank)

      call prif_this_image_with_coarray(coarray_handle, team, cosubscripts)

      cosubscript = cosubscripts(dim)
    end block
  end procedure

  module procedure prif_failed_images
    ! no current support for detecting image failure
    allocate(failed_images(0))
  end procedure

  module procedure prif_stopped_images
    ! no current support for detecting image stops
    allocate(stopped_images(0))
  end procedure

  module procedure prif_image_status
    ! no current support for detecting image failure/stops
    image_status = 0
  end procedure

end submodule image_queries_s
