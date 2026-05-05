! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(prif:prif_private_s) coarray_queries_s
  ! DO NOT ADD USE STATEMENTS HERE
  ! All use statements belong in prif_private_s.F90
  implicit none

contains

  module procedure prif_lcobound_with_dim
    type(prif_coarray_descriptor), pointer :: cdp

    call_assert(coarray_handle_check(coarray_handle))
    cdp => handle_to_cdp(coarray_handle)
    call_assert(dim >= 1 .and. dim <= cdp%corank)

    lcobound = cdp%lcobounds(dim)
  end procedure

  module procedure prif_lcobound_no_dim
    type(prif_coarray_descriptor), pointer :: cdp

    call_assert(coarray_handle_check(coarray_handle))
    cdp => handle_to_cdp(coarray_handle)
    call_assert(size(lcobounds) == cdp%corank)

    lcobounds = cdp%lcobounds(1:size(lcobounds))
  end procedure

  module procedure prif_ucobound_with_dim
    type(prif_coarray_descriptor), pointer :: cdp

    call_assert(coarray_handle_check(coarray_handle))
    call_assert(team_check(current_team))

    cdp => handle_to_cdp(coarray_handle)
    associate (corank => cdp%corank) 
      call_assert(dim >= 1 .and. dim <= corank)

      if (corank == 1) then ! common-case optimization
        ucobound = cdp%lcobounds(1) + current_team%info%num_images - 1
      elseif (dim < corank) then
        ucobound = cdp%ucobounds(dim)
      else ! compute trailing ucobound, based on current team size
        call_assert(dim == corank)
        associate (epp => cdp%coshape_epp(corank), num_imgs => current_team%info%num_images)
          if (epp >= num_imgs) then ! optimization to skip a divide
            ucobound = cdp%lcobounds(corank)
          else
            ucobound = cdp%lcobounds(corank) + (num_imgs + epp - 1) / epp - 1
          end if
        end associate
      end if
    end associate
  end procedure

  module procedure prif_ucobound_no_dim
    type(prif_coarray_descriptor), pointer :: cdp

    call_assert(coarray_handle_check(coarray_handle))

    cdp => handle_to_cdp(coarray_handle)
    associate (corank => size(ucobounds)) 
      call_assert(corank == cdp%corank)
      ucobounds(1:corank-1) = cdp%ucobounds(1:corank-1)
      call prif_ucobound_with_dim(coarray_handle, corank, ucobounds(corank))
    end associate
  end procedure

  module procedure prif_coshape
    type(prif_coarray_descriptor), pointer :: cdp
    integer :: corank

    call_assert(coarray_handle_check(coarray_handle))
    call_assert(team_check(current_team))

    cdp => handle_to_cdp(coarray_handle)
    corank = size(sizes)
    call_assert(corank == cdp%corank)
    if (corank == 1) then ! common-case optimization
      sizes(1) = current_team%info%num_images
    else
      sizes(1:corank-1) = cdp%ucobounds(1:corank-1) - cdp%lcobounds(1:corank-1) + 1
      associate (epp => cdp%coshape_epp(corank), num_imgs => current_team%info%num_images)
        if (epp >= num_imgs) then ! optimization to skip a divide
          sizes(corank) = 1
        else
          sizes(corank) = (num_imgs + epp - 1) / epp
        end if
      end associate
    end if
  end procedure

  subroutine image_index_helper(coarray_handle, sub, team, image_index)
    implicit none
    type(prif_coarray_handle), intent(in) :: coarray_handle
    integer(c_int64_t), intent(in) :: sub(:)
    type(prif_team_type), intent(in) :: team
    integer(c_int), intent(out) :: image_index

    type(prif_coarray_descriptor), pointer :: cdp
    integer :: dim

    call_assert(coarray_handle_check(coarray_handle))
    call_assert(team_check(team))

    cdp => handle_to_cdp(coarray_handle)

    associate (corank => size(sub)) 
      call_assert(corank == cdp%corank)
      if (sub(1) .lt. cdp%lcobounds(1) .or. &
          (corank > 1 .and. sub(1) .gt. cdp%ucobounds(1))) then
        image_index = 0
        return
      end if
      image_index = 1 + INT(sub(1) - cdp%lcobounds(1), c_int)
      do dim = 2, size(sub)
        if (sub(dim) .lt. cdp%lcobounds(dim) .or. &
            (dim < corank .and. sub(dim) .gt. cdp%ucobounds(dim))) then
          image_index = 0
          return
        end if
        image_index = image_index + INT(sub(dim) - cdp%lcobounds(dim), c_int) * cdp%coshape_epp(dim)
       end do
    end associate

    if (image_index .gt. team%info%num_images) then
       image_index = 0
    end if
  end subroutine

  module procedure prif_image_index
    call image_index_helper(coarray_handle, sub, current_team, image_index)
  end procedure

  module procedure prif_image_index_with_team
    call image_index_helper(coarray_handle, sub, team, image_index)
  end procedure

  module procedure prif_image_index_with_team_number
    call_assert(team_check(current_team))
    if (team_number == -1) then
      call image_index_helper(coarray_handle, sub, prif_team_type(initial_team), image_index)
    else if (team_number == current_team%info%team_number) then
      call image_index_helper(coarray_handle, sub, current_team, image_index)
    else
      call unimplemented("prif_image_index_with_team_number: no support for sibling teams")
    end if 
  end procedure

  !---------------------------------------------------------------------

  subroutine initial_index_helper(coarray_handle, sub, team, initial_team_index)
    implicit none
    type(prif_coarray_handle), intent(in) :: coarray_handle
    integer(c_int64_t), intent(in) :: sub(:)
    type(prif_team_type), intent(in) :: team
    integer(c_int), intent(out) :: initial_team_index

    type(prif_coarray_descriptor), pointer :: cdp
    integer :: dim
    integer(c_int) :: image_index

    call_assert(team_check(team))
    call_assert(coarray_handle_check(coarray_handle))

    cdp => handle_to_cdp(coarray_handle)
    associate (corank => size(sub)) 
      call_assert(corank == cdp%corank)
      call_assert(sub(1) .ge. cdp%lcobounds(1) .and. (corank == 1 .or. sub(1) .le. cdp%ucobounds(1)))
      image_index = 1 + INT(sub(1) - cdp%lcobounds(1), c_int)
      do dim = 2, size(sub)
        call_assert(sub(dim) .ge. cdp%lcobounds(dim) .and. (dim == corank .or. sub(dim) .le. cdp%ucobounds(dim)))
        image_index = image_index + INT(sub(dim) - cdp%lcobounds(dim), c_int) * cdp%coshape_epp(dim)
       end do
    end associate

    call_assert(image_index .le. team%info%num_images)
    initial_team_index = caf_image_to_initial(team%info%gex_team, image_index)
    call_assert(initial_team_index .ge. 1 .and. initial_team_index .le. initial_team%num_images)
  end subroutine

  module procedure prif_initial_team_index
    call initial_index_helper(coarray_handle, sub, current_team, initial_team_index)

    if (present(stat)) stat = 0
  end procedure

  module procedure prif_initial_team_index_with_team
    call initial_index_helper(coarray_handle, sub, team, initial_team_index)

    if (present(stat)) stat = 0
  end procedure

  module procedure prif_initial_team_index_with_team_number
    if (team_number == -1) then
      call initial_index_helper(coarray_handle, sub, prif_team_type(initial_team), initial_team_index)
    else if (team_number == current_team%info%team_number) then
      call initial_index_helper(coarray_handle, sub, current_team, initial_team_index)
    else
      call unimplemented("prif_initial_team_index_with_team_number: no support for sibling teams")
    end if

    if (present(stat)) stat = 0
  end procedure

  !---------------------------------------------------------------------

  module procedure prif_local_data_pointer
    type(prif_coarray_descriptor), pointer :: cdp

    call_assert(coarray_handle_check(coarray_handle))
    cdp => handle_to_cdp(coarray_handle)

    local_data = cdp%coarray_data
  end procedure

  module procedure prif_set_context_data
    type(c_ptr), pointer :: array_context_data
    type(prif_coarray_descriptor), pointer :: cdp

    call_assert(coarray_handle_check(coarray_handle))
    cdp => handle_to_cdp(coarray_handle)

    call c_f_pointer(cdp%p_context_data, array_context_data)
    array_context_data = context_data
  end procedure

  module procedure prif_get_context_data
    type(c_ptr), pointer :: array_context_data
    type(prif_coarray_descriptor), pointer :: cdp

    call_assert(coarray_handle_check(coarray_handle))
    cdp => handle_to_cdp(coarray_handle)

    call c_f_pointer(cdp%p_context_data, array_context_data)
    context_data = array_context_data
  end procedure

  module procedure prif_size_bytes
    type(prif_coarray_descriptor), pointer :: cdp

    call_assert(coarray_handle_check(coarray_handle))
    cdp => handle_to_cdp(coarray_handle)

    data_size = cdp%coarray_size
  end procedure

end submodule coarray_queries_s
