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

    associate (info => coarray_handle%info, corank => coarray_handle%info%corank) 
      call_assert(dim >= 1 .and. dim <= corank)

      if (dim < corank) then
        ucobound = info%ucobounds(dim)
      else ! compute trailing ucobound, based on current team size
        call_assert(dim == corank)
        associate (epp => info%coshape_epp(corank), num_imgs => current_team%info%num_images)
          if (epp >= num_imgs) then
            ucobound = info%lcobounds(corank)
          else
            associate (quot => num_imgs / epp, rem => mod(num_imgs, epp))
              ucobound = info%lcobounds(corank) + quot + merge(0,1,rem==0) - 1
            end associate
          end if
        end associate
      end if
    end associate
  end procedure

  module procedure prif_ucobound_no_dim
    call_assert(coarray_handle_check(coarray_handle))

    associate (corank => coarray_handle%info%corank) 
      ucobounds(1:corank-1) = coarray_handle%info%ucobounds(1:corank-1)
      call prif_ucobound_with_dim(coarray_handle, corank, ucobounds(corank))
    end associate
  end procedure

  module procedure prif_coshape
    integer(c_int64_t) :: trailing_ucobound

    call_assert(coarray_handle_check(coarray_handle))

    associate(info => coarray_handle%info, corank => coarray_handle%info%corank)
      sizes(1:corank-1) = info%ucobounds(1:corank-1) - info%lcobounds(1:corank-1) + 1
      call prif_ucobound_with_dim(coarray_handle, corank, trailing_ucobound)
      sizes(corank) = trailing_ucobound - info%lcobounds(corank) + 1
    end associate
  end procedure

  subroutine image_index_helper(coarray_handle, sub, num_images, image_index)
    implicit none
    type(prif_coarray_handle), intent(in) :: coarray_handle
    integer(c_int64_t), intent(in) :: sub(:)
    integer(c_int), intent(in) :: num_images
    integer(c_int), intent(out) :: image_index

    integer :: dim

    call_assert(coarray_handle_check(coarray_handle))

    associate (info => coarray_handle%info, corank => coarray_handle%info%corank) 
      call_assert(size(sub) == corank)
      if (sub(1) .lt. info%lcobounds(1) .or. &
          (corank > 1 .and. sub(1) .gt. info%ucobounds(1))) then
        image_index = 0
        return
      end if
      image_index = 1 + INT(sub(1) - info%lcobounds(1), c_int)
      do dim = 2, size(sub)
        if (sub(dim) .lt. info%lcobounds(dim) .or. &
            (dim < corank .and. sub(dim) .gt. info%ucobounds(dim))) then
          image_index = 0
          return
        end if
        image_index = image_index + INT(sub(dim) - info%lcobounds(dim), c_int) * info%coshape_epp(dim)
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

  !---------------------------------------------------------------------

  subroutine initial_index_helper(coarray_handle, sub, team, initial_team_index)
    implicit none
    type(prif_coarray_handle), intent(in) :: coarray_handle
    integer(c_int64_t), intent(in) :: sub(:)
    type(prif_team_type), intent(in) :: team
    integer(c_int), intent(out) :: initial_team_index

    integer :: dim
    integer(c_int) :: image_index

    call_assert(coarray_handle_check(coarray_handle))

    associate (info => coarray_handle%info, corank => coarray_handle%info%corank)
      call_assert(size(sub) == corank)
      call_assert(sub(1) .ge. info%lcobounds(1) .and. (corank == 1 .or. sub(1) .le. info%ucobounds(1)))
      image_index = 1 + INT(sub(1) - info%lcobounds(1), c_int)
      do dim = 2, size(sub)
        call_assert(sub(dim) .ge. info%lcobounds(dim) .and. (dim == corank .or. sub(dim) .le. info%ucobounds(dim)))
        image_index = image_index + INT(sub(dim) - info%lcobounds(dim), c_int) * info%coshape_epp(dim)
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
