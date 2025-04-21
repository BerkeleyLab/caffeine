! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(prif:prif_private_s) sync_stmt_s
  ! DO NOT ADD USE STATEMENTS HERE
  ! All use statements belong in prif_private_s.F90
  implicit none

  ! Data structures used to implement prif_sync_images
  type(prif_coarray_handle) :: si_coarray_handle
  type(prif_event_type), pointer :: si_evt(:)
  integer(c_size_t) :: sizeof_event

contains

  module procedure prif_sync_all
    call caf_sync_team(current_team%info%gex_team)
    if (present(stat)) stat = 0
  end procedure

  module procedure prif_sync_team
    call caf_sync_team(team%info%gex_team)
    if (present(stat)) stat = 0
  end procedure

  module procedure prif_sync_memory
    call caf_sync_memory
    if (present(stat)) stat = 0
  end procedure

  module procedure sync_init
    ! Create the array coarray used to implement prif_sync_images
    ! This following is effectively: 
    !   type(EVENT_TYPE), allocatable :: si_evt(:)[*]
    !   ALLOCATE( si_evt(NUM_IMAGES()) )
    type(prif_event_type) :: dummy_event
    type(c_ptr) :: allocated_memory

    associate(num_imgs => initial_team%num_images) 

      sizeof_event = int(storage_size(dummy_event)/8, c_size_t)

      call prif_allocate_coarray( &
            lcobounds = [1_c_int64_t], &
            ucobounds = [int(num_imgs,c_int64_t)], &
            size_in_bytes = sizeof_event * num_imgs, &
            final_func = c_null_funptr, &
            coarray_handle = si_coarray_handle, &
            allocated_memory = allocated_memory)
      call c_f_pointer(allocated_memory, si_evt, [num_imgs])
      si_evt = dummy_event ! default initialize

   end associate
    
  end procedure

  module procedure prif_sync_images
    integer(c_int) :: i, img, l, u
    integer(c_intptr_t) :: evt_ptr

    call_assert(coarray_handle_check(si_coarray_handle))

    call caf_sync_memory ! end segment and amortize release fence

    associate(num_imgs => current_team%info%num_images) 
      if (present(image_set)) then
        l = lbound(image_set,1)
        u = ubound(image_set,1)
#       if ASSERTIONS
          block ! input validation
            logical p(num_imgs)
            p = .false.
            do i = l,u
              call_assert(image_set(i) >= 1 .and. image_set(i) <= num_imgs)
              call_assert_describe(.not. p(image_set(i)), "image indices in SYNC IMAGES are not distinct!")
              p(image_set(i)) = .true.
            end do
          end block
#       endif
      else ! SYNC IMAGES (*)
        l = 1
        u = num_imgs
      endif
    end associate

    ! post an event to each peer in my slot
    do i=l,u
      if (present(image_set)) then
        img = image_set(i)
      else
        img = i
      endif
      img = caf_image_to_initial( current_team%info%gex_team, img )
      call base_pointer(si_coarray_handle, img, evt_ptr)
      evt_ptr = evt_ptr + sizeof_event * (initial_team%this_image - 1)
      call caf_event_post(img, evt_ptr, &
                          segment_boundary=0, release_fence=0)
    end do

    ! reap an event from each peer in its slot
    ! final iteration issues acquire fence
    do i=l,u
      if (present(image_set)) then
        img = image_set(i)
      else
        img = i
      endif
      img = caf_image_to_initial( current_team%info%gex_team, img )
      call caf_event_wait(c_loc(si_evt(img)), 1_c_int64_t, &
                          segment_boundary=0, &
                          acquire_fence=merge(1,0,i==u))
    end do

  end procedure

end submodule
