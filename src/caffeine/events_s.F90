! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"
    
submodule(prif:prif_private_s) events_s
  ! DO NOT ADD USE STATEMENTS HERE
  ! All use statements belong in prif_private_s.F90
  implicit none

contains

  module procedure prif_event_post
    integer(c_intptr_t) :: remote_base

    call_assert(coarray_handle_check(coarray_handle))
    call_assert(offset >= 0)

    call base_pointer(coarray_handle, image_num, remote_base)
    call prif_event_post_indirect( &
        image_num = image_num, &
        event_var_ptr = remote_base + offset, &
        stat = stat, errmsg = errmsg, errmsg_alloc = errmsg_alloc)
  end procedure

  module procedure prif_event_post_indirect
    call_assert_describe(image_num > 0 .and. image_num <= initial_team%num_images, "image_num not within valid range")

    call caf_event_post(image_num, event_var_ptr, &
           segment_boundary=1, release_fence=1)

    if (present(stat)) stat = 0
  end procedure

  module procedure prif_event_wait
    integer(c_int64_t) :: threshold

    if (present(until_count)) then
      threshold = MAX(until_count, 1)
    else
      threshold = 1
    endif
    call caf_event_wait(event_var_ptr, threshold, &
           segment_boundary=1, acquire_fence=1, maybe_concurrent=0)

    if (present(stat)) stat = 0
  end procedure

  module procedure prif_event_query
    call caf_event_query(event_var_ptr, count) 

    if (present(stat)) stat = 0
  end procedure

  module procedure prif_notify_wait
    integer(c_int64_t) :: threshold

    if (present(until_count)) then
      threshold = MAX(until_count, 1)
    else
      threshold = 1
    endif
    call caf_event_wait(notify_var_ptr, threshold, &
           segment_boundary=0, acquire_fence=1, maybe_concurrent=1)

    if (present(stat)) stat = 0
  end procedure

end submodule events_s
