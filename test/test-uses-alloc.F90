! This header provides use declarations needed for tests using prif_(de)allocate(_coarray),
! and hides the interface differences between PRIF spec revisions.
! It must be #included within a use block.

#ifndef CAF_INCLUDED_TEST_USES_ALLOC
#define CAF_INCLUDED_TEST_USES_ALLOC

#include "language-support.F90"

use prif, only : &
    prif_allocate_coarray, &
    prif_allocate, prif_deallocate, &
    prif_coarray_handle

#if !defined(CAF_PRIF_VERSION) || CAF_PRIF_VERSION >= 7
  ! PRIF 0.7+ deallocate
  use prif, only : prif_deallocate_coarray, prif_deallocate_coarrays
# define prif_deallocate_coarray3  prif_deallocate_coarray
# define prif_deallocate_coarrays3 prif_deallocate_coarrays
#else
  ! emulate PRIF 0.7 deallocate with older interfaces
  use prif, only : prif_deallocate_coarray_ => prif_deallocate_coarray
# define prif_deallocate_coarray(h)           prif_deallocate_coarray_([h])
# define prif_deallocate_coarrays(arr)        prif_deallocate_coarray_(arr)
# define prif_deallocate_coarray3(h,a2,a3)    prif_deallocate_coarray_([h],a2,a3)
# define prif_deallocate_coarrays3(arr,a2,a3) prif_deallocate_coarray_(arr,a2,a3)
#endif

  use iso_c_binding, only: &
      c_ptr, c_int, c_int64_t, c_size_t, c_intptr_t, &
      c_null_funptr, c_null_ptr, &
      c_associated, c_f_pointer, c_funloc, c_loc, c_sizeof

#endif
