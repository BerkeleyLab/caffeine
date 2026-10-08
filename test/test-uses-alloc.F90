! This header provides use declarations needed for tests using prif_(de)allocate(_coarray),
! and hides the interface differences between PRIF spec revisions.
! It must be #included within a use block.

#ifndef CAF_INCLUDED_TEST_USES_ALLOC
#define CAF_INCLUDED_TEST_USES_ALLOC

#include "caffeine-version.h"
#include "caffeine-language-support.F90"

use prif, only : &
    prif_allocate_coarray, &
    prif_deallocate_coarray, prif_deallocate_coarrays, &
    prif_allocate, prif_deallocate, &
    prif_coarray_handle


  ! final func support
  use unit_test_parameters_m, only: null_final_proc
  use unit_test_parameters_m, only: final_proc_usher
#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
#  define final_proc(proc) proc
#else
#  define final_proc(proc) final_proc_usher(proc)
#endif

  use, intrinsic :: iso_c_binding, only: &
      c_ptr, c_int, c_int64_t, c_size_t, c_intptr_t, &
      c_null_funptr, c_null_ptr, &
      c_associated, c_f_pointer, c_funloc, c_loc, c_sizeof

#endif
