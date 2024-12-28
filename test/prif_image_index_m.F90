! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module prif_image_index_test_m
  !! Unit test for the prif_image_index subroutine
  use iso_c_binding, only: c_int, c_intmax_t, c_ptr, c_size_t, c_null_funptr
  use prif, only: prif_coarray_handle, prif_allocate_coarray, prif_deallocate_coarray, prif_image_index, prif_num_images
  use julienne_m, only : test_t, test_result_t, test_description_t, test_description_substring
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : test_function_i
#endif
  implicit none

  private
  public :: prif_image_index_test_t

  type, extends(test_t) :: prif_image_index_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The prif_image_index subroutine" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ &
       test_description_t("returning 1 for the simplest case", check_simple_case) &
      ,test_description_t("returning 1 when given the lower bounds", check_lower_bounds) &
      ,test_description_t("returning 0 with invalid subscripts", check_invalid_subscripts) &
      ,test_description_t("returning the expected answer for a more complicated case", check_complicated) &
    ]

#else
    procedure(test_function_i), pointer :: &
      check_simple_case_ptr, &
      check_lower_bounds_ptr, &
      check_invalid_subscripts_ptr, &
      check_complicated_ptr

    check_simple_case_ptr => check_simple_case
    check_lower_bounds_ptr => check_lower_bounds
    check_invalid_subscripts_ptr => check_invalid_subscripts
    check_complicated_ptr => check_complicated

    test_descriptions = [ &
       test_description_t("returning 1 for the simplest case", check_simple_case_ptr) &
      ,test_description_t("returning 1 when given the lower bounds", check_lower_bounds_ptr) &
      ,test_description_t("returning 0 with invalid subscripts", check_invalid_subscripts_ptr) &
      ,test_description_t("returning the expected answer for a more complicated case", check_complicated_ptr) &
    ]
#endif

    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0  .or. test_descriptions%contains_text(test_description_substring))
    test_results = test_descriptions%run()
  end function


  function check_simple_case() result(test_passes)
    logical test_passes
    type(prif_coarray_handle) :: coarray_handle
    type(c_ptr) :: allocated_memory
    integer(c_int) :: answer

    call prif_allocate_coarray( &
      lcobounds = [1_c_intmax_t], &
      ucobounds = [2_c_intmax_t], &
      size_in_bytes = 1_c_size_t, &
      final_func = c_null_funptr, &
      coarray_handle = coarray_handle, &
      allocated_memory = allocated_memory)
    call prif_image_index(coarray_handle, [1_c_intmax_t], image_index=answer)
    test_passes = answer == 1_c_int
    call prif_deallocate_coarray([coarray_handle])
  end function

  function check_lower_bounds() result(test_passes)
    logical test_passes
    type(prif_coarray_handle) coarray_handle
    type(c_ptr) allocated_memory
    integer(c_int) answer

    call prif_allocate_coarray( &
      lcobounds = [2_c_intmax_t, 3_c_intmax_t], &
      ucobounds = [3_c_intmax_t, 4_c_intmax_t], &
      size_in_bytes = 1_c_size_t, &
      final_func = c_null_funptr, &
      coarray_handle = coarray_handle, &
      allocated_memory = allocated_memory)
    call prif_image_index(coarray_handle, [2_c_intmax_t, 3_c_intmax_t], image_index=answer)
    test_passes = answer == 1_c_int
    call prif_deallocate_coarray([coarray_handle])
  end function

  function check_invalid_subscripts() result(test_passes)
    logical test_passes
    type(prif_coarray_handle) coarray_handle
    type(c_ptr) allocated_memory
    integer(c_int) answer

    call prif_allocate_coarray( &
      lcobounds = [-2_c_intmax_t, 2_c_intmax_t], &
      ucobounds = [2_c_intmax_t, 6_c_intmax_t], &
      size_in_bytes = 1_c_size_t, &
      final_func = c_null_funptr, &
      coarray_handle = coarray_handle, &
      allocated_memory = allocated_memory)
    call prif_image_index(coarray_handle, [-1_c_intmax_t, 1_c_intmax_t], image_index=answer)
    test_passes = answer == 0_c_int
    call prif_deallocate_coarray([coarray_handle])
  end function

  function check_complicated() result(test_passes)
    logical test_passes
    type(prif_coarray_handle) coarray_handle
    type(c_ptr) allocated_memory
    integer(c_int) answer, ni

    call prif_num_images(num_images=ni)
    call prif_allocate_coarray( &
      lcobounds = [1_c_intmax_t, 2_c_intmax_t], &
      ucobounds = [2_c_intmax_t, 3_c_intmax_t], &
      final_func = c_null_funptr, &
      size_in_bytes = 1_c_size_t, &
      coarray_handle = coarray_handle, &
      allocated_memory = allocated_memory)
    call prif_image_index(coarray_handle, [1_c_intmax_t, 3_c_intmax_t], image_index=answer)
    test_passes = answer == merge(3_c_int, 0_c_int, ni >= 3)
    call prif_deallocate_coarray([coarray_handle])
  end function

end module prif_image_index_test_m
