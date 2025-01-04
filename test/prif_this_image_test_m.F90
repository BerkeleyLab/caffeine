! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module prif_this_image_test_m
  !! Unit test for the prif_this_image subroutine
  use prif, only : prif_this_image_no_coarray, prif_num_images, prif_co_sum
  use prif_test_m, only : prif_test_t, test_description_substring
  use julienne_m, only : test_result_t, test_description_t, test_diagnosis_t, string_t, operator(.cat.)
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : diagnosis_function_i
#endif
    implicit none

    private
    public :: prif_this_image_test_t

  type, extends(prif_test_t) :: prif_this_image_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The prif_this_image_no_coarray subroutine"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ & 
      test_description_t("returning its member of the image set if called with no arguments", check_this_image_set) &
    ]   
#else
    procedure(diagnosis_function_i), pointer :: check_this_image_set_ptr 

    check_this_image_set_ptr => check_this_image_set

    test_descriptions = [ & 
      test_description_t("returning its member of the image set if called with no arguments", check_this_image_set_ptr) &
    ]   
#endif

    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0  &
      .or. test_descriptions%contains_text(test_description_substring))
      
    test_results = test_descriptions%run()
  end function

  function check_this_image_set() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer, allocatable :: image_numbers(:)
    integer i, me, ni
    integer, allocatable :: expected_image_numbers(:)

    call prif_this_image_no_coarray(this_image=me)
    call prif_num_images(num_images=ni)
    image_numbers = [(merge(0, me, me/=i), i = 1, ni)]
    call prif_co_sum(image_numbers)
    associate(expected_values => [(i, i = 1, ni)])
      ! TODO: support support array aguments to string_t(); remove .cat. below; generate CSV list instead
      test_diagnosis = test_diagnosis_t( &
        test_passed = all(image_numbers == expected_values) .and. size(image_numbers) > 0, &
        diagnostics_string = &
          "expected: " // .cat. string_t(expected_values) // ", actual: " // .cat. string_t(image_numbers) &
    )
    end associate
  end function

end module prif_this_image_test_m
