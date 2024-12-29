! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module prif_num_images_test_m
  !! Unit test for the prif_num_images subroutine
  use prif, only : prif_num_images
  use prif_test_m, only : prif_test_t, test_description_substring
  use julienne_m, only : test_t, test_result_t, test_description_t
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : test_function_i
#endif
  implicit none

  private
  public :: prif_num_images_test_t

  type, extends(prif_test_t) :: prif_num_images_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The prif_num_images subroutine" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ & 
      test_description_t("providing a valid number of images when called with no arguments", check_num_images_valid) &
    ]   
#else
    procedure(test_function_i), pointer :: check_num_images_valid_ptr

    check_num_images_valid_ptr => check_num_images_valid

    test_descriptions = [ & 
      test_description_t("providing a valid number of images when invoked with no arguments", check_num_images_valid_ptr) &
    ]   
#endif

    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0  &
      .or. test_descriptions%contains_text(test_description_substring))
      
    test_results = test_descriptions%run()
  end function

  function check_num_images_valid() result(test_passes)
    logical test_passes
    integer num_imgs
    call prif_num_images(num_images=num_imgs)
    test_passes = num_imgs > 0
  end function

end module prif_num_images_test_m
