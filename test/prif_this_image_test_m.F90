! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#ifndef __GFORTRAN__
  #define F2008_PROC_PTR_ARG_ASSOCIATION
#endif

module prif_this_image_test_m
  use prif, only : prif_this_image_no_coarray, prif_num_images, prif_co_sum
  use julienne_m, only : test_t, test_result_t, test_description_t, test_description_substring
#ifndef F2008_PROC_PTR_ARG_ASSOCIATION
  use julienne_m, only : test_function_i
#endif
    implicit none

    private
    public :: prif_this_image_test_t

  type, extends(test_t) :: prif_this_image_test_t
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

#ifdef F2008_PROC_PTR_ARG_ASSOCIATION
    test_descriptions = [ & 
      test_description_t("returning its member of the image set when called with no arguments", check_this_image_set) &
    ]   
#else
    procedure(test_function_i), pointer :: check_this_image_set_ptr 

    check_this_image_set_ptr => check_this_image_set

    test_descriptions = [ & 
      test_description_t("returning its member of the image set when called with no arguments", check_this_image_set_ptr) &
    ]   
#endif

    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0  &
      .or. test_descriptions%contains_text(test_description_substring))
      
    test_results = test_descriptions%run()
  end function

  function check_this_image_set() result(test_passes)
    logical test_passes
    integer, allocatable :: image_numbers(:)
    integer i, me, ni

    call prif_this_image_no_coarray(this_image=me)
    call prif_num_images(num_images=ni)
    image_numbers = [(merge(0, me, me/=i), i = 1, ni)]
    call prif_co_sum(image_numbers)
    test_passes = all(image_numbers == [(i, i = 1, ni)]) .and. size(image_numbers) > 0
  end function

end module prif_this_image_test_m
