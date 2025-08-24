#include "language-support.F90"

module prif_this_image_no_coarray_test_m
  use prif, only : prif_this_image_no_coarray, prif_num_images, prif_co_sum
  use julienne_m, only: &
     operator(//) &
    ,operator(.all.) &
    ,operator(.equalsExpected.) &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only: diagnosis_function_i
#endif
  implicit none

  private
  public :: prif_this_image_no_coarray_test_t

  type, extends(test_t) :: prif_this_image_no_coarray_test_t
  contains
    procedure, nopass, non_overridable :: subject
    procedure, nopass, non_overridable :: results
  end type

contains
  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = "The prif_this_image_no_coarray subroutine"
  end function

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(prif_this_image_no_coarray_test_t) prif_this_image_no_coarray_test

    test_results = prif_this_image_no_coarray_test%run([ &
       test_description_t("returning a unique member of {1,...,num_images()} when called without arguments", check_this_image_set) &
    ])
  end function

#else

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(prif_this_image_no_coarray_test_t) prif_this_image_no_coarray_test
    procedure(diagnosis_function_i), pointer :: &
      check_this_image_set_ptr => check_this_image_set

    test_results = prif_this_image_no_coarray_test%run([ &
       test_description_t("returning a unique member of {1,...,num_images()} when called without arguments", check_this_image_set_ptr) &
    ])
  end function

#endif

  function check_this_image_set() result(test_diagnosis)
    type(test_diagnosis_t) :: test_diagnosis
    integer, allocatable :: image_numbers(:)
    integer i, me, ni

    call prif_this_image_no_coarray(this_image=me)
    call prif_num_images(num_images=ni)
    image_numbers = [(merge(0, me, me/=i), i = 1, ni)]
    call prif_co_sum(image_numbers)
    test_diagnosis = .all. (image_numbers .equalsExpected. [(i, i = 1, ni)]) // "correct image set"
  end function

end module prif_this_image_no_coarray_test_m
