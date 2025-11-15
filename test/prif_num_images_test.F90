module prif_num_images_test_m
  use prif, only : prif_num_images
  use julienne_m, only:  &
     operator(//) &
    ,operator(.isAtLeast.) &
    ,usher &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t

  implicit none
  private
  public :: prif_num_images_test_t

  type, extends(test_t) :: prif_num_images_test_t
  contains
    procedure, nopass, non_overridable :: subject
    procedure, nopass, non_overridable :: results
  end type

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = "The prif_num_images function"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(prif_num_images_test_t) prif_num_images_test

    test_results = prif_num_images_test%run([ &
        test_description_t("returning a valid number of images when invoked with no arguments", usher(check_num_images_valid)) &
      ])
  end function


  function check_num_images_valid() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer num_imgs
    call prif_num_images(num_images=num_imgs)
    test_diagnosis = (num_imgs .isAtLeast. 1) // "positive number of images"
  end function

end module prif_num_images_test_m
