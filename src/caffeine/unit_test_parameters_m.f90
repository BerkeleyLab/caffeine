! Copyright (c), The Regents of the University
! Terms of use are as specified in LICENSE.txt
module unit_test_parameters_m
  !! Define values for consistent use throughout the test suite
  implicit none

  private
  public :: expected_stop_code
  public :: expected_error_stop_code

  enum, bind(C)
    enumerator :: expected_stop_code=99, expected_error_stop_code
      ! used in stop/error-stop unit tests and example/test-support supporting programs
  end enum

end module unit_test_parameters_m