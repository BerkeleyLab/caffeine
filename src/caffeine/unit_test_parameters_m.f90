! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module unit_test_parameters_m
  !! Define values for use in the test suite, including example
  !! programs that the test suite builds and runs.
  implicit none

  enum, bind(C)
    enumerator :: expected_error_stop=99, unexpected_error_stop, expected_stop, unexpected_stop
  end enum
end module
