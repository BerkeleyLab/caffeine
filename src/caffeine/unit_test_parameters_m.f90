! Copyright (c), The Regents of the University
! Terms of use are as specified in LICENSE.txt
module unit_test_parameters_m
  implicit none

  private
  public :: expected_stop_code
  public :: expected_error_stop_code

  enum, bind(C)
    enumerator :: expected_stop_code=99, expected_error_stop_code
  end enum

end module unit_test_parameters_m