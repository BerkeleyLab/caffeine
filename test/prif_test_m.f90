! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt
module prif_test_m
  !! Override Julienne's test_t type-bound procedure "report" to replace parallel Fortran featuers with PRIF calls
  use julienne_m, only : test_t
  implicit none

  private
  public :: prif_test_t
  public :: test_description_substring

  type, abstract, extends(test_t) :: prif_test_t
  contains
    procedure report
  end type

  character(len=:), allocatable, protected :: test_description_substring

  interface

    module subroutine report(test, passes, tests)
      !! Print the test results and increment the tallies of passing tests and total tests
      implicit none
      class(prif_test_t), intent(in) :: test
      integer, intent(inout) :: passes, tests
    end subroutine

  end interface

end module prif_test_m
