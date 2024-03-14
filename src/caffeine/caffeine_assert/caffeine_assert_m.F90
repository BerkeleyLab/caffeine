! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module caffeine_assert_m
  implicit none

  private
  public :: assert

#ifndef ASSERTIONS
#define ASSERTIONS .true.
#endif

  logical, parameter :: assertions_=ASSERTIONS
   !! Turn off assertions with
   !! fpm test --flag "-DASSERTIONS=.false."
 
  interface

    pure module subroutine assert(assertion, description, diagnostics)
      implicit none
      logical, intent(in) :: assertion
      character(len=*), intent(in) :: description
      class(*), intent(in), optional :: diagnostics
    end subroutine

  end interface

end module caffeine_assert_m
