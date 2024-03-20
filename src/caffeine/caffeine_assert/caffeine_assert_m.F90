! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module caffeine_assert_m
  implicit none

  private
  public :: assert

#if CAF_ASSERTIONS || !defined(CAF_ASSERTIONS)
  logical, parameter :: assertions_=.true.
#else  
  logical, parameter :: assertions_=.false.
#endif

   !! Disable assertions with
   !! fpm test --flag "-DCAF_ASSERTIONS=0"
 
  interface

    pure module subroutine assert(assertion, description, diagnostics)
      implicit none
      logical, intent(in) :: assertion
      character(len=*), intent(in) :: description
      class(*), intent(in), optional :: diagnostics
    end subroutine

  end interface

end module caffeine_assert_m
