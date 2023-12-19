!
!     (c) 2019-2020 Guide Star Engineering, LLC
!     This Software was developed for the US Nuclear Regulatory Commission (US NRC) under contract
!     "Multi-Dimensional Physics Implementation into Fuel Analysis under Steady-state and Transients (FAST)",
!     contract # NRC-HQ-60-17-C-0007
!
module caffeine_assert_m
  !! summary: Utility for runtime checking of logical assertions.
  !! usage: error-terminate if the assertion fails:
  !!
  !!    use assertions_m, only : assert
  !!    call assert( 2 > 1, "2 > 1")
  !!
  !! Turn off assertions in production code by setting USE_ASSERTIONS to .false. via the preprocessor.
  !! This file's capitalized .F90 extension causes most Fortran compilers to preprocess this file so
  !! that building as follows turns off assertion enforcement:
  !!
  !!    fpm build --flag "-DUSE_ASSERTIONS=.false."
  !!
  !! Doing so may eliminate any associated runtime overhead by enabling optimizing compilers to ignore
  !! the assertion procedure body during a dead-code-removal phase of optimization.
  implicit none
  private
  public :: assert

#ifndef USE_ASSERTIONS
# define USE_ASSERTIONS .true.
#endif
  logical, parameter :: enforce_assertions=USE_ASSERTIONS
    !! Turn off assertions as follows: fpm build --flag "-DUSE_ASSERTIONS=.false."

  interface

    module subroutine assert(assertion, description, diagnostic_data)
      !! If assertion is .false., error-terminate with a character stop code that contains diagnostic_data if present
      implicit none
      logical, intent(in) :: assertion
        !! Most assertions will be expressions such as i>0
      character(len=*), intent(in) :: description
        !! A brief statement of what is being asserted such as "i>0" or "positive i"
      class(*), intent(in), optional :: diagnostic_data
        !! Data to include in an error ouptput: may be of an intrinsic type or a type that extends characterizable_t
    end subroutine

  end interface

end module
