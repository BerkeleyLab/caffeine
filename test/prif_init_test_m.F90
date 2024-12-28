! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module prif_init_test_m
  !! Unit test fort the prif_init program inititation subroutine
  use prif, only : prif_init, PRIF_STAT_ALREADY_INIT
  use julienne_m, only : test_t, test_result_t, test_description_t, test_description_substring
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : test_function_i
#endif
    implicit none

    private
    public :: prif_init_test_t

  type, extends(test_t) :: prif_init_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The prif_init subroutine" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ & 
      test_description_t("returning PRIF_STAT_ALREADY_INIT when called a second time", check_redundant_prif_init_call) &
    ]   
#else
    procedure(test_function_i), pointer :: check_caffeination_ptr, check_redundant_prif_init_call_ptr 
    check_redundant_prif_init_call_ptr => check_redundant_prif_init_call

    test_descriptions = [ & 
      test_description_t("returning PRIF_STAT_ALREADY_INIT when called a second time", check_redundant_prif_init_call_ptr) &
    ]   
#endif

    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0  &
      .or. test_descriptions%contains_text(test_description_substring))
    test_results = test_descriptions%run()
  end function

    function check_redundant_prif_init_call()  result(test_passes)
      logical test_passes
      integer stat
      call prif_init(stat)
      call prif_init(stat)
      test_passes = stat == PRIF_STAT_ALREADY_INIT
    end function

end module prif_init_test_m
