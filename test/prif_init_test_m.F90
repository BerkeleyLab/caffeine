! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#ifndef __GFORTRAN__
  #define F2008_PROC_PTR_ARG_ASSOCIATION
#endif

module prif_init_test_m
  use prif, only : prif_init, PRIF_STAT_ALREADY_INIT
  use julienne_m, only : test_t, test_result_t, test_description_t, test_description_substring
#ifndef F2008_PROC_PTR_ARG_ASSOCIATION
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

#ifdef F2008_PROC_PTR_ARG_ASSOCIATION
    test_descriptions = [ & 
      test_description_t("completing normally when called once", check_caffeination), &
      test_description_t("returning PRIF_STAT_ALREADY_INIT when called a second time", check_subsequent_prif_init_call) &
    ]   
#else
    procedure(test_function_i), pointer :: check_caffeination_ptr, check_subsequent_prif_init_call_ptr 

    check_caffeination_ptr              =>  check_caffeination
    check_subsequent_prif_init_call_ptr => check_subsequent_prif_init_call

    test_descriptions = [ & 
       test_description_t("completing normally when called once", check_caffeination_ptr) &
      ,test_description_t("returning PRIF_STAT_ALREADY_INIT when called a second time", check_subsequent_prif_init_call_ptr) &
    ]   
#endif

    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0  &
      .or. test_descriptions%contains_text(test_description_substring))
      
    test_results = test_descriptions%run()
  end function

    function check_caffeination() result(test_passes)
      !! check program initiation
      logical test_passes
      integer, parameter :: successful_initiation = 0
      integer init_exit_code

      call prif_init(init_exit_code)
      test_passes = init_exit_code == successful_initiation
    end function

    function check_subsequent_prif_init_call()  result(test_passes)
      logical test_passes

        integer :: stat

        call prif_init(stat)
        call prif_init(stat)
        test_passes = stat == PRIF_STAT_ALREADY_INIT
    end function

end module prif_init_test_m
