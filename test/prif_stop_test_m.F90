! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module prif_stop_test_m
  !! Unit test for the prif_stop subroutine
  use prif, only : prif_stop
  use unit_test_parameters_m, only : expected_stop
  use julienne_m, only : test_t, test_result_t, test_description_t, test_description_substring
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : test_function_i
#endif
    implicit none

    private
    public :: prif_stop_test_t

  type, extends(test_t) :: prif_stop_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "A program that calls prif_stop"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ &
      test_description_t("exiting with the integer stop code as the exitstat", exit_with_integer_stop_code) &
      !,test_description_t("exiting with exitstat=0 when no stop code is present", exit_with_no_stop_code) &
      !,test_description_t("exiting a character stop code and non-zero exitstat", exit_with_character_stop_code) &
    ]
#else
    procedure(test_function_i), pointer :: &
      exit_with_no_stop_code_ptr, exit_with_integer_stop_code_ptr, exit_with_character_stop_code_ptr

    exit_with_no_stop_code_ptr => exit_with_no_stop_code 
    exit_with_integer_stop_code_ptr => exit_with_integer_stop_code 
    exit_with_character_stop_code_ptr => exit_with_character_stop_code 

    test_descriptions = [ &
       test_description_t("exiting with exitstat=0 when no stop code is present", exit_with_no_stop_code_ptr) &
      ,test_description_t("exiting with the integer stop code exitstat", exit_with_integer_stop_code_ptr) &
      ,test_description_t("exiting a character stop code and non-zero exitstat", exit_with_character_stop_code_ptr) &
    ]
#endif

    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0  &
      .or. test_descriptions%contains_text(test_description_substring))

    test_results = test_descriptions%run()
  end function

  function exit_with_no_stop_code() result(test_passes)
      logical test_passes
      integer exit_status, command_status
      character(len=256), command_message

      call execute_command_line( &
        command = "./build/run-fpm.sh run --example stop_with_no_code > /dev/null 2>&1", &
        wait = .true., &
        exitstat = exit_status &
        ,cmdstat = command_status &
        ,cmdmsg = command_message &
      )   
        print *, exit_status, "<------ exit_status ---------"
        print *, command_status, "<----- command_status ----------"
        print *, command_message, "<-------command_message ----"
      test_passes = exit_status == 1

  end function

  function exit_with_integer_stop_code() result(test_passes)
      logical test_passes
      integer exit_status, command_status
      character(len=256), command_message

      call execute_command_line( &
        command = "./build/run-fpm.sh run --example stop_with_integer_code > /dev/null 2>&1" &
        ,wait = .true. &
        ,exitstat = exit_status &
        ,cmdstat = command_status &
        ,cmdmsg = command_message &
      )   
      test_passes = exit_status == expected_stop
  end function

  function exit_with_character_stop_code() result(test_passes)
      logical test_passes
      integer exit_status
      integer, parameter :: expected_exit_code=1 ! defined in src/caffeine/program_termination.f90

      call execute_command_line( &
        command = "./build/run-fpm.sh run --example stop_with_character_code > /dev/null 2>&1", &
        wait = .true., &
        exitstat = exit_status &
      )   
      test_passes = expected_exit_code == exit_status

  end function

end module prif_stop_test_m
