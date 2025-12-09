module prif_error_stop_test_m
    use unit_test_parameters_m, only : expected_error_stop_code, &
        image_one => subjob_setup, cmd_prefix => subjob_prefix, fpm_driver
    use julienne_m, only: test_description_t, test_diagnosis_t, test_result_t, test_t, usher &
      ,operator(.expect.), operator(.equalsExpected.), operator(//)

    implicit none
    private
    public :: prif_error_stop_test_t

    type, extends(test_t) :: prif_error_stop_test_t
    contains
      procedure, nopass, non_overridable :: subject
      procedure, nopass, non_overridable :: results
    end type

   integer, parameter :: max_message_len = 128

contains

    pure function subject()
      character(len=:), allocatable :: subject
      subject = "prif_error_stop"
    end function

    function results() result(test_results)
      type(test_result_t), allocatable :: test_results(:)
      type(prif_error_stop_test_t) prif_error_stop_test

      allocate(test_results, source = prif_error_stop_test%run([ &
         test_description_t("delivering a non-zero exitstat when the stop code is omitted", usher(exit_with_no_stop_code)) &
        ,test_description_t("printing a character stop code and delivering a non-zero exitstat", usher(exit_with_character_stop_code)) &
        ,test_description_t("printing an integer stop code and delivering the non-zero exitstat", usher(exit_with_integer_stop_code)) &
      ]))
    end function

    function exit_with_no_stop_code() result(diag)
        type(test_diagnosis_t) :: diag
        integer exit_status
        integer command_status
        character(len=max_message_len) command_message

      if (image_one()) then
        command_message = "exit_with_no_stop_code"

        call execute_command_line( &
          command = cmd_prefix//fpm_driver//" run --example error_stop_with_no_code > /dev/null 2>&1" &
         ,wait = .true. &
         ,exitstat = exit_status &
         ,cmdstat = command_status &
         ,cmdmsg = command_message &
        )   
        diag = .expect. (exit_status /= 0) // command_message
      else
        diag = .true.
      end if

    end function

    function exit_with_integer_stop_code() result(diag)
        type(test_diagnosis_t) :: diag
        integer exit_status
        integer command_status
        character(len=max_message_len) command_message

      if (image_one()) then
        command_message = "exit_with_integer_stop_code"

        call execute_command_line( &
          command = cmd_prefix//fpm_driver//" run --example error_stop_with_integer_code > /dev/null 2>&1" &
         ,wait = .true. &
         ,exitstat = exit_status &
         ,cmdstat = command_status &
         ,cmdmsg = command_message &
        )
        diag = (exit_status .equalsExpected. expected_error_stop_code) // command_message
      else
        diag = .true.
      end if

    end function

    function exit_with_character_stop_code() result(diag)
        type(test_diagnosis_t) :: diag
        integer exit_status
        integer command_status
        character(len=max_message_len) command_message

      if (image_one()) then
        command_message = "exit_with_character_stop_code"

        call execute_command_line( &
          command = cmd_prefix//fpm_driver//" run --example error_stop_with_character_code > /dev/null 2>&1" &
         ,wait = .true. &
         ,exitstat = exit_status &
         ,cmdstat = command_status &
         ,cmdmsg = command_message &
        )   
        diag = .expect. (exit_status /= 0) // command_message
      else
        diag = .true.
      end if

    end function

end module prif_error_stop_test_m
