module caf_error_stop_test
    use prif, only: prif_this_image_no_coarray, prif_sync_all
    use veggies, only: test_item_t, describe, result_t, it, assert_that, assert_equals, succeed
    use unit_test_parameters_m, only : expected_error_stop_code, &
        image_one => subjob_setup, cmd_prefix => subjob_prefix

    implicit none
    private
    public :: test_prif_error_stop

   integer, parameter :: max_message_len = 128

contains
    function test_prif_error_stop() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "A program that executes the prif_error_stop function", &
                [ it("exits with a non-zero exitstat when the program omits the stop code", exit_with_no_stop_code) &
                 ,it("prints a character stop code and exits with a non-zero exitstat", exit_with_character_stop_code) &
                 ,it("prints an integer stop code and exits with exitstat equal to the stop code", exit_with_integer_stop_code) &
                ])
    end function

    function exit_with_no_stop_code() result(result_)
        type(result_t) :: result_
        integer exit_status
        integer command_status
        character(len=max_message_len) command_message

      if (image_one()) then
        command_message = "exit_with_no_stop_code"

        call execute_command_line( &
          command = cmd_prefix//"./build/run-fpm.sh run --example error_stop_with_no_code > /dev/null 2>&1" &
         ,wait = .true. &
         ,exitstat = exit_status &
         ,cmdstat = command_status &
         ,cmdmsg = command_message &
        )   
        result_ = assert_that(exit_status /= 0, command_message)
      else
        result_ = succeed("skipped")
      end if

    end function

    function exit_with_integer_stop_code() result(result_)
        type(result_t) :: result_
        integer exit_status
        integer command_status
        character(len=max_message_len) command_message

      if (image_one()) then
        command_message = "exit_with_integer_stop_code"

        call execute_command_line( &
          command = cmd_prefix//"./build/run-fpm.sh run --example error_stop_with_integer_code > /dev/null 2>&1" &
         ,wait = .true. &
         ,exitstat = exit_status &
         ,cmdstat = command_status &
         ,cmdmsg = command_message &
        )
        result_ = &
         assert_equals(expected_error_stop_code, exit_status, command_message) 
      else
        result_ = succeed("skipped")
      end if

    end function

    function exit_with_character_stop_code() result(result_)
        type(result_t) :: result_
        integer exit_status
        integer command_status
        character(len=max_message_len) command_message

      if (image_one()) then
        command_message = "exit_with_character_stop_code"

        call execute_command_line( &
          command = cmd_prefix//"./build/run-fpm.sh run --example error_stop_with_character_code > /dev/null 2>&1" &
         ,wait = .true. &
         ,exitstat = exit_status &
         ,cmdstat = command_status &
         ,cmdmsg = command_message &
        )   
        result_ = assert_that(exit_status /= 0, command_message)
      else
        result_ = succeed("skipped")
      end if

    end function

end module caf_error_stop_test
