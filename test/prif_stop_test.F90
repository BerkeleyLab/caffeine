module caf_stop_test
    use prif, only: prif_this_image_no_coarray, prif_sync_all
    use veggies, only: test_item_t, describe, result_t, it, assert_that, assert_equals, succeed
    use unit_test_parameters_m, only : expected_stop_code

    implicit none
    private
    public :: test_prif_stop

   integer, parameter :: max_message_len = 128

contains
    function test_prif_stop() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "A program that executes the prif_stop function", &
                [ it("exits with a zero exitstat when the program omits the stop code", exit_with_no_stop_code) &
                 ,it("prints an integer stop code and exits with exitstat equal to the stop code", exit_with_integer_stop_code) &
                 ,it("prints a character stop code and exits with a non-zero exitstat", exit_with_character_stop_code) &
                 ,it("invokes a registered callback", check_callback_invocation) &
                ])
    end function

    function image_one() result(result_)
        logical :: result_
        integer :: me

        call prif_sync_all()
        call prif_this_image_no_coarray(this_image=me)
        result_ = (me == 1)
    end function 

    function exit_with_no_stop_code() result(result_)
        type(result_t) :: result_
        integer exit_status, cmd_stat
        character(len=max_message_len) command_message

      if (image_one()) then
        command_message = "exit_with_no_stop_code"

        call execute_command_line( &
          command = "./build/run-fpm.sh run --example stop_with_no_code > /dev/null 2>&1", &
          wait = .true., &
          exitstat = exit_status, &
          cmdstat = cmd_stat, &
          cmdmsg = command_message &
        )
        result_ = assert_equals(0, exit_status, command_message)
      else
        result_ = succeed("skipped")
      end if

    end function

    function exit_with_integer_stop_code() result(result_)
        type(result_t) :: result_
        integer exit_status, cmd_stat
        character(len=max_message_len) command_message

      if (image_one()) then
        command_message = "exit_with_integer_stop_code"

        call execute_command_line( &
          command = "./build/run-fpm.sh run --example stop_with_integer_code > /dev/null 2>&1", &
          wait = .true., &
          exitstat = exit_status, &
          cmdstat = cmd_stat, &
          cmdmsg = command_message &
        )
        result_ = assert_equals(expected_stop_code, exit_status, command_message)
      else
        result_ = succeed("skipped")
      end if

    end function

    function exit_with_character_stop_code() result(result_)
        type(result_t) :: result_
        integer exit_status, cmd_stat
        character(len=max_message_len) command_message

      if (image_one()) then
        command_message = "exit_with_character_stop_code"

        call execute_command_line( &
          command = "./build/run-fpm.sh run --example stop_with_character_code > /dev/null 2>&1", &
          wait = .true., &
          exitstat = exit_status, &
          cmdstat = cmd_stat, &
          cmdmsg = command_message &
        )   
        ! the standard recommends zero exit status for character stop codes
        result_ = assert_equals(0, exit_status, command_message) 
      else
        result_ = succeed("skipped")
      end if

    end function

    function check_callback_invocation() result(result_)
        type(result_t) :: result_
        integer exit_status, cmd_stat
        character(len=max_message_len) command_message

      if (image_one()) then
        call execute_command_line( &
          command = "./build/run-fpm.sh run --example register_stop_callback > /dev/null 2>&1", &
          wait = .true., &
          exitstat = exit_status, &
          cmdstat = cmd_stat, &
          cmdmsg = command_message &
        )
        result_ = assert_equals(0, exit_status, command_message)
      else
        result_ = succeed("skipped")
      end if
    end function

end module caf_stop_test
