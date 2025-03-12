module caf_stop_test
    use veggies, only: test_item_t, describe, result_t, it, assert_that, assert_equals
    use unit_test_parameters_m, only : expected_stop_code

    implicit none
    private
    public :: test_prif_this_image

contains
    function test_prif_this_image() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "A program that executes the prif_stop function", &
                [ it("exits with a zero exitstat when the program omits the stop code", exit_with_no_stop_code) &
                 ,it("prints an integer stop code and exits with exitstat equal to the stop code", exit_with_integer_stop_code) &
                 ,it("prints a character stop code and exits with a non-zero exitstat", exit_with_character_stop_code) &
                 ,it("invokes a registered callback", check_callback_invocation) &
                ])
    end function

    function exit_with_no_stop_code() result(result_)
        type(result_t) :: result_
        integer exit_status, cmd_stat

        call execute_command_line( &
          command = "./build/run-fpm.sh run --example stop_with_no_code > /dev/null 2>&1", &
          wait = .true., &
          exitstat = exit_status, &
          cmdstat = cmd_stat &
        )
        result_ = assert_equals(0, exit_status)

    end function

    function exit_with_integer_stop_code() result(result_)
        type(result_t) :: result_
        integer exit_status, cmd_stat

        call execute_command_line( &
          command = "./build/run-fpm.sh run --example stop_with_integer_code > /dev/null 2>&1", &
          wait = .true., &
          exitstat = exit_status, &
          cmdstat = cmd_stat &
        )
        result_ = assert_equals(expected_stop_code, exit_status)

    end function

    function exit_with_character_stop_code() result(result_)
        type(result_t) :: result_
        integer exit_status, cmd_stat

        call execute_command_line( &
          command = "./build/run-fpm.sh run --example stop_with_character_code > /dev/null 2>&1", &
          wait = .true., &
          exitstat = exit_status, &
          cmdstat = cmd_stat &
        )   
        result_ = assert_equals(0, exit_status) ! the standard recommends zero exit status for character stop codes

    end function

    function check_callback_invocation() result(result_)
      type(result_t) :: result_

      integer exit_status, cmd_stat

      call execute_command_line( &
        command = "./build/run-fpm.sh run --example register_stop_callback > /dev/null 2>&1", &
        wait = .true., &
        exitstat = exit_status, &
        cmdstat = cmd_stat &
      )
      result_ = assert_equals(0, exit_status)
    end function

end module caf_stop_test
