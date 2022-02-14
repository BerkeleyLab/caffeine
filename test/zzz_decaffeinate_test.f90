module zzz_decaffeinate_test
    use caffeinate_decaffeinate_m, only : caf_decaffeinate
    use vegetables, only: test_item_t, describe, result_t, it, assert_that

    implicit none
    private
    public :: test_decaffeinate

contains

    function test_decaffeinate() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
           "Caffeine", &
           [ it("has been consumed: decaffeinate(0) causes a program to exit with a zero stop code.", check_decaffeination) &
        ])
    end function

    function check_decaffeination() result(result_)
        type(result_t) :: result_
        integer, parameter :: normal_termination = 0
        integer exit_status

        call execute_command_line( &
          command = "./build/run-fpm.sh run --example normal_termination > /dev/null 2>&1", &
          wait = .true., &
          exitstat = exit_status &
        )   

        result_ = assert_that(exit_status == normal_termination)
    end function

end module zzz_decaffeinate_test
