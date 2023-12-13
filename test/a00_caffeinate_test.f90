module a00_caffeinate_test
    use program_startup_m, only : prif_init
    use veggies, only: test_item_t, describe, result_t, it, assert_that

    implicit none
    private
    public :: test_caffeinate

contains

    function test_caffeinate() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
           "A caffeinated beverage", &
           [ it("is served: the caffeinate() initiation function completes successfully.", check_caffeination) &
        ])
    end function

    function check_caffeination() result(result_)
        type(result_t) :: result_

        integer, parameter :: successful_initiation = 0
        integer :: init_exit_code

        call prif_init(init_exit_code)
        result_ = assert_that(init_exit_code == successful_initiation)
    end function

end module a00_caffeinate_test
