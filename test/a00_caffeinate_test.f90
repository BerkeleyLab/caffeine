module a00_caffeinate_test
    use prif, only : prif_init, PRIF_STAT_ALREADY_INIT
    use veggies, only: test_item_t, result_t, assert_that
    use prif_veggies, only: describe, it

    implicit none
    private
    public :: test_caffeinate

contains

    function test_caffeinate() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
           "A caffeinated beverage", &
           [ it("is served: the prif_init() function completes successfully.", check_caffeination) &
           , it("a subsequent prif_init call returns PRIF_STAT_ALREADY_INIT", &
              check_subsequent_prif_init_call) &
        ])
    end function

    function check_caffeination() result(result_)
        type(result_t) :: result_

        integer, parameter :: successful_initiation = 0
        integer :: init_exit_code

        call prif_init(init_exit_code)
        result_ = assert_that(init_exit_code == successful_initiation)
    end function

    function check_subsequent_prif_init_call() result(result_)
        type(result_t) :: result_

        integer :: stat

        call prif_init(stat)
        call prif_init(stat)
        result_ = assert_that(stat == PRIF_STAT_ALREADY_INIT)
    end function

end module a00_caffeinate_test
