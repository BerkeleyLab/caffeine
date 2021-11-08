module a00_caffeinate_test
    use caffeinate_decaffeinate_m, only : caffeinate
    use vegetables, only: test_item_t, describe, result_t, it, assert_that

    implicit none
    private
    public :: test_caffeinate

contains

    function test_caffeinate() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
           "A caffeinated beverage", &
           [ it("is served: caffeinate() result is zero.", check_caffeination) &
        ])
    end function

    function check_caffeination() result(result_)
        type(result_t) :: result_

        integer, parameter :: successful_initiation = 0

        result_ = assert_that(caffeinate() == successful_initiation)
    end function

end module a00_caffeinate_test
