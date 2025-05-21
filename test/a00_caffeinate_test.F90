module a00_caffeinate_test
    use prif, only : prif_init, PRIF_STAT_ALREADY_INIT
    use veggies, only: test_item_t, describe, result_t, it, assert_that

    implicit none
    private
    public :: test_caffeinate, check_caffeination

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
      ! this test needs to run very early at startup, so we memoize the result
      type(result_t) :: result_
      type(result_t), save :: myresult
      logical, save :: once = .false.

      if (once) then
        result_ = myresult
        return
      endif 
      once = .true.

      block
        integer, parameter :: successful_initiation = 0
        integer :: init_exit_code

        call prif_init(init_exit_code)
        myresult = assert_that(init_exit_code == successful_initiation)
        result_ = myresult
      end block
    end function

    function check_subsequent_prif_init_call() result(result_)
        type(result_t) :: result_

        integer :: stat

        call prif_init(stat)
        call prif_init(stat)
        result_ = assert_that(stat == PRIF_STAT_ALREADY_INIT)
    end function

end module a00_caffeinate_test
