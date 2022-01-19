module caf_co_sum_test
    use caffeine_m, only : caf_co_sum, caf_num_images
    use vegetables, only: result_t, test_item_t, assert_equals, describe, it

    implicit none
    private
    public :: test_caf_co_sum

contains
    function test_caf_co_sum() result(tests)
        type(test_item_t) tests
    
        tests = describe( &
          "The caf_co_sum subroutine", &
          [ it("sums scalars with result_image not present", sum_scalars_no_result_image) &
        ])
    end function

    function sum_scalars_no_result_image() result(result_)
        type(result_t) result_
        integer i
 
        i = 1
        call caf_co_sum(i)
        result_ = assert_equals(caf_num_images(), i)
    end function

end module caf_co_sum_test
