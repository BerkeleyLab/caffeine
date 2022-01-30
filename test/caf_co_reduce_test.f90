module caf_co_reduce_test
    use caffeine_m, only : caf_co_reduce, caf_num_images, caf_this_image
    use vegetables, only : result_t, test_item_t, assert_equals, describe, it, assert_that, assert_equals
    use collective_subroutines_m, only : c_int32_t_operation

    implicit none
    private
    public :: test_caf_co_reduce

contains

    function test_caf_co_reduce() result(tests)
        type(test_item_t) tests
    
        tests = describe( &
          "The caf_co_reduce subroutine", &
          [ it("sums default integer scalars with no optional arguments present", sum_default_integer_scalars) &
        ])
    end function

    pure function add(lhs, rhs) result(total)
      integer, intent(in) :: lhs, rhs
      integer total
      total = lhs + rhs 
    end function

    function sum_default_integer_scalars() result(result_)
        type(result_t) result_
        integer i
        procedure(c_int32_t_operation), pointer :: add_operation
  
        add_operation => add
        i = 1
        call caf_co_reduce(i, add_operation)
        result_ = assert_equals(caf_num_images(), i)
    end function

end module caf_co_reduce_test
