module caf_co_sum_test
    use caffeine_m, only : caf_co_sum, caf_num_images
    use vegetables, only: result_t, test_item_t, assert_equals, describe, it, assert_that, assert_equals

    implicit none
    private
    public :: test_caf_co_sum

contains
    function test_caf_co_sum() result(tests)
        type(test_item_t) tests
    
        tests = describe( &
          "The caf_co_sum subroutine", &
          [ it("sums default integer scalars with result_image not present", sum_default_integer_scalars) &
           ,it("sums integer(c_int64_t) scalars with result_image not present", sum_c_int64_scalars) &
           ,it("sums default integer 1D arrays with result_image not present", sum_default_integer_1D_array) &
           ,it("sums default integer 15D arrays with result_image not present", sum_default_integer_15D_array) &
           ,it("sums default real scalars with result_image not present", sum_default_real_scalars) &
           ,it("sums double precision 2D arrays with result_image not present", sum_double_precision_2D_array) &
        ])
    end function

    function sum_default_integer_scalars() result(result_)
        type(result_t) result_
        integer i
 
        i = 1
        call caf_co_sum(i)
        result_ = assert_equals(caf_num_images(), i)
    end function

    function sum_c_int64_scalars() result(result_)
        use iso_c_binding, only : c_int64_t 
        type(result_t) result_
        integer(c_int64_t) i
        integer i_default_kind
 
        i = 2_c_int64_t
        call caf_co_sum(i)
        i_default_kind = i
        result_ = assert_equals(2*caf_num_images(), int(i))
    end function

    function sum_default_integer_1D_array() result(result_)
        type(result_t) result_
        integer i
        integer, allocatable :: array(:)
 
        associate(images => caf_num_images(), sequence_ => [(i,i=1,caf_num_images())])
          array = sequence_
          call caf_co_sum(array)
          result_ = assert_that(all(images*sequence_== array))
        end associate
    end function

    function sum_default_integer_15D_array() result(result_)
        type(result_t) result_
        integer array(2,1,1, 1,1,1, 1,1,1, 1,1,1, 1,2,1)
 
        array = 3
        call caf_co_sum(array)
        result_ = assert_that(all(3*caf_num_images() == array))
    end function

    function sum_default_real_scalars() result(result_)
        type(result_t) result_
        real scalar
        real, parameter :: e = 2.7182818459045
        integer, parameter :: dp = kind(1.D0)

        scalar = e
        call caf_co_sum(scalar)
        result_ = assert_equals(real(caf_num_images()*e, dp), real(scalar, dp) )
    end function

    function sum_double_precision_2D_array() result(result_)
        type(result_t) result_
        double precision, allocatable :: array(:,:)
        double precision, parameter :: input(*,*) = reshape(-[6,5,4,3,2,1], [3,2])
 
        array = input
        call caf_co_sum(array)
        result_ = assert_equals(product(caf_num_images()*input), product(array))
    end function

end module caf_co_sum_test
