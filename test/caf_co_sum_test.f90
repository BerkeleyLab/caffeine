module caf_co_sum_test
    use caffeine_m, only : caf_co_sum, caf_num_images, caf_this_image
    use vegetables, only: result_t, test_item_t, assert_equals, describe, it, assert_that, assert_equals, succeed

    implicit none
    private
    public :: test_caf_co_sum

contains
    function test_caf_co_sum() result(tests)
        type(test_item_t) tests
    
        tests = describe( &
          "The caf_co_sum subroutine", &
          [ it("sums default integer scalars with no optional arguments present", sum_default_integer_scalars) &
           ,it("sums default integer scalars with all arguments present", sum_integers_all_arguments) &
           ,it("sums integer(c_int64_t) scalars with stat argument present", sum_c_int64_scalars) &
           ,it("sums default integer 1D arrays with no optional arguments present", sum_default_integer_1D_array) &
           ,it("sums default integer 15D arrays with stat argument present", sum_default_integer_15D_array) &
           ,it("sums default real scalars with result_image argument present", sum_default_real_scalars) &
           ,it("sums double precision 2D arrays with no optional arguments present", sum_double_precision_2D_array) &
           ,it("sums default complex scalars with stat argument present", sum_default_complex_scalars) &
           ,it("sums double precision 1D complex arrays with no optional arguments present", sum_dble_complex_1D_arrays) &
        ])
    end function

    function sum_default_integer_scalars() result(result_)
        type(result_t) result_
        integer i
 
        i = 1
        call caf_co_sum(i)
        result_ = assert_equals(caf_num_images(), i)
    end function

    function sum_integers_all_arguments() result(result_)
        type(result_t) result_
        integer i, status_, result_image_
        character(len=*), parameter :: whitespace = repeat(" ", ncopies=29)
        character(len=:), allocatable :: error_message

        i = 1
        result_image_ = 1
        status_ = -1
        error_message = whitespace

        associate(expected_i => merge(caf_num_images()*i, i, caf_this_image()==result_image_))
          call caf_co_sum(i, result_image_, status_, error_message)
          result_ = assert_equals(expected_i, i) .and. assert_equals(0, status_) .and. assert_equals(whitespace, error_message)
        end associate
    end function

    function sum_c_int64_scalars() result(result_)
        use iso_c_binding, only : c_int64_t 
        type(result_t) result_
        integer(c_int64_t) i
        integer i_default_kind, status_

        status_ = -1
        i = 2_c_int64_t
        call caf_co_sum(i, stat=status_)
        i_default_kind = i
        result_ = assert_equals(2*caf_num_images(), int(i)) .and. assert_equals(0, status_)
    end function

    function sum_default_integer_1D_array() result(result_)
        type(result_t) result_
        integer i
        integer, allocatable :: array(:)
 
        associate(images => caf_num_images())
          associate(sequence_ => [(i,i=1,images)])
            array = sequence_
            call caf_co_sum(array)
            result_ = assert_that(all(array==images*sequence_))
          end associate
        end associate
    end function

    function sum_default_integer_15D_array() result(result_)
        type(result_t) result_
        integer array(2,1,1, 1,1,1, 1,1,1, 1,1,1, 1,2,1)
        integer status_
 
        status_ = -1
        array = 3
        call caf_co_sum(array, stat=status_)
        result_ = assert_that(all(3*caf_num_images() == array)) .and.  assert_equals(0, status_)
    end function

    function sum_default_real_scalars() result(result_)
        type(result_t) result_
        real scalar
        real, parameter :: e = 2.7182818459045
        integer result_image_

        result_image_ = 1
        scalar = e
        call caf_co_sum(scalar, result_image=result_image_)
        associate(expected_result => merge(caf_num_images()*e, e, caf_this_image()==result_image_))
          result_ = assert_equals(dble(expected_result), dble(scalar))
        end associate
    end function

    function sum_double_precision_2D_array() result(result_)
        type(result_t) result_
        double precision, allocatable :: array(:,:)
        double precision, parameter :: input(*,*) = reshape(-[6,5,4,3,2,1], [3,2])
 
        array = input
        call caf_co_sum(array)
        result_ = assert_equals(product(caf_num_images()*input), product(array))
    end function

    function sum_default_complex_scalars() result(result_)
        type(result_t) result_
        real scalar
        complex z
        complex, parameter :: i=(0.,1.)
        integer status_

        status_ = -1
        z = i
        call caf_co_sum(z, stat=status_)
        result_ = assert_equals(dble(abs(i*caf_num_images())), dble(abs(z)) ) .and. assert_equals(0, status_)
    end function
             
    function sum_dble_complex_1D_arrays() result(result_)
        type(result_t) result_
        integer, parameter :: dp = kind(1.D0)
        complex(dp), allocatable :: array(:)
        complex(dp), parameter :: input(*) = [(1.D0,1.0D0)]
 
        array = [(1.D0,1.D0)]
        call caf_co_sum(array)
        result_ = assert_that(all([input*caf_num_images()] == array))
    end function

end module caf_co_sum_test
