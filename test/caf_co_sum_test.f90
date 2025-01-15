module caf_co_sum_test
    use prif, only: prif_co_sum, prif_num_images, prif_this_image_no_coarray
    use veggies, only: result_t, test_item_t, assert_equals, describe, it, assert_that, succeed

    implicit none
    private
    public :: test_prif_co_sum

contains

    ! Main test function that aggregates all sub-tests for prif_co_sum
    function test_prif_co_sum() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
            "The prif_co_sum subroutine", &
            [ it("sums default integer scalars without optional arguments", sum_default_integer_scalars), &
              it("sums default integer scalars with all arguments", sum_integers_all_arguments), &
              it("sums integer(c_int64_t) scalars with stat argument", sum_c_int64_scalars), &
              it("sums default integer 1D arrays without optional arguments", sum_default_integer_1D_array), &
              it("sums default integer 15D arrays with stat argument", sum_default_integer_15D_array), &
              it("sums default real scalars with result_image argument", sum_default_real_scalars), &
              it("sums double precision 2D arrays without optional arguments", sum_double_precision_2D_array), &
              it("sums default complex scalars with stat argument", sum_default_complex_scalars), &
              it("sums double precision 1D complex arrays without optional arguments", sum_dble_complex_1D_arrays) &
            ])
    end function test_prif_co_sum

    ! Test summation of default integer scalars
    function sum_default_integer_scalars() result(result_)
        type(result_t) :: result_
        integer :: scalar, num_imgs

        scalar = 1
        call prif_co_sum(scalar)
        call prif_num_images(num_imgs)
        result_ = assert_equals(num_imgs, scalar)
    end function sum_default_integer_scalars

    ! Test summation of default integer scalars with all arguments
    function sum_integers_all_arguments() result(result_)
        type(result_t) :: result_
        integer :: scalar, status, result_image, num_imgs, this_img
        character(len=29), parameter :: blank_msg = repeat(" ", 29)
        character(len=:), allocatable :: error_msg

        scalar = 1
        result_image = 1
        status = -1
        error_msg = blank_msg

        call prif_this_image_no_coarray(this_img)
        call prif_num_images(num_imgs)
        associate(expected_value => merge(num_imgs * scalar, scalar, this_img == result_image))
            call prif_co_sum(scalar, result_image, status, error_msg)
            result_ = assert_equals(expected_value, scalar) .and. &
                      assert_equals(0, status) .and. &
                      assert_equals(blank_msg, error_msg)
        end associate
    end function sum_integers_all_arguments

    ! Test summation of c_int64_t scalars
    function sum_c_int64_scalars() result(result_)
        use iso_c_binding, only: c_int64_t
        type(result_t) :: result_
        integer(c_int64_t) :: scalar
        integer :: num_imgs, status

        scalar = 2_c_int64_t
        status = -1
        call prif_co_sum(scalar, stat=status)
        call prif_num_images(num_imgs)
        result_ = assert_equals(2 * num_imgs, scalar) .and. assert_equals(0, status)
    end function sum_c_int64_scalars

    ! Test summation of default integer 1D arrays
    function sum_default_integer_1D_array() result(result_)
        type(result_t) :: result_
        integer, allocatable :: array(:)
        integer :: num_imgs, i

        call prif_num_images(num_imgs)
        array = [(i, i = 1, num_imgs)]
        call prif_co_sum(array)
        result_ = assert_that(all(array == num_imgs * [(i, i = 1, num_imgs)]))
    end function sum_default_integer_1D_array

    ! Test summation of default integer 15D arrays
    function sum_default_integer_15D_array() result(result_)
        type(result_t) :: result_
        integer :: array(2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1)
        integer :: num_imgs, status

        array = 3
        status = -1
        call prif_co_sum(array, stat=status)
        call prif_num_images(num_imgs)
        result_ = assert_that(all(array == 3 * num_imgs)) .and. assert_equals(0, status)
    end function sum_default_integer_15D_array

    ! Test summation of default real scalars
    function sum_default_real_scalars() result(result_)
        type(result_t) :: result_
        real :: scalar
        real, parameter :: e = 2.7182818459045
        integer :: result_image, num_imgs, this_img

        scalar = e
        result_image = 1
        call prif_co_sum(scalar, result_image=result_image)
        call prif_this_image_no_coarray(this_img)
        call prif_num_images(num_imgs)
        associate(expected_result => merge(num_imgs * e, e, this_img == result_image))
            result_ = assert_equals(expected_result, scalar)
        end associate
    end function sum_default_real_scalars

    ! Test summation of double precision 2D arrays
    function sum_double_precision_2D_array() result(result_)
        type(result_t) :: result_
        double precision, allocatable :: array(:,:)
        double precision, parameter :: input(3,2) = reshape([-6, -5, -4, -3, -2, -1], [3,2])
        integer :: num_imgs

        array = input
        call prif_co_sum(array)
        call prif_num_images(num_imgs)
        result_ = assert_equals(array, num_imgs * input)
    end function sum_double_precision_2D_array

    ! Test summation of default complex scalars
    function sum_default_complex_scalars() result(result_)
        type(result_t) :: result_
        complex :: scalar, i
        integer :: num_imgs, status

        scalar = (0.0, 1.0)
        status = -1
        call prif_co_sum(scalar, stat=status)
        call prif_num_images(num_imgs)
        result_ = assert_equals(abs(scalar), abs(num_imgs * (0.0, 1.0))) .and. assert_equals(0, status)
    end function sum_default_complex_scalars

    ! Test summation of double precision 1D complex arrays
    function sum_dble_complex_1D_arrays() result(result_)
        type(result_t) :: result_
        integer, parameter :: dp = kind(1.0d0)
        complex(dp), allocatable :: array(:)
        integer :: num_imgs

        array = [(1.0_dp, 1.0_dp)]
        call prif_co_sum(array)
        call prif_num_images(num_imgs)
        result_ = assert_that(all(array == [(num_imgs * (1.0_dp, 1.0_dp))]))
    end function sum_dble_complex_1D_arrays

end module caf_co_sum_test
