module caf_co_sum_test
    use prif, only : prif_co_sum, prif_num_images, prif_this_image
    use veggies, only: result_t, test_item_t, assert_equals, describe, it, assert_that, assert_equals, succeed

    implicit none
    private
    public :: test_prif_co_sum

contains
    function test_prif_co_sum() result(tests)
        type(test_item_t) tests

        tests = describe( &
          "The prif_co_sum subroutine", &
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
        integer i, num_imgs

        i = 1
        call prif_co_sum(i)
        call prif_num_images(num_images=num_imgs)
        result_ = assert_equals(num_imgs, i)
    end function

    function sum_integers_all_arguments() result(result_)
        type(result_t) result_
        integer i, status_, result_image_, me, num_imgs
        character(len=*), parameter :: whitespace = repeat(" ", ncopies=29)
        character(len=:), allocatable :: error_message

        i = 1
        result_image_ = 1
        status_ = -1
        error_message = whitespace

        call prif_this_image(image_index=me)
        call prif_num_images(num_images=num_imgs)
        associate(expected_i => merge(num_imgs*i, i, me==result_image_))
          call prif_co_sum(i, result_image_, status_, error_message)
          result_ = assert_equals(expected_i, i) .and. assert_equals(0, status_) .and. assert_equals(whitespace, error_message)
        end associate
    end function

    function sum_c_int64_scalars() result(result_)
        use iso_c_binding, only : c_int64_t
        type(result_t) result_
        integer(c_int64_t) i
        integer i_default_kind, status_, num_imgs

        status_ = -1
        i = 2_c_int64_t
        call prif_co_sum(i, stat=status_)
        i_default_kind = i
        call prif_num_images(num_images=num_imgs)
        result_ = assert_equals(2*num_imgs, int(i)) .and. assert_equals(0, status_)
    end function

    function sum_default_integer_1D_array() result(result_)
        type(result_t) result_
        integer i, images
        integer, allocatable :: array(:)

        call prif_num_images(num_images=images)
        associate(sequence_ => [(i,i=1,images)])
          array = sequence_
          call prif_co_sum(array)
          result_ = assert_that(all(array==images*sequence_))
        end associate
    end function

    function sum_default_integer_15D_array() result(result_)
        type(result_t) result_
        integer array(2,1,1, 1,1,1, 1,1,1, 1,1,1, 1,2,1)
        integer status_, num_imgs

        status_ = -1
        array = 3
        call prif_co_sum(array, stat=status_)
        call prif_num_images(num_images=num_imgs)
        result_ = assert_that(all(3*num_imgs == array)) .and.  assert_equals(0, status_)
    end function

    function sum_default_real_scalars() result(result_)
        type(result_t) result_
        real scalar
        real, parameter :: e = 2.7182818459045
        integer result_image_, me, num_imgs

        result_image_ = 1
        scalar = e
        call prif_co_sum(scalar, result_image=result_image_)
        call prif_this_image(image_index=me)
        call prif_num_images(num_images=num_imgs)
        associate(expected_result => merge(num_imgs*e, e, me==result_image_))
          result_ = assert_equals(dble(expected_result), dble(scalar))
        end associate
    end function

    function sum_double_precision_2D_array() result(result_)
        type(result_t) result_
        double precision, allocatable :: array(:,:)
        double precision, parameter :: input(*,*) = reshape(-[6,5,4,3,2,1], [3,2])
        integer :: num_imgs

        array = input
        call prif_co_sum(array)
        call prif_num_images(num_images=num_imgs)
        result_ = assert_equals(product(num_imgs*input), product(array))
    end function

    function sum_default_complex_scalars() result(result_)
        type(result_t) result_
        real scalar
        complex z
        complex, parameter :: i=(0.,1.)
        integer status_, num_imgs

        status_ = -1
        z = i
        call prif_co_sum(z, stat=status_)
        call prif_num_images(num_images=num_imgs)
        result_ = assert_equals(dble(abs(i*num_imgs)), dble(abs(z)) ) .and. assert_equals(0, status_)
    end function

    function sum_dble_complex_1D_arrays() result(result_)
        type(result_t) result_
        integer, parameter :: dp = kind(1.D0)
        integer :: num_imgs
        complex(dp), allocatable :: array(:)
        complex(dp), parameter :: input(*) = [(1.D0,1.0D0)]

        array = [(1.D0,1.D0)]
        call prif_co_sum(array)
        call prif_num_images(num_images=num_imgs)
        result_ = assert_that(all([input*num_imgs] == array))
    end function

end module caf_co_sum_test
