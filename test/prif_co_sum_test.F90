module caf_co_sum_test
    use iso_c_binding, only: c_int32_t, c_int64_t, c_float, c_double
    use prif, only : prif_co_sum, prif_num_images, prif_this_image_no_coarray
    use veggies, only: result_t, test_item_t, assert_equals, describe, it, succeed

    implicit none
    private
    public :: test_prif_co_sum

contains
    function test_prif_co_sum() result(tests)
        type(test_item_t) tests

        tests = describe( &
          "The prif_co_sum subroutine computes the sum across images for corresponding elements for", &
          [ it("a 1D default integer array", check_default_integer) &
          , it("32 bit integer scalars", check_32_bit_integer) &
          , it("a 1D 64 bit integer array", check_64_bit_integer) &
          , it("a 2D 32 bit real array", check_32_bit_real) &
          , it("a 1D 64 bit real array", check_64_bit_real) &
          , it("a 2D complex array with 32 bit components", check_32_bit_complex) &
          , it("a 1D complex array with 64 bit components", check_64_bit_complex) &
          ])
    end function

    function check_default_integer() result(result_)
        type(result_t) :: result_

        integer, parameter :: values(*,*) = reshape([1, 19, 5, 13, 11, 7, 17, 3], [2, 4])
        integer :: me, ni, i
        integer, dimension(size(values,1)) :: my_val, expected

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(ni)

        my_val = values(:, mod(me-1, size(values,2))+1)
        call prif_co_sum(my_val)

        expected = sum(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), dim=2)
        result_ = assert_equals(int(expected), int(my_val))
    end function

    function check_32_bit_integer() result(result_)
        type(result_t) :: result_

        integer(c_int32_t), parameter :: values(*) = [1, 19, 5, 13, 11, 7, 17, 3]
        integer :: me, ni, i
        integer(c_int32_t) :: my_val, expected

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(ni)

        my_val = values(mod(me-1, size(values))+1)
        call prif_co_sum(my_val)

        expected = sum([(values(mod(i-1,size(values))+1), i = 1, ni)])
        result_ = assert_equals(expected, my_val)
    end function

    function check_64_bit_integer() result(result_)
        type(result_t) :: result_

        integer(c_int64_t), parameter :: values(*,*) = reshape([1, 19, 5, 13, 11, 7, 17, 3], [2, 4])
        integer :: me, ni, i
        integer(c_int64_t), dimension(size(values,1)) :: my_val, expected

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(ni)

        my_val = values(:, mod(me-1, size(values,2))+1)
        call prif_co_sum(my_val)

        expected = sum(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), dim=2)
        result_ = assert_equals(int(expected), int(my_val))
    end function

    function check_32_bit_real() result(result_)
        type(result_t) :: result_

        real(c_float), parameter :: values(*,*,*) = reshape([1, 19, 5, 13, 11, 7, 17, 3], [2,2,2])
        integer :: me, ni, i
        real(c_float), dimension(size(values,1), size(values,2)) :: my_val, expected

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(ni)

        my_val = values(:, :, mod(me-1, size(values,3))+1)
        call prif_co_sum(my_val)

        expected = sum(reshape([(values(:,:,mod(i-1,size(values,3))+1), i = 1, ni)], [size(values,1), size(values,2), ni]), dim=3)
        result_ = assert_equals(real(expected,kind=c_double), real(my_val,kind=c_double))
    end function

    function check_64_bit_real() result(result_)
        type(result_t) :: result_

        real(c_double), parameter :: values(*,*) = reshape([1, 19, 5, 13, 11, 7, 17, 3], [2, 4])
        integer :: me, ni, i
        real(c_double), dimension(size(values,1)) :: my_val, expected

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(ni)

        my_val = values(:, mod(me-1, size(values,2))+1)
        call prif_co_sum(my_val)

        expected = sum(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), dim=2)
        result_ = assert_equals(expected, my_val)
    end function

    function check_32_bit_complex() result(result_)
        type(result_t) :: result_

        complex(c_float), parameter :: values(*,*,*) = reshape( &
                [ cmplx(1., 53.), cmplx(3., 47.) &
                , cmplx(5., 43.), cmplx(7., 41.) &
                , cmplx(11., 37.), cmplx(13., 31.) &
                , cmplx(17., 29.), cmplx(19., 23.) &
                ], &
                [2,2,2])
        integer :: me, ni, i
        complex(c_float), dimension(size(values,1),size(values,2)) :: my_val, expected

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(ni)

        my_val = values(:, :, mod(me-1, size(values,3))+1)
        call prif_co_sum(my_val)

        expected = sum(reshape([(values(:,:,mod(i-1,size(values,3))+1), i = 1, ni)], [size(values,1), size(values,2), ni]), dim=3)
        result_ = &
                assert_equals(real(expected, kind=c_double), real(my_val, kind=c_double)) &
                .and.assert_equals(real(aimag(expected), kind=c_double), real(aimag(my_val), kind=c_double))
    end function

    function check_64_bit_complex() result(result_)
        type(result_t) :: result_

        complex(c_double), parameter :: values(*,*) = reshape( &
                [ cmplx(1., 53.), cmplx(3., 47.) &
                , cmplx(5., 43.), cmplx(7., 41.) &
                , cmplx(11., 37.), cmplx(13., 31.) &
                , cmplx(17., 29.), cmplx(19., 23.) &
                ], &
                [2,4])
        integer :: me, ni, i
        complex(c_double), dimension(size(values,1)) :: my_val, expected

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(ni)

        my_val = values(:, mod(me-1, size(values,2))+1)
        call prif_co_sum(my_val)

        expected = sum(reshape([(values(:,mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1), ni]), dim=2)
        result_ = &
                assert_equals(real(expected), real(my_val)) &
                .and.assert_equals(aimag(expected), aimag(my_val))
    end function

end module caf_co_sum_test
