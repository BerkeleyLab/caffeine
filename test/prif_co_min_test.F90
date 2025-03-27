module caf_co_min_test
    use iso_c_binding, only: c_int8_t, c_int16_t, c_int32_t, c_int64_t, c_float, c_double
    use prif, only : prif_co_min, prif_co_min_character, prif_this_image_no_coarray, prif_num_images
    use veggies, only: result_t, test_item_t, assert_equals, describe, it, succeed

    implicit none
    private
    public :: test_prif_co_min

contains
    function test_prif_co_min() result(tests)
        type(test_item_t) tests

        tests = describe( &
          "The prif_co_min subroutine computes the minimum value across images for corresponding elements for", &
          [ it("a 1D default integer array", check_default_integer) &
          , it("a 1D 8-bit integer array", check_8_bit_integer) &
          , it("a 1D 16-bit integer array", check_16_bit_integer) &
          , it("32-bit integer scalars", check_32_bit_integer) &
          , it("a 1D 64-bit integer array", check_64_bit_integer) &
          , it("a 2D 32-bit real array", check_32_bit_real) &
          , it("a 1D 64-bit real array", check_64_bit_real) &
          , it("a character scalar", check_character) &
          ])
    end function

    function check_default_integer() result(result_)
        type(result_t) :: result_

        integer, parameter :: values(*,*) = reshape([1, -19, 5, 13, 11, 7, 17, 3], [2, 4])
        integer :: me, ni, i
        integer, dimension(size(values,1)) :: my_val, expected

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(ni)

        my_val = values(:, mod(me-1, size(values,2))+1)
        call prif_co_min(my_val)

        expected = minval(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), dim=2)
        result_ = assert_equals(int(expected), int(my_val))
    end function

    function check_8_bit_integer() result(result_)
        type(result_t) :: result_

        integer(c_int8_t), parameter :: values(*,*) = reshape(int([1, -19, 5, 13, 11, 7, 17, 3],c_int8_t), [2, 4])
        integer :: me, ni, i
        integer(c_int8_t), dimension(size(values,1)) :: my_val, expected

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(ni)

        my_val = values(:, mod(me-1, size(values,2))+1)
        call prif_co_min(my_val)

        expected = minval(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), dim=2)
        result_ = assert_equals(int(expected), int(my_val))
    end function

    function check_16_bit_integer() result(result_)
        type(result_t) :: result_

        integer(c_int16_t), parameter :: values(*,*) = reshape(int([1, -19, 5, 13, 11, 7, 17, 3],c_int16_t), [2, 4])
        integer :: me, ni, i
        integer(c_int16_t), dimension(size(values,1)) :: my_val, expected

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(ni)

        my_val = values(:, mod(me-1, size(values,2))+1)
        call prif_co_min(my_val)

        expected = minval(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), dim=2)
        result_ = assert_equals(int(expected), int(my_val))
    end function

    function check_32_bit_integer() result(result_)
        type(result_t) :: result_

        integer(c_int32_t), parameter :: values(*) = [1, -19, 5, 13, 11, 7, 17, 3]
        integer :: me, ni, i
        integer(c_int32_t) :: my_val, expected

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(ni)

        my_val = values(mod(me-1, size(values))+1)
        call prif_co_min(my_val)

        expected = minval([(values(mod(i-1,size(values))+1), i = 1, ni)])
        result_ = assert_equals(expected, my_val)
    end function

    function check_64_bit_integer() result(result_)
        type(result_t) :: result_

        integer(c_int64_t), parameter :: values(*,*) = reshape([1, -19, 5, 13, 11, 7, 17, 3], [2, 4])
        integer :: me, ni, i
        integer(c_int64_t), dimension(size(values,1)) :: my_val, expected

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(ni)

        my_val = values(:, mod(me-1, size(values,2))+1)
        call prif_co_min(my_val)

        expected = minval(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), dim=2)
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
        call prif_co_min(my_val)

        expected = minval(reshape([(values(:,:,mod(i-1,size(values,3))+1), i = 1, ni)], [size(values,1), size(values,2), ni]), dim=3)
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
        call prif_co_min(my_val)

        expected = minval(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), dim=2)
        result_ = assert_equals(expected, my_val)
    end function

    function check_character() result(result_)
        type(result_t) result_
        character(len=*), parameter :: values(*) = &
            [ "To be   ","or not  " &
            , "to      ","be.     " &
            , "that    ","is      " &
            , "the     ","question"]
        integer :: me, ni, i
        character(len=len(values)) :: my_val, expected

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(ni)

        my_val = values(mod(me-1, size(values))+1)
        call prif_co_min_character(my_val)

        expected = minval([(values(mod(i-1,size(values))+1), i = 1, ni)])
        result_ = assert_equals(expected, my_val)
    end function

end module caf_co_min_test
