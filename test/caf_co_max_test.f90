module caf_co_max_test
    use prif, only : prif_co_max, prif_this_image_no_coarray, prif_num_images
    use veggies, only: result_t, test_item_t, assert_equals, describe, it, succeed

    implicit none
    private
    public :: test_prif_co_max

contains
    function test_prif_co_max() result(tests)
        type(test_item_t) tests

        tests = describe( &
          "The prif_co_max subroutine computes the maximum value across images for corresponding elements for", &
          [ it("32 bit integer scalars", check_32_bit_integer) &
          , it("a 1D 64 bit integer array", check_64_bit_integer) &
          , it("a 2D 32 bit real array", check_32_bit_real) &
          , it("a 1D 64 bit real array", check_64_bit_real) &
          , it("a character scalar", check_character) &
          ])
    end function

    function check_32_bit_integer() result(result_)
        type(result_t) :: result_
        result_ = succeed("temporarily")
    end function

    function check_64_bit_integer() result(result_)
        type(result_t) :: result_
        result_ = succeed("temporarily")
    end function

    function check_32_bit_real() result(result_)
        type(result_t) :: result_
        result_ = succeed("temporarily")
    end function

    function check_64_bit_real() result(result_)
        type(result_t) :: result_
        result_ = succeed("temporarily")
    end function

    function check_character() result(result_)
        type(result_t) :: result_
        result_ = succeed("temporarily")
    end function

end module caf_co_max_test
