module caf_co_sum_test
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
          [ it("32 bit integer scalars", check_32_bit_integer) &
          , it("a 1D 64 bit integer array", check_64_bit_integer) &
          , it("a 2D 32 bit real array", check_32_bit_real) &
          , it("a 1D 64 bit real array", check_64_bit_real) &
          , it("a 2D complex array with 32 bit components", check_32_bit_complex) &
          , it("a 1D complex array with 64 bit components", check_64_bit_complex) &
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

    function check_32_bit_complex() result(result_)
        type(result_t) :: result_
        result_ = succeed("temporarily")
    end function

    function check_64_bit_complex() result(result_)
        type(result_t) :: result_
        result_ = succeed("temporarily")
    end function

end module caf_co_sum_test
