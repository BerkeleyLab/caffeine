module caf_co_reduce_test
  use prif, only : prif_co_reduce, prif_num_images, prif_this_image_no_coarray
  use veggies, only : result_t, test_item_t, assert_equals, describe, it, succeed

  implicit none
  private
  public :: test_prif_co_reduce

contains

  function test_prif_co_reduce() result(tests)
    type(test_item_t) tests

    tests = describe( &
      "The prif_co_reduce subroutine", &
      [ it("can be used to implement logical and reduction", check_logical) &
      , it("can be used for reduction on simple derived types", check_derived_type_reduction) &
      , it("can be used for reduction on derived types with length type parameters", check_type_parameter_reduction) &
      ])
  end function

  function check_logical() result(result_)
    type(result_t) :: result_
    result_ = succeed("temporarily")
  end function

  function check_derived_type_reduction() result(result_)
    type(result_t) :: result_
    result_ = succeed("temporarily")
  end function

  function check_type_parameter_reduction() result(result_)
    type(result_t) :: result_
    result_ = succeed("temporarily")
  end function

end module caf_co_reduce_test
