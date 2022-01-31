module caf_co_broadcast_test
  use caffeine_m, only : caf_co_broadcast, caf_num_images, caf_this_image
  use vegetables, only : result_t, test_item_t, assert_equals, describe, it, assert_equals
  use iso_c_binding, only : c_bool

  implicit none
  private
  public :: test_caf_co_broadcast

contains

  function test_caf_co_broadcast() result(tests)
    type(test_item_t) tests
  
    tests = describe( &
      "The caf_co_broadcast subroutine", &
      [ it("broadcasts a default integer scalar with no optional arguments present", broadcast_default_integer_scalar) &
    ])
  end function

  function broadcast_default_integer_scalar() result(result_)
    type(result_t) result_
    integer iPhone
    integer, parameter :: source_value = 7779311, junk = -99

    iPhone = merge(source_value, junk, caf_this_image()==1)
    call caf_co_broadcast(iPhone, source_image=1)
    result_ = assert_equals(source_value, iPhone)
  end function

end module caf_co_broadcast_test
