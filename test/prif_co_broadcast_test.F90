module caf_co_broadcast_test
  use prif, only : prif_co_broadcast, prif_num_images, prif_this_image_no_coarray
  use veggies, only : result_t, test_item_t, describe, it, assert_equals, assert_that

  implicit none
  private
  public :: test_prif_co_broadcast

  type object_t
    integer i
    logical fallacy
    character(len=len("fooey")) actor
    complex issues
  end type

  interface operator(==)
    module procedure equals
  end interface

contains

  function test_prif_co_broadcast() result(tests)
    type(test_item_t) tests
  
    tests = describe( &
      "The prif_co_broadcast subroutine", &
      [ it("broadcasts a default integer scalar with no optional arguments present", broadcast_default_integer_scalar) &
       ,it("broadcasts a derived type scalar with no allocatable components", broadcast_derived_type) &
    ])
  end function

  logical pure function equals(lhs, rhs) 
    type(object_t), intent(in) :: lhs, rhs 
    equals = all([ &
      lhs%i == rhs%i &
     ,lhs%fallacy .eqv. rhs%fallacy &
     ,lhs%actor == rhs%actor &
     ,lhs%issues == rhs%issues &
    ])
  end function

  function broadcast_default_integer_scalar() result(result_)
    type(result_t) result_
    integer iPhone, me
    integer, parameter :: source_value = 7779311, junk = -99

    call prif_this_image_no_coarray(this_image=me)
    iPhone = merge(source_value, junk, me==1)
    call prif_co_broadcast(iPhone, source_image=1)
    result_ = assert_equals(source_value, iPhone)
  end function

  function broadcast_derived_type() result(result_)
    type(result_t) result_
    type(object_t) object
    integer :: me, ni

    call prif_this_image_no_coarray(this_image=me)
    call prif_num_images(num_images=ni)
    object = object_t(me, .false., "gooey", me*(1.,0.))
    call prif_co_broadcast(object, source_image=ni)
    associate(expected_object => object_t(ni, .false., "gooey", ni*(1.,0.)))
      result_ = assert_that(expected_object == object, "co_broadcast derived type")
    end associate

  end function

end module caf_co_broadcast_test
