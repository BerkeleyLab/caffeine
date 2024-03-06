module caf_co_broadcast_test
  use prif, only : prif_co_broadcast, prif_num_images, prif_this_image
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
    module procedure equals_with_allocatable
  end interface
  
  type with_allocatable
    integer, allocatable :: i
  end type

contains

  function test_prif_co_broadcast() result(tests)
    type(test_item_t) tests
  
    tests = describe( &
      "The prif_co_broadcast subroutine", &
      [ it("broadcasts a default integer scalar with no optional arguments present", broadcast_default_integer_scalar) &
       ,it("broadcasts a derived type scalar with no allocatable components", broadcast_derived_type) &
       , it("broadcasts a derived type with an allocatable component", broadcast_with_allocatable) &
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
  
  logical pure function equals_with_allocatable(lhs, rhs)
    type(with_allocatable), intent(in) :: lhs, rhs
    if (allocated(lhs%i)) then
      if (allocated(rhs%i)) then
        equals_with_allocatable = lhs%i == rhs%i
      else
        equals_with_allocatable = .false.
      end if
    else
      equals_with_allocatable = .not.allocated(rhs%i)
    end if
  end function

  function broadcast_default_integer_scalar() result(result_)
    type(result_t) result_
    integer iPhone, me
    integer, parameter :: source_value = 7779311, junk = -99

    call prif_this_image(image_index=me)
    iPhone = merge(source_value, junk, me==1)
    call prif_co_broadcast(iPhone, source_image=1)
    result_ = assert_equals(source_value, iPhone)
  end function

  function broadcast_derived_type() result(result_)
    type(result_t) result_
    type(object_t) object
    integer :: me, ni

    call prif_this_image(image_index=me)
    call prif_num_images(image_count=ni)
    object = object_t(me, .false., "gooey", me*(1.,0.))
    call prif_co_broadcast(object, source_image=ni)
    associate(expected_object => object_t(ni, .false., "gooey", ni*(1.,0.)))
      result_ = assert_that(expected_object == object, "co_broadcast derived type")
    end associate

  end function
  
  function broadcast_with_allocatable() result(result_)
    type(result_t) :: result_
    
    integer, parameter :: expected_val = 42
    integer :: me
    type(with_allocatable) :: obj, expected
    
    expected%i = expected_val
    call prif_this_image(image_index=me)
    if (me == 1) obj%i = expected_val
    call prif_co_broadcast(obj, source_image=1)
    result_ = assert_that(expected == obj, "co_broadcast with allocatable component")
  end function

end module caf_co_broadcast_test
