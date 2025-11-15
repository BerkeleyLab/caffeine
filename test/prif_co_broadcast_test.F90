module prif_co_broadcast_test_m
  use prif, only : prif_co_broadcast, prif_num_images, prif_this_image_no_coarray
  use julienne_m, only : &
     usher &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t &
    ,operator(//) &
    ,operator(.expect.) &
    ,operator(.equalsExpected.)

  implicit none
  private
  public :: prif_co_broadcast_test_t

  type, extends(test_t) :: prif_co_broadcast_test_t
  contains
    procedure, nopass, non_overridable  :: subject
    procedure, nopass, non_overridable  :: results
  end type

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

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = "The prif_co_broadcast subroutine"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(prif_co_broadcast_test_t) prif_co_broadcast_test

    test_results = prif_co_broadcast_test%run([ &
       test_description_t("broadcasting a default integer scalar with no optional arguments present", usher(broadcast_default_integer_scalar)) &
      ,test_description_t("broadcasting a derived type scalar with no allocatable components", usher(broadcast_derived_type)) &
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

  function broadcast_default_integer_scalar() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer iPhone, me
    integer, parameter :: source_value = 7779311, junk = -99

    call prif_this_image_no_coarray(this_image=me)
    iPhone = merge(source_value, junk, me==1)
    call prif_co_broadcast(iPhone, source_image=1)
    test_diagnosis = iPhone .equalsExpected. source_value 
  end function

  function broadcast_derived_type() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    type(object_t) object
    integer me, ni

    call prif_this_image_no_coarray(this_image=me)
    call prif_num_images(num_images=ni)
    object = object_t(me, .false., "gooey", me*(1.,0.))
    call prif_co_broadcast(object, source_image=ni)
    associate(expected_object => object_t(ni, .false., "gooey", ni*(1.,0.)))
      test_diagnosis = .expect. (object == expected_object) // "co_broadcast derived type"
    end associate
  end function

end module prif_co_broadcast_test_m
