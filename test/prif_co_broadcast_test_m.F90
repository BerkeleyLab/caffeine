! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#ifndef __GFORTRAN__
  #define F2008_PROC_PTR_ARG_ASSOCIATION
#endif

module prif_co_broadcast_test_m
  use prif, only : prif_co_broadcast, prif_num_images, prif_this_image_no_coarray
  use julienne_m, only : test_t, test_result_t, test_description_t, test_description_substring
#ifndef F2008_PROC_PTR_ARG_ASSOCIATION
  use julienne_m, only : test_function_i
#endif
  implicit none

  private
  public :: prif_co_broadcast_test_t

  type, extends(test_t) :: prif_co_broadcast_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
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

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The prif_co_broadcast subroutine"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#ifdef F2008_PROC_PTR_ARG_ASSOCIATION
    test_descriptions = [ &
       test_description_t("broadcasts a default integer scalar with no optional arguments", broadcast_default_integer) &
      ,test_description_t("broadcasts a derived type scalar with no allocatable components", broadcast_derived_type) &
    ]
#else
    procedure(test_function_i), pointer :: broadcast_default_integer_ptr, broadcast_derived_type_ptr

    broadcast_default_integer_ptr => broadcast_default_integer
    broadcast_derived_type_ptr => broadcast_derived_type

    test_descriptions = [ &
      test_description_t("broadcasting a default integer when called without optional arguments", broadcast_default_integer_ptr) &
     ,test_description_t("broadcasting a derived type with no allocatable components", broadcast_derived_type_ptr) &
    ]
#endif

    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0  &
      .or. test_descriptions%contains_text(test_description_substring))

    test_results = test_descriptions%run()
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

  function broadcast_default_integer() result(test_passes)
    logical test_passes
    integer iPhone, me
    integer, parameter :: source_value = 7779311, junk = -99

    call prif_this_image_no_coarray(this_image=me)
    iPhone = merge(source_value, junk, me==1)
    call prif_co_broadcast(iPhone, source_image=1)
    test_passes = source_value == iPhone
  end function

  function broadcast_derived_type() result(test_passes)
    logical test_passes
    type(object_t) object
    integer :: me, ni

    call prif_this_image_no_coarray(this_image=me)
    call prif_num_images(num_images=ni)
    object = object_t(me, .false., "gooey", me*(1.,0.))
    call prif_co_broadcast(object, source_image=ni)
    associate(expected_object => object_t(ni, .false., "gooey", ni*(1.,0.)))
      test_passes = expected_object == object
    end associate

  end function

end module prif_co_broadcast_test_m
