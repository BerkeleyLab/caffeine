#include "test-utils.F90"

module prif_co_broadcast_test_m
  use iso_c_binding, only: c_loc, c_size_t
  use prif, only : prif_co_broadcast, prif_co_broadcast_cptr, prif_num_images, prif_this_image_no_coarray
  use julienne_m, only : &
     usher &
    ,string_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t &
    ,operator(//) &
    ,operator(.also.) &
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
    sequence  ! guarantee components reside in flat linear storage
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
    test_subject = "prif_co_broadcast"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(prif_co_broadcast_test_t) prif_co_broadcast_test

    allocate(test_results, source = prif_co_broadcast_test%run([ &
       test_description_t("broadcasting a default integer scalar with no optional arguments present", usher(broadcast_default_integer_scalar)) &
      ,test_description_t("prif_co_broadcast of a derived type scalar with no allocatable components", usher(broadcast_derived_type)) &
      ,test_description_t("prif_co_broadcast_cptr of a derived type scalar with no allocatable components" &
#   if __LFORTRAN__ && __LFORTRAN_MAJOR__ == 0 && __LFORTRAN_MINOR__ <= 63
       ! test disabled for LFortran issue 11191
#   else
        , usher(broadcast_derived_type_cptr) &
#   endif
      ) &
    ]))
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

  function broadcast_default_integer_scalar() result(diag)
    type(test_diagnosis_t) :: diag
    integer, target :: a, me
    integer, parameter :: source_value = 7779311, junk = -99

    diag = .true.

    call prif_this_image_no_coarray(this_image=me)

    a = merge(source_value, junk, me==1)
    call prif_co_broadcast(a, source_image=1)
    ALSO(a .equalsExpected. source_value)

    a = merge(source_value*7, junk, me==1)
    call prif_co_broadcast_cptr(c_loc(a), size_in_bytes=storage_size(a,c_size_t)/8, source_image=1)
    ALSO(a .equalsExpected. source_value*7)
  end function

  function broadcast_derived_type() result(diag)
    type(test_diagnosis_t) :: diag
    type(object_t) :: object
    integer me, ni

    diag = .true.

    call prif_this_image_no_coarray(this_image=me)
    call prif_num_images(num_images=ni)

    object = object_t(me, .false., "gooey", me*(1.,0.))
    call prif_co_broadcast(object, source_image=ni)
    associate(expected_object => object_t(ni, .false., "gooey", ni*(1.,0.)))
      ALSO2(object == expected_object, "co_broadcast derived type")
    end associate
  end function

  function broadcast_derived_type_cptr() result(diag)
    type(test_diagnosis_t) :: diag
    type(object_t), target :: object
    integer me, ni

    diag = .true.

    call prif_this_image_no_coarray(this_image=me)
    call prif_num_images(num_images=ni)

    object = object_t(me, .true., "hooey", me*(10.,0.))
    call prif_co_broadcast_cptr(c_loc(object), storage_size(object,c_size_t)/8, source_image=ni)
    associate(expected_object => object_t(ni, .true., "hooey", ni*(10.,0.)))
      ALSO2(object == expected_object, "co_broadcast_cptr derived type")
    end associate
  end function

end module prif_co_broadcast_test_m
