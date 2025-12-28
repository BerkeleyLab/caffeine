#include "test-utils.F90"
#include "julienne-assert-macros.h"

module prif_co_reduce_test_m
  use iso_c_binding, only: c_ptr, c_funptr, c_size_t, c_f_pointer, c_f_procpointer, c_funloc, c_loc, c_null_ptr, c_associated, c_int8_t
  use prif, only : prif_co_reduce, prif_co_reduce_cptr, prif_num_images, prif_this_image_no_coarray, prif_operation_wrapper_interface
  use julienne_m, only : &
     call_julienne_assert_ &
    ,operator(.all.) &
    ,operator(.also.) &
    ,operator(.approximates.) &
    ,operator(.equalsExpected.) &
    ,operator(.within.) &
    ,operator(//) &
    ,usher &
    ,string_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t
  implicit none

  private
  public :: prif_co_reduce_test_t

  type, extends(test_t) :: prif_co_reduce_test_t
  contains
    procedure, nopass, non_overridable :: subject
    procedure, nopass, non_overridable :: results
  end type

  type :: pair
    integer :: fst
    real :: snd
  end type

#if HAVE_PARAM_DERIVED
  type :: array(length)
    integer, len :: length = 2
    integer :: elements(length)
  end type

  type :: reduction_context_data
    type(c_funptr) :: user_op
    integer :: length
  end type
#endif

  integer, target :: dummy

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = "prif_co_reduce"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(prif_co_reduce_test_t) prif_co_reduce_test

    allocate(test_results, source = prif_co_reduce_test%run([ &
       test_description_t("performing a logical .and. reduction", usher(check_logical)) &
      ,test_description_t("performing a derived type reduction", usher(check_derived_type_reduction)) &
#if HAVE_PARAM_DERIVED
      ,test_description_t("performing a parameterized derived type reduction", usher(check_type_parameter_reduction)) &
#endif
    ]))
  end function

  function check_logical() result(diag)
    type(test_diagnosis_t) :: diag
    logical :: val
    integer :: me
    procedure(prif_operation_wrapper_interface), pointer :: op
    diag = .true.
    op => and_wrapper

    val = .true.
    call prif_co_reduce(val, op, c_null_ptr)
    ALSO(val)

    call prif_this_image_no_coarray(this_image=me)
    if (me == 1) then
      val = .false.
    end if
    call prif_co_reduce(val, op, c_null_ptr)
    ALSO(.not. val)
  end function

  subroutine and_wrapper(arg1, arg2_and_out, count, cdata) bind(C)
    type(c_ptr), intent(in), value :: arg1, arg2_and_out
    integer(c_size_t), intent(in), value :: count
    type(c_ptr), intent(in), value :: cdata

    logical, pointer :: lhs(:), rhs_and_result(:)
    integer(c_size_t) :: i

    if (count == 0) return
    ! this expression is buggy as of Julienne 3.6.0 (julienne#166)
    !call_julienne_assert(cdata .equalsExpected. c_null_ptr)
    call_julienne_assert(.not. c_associated(cdata))
    call c_f_pointer(arg1, lhs, [count])
    call c_f_pointer(arg2_and_out, rhs_and_result, [count])
    do i = 1, count
      rhs_and_result(i) = lhs(i).and.rhs_and_result(i)
    end do
  end subroutine

  function check_derived_type_reduction() result(diag)
    type(test_diagnosis_t) :: diag
    type(pair), parameter :: values(*,*) = reshape( &
        [ pair(1, 53.), pair(3, 47.) &
        , pair(5, 43.), pair(7, 41.) &
        , pair(11, 37.), pair(13, 31.) &
        , pair(17, 29.), pair(19, 23.) &
        ], &
        [2, 4])
    integer :: me, ni, i
    type(pair), dimension(size(values,1)) :: my_val, expected
    type(pair), dimension(:,:), allocatable :: tmp
    procedure(prif_operation_wrapper_interface), pointer :: op
    real, parameter :: tolerance = 0D0

    diag = .true.

    op => pair_adder
    call prif_this_image_no_coarray(this_image=me)
    call prif_num_images(ni)

    my_val = values(:, mod(me-1, size(values,2))+1)
    call prif_co_reduce(my_val, op, c_loc(dummy))

    allocate(tmp(size(values,1),ni))
    tmp = reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni])
#if defined(__GFORTRAN__)
    ! gfortran 14 lacks the F18 intrinsic REDUCE()
    block
      integer :: j
      do i = 1, size(tmp,1)
        expected(i) = tmp(i,1)
        do j = 2, size(tmp,2)
          expected(i) = add_pair(expected(i), tmp(i,j))
        end do
      end do
    end block
#else
    expected = reduce(tmp, add_pair, dim=2)
#endif
    ALSO(.all. (my_val%fst .equalsExpected. expected%fst))
    ALSO(.all. (my_val%snd .approximates. expected%snd .within. tolerance))

    ! now repeat the same test using the prif_co_reduce_cptr variant:
    my_val = values(:, mod(me-1, size(values,2))+1)
    block
      integer(c_size_t) :: element_size, element_count
      integer(c_int8_t), allocatable, target :: bytes(:)
      element_size = storage_size(my_val(1))/8
      element_count = size(my_val)
      bytes = transfer(my_val, bytes)
      call prif_co_reduce_cptr(c_loc(bytes), element_size, element_count, op, c_loc(dummy))
      my_val = transfer(bytes, my_val, element_count)
    end block
    ALSO(.all. (my_val%fst .equalsExpected. expected%fst))
    ALSO(.all. (my_val%snd .approximates. expected%snd .within. tolerance))

  end function

  pure function add_pair(lhs, rhs) result(total)
    type(pair), intent(in) :: lhs, rhs
    type(pair) :: total

    total%fst = lhs%fst + rhs%fst
    total%snd = lhs%snd + rhs%snd
  end function

  subroutine pair_adder(arg1, arg2_and_out, count, cdata) bind(C)
    type(c_ptr), intent(in), value :: arg1, arg2_and_out
    integer(c_size_t), intent(in), value :: count
    type(c_ptr), intent(in), value :: cdata

    type(pair), pointer :: lhs(:), rhs_and_result(:)
    integer(c_size_t) :: i

    if (count == 0) return
    call_julienne_assert(cdata .equalsExpected. c_loc(dummy))
    call c_f_pointer(arg1, lhs, [count])
    call c_f_pointer(arg2_and_out, rhs_and_result, [count])
    do i = 1, count
      rhs_and_result(i) = add_pair(lhs(i), rhs_and_result(i))
    end do
  end subroutine

#if HAVE_PARAM_DERIVED
! As of LLVM21, flang does not implement the types used by this test:
! flang/lib/Lower/ConvertType.cpp:482: not yet implemented: parameterized derived types

! Gfortran 14.2..15.2 also lack the type support for this test:
! Error: Derived type 'pdtarray' at (1) is being used before it is defined

  function check_type_parameter_reduction() result(diag)
    type(test_diagnosis_t) :: diag
    type(array), parameter :: values(*,*) = reshape( &
        [ array(elements=[1, 53]), array(elements=[3, 47]) &
        , array(elements=[5, 43]), array(elements=[7, 41]) &
        , array(elements=[11, 37]), array(elements=[13, 31]) &
        , array(elements=[17, 29]), array(elements=[19, 23]) &
        ], &
        [2, 4])
    integer :: me, ni, i
    type(array(values%length)), dimension(size(values,1)) :: my_val, expected
    procedure(prif_operation_wrapper_interface), pointer :: op
    type(reduction_context_data), target :: context

    diag = .true.

    op => array_wrapper
    context%user_op = c_funloc(add_array)
    context%length = values(1,1)%length
    call prif_this_image_no_coarray(this_image=me)
    call prif_num_images(ni)

    my_val = values(:, mod(me-1, size(values,2))+1)

#  if ALLOW_ASSUMED_TYPE_PDT
    ! Ideally here we'd directly pass the user data `my_val` to prif_co_reduce as follows:
      call prif_co_reduce(my_val, op, c_loc(context))
    ! Unfortunately the code above is not strictly standards-conformant, because Fortran forbids
    ! passing an actual argument of derived type with type parameters to a procedure where the
    ! corresponding dummy argument has assumed type (the first argument to `prif_co_reduce`).
    ! Example errors from gfortran and flang:
    ! error: Actual argument associated with TYPE(*) dummy argument 'a=' may not have a parameterized derived type
    ! Error: Actual argument at (1) to assumed-type dummy has type parameters or is of derived type with type-bound or FINAL procedures
#  else
    ! So instead, we stage the data through an type-erased buffer and call the _cptr variant
    block
      integer(c_size_t) :: element_size, element_count
      integer(c_int8_t), allocatable, target :: bytes(:)
      element_size = storage_size(my_val(1))/8
      element_count = size(my_val)
      bytes = transfer(my_val, bytes)
      call prif_co_reduce_cptr(c_loc(bytes), element_size, element_count, op, c_loc(context))
      my_val = transfer(bytes, my_val, element_count)
    end block
#  endif

    expected = reduce(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), add_array, dim=2)
    do i = 1, size(my_val)
      ALSO(.all. (my_val(i)%elements .equalsExpected. expected(i)%elements))
    end do
  end function

  pure function add_array(lhs, rhs) result(total)
    type(array), intent(in) :: lhs, rhs
    type(array) :: total

    total%elements = lhs%elements + rhs%elements
  end function

  subroutine array_wrapper(arg1, arg2_and_out, count, cdata) bind(C)
    type(c_ptr), intent(in), value :: arg1, arg2_and_out
    integer(c_size_t), intent(in), value :: count
    type(c_ptr), intent(in), value :: cdata

    type(reduction_context_data), pointer :: context

    if (count == 0) return
    call c_f_pointer(cdata, context)
    block
      abstract interface
        pure function op_interface(lhs, rhs) result(res)
          import :: array, context
          implicit none
          type(array(context%length)), intent(in) :: lhs, rhs
          type(array(context%length)) :: res
        end function
      end interface
      procedure(op_interface), pointer :: op
      type(array(context%length)), pointer :: lhs(:), rhs_and_result(:)
      integer(c_size_t) :: i

      call c_f_procpointer(context%user_op, op)
      call c_f_pointer(arg1, lhs, [count])
      call c_f_pointer(arg2_and_out, rhs_and_result, [count])
      do i = 1, count
        rhs_and_result(i) = op(lhs(i), rhs_and_result(i))
      end do
    end block
  end subroutine
#endif /* HAVE_PARAM_DERIVED */

end module prif_co_reduce_test_m
