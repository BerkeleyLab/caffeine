module caf_co_reduce_test
  use iso_c_binding, only: c_ptr, c_funptr, c_size_t, c_f_pointer, c_f_procpointer, c_funloc, c_loc, c_null_ptr
  use prif, only : prif_co_reduce, prif_num_images, prif_this_image_no_coarray, prif_operation_wrapper_interface
  use veggies, only : result_t, test_item_t, assert_equals, assert_not, assert_that, describe, it, succeed

  implicit none
  private
  public :: test_prif_co_reduce

  type :: pair
    integer :: fst
    real :: snd
  end type

  ! type :: array(length)
  !   integer, len :: length = 2
  !   integer :: elements(length)
  ! end type

  ! type :: reduction_context_data
  !   type(c_funptr) :: user_op
  !   integer :: length
  ! end type

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
    logical :: val
    integer :: me
    procedure(prif_operation_wrapper_interface), pointer :: op
    op => and_wrapper

    val = .true.
    call prif_co_reduce(val, op, c_null_ptr)
    result_ = assert_that(val)

    call prif_this_image_no_coarray(this_image=me)
    if (me == 1) then
      val = .false.
    end if
    call prif_co_reduce(val, op, c_null_ptr)
    result_ = result_.and.assert_not(val)
  end function

  subroutine and_wrapper(arg1, arg2_and_out, count, cdata) bind(C)
    type(c_ptr), intent(in), value :: arg1, arg2_and_out
    integer(c_size_t), intent(in), value :: count
    type(c_ptr), intent(in), value :: cdata

    logical, pointer :: lhs(:), rhs_and_result(:)
    integer(c_size_t) :: i

    if (count == 0) return
    call c_f_pointer(arg1, lhs, [count])
    call c_f_pointer(arg2_and_out, rhs_and_result, [count])
    do i = 1, count
      rhs_and_result(i) = lhs(i).and.rhs_and_result(i)
    end do
  end subroutine

  function check_derived_type_reduction() result(result_)
    type(result_t) :: result_
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

    op => pair_adder
    call prif_this_image_no_coarray(this_image=me)
    call prif_num_images(ni)

    my_val = values(:, mod(me-1, size(values,2))+1)
    call prif_co_reduce(my_val, op, c_null_ptr)

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
    result_ = &
        assert_equals(expected%fst, my_val%fst) &
        .and. assert_equals(real(expected%snd, kind=kind(0.d0)), real(my_val%snd, kind=kind(0.d0)))
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
    call c_f_pointer(arg1, lhs, [count])
    call c_f_pointer(arg2_and_out, rhs_and_result, [count])
    do i = 1, count
      rhs_and_result(i) = add_pair(lhs(i), rhs_and_result(i))
    end do
  end subroutine

  function check_type_parameter_reduction() result(result_)
    type(result_t) :: result_
    result_ = succeed("skip for now")
    
    ! type(array), parameter :: values(*,*) = reshape( &
    !     [ array([1, 53]), array([3, 47]) &
    !     , array([5, 43]), array([7, 41]) &
    !     , array([11, 37]), array([13, 31]) &
    !     , array([17, 29]), array([19, 23]) &
    !     ], &
    !     [2, 4])
    ! integer :: me, ni, i
    ! type(array(values%length)), dimension(size(values,1)) :: my_val, expected
    ! procedure(prif_operation_wrapper_interface), pointer :: op
    ! type(reduction_context_data), target :: context

    ! op => array_wrapper
    ! context%user_op = c_funloc(add_array)
    ! context%length = values%length
    ! call prif_this_image_no_coarray(this_image=me)
    ! call prif_num_images(ni)

    ! my_val = values(:, mod(me-1, size(values,2))+1)
    ! call prif_co_reduce(my_val, op, c_loc(context))

    ! expected = reduce(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), add_array, dim=2)
    ! do i = 1, size(expected)
    !   result_ = result_.and.assert_equals(expected(i)%elements, my_val(i)%elements)
    ! end do
  end function

  ! pure function add_array(lhs, rhs) result(total)
  !   type(array), intent(in) :: lhs, rhs
  !   type(array) :: total

  !   total%elements = lhs%elements + rhs%elements
  ! end function

  ! subroutine array_wrapper(arg1, arg2_and_out, count, cdata) bind(C)
  !   type(c_ptr), intent(in), value :: arg1, arg2_and_out
  !   integer(c_size_t), intent(in), value :: count
  !   type(c_ptr), intent(in), value :: cdata

  !   type(reduction_context_data), pointer :: context

  !   if (count == 0) return
  !   call c_f_pointer(cdata, context)
  !   block
  !     abstract interface
  !       pure function op_interface(lhs, rhs) result(res)
  !         import :: array, context
  !         implicit none
  !         type(array(context%length)), intent(in) :: lhs, rhs
  !         type(array(context%length)) :: res
  !       end function
  !     end interface
  !     procedure(op_interface), pointer :: op
  !     type(array(context%length)), pointer :: lhs(:), rhs_and_result(:)
  !     integer(c_size_t) :: i

  !     call c_f_procpointer(context%user_op, op)
  !     call c_f_pointer(arg1, lhs, [count])
  !     call c_f_pointer(arg2_and_out, rhs_and_result, [count])
  !     do i = 1, count
  !       rhs_and_result(i) = op(lhs(i), rhs_and_result(i))
  !     end do
  !   end block
  ! end subroutine

end module caf_co_reduce_test
