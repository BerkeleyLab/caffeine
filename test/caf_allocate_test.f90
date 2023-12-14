module caf_allocate_test
  use prif, only : prif_allocate, prif_coarray_handle
  use veggies, only: result_t, test_item_t, assert_that, assert_equals, describe, it
  use iso_c_binding, only: c_ptr, c_int, c_intmax_t, c_size_t, c_funptr, c_null_funptr, c_f_pointer, c_null_ptr

  implicit none
  private
  public :: test_prif_allocate

contains
  function test_prif_allocate() result(tests)
    type(test_item_t) :: tests

    tests = &
      describe( &
        "The prif_allocate subroutine", &
        [ it("allocates an integer scalar coarray with a corank of 1", &
              check_allocate_integer_scalar_coarray_with_corank1) &
      ])
  end function

  function check_allocate_integer_scalar_coarray_with_corank1() result(result_)
    type(result_t) :: result_

    ! Allocate memory for an integer scalar single corank coarray, such as the following decl
    ! integer :: coarr[*]

    integer(kind=c_intmax_t), dimension(1) :: lcobounds, ucobounds
    integer(kind=c_intmax_t), dimension(0), parameter :: lbounds = [integer(kind=c_intmax_t) ::]
    integer(kind=c_intmax_t), dimension(0), parameter :: ubounds = [integer(kind=c_intmax_t) ::]
    integer :: dummy_element
    type(prif_coarray_handle) :: coarray_handle
    type(c_ptr) :: allocated_memory
    integer, pointer :: local_slice

    allocated_memory = c_null_ptr
    local_slice => null()
    result_ = assert_that(.not.associated(local_slice))

    call prif_allocate( &
      lcobounds, ucobounds, lbounds, ubounds, int(storage_size(dummy_element)/8, c_size_t), c_null_funptr, &
      coarray_handle, allocated_memory)

    call c_f_pointer(allocated_memory, local_slice)
    result_ = result_ .and. assert_that(associated(local_slice))

  end function

end module caf_allocate_test
