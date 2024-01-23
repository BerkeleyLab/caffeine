module caf_allocate_test
  use prif, only : &
      prif_allocate, prif_deallocate, &
      prif_allocate_non_symmetric, prif_deallocate_non_symmetric, &
      prif_coarray_handle, prif_num_images
  use veggies, only: result_t, test_item_t, assert_that, assert_equals, describe, it
  use iso_c_binding, only: &
      c_ptr, c_int, c_intmax_t, c_size_t, c_funptr, c_null_funptr, &
      c_f_pointer, c_null_ptr, c_loc, c_sizeof

  implicit none
  private
  public :: test_prif_allocate

contains
  function test_prif_allocate() result(tests)
    type(test_item_t) :: tests

    tests = &
      describe( &
        "PRIF allocation can", &
        [ it("allocate, use and deallocate an integer scalar coarray with a corank of 1", &
              check_allocate_integer_scalar_coarray_with_corank1) &
        , it("allocate, use and deallocate memory non-symmetrically", &
              check_allocate_non_symmetric) &
      ])
  end function

  function check_allocate_integer_scalar_coarray_with_corank1() result(result_)
    type(result_t) :: result_

    ! Allocate memory for an integer scalar single corank coarray, such as the following decl
    ! integer :: coarr[*]

    integer(kind=c_intmax_t), dimension(1) :: lcobounds, ucobounds
    integer(kind=c_intmax_t), dimension(0), parameter :: lbounds = [integer(kind=c_intmax_t) ::]
    integer(kind=c_intmax_t), dimension(0), parameter :: ubounds = [integer(kind=c_intmax_t) ::]
    integer :: dummy_element, num_imgs
    type(prif_coarray_handle) :: coarray_handle
    type(c_ptr) :: allocated_memory
    integer, pointer :: local_slice

    call prif_num_images(image_count=num_imgs)
    lcobounds(1) = 1
    ucobounds(1) = num_imgs

    allocated_memory = c_null_ptr
    local_slice => null()
    result_ = assert_that(.not.associated(local_slice))

    call prif_allocate( &
      lcobounds, ucobounds, lbounds, ubounds, int(storage_size(dummy_element)/8, c_size_t), c_null_funptr, &
      coarray_handle, allocated_memory)

    call c_f_pointer(allocated_memory, local_slice)
    result_ = result_ .and. assert_that(associated(local_slice))

    local_slice = 42
    result_ = result_ .and. assert_equals(42, local_slice)

    call prif_deallocate([coarray_handle])

  end function

  function check_allocate_non_symmetric() result(result_)
    type(result_t) :: result_

    type(c_ptr) :: allocated_memory
    integer(c_int), pointer :: local_slice

    call prif_allocate_non_symmetric(sizeof(local_slice), allocated_memory)
    call c_f_pointer(allocated_memory, local_slice)

    local_slice = 42
    result_ = assert_equals(42, local_slice)

    call prif_deallocate_non_symmetric(c_loc(local_slice))
  end function

end module caf_allocate_test
