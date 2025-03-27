module caf_allocate_test
  use prif, only : &
      prif_allocate_coarray, prif_deallocate_coarray, &
      prif_allocate, prif_deallocate, &
      prif_coarray_handle, prif_num_images, prif_size_bytes, &
      prif_set_context_data, prif_get_context_data
  use veggies, only: result_t, test_item_t, assert_that, assert_equals, describe, it
  use iso_c_binding, only: &
      c_ptr, c_int, c_int64_t, c_size_t, c_funptr, c_null_funptr, &
      c_f_pointer, c_null_ptr, c_loc, c_sizeof, c_associated

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
        , it("allocate, use and deallocate an integer array coarray with a corank of 2", &
              check_allocate_integer_array_coarray_with_corank2) &
        , it("allocate, use and deallocate memory non-symmetrically", &
              check_allocate_non_symmetric) &
      ])
  end function

  function check_allocate_integer_scalar_coarray_with_corank1() result(result_)
    type(result_t) :: result_

    ! Allocate memory for an integer scalar single corank coarray, such as the following decl
    ! integer :: coarr[*]

    integer(kind=c_int64_t), dimension(1) :: lcobounds, ucobounds
    integer :: dummy_element, num_imgs
    type(prif_coarray_handle) :: coarray_handle
    type(c_ptr) :: allocated_memory
    integer, pointer :: local_slice
    integer(c_size_t) :: data_size, query_size

    call prif_num_images(num_images=num_imgs)
    lcobounds(1) = 1
    ucobounds(1) = num_imgs

    allocated_memory = c_null_ptr
    local_slice => null()
    result_ = assert_that(.not.associated(local_slice))

    data_size = storage_size(dummy_element)/8
    call prif_allocate_coarray( &
      lcobounds, ucobounds, data_size, c_null_funptr, &
      coarray_handle, allocated_memory)

    call c_f_pointer(allocated_memory, local_slice)
    result_ = result_ .and. assert_that(associated(local_slice))

    local_slice = 42
    result_ = result_ .and. assert_equals(42, local_slice)

    call prif_size_bytes(coarray_handle, data_size=query_size)
    result_ = result_ .and. assert_that(query_size == data_size, "prif_size_bytes is valid")

    block ! Check prif_{set,get}_context_data
      integer, target :: dummy(10), i
      type(c_ptr) :: expect, actual
      do i = 1,10 
        expect = c_loc(dummy(i))
        actual = c_null_ptr
        call prif_set_context_data(coarray_handle, expect)
        call prif_get_context_data(coarray_handle, actual)
        result_ = result_ .and. &
          assert_that(c_associated(expect, actual), "prif_{set,get}_context_data are working")
      end do
    end block

    call prif_deallocate_coarray([coarray_handle])

  end function

  function check_allocate_non_symmetric() result(result_)
    type(result_t) :: result_

    type(c_ptr) :: allocated_memory
    integer(c_int), pointer :: local_slice

    call prif_allocate(sizeof(local_slice), allocated_memory)
    call c_f_pointer(allocated_memory, local_slice)

    local_slice = 42
    result_ = assert_equals(42, local_slice)

    call prif_deallocate(c_loc(local_slice))
  end function

  function check_allocate_integer_array_coarray_with_corank2() result(result_)
    type(result_t) :: result_

    ! Allocate memory for an integer scalar single corank coarray, such as the following decl
    ! integer :: coarr(10)[4][*]

    integer(kind=c_int64_t), dimension(2) :: lcobounds, ucobounds
    integer :: dummy_element, num_imgs, i
    type(prif_coarray_handle) :: coarray_handle
    type(c_ptr) :: allocated_memory
    integer, pointer :: local_slice(:)
    integer(c_size_t) :: data_size, query_size

    call prif_num_images(num_images=num_imgs)
    lcobounds(1) = 1
    ucobounds(1) = 4
    lcobounds(2) = 1
    ucobounds(2) = num_imgs

    allocated_memory = c_null_ptr
    local_slice => null()
    result_ = assert_that(.not.associated(local_slice))

    data_size = 10*storage_size(dummy_element)/8
    call prif_allocate_coarray( &
      lcobounds, ucobounds, data_size, c_null_funptr, &
      coarray_handle, allocated_memory)

    call prif_size_bytes(coarray_handle, data_size=query_size)
    result_ = result_ .and. assert_that(query_size == data_size, "prif_size_bytes is valid")

    call c_f_pointer(allocated_memory, local_slice, [10])
    result_ = result_ .and. assert_that(associated(local_slice))

    do i = 1,10 
      local_slice(i) = i*i
    end do
    do i = 1,10 
      result_ = result_ .and. assert_equals(i*i, local_slice(i))
    end do

    block ! Check prif_{set,get}_context_data
      integer, target :: dummy(10), i
      type(c_ptr) :: expect, actual
      do i = 1,10 
        expect = c_loc(dummy(i))
        actual = c_null_ptr
        call prif_set_context_data(coarray_handle, expect)
        call prif_get_context_data(coarray_handle, actual)
        result_ = result_ .and. &
          assert_that(c_associated(expect, actual), "prif_{set,get}_context_data are working")
      end do
    end block

    call prif_deallocate_coarray([coarray_handle])

  end function
end module caf_allocate_test
