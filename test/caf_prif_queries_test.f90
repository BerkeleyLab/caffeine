module caf_prif_queries_test
  use prif, only : &
      prif_allocate, prif_deallocate, prif_coarray_handle, prif_num_images, prif_this_image, prif_base_pointer
  use caffeine_h_m, only: as_int
  use veggies, only: result_t, test_item_t, assert_that, describe, it
  use iso_c_binding, only: c_ptr, c_int, c_intmax_t, c_size_t, c_null_funptr, c_intptr_t

  implicit none
  private

  public :: test_prif_queries

contains
  function test_prif_queries() result(tests)
    type(test_item_t) :: tests

    tests = &
      describe( &
        "PRIF queries can", &
        [ it("get a non null base pointer", &
              check_base_pointer_not_null) &
        , it("get same address passed into to prif_base_pointer when querying about the same image", &
              check_same_image_same_addr) &
      ])
  end function

  function check_base_pointer_not_null() result(result_)
    type(result_t) :: result_

    integer(c_intmax_t), dimension(1) :: ucobounds
    integer :: num_imgs
    integer(c_intptr_t) :: ptr
    type(prif_coarray_handle) :: coarray_handle
    type(c_ptr) :: allocated_mem

    call prif_num_images(image_count=num_imgs)
    ucobounds(1) = num_imgs

    call prif_allocate( &
      [1_c_intmax_t], ucobounds, [integer(kind=c_intmax_t) ::], [integer(kind=c_intmax_t) ::], 1_c_size_t, c_null_funptr, &
      coarray_handle, allocated_mem)

    call prif_base_pointer(coarray_handle, 2, ptr)
    result_ = assert_that(ptr .ne. 0)

    call prif_deallocate([coarray_handle])

  end function

  function check_same_image_same_addr() result(result_)
    type(result_t) :: result_

    integer(c_intmax_t), dimension(1) :: ucobounds
    integer :: num_imgs
    integer(c_int) :: this_img
    integer(c_intptr_t) :: ptr
    type(prif_coarray_handle) :: coarray_handle
    type(c_ptr) :: allocated_mem

    call prif_num_images(image_count=num_imgs)
    ucobounds(1) = num_imgs
    call prif_this_image(image_index=this_img)

    call prif_allocate( &
      [1_c_intmax_t], ucobounds, [integer(kind=c_intmax_t) ::], [integer(kind=c_intmax_t) ::], 1_c_size_t, c_null_funptr, &
      coarray_handle, allocated_mem)

    call prif_base_pointer(coarray_handle, this_img, ptr)

    result_ = assert_that(as_int(coarray_handle%info%coarray_data) .eq. ptr)

    call prif_deallocate([coarray_handle])
  end function

end module caf_prif_queries_test
