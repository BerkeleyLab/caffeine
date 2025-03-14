module caf_coarray_inquiry_test
    use prif, only : &
        prif_allocate_coarray, prif_deallocate_coarray, &
        prif_coarray_handle, prif_num_images, &
        prif_local_data_pointer, prif_size_bytes, &
        prif_lcobound_no_dim, prif_lcobound_with_dim, & 
        prif_ucobound_no_dim, prif_ucobound_with_dim, & 
        prif_coshape
    use veggies, only: result_t, test_item_t, assert_that, describe, it, succeed
    use iso_c_binding, only: &
        c_ptr, c_null_ptr, c_int64_t, c_int, c_size_t, c_null_funptr, c_associated

    implicit none
    private
    public :: test_coarray_inquiry
contains
    function test_coarray_inquiry() result(tests)
        type(test_item_t) :: tests

        tests = &
            describe( &
                "PRIF coarray inquiry functions", &
                [ describe( &
                    "prif_local_data_pointer", &
                    [ it( &
                        "returns the same pointer as when the coarray was allocated", &
                        check_prif_local_data_pointer) &
                    ]), &
                  describe( &
                    "PRIF coarrays", &
                    [ it("pass cobounds testing", check_cobounds) ]) &
                ])
    end function

    function check_prif_local_data_pointer() result(result_)
        type(result_t) :: result_

        integer(kind=c_int64_t), dimension(1) :: lcobounds, ucobounds
        integer :: dummy_element, num_imgs
        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocation_ptr, local_ptr

        call prif_num_images(num_images=num_imgs)
        lcobounds(1) = 1
        ucobounds(1) = num_imgs

        call prif_allocate_coarray( &
                lcobounds, &
                ucobounds, &
                int(storage_size(dummy_element)/8, c_size_t), &
                c_null_funptr, &
                coarray_handle, &
                allocation_ptr)
        call prif_local_data_pointer(coarray_handle, local_ptr)
        result_ = assert_that(c_associated(local_ptr, allocation_ptr))
        call prif_deallocate_coarray([coarray_handle])
    end function

    function check_cobound(corank) result(result_)
      type(result_t) :: result_
      integer(c_int), intent(in) :: corank
  
      ! Allocate memory for an integer scalar coarray with given corank
      ! and then test some queries on it
  
      integer :: num_imgs, i
      integer(kind=c_int64_t), dimension(corank) :: lcobounds, ucobounds, tmp_bounds
      integer(kind=c_int64_t) :: tmp_bound
      integer(kind=c_size_t), dimension(corank)  :: sizes
      type(prif_coarray_handle) :: coarray_handle
      type(c_ptr) :: allocated_memory
      integer(c_size_t) :: data_size, query_size
  
      result_ = succeed("")
  
      call prif_num_images(num_images=num_imgs)
      lcobounds(1) = 1
      ucobounds(1) = num_imgs
      do i = 2,corank
        lcobounds(i) = i
        ucobounds(i) = i*2
      end do
  
      allocated_memory = c_null_ptr
      data_size = 64 * corank
  
      call prif_allocate_coarray( &
        lcobounds, ucobounds, data_size, c_null_funptr, &
        coarray_handle, allocated_memory)
  
      result_ = result_ .and. &
        assert_that(c_associated(allocated_memory))
  
      call prif_size_bytes(coarray_handle, data_size=query_size)
      result_ = result_ .and. &
        assert_that(query_size == data_size, "prif_size_bytes is valid")
      
      call prif_lcobound_no_dim(coarray_handle, tmp_bounds)
      result_ = result_ .and. &
        assert_that(all(tmp_bounds == lcobounds), "prif_lcobound_no_dim is valid")
  
      call prif_ucobound_no_dim(coarray_handle, tmp_bounds)
      result_ = result_ .and. &
        assert_that(all(tmp_bounds == ucobounds), "prif_ucobound_no_dim is valid")
  
      do i = 1,corank
        call prif_lcobound_with_dim(coarray_handle, i, tmp_bound)
        result_ = result_ .and. &
          assert_that(tmp_bound == lcobounds(i), "prif_lcobound_with_dim is valid")
  
        call prif_ucobound_with_dim(coarray_handle, i, tmp_bound)
        result_ = result_ .and. &
          assert_that(tmp_bound == ucobounds(i), "prif_ucobound_with_dim is valid")
      end do
  
      call prif_coshape(coarray_handle, sizes)
      result_ = result_ .and. &
        assert_that(all(sizes == (ucobounds - lcobounds + 1)), "prif_coshape is valid")
  
      call prif_deallocate_coarray([coarray_handle])
    end function
  
    function check_cobounds() result(result_)
      type(result_t) :: result_
      integer(c_int) :: corank
  
      result_ = succeed("")
  
      do corank = 1, 15
        result_ = result_ .and. check_cobound(corank)
      end do
  
    end function
  
end module
