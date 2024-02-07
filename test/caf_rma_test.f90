module caf_rma_test
    use iso_c_binding, only: c_ptr, c_intmax_t, c_size_t, c_null_funptr, c_f_pointer
    use prif, only: &
            prif_coarray_handle, &
            prif_allocate, &
            prif_num_images, &
            prif_put, &
            prif_sync_all, &
            prif_this_image
    use veggies, only: result_t, test_item_t, assert_equals, describe, it

    implicit none
    private
    public :: test_prif_rma
contains
    function test_prif_rma() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
            "PRIF RMA", &
            [ it("can send value to another image", check_put) &
            ])
    end function

    function check_put() result(result_)
        type(result_t) :: result_

        integer :: dummy_element, num_imgs, me, expected
        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer, pointer :: local_slice
        integer(c_intmax_t) :: lcobounds(1), ucobounds(1), neighbor

        call prif_num_images(image_count=num_imgs)
        lcobounds(1) = 1
        ucobounds(1) = num_imgs
        call prif_allocate( &
                lcobounds = lcobounds, &
                ucobounds = ucobounds, &
                lbounds = [integer(c_intmax_t)::], &
                ubounds = [integer(c_intmax_t)::], &
                element_length = int(storage_size(dummy_element)/8, c_size_t), &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call c_f_pointer(allocated_memory, local_slice)

        call prif_this_image(image_index=me)
        neighbor = merge(me+1, 1, me < num_imgs)
        expected = merge(me-1, num_imgs, me > 1)

        call prif_put( &
                coarray_handle = coarray_handle, &
                coindices = [neighbor], &
                value = me, &
                first_element_addr = allocated_memory)
        call prif_sync_all

        result_ = assert_equals(expected, local_slice)
    end function
end module