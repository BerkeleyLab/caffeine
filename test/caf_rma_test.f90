module caf_rma_test
    use iso_c_binding, only: &
            c_ptr, c_intmax_t, c_intptr_t, c_size_t, c_null_funptr, c_f_pointer, c_loc
    use prif, only: &
            prif_coarray_handle, &
            prif_allocate_coarray, &
            prif_deallocate_coarray, &
            prif_base_pointer, &
            prif_num_images, &
            prif_put, &
            prif_put_raw, &
            prif_get, &
            prif_get_indirect, &
            prif_sync_all, &
            prif_this_image_no_coarray
    use veggies, only: result_t, test_item_t, assert_equals, describe, it

    implicit none
    private
    public :: test_prif_rma
contains
    function test_prif_rma() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
            "PRIF RMA", &
            [ it("can send a value to another image", check_put) &
            , it("can send a value with raw interface", check_put_raw) &
            , it("can get a value from another image", check_get) &
            , it("can get a value with indirect interface", check_get_indirect) &
            ])
    end function

    function check_put() result(result_)
        type(result_t) :: result_

        integer :: dummy_element, num_imgs, me, expected
        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer, pointer :: local_slice
        integer(c_intmax_t) :: lcobounds(1), ucobounds(1), neighbor

        call prif_num_images(num_images=num_imgs)
        lcobounds(1) = 1
        ucobounds(1) = num_imgs
        call prif_allocate_coarray( &
                lcobounds = lcobounds, &
                ucobounds = ucobounds, &
                lbounds = [integer(c_intmax_t)::], &
                ubounds = [integer(c_intmax_t)::], &
                element_size = int(storage_size(dummy_element)/8, c_size_t), &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call c_f_pointer(allocated_memory, local_slice)

        call prif_this_image_no_coarray(this_image=me)
        neighbor = merge(me+1, 1, me < num_imgs)
        expected = merge(me-1, num_imgs, me > 1)

        call prif_put( &
                coarray_handle = coarray_handle, &
                cosubscripts = [neighbor], &
                value = me, &
                first_element_addr = allocated_memory)
        call prif_sync_all

        result_ = assert_equals(expected, local_slice)

        call prif_deallocate_coarray([coarray_handle])
    end function

    function check_put_raw() result(result_)
        type(result_t) :: result_

        integer :: dummy_element, num_imgs, expected, neighbor
        integer, target :: me
        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer, pointer :: local_slice
        integer(c_intmax_t) :: lcobounds(1), ucobounds(1)
        integer(c_intptr_t) :: base_addr

        call prif_num_images(num_images=num_imgs)
        lcobounds(1) = 1
        ucobounds(1) = num_imgs
        call prif_allocate_coarray( &
                lcobounds = lcobounds, &
                ucobounds = ucobounds, &
                lbounds = [integer(c_intmax_t)::], &
                ubounds = [integer(c_intmax_t)::], &
                element_size = int(storage_size(dummy_element)/8, c_size_t), &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call c_f_pointer(allocated_memory, local_slice)

        call prif_this_image_no_coarray(this_image=me)
        neighbor = merge(me+1, 1, me < num_imgs)
        expected = merge(me-1, num_imgs, me > 1)

        call prif_base_pointer(coarray_handle, neighbor, base_addr)
        call prif_put_raw( &
                image_num = neighbor, &
                current_image_buffer = c_loc(me), &
                remote_ptr = base_addr, &
                size = int(storage_size(me)/8, c_size_t))
        call prif_sync_all

        result_ = assert_equals(expected, local_slice)

        call prif_deallocate_coarray([coarray_handle])
    end function

    function check_get() result(result_)
        type(result_t) :: result_

        integer :: dummy_element, num_imgs, me, expected, retrieved
        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer, pointer :: local_slice
        integer(c_intmax_t) :: lcobounds(1), ucobounds(1), neighbor

        call prif_num_images(num_images=num_imgs)
        lcobounds(1) = 1
        ucobounds(1) = num_imgs
        call prif_allocate_coarray( &
                lcobounds = lcobounds, &
                ucobounds = ucobounds, &
                lbounds = [integer(c_intmax_t)::], &
                ubounds = [integer(c_intmax_t)::], &
                element_size = int(storage_size(dummy_element)/8, c_size_t), &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call c_f_pointer(allocated_memory, local_slice)

        call prif_this_image_no_coarray(this_image=me)
        neighbor = merge(me+1, 1, me < num_imgs)
        expected = neighbor
        local_slice = me
        call prif_sync_all

        call prif_get( &
                coarray_handle = coarray_handle, &
                cosubscripts = [neighbor], &
                first_element_addr = allocated_memory, &
                value = retrieved)

        result_ = assert_equals(expected, retrieved)

        call prif_deallocate_coarray([coarray_handle])
    end function

    function check_get_indirect() result(result_)
        type(result_t) :: result_

        integer :: dummy_element, num_imgs, me, expected, neighbor
        integer, target :: retrieved
        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer, pointer :: local_slice
        integer(c_intmax_t) :: lcobounds(1), ucobounds(1)
        integer(c_intptr_t) :: base_addr

        call prif_num_images(num_images=num_imgs)
        lcobounds(1) = 1
        ucobounds(1) = num_imgs
        call prif_allocate_coarray( &
                lcobounds = lcobounds, &
                ucobounds = ucobounds, &
                lbounds = [integer(c_intmax_t)::], &
                ubounds = [integer(c_intmax_t)::], &
                element_size = int(storage_size(dummy_element)/8, c_size_t), &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call c_f_pointer(allocated_memory, local_slice)

        call prif_this_image_no_coarray(this_image=me)
        neighbor = merge(me+1, 1, me < num_imgs)
        expected = neighbor
        local_slice = me
        call prif_base_pointer(coarray_handle, neighbor, base_addr)
        call prif_sync_all

        call prif_get_indirect( &
                image_num = neighbor, &
                current_image_buffer = c_loc(retrieved), &
                remote_ptr = base_addr, &
                size_in_bytes = int(storage_size(retrieved)/8, c_size_t))

        result_ = assert_equals(expected, retrieved)

        call prif_deallocate_coarray([coarray_handle])
    end function
end module
