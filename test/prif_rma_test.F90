module caf_rma_test
    use iso_c_binding, only: &
            c_ptr, c_int64_t, c_intptr_t, c_size_t, c_null_funptr, c_f_pointer, c_loc, c_sizeof
    use prif, only: &
            prif_coarray_handle, &
            prif_allocate_coarray, &
            prif_deallocate_coarray, &
            prif_allocate, &
            prif_deallocate, &
            prif_num_images, &
            prif_put, &
            prif_put_indirect, &
            prif_get, &
            prif_get_indirect, &
            prif_sync_all, &
            prif_sync_memory, &
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
            , it("can send a value with indirect interface", check_put_indirect) &
            , it("can get a value from another image", check_get) &
            , it("can get a value with indirect interface", check_get_indirect) &
            ])
    end function

    function check_put() result(result_)
        type(result_t) :: result_

        integer :: dummy_element, num_imgs, expected, neighbor
        integer, target :: me
        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer, pointer :: local_slice
        integer(c_int64_t) :: lcobounds(1), ucobounds(1)

        call prif_num_images(num_images=num_imgs)
        lcobounds(1) = 1
        ucobounds(1) = num_imgs
        call prif_allocate_coarray( &
                lcobounds = lcobounds, &
                ucobounds = ucobounds, &
                size_in_bytes = int(storage_size(dummy_element)/8, c_size_t), &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call c_f_pointer(allocated_memory, local_slice)

        call prif_this_image_no_coarray(this_image=me)
        neighbor = merge(me+1, 1, me < num_imgs)
        expected = merge(me-1, num_imgs, me > 1)

        call prif_put( &
                image_num = neighbor, &
                coarray_handle = coarray_handle, &
                offset = 0_c_size_t, &
                current_image_buffer = c_loc(me), &
                size_in_bytes = c_sizeof(me))
        call prif_sync_all

        ! superfluous, just to ensure prif_sync_memory is usable
        call prif_sync_memory

        result_ = assert_equals(expected, local_slice)

        call prif_deallocate_coarray([coarray_handle])
    end function

    function check_put_indirect() result(result_)
        type(result_t) :: result_

        type :: my_type
          type(c_ptr) :: my_component
        end type

        type(my_type), target :: dummy_element
        integer, pointer :: component_access
        integer :: dummy_component, num_imgs, expected, neighbor
        integer, target :: me
        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        type(my_type), pointer :: local_slice
        integer(c_int64_t) :: lcobounds(1), ucobounds(1)
        integer(c_intptr_t) :: base_addr

        call prif_num_images(num_images=num_imgs)
        lcobounds(1) = 1
        ucobounds(1) = num_imgs
        call prif_allocate_coarray( &
                lcobounds = lcobounds, &
                ucobounds = ucobounds, &
                size_in_bytes = int(storage_size(dummy_element)/8, c_size_t), &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call c_f_pointer(allocated_memory, local_slice)
        call prif_allocate( &
                size_in_bytes = int(storage_size(dummy_component)/8, c_size_t), &
                allocated_memory = local_slice%my_component)
        call prif_sync_all

        call prif_this_image_no_coarray(this_image=me)
        neighbor = merge(me+1, 1, me < num_imgs)
        expected = merge(me-1, num_imgs, me > 1)

        call prif_get( &
                image_num = neighbor, &
                coarray_handle = coarray_handle, &
                offset = 0_c_size_t, &
                current_image_buffer = c_loc(dummy_element), &
                size_in_bytes = int(storage_size(dummy_element)/8, c_size_t))
        base_addr = transfer(dummy_element%my_component, base_addr)
        call prif_put_indirect( &
                image_num = neighbor, &
                remote_ptr = base_addr, &
                current_image_buffer = c_loc(me), &
                size_in_bytes = int(storage_size(me)/8, c_size_t))
        call prif_sync_all

        call c_f_pointer(local_slice%my_component, component_access)
        result_ = assert_equals(expected, component_access)

        call prif_deallocate(local_slice%my_component)
        call prif_deallocate_coarray([coarray_handle])
    end function

    function check_get() result(result_)
        type(result_t) :: result_

        integer :: dummy_element, num_imgs, me, neighbor, expected
        integer, target :: retrieved
        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer, pointer :: local_slice
        integer(c_int64_t) :: lcobounds(1), ucobounds(1)

        call prif_num_images(num_images=num_imgs)
        lcobounds(1) = 1
        ucobounds(1) = num_imgs
        call prif_allocate_coarray( &
                lcobounds = lcobounds, &
                ucobounds = ucobounds, &
                size_in_bytes = int(storage_size(dummy_element)/8, c_size_t), &
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
                image_num = neighbor, &
                coarray_handle = coarray_handle, &
                offset = 0_c_size_t, &
                current_image_buffer = c_loc(retrieved), &
                size_in_bytes = c_sizeof(retrieved))

        result_ = assert_equals(expected, retrieved)

        call prif_deallocate_coarray([coarray_handle])
    end function

    function check_get_indirect() result(result_)
        type(result_t) :: result_

        type :: my_type
          type(c_ptr) :: my_component
        end type

        type(my_type), target :: dummy_element
        integer, pointer :: component_access
        integer :: dummy_component, num_imgs, me, expected, neighbor
        integer, target :: retrieved
        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        type(my_type), pointer :: local_slice
        integer(c_int64_t) :: lcobounds(1), ucobounds(1)
        integer(c_intptr_t) :: base_addr

        call prif_num_images(num_images=num_imgs)
        lcobounds(1) = 1
        ucobounds(1) = num_imgs
        call prif_allocate_coarray( &
                lcobounds = lcobounds, &
                ucobounds = ucobounds, &
                size_in_bytes = int(storage_size(dummy_element)/8, c_size_t), &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call c_f_pointer(allocated_memory, local_slice)
        call prif_allocate( &
                size_in_bytes = int(storage_size(dummy_component)/8, c_size_t), &
                allocated_memory = local_slice%my_component)

        call prif_this_image_no_coarray(this_image=me)
        neighbor = merge(me+1, 1, me < num_imgs)
        expected = neighbor
        call c_f_pointer(local_slice%my_component, component_access)
        component_access = me
        call prif_sync_all

        call prif_get( &
                image_num = neighbor, &
                coarray_handle = coarray_handle, &
                offset = 0_c_size_t, &
                current_image_buffer = c_loc(dummy_element), &
                size_in_bytes = int(storage_size(dummy_element)/8, c_size_t))
        base_addr = transfer(dummy_element%my_component, base_addr)
        call prif_get_indirect( &
                image_num = neighbor, &
                remote_ptr = base_addr, &
                current_image_buffer = c_loc(retrieved), &
                size_in_bytes = int(storage_size(retrieved)/8, c_size_t))

        result_ = assert_equals(expected, retrieved)

        call prif_deallocate(local_slice%my_component)
        call prif_deallocate_coarray([coarray_handle])
    end function
end module
