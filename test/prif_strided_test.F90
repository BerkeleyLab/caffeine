module caf_strided_test
    use iso_c_binding, only: &
            c_ptr, c_int64_t, c_intptr_t, c_size_t, c_null_funptr, c_f_pointer, c_loc, c_sizeof
    use prif, only: &
            prif_coarray_handle, &
            prif_allocate_coarray, &
            prif_deallocate_coarray, &
            prif_allocate, &
            prif_deallocate, &
            prif_num_images, &
            prif_get, &
            prif_put_strided, &
            prif_put_strided_indirect, &
            prif_get_strided, &
            prif_get_strided_indirect, &
            prif_sync_all, &
            prif_this_image_no_coarray
    use veggies, only: result_t, test_item_t, assert_equals, describe, it, succeed, fail

    implicit none
    private
    public :: test_prif_rma_strided
contains
    function test_prif_rma_strided() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
            "PRIF Strided RMA", &
            [ it("can put strided data to another image", check_put) &
            , it("can put strided data with indirect interface", check_put_indirect) &
            , it("can get strided data from another image", check_get) &
            , it("can get strided data with indirect interface", check_get_indirect) &
            ])
    end function

    function assert_equals_array2d(expected, actual) result(result_)
        integer, intent(in) :: expected(:,:)
        integer, intent(in) :: actual(:,:)
        type(result_t) :: result_ 
        integer :: i,j
        
        result_ = succeed("")
        result_ = result_ .and. assert_equals(size(expected,1), size(actual,1))
        result_ = result_ .and. assert_equals(size(expected,2), size(actual,2))

        do i = lbound(actual,1), ubound(actual,1) 
          do j = lbound(actual,2), ubound(actual,2) 
           block
            character(len=100) :: result_string

            write(result_string, '("At position (", I0, ",", I0, ") expected=", I0, " actual=", I0)') &
                  i, j, expected(i,j), actual(i,j)

            result_ = result_ .and. &
              assert_equals(expected(i,j), actual(i,j), result_string)
           end block
          end do
        end do
        
    end function

    function check_put() result(result_)
        type(result_t) :: result_

        integer :: me, num_imgs, neighbor
        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer, target :: mydata(1:4, 1:4)
        integer, target :: expected(1:4, 1:4)
        integer, pointer :: local_slice(:,:)
        integer(c_int64_t) :: lcobounds(1), ucobounds(1)
        integer(c_size_t) :: sizeof_int

        sizeof_int = storage_size(me)/8
        call prif_num_images(num_images=num_imgs)
        call prif_this_image_no_coarray(this_image=me)
        neighbor = merge(me+1, 1, me < num_imgs)

        lcobounds(1) = 1
        ucobounds(1) = num_imgs
        call prif_allocate_coarray( &
                lcobounds = lcobounds, &
                ucobounds = ucobounds, &
                size_in_bytes = sizeof_int*product(shape(mydata)), &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call c_f_pointer(allocated_memory, local_slice, shape(mydata))

        ! init data arrays to known values
        local_slice = -1
        expected = -1
        mydata = 0

        call prif_sync_all

        ! simple example: we set, then copy the interior rectangle of a 4x4 array
        mydata(2:3, 2:3) = me
        expected(2:3, 2:3) = merge(me-1, num_imgs, me > 1)

        call prif_put_strided( &
                image_num = neighbor, &
                coarray_handle = coarray_handle, &
                offset = 5*sizeof_int, &
                remote_stride = [4*sizeof_int, sizeof_int], &
                current_image_buffer = c_loc(mydata(2,2)), &
                current_image_stride = [4*sizeof_int, sizeof_int], &
                element_size = sizeof_int, &
                extent = [2_c_size_t, 2_c_size_t])

        call prif_sync_all

        result_ = assert_equals_array2d(expected, local_slice)

        call prif_deallocate_coarray([coarray_handle])
    end function

    function check_put_indirect() result(result_)
        type(result_t) :: result_

        type :: my_type
          type(c_ptr) :: my_component
        end type

        type(my_type), target :: dummy_element
        integer, pointer :: component_access(:,:)
        integer :: me, num_imgs, neighbor
        integer, target :: mydata(1:4, 1:4)
        integer, target :: expected(1:4, 1:4)
        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        type(my_type), pointer :: local_slice
        integer(c_int64_t) :: lcobounds(1), ucobounds(1)
        integer(c_intptr_t) :: base_addr
        integer(c_size_t) :: sizeof_int

        sizeof_int = storage_size(me)/8
        call prif_num_images(num_images=num_imgs)
        call prif_this_image_no_coarray(this_image=me)
        neighbor = merge(me+1, 1, me < num_imgs)

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
                size_in_bytes = int(sizeof_int*product(shape(mydata)), c_size_t), &
                allocated_memory = local_slice%my_component)
        call c_f_pointer(local_slice%my_component, component_access, shape(mydata))

        ! init data arrays to known values
        component_access = -1
        expected = -1
        mydata = 0

        call prif_sync_all

        ! simple example: we set, then copy the interior rectangle of a 4x4 array
        mydata(2:3, 2:3) = me
        expected(2:3, 2:3) = merge(me-1, num_imgs, me > 1)

        call prif_get( &
                image_num = neighbor, &
                coarray_handle = coarray_handle, &
                offset = 0_c_size_t, &
                current_image_buffer = c_loc(dummy_element), &
                size_in_bytes = int(storage_size(dummy_element)/8, c_size_t))
        base_addr = transfer(dummy_element%my_component, base_addr)

        call prif_put_strided_indirect( &
                image_num = neighbor, &
                remote_ptr = base_addr + 5*sizeof_int, &
                remote_stride = [4*sizeof_int, sizeof_int], &
                current_image_buffer = c_loc(mydata(2,2)), &
                current_image_stride = [4*sizeof_int, sizeof_int], &
                element_size = sizeof_int, &
                extent = [2_c_size_t, 2_c_size_t])

        call prif_sync_all

        result_ = assert_equals_array2d(expected, component_access)

        call prif_deallocate(local_slice%my_component)
        call prif_deallocate_coarray([coarray_handle])
    end function

    function check_get() result(result_)
        type(result_t) :: result_

        integer :: me, num_imgs, neighbor
        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer, target :: mydata(1:4, 1:4)
        integer, target :: expected(1:4, 1:4)
        integer, pointer :: local_slice(:,:)
        integer(c_int64_t) :: lcobounds(1), ucobounds(1)
        integer(c_size_t) :: sizeof_int

        sizeof_int = storage_size(me)/8
        call prif_num_images(num_images=num_imgs)
        call prif_this_image_no_coarray(this_image=me)
        neighbor = merge(me+1, 1, me < num_imgs)

        lcobounds(1) = 1
        ucobounds(1) = num_imgs
        call prif_allocate_coarray( &
                lcobounds = lcobounds, &
                ucobounds = ucobounds, &
                size_in_bytes = sizeof_int*product(shape(mydata)), &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call c_f_pointer(allocated_memory, local_slice, shape(mydata))

        ! simple example: we copy the interior rectangle of a 4x4 array
        local_slice = -1
        local_slice(2:3, 2:3) = me
        expected = 0
        expected(2:3, 2:3) = neighbor
        mydata = 0

        call prif_sync_all

        call prif_get_strided( &
                image_num = neighbor, &
                coarray_handle = coarray_handle, &
                offset = 5*sizeof_int, &
                remote_stride = [4*sizeof_int, sizeof_int], &
                current_image_buffer = c_loc(mydata(2,2)), &
                current_image_stride = [4*sizeof_int, sizeof_int], &
                element_size = sizeof_int, &
                extent = [2_c_size_t, 2_c_size_t])

        call prif_sync_all

        result_ = assert_equals_array2d(expected, mydata)

        call prif_deallocate_coarray([coarray_handle])
    end function

    function check_get_indirect() result(result_)
        type(result_t) :: result_

        type :: my_type
          type(c_ptr) :: my_component
        end type

        type(my_type), target :: dummy_element
        integer, pointer :: component_access(:,:)
        integer :: me, num_imgs, neighbor
        integer, target :: mydata(1:4, 1:4)
        integer, target :: expected(1:4, 1:4)
        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        type(my_type), pointer :: local_slice
        integer(c_int64_t) :: lcobounds(1), ucobounds(1)
        integer(c_intptr_t) :: base_addr
        integer(c_size_t) :: sizeof_int

        sizeof_int = storage_size(me)/8
        call prif_num_images(num_images=num_imgs)
        call prif_this_image_no_coarray(this_image=me)
        neighbor = merge(me+1, 1, me < num_imgs)

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
                size_in_bytes = int(sizeof_int*product(shape(mydata)), c_size_t), &
                allocated_memory = local_slice%my_component)
        call c_f_pointer(local_slice%my_component, component_access, shape(mydata))

        ! simple example: we copy the interior rectangle of a 4x4 array
        component_access = -1
        component_access(2:3, 2:3) = me
        expected = 0
        expected(2:3, 2:3) = neighbor
        mydata = 0

        call prif_sync_all

        call prif_get( &
                image_num = neighbor, &
                coarray_handle = coarray_handle, &
                offset = 0_c_size_t, &
                current_image_buffer = c_loc(dummy_element), &
                size_in_bytes = int(storage_size(dummy_element)/8, c_size_t))
        base_addr = transfer(dummy_element%my_component, base_addr)

        call prif_get_strided_indirect( &
                image_num = neighbor, &
                remote_ptr = base_addr + 5*sizeof_int, &
                remote_stride = [4*sizeof_int, sizeof_int], &
                current_image_buffer = c_loc(mydata(2,2)), &
                current_image_stride = [4*sizeof_int, sizeof_int], &
                element_size = sizeof_int, &
                extent = [2_c_size_t, 2_c_size_t])

        call prif_sync_all

        result_ = assert_equals_array2d(expected, mydata)

        call prif_deallocate(local_slice%my_component)
        call prif_deallocate_coarray([coarray_handle])
    end function

end module
