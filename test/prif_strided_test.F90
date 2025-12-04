module prif_strided_test_m
    use iso_c_binding, only: &
            c_ptr, c_int64_t, c_intptr_t, c_size_t, c_null_funptr, c_f_pointer, c_loc, c_sizeof
    use prif, only: &
            prif_coarray_handle, &
            prif_allocate_coarray, &
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
#if FORCE_PRIF_0_5 || FORCE_PRIF_0_6
  use prif, only : prif_deallocate_coarray_ => prif_deallocate_coarray
# define prif_deallocate_coarray(h)    prif_deallocate_coarray_([h])
# define prif_deallocate_coarrays(arr) prif_deallocate_coarray_(arr)
#else
  use prif, only : prif_deallocate_coarray, prif_deallocate_coarrays
#endif
    use julienne_m, only: test_description_t, test_diagnosis_t, test_result_t, test_t, usher &
      ,operator(.all.), operator(.equalsExpected.)
      
    implicit none
    private
    public :: prif_strided_test_t

    type, extends(test_t) :: prif_strided_test_t
    contains
      procedure, nopass, non_overridable :: subject
      procedure, nopass, non_overridable :: results
    end type

contains

    pure function subject()
      character(len=:), allocatable :: subject
      subject = "PRIF Strided RMA"
    end function

    function results() result(test_results)
        type(test_result_t), allocatable :: test_results(:)
        type(prif_strided_test_t) prif_strided_test

        test_results = prif_strided_test%run([ &
             test_description_t("putting strided data to another image", usher(check_put)) &
            ,test_description_t("putting strided data with indirect interface", usher(check_put_indirect)) &
            ,test_description_t("getting strided data from another image", usher(check_get)) &
            ,test_description_t("getting strided data with indirect interface", usher(check_get_indirect)) &
        ])
    end function

    function check_put() result(diag)
        type(test_diagnosis_t) :: diag

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

        diag = .all. (local_slice .equalsExpected. expected)

        call prif_deallocate_coarray(coarray_handle)
    end function

    function check_put_indirect() result(diag)
        type(test_diagnosis_t) :: diag

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

        diag = .all. (component_access .equalsExpected. expected)

        call prif_deallocate(local_slice%my_component)
        call prif_deallocate_coarray(coarray_handle)
    end function

    function check_get() result(diag)
        type(test_diagnosis_t) :: diag

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

        diag = .all. (mydata .equalsExpected. expected)

        call prif_deallocate_coarray(coarray_handle)
    end function

    function check_get_indirect() result(diag)
        type(test_diagnosis_t) :: diag

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

        diag = .all. (mydata .equalsExpected. expected)

        call prif_deallocate(local_slice%my_component)
        call prif_deallocate_coarray(coarray_handle)
    end function

end module
