module prif_rma_test_m
    use iso_c_binding, only: &
            c_ptr, c_int64_t, c_intptr_t, c_size_t, c_null_funptr, c_f_pointer, c_loc, c_sizeof
    use prif, only: &
            prif_coarray_handle, &
            prif_allocate_coarray, &
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
#if FORCE_PRIF_0_5 || FORCE_PRIF_0_6
  use prif, only : prif_deallocate_coarray_ => prif_deallocate_coarray
# define prif_deallocate_coarray(h)    prif_deallocate_coarray_([h])
# define prif_deallocate_coarrays(arr) prif_deallocate_coarray_(arr)
#else
  use prif, only : prif_deallocate_coarray, prif_deallocate_coarrays
#endif
    use julienne_m, only: test_description_t, test_diagnosis_t, test_result_t, test_t, usher, operator(.equalsExpected.)

    implicit none
    private
    public :: prif_rma_test_t

    type, extends(test_t) :: prif_rma_test_t
    contains
      procedure, nopass, non_overridable :: subject
      procedure, nopass, non_overridable :: results
    end type

contains

    pure function subject()
      character(len=:), allocatable :: subject
      subject = "PRIF RMA"
    end function

    function results() result(test_results)
        type(test_result_t), allocatable :: test_results(:)
        type(prif_rma_test_t) prif_rma_test

        test_results = prif_rma_test%run([ &
           test_description_t("can send a value to another image", usher(check_put)) &
          ,test_description_t("can send a value with indirect interface", usher(check_put_indirect)) &
          ,test_description_t("can get a value from another image", usher(check_get)) &
          ,test_description_t("can get a value with indirect interface", usher(check_get_indirect)) &
        ])
    end function

    function check_put() result(diag)
        type(test_diagnosis_t) :: diag

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

        diag = local_slice .equalsExpected. expected

        call prif_deallocate_coarray(coarray_handle)
    end function

    function check_put_indirect() result(diag)
        type(test_diagnosis_t) :: diag

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
        diag = component_access .equalsExpected. expected

        call prif_deallocate(local_slice%my_component)
        call prif_deallocate_coarray(coarray_handle)
    end function

    function check_get() result(diag)
        type(test_diagnosis_t) :: diag

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

        diag = retrieved .equalsExpected. expected

        call prif_deallocate_coarray(coarray_handle)
    end function

    function check_get_indirect() result(diag)
        type(test_diagnosis_t) :: diag

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

        diag = retrieved .equalsExpected. expected

        call prif_deallocate(local_slice%my_component)
        call prif_deallocate_coarray(coarray_handle)
    end function
end module
