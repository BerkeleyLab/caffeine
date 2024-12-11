module caf_coarray_inquiry_test
    use prif, only : &
        prif_allocate_coarray, prif_deallocate_coarray, &
        prif_coarray_handle, prif_num_images, &
        prif_local_data_pointer
    use veggies, only: result_t, test_item_t, assert_that, describe, it
    use iso_c_binding, only: &
        c_ptr, c_intmax_t, c_size_t, c_null_funptr, c_associated

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
                    ]) &
                ])
    end function

    function check_prif_local_data_pointer() result(result_)
        type(result_t) :: result_

        integer(kind=c_intmax_t), dimension(1) :: lcobounds, ucobounds
        integer(kind=c_intmax_t), dimension(0), parameter :: lbounds = [integer(kind=c_intmax_t) ::]
        integer(kind=c_intmax_t), dimension(0), parameter :: ubounds = [integer(kind=c_intmax_t) ::]
        integer :: dummy_element, num_imgs
        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocation_ptr, local_ptr

        call prif_num_images(num_images=num_imgs)
        lcobounds(1) = 1
        ucobounds(1) = num_imgs

        call prif_allocate_coarray( &
                lcobounds, &
                ucobounds, &
                lbounds, &
                ubounds, &
                int(storage_size(dummy_element)/8, c_size_t), &
                c_null_funptr, &
                coarray_handle, &
                allocation_ptr)
        call prif_local_data_pointer(coarray_handle, local_ptr)
        result_ = assert_that(c_associated(local_ptr, allocation_ptr))
        call prif_deallocate_coarray([coarray_handle])
    end function
end module