module caf_image_index_test
    use iso_c_binding, only: c_int, c_intmax_t, c_ptr, c_size_t, c_null_funptr
    use prif, only: prif_coarray_handle, prif_allocate, prif_deallocate, prif_image_index
    use veggies, only: result_t, test_item_t, assert_equals, describe, it

    implicit none
    private
    public :: test_prif_image_index
contains
    function test_prif_image_index() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
          "prif_image_index", &
          [ it("returns 1 for the simplest case", check_simple_case) &
          , it("returns 1 when given the lower bounds", check_lower_bounds) &
          , it("returns the expected answer for a more complicated case", check_complicated) &
          ])
    end function

    function check_simple_case() result(result_)
        type(result_t) :: result_

        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer(c_int) :: answer

        call prif_allocate( &
                lcobounds = [1_c_intmax_t], &
                ucobounds = [2_c_intmax_t], &
                lbounds = [integer(c_intmax_t)::], &
                ubounds = [integer(c_intmax_t)::], &
                element_length = 1_c_size_t, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call prif_image_index(coarray_handle, [1_c_intmax_t], image_index=answer)
        result_ = assert_equals(1_c_int, answer)
        call prif_deallocate([coarray_handle])
    end function

    function check_lower_bounds() result(result_)
        type(result_t) :: result_

        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer(c_int) :: answer

        call prif_allocate( &
                lcobounds = [2_c_intmax_t, 3_c_intmax_t], &
                ucobounds = [3_c_intmax_t, 4_c_intmax_t], &
                lbounds = [integer(c_intmax_t)::], &
                ubounds = [integer(c_intmax_t)::], &
                element_length = 1_c_size_t, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call prif_image_index(coarray_handle, [2_c_intmax_t, 3_c_intmax_t], image_index=answer)
        result_ = assert_equals(1_c_int, answer)
        call prif_deallocate([coarray_handle])
    end function

    function check_complicated() result(result_)
        type(result_t) :: result_

        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer(c_int) :: answer

        call prif_allocate( &
                lcobounds = [1_c_intmax_t, 2_c_intmax_t, 3_c_intmax_t], &
                ucobounds = [3_c_intmax_t, 5_c_intmax_t, 7_c_intmax_t], &
                lbounds = [integer(c_intmax_t)::], &
                ubounds = [integer(c_intmax_t)::], &
                element_length = 1_c_size_t, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call prif_image_index(coarray_handle, [2_c_intmax_t, 4_c_intmax_t, 6_c_intmax_t], image_index=answer)
        result_ = assert_equals(44_c_int, answer)
        call prif_deallocate([coarray_handle])
    end function
end module