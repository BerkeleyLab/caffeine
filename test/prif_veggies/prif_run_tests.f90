module prif_run_tests
    use iso_c_binding, only: c_int64_t, c_size_t, c_ptr, c_null_ptr, c_null_funptr, c_f_pointer
    use iso_fortran_env, only: int64, output_unit, error_unit
    use iso_varying_string, only: put_line, var_str, operator(//)
    use prif, only: &
        prif_this_image_no_coarray, prif_num_images, &
        prif_critical_type, prif_coarray_handle, &
        prif_allocate_coarray, prif_deallocate_coarray, &
        prif_co_reduce, prif_operation_wrapper_interface
    use strff, only: to_string
    use veggies, only: filter_item_result_t, test_item_t, test_result_item_t
    use veggies_command_line_m, only: options_t, get_options, DEBUG

    implicit none
    private
    public :: run_tests
contains
    function run_tests(tests) result(passed)
        type(test_item_t), intent(in) :: tests
        logical :: passed

        integer(int64) :: clock_rate
        real :: elapsed_time
        integer(int64) :: end_time
        type(filter_item_result_t) :: filtered_tests
        type(options_t) :: options
        type(test_result_item_t) :: results
        integer(int64) :: start_time
        logical :: suite_failed
        type(test_item_t) :: tests_to_run
        integer :: i, me, ni

        suite_failed = .false.

        options = get_options()

        tests_to_run = tests
        do i = 1, size(options%filter_strings)
            filtered_tests = tests_to_run%filter(options%filter_strings(i))
            if (filtered_tests%matched()) then
                tests_to_run = filtered_tests%test()
            else
                call put_line(error_unit, "No matching tests found")
                passed = .false.
                return
            end if
        end do

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(ni)
        if (me == 1) then
            call put_line(output_unit, "Running Tests")
            call put_line(output_unit, "")

            if (.not.options%quiet) then
                call put_line(output_unit, tests_to_run%description())
                call put_line(output_unit, "")
            end if

            call put_line( &
                    output_unit, &
                    "A total of " // to_string(tests_to_run%num_cases()) // " test cases")
            call put_line(output_unit, "")
        end if

        if (DEBUG) call put_line( &
                "Beginning execution of test suite" &
                // merge(" on image " // to_string(me), var_str(""), ni > 1))
        call system_clock(start_time, clock_rate)
        results = tests_to_run%run()
        call system_clock(end_time)
        if (DEBUG) call put_line( &
                "Completed execution of test suite." &
                // merge(" on image " // to_string(me), var_str(""), ni > 1))
        elapsed_time = real(end_time - start_time) / real(clock_rate)

        block
            type(prif_critical_type) :: critical_mold
            type(prif_coarray_handle) :: critical_coarray
            type(c_ptr) :: allocated_memory

            call prif_allocate_coarray( &
                    lcobounds = [1_c_int64_t], &
                    ucobounds = [int(ni, kind=c_int64_t)], &
                    size_in_bytes = storage_size(critical_mold, kind=c_size_t), &
                    final_func = c_null_funptr, &
                    coarray_handle = critical_coarray, &
                    allocated_memory = allocated_memory)
            call prif_critical(critical_coarray)
            if (ni > 1) then
                call put_line(output_unit, "On image " // to_string(me))
            end if
            if (results%passed()) then
                call put_line(output_unit, "All Passed")
                call put_line( &
                        output_unit, &
                        "Took " // to_string(elapsed_time, 6) // " seconds")
                call put_line(output_unit, "")
                if (options%verbose) then
                    call put_line( &
                            output_unit, &
                            results%verbose_description(options%colorize))
                    call put_line(output_unit, "")
                end if
                call put_line( &
                        output_unit, &
                        "A total of " // to_string(results%num_cases()) &
                            // " test cases containing a total of " &
                            // to_string(results%num_asserts()) // " assertions")
                call put_line(output_unit, "")
            else
                call put_line(error_unit, "Failed")
                call put_line( &
                        error_unit, &
                        "Took " // to_string(elapsed_time, 6) // " seconds")
                call put_line(error_unit, "")
                if (options%verbose) then
                    call put_line( &
                            error_unit, &
                            results%verbose_description(options%colorize))
                else
                    call put_line( &
                            error_unit, &
                            results%failure_description(options%colorize))
                end if
                call put_line(error_unit, "")
                call put_line( &
                        error_unit, &
                        to_string(results%num_failing_cases()) // " of " &
                            // to_string(results%num_cases()) // " cases failed")
                call put_line( &
                        error_unit, &
                        to_string(results%num_failing_asserts()) // " of " &
                            // to_string(results%num_asserts()) // " assertions failed")
                call put_line(error_unit, "")
                suite_failed = .true.
            end if

            call prif_end_critical(critical_coarray)
            call prif_deallocate_coarray([critical_coarray])
        end block
        if (any_image_failed(suite_failed)) then
            passed = .false.
        else
            passed = .true.
        end if
    end function

    function any_image_failed(image_failed)
        logical, intent(in) :: image_failed
        logical :: any_image_failed

        any_image_failed = image_failed
        call co_any(any_image_failed)
    end function

    subroutine co_any(x)
        logical, intent(inout) :: x

        procedure(prif_operation_wrapper_interface), pointer :: op

        op => or_wrapper
        call prif_co_reduce(x, op, c_null_ptr)
    end subroutine

    subroutine or_wrapper(arg1, arg2_and_out, count, cdata) bind(C)
        type(c_ptr), intent(in), value :: arg1, arg2_and_out
        integer(c_size_t), intent(in), value :: count
        type(c_ptr), intent(in), value :: cdata

        logical, pointer :: lhs(:), rhs_and_result(:)
        integer(c_size_t) :: i

        if (count == 0) return
        call c_f_pointer(arg1, lhs, [count])
        call c_f_pointer(arg2_and_out, rhs_and_result, [count])
        do i = 1, count
            rhs_and_result(i) = lhs(i).or.rhs_and_result(i)
        end do
    end subroutine
end module
