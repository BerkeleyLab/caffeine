module caf_image_index_test
    use iso_c_binding, only: c_int, c_ptr, c_size_t, c_null_funptr, c_int64_t
    use prif, only: prif_coarray_handle, prif_allocate_coarray, prif_deallocate_coarray, &
                    prif_image_index, prif_num_images, &
                    prif_team_type, prif_get_team, &
                    prif_this_image_no_coarray, &
                    prif_form_team, prif_change_team, prif_end_team, &
                    prif_image_index_with_team, prif_num_images_with_team
    use veggies, only: result_t, test_item_t, assert_equals, describe, it, succeed

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
          , it("returns 0 with invalid subscripts", check_invalid_subscripts) &
          , it("returns the expected answer for a more complicated case w/corank=2", check_complicated_2d) &
          , it("returns the expected answer for a more complicated case w/corank=3", check_complicated_3d) &
          , it("returns the expected answer with a child team and corank=2", check_complicated_2d_team) &
          ])
    end function

    function check_simple_case() result(result_)
        type(result_t) :: result_

        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer(c_int) :: answer, ni
        call prif_num_images(num_images=ni)

        call prif_allocate_coarray( &
                lcobounds = [1_c_int64_t], &
                ucobounds = [ni+2_c_int64_t], &
                size_in_bytes = 1_c_size_t, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call prif_image_index(coarray_handle, [1_c_int64_t], image_index=answer)
        result_ = assert_equals(1_c_int, answer)
        call prif_deallocate_coarray([coarray_handle])
    end function

    function check_lower_bounds() result(result_)
        type(result_t) :: result_

        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer(c_int) :: answer, ni
        call prif_num_images(num_images=ni)

        call prif_allocate_coarray( &
                lcobounds = [2_c_int64_t, 3_c_int64_t], &
                ucobounds = [3_c_int64_t, ni+4_c_int64_t], &
                size_in_bytes = 1_c_size_t, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call prif_image_index(coarray_handle, [2_c_int64_t, 3_c_int64_t], image_index=answer)
        result_ = assert_equals(1_c_int, answer)
        call prif_deallocate_coarray([coarray_handle])
    end function

    function check_invalid_subscripts() result(result_)
        type(result_t) :: result_

        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer(c_int) :: answer, ni
        call prif_num_images(num_images=ni)

        call prif_allocate_coarray( &
                lcobounds = [-2_c_int64_t, 2_c_int64_t], &
                ucobounds = [2_c_int64_t, ni+6_c_int64_t], &
                size_in_bytes = 1_c_size_t, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call prif_image_index(coarray_handle, [-1_c_int64_t, 1_c_int64_t], image_index=answer)
        result_ = assert_equals(0_c_int, answer)
        call prif_deallocate_coarray([coarray_handle])
    end function

    function check_complicated_2d() result(result_)
        type(result_t) :: result_

        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer(c_int) :: answer, ni
        call prif_num_images(num_images=ni)

        call prif_allocate_coarray( &
                lcobounds = [1_c_int64_t, 2_c_int64_t], &
                ucobounds = [2_c_int64_t, ni+3_c_int64_t], &
                size_in_bytes = 1_c_size_t, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call prif_image_index(coarray_handle, [1_c_int64_t, 3_c_int64_t], image_index=answer)
        result_ = assert_equals(merge(3_c_int,0_c_int,ni >= 3), answer)
        call prif_deallocate_coarray([coarray_handle])
    end function

    function check_complicated_3d() result(result_)
        type(result_t) :: result_

        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer(c_int) :: answer, ni
        type(prif_team_type) :: initial_team
        call prif_get_team(team=initial_team)
        call prif_num_images_with_team(team=initial_team, num_images=ni)

        call prif_allocate_coarray( &
                lcobounds = [1_c_int64_t, 0_c_int64_t, 0_c_int64_t], &
                ucobounds = [2_c_int64_t, 1_c_int64_t, ni+0_c_int64_t], &
                size_in_bytes = 1_c_size_t, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call prif_image_index_with_team(coarray_handle, &
                           [2_c_int64_t, 1_c_int64_t, 1_c_int64_t], &
                           team=initial_team, image_index=answer)
        result_ = assert_equals(merge(8_c_int,0_c_int,ni >= 8), answer)
        call prif_deallocate_coarray([coarray_handle])
    end function

    function check_complicated_2d_team() result(result_)
        type(result_t) :: result_

        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer(c_int) :: answer, ni, cni, me
        integer(c_int64_t) :: which_team
        type(prif_team_type) :: initial_team, child_team

        result_ = succeed("")

        call prif_get_team(team=initial_team)
        call prif_num_images_with_team(team=initial_team, num_images=ni)
        call prif_this_image_no_coarray(this_image=me)

        call prif_allocate_coarray( &
                lcobounds = [0_c_int64_t, 2_c_int64_t], &
                ucobounds = [1_c_int64_t, ni+3_c_int64_t], &
                size_in_bytes = 1_c_size_t, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)

        which_team = merge(1_c_int64_t, 2_c_int64_t, mod(me, 2) == 0)
        call prif_form_team(team_number = which_team, team = child_team)
        call prif_change_team(child_team)

          call prif_num_images_with_team(team=child_team, num_images=cni)

          call prif_image_index_with_team(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              team=initial_team, image_index=answer)
          result_ = result_ .and. &
            assert_equals(1_c_int, answer)

          call prif_image_index_with_team(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              team=child_team, image_index=answer)
          result_ = result_ .and. &
            assert_equals(1_c_int, answer)

          call prif_image_index(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              image_index=answer)
          result_ = result_ .and. &
            assert_equals(1_c_int, answer)

          call prif_image_index_with_team(coarray_handle, &
                              [0_c_int64_t, 3_c_int64_t], &
                              team=initial_team, image_index=answer)
          result_ = result_ .and. &
            assert_equals(merge(3_c_int,0_c_int,ni >= 3), answer)

          call prif_image_index_with_team(coarray_handle, &
                              [0_c_int64_t, 3_c_int64_t], &
                              team=child_team, image_index=answer)
          result_ = result_ .and. &
            assert_equals(merge(3_c_int,0_c_int,cni >= 3), answer)

          call prif_image_index(coarray_handle, &
                              [0_c_int64_t, 3_c_int64_t], &
                              image_index=answer)
          result_ = result_ .and. &
            assert_equals(merge(3_c_int,0_c_int,cni >= 3), answer)

        call prif_end_team()
        call prif_deallocate_coarray([coarray_handle])
    end function



end module
