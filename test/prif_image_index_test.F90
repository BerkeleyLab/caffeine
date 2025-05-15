module caf_image_index_test
    use iso_c_binding, only: c_int, c_ptr, c_size_t, c_null_funptr, c_int64_t
    use prif, only: prif_coarray_handle, prif_allocate_coarray, prif_deallocate_coarray, &
                    prif_image_index, prif_num_images, &
                    prif_team_type, prif_get_team, &
                    prif_this_image_no_coarray, &
                    prif_form_team, prif_change_team, prif_end_team, &
                    prif_image_index_with_team, prif_image_index_with_team_number, &
                    prif_initial_team_index, prif_initial_team_index_with_team, prif_initial_team_index_with_team_number, &
                    prif_this_image_with_coarray, prif_this_image_with_dim, &
                    prif_lcobound_no_dim, prif_ucobound_no_dim, &
                    prif_num_images_with_team, PRIF_INITIAL_TEAM
    use veggies, only: result_t, test_item_t, assert_equals, assert_that, describe, it, succeed

    implicit none
    private
    public :: test_prif_image_index
contains
    function test_prif_image_index() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
          "prif_image_index and prif_initial_team_index", &
          [ it("returns 1 for the simplest case", check_simple_case) &
          , it("returns 1 when given the lower bounds", check_lower_bounds) &
          , it("returns 0 with invalid subscripts", check_invalid_subscripts) &
          , it("returns the expected answer for a more complicated case w/corank=2", check_complicated_2d) &
          , it("returns the expected answer for a more complicated case w/corank=3", check_complicated_3d) &
          , it("returns the expected answer with a child team and corank=2", check_complicated_2d_team) &
          ])
    end function

    function check_this_image_coarray(coarray_handle, corank, team) result(result_)
        type(prif_coarray_handle) :: coarray_handle
        integer(c_int) :: corank
        type(prif_team_type), optional :: team
        type(result_t) :: result_

        integer(c_int64_t) :: co, cosubscripts(corank), colbound(corank), coubound(corank)
        integer(c_int) :: i, me, me_initial
        type(prif_team_type) :: initial_team

        call prif_get_team(PRIF_INITIAL_TEAM, team=initial_team)

        result_ = succeed("")

        call prif_lcobound_no_dim(coarray_handle, colbound)
        call prif_ucobound_no_dim(coarray_handle, coubound)
        call prif_this_image_no_coarray(team, me)
        call prif_this_image_no_coarray(initial_team, me_initial)

        call prif_this_image_with_coarray(coarray_handle, team=team, cosubscripts=cosubscripts)
        do i=1,corank
          call prif_this_image_with_dim(coarray_handle, dim=i, team=team, cosubscript=co)
          result_ = result_ .and. assert_equals(int(co), int(cosubscripts(i)))

          result_ = result_ .and. assert_that(co >= colbound(i))
          result_ = result_ .and. assert_that(co <= coubound(i))
        end do

        ! verify reverse mapping
        if (present(team)) then
          call prif_image_index_with_team(coarray_handle, cosubscripts, team, i)
        else
          call prif_image_index(coarray_handle, cosubscripts, i)
        end if
        result_ = result_ .and. assert_equals(i, me)

        ! and prif_initial_team_index
        if (present(team)) then
          call prif_initial_team_index_with_team(coarray_handle, cosubscripts, team, i)
        else
          call prif_initial_team_index(coarray_handle, cosubscripts, i)
        end if
        result_ = result_ .and. assert_equals(i, me_initial)

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

        call prif_initial_team_index(coarray_handle, [1_c_int64_t], initial_team_index=answer)
        result_ =  result_ .and. assert_equals(1_c_int, answer)

        result_ = result_ .and. &
          check_this_image_coarray(coarray_handle, 1)

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

        call prif_initial_team_index(coarray_handle, [2_c_int64_t, 3_c_int64_t], initial_team_index=answer)
        result_ =  result_ .and. assert_equals(1_c_int, answer)

        result_ = result_ .and. &
          check_this_image_coarray(coarray_handle, 2)

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

        result_ = result_ .and. &
          check_this_image_coarray(coarray_handle, 2)

        call prif_deallocate_coarray([coarray_handle])
    end function

    function check_complicated_2d() result(result_)
        type(result_t) :: result_

        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer(c_int) :: answer, ni, expected
        call prif_num_images(num_images=ni)

        call prif_allocate_coarray( &
                lcobounds = [1_c_int64_t, 2_c_int64_t], &
                ucobounds = [2_c_int64_t, ni+3_c_int64_t], &
                size_in_bytes = 1_c_size_t, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call prif_image_index(coarray_handle, [1_c_int64_t, 3_c_int64_t], image_index=answer)
        expected = merge(3_c_int,0_c_int,ni >= 3)
        result_ = assert_equals(expected, answer)

        if (expected > 0) then
          call prif_initial_team_index(coarray_handle, [1_c_int64_t, 3_c_int64_t], initial_team_index=answer)
          result_ =  result_ .and. assert_equals(expected, answer)
        end if

        result_ = result_ .and. &
          check_this_image_coarray(coarray_handle, 2)

        call prif_deallocate_coarray([coarray_handle])
    end function

    function check_complicated_3d() result(result_)
        type(result_t) :: result_

        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer(c_int) :: answer, ni, expected
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
        expected = merge(8_c_int,0_c_int,ni >= 8)
        result_ = assert_equals(expected, answer)

        if (expected > 0) then
          call prif_initial_team_index_with_team(coarray_handle, &
                           [2_c_int64_t, 1_c_int64_t, 1_c_int64_t], &
                           team=initial_team, initial_team_index=answer)
          result_ =  result_ .and. assert_equals(expected, answer)
        endif

        result_ = result_ .and. &
          check_this_image_coarray(coarray_handle, 3)

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

        which_team = merge(2_c_int64_t, 1_c_int64_t, mod(me, 2) == 0)
        call prif_form_team(team_number = which_team, team = child_team)
        call prif_change_team(child_team)

          call prif_num_images_with_team(team=child_team, num_images=cni)

          ! image_index lcobound

          call prif_image_index_with_team(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              team=initial_team, image_index=answer)
          result_ = result_ .and. &
            assert_equals(1_c_int, answer)

          call prif_image_index_with_team_number(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              team_number=-1_c_int64_t, image_index=answer)
          result_ = result_ .and. &
            assert_equals(1_c_int, answer)

          call prif_image_index_with_team(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              team=child_team, image_index=answer)
          result_ = result_ .and. &
            assert_equals(1_c_int, answer)

          call prif_image_index_with_team_number(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              team_number=which_team, image_index=answer)
          result_ = result_ .and. &
            assert_equals(1_c_int, answer)

          call prif_image_index(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              image_index=answer)
          result_ = result_ .and. &
            assert_equals(1_c_int, answer)

          ! initial_team_index lcobound

          call prif_initial_team_index_with_team(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              initial_team, answer)
          result_ = result_ .and. &
            assert_equals(1_c_int, answer)

          call prif_initial_team_index_with_team_number(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              -1_c_int64_t, answer)
          result_ = result_ .and. &
            assert_equals(1_c_int, answer)

          call prif_initial_team_index_with_team(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              child_team, answer)
          result_ = result_ .and. &
            assert_equals(merge(1_c_int,2_c_int,which_team==1), answer)

          call prif_initial_team_index_with_team_number(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              which_team, answer)
          result_ = result_ .and. &
            assert_equals(merge(1_c_int,2_c_int,which_team==1), answer)

          call prif_initial_team_index(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              answer)
          result_ = result_ .and. &
            assert_equals(merge(1_c_int,2_c_int,which_team==1), answer)

          ! image_index 3

          call prif_image_index_with_team(coarray_handle, &
                              [0_c_int64_t, 3_c_int64_t], &
                              team=initial_team, image_index=answer)
          result_ = result_ .and. &
            assert_equals(merge(3_c_int,0_c_int,ni >= 3), answer)

          call prif_image_index_with_team_number(coarray_handle, &
                              [0_c_int64_t, 3_c_int64_t], &
                              team_number=-1_c_int64_t, image_index=answer)
          result_ = result_ .and. &
            assert_equals(merge(3_c_int,0_c_int,ni >= 3), answer)

          call prif_image_index_with_team(coarray_handle, &
                              [0_c_int64_t, 3_c_int64_t], &
                              team=child_team, image_index=answer)
          result_ = result_ .and. &
            assert_equals(merge(3_c_int,0_c_int,cni >= 3), answer)

          call prif_image_index_with_team_number(coarray_handle, &
                              [0_c_int64_t, 3_c_int64_t], &
                              team_number=which_team, image_index=answer)
          result_ = result_ .and. &
            assert_equals(merge(3_c_int,0_c_int,cni >= 3), answer)

          call prif_image_index(coarray_handle, &
                              [0_c_int64_t, 3_c_int64_t], &
                              image_index=answer)
          result_ = result_ .and. &
            assert_equals(merge(3_c_int,0_c_int,cni >= 3), answer)

          ! initial_team_index 3
          if (ni >= 3) then
            call prif_initial_team_index_with_team(coarray_handle, &
                                [0_c_int64_t, 3_c_int64_t], &
                                team=initial_team, initial_team_index=answer)
            result_ = result_ .and. &
              assert_equals(3_c_int, answer)
  
            call prif_initial_team_index_with_team_number(coarray_handle, &
                                [0_c_int64_t, 3_c_int64_t], &
                                team_number=-1_c_int64_t, initial_team_index=answer)
            result_ = result_ .and. &
              assert_equals(3_c_int, answer)
          end if
          if (cni >= 3) then
            call prif_initial_team_index_with_team(coarray_handle, &
                                [0_c_int64_t, 3_c_int64_t], &
                                team=child_team, initial_team_index=answer)
            result_ = result_ .and. &
              assert_equals(merge(5_c_int,6_c_int,which_team==1), answer)
  
            call prif_initial_team_index_with_team_number(coarray_handle, &
                                [0_c_int64_t, 3_c_int64_t], &
                                team_number=which_team, initial_team_index=answer)
            result_ = result_ .and. &
              assert_equals(merge(5_c_int,6_c_int,which_team==1), answer)
  
            call prif_initial_team_index(coarray_handle, &
                                [0_c_int64_t, 3_c_int64_t], &
                                initial_team_index=answer)
            result_ = result_ .and. &
              assert_equals(merge(5_c_int,6_c_int,which_team==1), answer)
          end if

          result_ = result_ .and. &
            check_this_image_coarray(coarray_handle, 2, initial_team)
          result_ = result_ .and. &
            check_this_image_coarray(coarray_handle, 2, child_team)

        call prif_end_team()
        call prif_deallocate_coarray([coarray_handle])
    end function



end module
