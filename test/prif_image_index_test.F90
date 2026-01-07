#include "test-utils.F90"

module prif_image_index_test_m
# include "test-uses-alloc.F90"
    use prif, only: &
                    prif_image_index, prif_num_images, &
                    prif_team_type, prif_get_team, &
                    prif_this_image_no_coarray, &
                    prif_form_team, prif_change_team, prif_end_team, &
                    prif_image_index_with_team, prif_image_index_with_team_number, &
                    prif_initial_team_index, prif_initial_team_index_with_team, prif_initial_team_index_with_team_number, &
                    prif_this_image_with_coarray, prif_this_image_with_dim, &
                    prif_lcobound_no_dim, prif_ucobound_no_dim, &
                    prif_num_images_with_team, PRIF_INITIAL_TEAM
    use julienne_m, only: test_description_t, test_diagnosis_t, test_result_t, test_t, string_t, usher &
       ,operator(.also.), operator(.equalsExpected.), operator(.isAtLeast.), operator(.isAtMost.), operator(//)

    implicit none
    private
    public :: prif_image_index_test_t

    type, extends(test_t) :: prif_image_index_test_t
    contains
      procedure, nopass, non_overridable :: subject
      procedure, nopass, non_overridable :: results
    end type
contains
    pure function subject()
      character(len=:), allocatable :: subject
      subject = "prif_image_index and prif_initial_team_index"
    end function
      
    function results() result(test_results)
      type(test_result_t), allocatable :: test_results(:)
      type(prif_image_index_test_t) prif_image_index_test

      allocate(test_results, source = prif_image_index_test%run([ &
         test_description_t("returning 1 for the simplest case", usher(check_simple_case)) &
        ,test_description_t("returning 1 when given the lower bounds", usher(check_lower_bounds)) &
        ,test_description_t("returning 0 with invalid subscripts", usher(check_invalid_subscripts)) &
        ,test_description_t("returning the expected answer for a more complicated case w/corank=2", usher(check_complicated_2d)) &
        ,test_description_t("returning the expected answer for a more complicated case w/corank=3", usher(check_complicated_3d)) &
        ,test_description_t("returning the expected answer with a child team and corank=2", usher(check_complicated_2d_team)) &
      ]))
    end function

    function check_this_image_coarray(coarray_handle, corank, team) result(diag)
        type(prif_coarray_handle) :: coarray_handle
        integer(c_int) :: corank
        type(prif_team_type), optional :: team
        type(test_diagnosis_t) :: diag

        integer(c_int64_t) :: co, cosubscripts(corank), colbound(corank), coubound(corank)
        integer(c_int) :: i, me, me_initial
        type(prif_team_type) :: initial_team

        diag = .true.

        call prif_get_team(PRIF_INITIAL_TEAM, team=initial_team)

        call prif_lcobound_no_dim(coarray_handle, colbound)
        call prif_ucobound_no_dim(coarray_handle, coubound)
        call prif_this_image_no_coarray(team, me)
        call prif_this_image_no_coarray(initial_team, me_initial)

        call prif_this_image_with_coarray(coarray_handle, team=team, cosubscripts=cosubscripts)
        do i=1,corank
          call prif_this_image_with_dim(coarray_handle, dim=i, team=team, cosubscript=co)
          ALSO(co .equalsExpected. cosubscripts(i))

          ALSO(co .isAtLeast. colbound(i))
          ALSO(co .isatMost. coubound(i))
        end do

        ! verify reverse mapping
        if (present(team)) then
          call prif_image_index_with_team(coarray_handle, cosubscripts, team, i)
        else
          call prif_image_index(coarray_handle, cosubscripts, i)
        end if
        ALSO(i .equalsExpected. me)

        ! and prif_initial_team_index
        if (present(team)) then
          call prif_initial_team_index_with_team(coarray_handle, cosubscripts, team, i)
        else
          call prif_initial_team_index(coarray_handle, cosubscripts, i)
        end if
        ALSO(i .equalsExpected. me_initial)

    end function

    function check_simple_case() result(diag)
        type(test_diagnosis_t) :: diag

        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer(c_int) :: answer, ni

        diag = .true.

        call prif_num_images(num_images=ni)

        call prif_allocate_coarray( &
                lcobounds = [1_c_int64_t], &
                ucobounds = [ni+2_c_int64_t], &
                size_in_bytes = 1_c_size_t, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call prif_image_index(coarray_handle, [1_c_int64_t], image_index=answer)
        ALSO(answer .equalsExpected. 1_c_int )

        call prif_initial_team_index(coarray_handle, [1_c_int64_t], initial_team_index=answer)
        ALSO(answer .equalsExpected. 1_c_int)

        ALSO(check_this_image_coarray(coarray_handle, 1))

        call prif_deallocate_coarray(coarray_handle)
    end function

    function check_lower_bounds() result(diag)
        type(test_diagnosis_t) :: diag

        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer(c_int) :: answer, ni

        diag = .true.

        call prif_num_images(num_images=ni)

        call prif_allocate_coarray( &
                lcobounds = [2_c_int64_t, 3_c_int64_t], &
                ucobounds = [3_c_int64_t, ni+4_c_int64_t], &
                size_in_bytes = 1_c_size_t, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call prif_image_index(coarray_handle, [2_c_int64_t, 3_c_int64_t], image_index=answer)
        ALSO(answer .equalsExpected. 1_c_int)

        call prif_initial_team_index(coarray_handle, [2_c_int64_t, 3_c_int64_t], initial_team_index=answer)
        ALSO(answer .equalsExpected. 1_c_int)

        ALSO(check_this_image_coarray(coarray_handle, 2))

        call prif_deallocate_coarray(coarray_handle)
    end function

    function check_invalid_subscripts() result(diag)
        type(test_diagnosis_t) diag

        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer(c_int) :: answer, ni

        diag = .true.

        call prif_num_images(num_images=ni)

        call prif_allocate_coarray( &
                lcobounds = [-2_c_int64_t, 2_c_int64_t], &
                ucobounds = [2_c_int64_t, ni+6_c_int64_t], &
                size_in_bytes = 1_c_size_t, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call prif_image_index(coarray_handle, [-1_c_int64_t, 1_c_int64_t], image_index=answer)
        ALSO(answer .equalsExpected. 0_c_int)

        ALSO(check_this_image_coarray(coarray_handle, 2))

        call prif_deallocate_coarray(coarray_handle)
    end function

    function check_complicated_2d() result(diag)
        type(test_diagnosis_t) :: diag

        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer(c_int) :: answer, ni, expected

        diag = .true.

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
        ALSO(answer .equalsExpected. expected)

        if (expected > 0) then
          call prif_initial_team_index(coarray_handle, [1_c_int64_t, 3_c_int64_t], initial_team_index=answer)
          ALSO(answer .equalsExpected. expected)
        end if

        ALSO(check_this_image_coarray(coarray_handle, 2))

        call prif_deallocate_coarray(coarray_handle)
    end function

    function check_complicated_3d() result(diag)
        type(test_diagnosis_t) diag

        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer(c_int) :: answer, ni, expected
        type(prif_team_type) :: initial_team

        diag = .true.

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
        ALSO(answer .equalsExpected. expected)

        if (expected > 0) then
          call prif_initial_team_index_with_team(coarray_handle, &
                           [2_c_int64_t, 1_c_int64_t, 1_c_int64_t], &
                           team=initial_team, initial_team_index=answer)
          ALSO(answer .equalsExpected. expected)
        endif

        ALSO(check_this_image_coarray(coarray_handle, 3))

        call prif_deallocate_coarray(coarray_handle)
    end function

    function check_complicated_2d_team() result(diag)
        type(test_diagnosis_t) diag

        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        integer(c_int) :: answer, ni, cni, me
        integer(c_int64_t) :: which_team
        type(prif_team_type) :: initial_team, child_team

        diag = .true.

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
          ALSO(answer .equalsExpected. 1_c_int)

          call prif_image_index_with_team_number(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              team_number=-1_c_int64_t, image_index=answer)
          ALSO(answer .equalsExpected. 1_c_int)

          call prif_image_index_with_team(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              team=child_team, image_index=answer)
          ALSO(answer .equalsExpected. 1_c_int)

          call prif_image_index_with_team_number(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              team_number=which_team, image_index=answer)
          ALSO(answer .equalsExpected. 1_c_int)

          call prif_image_index(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              image_index=answer)
          ALSO(answer .equalsExpected. 1_c_int)

          ! initial_team_index lcobound

          call prif_initial_team_index_with_team(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              initial_team, answer)
          ALSO(answer .equalsExpected. 1_c_int)

          call prif_initial_team_index_with_team_number(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              -1_c_int64_t, answer)
          ALSO(answer .equalsExpected. 1_c_int)

          call prif_initial_team_index_with_team(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              child_team, answer)
          ALSO(answer .equalsExpected. merge(1_c_int,2_c_int,which_team==1))

          call prif_initial_team_index_with_team_number(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              which_team, answer)
          ALSO(answer .equalsExpected. merge(1_c_int,2_c_int,which_team==1))

          call prif_initial_team_index(coarray_handle, &
                              [0_c_int64_t, 2_c_int64_t], &
                              answer)
          ALSO(answer .equalsExpected. merge(1_c_int,2_c_int,which_team==1))

          ! image_index 3

          call prif_image_index_with_team(coarray_handle, &
                              [0_c_int64_t, 3_c_int64_t], &
                              team=initial_team, image_index=answer)
          ALSO(answer .equalsExpected. merge(3_c_int,0_c_int,ni >= 3))

          call prif_image_index_with_team_number(coarray_handle, &
                              [0_c_int64_t, 3_c_int64_t], &
                              team_number=-1_c_int64_t, image_index=answer)
          ALSO(answer .equalsExpected. merge(3_c_int,0_c_int,ni >= 3))

          call prif_image_index_with_team(coarray_handle, &
                              [0_c_int64_t, 3_c_int64_t], &
                              team=child_team, image_index=answer)
          ALSO(answer .equalsExpected. merge(3_c_int,0_c_int,cni >= 3))

          call prif_image_index_with_team_number(coarray_handle, &
                              [0_c_int64_t, 3_c_int64_t], &
                              team_number=which_team, image_index=answer)
          ALSO(answer .equalsExpected. merge(3_c_int,0_c_int,cni >= 3))

          call prif_image_index(coarray_handle, &
                              [0_c_int64_t, 3_c_int64_t], &
                              image_index=answer)
          ALSO(answer .equalsExpected. merge(3_c_int,0_c_int,cni >= 3))

          ! initial_team_index 3
          if (ni >= 3) then
            call prif_initial_team_index_with_team(coarray_handle, &
                                [0_c_int64_t, 3_c_int64_t], &
                                team=initial_team, initial_team_index=answer)
            ALSO(answer .equalsExpected. 3_c_int)
  
            call prif_initial_team_index_with_team_number(coarray_handle, &
                                [0_c_int64_t, 3_c_int64_t], &
                                team_number=-1_c_int64_t, initial_team_index=answer)
            ALSO(answer .equalsExpected. 3_c_int)
          end if
          if (cni >= 3) then
            call prif_initial_team_index_with_team(coarray_handle, &
                                [0_c_int64_t, 3_c_int64_t], &
                                team=child_team, initial_team_index=answer)
            ALSO(answer .equalsExpected. merge(5_c_int,6_c_int,which_team==1))
  
            call prif_initial_team_index_with_team_number(coarray_handle, &
                                [0_c_int64_t, 3_c_int64_t], &
                                team_number=which_team, initial_team_index=answer)
            ALSO(answer .equalsExpected. merge(5_c_int,6_c_int,which_team==1))
  
            call prif_initial_team_index(coarray_handle, &
                                [0_c_int64_t, 3_c_int64_t], &
                                initial_team_index=answer)
            ALSO(answer .equalsExpected. merge(5_c_int,6_c_int,which_team==1))
          end if

          ALSO(check_this_image_coarray(coarray_handle, 2, initial_team))
          ALSO(check_this_image_coarray(coarray_handle, 2, child_team))

        call prif_end_team()
        call prif_deallocate_coarray(coarray_handle)
    end function

end module
