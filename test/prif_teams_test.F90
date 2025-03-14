module caf_teams_test
    use iso_c_binding, only: c_size_t, c_ptr, c_null_funptr, c_int64_t, c_int
    use prif
    use veggies, only: result_t, test_item_t, assert_equals, assert_that, describe, it, succeed, fail

    implicit none
    private
    public :: test_caf_teams
contains
    function test_caf_teams() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
            "Teams", &
            [ it("can be created, changed to, and allocate coarrays", check_teams) &
            ])
    end function

    function check_teams() result(result_)
        type(result_t) :: result_

        ! TODO: use final_func to observe automatic deallocation of coarrays
        integer :: dummy_element, i
        integer(c_int) :: initial_num_imgs, num_imgs, me, me_child, x
        integer(c_size_t) :: element_size
        integer(c_int64_t) :: which_team, n
        integer, parameter :: num_coarrays = 4
        type(prif_coarray_handle) :: coarrays(num_coarrays)
        type(c_ptr) :: allocated_memory
        type(prif_team_type) :: team, initial_team, t

        result_ = succeed("")

        call prif_num_images(num_images=initial_num_imgs)
        result_ = result_ .and. &
          assert_that(initial_num_imgs > 0, "prif_num_images is valid")

        call prif_this_image_no_coarray(this_image=me)
        result_ = result_ .and. &
          assert_that(me >= 1 .and. me <= initial_num_imgs, "prif_this_image is valid")

        n = 0 ! clear outputs
        call prif_team_number(team_number=n)
        result_ = result_ .and. &
          assert_equals(int(n), -1, "Initial team number is -1")

        n = 0 ! clear outputs
        call prif_get_team(team=initial_team)
        call prif_team_number(team=initial_team, team_number=n)
        result_ = result_ .and. &
          assert_equals(int(n), -1, "prif_get_team retrieves current initial team")

        ! ensure prif_sync_team is usable
        call prif_sync_team(team=initial_team)

        x = 0 ! clear outputs
        call prif_num_images_with_team(team=initial_team, num_images=x)
        result_ = result_ .and. &
          assert_equals(x, initial_num_imgs, "prif_num_images works with initial team")

        x = 0 ! clear outputs
        call prif_this_image_no_coarray(team=initial_team, this_image=x)
        result_ = result_ .and. &
          assert_equals(x, me, "prif_this_image_no_coarray works with initial team")

        t = prif_team_type() ; n = 0 ! clear outputs
        call prif_get_team(level=PRIF_INITIAL_TEAM, team=t)
        call prif_team_number(team=t, team_number=n)
        result_ = result_ .and. &
          assert_equals(int(n), -1, "prif_get_team(PRIF_INITIAL_TEAM) retrieves initial team")

        t = prif_team_type() ; n = 0 ! clear outputs
        call prif_get_team(level=PRIF_CURRENT_TEAM, team=t)
        call prif_team_number(team=t, team_number=n)
        result_ = result_ .and. &
          assert_equals(int(n), -1, "prif_get_team(PRIF_CURRENT_TEAM) retrieves initial team when current team is initial team")

        t = prif_team_type() ; n = 0 ! clear outputs
        call prif_get_team(level=PRIF_PARENT_TEAM, team=t)
        call prif_team_number(team=t, team_number=n)
        result_ = result_ .and. &
          assert_equals(int(n), -1, "prif_get_team(PRIF_PARENT_TEAM) retrieves initial team when parent team is initial team")

        which_team = merge(1_c_int64_t, 2_c_int64_t, mod(me, 2) == 0)
        element_size = int(storage_size(dummy_element)/8, c_size_t)
        call prif_form_team(team_number = which_team, team = team)
        call prif_change_team(team)
            call prif_num_images(num_images=num_imgs)
            result_ = result_ .and. &
              assert_equals( &
                    initial_num_imgs/2 + mod(initial_num_imgs,2)*(int(which_team)-1), &
                    num_imgs, &
                    "Team has correct number of images")

            ! ensure prif_sync_team is usable
            call prif_sync_team(team=team)
            call prif_sync_team(team=initial_team)

            x = 0 ! clear outputs
            call prif_num_images_with_team(team=team, num_images=x)
            result_ = result_ .and. &
              assert_equals(x, num_imgs, "prif_num_images works with team")

            call prif_this_image_no_coarray(this_image=me_child)
            result_ = result_ .and. &
              assert_equals(me_child, (me - 1)/2 + 1, "prif_this_image is valid")

            x = 0 ! clear outputs
            call prif_this_image_no_coarray(team=team, this_image=x)
            result_ = result_ .and. &
              assert_equals(x, me_child, "prif_this_image is valid")

            n = 0 ! clear outputs
            call prif_team_number(team_number=n)
            result_ = result_ .and. &
              assert_equals(int(n), int(which_team), "Correct current team number")
    
            n = 0 ! clear outputs
            call prif_team_number(team=team, team_number=n)
            result_ = result_ .and. &
              assert_equals(int(n), int(which_team), "Correct current team number")
    
            t = prif_team_type() ; n = 0 ! clear outputs
            call prif_get_team(team=t)
            call prif_team_number(team=t, team_number=n)
            result_ = result_ .and. &
              assert_equals(int(n), int(which_team), "prif_get_team retrieves current team")
    
            t = prif_team_type() ; n = 0 ! clear outputs
            call prif_get_team(level=PRIF_INITIAL_TEAM, team=t)
            call prif_team_number(team=t, team_number=n)
            result_ = result_ .and. &
              assert_equals(int(n), -1, "prif_get_team(PRIF_INITIAL_TEAM) retrieves initial team")
    
            t = prif_team_type() ; n = 0 ! clear outputs
            call prif_get_team(level=PRIF_CURRENT_TEAM, team=t)
            call prif_team_number(team=t, team_number=n)
            result_ = result_ .and. &
              assert_equals(int(n), int(which_team), "prif_get_team(PRIF_CURRENT_TEAM) retrieves current team")
    
            t = prif_team_type() ; n = 0 ! clear outputs
            call prif_get_team(level=PRIF_PARENT_TEAM, team=t)
            call prif_team_number(team=t, team_number=n)
            result_ = result_ .and. &
              assert_equals(int(n), -1, "prif_get_team(PRIF_PARENT_TEAM) retrieves initial team when parent team is initial team")

            x = 0 ! clear outputs
            call prif_num_images_with_team(team=initial_team, num_images=x)
            result_ = result_ .and. &
              assert_equals(x, initial_num_imgs, "prif_num_images works with initial team")

            x = 0 ! clear outputs
            call prif_this_image_no_coarray(team=initial_team, this_image=x)
            result_ = result_ .and. &
              assert_equals(x, me, "prif_this_image_no_coarray works with initial team")


            do i = 1, num_coarrays
                call prif_allocate_coarray( &
                    lcobounds = [1_c_int64_t], &
                    ucobounds = [int(num_imgs, c_int64_t)], &
                    size_in_bytes = element_size, &
                    final_func = c_null_funptr, &
                    coarray_handle = coarrays(i), &
                    allocated_memory = allocated_memory)
            end do
            call prif_deallocate_coarray(coarrays(4:4))
            call prif_deallocate_coarray(coarrays(2:2))

        call prif_end_team()

        ! ensure prif_sync_team is usable
        call prif_sync_team(team=team)
        call prif_sync_team(team=initial_team)

        t = prif_team_type() ; n = 0 ! clear outputs
        call prif_get_team(team=t)
        call prif_team_number(team=t, team_number=n)
        result_ = result_ .and. &
          assert_equals(int(n), -1, "prif_end_team restores initial team")

        result_ = result_.and.succeed("Seems to have worked")
    end function
end module
