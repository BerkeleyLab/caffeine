module caf_teams_test
    use iso_c_binding, only: c_size_t, c_ptr, c_intmax_t, c_null_funptr
    use prif, only: &
            prif_coarray_handle, &
            prif_allocate_coarray, &
            prif_deallocate_coarray, &
            prif_this_image_no_coarray, &
            prif_num_images, &
            prif_team_type, &
            prif_form_team, &
            prif_change_team, &
            prif_end_team
    use veggies, only: result_t, test_item_t, assert_equals, describe, it, succeed

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
        integer :: dummy_element, initial_num_imgs, num_imgs, me, i
        integer(c_size_t) :: element_size
        integer(c_intmax_t) :: which_team
        integer, parameter :: num_coarrays = 4
        type(prif_coarray_handle) :: coarrays(num_coarrays)
        type(c_ptr) :: allocated_memory
        type(prif_team_type) :: team

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(num_images=initial_num_imgs)
        which_team = merge(1_c_intmax_t, 2_c_intmax_t, mod(me, 2) == 0)
        element_size = int(storage_size(dummy_element)/8, c_size_t)
        call prif_form_team(team_number = which_team, team = team)
        call prif_change_team(team)
            call prif_num_images(num_images=num_imgs)
            result_ = assert_equals( &
                    initial_num_imgs/2 + mod(initial_num_imgs,2)*(int(which_team)-1), &
                    num_imgs, &
                    "Team has correct number of images")
            do i = 1, num_coarrays
                call prif_allocate_coarray( &
                    lcobounds = [1_c_intmax_t], &
                    ucobounds = [int(num_imgs, c_intmax_t)], &
                    lbounds = [integer(c_intmax_t)::], &
                    ubounds = [integer(c_intmax_t)::], &
                    element_size = element_size, &
                    final_func = c_null_funptr, &
                    coarray_handle = coarrays(i), &
                    allocated_memory = allocated_memory)
            end do
            call prif_deallocate_coarray(coarrays(4:4))
            call prif_deallocate_coarray(coarrays(2:2))
        call prif_end_team()
        result_ = result_.and.succeed("Seems to have worked")
    end function
end module
