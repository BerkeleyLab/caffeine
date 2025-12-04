module prif_teams_test_m
    use iso_c_binding, only: c_size_t, c_ptr, c_null_funptr, c_int64_t, c_int
    use prif
#if FORCE_PRIF_0_5 || FORCE_PRIF_0_6
  use prif, only : prif_deallocate_coarray_ => prif_deallocate_coarray
# define prif_deallocate_coarray(h)    prif_deallocate_coarray_([h])
# define prif_deallocate_coarrays(arr) prif_deallocate_coarray_(arr)
#endif
    use julienne_m, only: passing_test, test_description_t, test_diagnosis_t, test_result_t, test_t, usher &
      ,operator(.also.), operator(.isAtLeast.), operator(.isAtMost.), operator(.equalsExpected.), operator(//)

    implicit none
    private
    public :: prif_teams_test_t

    type, extends(test_t) :: prif_teams_test_t
    contains
      procedure, nopass, non_overridable :: subject
      procedure, nopass, non_overridable :: results
    end type

contains
    pure function subject()
      character(len=:), allocatable :: subject
      subject = "PRIF Teams"
    end function

    function results() result(test_results)
        type(test_result_t), allocatable :: test_results(:)
        type(prif_teams_test_t) prif_teams_test

        test_results = prif_teams_test%run([ &
            test_description_t("creating, changing to, and allocating coarrays", usher(check_teams)) &
        ])
    end function

    function check_teams() result(diag)
        type(test_diagnosis_t) :: diag

        ! TODO: use final_func to observe automatic deallocation of coarrays
        integer :: dummy_element, i
        integer(c_int) :: initial_num_imgs, num_imgs, me, me_child, x
        integer(c_size_t) :: element_size
        integer(c_int64_t) :: which_team, n
        integer, parameter :: num_coarrays = 4
        type(prif_coarray_handle) :: coarrays(num_coarrays)
        type(c_ptr) :: allocated_memory
        type(prif_team_type) :: team, initial_team, t

        diag = passing_test()

        call prif_num_images(num_images=initial_num_imgs)
        diag = diag .also. (initial_num_imgs .isAtLeast. 1) &
          // "invalid prif_num_images"

        call prif_this_image_no_coarray(this_image=me)
        diag = diag .also. ((me .isAtLeast. 1) .also. (me .isAtMost. initial_num_imgs)) &
          // "invalid prif_this_image"

        n = 0 ! clear outputs
        call prif_team_number(team_number=n)
        diag = diag .also. (int(n) .equalsExpected. -1) &
          // "Initial team number is -1"

        n = 0 ! clear outputs
        call prif_get_team(team=initial_team)
        call prif_team_number(team=initial_team, team_number=n)
        diag = diag .also. (-1 .equalsExpected. int(n)) &
          // "prif_get_team retrieval of current initial team"
          
        ! ensure prif_sync_team is usable
        call prif_sync_team(team=initial_team)

        x = 0 ! clear outputs
        call prif_num_images_with_team(team=initial_team, num_images=x)
        diag = diag .also. (initial_num_imgs .equalsExpected. x) &
          // "prif_num_images in initial team"
          
        x = 0 ! clear outputs
        call prif_num_images_with_team_number(team_number=-1_c_int64_t, num_images=x)
        diag  = diag .also. (initial_num_imgs .equalsExpected. x) &
          // "prif_num_images_with_team_number in initial team"

        x = 0 ! clear outputs
        call prif_this_image_no_coarray(team=initial_team, this_image=x)
        diag = diag .also.(me .equalsExpected. x) &
          // "prif_this_image_no_coarray in initial team"
          
        t = prif_team_type() ; n = 0 ! clear outputs
        call prif_get_team(level=PRIF_INITIAL_TEAM, team=t)
        call prif_team_number(team=t, team_number=n)
        diag = diag .also. (int(n) .equalsExpected. -1) &
          // "prif_get_team(PRIF_INITIAL_TEAM) retrieves initial team"
          
        t = prif_team_type() ; n = 0 ! clear outputs
        call prif_get_team(level=PRIF_CURRENT_TEAM, team=t)
        call prif_team_number(team=t, team_number=n)
        diag = diag .also. (int(n) .equalsExpected. -1) &
          // "prif_get_team(PRIF_CURRENT_TEAM) retrieval of initial team when current team is initial team"

        t = prif_team_type() ; n = 0 ! clear outputs
        call prif_get_team(level=PRIF_PARENT_TEAM, team=t)
        call prif_team_number(team=t, team_number=n)
        diag = diag .also. (-1 .equalsExpected. int(n)) &
          // "prif_get_team(PRIF_PARENT_TEAM) retrieves initial team when parent team is initial team"
          
        which_team = merge(1_c_int64_t, 2_c_int64_t, mod(me, 2) == 0)
        element_size = int(storage_size(dummy_element)/8, c_size_t)
        call prif_form_team(team_number = which_team, team = team)
        call prif_change_team(team)
            call prif_num_images(num_images=num_imgs)
            diag = diag .also. (num_imgs .equalsExpected.  initial_num_imgs/2 + mod(initial_num_imgs,2)*(int(which_team)-1)) &
              // "Team has correct number of images"

            ! ensure prif_sync_team is usable
            call prif_sync_team(team=team)
            call prif_sync_team(team=initial_team)

            x = 0 ! clear outputs
            call prif_num_images_with_team(team=team, num_images=x)
            diag = diag .also. (num_imgs .equalsExpected. x) &
              // "prif_num_images works with team"

            x = 0 ! clear outputs
            call prif_num_images_with_team_number(team_number=which_team, num_images=x)
            diag = diag .also.  (num_imgs .equalsExpected. x) &
              // "prif_num_images_with_team_number works with current team"

            call prif_this_image_no_coarray(this_image=me_child)
            diag = diag .also. ((me - 1)/2 + 1 .equalsExpected. me_child) &
              // "prif_this_image is valid"
              

            x = 0 ! clear outputs
            call prif_this_image_no_coarray(team=team, this_image=x)
            diag = diag .also.  (me_child .equalsExpected. x) &
              // "prif_this_image is valid"

            n = 0 ! clear outputs
            call prif_team_number(team_number=n)
            diag = diag .also. (int(which_team) .equalsExpected. int(n)) &
              // "Correct current team number"
    
            n = 0 ! clear outputs
            call prif_team_number(team=team, team_number=n)
            diag = diag .also. (int(which_team) .equalsExpected. int(n)) &
              // "Correct current team number"
    
            t = prif_team_type() ; n = 0 ! clear outputs
            call prif_get_team(team=t)
            call prif_team_number(team=t, team_number=n)
            diag = diag .also. (int(which_team) .equalsExpected. int(n)) &
              // "prif_get_team retrieves current team"
    
            t = prif_team_type() ; n = 0 ! clear outputs
            call prif_get_team(level=PRIF_INITIAL_TEAM, team=t)
            call prif_team_number(team=t, team_number=n)
            diag = diag .also. (-1 .equalsExpected. int(n)) &
              // "prif_get_team(PRIF_INITIAL_TEAM) retrieves initial team"
    
            t = prif_team_type() ; n = 0 ! clear outputs
            call prif_get_team(level=PRIF_CURRENT_TEAM, team=t)
            call prif_team_number(team=t, team_number=n)
            diag = diag .also. (int(which_team) .equalsExpected. int(n)) &
              // "prif_get_team(PRIF_CURRENT_TEAM) retrieves current team"
    
            t = prif_team_type() ; n = 0 ! clear outputs
            call prif_get_team(level=PRIF_PARENT_TEAM, team=t)
            call prif_team_number(team=t, team_number=n)
            diag = diag .also. (-1 .equalsExpected. int(n)) &
              // "prif_get_team(PRIF_PARENT_TEAM) retrieves initial team when parent team is initial team"

            x = 0 ! clear outputs
            call prif_num_images_with_team(team=initial_team, num_images=x)
            diag = diag .also. (initial_num_imgs .equalsExpected. x) &
              // "prif_num_images works with initial team"

            x = 0 ! clear outputs
            call prif_num_images_with_team_number(team_number=-1_c_int64_t, num_images=x)
            diag = diag .also. (initial_num_imgs .equalsExpected. x) &
              // "prif_num_images_with_team_number works with initial team"

            x = 0 ! clear outputs
            call prif_this_image_no_coarray(team=initial_team, this_image=x)
            diag = diag .also. (me .equalsExpected. x) &
              // "prif_this_image_no_coarray works with initial team"

            do i = 1, num_coarrays
                call prif_allocate_coarray( &
                    lcobounds = [1_c_int64_t], &
                    ucobounds = [int(num_imgs, c_int64_t)], &
                    size_in_bytes = element_size, &
                    final_func = c_null_funptr, &
                    coarray_handle = coarrays(i), &
                    allocated_memory = allocated_memory)
            end do
            call prif_deallocate_coarrays(coarrays(4:4))
            call prif_deallocate_coarrays(coarrays(2:2))

        call prif_end_team()

        ! ensure prif_sync_team is usable
        call prif_sync_team(team=team)
        call prif_sync_team(team=initial_team)

        t = prif_team_type() ; n = 0 ! clear outputs
        call prif_get_team(team=t)
        call prif_team_number(team=t, team_number=n)
        diag = diag .also. (-1 .equalsExpected. int(n)) &
          // "prif_end_team restores initial team"

        diag = diag // "prif_team failure(s)"
    end function
end module prif_teams_test_m
