! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module prif_teams_test_m
  !! Unit test for Caffeine's support for teams
  use iso_c_binding, only: c_size_t, c_ptr, c_intmax_t, c_null_funptr
  use prif_test_m, only : prif_test_t, test_description_substring
  use prif, only: &
     prif_coarray_handle, prif_allocate_coarray, prif_deallocate_coarray, prif_this_image_no_coarray, prif_num_images &
    ,prif_team_type,      prif_form_team,        prif_change_team,        prif_end_team
  use julienne_m, only : test_result_t, test_description_t
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : test_function_i
#endif
  implicit none

  private
  public :: prif_teams_test_t

  type, extends(prif_test_t) :: prif_teams_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The teams feature set" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [test_description_t("team creation, change, and coarray allocation", check_teams)]

#else
    procedure(test_function_i), pointer :: check_teams_ptr
    check_teams_ptr => check_teams
    test_descriptions = [test_description_t("team creation, change, and coarray allocation", check_teams_ptr)]
#endif

    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0  &
      .or. test_descriptions%contains_text(test_description_substring))
      
    test_results = test_descriptions%run()
  end function

  function check_teams() result(test_passes)
      logical test_passes
      integer dummy_element, initial_num_imgs, num_imgs, me, i
      integer(c_size_t) element_size
      integer(c_intmax_t) which_team
      integer, parameter :: num_coarrays = 4
      type(prif_coarray_handle) coarrays(num_coarrays)
      type(c_ptr) allocated_memory
      type(prif_team_type) team

      call prif_this_image_no_coarray(this_image=me)
      call prif_num_images(num_images=initial_num_imgs)
      which_team = merge(1_c_intmax_t, 2_c_intmax_t, mod(me, 2) == 0)
      call prif_form_team(team_number = which_team, team = team)
      call prif_change_team(team)
      call prif_num_images(num_images=num_imgs)

      test_passes = num_imgs == initial_num_imgs/2 + mod(initial_num_imgs,2)*(int(which_team)-1)

      do i = 1, num_coarrays
          call prif_allocate_coarray( &
              lcobounds = [1_c_intmax_t], &
              ucobounds = [int(num_imgs, c_intmax_t)], &
              final_func = c_null_funptr, &
              size_in_bytes = int(storage_size(dummy_element)/8, c_size_t), &
              coarray_handle = coarrays(i), &
              allocated_memory = allocated_memory)
      end do
      call prif_deallocate_coarray(coarrays(4:4))
      call prif_deallocate_coarray(coarrays(2:2))

      ! TODO: use final_func to observe automatic deallocation of coarrays

      call prif_end_team()
  end function

end module prif_teams_test_m
