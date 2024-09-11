! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

program main
  !! Test the Caffeine implementation of the Parallel Runtime Interface for Fortran (PRIF)
  use julienne_m, only : command_line_t, GitHub_CI
  use prif, only : prif_this_image_no_coarray, prif_sync_all
  use prif_allocate_test_m, only : prif_allocate_test_t
  use prif_co_broadcast_test_m, only : prif_co_broadcast_test_t
  use prif_co_max_test_m, only : prif_co_max_test_t
  use prif_co_min_test_m, only : prif_co_min_test_t
  use prif_co_reduce_test_m, only : prif_co_reduce_test_t
  use prif_co_sum_test_m, only : prif_co_sum_test_t
  use prif_error_stop_test_m, only : prif_error_stop_test_t
  use prif_image_index_test_m, only : prif_image_index_test_t
  use prif_init_test_m, only : prif_init_test_t
  use prif_num_images_test_m, only : prif_num_images_test_t
  use prif_rma_test_m, only : prif_rma_test_t
  use prif_stop_test_m, only : prif_stop_test_t
  use prif_teams_test_m, only : prif_teams_test_t
  use prif_this_image_test_m, only : prif_this_image_test_t
  implicit none

  integer :: passes=0, tests=0
  integer me

  call stop_and_print_usage_info_if_help_requested
  call run_tests_and_report(passes, tests)
  call prif_this_image_no_coarray(this_image=me)

  if (me==1) print *, new_line(''), "_________ In total, ",passes," of ",tests, " tests pass. _________"
  call prif_sync_all
  ! PRIF sec 5.2 requires the client to eventually call prif_error_stop or prif_stop
  if (passes /= tests) call prif_error_stop(quiet=.false.)
  else call prif_stop(quiet=.true.)

contains

  subroutine stop_and_print_usage_info_if_help_requested
    type(command_line_t) command_line

    character(len=*), parameter :: usage = &
      new_line('') // new_line('') // &
      'Usage: fpm test -- [--help] | [--contains <substring>]' // &
      new_line('') // new_line('') // &
      'where square brackets ([]) denote optional arguments, a pipe (|) separates alternative arguments,' // new_line('') // &
      'angular brackets (<>) denote a user-provided value, and passing a substring limits execution to' // new_line('') // &
      'the tests with test subjects or test descriptions containing the user-specified substring.' // new_line('') 

    if (command_line%argument_present([character(len=len("--help"))::"--help","-h"])) stop usage

  end subroutine

  subroutine run_tests_and_report(passes, tests)
    integer, intent(inout) :: passes, tests

    type(prif_allocate_test_t) prif_allocate_test
    type(prif_co_broadcast_test_t) prif_co_broadcast_test
    type(prif_co_max_test_t) prif_co_max_test
    type(prif_co_min_test_t) prif_co_min_test
    type(prif_co_reduce_test_t) prif_co_reduce_test
    type(prif_co_sum_test_t) prif_co_sum_test
    type(prif_error_stop_test_t) prif_error_stop_test
    type(prif_image_index_test_t) prif_image_index_test
    type(prif_init_test_t) prif_init_test
    type(prif_num_images_test_t) prif_num_images_test
    type(prif_rma_test_t) prif_rma_test
    type(prif_stop_test_t) prif_stop_test
    type(prif_teams_test_t) prif_teams_test
    type(prif_this_image_test_t) prif_this_image_test

    call prif_init_test%report(passes, tests)
    call prif_allocate_test%report(passes, tests)
    call prif_num_images_test%report(passes, tests)
    call prif_this_image_test%report(passes, tests)
    call prif_co_broadcast_test%report(passes, tests)
    call prif_teams_test%report(passes, tests)
#ifdef __flang__
    print *
    print *,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    print *
    print *,"LLVM Flang detected. Skipping tests that crash:"
    print *,"  - prif_co_max_test"
    print *,"  - prif_co_min_test"
    print *,"  - prif_co_reduce_test"
    print *,"  - prif_co_sum_test"
    print *,"  - prif_error_stop_test"
    print *,"  - prif_image_index_test"
    print *,"  - prif_stop_test"
    print *
    print *,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
#else
    call prif_co_max_test%report(passes, tests)
    call prif_co_min_test%report(passes, tests)
    call prif_co_reduce_test%report(passes, tests)
    call prif_co_sum_test%report(passes, tests)
    call prif_error_stop_test%report(passes, tests)
    call prif_image_index_test%report(passes, tests)
    call prif_rma_test%report(passes, tests)
    call prif_stop_test%report(passes, tests)
#endif

  end subroutine run_tests_and_report

end program
