! Copyright (c) 2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#if defined(__flang__)
  #define NO_MULTI_IMAGE_SUPPORT
#endif

program main
  !! Test Caffeine implementation of PRIF
  use julienne_m, only : command_line_t, GitHub_CI
  use prif_init_test_m, only : prif_init_test_t
  use prif_allocate_test_m, only : prif_allocate_test_t
  use prif_num_images_test_m, only : prif_num_images_test_t
  use prif_this_image_test_m, only : prif_this_image_test_t
  use prif_error_stop_test_m, only : prif_error_stop_test_t
  use prif_stop_test_m, only : prif_stop_test_t
  implicit none

  call stop_and_print_usage_info_if_help_requested
  call run_tests_and_report_results

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

  subroutine run_tests_and_report_results
    type(prif_init_test_t) prif_init_test
    type(prif_allocate_test_t) prif_allocate_test
    type(prif_num_images_test_t) prif_num_images_test
    type(prif_this_image_test_t) prif_this_image_test
    type(prif_stop_test_t) prif_stop_test
    type(prif_error_stop_test_t) prif_error_stop_test
    integer :: passes=0, tests=0

    call prif_init_test%report(passes, tests)
    call prif_allocate_test%report(passes, tests)
    call prif_num_images_test%report(passes, tests)
    call prif_this_image_test%report(passes, tests)
    call prif_stop_test%report(passes, tests)
    call prif_stop_test%report(passes, tests)
    call prif_error_stop_test%report(passes, tests)

#ifndef NO_MULTI_IMAGE_SUPPORT
    if (this_image()==1) &
#endif
    print *, new_line(''), "_________ In total, ",passes," of ",tests, " tests pass. _________"
    if (passes /= tests) error stop
  end subroutine run_tests_and_report_results

end program
