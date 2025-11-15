! Copyright (c) 2024-2025, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

program test_suite_driver
  use julienne_m, only : test_fixture_t, test_harness_t
  use prif_init_test_m, only : prif_init_test_t
  use prif_coarray_inquiry_test_m, only :  prif_coarray_inquiry_test_t
  use prif_co_broadcast_test_m, only : prif_co_broadcast_test_t
  use prif_co_max_test_m, only : prif_co_max_test_t
  use prif_co_min_test_m, only : prif_co_min_test_t
  use prif_co_reduce_test_m, only :prif_co_reduce_test_t 
  use prif_co_sum_test_m, only : prif_co_sum_test_t
  use prif_image_queries_test_m, only : prif_image_queries_test_t
  use prif_num_images_test_m, only : prif_num_images_test_t
  use prif_sync_images_test_m, only : prif_sync_images_test_t
  use prif_this_image_no_coarray_test_m, only : prif_this_image_no_coarray_test_t
  implicit none

  associate(test_harness => test_harness_t([ &
     test_fixture_t( prif_init_test_t() ) &
    ,test_fixture_t( prif_num_images_test_t() ) &
    ,test_fixture_t( prif_this_image_no_coarray_test_t() ) &
    ,test_fixture_t( prif_image_queries_test_t() ) &
    ,test_fixture_t( prif_co_broadcast_test_t() ) &
    ,test_fixture_t( prif_co_sum_test_t() ) &
    ,test_fixture_t( prif_co_max_test_t() ) &
    ,test_fixture_t( prif_co_min_test_t() ) &
    ,test_fixture_t( prif_co_reduce_test_t() ) &
    ,test_fixture_t( prif_coarray_inquiry_test_t() ) &
    ,test_fixture_t( prif_sync_images_test_t() ) &
  ]))
    call test_harness%report_results
  end associate
end program test_suite_driver
