! Copyright (c) 2024-2025, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

program test_suite_driver
  use julienne_m
  use prif, only: prif_this_image_no_coarray, prif_num_images, prif_sync_all, prif_co_sum, prif_error_stop
  use iso_c_binding, only: c_int, c_bool
  use prif_init_test_m, only : prif_init_test_t, check_caffeination
  use prif_num_images_test_m, only : prif_num_images_test_t
  use prif_this_image_no_coarray_test_m, only : prif_this_image_no_coarray_test_t
  use prif_image_queries_test_m, only : prif_image_queries_test_t
  use prif_types_test_m, only : prif_types_test_t
  use prif_co_broadcast_test_m, only : prif_co_broadcast_test_t
  use prif_co_sum_test_m, only : prif_co_sum_test_t
  use prif_co_max_test_m, only : prif_co_max_test_t
  use prif_co_min_test_m, only : prif_co_min_test_t
  use prif_co_reduce_test_m, only :prif_co_reduce_test_t 
  use prif_sync_images_test_m, only : prif_sync_images_test_t
  use prif_image_index_test_m, only : prif_image_index_test_t
  use prif_allocate_test_m, only : prif_allocate_test_t
  use prif_coarray_inquiry_test_m, only :  prif_coarray_inquiry_test_t
  use prif_teams_test_m, only : prif_teams_test_t 
  use prif_rma_test_m, only : prif_rma_test_t   
  use prif_strided_test_m, only : prif_strided_test_t
  use prif_event_test_m, only : prif_event_test_t 
  use prif_image_queries_test_m, only : prif_image_queries_test_t
  use prif_atomic_test_m, only : prif_atomic_test_t
  use prif_error_stop_test_m, only : prif_error_stop_test_t
  use prif_stop_test_m, only : prif_stop_test_t  
  implicit none

  type(test_diagnosis_t) :: dummy
  dummy = check_caffeination() ! ensure an early call to prif_init

# if JULIENNE_PARALLEL_CALLBACKS
    julienne_this_image => julienne_callback_this_image
    julienne_num_images => julienne_callback_num_images
    julienne_sync_all => julienne_callback_sync_all
    julienne_co_sum_integer => julienne_callback_co_sum_integer
    julienne_error_stop => julienne_callback_error_stop
# endif

  associate(test_harness => test_harness_t([ &
    ! tests for basic functionality that are mostly self-contained
     test_fixture_t( prif_init_test_t() ) &
    ,test_fixture_t( prif_num_images_test_t() ) &
    ,test_fixture_t( prif_this_image_no_coarray_test_t() ) &
    ,test_fixture_t( prif_image_queries_test_t() ) &
    ,test_fixture_t( prif_types_test_t() ) &

    ! collectives tests
    ,test_fixture_t( prif_co_broadcast_test_t() ) &
    ,test_fixture_t( prif_co_sum_test_t() ) &
    ,test_fixture_t( prif_co_max_test_t() ) &
    ,test_fixture_t( prif_co_min_test_t() ) &
    ,test_fixture_t( prif_co_reduce_test_t() ) &

    ! tests that rely primarily upon coarrays
    ,test_fixture_t( prif_allocate_test_t() ) & ! should be first coarray test
    ,test_fixture_t( prif_coarray_inquiry_test_t() ) &
    ,test_fixture_t( prif_image_index_test_t() ) &
    ,test_fixture_t( prif_rma_test_t() ) &
    ,test_fixture_t( prif_strided_test_t() ) &

    ! synchronization and data race tests
    ,test_fixture_t( prif_event_test_t() ) &
    ,test_fixture_t( prif_atomic_test_t() ) &
    ,test_fixture_t( prif_sync_images_test_t() ) & ! internally uses coarrays and events

    ! complicated multi-feature tests
    ,test_fixture_t( prif_teams_test_t() ) &

    ! exit tests
    ,test_fixture_t( prif_error_stop_test_t() ) &
    ,test_fixture_t( prif_stop_test_t() ) &
  ]))
    call test_harness%report_results
  end associate

contains

  function julienne_callback_this_image() result(this_image_id)
    implicit none
    integer :: this_image_id
    integer(c_int) :: me

    call prif_this_image_no_coarray(this_image=me)

    this_image_id = int(me)
  end function

  function julienne_callback_num_images() result(image_count)
    implicit none
    integer :: image_count
    integer(c_int) :: ni

    call prif_num_images(ni)

    image_count = int(ni)
  end function

  subroutine julienne_callback_sync_all()
    implicit none
    
    call prif_sync_all()
  end subroutine

  subroutine julienne_callback_co_sum_integer(a, result_image)
    implicit none
    integer, intent(inout), target :: a(:)
    integer, intent(in), optional :: result_image
    
    call prif_co_sum(a, result_image)
  end subroutine

  subroutine julienne_callback_error_stop(stop_code_char)
    implicit none
    character(len=*), intent(in) :: stop_code_char
    
    call prif_error_stop(quiet=.false._c_bool, stop_code_char=stop_code_char)    
  end subroutine

end program test_suite_driver
