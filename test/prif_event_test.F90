#include "test-utils.F90"
#include "assert_macros.h"

! TEST_ASSERT activates immediate assertions in test code
#if !TEST_ASSERT
#undef  call_assert
#define call_assert(c)
#undef  call_assert_describe
#define call_assert_describe(c,d)
#endif

module prif_event_test_m
    use assert_m
    use iso_c_binding, only: &
            c_ptr, c_int64_t, c_intptr_t, c_size_t, c_null_funptr, c_f_pointer, c_loc, c_sizeof
    use prif, only: &
            prif_event_type, prif_event_post, prif_event_post_indirect, prif_event_wait, prif_event_query, &
            prif_notify_type, prif_notify_wait, prif_put_with_notify, prif_put_strided_with_notify, &
            prif_coarray_handle, &
            prif_allocate_coarray, &
            prif_num_images, &
            prif_put, &
            prif_sync_all, &
            prif_this_image_no_coarray
#if FORCE_PRIF_0_5 || FORCE_PRIF_0_6
  use prif, only : prif_deallocate_coarray_ => prif_deallocate_coarray
# define prif_deallocate_coarray(h)    prif_deallocate_coarray_([h])
# define prif_deallocate_coarrays(arr) prif_deallocate_coarray_(arr)
#else
  use prif, only : prif_deallocate_coarray, prif_deallocate_coarrays
#endif
    use julienne_m, only: test_description_t, test_diagnosis_t, test_result_t, test_t, string_t, usher &
      ,operator(.all.), operator(.also.), operator(.equalsExpected.), operator(//)

    implicit none
    private
    public :: prif_event_test_t

    type, extends(test_t) :: prif_event_test_t
    contains
      procedure, nopass, non_overridable :: subject
      procedure, nopass, non_overridable :: results
    end type

contains
   pure function subject()
     character(len=:), allocatable :: subject
     subject = "PRIF Events"
   end function

    function results() result(test_results)
        type(test_result_t), allocatable :: test_results(:)
        type(prif_event_test_t) prif_event_test

        allocate(test_results, source = prif_event_test%run([ &
           test_description_t("a serial event test", usher(check_event_serial)) &
          ,test_description_t("a parallel hot-spot event test", usher(check_event_parallel)) &
          ,test_description_t("a parallel hot-spot notify test", usher(check_notify)) &
        ]))
    end function

    function test_rand(lo, hi) result(result_)
        integer :: lo, hi, result_
        real :: r
        call random_number(r) ! Generate a uniform random number in [0, 1)
        result_ = int(r * (hi - lo + 1)) + lo
        call_assert(result_ >= lo .and. result_ <= hi)
    end function

    function check_event_serial() result(diag)
        type(test_diagnosis_t) diag 

        integer :: me, num_imgs
        type(prif_event_type) :: dummy_event
        integer(c_size_t) :: sizeof_event
        type(prif_coarray_handle) :: coarray_handle
        type(c_ptr) :: allocated_memory
        type(prif_event_type), pointer :: local_event
        integer(c_intptr_t) :: base_addr

        diag = .true.
        call RANDOM_INIT(REPEATABLE=.true., IMAGE_DISTINCT=.true.)

        sizeof_event = int(storage_size(dummy_event)/8, c_size_t)
        call prif_num_images(num_images=num_imgs)
        call prif_this_image_no_coarray(this_image=me)

        ! type(event_type) :: evt[*]
        call prif_allocate_coarray( &
                lcobounds = [1_c_int64_t], &
                ucobounds = [int(num_imgs,c_int64_t)], &
                size_in_bytes = sizeof_event, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle, &
                allocated_memory = allocated_memory)
        call c_f_pointer(allocated_memory, local_event)
        base_addr = transfer(allocated_memory, base_addr)
        local_event = dummy_event ! default initialize

        block
          integer, parameter :: lim = 10
          integer i, j, expect, c, r
          integer(c_int64_t) :: count
          character(len=50) :: context

          expect = 0
          do i=1, lim
            call prif_event_query(c_loc(local_event), count)
            ALSO2(int(count) .equalsExpected. expect, "event count at top of loop")
            call_assert(expect == int(count))

            do j=1,i
              call prif_event_post(me, coarray_handle, 0_c_size_t)
              expect = expect + 1

              call prif_event_query(c_loc(local_event), count)
              ALSO2(int(count) .equalsExpected. expect, "after event_post")
              call_assert(expect == int(count))

              call prif_event_post_indirect(me, base_addr)
              expect = expect + 1

              call prif_event_query(c_loc(local_event), count)
              ALSO2(int(count) .equalsExpected. expect, "event count after event_post_indirect")
              call_assert(expect == int(count))

              if (expect >= 1) then
                c = test_rand(1, expect)
                if (c > 1) then
                  context = "after event_wait(c)"
                  call prif_event_wait(c_loc(local_event), int(c,c_int64_t))
                else if (test_rand(0,1) == 0) then
                  call_assert(c == 1)
                  context = "after event_wait(1)"
                  call prif_event_wait(c_loc(local_event), 1_c_int64_t)
                else if (test_rand(0,1) == 0) then
                  call_assert(c == 1)
                  context = "after event_wait()"
                  call prif_event_wait(c_loc(local_event))
                else
                  call_assert(c == 1)
                  context = "after event_wait(r)"
                  r = test_rand(-50, 0)
                  call prif_event_wait(c_loc(local_event), int(r,c_int64_t))
                endif
                expect = expect - c

                call prif_event_query(c_loc(local_event), count)
                ALSO2(int(count) .equalsExpected. expect, context)
                call_assert_describe(expect == int(count), context)
              end if
            end do
          end do
        end block 

        call prif_deallocate_coarray(coarray_handle)
    end function


    function check_event_parallel() result(diag)
        type(test_diagnosis_t) :: diag

        integer :: me, num_imgs
        type(prif_event_type) :: dummy_event
        integer(c_size_t) :: sizeof_event, sizeof_int
        type(prif_coarray_handle) :: coarray_handle_evt
        type(prif_coarray_handle) :: coarray_handle_ctr
        type(c_ptr) :: allocated_memory
        type(prif_event_type), pointer :: local_evt
        integer, pointer :: local_ctr(:)

        diag = .true.
        sizeof_event = int(storage_size(dummy_event)/8, c_size_t)
        sizeof_int = c_sizeof(me)
        call prif_num_images(num_images=num_imgs)
        call prif_this_image_no_coarray(this_image=me)

        ! type(event_type) :: evt[*]
        call prif_allocate_coarray( &
                lcobounds = [1_c_int64_t], &
                ucobounds = [int(num_imgs,c_int64_t)], &
                size_in_bytes = sizeof_event, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle_evt, &
                allocated_memory = allocated_memory)
        call c_f_pointer(allocated_memory, local_evt)
        local_evt = dummy_event ! default initialize

        ! integer :: ctr(num_images())[*]
        call prif_allocate_coarray( &
                lcobounds = [1_c_int64_t], &
                ucobounds = [int(num_imgs,c_int64_t)], &
                size_in_bytes = num_imgs * sizeof_int, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle_ctr, &
                allocated_memory = allocated_memory)
        call c_f_pointer(allocated_memory, local_ctr, [num_imgs])
        local_ctr = 0 ! initialize

        call prif_sync_all

        block
          integer, parameter :: lim = 10
          integer, target :: i, j

          do i=1, lim
            ! every image writes a coarray value on image 1, then posts event

            ! ctr(me)[1] = i
            call prif_put( &
                image_num = 1, &
                coarray_handle = coarray_handle_ctr, &
                offset = (me-1) * sizeof_int, &
                current_image_buffer = c_loc(i), &
                size_in_bytes = sizeof_int)

            ! EVENT POST ( evt[1] )
            call prif_event_post(1, coarray_handle_evt, 0_c_size_t)

            if (me == 1) then
              ! image 1 waits on the events, then validates data arrival

              ! EVENT WAIT ( evt, UNTIL_COUNT=num_imgs )
              call prif_event_wait(c_loc(local_evt), int(num_imgs,c_int64_t))

              ! validate ctr(:)[1] == i
              ALSO2(.all. (local_ctr(1:num_imgs) .equalsExpected. i), "gather result")

              ! image 1 writes back a coarray value to each image, then posts an event
              do j=1,num_imgs
                ! ctr(1)[j] = i
                call prif_put( &
                  image_num = j, &
                  coarray_handle = coarray_handle_ctr, &
                  offset = 0_c_size_t, &
                  current_image_buffer = c_loc(i), &
                  size_in_bytes = sizeof_int)

                ! EVENT POST ( evt[j] )
                call prif_event_post(j, coarray_handle_evt, 0_c_size_t)
              end do
            end if


            ! EVENT WAIT ( evt )
            call prif_event_wait(c_loc(local_evt))

            ! validate ctr(1)[me] == i
            ALSO2(local_ctr(1) .equalsExpected. i, "scatter result")

          end do
        end block

        call prif_deallocate_coarrays(([coarray_handle_ctr, coarray_handle_evt]))
    end function

    function check_notify() result(diag)
        type(test_diagnosis_t) diag

        integer :: me, num_imgs
        type(prif_notify_type) :: dummy_notify
        integer(c_size_t) :: sizeof_notify, sizeof_int
        type(prif_coarray_handle) :: coarray_handle_evt
        type(prif_coarray_handle) :: coarray_handle_ctr
        type(c_ptr) :: allocated_memory
        type(prif_notify_type), pointer :: local_evt
        integer, pointer :: local_ctr(:)

        diag = .true.
        sizeof_notify = int(storage_size(dummy_notify)/8, c_size_t)
        sizeof_int = c_sizeof(me)
        call prif_num_images(num_images=num_imgs)
        call prif_this_image_no_coarray(this_image=me)

        ! type(notify_type) :: evt[*]
        call prif_allocate_coarray( &
                lcobounds = [1_c_int64_t], &
                ucobounds = [int(num_imgs,c_int64_t)], &
                size_in_bytes = sizeof_notify, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle_evt, &
                allocated_memory = allocated_memory)
        call c_f_pointer(allocated_memory, local_evt)
        local_evt = dummy_notify ! default initialize

        ! integer :: ctr(num_images())[*]
        call prif_allocate_coarray( &
                lcobounds = [1_c_int64_t], &
                ucobounds = [int(num_imgs,c_int64_t)], &
                size_in_bytes = num_imgs * sizeof_int, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle_ctr, &
                allocated_memory = allocated_memory)
        call c_f_pointer(allocated_memory, local_ctr, [num_imgs])
        local_ctr = 0 ! initialize

        call prif_sync_all

        block
          integer, parameter :: lim = 10
          integer, target :: i, j

          do i=1, lim
            ! every image writes a coarray value on image 1 with notify

            ! ctr(me)[1,notify=evt] = i
            call prif_put_with_notify( &
                image_num = 1, &
                coarray_handle = coarray_handle_ctr, &
                offset = (me-1) * sizeof_int, &
                current_image_buffer = c_loc(i), &
                size_in_bytes = sizeof_int, &
                notify_coarray_handle = coarray_handle_evt, &
                notify_offset = 0_c_size_t)

            if (me == 1) then
              ! image 1 waits on the notifys, then validates data arrival

              ! NOTIFY WAIT ( evt, UNTIL_COUNT=num_imgs )
              call prif_notify_wait(c_loc(local_evt), int(num_imgs,c_int64_t))

              ! validate ctr(:)[1] == i
              ALSO2(.all. (local_ctr(1:num_imgs) .equalsExpected. i), "gather result")

              ! image 1 writes back a coarray value to each image with notify
              do j=1,num_imgs
                ! ctr(1)[j, notify=evt] = i
                call prif_put_strided_with_notify( &
                  image_num = j, &
                  coarray_handle = coarray_handle_ctr, &
                  offset = 0_c_size_t, &
                  remote_stride = [sizeof_int], &
                  current_image_buffer = c_loc(i), &
                  current_image_stride = [sizeof_int], &
                  element_size = sizeof_int, &
                  extent = [1_c_size_t], &
                  notify_coarray_handle = coarray_handle_evt, &
                  notify_offset = 0_c_size_t)
              end do
            end if


            ! NOTIFY WAIT ( evt )
            call prif_notify_wait(c_loc(local_evt))

            ! validate ctr(1)[me] == i
            ALSO2(local_ctr(1) .equalsExpected. i, "scatter result")

          end do
        end block

        call prif_deallocate_coarrays(([coarray_handle_ctr, coarray_handle_evt]))
    end function

end module prif_event_test_m
