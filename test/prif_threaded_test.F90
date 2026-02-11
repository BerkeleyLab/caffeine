#include "test-utils.F90"

module prif_threaded_test_m
# include "test-uses-alloc.F90"
    use iso_c_binding, only: c_funptr, c_funloc, c_f_procpointer
    use prif, only: &
            prif_notify_type, prif_notify_wait, prif_put_with_notify, &
            prif_this_image_no_coarray, prif_num_images, &
            prif_sync_all
    use julienne_m, only: test_description_t, test_diagnosis_t, test_result_t, test_t, usher 

    implicit none
    private
    public :: prif_threaded_test_t

    type, extends(test_t) :: prif_threaded_test_t
    contains
      procedure, nopass, non_overridable :: subject
      procedure, nopass, non_overridable :: results
    end type

    abstract interface
      pure subroutine pure_allocate_i(size_in_bytes, allocated_memory, stat, errmsg, errmsg_alloc)
        import c_size_t, c_int, c_ptr
        integer(c_size_t), intent(in) :: size_in_bytes
        type(c_ptr), intent(out) :: allocated_memory
        integer(c_int), intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        character(len=:), intent(inout), allocatable , optional :: errmsg_alloc
      end subroutine
    end interface

    abstract interface
      pure subroutine pure_deallocate_i(mem, stat, errmsg, errmsg_alloc)
        import c_int, c_ptr
        type(c_ptr), intent(in) :: mem
        integer(c_int), intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        character(len=:), intent(inout), allocatable , optional :: errmsg_alloc
      end subroutine
    end interface

    abstract interface
      pure subroutine pure_notify_wait_i(notify_var_ptr, until_count, stat, errmsg, errmsg_alloc)
        import c_int64_t, c_int, c_ptr
        type(c_ptr), intent(in) :: notify_var_ptr
        integer(c_int64_t), intent(in), optional :: until_count
        integer(c_int), intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        character(len=:), intent(inout), allocatable , optional :: errmsg_alloc
      end subroutine
    end interface

contains
   pure function subject()
     character(len=:), allocatable :: subject
     subject = "PRIF Thread-safe operations"
   end function

    function results() result(test_results)
        type(test_result_t), allocatable :: test_results(:)
        type(prif_threaded_test_t) prif_threaded_test

        allocate(test_results, source = prif_threaded_test%run([ &
           test_description_t("prif_allocate in do concurrent" &
#           if CAF_THREAD_SAFE
              ,usher(check_allocate) &
#           endif
           ) &
          ,test_description_t("prif_notify_wait in do concurrent" &
#           if CAF_THREAD_SAFE
              ,usher(check_notify) &
#           endif
           ) &
        ]))
    end function

    function check_allocate() result(diag)
        type(test_diagnosis_t) diag
        integer, parameter :: lim = 10000
        integer(c_size_t), parameter :: sz = 64
        integer :: i
        type(c_funptr) :: fp
        procedure(pure_allocate_i), pointer :: pure_allocate
        procedure(pure_deallocate_i), pointer :: pure_deallocate

        ! workaround the fact that PRIF routines are not pure,
        ! but a compiler client might still invoke them from within do concurrent
        fp = c_funloc(prif_allocate)
        call c_f_procpointer(fp, pure_allocate)
        fp = c_funloc(prif_deallocate)
        call c_f_procpointer(fp, pure_deallocate)

        do concurrent(i = 1:lim)
          block
            type(c_ptr) :: p
            call pure_allocate(sz, p)
            call pure_deallocate(p)
          end block
        end do

        diag = .true.
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
        integer, pointer :: local_ctr
        type(c_funptr) :: fp
        procedure(pure_notify_wait_i), pointer :: pure_notify_wait

        fp = c_funloc(prif_notify_wait)
        call c_f_procpointer(fp, pure_notify_wait)

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

        ! integer :: ctr[*]
        call prif_allocate_coarray( &
                lcobounds = [1_c_int64_t], &
                ucobounds = [int(num_imgs,c_int64_t)], &
                size_in_bytes = sizeof_int, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle_ctr, &
                allocated_memory = allocated_memory)
        call c_f_pointer(allocated_memory, local_ctr)
        local_ctr = 0 ! initialize

        call prif_sync_all

        block
          integer, parameter :: lim = 10000
          integer, target :: i

          do i=1, lim
            ! every image writes to itself with notify
            ! ctr[me, notify=evt] = i
            call prif_put_with_notify( &
                image_num = me, &
                coarray_handle = coarray_handle_ctr, &
                offset = 0_c_size_t, &
                current_image_buffer = c_loc(i), &
                size_in_bytes = sizeof_int, &
                notify_coarray_handle = coarray_handle_evt, &
                notify_offset = 0_c_size_t)
          end do

          do concurrent(i = 1:lim)
            ! NOTIFY WAIT( evt, UNTIL_COUNT=1 )
            call pure_notify_wait(c_loc(local_evt), until_count=1_c_int64_t)
          end do

        end block

        call prif_deallocate_coarrays(([coarray_handle_ctr, coarray_handle_evt]))
    end function

end module
