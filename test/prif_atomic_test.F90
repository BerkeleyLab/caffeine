#include "assert_macros.h"

module caf_atomic_test
    use assert_m
    use iso_c_binding, only: &
            c_ptr, c_int64_t, c_intptr_t, c_size_t, c_null_funptr, c_f_pointer, c_loc, c_sizeof
    use veggies, only: result_t, test_item_t, assert_equals, assert_that, describe, it, succeed
    use prif

    implicit none
    private
    public :: test_prif_atomic
contains
    function test_prif_atomic() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
            "PRIF atomics", &
            [ it("pass uncontended atomic test", check_atomic_uncontended) &
            , it("pass contended hot-spot atomic test", check_atomic_contended) &
            ])
    end function

    subroutine test_srand(seed)
        integer, intent(in) :: seed
        integer :: size
        call random_seed(size=size)
        block
          integer :: vals(size)
          vals = seed
          call random_seed(put=vals)
        end block
    end subroutine

    function test_rand(lo, hi) result(result_)
        integer :: lo, hi, result_
        real :: r
        call random_number(r) ! Generate a uniform random number in [0, 1)
        result_ = int(r * (hi - lo + 1)) + lo
        call_assert(result_ >= lo .and. result_ <= hi)
    end function

    function assert_equals_int(expect, actual, desc) result(result_)
        integer(PRIF_ATOMIC_INT_KIND), intent(in) :: expect, actual
        character(len=*), intent(in) :: desc
        type(result_t) :: result_

        ! TODO: would like a 64-bit integer compare, but our current
        ! veggies version does not yet support that.
        if (expect == int(expect) .and. actual == int(actual)) then
          ! safe to truncate
          result_ = assert_equals(int(expect), int(actual), desc)
        else
          result_ = assert_that(expect == actual, desc)
        endif
    end function

    function assert_equals_logical(expect, actual, desc) result(result_)
        logical(PRIF_ATOMIC_LOGICAL_KIND), intent(in) :: expect, actual
        character(len=*), intent(in) :: desc
        type(result_t) :: result_
        character(len=:), allocatable :: expect_str, actual_str
        expect_str = merge(".true. ",".false.",expect)
        actual_str = merge(".true. ",".false.",actual)

        result_ = assert_equals(expect_str, actual_str, desc)
    end function

    function check_atomic_uncontended() result(result_)
        type(result_t) :: result_

        integer, parameter :: lim = 100
        integer :: me, num_imgs, peer, i
        integer(PRIF_ATOMIC_INT_KIND) :: dummy_atomic_int
        logical(PRIF_ATOMIC_LOGICAL_KIND) :: dummy_atomic_logical
        integer(c_size_t) :: sizeof_atomic_int, sizeof_atomic_logical
        type(prif_coarray_handle) :: coarray_handle_int, coarray_handle_logical
        type(c_ptr) :: c_ptr_int, c_ptr_logical

        integer(c_intptr_t) :: base_addr_int, base_addr_logical

        result_ = succeed("")

        sizeof_atomic_int = int(storage_size(dummy_atomic_int)/8, c_size_t)
        sizeof_atomic_logical = int(storage_size(dummy_atomic_logical)/8, c_size_t)
        ! Check an invariant of the current Caffeine impl, not required by PRIF:
        call_assert(sizeof_atomic_int == 8)
        call_assert(sizeof_atomic_logical == 8)

        call prif_num_images(num_images=num_imgs)
        call prif_this_image_no_coarray(this_image=me)
        call test_srand(me)

        ! integer(PRIF_ATOMIC_INT_KIND) :: atomic_int[*]
        call prif_allocate_coarray( &
                lcobounds = [1_c_int64_t], &
                ucobounds = [int(num_imgs,c_int64_t)], &
                size_in_bytes = sizeof_atomic_int, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle_int, &
                allocated_memory = c_ptr_int)
        base_addr_int = transfer(c_ptr_int, base_addr_int)

        ! logical(PRIF_ATOMIC_LOGICAL_KIND) :: atomic_logical[*]
        call prif_allocate_coarray( &
                lcobounds = [1_c_int64_t], &
                ucobounds = [int(num_imgs,c_int64_t)], &
                size_in_bytes = sizeof_atomic_logical, &
                final_func = c_null_funptr, &
                coarray_handle = coarray_handle_logical, &
                allocated_memory = c_ptr_logical)
        base_addr_logical = transfer(c_ptr_logical, base_addr_logical)

        block 
          integer(PRIF_ATOMIC_INT_KIND) :: value_int, expect_int
          logical(PRIF_ATOMIC_LOGICAL_KIND) :: value_logical, expect_logical

          ! local init and access operations

          do i=1, lim

            call prif_sync_all() ! only here for subtest isolation

            ! integer tests

            expect_int = me
            call prif_atomic_define_int(me, coarray_handle_int, 0_c_size_t, value=expect_int)
            call prif_atomic_ref_int(me, coarray_handle_int, 0_c_size_t, value=value_int)
            result_ = result_ .and. &
              assert_equals_int(expect_int, value_int, "local define direct / ref direct")
            call prif_atomic_ref_int_indirect(me, base_addr_int, value=value_int)
            result_ = result_ .and. &
              assert_equals_int(expect_int, value_int, "local define direct / ref indirect")

            expect_int = me * 100 
            call prif_atomic_define_int_indirect(me, base_addr_int, value=expect_int)
            call prif_atomic_ref_int_indirect(me, base_addr_int, value=value_int)
            result_ = result_ .and. &
              assert_equals_int(expect_int, value_int, "local define indirect / ref indirect")

            call prif_atomic_cas_int(me, coarray_handle_int, 0_c_size_t, &
                                     old=value_int, compare=expect_int, new=(expect_int*10))
            result_ = result_ .and. &
              assert_equals_int(expect_int, value_int, "local cas direct")
            expect_int = expect_int * 10

            call prif_atomic_cas_int_indirect(me, base_addr_int, &
                                              old=value_int, compare=expect_int, new=(expect_int*10))
            result_ = result_ .and. &
              assert_equals_int(expect_int, value_int, "local cas indirect")
            expect_int = expect_int * 10

            call prif_atomic_ref_int(me, coarray_handle_int, 0_c_size_t, value=value_int)
            result_ = result_ .and. &
              assert_equals_int(expect_int, value_int, "local cas / ref direct")

            expect_int = 0
            call prif_atomic_define_int(me, coarray_handle_int, 0_c_size_t, value=expect_int)
            call prif_atomic_ref_int(me, coarray_handle_int, 0_c_size_t, value=value_int)
            result_ = result_ .and. &
              assert_equals_int(expect_int, value_int, "local define direct / ref direct (final)")

            call prif_sync_all() ! only here for subtest isolation

            ! logical tests

            expect_logical = (IOR(me,1) == 1)
            call prif_atomic_define_logical(me, coarray_handle_logical, 0_c_size_t, value=expect_logical)
            call prif_atomic_ref_logical(me, coarray_handle_logical, 0_c_size_t, value=value_logical)
            result_ = result_ .and. &
              assert_equals_logical(expect_logical, value_logical, "local define direct / ref direct")
            call prif_atomic_ref_logical_indirect(me, base_addr_logical, value=value_logical)
            result_ = result_ .and. &
              assert_equals_logical(expect_logical, value_logical, "local define direct / ref indirect")

            expect_logical = .not. expect_logical
            call prif_atomic_define_logical_indirect(me, base_addr_logical, value=expect_logical)
            call prif_atomic_ref_logical_indirect(me, base_addr_logical, value=value_logical)
            result_ = result_ .and. &
              assert_equals_logical(expect_logical, value_logical, "local define indirect / ref indirect")

            call prif_atomic_cas_logical(me, coarray_handle_logical, 0_c_size_t, &
                                         old=value_logical, compare=expect_logical, new=(.not. expect_logical))
            result_ = result_ .and. &
              assert_equals_logical(expect_logical, value_logical, "local cas direct")
            expect_logical = .not. expect_logical

            call prif_atomic_cas_logical_indirect(me, base_addr_logical, &
                                                  old=value_logical, compare=expect_logical, new=(.not. expect_logical))
            result_ = result_ .and. &
              assert_equals_logical(expect_logical, value_logical, "local cas indirect")
            expect_logical = .not. expect_logical

            call prif_atomic_ref_logical(me, coarray_handle_logical, 0_c_size_t, value=value_logical)
            result_ = result_ .and. &
              assert_equals_logical(expect_logical, value_logical, "local cas / ref direct")

            expect_logical = .false.
            call prif_atomic_define_logical(me, coarray_handle_logical, 0_c_size_t, value=expect_logical)
            call prif_atomic_ref_logical(me, coarray_handle_logical, 0_c_size_t, value=value_logical)
            result_ = result_ .and. &
              assert_equals_logical(expect_logical, value_logical, "local define direct / ref direct (final)")

          end do

          call prif_sync_all()

          ! uncontended test targeting peer's location

          call_assert(expect_int == 0)
          call_assert(logical(expect_logical .eqv. .false.))
          
          peer = mod(me,num_imgs)+1

          ! logical test

          do i=1, lim
          block
            character(:), allocatable :: test_desc
            logical(PRIF_ATOMIC_LOGICAL_KIND) :: tmp

            tmp = (IAND(i,1) == 1)
            select case (test_rand(1,3))
              case (1) ; test_desc = "define"
                expect_logical = tmp
                call prif_atomic_define_logical(peer, coarray_handle_logical, 0_c_size_t, value=expect_logical)

              case (2) ; test_desc = "cas succeed"
                call prif_atomic_cas_logical(peer, coarray_handle_logical, 0_c_size_t, &
                                         old=value_logical, compare=expect_logical, new=tmp)
                result_ = result_ .and. &
                  assert_equals_logical(expect_logical, value_logical, "int cas direct succeed")
                expect_logical = tmp

              case (3) ; test_desc = "cas fail"
                call prif_atomic_cas_logical(peer, coarray_handle_logical, 0_c_size_t, &
                                         old=value_logical, compare=(.not. expect_logical), new=tmp)
                result_ = result_ .and. &
                  assert_equals_logical(expect_logical, value_logical, "int cas direct fail")

            end select

            call prif_atomic_ref_logical(peer, coarray_handle_logical, 0_c_size_t, value=value_logical)
            result_ = result_ .and. &
              assert_equals_logical(expect_logical, value_logical, "result check for peer int "//test_desc)
          end block
          end do

          call prif_sync_all() ! only here for subtest isolation

          ! integer test

          do i=1, lim
          block
            character(:), allocatable :: test_desc
            integer(PRIF_ATOMIC_INT_KIND) :: tmp

            tmp = i
            select case (test_rand(1,11))
              case (1) ; test_desc = "define"
                expect_int = i
                call prif_atomic_define_int(peer, coarray_handle_int, 0_c_size_t, value=expect_int)

              case (2) ; test_desc = "cas succeed"
                call prif_atomic_cas_int(peer, coarray_handle_int, 0_c_size_t, &
                                         old=value_int, compare=expect_int, new=tmp)
                result_ = result_ .and. &
                  assert_equals_int(expect_int, value_int, "int cas direct succeed")
                expect_int = tmp

              case (3) ; test_desc = "cas fail"
                call prif_atomic_cas_int(peer, coarray_handle_int, 0_c_size_t, &
                                         old=value_int, compare=expect_int+1, new=tmp)
                result_ = result_ .and. &
                  assert_equals_int(expect_int, value_int, "int cas direct fail")

              case (4) ; test_desc = "add"
                call prif_atomic_add(peer, coarray_handle_int, 0_c_size_t, value=tmp)
                expect_int = expect_int + tmp

              case (5) ; test_desc = "fetch_add"
                call prif_atomic_fetch_add(peer, coarray_handle_int, 0_c_size_t, value=tmp, old=value_int)
                result_ = result_ .and. &
                  assert_equals_int(expect_int, value_int, "fetch_add fetch check")
                expect_int = expect_int + tmp

              case (6) ; test_desc = "and"
                call prif_atomic_and(peer, coarray_handle_int, 0_c_size_t, value=tmp)
                expect_int = IAND(expect_int, tmp)

              case (7) ; test_desc = "fetch_and"
                call prif_atomic_fetch_and(peer, coarray_handle_int, 0_c_size_t, value=tmp, old=value_int)
                result_ = result_ .and. &
                  assert_equals_int(expect_int, value_int, "fetch_and fetch check")
                expect_int = IAND(expect_int, tmp)

              case (8) ; test_desc = "or"
                call prif_atomic_or(peer, coarray_handle_int, 0_c_size_t, value=tmp)
                expect_int = IOR(expect_int, tmp)

              case (9) ; test_desc = "fetch_or"
                call prif_atomic_fetch_or(peer, coarray_handle_int, 0_c_size_t, value=tmp, old=value_int)
                result_ = result_ .and. &
                  assert_equals_int(expect_int, value_int, "fetch_or fetch check")
                expect_int = IOR(expect_int, tmp)

              case (10) ; test_desc = "xor"
                call prif_atomic_xor(peer, coarray_handle_int, 0_c_size_t, value=tmp)
                expect_int = IEOR(expect_int, tmp)

              case (11) ; test_desc = "fetch_xor"
                call prif_atomic_fetch_xor(peer, coarray_handle_int, 0_c_size_t, value=tmp, old=value_int)
                result_ = result_ .and. &
                  assert_equals_int(expect_int, value_int, "fetch_xor fetch check")
                expect_int = IEOR(expect_int, tmp)


            end select

            call prif_atomic_ref_int(peer, coarray_handle_int, 0_c_size_t, value=value_int)
            result_ = result_ .and. &
              assert_equals_int(expect_int, value_int, "result check for peer int "//test_desc)
          end block
          end do

        end block

        call prif_deallocate_coarray([coarray_handle_int])
        call prif_deallocate_coarray([coarray_handle_logical])
    end function

    function check_atomic_contended() result(result_)
        type(result_t) :: result_

        integer, parameter :: lim = 100
        integer :: me, num_imgs, root, i
        integer(PRIF_ATOMIC_INT_KIND) :: dummy_atomic_int
        logical(PRIF_ATOMIC_LOGICAL_KIND) :: dummy_atomic_logical
        integer(c_size_t) :: sizeof_atomic_int, sizeof_atomic_logical
        type(c_ptr) :: c_ptr_int, c_ptr_logical
        integer(c_intptr_t) :: base_addr_int, base_addr_logical
        integer(PRIF_ATOMIC_INT_KIND), parameter :: zero = 0, plus_one = 1, minus_one = -1
        integer(PRIF_ATOMIC_INT_KIND) :: value_int, expect_int, tmp_int, my_bit
        logical(PRIF_ATOMIC_LOGICAL_KIND) :: false = .false.
        logical(PRIF_ATOMIC_LOGICAL_KIND) :: value_logical, expect_logical, tmp_logical
        character(len=:),allocatable :: desc

        result_ = succeed("")

        sizeof_atomic_int = int(storage_size(dummy_atomic_int)/8, c_size_t)
        sizeof_atomic_logical = int(storage_size(dummy_atomic_logical)/8, c_size_t)

        call prif_num_images(num_images=num_imgs)
        call prif_this_image_no_coarray(this_image=me)
        call test_srand(me)
        root = 1

        ! allocate centralized atomic variables on the root image
        if (me == root) then
          call prif_allocate(sizeof_atomic_int, c_ptr_int)
          base_addr_int = transfer(c_ptr_int, base_addr_int)
          call prif_atomic_define_int_indirect(me, base_addr_int, value=zero)

          call prif_allocate(sizeof_atomic_logical, c_ptr_logical)
          base_addr_logical = transfer(c_ptr_logical, base_addr_logical)
          call prif_atomic_define_logical_indirect(me, base_addr_logical, value=false)
        endif
        call prif_co_broadcast(base_addr_int, root)
        call prif_co_broadcast(base_addr_logical, root)

        desc = "integer add-up test "
        expect_int = zero
        do i=1, lim

          call prif_sync_all()

          call prif_atomic_add_indirect(root, base_addr_int, value=plus_one)
          call prif_atomic_add_indirect(root, base_addr_int, value=minus_one)
          call prif_atomic_fetch_add_indirect(root, base_addr_int, value=plus_one, old=value_int)
          result_ = result_ .and. &
            assert_that(value_int >= expect_int, desc//"mid-increment lower bound")
          result_ = result_ .and. &
            assert_that(value_int < expect_int + num_imgs, desc//"mid-increment upper bound")

          call prif_sync_all()

          expect_int = expect_int + num_imgs
          call prif_atomic_ref_int_indirect(root, base_addr_int, value=value_int)
          result_ = result_ .and. &
            assert_equals_int(expect_int, value_int, desc//"loop-bottom check")
        end do


        desc = "integer cas-up test "
        do i=1, MAX(2,lim/num_imgs**2) ! running time grows superlinearly with scale due to contention

          call prif_sync_all()

          tmp_int = expect_int
          do
            call prif_atomic_cas_int_indirect(root, base_addr_int, &
                                              old=value_int, compare=tmp_int, new=(tmp_int+1))
            result_ = result_ .and. &
              assert_that(value_int >= expect_int, desc//"mid-increment lower bound")
            result_ = result_ .and. &
              assert_that(value_int < expect_int + num_imgs, desc//"mid-increment upper bound")
            if (value_int == tmp_int) exit ! success 
            tmp_int = value_int ! collision => retry
          end do

          call prif_sync_all()

          expect_int = expect_int + num_imgs
          call prif_atomic_ref_int_indirect(root, base_addr_int, value=value_int)
          result_ = result_ .and. &
            assert_equals_int(expect_int, value_int, desc//"loop-bottom check")
        end do


        desc = "logical cas-toggle test "
        expect_logical = false
        do i=1, MAX(2,lim/num_imgs**2) ! running time grows superlinearly with scale due to contention

          call prif_sync_all()

          tmp_logical = expect_logical
          do
            call prif_atomic_cas_logical_indirect(root, base_addr_logical, &
                                              old=value_logical, compare=tmp_logical, new=(.not. tmp_logical))
            if (value_logical .eqv. tmp_logical) exit ! success 
            result_ = result_ .and. &
              assert_equals_logical(value_logical, .not. tmp_logical, desc//"mid-swap sanity check")
            tmp_logical = value_logical ! collision => retry
          end do

          call prif_sync_all()

          expect_logical = merge(expect_logical, .not. expect_logical, mod(num_imgs,2) == 0)
          call prif_atomic_ref_logical_indirect(root, base_addr_logical, value=value_logical)
          result_ = result_ .and. &
            assert_equals_logical(expect_logical, value_logical, desc//"loop-bottom check")
        end do

        call prif_sync_all()
        if (me == root) then
          call prif_atomic_define_int_indirect(me, base_addr_int, value=zero)
        endif
        call prif_sync_all()

        desc = "randomized integer bitwise test "
        if (me <= sizeof_atomic_int*8) then
          my_bit = SHIFTL(plus_one, me-1) 
        else
          my_bit = 0
        endif
        expect_int = 0
        do i=1, lim
        block
          character(:), allocatable :: test_desc

          select case (test_rand(1,6))
            case (1) ; test_desc = "and"
              call prif_atomic_and_indirect(root, base_addr_int, value=NOT(my_bit))
              expect_int = IAND(expect_int, NOT(my_bit))

            case (2) ; test_desc = "fetch_and"
              call prif_atomic_fetch_and_indirect(root, base_addr_int, value=NOT(my_bit), old=value_int)
              result_ = result_ .and. &
                assert_equals_int(expect_int, IAND(value_int,my_bit), desc//"fetch_and fetch check")
              expect_int = IAND(expect_int, NOT(my_bit))

            case (3) ; test_desc = "or"
              call prif_atomic_or_indirect(root, base_addr_int, value=my_bit)
              expect_int = IOR(expect_int, my_bit)

            case (4) ; test_desc = "fetch_or"
              call prif_atomic_fetch_or_indirect(root, base_addr_int, value=my_bit, old=value_int)
              result_ = result_ .and. &
                assert_equals_int(expect_int, IAND(value_int,my_bit), desc//"fetch_or fetch check")
              expect_int = IOR(expect_int, my_bit)

            case (5) ; test_desc = "xor"
              call prif_atomic_xor_indirect(root, base_addr_int, value=my_bit)
              expect_int = IEOR(expect_int, my_bit)

            case (6) ; test_desc = "fetch_xor"
              call prif_atomic_fetch_xor_indirect(root, base_addr_int, value=my_bit, old=value_int)
              result_ = result_ .and. &
                assert_equals_int(expect_int, IAND(value_int,my_bit), desc//"fetch_xor fetch check")
              expect_int = IEOR(expect_int, my_bit)


          end select

          call prif_atomic_ref_int_indirect(root, base_addr_int, value=value_int)
          result_ = result_ .and. &
            assert_equals_int(expect_int, IAND(value_int,my_bit), desc//"result check for int "//test_desc)

        end block
        end do

        call prif_sync_all()
        if (me == root) then
          call prif_deallocate(c_ptr_int)
          call prif_deallocate(c_ptr_logical)
        endif
    end function

end module
