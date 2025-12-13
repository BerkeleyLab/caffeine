#include "test-utils.F90"

module prif_allocate_test_m
  use prif, only : &
      prif_allocate_coarray, &
      prif_allocate, prif_deallocate, &
      prif_coarray_handle, prif_num_images, prif_size_bytes, &
      prif_set_context_data, prif_get_context_data, prif_local_data_pointer, &
      prif_alias_create, prif_alias_destroy
#if FORCE_PRIF_0_5 || FORCE_PRIF_0_6
  use prif, only : prif_deallocate_coarray_ => prif_deallocate_coarray
# define prif_deallocate_coarray(h)    prif_deallocate_coarray_([h])
# define prif_deallocate_coarrays(arr) prif_deallocate_coarray_(arr)
#else
  use prif, only : prif_deallocate_coarray, prif_deallocate_coarrays
#endif
  use julienne_m, only: test_description_t, test_diagnosis_t, test_result_t, test_t, string_t, usher &
    ,operator(.all.), operator(.also.), operator(.equalsExpected.), operator(//)
  use iso_c_binding, only: &
      c_ptr, c_int, c_int64_t, c_size_t, c_null_funptr, &
      c_f_pointer, c_null_ptr, c_loc, c_associated, c_intptr_t

  implicit none
  private
  public :: prif_allocate_test_t

  type, extends(test_t) :: prif_allocate_test_t
  contains
    procedure, nopass, non_overridable :: subject
    procedure, nopass, non_overridable :: results
  end type

contains

  pure function subject()
    character(len=:), allocatable :: subject
    subject = "PRIF Allocation"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(prif_allocate_test_t) prif_allocate_test

    allocate(test_results, source = prif_allocate_test%run([ &
       test_description_t("allocating, using and deallocating an integer scalar coarray with a corank of 1", &
         usher(check_allocate_integer_scalar_coarray_with_corank1)) &
      ,test_description_t("allocating, using and deallocating an integer array coarray with a corank of 2", &
         usher(check_allocate_integer_array_coarray_with_corank2)) &
      ,test_description_t("allocating, using and deallocating memory non-symmetrically", &
         usher(check_allocate_non_symmetric)) &
    ]))
  end function

  function check_allocate_integer_scalar_coarray_with_corank1() result(diag)
    type(test_diagnosis_t) diag

    ! Allocate memory for an integer scalar single corank coarray, such as the following decl
    ! integer :: coarr[*]

    integer(kind=c_int64_t), dimension(1) :: lcobounds, ucobounds
    integer :: dummy_element, num_imgs
    type(prif_coarray_handle) :: coarray_handle
    type(c_ptr) :: allocated_memory
    integer, pointer :: local_slice
    integer(c_size_t) :: data_size, query_size

    diag = .true.

    call prif_num_images(num_images=num_imgs)
    lcobounds(1) = 1
    ucobounds(1) = num_imgs

    allocated_memory = c_null_ptr
    local_slice => null()
    ALSO(.not. associated(local_slice))

    data_size = storage_size(dummy_element)/8
    call prif_allocate_coarray( &
      lcobounds, ucobounds, data_size, c_null_funptr, &
      coarray_handle, allocated_memory)

    call c_f_pointer(allocated_memory, local_slice)
    ALSO(associated(local_slice))

    local_slice = 42
    ALSO(local_slice .equalsExpected. 42)

    call prif_size_bytes(coarray_handle, data_size=query_size)
    ALSO2(query_size .equalsExpected. data_size, "invalid prif_size_bytes")

    block ! Check prif_{set,get}_context_data
      integer, target :: dummy(10), i
      type(c_ptr) :: expect, actual
      do i = 1,10 
        expect = c_loc(dummy(i))
        actual = c_null_ptr
        call prif_set_context_data(coarray_handle, expect)
        call prif_get_context_data(coarray_handle, actual)
        ALSO2(actual .equalsExpected. expect, "prif_{set,get}_context_data are not working")
      end do
    end block

    call prif_deallocate_coarray(coarray_handle)

  end function

  function check_allocate_non_symmetric() result(diag)
    type(test_diagnosis_t) diag 

    type(c_ptr) :: allocated_memory
    integer(c_int), pointer :: local_slice

    call prif_allocate(sizeof(local_slice), allocated_memory)
    call c_f_pointer(allocated_memory, local_slice)

    local_slice = 42
    diag = local_slice .equalsExpected. 42

    call prif_deallocate(c_loc(local_slice))
  end function

  ! returns (p + off)
  pure function c_ptr_add(p, off)
    type(c_ptr), intent(in) :: p
    integer(c_size_t), intent(in) :: off
    type(c_ptr) :: c_ptr_add
    integer(c_intptr_t) :: tmp
      
    tmp = transfer(p, tmp)
    tmp = tmp + off
    c_ptr_add = transfer(tmp, c_ptr_add)
  end function

  function assert_aliased(h1, h2, offset) result(diag)
    type(test_diagnosis_t) :: diag
    type(prif_coarray_handle) :: h1, h2
    integer(c_size_t), optional :: offset
    integer(c_size_t) :: offset_
    type(c_ptr) :: p1, p2
    integer(c_size_t) :: s1, s2
    type(c_ptr) :: c1, c2, cx
    integer, save, target :: dummy(10)
    integer, save :: di = 1

    diag = .true.

    if (present(offset)) then
      offset_ = offset
    else
      offset_ = 0
    endif

    call prif_local_data_pointer(h1, p1)
    call prif_local_data_pointer(h2, p2)
    ALSO(p2 .equalsExpected. c_ptr_add(p1, offset_))

    ! As of PRIF 0.6. prif_size_bytes is unspecified for aliases, 
    ! so this particular check is specific to the current Caffeine implementation
    call prif_size_bytes(h1, s1)
    call prif_size_bytes(h2, s2)
    ALSO(s2 .equalsExpected. s1)
      
    cx = c_loc(dummy(di))
    di = mod(di,size(dummy)) + 1

    call prif_set_context_data(h1, cx)
    call prif_get_context_data(h1, c1)
    ALSO(c1 .equalsExpected. cx)

    call prif_get_context_data(h2, c2)
    ALSO(c2 .equalsExpected. cx)
      
    call prif_set_context_data(h2, c_null_ptr)
    call prif_get_context_data(h1, c1)
    ALSO(.not. c_associated(c1))

  end function

  function check_allocate_integer_array_coarray_with_corank2() result(diag)
    type(test_diagnosis_t) :: diag

    ! Allocate memory for an integer scalar single corank coarray, such as the following decl
    ! integer :: coarr(10)[4,*]

    integer(kind=c_int64_t), dimension(2) :: lcobounds, ucobounds
    integer :: dummy_element, num_imgs, i
    type(prif_coarray_handle) :: coarray_handle
    type(c_ptr) :: allocated_memory
    integer, pointer :: local_slice(:)
    integer(c_size_t) :: data_size, query_size

    diag = .true.

    call prif_num_images(num_images=num_imgs)
    lcobounds(1) = 1
    ucobounds(1) = 4
    lcobounds(2) = 1
    ucobounds(2) = num_imgs

    allocated_memory = c_null_ptr
    local_slice => null()
    ALSO(.not.associated(local_slice))

    data_size = 10*storage_size(dummy_element)/8
    call prif_allocate_coarray( &
      lcobounds, ucobounds, data_size, c_null_funptr, &
      coarray_handle, allocated_memory)

    call prif_size_bytes(coarray_handle, data_size=query_size)
    ALSO2(query_size .equalsExpected. data_size, "invalid prif_size_bytes")

    call c_f_pointer(allocated_memory, local_slice, [10])
    ALSO(associated(local_slice))

    local_slice = [(i*i, i = 1, 10)]
    ALSO(.all. (local_slice .equalsExpected. [(i*i, i = 1, 10)]))


    block ! Check prif_{set,get}_context_data
      integer, target :: dummy(10), i
      type(c_ptr) :: expect, actual
      do i = 1,10 
        expect = c_loc(dummy(i))
        actual = c_null_ptr
        call prif_set_context_data(coarray_handle, expect)
        call prif_get_context_data(coarray_handle, actual)
        ALSO2(actual .equalsExpected. expect, "prif_{set,get}_context_data not working")
      end do
    end block

    block ! check aliasing creation
#   if FORCE_PRIF_0_5
#     define data_pointer_offset
#   else
#     define data_pointer_offset 0_c_size_t,
#   endif
      integer i, j
      integer, parameter :: lim = 10
      type(prif_coarray_handle) :: a(lim)
      integer(c_int64_t) :: lco(1), uco(1)
      a(1) = coarray_handle
      do i=2, lim
        lco(1) = i
        uco(1) = i + num_imgs
        call prif_alias_create(a(i-1), lco, uco, data_pointer_offset a(i))
        ALSO(assert_aliased(a(i-1), a(i)))
        do j = i+1,lim
          lco(1) = j
          uco(1) = j + num_imgs
          call prif_alias_create(a(i), lco, uco, data_pointer_offset a(j))
          ALSO(assert_aliased(a(i), a(j)))
          ALSO(assert_aliased(a(j), coarray_handle))
        end do
#       if !FORCE_PRIF_0_5
          ! test PRIF 0.6 data_pointer_offset
          block
            type(prif_coarray_handle) :: b
            integer(c_size_t) :: off
            off = i
            call prif_alias_create(a(i), lco, uco, off, b)
            ALSO(assert_aliased(a(i), b, off))
            call prif_alias_destroy(b)
          end block
#       endif
        do j = i+1,lim
          call prif_alias_destroy(a(j))
        end do
      end do
      do i=2, lim
        call prif_alias_destroy(a(i))
      end do
    end block

    call prif_deallocate_coarray(coarray_handle)

  end function
end module prif_allocate_test_m
