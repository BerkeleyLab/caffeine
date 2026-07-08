#include "julienne-assert-macros.h"
#include "test-utils.F90"

module prif_coarray_inquiry_test_m
# include "test-uses-alloc.F90"
  use prif, only : &
      prif_coarray_handle, prif_this_image_no_coarray, prif_num_images, &
      prif_local_data_pointer, prif_size_bytes, &
      prif_lcobound_no_dim, prif_lcobound_with_dim, &
      prif_ucobound_no_dim, prif_ucobound_with_dim, &
      prif_coshape

  use julienne_m, only: &
     operator(//) &
    ,operator(.all.) &
    ,operator(.also.) &
    ,operator(.equalsExpected.) &
    ,call_julienne_assert_ &
    ,usher &
    ,string_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t

  implicit none
  private
  public :: prif_coarray_inquiry_test_t

  type, extends(test_t) :: prif_coarray_inquiry_test_t
  contains
    procedure, nopass, non_overridable :: subject
    procedure, nopass, non_overridable :: results
  end type

contains

  pure function subject() result(test_subject)
     character(len=:), allocatable :: test_subject
     test_subject = "PRIF Coarray Inquiries"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(prif_coarray_inquiry_test_t) prif_coarray_inquiry_test

    allocate(test_results, source = prif_coarray_inquiry_test%run([ &
       test_description_t("preserving the prif_local_data_pointer for an allocated coarray", usher(check_prif_local_data_pointer)) &
      ,test_description_t("checking passed cobounds", usher(check_cobounds)) &
    ]))
  end function

  function check_prif_local_data_pointer() result(diag)
      type(test_diagnosis_t) :: diag

      integer :: dummy_element
      type(prif_coarray_handle) :: coarray_handle
      type(c_ptr) :: allocation_ptr, local_ptr


      call prif_allocate_coarray( &
              [integer(c_int64_t):: 1], &
              [integer(c_int64_t)::], &
              int(storage_size(dummy_element)/8, c_size_t), &
              null_final_proc, &
              coarray_handle, &
              allocation_ptr)
      call prif_local_data_pointer(coarray_handle, local_ptr)
      diag = c_associated(local_ptr, allocation_ptr)
      call prif_deallocate_coarray(coarray_handle)
  end function

  function check_cobound(lcobounds, ucobounds, omit_trailing) result(diag)
    !! Allocate a coarray with given cobounds and test some queries on it
    integer(kind=c_int64_t), intent(in) :: lcobounds(:), ucobounds(:)
    logical, intent(in) :: omit_trailing

    integer(kind=c_int64_t) :: tmp_bounds(size(lcobounds)), actual_ucobounds(size(lcobounds)), leading_ucobounds(size(lcobounds)-1)
    integer(kind=c_int64_t) :: tmp_bound
    integer(kind=c_size_t)  :: sizes(size(lcobounds))
    type(prif_coarray_handle) :: coarray_handle
    type(c_ptr) :: allocated_memory
    integer(c_size_t) :: data_size, query_size
    integer :: i, corank, num_imgs
    type(test_diagnosis_t) :: diag

    diag = .true.

    call_julienne_assert(size(lcobounds) == size(ucobounds))
    corank = size(lcobounds)
    call prif_num_images(num_images=num_imgs)

    ! compute trailing ucobound
    actual_ucobounds = ucobounds
    tmp_bound = product(ucobounds(1:corank-1) - lcobounds(1:corank-1) + 1) 
    actual_ucobounds(corank) = lcobounds(corank) + (num_imgs + tmp_bound - 1) / tmp_bound - 1

    allocated_memory = c_null_ptr
    data_size = 64 * corank
    if (omit_trailing) then
      leading_ucobounds = ucobounds(1:corank-1)
      call prif_allocate_coarray( lcobounds, leading_ucobounds, data_size, null_final_proc, &
        coarray_handle, allocated_memory)
    else
      call prif_allocate_coarray( lcobounds, actual_ucobounds, data_size, null_final_proc, &
        coarray_handle, allocated_memory)
    end if

    ALSO(c_associated(allocated_memory))

    call prif_size_bytes(coarray_handle, data_size=query_size)
    ALSO2(query_size .equalsExpected. data_size, "prif_size_bytes is valid")

    call prif_lcobound_no_dim(coarray_handle, tmp_bounds)
    ALSO2(.all. (tmp_bounds .equalsExpected. lcobounds), "prif_lcobound_no_dim is valid")

    call prif_ucobound_no_dim(coarray_handle, tmp_bounds)
    ALSO2(.all. (tmp_bounds .equalsExpected. actual_ucobounds), "prif_ucobound_no_dim is valid")

    do i = 1, corank
      call prif_lcobound_with_dim(coarray_handle, i, tmp_bound)
      ALSO2(tmp_bound .equalsExpected. lcobounds(i), "prif_lcobound_with_dim is valid")

      call prif_ucobound_with_dim(coarray_handle, i, tmp_bound)
      ALSO2(tmp_bound .equalsExpected. actual_ucobounds(i), "prif_ucobound_with_dim is valid")
    end do

    call prif_coshape(coarray_handle, sizes)
    ALSO2(.all. ((actual_ucobounds - lcobounds + 1) .equalsExpected. sizes), "prif_coshape is valid")

#   if VERBOSE
    block
      integer :: me
      call prif_this_image_no_coarray(this_image=me)
      if (me == 1) then
        write(*,'(A,*(I4))') "lcobounds=" , lcobounds
        write(*,'(A,*(I4))') "ucobounds=" , actual_ucobounds
        write(*,'(A,*(I4))') "sizes=    " , sizes
      end if
    end block
#   endif

    call prif_deallocate_coarray(coarray_handle)
  end function

  impure elemental function check_corank(corank, omit_trailing) result(diag)
    !! Allocate a coarray with given corank and test some queries on it
    type(test_diagnosis_t) :: diag
    integer(c_int), intent(in) :: corank
    logical, intent(in) :: omit_trailing

    integer :: i
    integer(kind=c_int64_t), dimension(corank) :: lcobounds, ucobounds

    lcobounds(1) = 10
    ucobounds(1) = 11
    do i = 2,corank
      lcobounds(i) = 10*i
      ucobounds(i) = 10*i + merge(1,0,mod(i,2)==0)
    end do

    diag = check_cobound(lcobounds, ucobounds, omit_trailing)

  end function

  function check_cobounds() result(diag)
    type(test_diagnosis_t) :: diag
    integer(c_int) :: corank

    diag = .true.

    ! check some simple cases
    ALSO(check_cobound([integer(c_int64_t) :: 1], [integer(c_int64_t) :: 0], .true.))

    ALSO(check_cobound([integer(c_int64_t) :: 1, 1], [integer(c_int64_t) :: 2, 0], .true.))

    ALSO(check_cobound([integer(c_int64_t) :: 1, 1, 1], [integer(c_int64_t) :: 2, 3, 0], .true.))

    ALSO(check_cobound([integer(c_int64_t) :: 101, 101, 101], [integer(c_int64_t) :: 104, 102, 0], .true.))

    ! cover all the possible coranks
    ALSO(.all. check_corank([(corank, corank = 1_c_int, 15_c_int)], .false.))
    ALSO(.all. check_corank([(corank, corank = 1_c_int, 15_c_int)], .true.))
  end function

end module prif_coarray_inquiry_test_m
