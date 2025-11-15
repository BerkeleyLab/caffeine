module prif_coarray_inquiry_test_m
  use prif, only : &
      prif_allocate_coarray, prif_deallocate_coarray, &
      prif_coarray_handle, prif_num_images, &
      prif_local_data_pointer, prif_size_bytes, &
      prif_lcobound_no_dim, prif_lcobound_with_dim, &
      prif_ucobound_no_dim, prif_ucobound_with_dim, &
      prif_coshape
  use julienne_m, only: &
     operator(//) &
    ,operator(.all.) &
    ,operator(.also.) &
    ,operator(.equalsExpected.) &
    ,operator(.expect.) &
    ,usher &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t
  use iso_c_binding, only: &
      c_ptr, c_null_ptr, c_int64_t, c_int, c_size_t, c_null_funptr, c_associated

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
     test_subject = "The PRIF coarray inquiry subroutines"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(prif_coarray_inquiry_test_t) prif_coarray_inquiry_test

    test_results = prif_coarray_inquiry_test%run([ &
       test_description_t("preserving the prif_local_data_pointer for an allocated coarray", usher(check_prif_local_data_pointer)) &
      ,test_description_t("checking passed cobounds", usher(check_cobounds)) &
    ])
  end function

  function check_prif_local_data_pointer() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis

      integer(kind=c_int64_t), dimension(1) :: lcobounds, ucobounds
      integer :: dummy_element, num_imgs
      type(prif_coarray_handle) :: coarray_handle
      type(c_ptr) :: allocation_ptr, local_ptr

      call prif_num_images(num_images=num_imgs)
      lcobounds(1) = 1
      ucobounds(1) = num_imgs

      call prif_allocate_coarray( &
              lcobounds, &
              ucobounds, &
              int(storage_size(dummy_element)/8, c_size_t), &
              c_null_funptr, &
              coarray_handle, &
              allocation_ptr)
      call prif_local_data_pointer(coarray_handle, local_ptr)
      test_diagnosis = .expect. c_associated(local_ptr, allocation_ptr)
      call prif_deallocate_coarray([coarray_handle])
  end function

  impure elemental function check_cobound(corank) result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer(c_int), intent(in) :: corank

    ! Allocate memory for an integer scalar coarray with given corank
    ! and then test some queries on it

    integer :: num_imgs, i
    integer(kind=c_int64_t), dimension(corank) :: lcobounds, ucobounds, tmp_bounds
    integer(kind=c_int64_t) :: tmp_bound
    integer(kind=c_size_t), dimension(corank)  :: sizes
    type(prif_coarray_handle) :: coarray_handle
    type(c_ptr) :: allocated_memory
    integer(c_size_t) :: data_size, query_size

    call prif_num_images(num_images=num_imgs)
    lcobounds(1) = 1
    ucobounds(1) = num_imgs
    do i = 2,corank
      lcobounds(i) = i
      ucobounds(i) = i*2
    end do

    allocated_memory = c_null_ptr
    data_size = 64 * corank

    call prif_allocate_coarray( &
      lcobounds, ucobounds, data_size, c_null_funptr, &
      coarray_handle, allocated_memory)

    test_diagnosis = .expect. c_associated(allocated_memory)

    call prif_size_bytes(coarray_handle, data_size=query_size)
    test_diagnosis = test_diagnosis .also. (query_size .equalsExpected.  data_size) // "prif_size_bytes is valid"

    call prif_lcobound_no_dim(coarray_handle, tmp_bounds)
    test_diagnosis = test_diagnosis .also. (.all. (tmp_bounds .equalsExpected. lcobounds)) // "prif_lcobound_no_dim is valid"

    call prif_ucobound_no_dim(coarray_handle, tmp_bounds)
    test_diagnosis = test_diagnosis .also. (.all. (tmp_bounds .equalsExpected. ucobounds)) // "prif_ucobound_no_dim is valid"

    do i = 1, corank
      call prif_lcobound_with_dim(coarray_handle, i, tmp_bound)
      test_diagnosis = test_diagnosis .also. (tmp_bound .equalsExpected. lcobounds(i)) // "prif_lcobound_with_dim is valid"

      call prif_ucobound_with_dim(coarray_handle, i, tmp_bound)
      test_diagnosis = test_diagnosis .also. (tmp_bound .equalsExpected. ucobounds(i)) // "prif_ucobound_with_dim is valid"
    end do

    call prif_coshape(coarray_handle, sizes)
    test_diagnosis = test_diagnosis .also. (.all. ((ucobounds - lcobounds + 1) .equalsExpected. sizes)) // "prif_coshape is valid"

    call prif_deallocate_coarray([coarray_handle])
  end function

  function check_cobounds() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer(c_int) :: corank

    test_diagnosis = .all. check_cobound([(corank, corank = 1_c_int, 15_c_int)])
  end function

end module prif_coarray_inquiry_test_m
