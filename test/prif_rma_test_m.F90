! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module prif_rma_test_m
  !! Unit test fort the prif_rma program inititation subroutine
  use julienne_m, only : test_t, test_result_t, test_description_t
  use iso_c_binding, only: &
          c_ptr, c_intmax_t, c_intptr_t, c_size_t, c_null_funptr, c_f_pointer, c_loc, c_sizeof
  use prif_test_m, only : prif_test_t, test_description_substring
  use prif, only: &
          prif_coarray_handle, &
          prif_allocate_coarray, &
          prif_deallocate_coarray, &
          prif_allocate, &
          prif_deallocate, &
          prif_num_images, &
          prif_put, &
          prif_put_indirect, &
          prif_get, &
          prif_get_indirect, &
          prif_sync_all, &
          prif_this_image_no_coarray
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : test_function_i
#endif
  implicit none

  private
  public :: prif_rma_test_t

  type, extends(prif_test_t) :: prif_rma_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The prif_rma subroutine" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ & 
       test_description_t("sending a value to another image",        check_put) &
      ,test_description_t("sending a value with indirect interface", check_put_indirect) &
      ,test_description_t("getting a value from another image",      check_get) &
      ,test_description_t("getting a value with indirect interface", check_get_indirect) &
    ]   
#else
    procedure(test_function_i), pointer :: &
      check_put_ptr          => check_put          &
     ,check_put_indirect_ptr => check_put_indirect &
     ,check_get_ptr          => check_get          &
     ,check_get_indirect_ptr => check_get_indirect

    test_descriptions = [ & 
       test_description_t("sending a value to another image",        check_put_ptr) &
      ,test_description_t("sending a value with indirect interface", check_put_indirect_ptr) &
      ,test_description_t("getting a value from another image",      check_get_ptr) &
      ,test_description_t("getting a value with indirect interface", check_get_indirect_ptr) &
    ]   
#endif

    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0  &
      .or. test_descriptions%contains_text(test_description_substring))
      
    test_results = test_descriptions%run()
  end function

  function check_put() result(test_passes)
    logical test_passes

    integer :: dummy_element, num_imgs, expected, neighbor
    integer, target :: me
    type(prif_coarray_handle) :: coarray_handle
    type(c_ptr) :: allocated_memory
    integer, pointer :: local_slice
    integer(c_intmax_t) :: lcobounds(1), ucobounds(1)

    call prif_num_images(num_images=num_imgs)
    lcobounds(1) = 1
    ucobounds(1) = num_imgs
    call prif_allocate_coarray( &
            lcobounds = lcobounds, &
            ucobounds = ucobounds, &
            size_in_bytes = int(storage_size(dummy_element)/8, c_size_t), &
            final_func = c_null_funptr, &
            coarray_handle = coarray_handle, &
            allocated_memory = allocated_memory)
    call c_f_pointer(allocated_memory, local_slice)

    call prif_this_image_no_coarray(this_image=me)
    neighbor = merge(me+1, 1, me < num_imgs)
    expected = merge(me-1, num_imgs, me > 1)

    call prif_put( &
            image_num = neighbor, &
            coarray_handle = coarray_handle, &
            offset = 0_c_size_t, &
            current_image_buffer = c_loc(me), &
            size_in_bytes = c_sizeof(me))
    call prif_sync_all

    test_passes = expected == local_slice

    call prif_deallocate_coarray([coarray_handle])
  end function

  function check_put_indirect() result(test_passes)
    logical test_passes

    type :: my_type
      type(c_ptr) :: my_component
    end type

    type(my_type), target :: dummy_element
    integer, pointer :: component_access
    integer :: dummy_component, num_imgs, expected, neighbor
    integer, target :: me
    type(prif_coarray_handle) :: coarray_handle
    type(c_ptr) :: allocated_memory
    type(my_type), pointer :: local_slice
    integer(c_intmax_t) :: lcobounds(1), ucobounds(1)
    integer(c_intptr_t) :: base_addr

    call prif_num_images(num_images=num_imgs)
    lcobounds(1) = 1
    ucobounds(1) = num_imgs
    call prif_allocate_coarray( &
            lcobounds = lcobounds, &
            ucobounds = ucobounds, &
            size_in_bytes = int(storage_size(dummy_element)/8, c_size_t), &
            final_func = c_null_funptr, &
            coarray_handle = coarray_handle, &
            allocated_memory = allocated_memory)
    call c_f_pointer(allocated_memory, local_slice)
    call prif_allocate( &
            size_in_bytes = int(storage_size(dummy_component)/8, c_size_t), &
            allocated_memory = local_slice%my_component)
    call prif_sync_all

    call prif_this_image_no_coarray(this_image=me)
    neighbor = merge(me+1, 1, me < num_imgs)
    expected = merge(me-1, num_imgs, me > 1)

    call prif_get( &
            image_num = neighbor, &
            coarray_handle = coarray_handle, &
            offset = 0_c_size_t, &
            current_image_buffer = c_loc(dummy_element), &
            size_in_bytes = int(storage_size(dummy_element)/8, c_size_t))
    base_addr = transfer(dummy_element%my_component, base_addr)
    call prif_put_indirect( &
            image_num = neighbor, &
            remote_ptr = base_addr, &
            current_image_buffer = c_loc(me), &
            size_in_bytes = int(storage_size(me)/8, c_size_t))
    call prif_sync_all

    call c_f_pointer(local_slice%my_component, component_access)
    test_passes = expected == component_access

    call prif_deallocate(local_slice%my_component)
    call prif_deallocate_coarray([coarray_handle])
  end function

  function check_get() result(test_passes)
    logical test_passes

    integer :: dummy_element, num_imgs, me, neighbor, expected
    integer, target :: retrieved
    type(prif_coarray_handle) :: coarray_handle
    type(c_ptr) :: allocated_memory
    integer, pointer :: local_slice
    integer(c_intmax_t) :: lcobounds(1), ucobounds(1)

    call prif_num_images(num_images=num_imgs)
    lcobounds(1) = 1
    ucobounds(1) = num_imgs
    call prif_allocate_coarray( &
            lcobounds = lcobounds, &
            ucobounds = ucobounds, &
            size_in_bytes = int(storage_size(dummy_element)/8, c_size_t), &
            final_func = c_null_funptr, &
            coarray_handle = coarray_handle, &
            allocated_memory = allocated_memory)
    call c_f_pointer(allocated_memory, local_slice)

    call prif_this_image_no_coarray(this_image=me)
    neighbor = merge(me+1, 1, me < num_imgs)
    expected = neighbor
    local_slice = me
    call prif_sync_all

    call prif_get( &
            image_num = neighbor, &
            coarray_handle = coarray_handle, &
            offset = 0_c_size_t, &
            current_image_buffer = c_loc(retrieved), &
            size_in_bytes = c_sizeof(retrieved))

    test_passes = expected == retrieved

    call prif_deallocate_coarray([coarray_handle])
  end function

  function check_get_indirect() result(test_passes)
    logical test_passes

    type :: my_type
      type(c_ptr) :: my_component
    end type

    type(my_type), target :: dummy_element
    integer, pointer :: component_access
    integer :: dummy_component, num_imgs, me, expected, neighbor
    integer, target :: retrieved
    type(prif_coarray_handle) :: coarray_handle
    type(c_ptr) :: allocated_memory
    type(my_type), pointer :: local_slice
    integer(c_intmax_t) :: lcobounds(1), ucobounds(1)
    integer(c_intptr_t) :: base_addr

    call prif_num_images(num_images=num_imgs)
    lcobounds(1) = 1
    ucobounds(1) = num_imgs
    call prif_allocate_coarray( &
            lcobounds = lcobounds, &
            ucobounds = ucobounds, &
            size_in_bytes = int(storage_size(dummy_element)/8, c_size_t), &
            final_func = c_null_funptr, &
            coarray_handle = coarray_handle, &
            allocated_memory = allocated_memory)
    call c_f_pointer(allocated_memory, local_slice)
    call prif_allocate( &
            size_in_bytes = int(storage_size(dummy_component)/8, c_size_t), &
            allocated_memory = local_slice%my_component)

    call prif_this_image_no_coarray(this_image=me)
    neighbor = merge(me+1, 1, me < num_imgs)
    expected = neighbor
    call c_f_pointer(local_slice%my_component, component_access)
    component_access = me
    call prif_sync_all

    call prif_get( &
            image_num = neighbor, &
            coarray_handle = coarray_handle, &
            offset = 0_c_size_t, &
            current_image_buffer = c_loc(dummy_element), &
            size_in_bytes = int(storage_size(dummy_element)/8, c_size_t))
    base_addr = transfer(dummy_element%my_component, base_addr)
    call prif_get_indirect( &
            image_num = neighbor, &
            remote_ptr = base_addr, &
            current_image_buffer = c_loc(retrieved), &
            size_in_bytes = int(storage_size(retrieved)/8, c_size_t))

    test_passes = expected == retrieved

    call prif_deallocate(local_slice%my_component)
    call prif_deallocate_coarray([coarray_handle])
  end function

end module
