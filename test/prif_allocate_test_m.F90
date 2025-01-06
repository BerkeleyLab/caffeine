! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module prif_allocate_test_m
  !! Unit test for Caffeine's support for symmetric and asymmetric memory allocations
  use prif, only : prif_allocate_coarray, prif_deallocate_coarray, prif_coarray_handle &
                  ,prif_allocate,         prif_deallocate,         prif_num_images 
  use prif_test_m, only : prif_test_t, test_description_substring
  use julienne_m,  only : test_result_t     , vector_function_strategy_t, string_t & 
                         ,test_description_t, vector_test_description_t
                         
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : test_function_i
#endif
  use iso_c_binding, only:  c_ptr, c_int, c_intmax_t, c_size_t, c_null_funptr, c_f_pointer, c_null_ptr, c_loc
  implicit none

  private
  public :: prif_allocate_test_t

  type, extends(prif_test_t) :: prif_allocate_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  type, extends(vector_function_strategy_t) :: symmetric_allocation_test_function_t
  contains
    procedure, nopass :: vector_function => check_symmetric_allocation
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The prif_allocate_coarray subroutine"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:), vector_test_results(:)
    type(test_description_t), allocatable :: scalar_test_descriptions(:)
    type(vector_test_description_t), allocatable :: vector_test_descriptions(:)
    type(symmetric_allocation_test_function_t) symmetric_allocation_test_function

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    scalar_test_descriptions = [ & 
      test_description_t("allocating/using/deallocating a corank-1 integer scalar coarray", &
              check_asymmetric_allocation) &
    ]
#else
    procedure(test_function_i), pointer :: check_asymmetric_allocation_ptr

    check_asymmetric_allocation_ptr => check_asymmetric_allocation

    scalar_test_descriptions = [ & 
      test_description_t("allocating/using/deallocating a corank-1 integer scalar coarray", &
              check_asymmetric_allocation_ptr) &
    ]
#endif

   vector_test_descriptions = [ &
     vector_test_description_t( &
       [  string_t("a local slice being initially unassociated") &
         ,string_t("an allocated slice being associated") &
         ,string_t("a defined slice having the expected value") &
       ], symmetric_allocation_test_function &
     ) &
   ]

    associate( &
      substring_in_subject => index(subject(), test_description_substring) /= 0, &
      substring_in_description => scalar_test_descriptions%contains_text(string_t(test_description_substring)), &
      num_vector_tests => size(vector_test_descriptions) &
    )
      scalar_test_descriptions = pack(scalar_test_descriptions, substring_in_subject .or. substring_in_description)

      block
        integer i

        associate( &
          substring_in_description_vector => &
            [(any(vector_test_descriptions(i)%contains_text(test_description_substring)), i=1,num_vector_tests)] &
        )
          if (substring_in_subject) then
            vector_test_results = [(vector_test_descriptions(i)%run(), i=1,num_vector_tests)]
          else if (any(substring_in_description_vector)) then
              vector_test_descriptions = pack(vector_test_descriptions, substring_in_description_vector)
              vector_test_results =  [(vector_test_descriptions(i)%run(), i=1,size(vector_test_descriptions))]
              vector_test_results =  &
                pack(vector_test_results, vector_test_results%description_contains(string_t(test_description_substring)))
           else
            vector_test_results = [test_result_t::]
          end if
          test_results = [scalar_test_descriptions%run(), vector_test_results]
        end associate
      end block
    end associate

  end function results

  function check_symmetric_allocation() result(test_passes)
    !! Allocate memory for an integer scalar single corank coarray, such as the following decl
    !! integer :: coarr[*]
    logical, allocatable :: test_passes(:)
    integer(kind=c_intmax_t), dimension(1) :: lcobounds, ucobounds
    integer, pointer :: local_slice
    integer dummy_element, num_imgs
    type(prif_coarray_handle) coarray_handle
    type(c_ptr) allocated_memory

    call prif_num_images(num_images=num_imgs)
    lcobounds(1) = 1
    ucobounds(1) = num_imgs
    allocated_memory = c_null_ptr
    local_slice => null()

    associate(slice_initially_unassociated => .not. associated(local_slice))

      call prif_allocate_coarray( &
        lcobounds, ucobounds, int(storage_size(dummy_element)/8, c_size_t), c_null_funptr, coarray_handle, allocated_memory)
      call c_f_pointer(allocated_memory, local_slice)

      associate(allocated_slice_associated => associated(local_slice))
        local_slice = 42
        associate(defined_slice_has_expected_value => 42 == local_slice)
          call prif_deallocate_coarray([coarray_handle])
          test_passes = [slice_initially_unassociated, allocated_slice_associated, defined_slice_has_expected_value]
        end associate
      end associate

    end associate

  end function

  function check_asymmetric_allocation() result(test_passes)
    logical test_passes

    type(c_ptr) :: allocated_memory
    integer(c_int), pointer :: local_slice

    call prif_allocate(sizeof(local_slice), allocated_memory)
    call c_f_pointer(allocated_memory, local_slice)

    local_slice = 42

    associate(definition_succeeds => 42 == local_slice)
      test_passes = definition_succeeds 
    end associate

    call prif_deallocate(c_loc(local_slice))
  end function

end module prif_allocate_test_m
