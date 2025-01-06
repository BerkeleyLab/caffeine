! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module prif_co_sum_test_m
  !! Unit test fort the prif_co_sum program inititation subroutine
  use prif, only : prif_co_sum, prif_num_images, prif_this_image_no_coarray
  use prif_test_m, only : prif_test_t, test_description_substring
  use julienne_m, only : test_result_t, test_description_t, test_diagnosis_t, string_t, operator(.csv.)
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : diagnosis_function_i
#endif
  implicit none

  private
  public :: prif_co_sum_test_t

  type, extends(prif_test_t) :: prif_co_sum_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The prif_co_sum subroutine"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ & 
       test_description_t("summing default integer scalars with no optional arguments",            sum_default_integer_scalars) &
      ,test_description_t("summing default integer scalars with all arguments",                    sum_integers_all_arguments) &
      ,test_description_t("summing integer(c_int64_t) scalars with stat argument",                 sum_c_int64_scalars) &
      ,test_description_t("summing default integer 1D arrays with no optional arguments",          sum_default_integer_1D_array) &
      ,test_description_t("summing default integer 15D arrays with stat argument",                 sum_default_integer_15D_array) &
      ,test_description_t("summing default real scalars with result_image argument",               sum_default_real_scalars) &
      ,test_description_t("summing double precision 2D arrays with no optional arguments",         sum_double_precision_2D_array) &
      ,test_description_t("summing default complex scalars with stat argument",                    sum_default_complex_scalars) &
      ,test_description_t("summing double-precision 1D complex arrays with no optional arguments", sum_dble_complex_1D_arrays) &
    ]   
#else
    procedure(diagnosis_function_i), pointer :: &
       sum_default_integer_scalars_ptr   =>  sum_default_integer_scalars  &
      ,sum_integers_all_arguments_ptr    =>  sum_integers_all_arguments   &
      ,sum_c_int64_scalars_ptr           =>  sum_c_int64_scalars          &
      ,sum_default_integer_1D_array_ptr  =>  sum_default_integer_1D_array &
      ,sum_default_integer_15D_array_ptr =>  sum_default_integer_15D_array&
      ,sum_default_real_scalars_ptr      =>  sum_default_real_scalars     &
      ,sum_double_precision_2D_array_ptr =>  sum_double_precision_2D_array&
      ,sum_default_complex_scalars_ptr   =>  sum_default_complex_scalars  &
      ,sum_dble_complex_1D_arrays_ptr    =>  sum_dble_complex_1D_arrays   

    test_descriptions = [ & 
       test_description_t("summing default integer scalars with no optional arguments",          sum_default_integer_scalars_ptr)  &
      ,test_description_t("summing default integer scalars with all arguments",                  sum_integers_all_arguments_ptr)   &
      ,test_description_t("summing integer(c_int64_t) scalars with stat argument",               sum_c_int64_scalars_ptr)          &
      ,test_description_t("summing default integer 1D arrays with no optional arguments",        sum_default_integer_1D_array_ptr) &
      ,test_description_t("summing default integer 15D arrays with stat argument",               sum_default_integer_15D_array_ptr)&
      ,test_description_t("summing default real scalars with result_image argument",             sum_default_real_scalars_ptr)     &
      ,test_description_t("summing double precision 2D arrays with no optional arguments",       sum_double_precision_2D_array_ptr)&
      ,test_description_t("summing default complex scalars with stat argument",                  sum_default_complex_scalars_ptr)  &
      ,test_description_t("summing double-precision 1D complex arrays with no optional arguments", sum_dble_complex_1D_arrays_ptr)&
    ]   
#endif

    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0  &
      .or. test_descriptions%contains_text(test_description_substring))
      
    test_results = test_descriptions%run()
  end function

  function sum_default_integer_scalars() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis
      integer image_count, n

      image_count = 1
      call prif_co_sum(image_count)
      call prif_num_images(num_images=n)
      test_diagnosis = test_diagnosis_t( &
         test_passed = image_count == n &
        ,diagnostics_string = "expected " // string_t(n) // ", actual " // string_t(image_count) &
      )
  end function

  function sum_integers_all_arguments() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis
      integer i, status_, result_image_, me, num_imgs
      character(len=*), parameter :: whitespace = repeat(" ", ncopies=29)
      character(len=:), allocatable :: error_message

      i = 1
      result_image_ = 1
      status_ = -1
      error_message = whitespace

      call prif_this_image_no_coarray(this_image=me)
      call prif_num_images(num_images=num_imgs)
      associate(expected_i => merge(num_imgs*i, i, me==result_image_))
        call prif_co_sum(i, result_image_, status_, error_message)
        test_diagnosis = test_diagnosis_t( &
           test_passed = (expected_i == i) .and. (status_ == 0) .and. (whitespace == error_message) &
          ,diagnostics_string = &
               "expected i=" // string_t(expected_i) // ", status=" // "0"               // ", error_message='" // whitespace    // "'" & 
            // ", actual i=" // string_t(i)          // ", status=" // string_t(status_) // ", error_message='" // error_message // "'" &
        )
      end associate
  end function

  function sum_c_int64_scalars() result(test_diagnosis)
      use iso_c_binding, only : c_int64_t
      type(test_diagnosis_t) test_diagnosis
      integer(c_int64_t) i
      integer i_default_kind, status_, num_imgs

      status_ = -1
      i = 2_c_int64_t
      call prif_co_sum(i, stat=status_)
      i_default_kind = int(i,kind(i_default_kind))
      call prif_num_images(num_images=num_imgs)
      test_diagnosis = test_diagnosis_t( &
         test_passed = (2*num_imgs == i_default_kind) .and. (status_ == 0) &
        ,diagnostics_string = &
             "expected i=" // string_t(2*num_imgs)     // ", status_= " // "0" &
          // "; actual i=" // string_t(i_default_kind) // ", status_= " // string_t(status_) &
      )
  end function

  function sum_default_integer_1D_array() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis
      integer i, images
      integer, allocatable :: array(:)

      call prif_num_images(num_images=images)
      associate(sequence_ => [(i,i=1,images)])
        array = sequence_
        call prif_co_sum(array)
        test_diagnosis = test_diagnosis_t( &
           test_passed = all(array == images*sequence_) &
          ,diagnostics_string = &
               "expected " // .csv. string_t(images*sequence_)  &
            // "; actual " // .csv. string_t(array) & 
        )
      end associate
  end function

  function sum_default_integer_15D_array() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis
      integer, target :: array(2,1,1, 1,1,1, 1,1,1, 1,1,1, 1,2,1)
      integer, pointer :: array_1D_ptr(:)
      integer status_, num_imgs

      status_ = -1
      array = 3
      call prif_co_sum(array, stat=status_)
      call prif_num_images(num_images=num_imgs)
      associate(expected_sum => 3*num_imgs)
        array_1D_ptr(1:size(array)) => array
        test_diagnosis = test_diagnosis_t( &
           test_passed = all(array == expected_sum) .and. (status_ == 0) &
          ,diagnostics_string = &
                "expected element value  = " //       string_t(expected_sum) // ", status_= " // "0" &
             // "; actual element values = " // .csv. string_t(array_1D_ptr) // ", status_= " // string_t(status_) &
        )
      end associate
  end function

  function sum_default_real_scalars() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis
      real scalar
      real, parameter :: e = 2.7182818459045, tolerance = 1E-07
      integer result_image_, me, num_imgs

      result_image_ = 1
      scalar = e
      call prif_co_sum(scalar, result_image=result_image_)
      call prif_this_image_no_coarray(this_image=me)
      call prif_num_images(num_images=num_imgs)
      associate(expected_result => merge(num_imgs*e, e, me==result_image_))
        test_diagnosis = test_diagnosis_t( &
           test_passed = abs(expected_result - scalar) < tolerance &
          ,diagnostics_string = "expected " // string_t(expected_result) &
                             // "; actual " // string_t(scalar) &
        )
      end associate
  end function

  function sum_double_precision_2D_array() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis
      double precision, allocatable, target :: array(:,:)
      double precision, parameter :: input(*,*) = reshape(-[6,5,4,3,2,1], [3,2]), tolerance = 1D-14
      double precision, pointer :: array_1D_ptr(:)
      integer num_imgs

      array = input
      call prif_co_sum(array)
      call prif_num_images(num_images=num_imgs)

      associate(expected_sum => input*num_imgs)
        array_1D_ptr(1:size(array)) => array
        test_diagnosis = test_diagnosis_t( &
           test_passed = all(abs(expected_sum - array) < tolerance)  &
          ,diagnostics_string = &
            "expected " // .csv. string_t(reshape(expected_sum, [size(expected_sum)])) // &
            "; actual " // .csv. string_t(array_1D_ptr) &
        )
      end associate
  end function

  function sum_default_complex_scalars() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis
      real scalar
      real, parameter :: tolerance = 1E-07
      complex z
      complex, parameter :: i=(0.,1.)
      integer status_, num_imgs

      status_ = -1
      z = i
      call prif_co_sum(z, stat=status_)
      call prif_num_images(num_images=num_imgs)
      associate(expected_z => i*num_imgs)
        test_diagnosis = test_diagnosis_t( &
           test_passed = (abs(expected_z - z) < tolerance ) .and. (status_ == 0) &
          ,diagnostics_string = &
               "expected (" // string_t(expected_z%Re) //","// string_t(expected_z%Im) // "; status= " // "0" &
            // "; actual " // string_t(          z%Re) //","// string_t(         z%Im) // "; status= " // string_t(status_) &
        )
      end associate
  end function

  function sum_dble_complex_1D_arrays() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis
      integer, parameter :: dp = kind(1.D0)
      integer :: num_imgs
      complex(dp), allocatable :: array(:)
      complex(dp), parameter :: input(*) = [(1.D0,1.0D0)]
      double precision, parameter :: tolerance = 1E-14

      array = input
      call prif_co_sum(array)
      call prif_num_images(num_images=num_imgs)
      associate(expected_sum => input*num_imgs)
        test_diagnosis = test_diagnosis_t( &
           test_passed = all(abs(expected_sum - array) < tolerance) &
          ,diagnostics_string = &
               "expected (" // string_t(expected_sum(1)%Re) // "," // string_t(expected_sum(1)%Im) // ")" &
            // "; actual (" // string_t(       array(1)%Re) // "," // string_t(       array(1)%Im) // ")" &
        )
      end associate
  end function

end module prif_co_sum_test_m
