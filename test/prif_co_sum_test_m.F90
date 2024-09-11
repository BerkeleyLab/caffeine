! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module prif_co_sum_test_m
  !! Unit test fort the prif_co_sum program inititation subroutine
  use prif, only : prif_co_sum, prif_num_images, prif_this_image_no_coarray
  use julienne_m, only : test_t, test_result_t, test_description_t, test_description_substring
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : test_function_i
#endif
  implicit none

  private
  public :: prif_co_sum_test_t

  type, extends(test_t) :: prif_co_sum_test_t
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
    procedure(test_function_i), pointer :: &
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

  function sum_default_integer_scalars() result(test_passes)
      logical test_passes
      integer i, num_imgs

      i = 1
      call prif_co_sum(i)
      call prif_num_images(num_images=num_imgs)
      test_passes = num_imgs == i
  end function

  function sum_integers_all_arguments() result(test_passes)
      logical test_passes
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
        test_passes = (expected_i == i) .and. (status_ == 0) .and. (whitespace == error_message)
      end associate
  end function

  function sum_c_int64_scalars() result(test_passes)
      use iso_c_binding, only : c_int64_t
      logical test_passes
      integer(c_int64_t) i
      integer i_default_kind, status_, num_imgs

      status_ = -1
      i = 2_c_int64_t
      call prif_co_sum(i, stat=status_)
      i_default_kind = i
      call prif_num_images(num_images=num_imgs)
      test_passes = (2*num_imgs == int(i)) .and. (status_ == 0)
  end function

  function sum_default_integer_1D_array() result(test_passes)
      logical test_passes
      integer i, images
      integer, allocatable :: array(:)

      call prif_num_images(num_images=images)
      associate(sequence_ => [(i,i=1,images)])
        array = sequence_
        call prif_co_sum(array)
        test_passes = all(array == images*sequence_)
      end associate
  end function

  function sum_default_integer_15D_array() result(test_passes)
      logical test_passes
      integer array(2,1,1, 1,1,1, 1,1,1, 1,1,1, 1,2,1)
      integer status_, num_imgs

      status_ = -1
      array = 3
      call prif_co_sum(array, stat=status_)
      call prif_num_images(num_images=num_imgs)
      test_passes = (all(3*num_imgs == array)) .and.  (0 == status_)
  end function

  function sum_default_real_scalars() result(test_passes)
      logical test_passes
      real scalar
      real, parameter :: e = 2.7182818459045
      integer result_image_, me, num_imgs

      result_image_ = 1
      scalar = e
      call prif_co_sum(scalar, result_image=result_image_)
      call prif_this_image_no_coarray(this_image=me)
      call prif_num_images(num_images=num_imgs)
      associate(expected_result => merge(num_imgs*e, e, me==result_image_))
        test_passes = dble(expected_result) == dble(scalar)
      end associate
  end function

  function sum_double_precision_2D_array() result(test_passes)
      logical test_passes
      double precision, allocatable :: array(:,:)
      double precision, parameter :: input(*,*) = reshape(-[6,5,4,3,2,1], [3,2])
      integer :: num_imgs

      array = input
      call prif_co_sum(array)
      call prif_num_images(num_images=num_imgs)
      test_passes = product(num_imgs*input) == product(array)
  end function

  function sum_default_complex_scalars() result(test_passes)
      logical test_passes
      real scalar
      complex z
      complex, parameter :: i=(0.,1.)
      integer status_, num_imgs

      status_ = -1
      z = i
      call prif_co_sum(z, stat=status_)
      call prif_num_images(num_images=num_imgs)
      test_passes = (dble(abs(i*num_imgs)) == dble(abs(z)) ) .and. (status_ == 0)
  end function

  function sum_dble_complex_1D_arrays() result(test_passes)
      logical test_passes
      integer, parameter :: dp = kind(1.D0)
      integer :: num_imgs
      complex(dp), allocatable :: array(:)
      complex(dp), parameter :: input(*) = [(1.D0,1.0D0)]

      array = [(1.D0,1.D0)]
      call prif_co_sum(array)
      call prif_num_images(num_images=num_imgs)
      test_passes = all([input*num_imgs] == array)
  end function

end module prif_co_sum_test_m
