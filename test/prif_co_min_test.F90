#include "language-support.F90"

module prif_co_min_test_m
  use iso_c_binding, only: c_int8_t, c_int16_t, c_int32_t, c_int64_t, c_float, c_double
  use prif, only : prif_co_min, prif_co_min_character, prif_this_image_no_coarray, prif_num_images
  use julienne_m, only: &
    operator(.all.) &
   ,operator(.approximates.) &
   ,operator(.within.) &
   ,operator(.equalsExpected.) &
   ,usher &
   ,test_description_t &
   ,test_diagnosis_t &
   ,test_result_t &
   ,test_t
  implicit none

  private
  public :: prif_co_min_test_t

  type, extends(test_t) :: prif_co_min_test_t
  contains
    procedure, nopass, non_overridable :: subject
    procedure, nopass, non_overridable :: results
  end type

contains
  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = "The prif_co_min subroutine"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(prif_co_min_test_t) prif_co_min_test

    test_results = prif_co_min_test%run([ &
       test_description_t("computing element-wise minima for integer(c_int32_t) scalars", usher(check_32_bit_integer)) &
      ,test_description_t("computing element-wise minima for a 1D default integer array", usher(check_default_integer)) &
      ,test_description_t("computing element-wise minima for a 1D integer(c_int8t) array", usher(check_8_bit_integer)) &
      ,test_description_t("computing element-wise minima for a 1D integer(c_int16_t) array", usher(check_16_bit_integer)) &
      ,test_description_t("computing element-wise minima for a 1D integer(c_int64_t) array", usher(check_64_bit_integer)) &
      ,test_description_t("computing element-wise minima for a 2D real(c_float) array", usher(check_32_bit_real)) &
      ,test_description_t("computing element-wise minima for a 1D real(c_double) array", usher(check_64_bit_real)) &
      ,test_description_t("computing element-wise minima for a character scalar", usher(check_character)) &
    ])
  end function

  function check_default_integer() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis

      integer, parameter :: values(*,*) = reshape([1, -19, 5, 13, 11, 7, 17, 3], [2, 4])
      integer, dimension(size(values,1)) :: my_val, expected
      integer me, ni, i

      call prif_this_image_no_coarray(this_image=me)
      call prif_num_images(ni)

      my_val = values(:, mod(me-1, size(values,2))+1)
      call prif_co_min(my_val)

      expected = minval(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), dim=2)
      test_diagnosis = .all. (int(my_val) .equalsExpected. int(expected))
  end function

  function check_8_bit_integer() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis

      integer(c_int8_t), parameter :: values(*,*) = reshape(int([1, -19, 5, 13, 11, 7, 17, 3],c_int8_t), [2, 4])
      integer :: me, ni, i
      integer(c_int8_t), dimension(size(values,1)) :: my_val, expected

      call prif_this_image_no_coarray(this_image=me)
      call prif_num_images(ni)

      my_val = values(:, mod(me-1, size(values,2))+1)
      call prif_co_min(my_val)

      expected = minval(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), dim=2)
      test_diagnosis = .all. (int(my_val) .equalsExpected. int(expected))
  end function

  function check_16_bit_integer() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis

      integer(c_int16_t), parameter :: values(*,*) = reshape(int([1, -19, 5, 13, 11, 7, 17, 3],c_int16_t), [2, 4])
      integer :: me, ni, i
      integer(c_int16_t), dimension(size(values,1)) :: my_val, expected

      call prif_this_image_no_coarray(this_image=me)
      call prif_num_images(ni)

      my_val = values(:, mod(me-1, size(values,2))+1)
      call prif_co_min(my_val)

      expected = minval(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), dim=2)
      test_diagnosis = .all. (int(my_val) .equalsExpected. int(expected))
  end function

  function check_32_bit_integer() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis

      integer(c_int32_t), parameter :: values(*) = [1, -19, 5, 13, 11, 7, 17, 3]
      integer :: me, ni, i
      integer(c_int32_t) :: my_val, expected

      call prif_this_image_no_coarray(this_image=me)
      call prif_num_images(ni)

      my_val = values(mod(me-1, size(values))+1)
      call prif_co_min(my_val)

      expected = minval([(values(mod(i-1,size(values))+1), i = 1, ni)])
      test_diagnosis = int(my_val) .equalsExpected. int(expected)
  end function

  function check_64_bit_integer() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis

      integer(c_int64_t), parameter :: values(*,*) = reshape([1, -19, 5, 13, 11, 7, 17, 3], [2, 4])
      integer :: me, ni, i
      integer(c_int64_t), dimension(size(values,1)) :: my_val, expected

      call prif_this_image_no_coarray(this_image=me)
      call prif_num_images(ni)

      my_val = values(:, mod(me-1, size(values,2))+1)
      call prif_co_min(my_val)

      expected = minval(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), dim=2)
      test_diagnosis = .all. (int(my_val) .equalsExpected. int(expected))
  end function

  function check_32_bit_real() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis

      real(c_float), parameter :: values(*,*,*) = reshape([1, 19, 5, 13, 11, 7, 17, 3], [2,2,2])
      real(c_double), parameter :: tolerance = 0_c_double
      integer :: me, ni, i
      real(c_float), dimension(size(values,1), size(values,2)) :: my_val, expected

      call prif_this_image_no_coarray(this_image=me)
      call prif_num_images(ni)

      my_val = values(:, :, mod(me-1, size(values,3))+1)
      call prif_co_min(my_val)

      expected = minval(reshape([(values(:,:,mod(i-1,size(values,3))+1), i = 1, ni)], [size(values,1), size(values,2), ni]), dim=3)
      test_diagnosis = .all. (real(expected,kind=c_double) .approximates. real(my_val,kind=c_double) .within. tolerance)
  end function

  function check_64_bit_real() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis

      real(c_double), parameter :: values(*,*) = reshape([1, 19, 5, 13, 11, 7, 17, 3], [2, 4])
      real(c_double), parameter :: tolerance = 0_c_double
      integer :: me, ni, i
      real(c_double), dimension(size(values,1)) :: my_val, expected

      call prif_this_image_no_coarray(this_image=me)
      call prif_num_images(ni)

      my_val = values(:, mod(me-1, size(values,2))+1)
      call prif_co_min(my_val)

      expected = minval(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), dim=2)
      test_diagnosis = .all. (my_val .approximates. expected .within. tolerance)
  end function

  function check_character() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis
      character(len=*), parameter :: values(*) = &
          [ "To be   ","or not  " &
          , "to      ","be.     " &
          , "that    ","is      " &
          , "the     ","question"]
      character(len=len(values)) my_val
      integer me, ni, i

      call prif_this_image_no_coarray(this_image=me)
      call prif_num_images(ni)

      my_val = values(mod(me-1, size(values))+1)
      call prif_co_min_character(my_val)

      ! issue #205: workaround flang optimizer bug with a temp
      associate(tmp => [(values(mod(i-1,size(values))+1), i = 1, ni)])
        test_diagnosis = .all. (my_val .equalsExpected. minval(tmp))
      end associate
  end function

end module prif_co_min_test_m
