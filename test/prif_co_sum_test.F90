module prif_co_sum_test_m
    use iso_c_binding, only: c_int8_t, c_int16_t, c_int32_t, c_int64_t, c_float, c_double
    use prif, only : prif_co_sum, prif_num_images, prif_this_image_no_coarray
    use julienne_m, only: &
       operator(.all.) &
      ,operator(.also.) &
      ,operator(.approximates.) &
      ,operator(.equalsExpected.) &
      ,operator(.within.) &
      ,usher &
      ,test_description_t &
      ,test_diagnosis_t &
      ,test_result_t &
      ,test_t

    implicit none
    private
    public :: prif_co_sum_test_t

    type, extends(test_t) :: prif_co_sum_test_t
    contains
      procedure, nopass, non_overridable :: subject
      procedure, nopass, non_overridable :: results
    end type

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = "The prif_co_sum subroutine"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(prif_co_sum_test_t) prif_co_sum_test

      test_results = prif_co_sum_test%run([ &
         test_description_t("computing the element-wise sum of a 1D default integer array", usher(check_default_integer)) &
        ,test_description_t("computing the element-wise sum of a 1D 8-bit integer(c_int8_t) array", usher(check_8_bit_integer)) &
        ,test_description_t("computing the element-wise sum of a 1D 16-bit integer(c_int16_t) array", usher(check_16_bit_integer)) &
        ,test_description_t("computing the element-wise sum of integer(c_int32_t) scalars", usher(check_32_bit_integer)) &
        ,test_description_t("computing the element-wise sum of a 1D 64-bit integer(c_int64_t) array", usher(check_64_bit_integer)) &
        ,test_description_t("computing the element-wise sum of a 2D 32-bit real(c_float) array", usher(check_32_bit_real)) &
        ,test_description_t("computing the element-wise sum of a 1D 64-bit real(c_double) array", usher(check_64_bit_real)) &
        ,test_description_t("computing the element-wise sum of a 2D complex(c_float) array", usher(check_32_bit_complex)) &
        ,test_description_t("computing the element-wise sum of a 1D complex(c_double) array", usher(check_64_bit_complex)) &
      ])
  end function

  function check_default_integer() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis

      integer, parameter :: values(*,*) = reshape([1, -19, 5, 13, 11, 7, 17, 3], [2, 4])
      integer :: me, ni, i
      integer, dimension(size(values,1)) :: my_val, expected

      call prif_this_image_no_coarray(this_image=me)
      call prif_num_images(ni)

      my_val = values(:, mod(me-1, size(values,2))+1)
      call prif_co_sum(my_val)

      expected = sum(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), dim=2)
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
      call prif_co_sum(my_val)

      expected = sum(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), dim=2)
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
      call prif_co_sum(my_val)

      expected = sum(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), dim=2)
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
      call prif_co_sum(my_val)

      expected = sum([(values(mod(i-1,size(values))+1), i = 1, ni)])
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
      call prif_co_sum(my_val)

      expected = sum(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), dim=2)
      test_diagnosis = .all. (int(my_val) .equalsExpected. int(expected))
  end function

  function check_32_bit_real() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis

      real(c_float), parameter :: values(*,*,*) = reshape([1, 19, 5, 13, 11, 7, 17, 3], [2,2,2])
      real(c_float), parameter :: tolerance = 0_c_float
      integer :: me, ni, i
      real(c_float), dimension(size(values,1), size(values,2)) :: my_val, expected

      call prif_this_image_no_coarray(this_image=me)
      call prif_num_images(ni)

      my_val = values(:, :, mod(me-1, size(values,3))+1)
      call prif_co_sum(my_val)

      expected = sum(reshape([(values(:,:,mod(i-1,size(values,3))+1), i = 1, ni)], [size(values,1), size(values,2), ni]), dim=3)
      test_diagnosis = .all. (my_val .approximates. expected .within. tolerance)
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
      call prif_co_sum(my_val)

      expected = sum(reshape([(values(:, mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1),ni]), dim=2)
      test_diagnosis = .all. (my_val .approximates. expected .within. tolerance)
  end function

  function check_32_bit_complex() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis

      complex(c_float), parameter :: values(*,*,*) = reshape( &
              [ cmplx(1., 53.), cmplx(3., 47.) &
              , cmplx(5., 43.), cmplx(7., 41.) &
              , cmplx(11., 37.), cmplx(13., 31.) &
              , cmplx(17., 29.), cmplx(19., 23.) &
              ], &
              [2,2,2])
      real(c_float), parameter :: tolerance = 0_c_float
      integer :: me, ni, i
      complex(c_float), dimension(size(values,1),size(values,2)) :: my_val, expected

      call prif_this_image_no_coarray(this_image=me)
      call prif_num_images(ni)

      my_val = values(:, :, mod(me-1, size(values,3))+1)
      call prif_co_sum(my_val)

      expected = sum(reshape([(values(:,:,mod(i-1,size(values,3))+1), i = 1, ni)], [size(values,1), size(values,2), ni]), dim=3)

      test_diagnosis = &
        .all. (real(my_val) .approximates. real(expected) .within. tolerance) &
        .also. (.all. (aimag(my_val) .approximates. aimag(expected) .within. tolerance))
  end function

  function check_64_bit_complex() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis

      complex(c_double), parameter :: values(*,*) = reshape( &
              [ cmplx(1., 53.), cmplx(3., 47.) &
              , cmplx(5., 43.), cmplx(7., 41.) &
              , cmplx(11., 37.), cmplx(13., 31.) &
              , cmplx(17., 29.), cmplx(19., 23.) &
              ], &
              [2,4])
      real(c_double), parameter :: tolerance = 0_c_double
      integer me, ni, i
      complex(c_double), dimension(size(values,1)) :: my_val, expected

      call prif_this_image_no_coarray(this_image=me)
      call prif_num_images(ni)

      my_val = values(:, mod(me-1, size(values,2))+1)
      call prif_co_sum(my_val)

      expected = sum(reshape([(values(:,mod(i-1,size(values,2))+1), i = 1, ni)], [size(values,1), ni]), dim=2)

      test_diagnosis = &
        .all. (real(my_val, c_double) .approximates. real(expected, c_double) .within. tolerance) &
        .also. (.all. (real(aimag(my_val), c_double) .approximates. real(aimag(expected), c_double) .within. tolerance))
  end function

end module prif_co_sum_test_m
