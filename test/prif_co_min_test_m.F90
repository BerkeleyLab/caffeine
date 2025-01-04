! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module prif_co_min_test_m
  !! Unit test for the prif_co_min subroutine
  use iso_c_binding, only: c_size_t, c_ptr, c_intmax_t, c_null_funptr
  use prif, only : prif_co_min, prif_num_images, prif_this_image_no_coarray, prif_num_images
  use prif_test_m, only : prif_test_t, test_description_substring
  use julienne_m, only : test_result_t, test_description_t, test_diagnosis_t, string_t, operator(.csv.)
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : diagnosis_function_i
#endif
  implicit none

  private
  public :: prif_co_min_test_t

  type, extends(prif_test_t) :: prif_co_min_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The prif_co_min subroutine global minimum computation"
          
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ &
       test_description_t("default integer scalar with stat argument",                     min_default_integer_scalars     ) &
      ,test_description_t("integer(c_int64_t) scalar with no optional arguments",          min_c_int64_scalars             ) &
      ,test_description_t("default integer 1D array elements with no optional arguments",  min_default_integer_1D_array    ) &
      ,test_description_t("default integer 7D array elements with stat argument present",  min_default_integer_7D_array    ) &
      ,test_description_t("default real scalars with stat argument present",               min_default_real_scalars        ) &
      ,test_description_t("double precision 2D array elements with no optional arguments", min_double_precision_2D_array   ) &
      ,test_description_t("elements across images with 2D arrays of strings",              min_elements_in_2D_string_arrays) &
      ,test_description_t("length-5 string with no optional arguments",                    alphabetically_1st_scalar_string) &
    ]
#else
    procedure(diagnosis_function_i), pointer :: & 
       min_default_integer_scalars_ptr      => min_default_integer_scalars      &
      ,min_c_int64_scalars_ptr              => min_c_int64_scalars              &
      ,min_default_integer_1D_array_ptr     => min_default_integer_1D_array     &
      ,min_default_integer_7D_array_ptr     => min_default_integer_7D_array     &
      ,min_default_real_scalars_ptr         => min_default_real_scalars         &
      ,min_double_precision_2D_array_ptr    => min_double_precision_2D_array    &
      ,min_elements_in_2D_string_arrays_ptr => min_elements_in_2D_string_arrays &
      ,alphabetically_1st_scalar_string_ptr => alphabetically_1st_scalar_string 

    test_descriptions = [ &
       test_description_t("default integer scalar with stat argument",                     min_default_integer_scalars_ptr     ) &
      ,test_description_t("integer(c_int64_t) scalar with no optional arguments",          min_c_int64_scalars_ptr             ) &
      ,test_description_t("default integer 1D array elements with no optional arguments",  min_default_integer_1D_array_ptr    ) &
      ,test_description_t("default integer 7D array elements with stat argument present",  min_default_integer_7D_array_ptr    ) &
      ,test_description_t("default real scalars with stat argument present",               min_default_real_scalars_ptr        ) &
      ,test_description_t("double precision 2D array elements with no optional arguments", min_double_precision_2D_array_ptr   ) &
      ,test_description_t("elements across images with 2D arrays of strings",              min_elements_in_2D_string_arrays_ptr) &
      ,test_description_t("length-5 string with no optional arguments",                    alphabetically_1st_scalar_string_ptr) &
    ]
#endif

    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0  &
      .or. test_descriptions%contains_text(test_description_substring))
      
    test_results = test_descriptions%run()
  end function

    function min_default_integer_scalars() result(test_diagnosis)
        type(test_diagnosis_t) test_diagnosis
        integer i, status_, me, num_imgs

        status_ = -1
        call prif_this_image_no_coarray(this_image=me)
        i = -me
        call prif_co_min(i, stat=status_)
        call prif_num_images(num_images=num_imgs)
        test_diagnosis = test_diagnosis_t( &
            test_passed = (i == -num_imgs) .and. (status_ == 0) &
           ,diagnostics_string = "expected i = " // string_t(-num_imgs)  // ", status = 0" &
                              // "; actual i = " // string_t(i)          // ", status = " // string_t(status_) &
        )
    end function

    function min_c_int64_scalars() result(test_diagnosis)
        use iso_c_binding, only : c_int64_t
        type(test_diagnosis_t) test_diagnosis
        integer(c_int64_t) i
        integer me

        call prif_this_image_no_coarray(this_image=me)
        i = int(me, c_int64_t)
        call prif_co_min(i)
        test_diagnosis = test_diagnosis_t( &
            test_passed= i == 1_c_int64_t &
           ,diagnostics_string = "expected i = 1; actual i = " // string_t(int(i)) &
        )
    end function

    function min_default_integer_1D_array() result(test_diagnosis)
        type(test_diagnosis_t) test_diagnosis
        integer i, me, num_imgs
        integer, allocatable :: array(:)

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(num_images=num_imgs)
        associate(sequence_ => me*[(i, i=1, num_imgs)])
          array = sequence_
          call prif_co_min(array)
          associate(min_sequence => [(i, i=1, num_imgs)])
            test_diagnosis = test_diagnosis_t( &
                test_passed = all(min_sequence == array) &
               ,diagnostics_string = "expected " // .csv. string_t(min_sequence) // "; actual = " // .csv. string_t(array) &
            )
          end associate
        end associate
    end function

    function min_default_integer_7D_array() result(test_diagnosis)
        type(test_diagnosis_t) test_diagnosis
        integer, target :: array(2,1,1, 1,1,1, 2)
        integer, pointer :: array_1D_ptr(:)
        integer status_, me, num_imgs

        status_ = -1
        call prif_this_image_no_coarray(this_image=me)
        array = 3 - me
        call prif_co_min(array, stat=status_)
        call prif_num_images(num_images=num_imgs)
        array_1D_ptr(1:size(array)) => array
        test_diagnosis = test_diagnosis_t( &
            test_passed = all(array == 3 - num_imgs) .and. status_ == 0 &
           ,diagnostics_string = "expected element values " //       string_t(3 - num_imgs) // ", status_ = 0" & 
                              // "; actual element values " // .csv. string_t(array_1D_ptr) // ", status_ = " // string_t(status_) &
        )
    end function

    function min_default_real_scalars() result(test_diagnosis)
        type(test_diagnosis_t) test_diagnosis
        real scalar
        real, parameter :: pi = 3.141592654, tolerance = 1E-07
        integer status_, me, num_imgs

        status_ = -1
        call prif_this_image_no_coarray(this_image=me)
        scalar = -pi*me
        call prif_co_min(scalar, stat=status_)
        call prif_num_images(num_images=num_imgs)
        associate(expected_value => -pi*num_imgs)
          test_diagnosis = test_diagnosis_t( &
             test_passed =(abs(scalar - expected_value) < tolerance) .and. (status_ == 0) &
            ,diagnostics_string = "expected " // string_t(expected_value) // ", status = 0" &
                               // "; actual " // string_t(scalar)         // ", status = " // string_t(status_) &
          )
        end associate
    end function

    function min_double_precision_2D_array() result(test_diagnosis)
        type(test_diagnosis_t) test_diagnosis
        double precision, dimension(:,:), allocatable, target :: array, expected
        double precision, dimension(:), pointer :: array_1D_ptr, expected_1D_ptr
        double precision, parameter :: tent(*,*) = dble(reshape(-[0,1,2,3,2,1], [3,2])), tolerance=1D-14
        integer me, num_imgs

        call prif_this_image_no_coarray(this_image=me)
        array = tent*dble(me)
        call prif_co_min(array)
        call prif_num_images(num_images=num_imgs)
        expected = tent*dble(num_imgs)
        array_1D_ptr(1:size(array)) => array
        expected_1D_ptr(1:size(expected)) => expected
        test_diagnosis = test_diagnosis_t( &
           test_passed = all(abs(array - tent*dble(num_imgs)) < tolerance) &
          ,diagnostics_string = "expected " // .csv. string_t(expected_1D_ptr) // "; actual " // .csv. string_t(array_1D_ptr) &
        )
    end function

    function min_elements_in_2D_string_arrays() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis
      character(len=*), parameter :: script(*,*,*) = reshape( &
          [ "To be   ","or not  "   & ! odd images get
          , "to      ","be.     "   & ! this slice: script(:,:,1)
                                      !--------------------------
          , "that    ","is      "   & ! even images get 
          , "the     ","question"], & ! this slice: script(:,:,2)
          [2,2,2])
      character(len=len(script)), dimension(size(script,1),size(script,2)), target :: slice, expected
      character(len=len(script)), dimension(:), pointer :: slice_1D_ptr, expected_1D_ptr

      integer me, ni

      call prif_this_image_no_coarray(this_image=me)
      call prif_num_images(ni)
      slice = script(:,:,mod(me-1,size(script,3))+1)
      call prif_co_min(slice)
      slice_1D_ptr(1:size(slice)) => slice
      expected = minval(script(:,:,1:min(ni,size(script,3))), dim=3)
      expected_1D_ptr(1:size(expected)) => expected
      test_diagnosis = test_diagnosis_t( &
          test_passed = all(expected == slice) &
         ,diagnostics_string = "expected " // .csv. string_t(expected_1D_ptr) // "; actual " // .csv. string_t(slice_1D_ptr) &
      )
    end function

    function alphabetically_1st_scalar_string() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis
      integer, parameter :: length = len("to party!")
      character(len=length), parameter :: words(*) = [character(len=length):: "Loddy","doddy","we","like","to party!"]
      character(len=:), allocatable :: my_word, expected_word
      integer me, num_imgs

      call prif_this_image_no_coarray(this_image=me)
      associate(periodic_index => 1 + mod(me-1,size(words)))
        my_word = words(periodic_index)
        call prif_co_min(my_word)
      end associate

      call prif_num_images(num_images=num_imgs)
      expected_word = minval(words(1:min(num_imgs, size(words)))) ! this line exposes a flang bug
      test_diagnosis = test_diagnosis_t( &
          test_passed = expected_word == my_word &
         ,diagnostics_string = "expected " // expected_word // "; actual " // my_word &
      )
    end function

end module prif_co_min_test_m
