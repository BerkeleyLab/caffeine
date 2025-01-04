! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module prif_co_max_test_m
  !! Unit test for the prif_co_max subroutine
  use iso_c_binding, only: c_size_t, c_ptr, c_intmax_t, c_null_funptr
  use prif, only : prif_co_max, prif_num_images, prif_this_image_no_coarray, prif_num_images
  use prif_test_m, only : prif_test_t, test_description_substring
  use julienne_m, only : test_result_t, test_description_t, test_diagnosis_t, string_t, operator(.csv.)
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : diagnosis_function_i
#endif
  implicit none

  private
  public :: prif_co_max_test_t

  type, extends(prif_test_t) :: prif_co_max_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The prif_co_max subroutine global maximum computation"
          
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ &
       test_description_t("default-integer scalars with stat argument",             max_default_integer_scalars           ) &
      ,test_description_t("integer(c_int64_t) scalars with no optional arguments",  max_c_int64_scalars                   ) &
      ,test_description_t("default-integer 1D arrays with no optional arguments",   max_default_integer_1D_array          ) &
      ,test_description_t("default-integer 7D arrays with stat argument",           max_default_integer_7D_array          ) &
      ,test_description_t("default-real scalars with stat argument present",        max_default_real_scalars              ) &
      ,test_description_t("double-precision 2D arrays  with no optional arguments", max_double_precision_2D_array         ) &
      ,test_description_t("elements across images with 2D arrays of strings",       max_elements_in_2D_string_arrays      ) &
      ,test_description_t("default-character variables with no optional arguments", reverse_alphabetize_default_characters) &
    ]
#else
    procedure(diagnosis_function_i), pointer :: & 
       max_default_integer_scalars_ptr            =>  max_default_integer_scalars            &
      ,max_c_int64_scalars_ptr                    =>  max_c_int64_scalars                    &
      ,max_default_integer_1D_array_ptr           =>  max_default_integer_1D_array           &
      ,max_default_integer_7D_array_ptr           =>  max_default_integer_7D_array           &
      ,max_default_real_scalars_ptr               =>  max_default_real_scalars               &
      ,max_double_precision_2D_array_ptr          =>  max_double_precision_2D_array          &
      ,max_elements_in_2D_string_arrays_ptr       =>  max_elements_in_2D_string_arrays       &
      ,reverse_alphabetize_default_characters_ptr =>  reverse_alphabetize_default_characters

    test_descriptions = [ &
       test_description_t("default-integer scalars with stat argument",             max_default_integer_scalars_ptr           ) &
      ,test_description_t("integer(c_int64_t) scalars with no optional arguments",  max_c_int64_scalars_ptr                   ) &
      ,test_description_t("default-integer 1D arrays with no optional arguments",   max_default_integer_1D_array_ptr          ) &
      ,test_description_t("default-integer 7D arrays with stat argument",           max_default_integer_7D_array_ptr          ) &
      ,test_description_t("default-real scalars with stat argument present",        max_default_real_scalars_ptr              ) &
      ,test_description_t("double-precision 2D arrays  with no optional arguments", max_double_precision_2D_array_ptr         ) &
      ,test_description_t("elements across images with 2D arrays of strings",       max_elements_in_2D_string_arrays_ptr      ) &
      ,test_description_t("default-character variables with no optional arguments", reverse_alphabetize_default_characters_ptr) &
    ]
#endif

    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0  &
      .or. test_descriptions%contains_text(test_description_substring))
      
    test_results = test_descriptions%run()
  end function

    function max_default_integer_scalars() result(test_diagnosis)
        type(test_diagnosis_t) test_diagnosis
        integer i, status_, n

        status_ = -1
        call prif_this_image_no_coarray(this_image=i)
        call prif_co_max(i, stat=status_)
        call prif_num_images(num_images=n)
        test_diagnosis = test_diagnosis_t( &
            test_passed = (i==n) .and. (status_==0) &
           ,diagnostics_string = "expected i = " // string_t(i) // ", status_ = 0" &
                             // "; actual i = " // string_t(n) // ", status_ = " // string_t(status_) &
        )
    end function

    function max_c_int64_scalars() result(test_diagnosis)
        use iso_c_binding, only : c_int64_t
        type(test_diagnosis_t) test_diagnosis
        integer(c_int64_t) i
        integer me, status_, n

        status_ = -1
        call prif_this_image_no_coarray(this_image=me)
        i = me
        call prif_co_max(i, stat=status_)
        call prif_num_images(num_images=n)
        test_diagnosis = test_diagnosis_t( &
           test_passed = (i == int(n,c_int64_t)) .and. (status_ == 0) &
          ,diagnostics_string =  "expected i = " // string_t(int(i)) // ", status = 0" &
                             // "; actual i = " // string_t(n)      // ", status = " // string_t(status_) &
        )
    end function

    function max_default_integer_1D_array() result(test_diagnosis)
        type(test_diagnosis_t) test_diagnosis
        integer i, me, n
        integer, allocatable :: array(:)

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(num_images=n)
        associate(sequence_ => me*[(i, i=1, n)])
          array = sequence_
          call prif_co_max(array)
          associate(max_sequence => n*[(i, i=1, n)])
            test_diagnosis = test_diagnosis_t( &
              test_passed = all(max_sequence == array), &
              diagnostics_string = "expected element values " // .csv. string_t(max_sequence) &
                                // "; actual element values " // .csv. string_t(array) &
            )
          end associate
        end associate
    end function

    function max_default_integer_7D_array() result(test_diagnosis)
        type(test_diagnosis_t) test_diagnosis
        integer, target :: array(2,1,1, 1,1,1, 2)
        integer, pointer :: array_1D_ptr(:)
        integer status_, me, n

        status_ = -1
        call prif_this_image_no_coarray(this_image=me)
        array = 3 - me
        call prif_co_max(array, stat=status_)
        call prif_num_images(num_images=n)
        array_1D_ptr(1:size(array)) => array
        test_diagnosis = test_diagnosis_t( &
           test_passed = all(array == 3 - 1) .and. status_ == 0 &
          ,diagnostics_string = "expected element values " // string_t(3 - 1) &
                            // "; actual element values " // .csv. string_t(array_1D_ptr) &
        )
    end function

    function max_default_real_scalars() result(test_diagnosis)
        type(test_diagnosis_t) test_diagnosis
        real scalar
        real, parameter :: pi = 3.141592654, tolerance = 1E-7, expected = -pi
        integer status_, me, n

        status_ = -1
        call prif_this_image_no_coarray(this_image=me)
        scalar = -pi*me
        call prif_co_max(scalar, stat=status_)
        call prif_num_images(num_images=n)
        test_diagnosis = test_diagnosis_t( &
           test_passed = (abs(scalar-expected) < tolerance) .and. (status_ == 0) &
          ,diagnostics_string = "expected scalar " // string_t(expected) // ", status = 0" &
                             // "; actual scalar " // string_t(scalar) // ", status = " // string_t(status_) &
        )
    end function

    function max_double_precision_2D_array() result(test_diagnosis)
        type(test_diagnosis_t) test_diagnosis
        double precision, dimension(:,:), allocatable, target :: array, expected
        double precision, dimension(:), pointer :: array_1D_ptr, expected_1D_ptr
        double precision, parameter :: tent(*,*) = dble(reshape(-[0,1,2,3,2,1], [3,2])), tolerance = 1D-14
        integer :: me, n

        call prif_this_image_no_coarray(this_image=me)
        array = tent*dble(me)
        call prif_co_max(array)
        call prif_num_images(num_images=n)
        expected = tent*dble(1)
        array_1D_ptr(1:size(array)) => array
        expected_1D_ptr(1:size(expected)) => expected
        test_diagnosis = test_diagnosis_t( &
          test_passed = all(abs(array-expected) < tolerance) &
          ,diagnostics_string = "expected element values " // .csv. string_t(expected_1D_ptr) &
                             // "; actual element values " // .csv. string_t(array_1D_ptr) &
        )
    end function

    function max_elements_in_2D_string_arrays() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis
      character(len=*), parameter :: script(*,*,*) = reshape( &
          [ "To be   ","or not  "   & ! odd images get
          , "to      ","be.     "   & ! this slice: script(:,:,1)
                                      !--------------------------
          , "that    ","is      "   & ! even images get 
          , "the     ","question"], & ! this slice: script(:,:,2)
          [2,2,2])
      character(len=len(script)), dimension(size(script,1),size(script,2)), target :: slice, expected
      character(len=len(script)), dimension(:), pointer ::  slice_1D_ptr, expected_1D_ptr
      integer me, ni

      call prif_this_image_no_coarray(this_image=me)
      call prif_num_images(ni)
      slice = script(:,:,mod(me-1,size(script,3))+1)
      call prif_co_max(slice)
      expected = maxval(script(:,:,1:min(ni,size(script,3))), dim=3)
      expected_1D_ptr(1:size(expected)) => expected
      slice_1D_ptr(1:size(slice)) => slice
      test_diagnosis = test_diagnosis_t( &
        test_passed = all(expected == slice) &
        ,diagnostics_string = "expected slice " // .csv. string_t(expected_1D_ptr) &
                           // "; actual slice " // .csv. string_t(slice_1D_ptr) &
      )
    end function

    function reverse_alphabetize_default_characters() result(test_diagnosis)
      type(test_diagnosis_t) test_diagnosis
      integer, parameter :: length = len("to party!")
      character(len=length), parameter :: words(*) = [character(len=length):: "Loddy","doddy","we","like","to party!"]
      character(len=:), allocatable :: my_word, expected_word
      integer :: me, n

      call prif_this_image_no_coarray(this_image=me)
      associate(periodic_index => 1 + mod(me-1,size(words)))
        my_word = words(periodic_index)
        call prif_co_max(my_word)
      end associate

      call prif_num_images(num_images=n)
      expected_word = maxval(words(1:min(n, size(words)))) ! this line exposes a flang bug
      test_diagnosis = test_diagnosis_t( &
         test_passed = expected_word == my_word &
        ,diagnostics_string = "expected " // expected_word // "; actual " // my_word &
      )
    end function

end module prif_co_max_test_m