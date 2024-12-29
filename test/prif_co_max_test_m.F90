! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module prif_co_max_test_m
  !! Unit test for the prif_co_max subroutine
  use iso_c_binding, only: c_size_t, c_ptr, c_intmax_t, c_null_funptr
  use prif, only : prif_co_max, prif_num_images, prif_this_image_no_coarray, prif_num_images
  use prif_test_m, only : prif_test_t, test_description_substring
  use julienne_m, only : test_result_t, test_description_t
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : test_function_i
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
    procedure(test_function_i), pointer :: & 
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

    function max_default_integer_scalars() result(test_passes)
        logical test_passes
        integer i, status_, me, num_imgs

        status_ = -1
        call prif_this_image_no_coarray(this_image=me)
        i = -me
        call prif_co_max(i, stat=status_)
        call prif_num_images(num_images=num_imgs)
        test_passes = i == -num_imgs .and. status_ == 0
    end function

    function max_c_int64_scalars() result(test_passes)
        use iso_c_binding, only : c_int64_t
        logical test_passes
        integer(c_int64_t) i
        integer :: me

        call prif_this_image_no_coarray(this_image=me)
        i = me
        call prif_co_max(i)
        test_passes = int(i) == 1
    end function

    function max_default_integer_1D_array() result(test_passes)
        logical test_passes
        integer i, me, num_imgs
        integer, allocatable :: array(:)

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(num_images=num_imgs)
        associate(sequence_ => me*[(i, i=1, num_imgs)])
          array = sequence_
          call prif_co_max(array)
          associate(min_sequence => [(i, i=1, num_imgs)])
            test_passes = all(min_sequence == array)
          end associate
        end associate
    end function

    function max_default_integer_7D_array() result(test_passes)
        logical test_passes
        integer array(2,1,1, 1,1,1, 2), status_, me, num_imgs

        status_ = -1
        call prif_this_image_no_coarray(this_image=me)
        array = 3 - me
        call prif_co_max(array, stat=status_)
        call prif_num_images(num_images=num_imgs)
        test_passes = all(array == 3 - num_imgs) .and. status_ == 0
    end function

    function max_default_real_scalars() result(test_passes)
        logical test_passes
        real scalar
        real, parameter :: pi = 3.141592654
        integer status_, me, num_imgs

        status_ = -1
        call prif_this_image_no_coarray(this_image=me)
        scalar = -pi*me
        call prif_co_max(scalar, stat=status_)
        call prif_num_images(num_images=num_imgs)
        test_passes = -dble(pi*num_imgs) == dble(scalar) .and. status_ == 0
    end function

    function max_double_precision_2D_array() result(test_passes)
        logical test_passes
        double precision, allocatable :: array(:,:)
        double precision, parameter :: tent(*,*) = dble(reshape(-[0,1,2,3,2,1], [3,2]))
        integer :: me, num_imgs

        call prif_this_image_no_coarray(this_image=me)
        array = tent*dble(me)
        call prif_co_max(array)
        call prif_num_images(num_images=num_imgs)
        test_passes = all(array==tent*num_imgs)
    end function

    function max_elements_in_2D_string_arrays() result(test_passes)
      logical test_passes
      character(len=*), parameter :: script(*) = &
        [character(len=len("the question.")) :: "To be ","or not"," to ","be.","  That is ","the question."]
      character(len=len(script)), dimension(3,2) :: scramlet, co_max_scramlet
      integer i, cyclic_permutation(size(script)), me

      call prif_this_image_no_coarray(this_image=me)
      associate(cyclic_permutation => [(1 + mod(i-1,size(script)), i=me, me+size(script) )])
        scramlet = reshape(script(cyclic_permutation), shape(scramlet))
      end associate

      co_max_scramlet = scramlet
      call prif_co_max(co_max_scramlet, result_image=1)

      block
        integer j, delta_j, num_imgs
        character(len=len(script)) expected_script(size(script)), expected_scramlet(size(scramlet,1),size(scramlet,2))

        call prif_num_images(num_images=num_imgs)
        do j=1, size(script)
          expected_script(j) = script(j)
          do delta_j = 1, min(num_imgs-1, size(script))
            associate(periodic_index => 1 + mod(j+delta_j-1, size(script)))
              expected_script(j) = min(expected_script(j), script(periodic_index))
            end associate
          end do
        end do
        expected_scramlet = reshape(expected_script, shape(scramlet))
        test_passes =  all(scramlet == co_max_scramlet)
      end block

    end function

    function reverse_alphabetize_default_characters() result(test_passes)
      logical test_passes
      integer, parameter :: length = len("to party!")
      character(len=length), parameter :: words(*) = [character(len=length):: "Loddy","doddy","we","like","to party!"]
      character(len=:), allocatable :: my_word, expected_word
      integer :: me, num_imgs

      call prif_this_image_no_coarray(this_image=me)
      associate(periodic_index => 1 + mod(me-1,size(words)))
        my_word = words(periodic_index)
        call prif_co_max(my_word)
      end associate

      call prif_num_images(num_images=num_imgs)
      ! expected_word = minval(words(1:min(num_imgs, size(words)))) ! this line exposes a flang bug
      expected_word = "Loddy"
      test_passes = expected_word == my_word
    end function

end module prif_co_max_test_m
