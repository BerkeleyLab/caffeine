! Copyright (c) 2022-2024, The Regents of the University of California and Sourcery Institute
! Terms of use are as specified in LICENSE.txt

#include "language-support.F90"

module prif_co_reduce_test_m
  !! Unit test fort the prif_init program inititation subroutine
  use prif, only : prif_co_reduce, prif_num_images, prif_this_image_no_coarray, prif_error_stop, prif_co_max
  use prif_test_m, only : prif_test_t, test_description_substring
  use iso_c_binding, only : c_bool, c_funloc, c_char, c_double, c_int64_t
  use julienne_m, only : test_result_t, test_description_t, test_diagnosis_t, string_t, operator(.csv.)
#if ! HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  use julienne_m, only : diagnosis_function_i
#endif
  implicit none

  private
  public :: prif_co_reduce_test_t

  type, extends(prif_test_t) :: prif_co_reduce_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The prif_co_reduce subroutine"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#if HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
    test_descriptions = [ & 
       test_description_t("sums default integer scalars with no optional arguments",       sum_default_integer_scalars) &
      ,test_description_t("sums integer(c_int64_t) scalars with no optional arguments",    sum_c_int64_t_scalars) &
      ,test_description_t("multiplies default real scalars with all optional arguments",   multiply_default_real_scalars) &
      ,test_description_t("multiplies real(c_double) scalars with all optional arguments", multiply_c_double_scalars) &
      ,test_description_t("performs a collective .and. operation across logical scalars",  reports_on_consensus) &
      ,test_description_t("sums default complex scalars with a stat-variable present",     sum_default_complex_scalars) &
      ,test_description_t("sums complex(c_double) scalars with a stat-variable present",   sum_complex_c_double_scalars) &
      ,test_description_t("sums default integer elements of a 2D array across images",     sum_integer_array_elements) &
      ,test_description_t("alphabetical reduction of strings with result_image present",   alphabetically_first_string) &
    ]   
#else
    procedure(diagnosis_function_i), pointer :: &
      alphabetically_first_string_ptr    => alphabetically_first_string   &
     ,sum_default_integer_scalars_ptr    => sum_default_integer_scalars   &
     ,sum_c_int64_t_scalars_ptr          => sum_c_int64_t_scalars         &
     ,multiply_default_real_scalars_ptr  => multiply_default_real_scalars &
     ,multiply_c_double_scalars_ptr      => multiply_c_double_scalars     &
     ,reports_on_consensus_ptr           => reports_on_consensus          &
     ,sum_default_complex_scalars_ptr    => sum_default_complex_scalars   &
     ,sum_complex_c_double_scalars_ptr   => sum_complex_c_double_scalars  &
     ,sum_integer_array_elements_ptr     => sum_integer_array_elements

    test_descriptions = [ & 
       test_description_t("sums default integer scalars with no optional arguments",       sum_default_integer_scalars_ptr) &
      ,test_description_t("sums integer(c_int64_t) scalars with no optional arguments",    sum_c_int64_t_scalars_ptr) &
      ,test_description_t("multiplies default real scalars with all optional arguments",   multiply_default_real_scalars_ptr) &
      ,test_description_t("multiplies real(c_double) scalars with all optional arguments", multiply_c_double_scalars_ptr) &
      ,test_description_t("performs a collective .and. operation across logical scalars",  reports_on_consensus_ptr) &
      ,test_description_t("sums default complex scalars with a stat-variable present",     sum_default_complex_scalars_ptr) &
      ,test_description_t("sums complex(c_double) scalars with a stat-variable present",   sum_complex_c_double_scalars_ptr) &
      ,test_description_t("sums default integer elements of a 2D array across images",     sum_integer_array_elements_ptr) &
      ,test_description_t("alphabetical reduction of strings with result_image present",   alphabetically_first_string_ptr) &
    ]
#endif

    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0  &
      .or. test_descriptions%contains_text(test_description_substring))
      
    test_results = test_descriptions%run()
  end function

  function alphabetically_first_string() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    character(len=*, kind=c_char), parameter :: names(*) = ["larry","harry","carey","betty","tommy","billy"]
    character(len=len(names), kind=c_char) my_name, expected_name
    integer :: me, n

    call prif_this_image_no_coarray(this_image=me)
    associate(periodic_index => 1 + mod(me-1,size(names)))
      my_name = names(periodic_index)
      call prif_co_reduce(my_name, c_funloc(alphabetize))
    end associate

    call prif_num_images(num_images=n)
    expected_name = minval(names(1:min(n, size(names)))) ! this exposes a flang bug
    test_diagnosis = test_diagnosis_t( &
      test_passed = expected_name == my_name &
     ,diagnostics_string = "expected: " // expected_name // ", got: " // my_name &
    )    
  contains

    function alphabetize(lhs, rhs) result(first_alphabetically)
      character(len=*), intent(in) :: lhs, rhs
      character(len=len(lhs)) first_alphabetically
      if (len(lhs) /= len(rhs)) call prif_error_stop(quiet=.false._c_bool, stop_code_char="argument size mismatchin in function alphabetize")
      first_alphabetically = min(lhs,rhs)
    end function

  end function

  function sum_integer_array_elements() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer status_, num_imgs
    integer, dimension(*,*), parameter :: input_array = reshape([1, 2, 3, 4], [2, 2])
    integer, dimension(2,2), target :: array, expected_array
    integer, dimension(:), pointer :: expected_array_1D_ptr, array_1D_ptr

    array = input_array
    call prif_co_reduce(array, c_funloc(add_integers))
    call prif_num_images(num_images=num_imgs)
    expected_array = num_imgs*input_array
    expected_array_1D_ptr(1:size(input_array)) => expected_array
    array_1D_ptr(1:size(array)) => array
    test_diagnosis = test_diagnosis_t( &
      test_passed = all(array==expected_array) &
     ,diagnostics_string = "expected " // .csv. string_t(expected_array_1D_ptr) // ", actual " // .csv. string_t(array_1D_ptr) &
    )

  contains

    pure function add_integers(lhs, rhs) result(total)
      integer, intent(in) :: lhs, rhs
      integer total
      total = lhs + rhs
    end function

  end function

  function sum_complex_c_double_scalars() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer status_, num_imgs
    complex(c_double) z
    complex(c_double), parameter :: z_input=(1._c_double, 1._c_double)
    real(c_double), parameter :: tolerance = 1.e-14_c_double

    z = z_input
    call prif_co_reduce(z, c_funloc(add_complex), stat=status_)
    call prif_num_images(num_images=num_imgs)

    associate(expected => num_imgs*z_input)
      test_diagnosis = test_diagnosis_t( &
        test_passed = (abs(num_imgs*z_input - z) < tolerance) .and. (status_ == 0)&
       ,diagnostics_string = &
             "expected " // string_t(expected) // " and status_= 0" &
          // "; actual " // string_t(z)        // " and status_= " // string_t(status_) &
      )
    end associate

  contains

    pure function add_complex(lhs, rhs) result(total)
      complex(c_double), intent(in) :: lhs, rhs
      complex(c_double) total
      total = lhs + rhs
    end function

  end function

  function sum_default_complex_scalars() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer status_, num_imgs
    complex z
    complex, parameter :: z_input=(1.,1.)
    real, parameter :: tolerance = 1E-07

    z = z_input
    call prif_co_reduce(z, c_funloc(add_complex), stat=status_)
    call prif_num_images(num_images=num_imgs)

    associate(expected => num_imgs*z_input)
      test_diagnosis = test_diagnosis_t( &
        test_passed = abs(z - expected) < tolerance .and. status_ == 0 &
       ,diagnostics_string = "expected " // string_t(expected) // " and status_= 0" &
                         // "; actual " // string_t(       z) // " and status_= " // string_t(status_) &
      )
    end associate

  contains

    pure function add_complex(lhs, rhs) result(total)
      complex, intent(in) :: lhs, rhs
      complex total
      total = lhs + rhs
    end function

  end function

  function sum_default_integer_scalars() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer i, num_imgs

    i = 1
    call prif_co_reduce(i, c_funloc(add))
    call prif_num_images(num_images=num_imgs)

    test_diagnosis = test_diagnosis_t( &
      test_passed = i == num_imgs &
     ,diagnostics_string = "expected " // string_t(num_imgs) // ", actual " // string_t(i) &
    )
  contains

    pure function add(lhs, rhs) result(total)
      integer, intent(in) :: lhs, rhs
      integer total
      total = lhs + rhs
    end function

  end function

  function sum_c_int64_t_scalars() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    integer(c_int64_t) i
    integer :: num_imgs

    i = 1_c_int64_t
    call prif_co_reduce(i, c_funloc(add))
    call prif_num_images(num_images=num_imgs)

    associate(expected => int(num_imgs, c_int64_t))
      test_diagnosis = test_diagnosis_t( &
         test_passed = i == int(num_imgs,c_int64_t) &
        ,diagnostics_string = "expected " // string_t(int(expected)) // ", actual " // string_t(int(i)) &
      )
    end associate

  contains

    pure function add(lhs, rhs) result(total)
      integer(c_int64_t), intent(in) :: lhs, rhs
      integer(c_int64_t) total
      total = lhs + rhs
    end function

  end function

  function reports_on_consensus() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    logical(c_bool) one_false, one_true, all_true
    logical(c_bool), parameter :: c_true=.true._c_bool, c_false=.false._c_bool
    character(len=1), parameter :: c_true_char='T', c_false_char='F'
    logical ans1, ans2, ans3
    integer :: me, num_imgs

    call prif_this_image_no_coarray(this_image=me)
    one_false = merge(c_false, c_true, me==1)
    call prif_co_reduce(one_false, c_funloc(logical_and))

    call prif_this_image_no_coarray(this_image=me)
    one_true = merge(c_true, c_false, me==1)
    call prif_co_reduce(one_true, c_funloc(logical_and))

    all_true = c_true
    call prif_co_reduce(all_true, c_funloc(logical_and))
    call prif_num_images(num_images=num_imgs)

    associate(expected_one_true => merge(c_true,c_false,num_imgs==1))

      ans1 = one_false .eqv. c_false
      ans2 = one_true  .eqv. expected_one_true
      ans3 = all_true  .eqv. c_true

      test_diagnosis = test_diagnosis_t( &
         test_passed = ans1 .and. ans2 .and. ans3 &
        ,diagnostics_string = &
          "expected one_false " //       "F"           // ", one_true "  //        "T"        // ", all_true " //      "T"       //&
          "; actual one_false " // string_t(one_false) // ", one_true " // string_t(one_true) // ", all_true " //string_t(all_true)&
      )
    end associate
                  
  contains

    pure function logical_and(lhs, rhs) result(lhs_and_rhs)
      logical(c_bool), intent(in) :: lhs, rhs
      logical(c_bool) lhs_and_rhs
      lhs_and_rhs = lhs .and. rhs
    end function

  end function

  function multiply_c_double_scalars() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    real(c_double) p
    real(c_double), parameter :: tolerance = 1D-14
    integer j, status_, me, num_imgs
    character(len=:), allocatable :: error_message

    error_message = "unused"
    call prif_this_image_no_coarray(this_image=me)
    p = real(me,c_double)
    call prif_co_reduce(p, c_funloc(multiply_doubles), result_image=1, stat=status_, errmsg=error_message)
    call prif_num_images(num_images=num_imgs)
    associate(expected_result => merge( product([(real(j,c_double), j = 1, num_imgs)]), real(me,c_double), me==1 ))
      test_diagnosis = test_diagnosis_t( &
         test_passed = abs(expected_result - real(p,c_double)) < tolerance  .and. (0 == status_) .and. ("unused" == error_message) &
        ,diagnostics_string = "expected " // string_t(expected_result) // " and status_= 0 "                   &
                          // "; actual " // string_t(p)               // " and status_= " // string_t(status_) &
      )
    end associate

  contains

    pure function multiply_doubles(lhs, rhs) result(product_)
      real(c_double), intent(in) :: lhs, rhs
      real(c_double) product_
      product_ = lhs * rhs
    end function

  end function

  function multiply_default_real_scalars() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    real p
    real, parameter :: tolerance = 1E-07
    integer j, status_, me, num_imgs
    character(len=:), allocatable :: error_message

    error_message = "unused"
    call prif_this_image_no_coarray(this_image=me)
    p = real(me)
    call prif_co_reduce(p, c_funloc(multiply), result_image=1, stat=status_, errmsg=error_message)
    call prif_num_images(num_images=num_imgs)
    associate(expected => merge( product([(dble(j), j = 1, num_imgs)]), dble(me), me==1 ))
      test_diagnosis = test_diagnosis_t( &
         test_passed = abs(expected - p) < tolerance .and. (0 == status_) .and. ("unused" == error_message)   &
        ,diagnostics_string = "expected " // string_t(expected) // " and status_=0"                    &
                          // "; actual " // string_t(p)         // " and status_=" // string_t(status_) &
      )
    end associate

  contains

    pure function multiply(lhs, rhs) result(product_)
      real, intent(in) :: lhs, rhs
      real product_
      product_ = lhs * rhs
    end function

  end function

end module prif_co_reduce_test_m