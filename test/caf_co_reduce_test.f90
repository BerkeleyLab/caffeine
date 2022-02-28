module caf_co_reduce_test
  use caffeine_m, only : caf_co_reduce, caf_num_images, caf_this_image
  use vegetables, only : result_t, test_item_t, assert_equals, describe, it, assert_that, assert_equals
  use collective_subroutines_m, only : &
    c_int32_t_operation, c_float_operation, c_char_operation, c_bool_operation, c_float_complex_operation, c_double_operation
  use assert_m, only : assert
  use iso_c_binding, only : c_bool, c_funloc, c_char, c_double

  implicit none
  private
  public :: test_caf_co_reduce

contains

  function test_caf_co_reduce() result(tests)
    type(test_item_t) tests
  
    tests = describe( &
      "The caf_co_reduce subroutine", &
      [ it("finds the alphabetically first length-5 string with result_image present", alphabetically_1st_size1_string_array) & 
       ,it("sums default integer scalars with no optional arguments present", sum_default_integer_scalars) &
       ,it("multiplies default real scalars with all optional arguments present", multiply_default_real_scalars) &
       ,it("multiplies real(c_double) scalars with all optional arguments present", multiply_c_double_scalars) &
       ,it("performs a collective .and. operation across logical scalars", reports_on_consensus) &
       ,it("sums default complex scalars with a stat-variable present", sum_default_complex_scalars) &
       ,it("sums default integer elements of a 2D array across images", sum_integer_array_elements) &
    ])
  end function

  function alphabetically_1st_size1_string_array() result(result_)
    type(result_t) result_
    procedure(c_char_operation), pointer :: alphabetize_operation
    character(len=*, kind=c_char), parameter :: names(*) = ["larry","harry","carey","betty","tommy","billy"]
    character(len=:, kind=c_char), allocatable :: my_name(:)

    alphabetize_operation => alphabetize

    associate(me => caf_this_image())
      associate(periodic_index => 1 + mod(me-1,size(names)))
        my_name = [names(periodic_index)]
        call caf_co_reduce(my_name, c_funloc(alphabetize_operation))
      end associate
    end associate

    associate(expected_name => minval(names(1:min(caf_num_images(), size(names)))))
      result_ = assert_that(all(expected_name == my_name))
    end associate

  contains

    pure function alphabetize(lhs, rhs) result(first_alphabetically)
      character(len=*), intent(in) :: lhs, rhs
      character(len=:), allocatable :: first_alphabetically
      call assert(len(lhs)==len(rhs), "co_reduce_s alphabetize: LHS/RHS length match", lhs//" , "//rhs)
      first_alphabetically = min(lhs,rhs)
    end function

  end function

  function sum_integer_array_elements() result(result_)
    type(result_t) result_
    integer status_
    integer, parameter :: input_array(*,*) = reshape([1, 2, 3, 4], [2, 2])
    integer array(2,2)
    procedure(c_int32_t_operation), pointer :: add_operation=>null()
    complex, parameter :: array_input=1

    add_operation => add_integers
    array = input_array
    call caf_co_reduce(array, c_funloc(add_operation))
    result_ = assert_that(all(caf_num_images()*input_array==array))

  contains

    pure function add_integers(lhs, rhs) result(total)
      integer, intent(in) :: lhs, rhs
      integer total
      total = lhs + rhs 
    end function

  end function

  function sum_default_complex_scalars() result(result_)
    type(result_t) result_
    integer status_
    complex z
    procedure(c_float_complex_operation), pointer :: add_operation
    complex, parameter :: z_input=(1.,1.)

    add_operation => add_complex
    z = z_input
    call caf_co_reduce(z, c_funloc(add_operation), stat=status_)
    result_ = assert_equals(dble(caf_num_images()*z_input), dble(z)) .and. assert_equals(0, status_)

  contains

    pure function add_complex(lhs, rhs) result(total)
      complex, intent(in) :: lhs, rhs
      complex total
      total = lhs + rhs 
    end function

  end function

  function sum_default_integer_scalars() result(result_)
    type(result_t) result_
    integer i
    procedure(c_int32_t_operation), pointer :: add_operation

    add_operation => add
    i = 1
    call caf_co_reduce(i, c_funloc(add_operation))
    result_ = assert_equals(caf_num_images(), i)

  contains

    pure function add(lhs, rhs) result(total)
      integer, intent(in) :: lhs, rhs
      integer total
      total = lhs + rhs 
    end function

  end function

  function reports_on_consensus() result(result_)
    type(result_t) result_
    logical(c_bool) one_false, one_true, all_true
    procedure(c_bool_operation), pointer :: boolean_operation

    boolean_operation => logical_and

    one_false = merge(.false., .true., caf_this_image()==1)
    call caf_co_reduce(one_false, c_funloc(boolean_operation))

    one_true = merge(.true., .false., caf_this_image()==1)
    call caf_co_reduce(one_true, c_funloc(boolean_operation))
 
    all_true = .true.
    call caf_co_reduce(all_true, c_funloc(boolean_operation))

    result_ = assert_that(one_false .eqv. .false.) .and. &
              assert_that(one_true .eqv. merge(.true.,.false.,caf_num_images()==1)) .and. &
              assert_that(all_true .eqv. .true.)
  contains

    pure function logical_and(lhs, rhs) result(lhs_and_rhs)
      logical(c_bool), intent(in) :: lhs, rhs
      logical(c_bool) lhs_and_rhs
      lhs_and_rhs = lhs .and. rhs 
    end function

  end function

  function multiply_c_double_scalars() result(result_)
    type(result_t) result_
    real(c_double) p
    integer j, status_
    character(len=:), allocatable :: error_message
    procedure(c_double_operation), pointer :: multiply_operation

    error_message = "unused"
    multiply_operation => multiply_doubles
    associate(me => caf_this_image())
      p = real(me,c_double)
      call caf_co_reduce(p, c_funloc(multiply_operation), result_image=1, stat=status_, errmsg=error_message)
      associate(expected_result => merge( product([(real(j,c_double), j = 1, caf_num_images())]), real(me,c_double), me==1 ))
        result_ = &
          assert_equals(expected_result, real(p,c_double)) .and. &
          assert_equals(0, status_) .and. &
          assert_equals("unused", error_message)
      end associate
    end associate

  contains

    pure function multiply_doubles(lhs, rhs) result(product_)
      real(c_double), intent(in) :: lhs, rhs
      real(c_double) product_
      product_ = lhs * rhs 
    end function

  end function

  function multiply_default_real_scalars() result(result_)
    type(result_t) result_
    real p
    integer j, status_
    character(len=:), allocatable :: error_message
    procedure(c_float_operation), pointer :: multiply_operation

    error_message = "unused"
    multiply_operation => multiply
    associate(me => caf_this_image())
      p = real(me)
      call caf_co_reduce(p, c_funloc(multiply_operation), result_image=1, stat=status_, errmsg=error_message)
      associate(expected_result => merge( product([(dble(j), j = 1, caf_num_images())]), dble(me), me==1 ))
        result_ = &
          assert_equals(expected_result, dble(p)) .and. &
          assert_equals(0, status_) .and. &
          assert_equals("unused", error_message)
      end associate
    end associate

  contains

    pure function multiply(lhs, rhs) result(product_)
      real, intent(in) :: lhs, rhs
      real product_
      product_ = lhs * rhs 
    end function

  end function
end module caf_co_reduce_test
