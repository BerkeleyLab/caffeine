module caf_co_reduce_test
  use caffeine_m, only : caf_co_reduce, caf_num_images, caf_this_image
  use vegetables, only : result_t, test_item_t, assert_equals, describe, it, assert_that, assert_equals
  use collective_subroutines_m, only : c_int32_t_operation, c_float_operation, c_char_operation, c_bool_operation
  use assert_m, only : assert
  use iso_c_binding, only : c_bool

  implicit none
  private
  public :: test_caf_co_reduce

contains

  function test_caf_co_reduce() result(tests)
    type(test_item_t) tests
  
    tests = describe( &
      "The caf_co_reduce subroutine", &
      [ it("sums default integer scalars with no optional arguments present", sum_default_integer_scalars) &
       ,it("multiplies default real scalars with all optional arguments present", multiply_default_real_scalars) &
       ,it("alphabetizes length-5 default character scalars with result_image present", alphabetize_default_character_scalars) &
       ,it("reports whether there is consensus across logical scalars", reports_on_consensus) &
    ])
  end function

  function alphabetize_default_character_scalars() result(result_)
    type(result_t) result_
    procedure(c_char_operation), pointer :: alphabetize_operation
    character(len=*), parameter :: names(*) = ["larry","harry","carey","betty","tommy","billy"]
    character(len=:), allocatable :: my_name

    alphabetize_operation => alphabetize

    associate(me => caf_this_image())
      associate(periodic_index => 1 + mod(me-1,size(names)))
        my_name = names(periodic_index)
        call caf_co_reduce(my_name, alphabetize_operation)
      end associate
    end associate

    result_ = assert_equals(minval(names), my_name)

  contains

    pure function alphabetize(lhs, rhs) result(first_alphabetically)
      character(len=*), intent(in) :: lhs, rhs
      character(len=:), allocatable :: first_alphabetically
      call assert(len(lhs)==len(rhs), "co_reduce_s alphabetize: LHS/RHS length match", lhs//" , "//rhs)
      first_alphabetically = min(lhs,rhs)
    end function

  end function

  function sum_default_integer_scalars() result(result_)
    type(result_t) result_
    integer i
    procedure(c_int32_t_operation), pointer :: add_operation

    add_operation => add
    i = 1
    call caf_co_reduce(i, add_operation)
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
    call caf_co_reduce(one_false, boolean_operation)

    one_true = merge(.true., .false., caf_this_image()==1)
    call caf_co_reduce(one_true, boolean_operation)
 
    all_true = .true.
    call caf_co_reduce(all_true, boolean_operation)

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
      call caf_co_reduce(p, multiply_operation, result_image=1, stat=status_, errmsg=error_message)
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
