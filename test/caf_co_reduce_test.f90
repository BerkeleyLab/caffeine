module caf_co_reduce_test
  use prif, only : prif_co_reduce, prif_num_images, prif_this_image
  use veggies, only : result_t, test_item_t, assert_equals, describe, it, assert_that, assert_equals
  use collective_subroutines_m, only : &
     c_int32_t_operation, c_int64_t_operation, c_float_operation, c_double_operation, c_char_operation, c_bool_operation &
    ,c_float_complex_operation, c_double_complex_operation
  use caffeine_assert_m, only : assert
  use iso_c_binding, only : c_bool, c_funloc, c_char, c_double, c_int64_t

  implicit none
  private
  public :: test_prif_co_reduce

contains

  function test_prif_co_reduce() result(tests)
    type(test_item_t) tests

    tests = describe( &
      "The prif_co_reduce subroutine", &
      [ it("finds the alphabetically first length-5 string with result_image present", alphabetically_1st_size1_string_array) &
       ,it("sums default integer scalars with no optional arguments present", sum_default_integer_scalars) &
       ,it("sums integer(c_int64_t) scalars with no optional arguments present", sum_c_int64_t_scalars) &
       ,it("multiplies default real scalars with all optional arguments present", multiply_default_real_scalars) &
       ,it("multiplies real(c_double) scalars with all optional arguments present", multiply_c_double_scalars) &
       ,it("performs a collective .and. operation across logical scalars", reports_on_consensus) &
       ,it("sums default complex scalars with a stat-variable present", sum_default_complex_scalars) &
       ,it("sums complex(c_double) scalars with a stat-variable present", sum_complex_c_double_scalars) &
       ,it("sums default integer elements of a 2D array across images", sum_integer_array_elements) &
    ])
  end function

  function alphabetically_1st_size1_string_array() result(result_)
    type(result_t) result_
    procedure(c_char_operation), pointer :: alphabetize_operation
    character(len=*, kind=c_char), parameter :: names(*) = ["larry","harry","carey","betty","tommy","billy"]
    character(len=:, kind=c_char), allocatable :: my_name(:)
    integer :: me, num_imgs

    alphabetize_operation => alphabetize

    call prif_this_image(image_index=me)
    associate(periodic_index => 1 + mod(me-1,size(names)))
      my_name = [names(periodic_index)]
      call prif_co_reduce(my_name, c_funloc(alphabetize_operation))
    end associate

    call prif_num_images(image_count=num_imgs)
    associate(expected_name => minval(names(1:min(num_imgs, size(names)))))
      result_ = assert_that(all(expected_name == my_name))
    end associate

  contains

    function alphabetize(lhs, rhs) result(first_alphabetically)
      character(len=*), intent(in) :: lhs, rhs
      character(len=:), allocatable :: first_alphabetically
      call assert(len(lhs)==len(rhs), "co_reduce_s alphabetize: LHS/RHS length match", lhs//" , "//rhs)
      first_alphabetically = min(lhs,rhs)
    end function

  end function

  function sum_integer_array_elements() result(result_)
    type(result_t) result_
    integer status_, num_imgs
    integer, parameter :: input_array(*,*) = reshape([1, 2, 3, 4], [2, 2])
    integer array(2,2)
    procedure(c_int32_t_operation), pointer :: add_operation=>null()

    add_operation => add_integers
    array = input_array
    call prif_co_reduce(array, c_funloc(add_operation))
    call prif_num_images(image_count=num_imgs)
    result_ = assert_that(all(num_imgs*input_array==array))

  contains

    pure function add_integers(lhs, rhs) result(total)
      integer, intent(in) :: lhs, rhs
      integer total
      total = lhs + rhs
    end function

  end function

  function sum_complex_c_double_scalars() result(result_)
    type(result_t) result_
    integer status_, num_imgs
    complex(c_double) z
    procedure(c_double_complex_operation), pointer :: add_operation
    complex(c_double), parameter :: z_input=(1._c_double, 1._c_double)

    add_operation => add_complex
    z = z_input
    call prif_co_reduce(z, c_funloc(add_operation), stat=status_)
    call prif_num_images(image_count=num_imgs)
    result_ = assert_equals(real(num_imgs*z_input, c_double), real(z, c_double)) .and. assert_equals(0, status_)

  contains

    pure function add_complex(lhs, rhs) result(total)
      complex(c_double), intent(in) :: lhs, rhs
      complex(c_double) total
      total = lhs + rhs
    end function

  end function

  function sum_default_complex_scalars() result(result_)
    type(result_t) result_
    integer status_, num_imgs
    complex z
    procedure(c_float_complex_operation), pointer :: add_operation
    complex, parameter :: z_input=(1.,1.)

    add_operation => add_complex
    z = z_input
    call prif_co_reduce(z, c_funloc(add_operation), stat=status_)
    call prif_num_images(image_count=num_imgs)
    result_ = assert_equals(dble(num_imgs*z_input), dble(z)) .and. assert_equals(0, status_)

  contains

    pure function add_complex(lhs, rhs) result(total)
      complex, intent(in) :: lhs, rhs
      complex total
      total = lhs + rhs
    end function

  end function

  function sum_default_integer_scalars() result(result_)
    type(result_t) result_
    integer i, num_imgs
    procedure(c_int32_t_operation), pointer :: add_operation

    add_operation => add
    i = 1
    call prif_co_reduce(i, c_funloc(add_operation))
    call prif_num_images(image_count=num_imgs)
    result_ = assert_equals(num_imgs, i)

  contains

    pure function add(lhs, rhs) result(total)
      integer, intent(in) :: lhs, rhs
      integer total
      total = lhs + rhs
    end function

  end function

  function sum_c_int64_t_scalars() result(result_)
    type(result_t) result_
    integer(c_int64_t) i
    integer :: num_imgs
    procedure(c_int64_t_operation), pointer :: add_operation

    add_operation => add
    i = 1_c_int64_t
    call prif_co_reduce(i, c_funloc(add_operation))
    call prif_num_images(image_count=num_imgs)
    result_ = assert_that(int(num_imgs, c_int64_t)==i)

  contains

    pure function add(lhs, rhs) result(total)
      integer(c_int64_t), intent(in) :: lhs, rhs
      integer(c_int64_t) total
      total = lhs + rhs
    end function

  end function

  function reports_on_consensus() result(result_)
    type(result_t) result_
    logical(c_bool) one_false, one_true, all_true
    procedure(c_bool_operation), pointer :: boolean_operation
    logical(c_bool), parameter :: c_true=.true._c_bool, c_false=.false._c_bool
    logical ans1, ans2, ans3
    integer :: me, num_imgs

    boolean_operation => logical_and

    call prif_this_image(image_index=me)
    one_false = merge(c_false, c_true, me==1)
    call prif_co_reduce(one_false, c_funloc(boolean_operation))

    call prif_this_image(image_index=me)
    one_true = merge(c_true, c_false, me==1)
    call prif_co_reduce(one_true, c_funloc(boolean_operation))

    all_true = c_true
    call prif_co_reduce(all_true, c_funloc(boolean_operation))
    call prif_num_images(image_count=num_imgs)

    ans1 = one_false .eqv. c_false
    ans2 = one_true  .eqv. merge(c_true,c_false,num_imgs==1)
    ans3 = all_true  .eqv. c_true
    result_ = assert_that(ans1) .and. &
              assert_that(ans2) .and. &
              assert_that(ans3)
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
    integer j, status_, me, num_imgs
    character(len=:), allocatable :: error_message
    procedure(c_double_operation), pointer :: multiply_operation

    error_message = "unused"
    multiply_operation => multiply_doubles
    call prif_this_image(image_index=me)
    p = real(me,c_double)
    call prif_co_reduce(p, c_funloc(multiply_operation), result_image=1, stat=status_, errmsg=error_message)
    call prif_num_images(image_count=num_imgs)
    associate(expected_result => merge( product([(real(j,c_double), j = 1, num_imgs)]), real(me,c_double), me==1 ))
      result_ = &
        assert_equals(expected_result, real(p,c_double)) .and. &
        assert_equals(0, status_) .and. &
        assert_equals("unused", error_message)
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
    integer j, status_, me, num_imgs
    character(len=:), allocatable :: error_message
    procedure(c_float_operation), pointer :: multiply_operation

    error_message = "unused"
    multiply_operation => multiply
    call prif_this_image(image_index=me)
    p = real(me)
    call prif_co_reduce(p, c_funloc(multiply_operation), result_image=1, stat=status_, errmsg=error_message)
    call prif_num_images(image_count=num_imgs)
    associate(expected_result => merge( product([(dble(j), j = 1, num_imgs)]), dble(me), me==1 ))
      result_ = &
        assert_equals(expected_result, dble(p)) .and. &
        assert_equals(0, status_) .and. &
        assert_equals("unused", error_message)
    end associate

  contains

    pure function multiply(lhs, rhs) result(product_)
      real, intent(in) :: lhs, rhs
      real product_
      product_ = lhs * rhs
    end function

  end function
end module caf_co_reduce_test
