module prif_simple_test_case_m
    use forgex, only: operator(.in.)
    use iso_varying_string, only: &
            varying_string, operator(//), char, put_line, var_str
    use prif, only: prif_this_image_no_coarray, prif_num_images
    use strff, only: to_string
    use veggies_command_line_m, only: DEBUG
    use veggies_input_m, only: input_t
    use veggies_test_m, only: &
            filter_result_t, test_t, filter_failed, filter_matched
    use veggies_test_case_result_m, only: test_case_result_t
    use veggies_test_interfaces_m, only: computation_i, simple_test_i
    use veggies_test_result_item_m, only: test_result_item_t

    implicit none
    private
    public :: simple_test_case_t

    type, extends(test_t) :: simple_test_case_t
        private
        type(varying_string) :: description_
        procedure(simple_test_i), nopass, pointer :: test
        logical :: has_setup_and_teardown
        procedure(computation_i), nopass, pointer :: setup
        procedure(computation_i), nopass, pointer :: teardown
    contains
        private
        procedure, public :: description
        procedure, public :: filter
        procedure, public :: num_cases
        procedure, public :: run_with_input
        procedure, public :: run_without_input
    end type

    interface simple_test_case_t
        module procedure constructor_basic
        module procedure constructor_bracketed
    end interface
contains
    function constructor_basic(description, test) result(simple_test_case)
        type(varying_string), intent(in) :: description
        procedure(simple_test_i) :: test
        type(simple_test_case_t) :: simple_test_case

        simple_test_case%description_ = description
        simple_test_case%test => test
        simple_test_case%has_setup_and_teardown = .false.
    end function

    function constructor_bracketed( &
            description, test, setup, teardown) result(simple_test_case)
        type(varying_string), intent(in) :: description
        procedure(simple_test_i) :: test
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(simple_test_case_t) :: simple_test_case

        simple_test_case%description_ = description
        simple_test_case%test => test
        simple_test_case%has_setup_and_teardown = .true.
        simple_test_case%setup => setup
        simple_test_case%teardown => teardown
    end function

    pure function description(self)
        class(simple_test_case_t), intent(in) :: self
        type(varying_string) :: description

        description = self%description_
    end function

    function filter(self, filter_string) result(filter_result)
        class(simple_test_case_t), intent(in) :: self
        type(varying_string), intent(in) :: filter_string
        type(filter_result_t) :: filter_result

        if (char(filter_string).in.char(self%description_)) then
            filter_result = filter_matched(self)
        else
            filter_result = filter_failed()
        end if
    end function

    pure function num_cases(self)
        class(simple_test_case_t), intent(in) :: self
        integer :: num_cases

        associate(unused => self)
        end associate

        num_cases = 1
    end function

    recursive function run_with_input(self, input) result(result_)
        class(simple_test_case_t), intent(in) :: self
        class(input_t), intent(in) :: input
        type(test_result_item_t) :: result_

        associate(unused => input)
        end associate

        result_ = self%run()
    end function

    recursive function run_without_input(self) result(result_)
        class(prif_simple_test_case_t), intent(in) :: self
        type(test_result_item_t) :: result_

        integer :: me, ni

        call prif_this_image_no_coarray(this_image = me)
        call prif_num_images(ni)
        if (DEBUG) call put_line( &
                "Beginning execution of: " // self%description_&
                // merge(" on image " // to_string(me), var_str(""), ni > 1))
        if (self%has_setup_and_teardown) call self%setup
        result_ = test_result_item_t(test_case_result_t( &
                self%description_, self%test()))
        if (self%has_setup_and_teardown) call self%teardown
        if (DEBUG) call put_line( &
                "Completed execution of: " // self%description_&
                // merge(" on image " // to_string(me), var_str(""), ni > 1))
    end function
end module
