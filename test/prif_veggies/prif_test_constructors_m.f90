module prif_test_constructors_m
    use iso_varying_string, only: varying_string, operator(//), var_str
    use veggies_example_m, only: example_t
    use veggies_generator_m, only: generator_t
    use veggies_input_m, only: input_t
    use prif_input_test_case_m, only: input_test_case_t
    use prif_simple_test_case_m, only: simple_test_case_t
    use prif_simple_test_collection_m, only: simple_test_collection_t
    use prif_test_case_with_examples_m, only: test_case_with_examples_t
    use prif_test_case_with_generator_m, only: test_case_with_generator_t
    use prif_test_collection_with_input_m, only: &
            test_collection_with_input_t
    use veggies_test_interfaces_m, only: &
            computation_i, input_test_i, simple_test_i, transformer_i
    use veggies_test_item_m, only: test_item_t
    use prif_transforming_test_collection_m, only: &
            transforming_test_collection_t

    implicit none
    private
    public :: describe, given, it, it_, test_that, then_, then__, when

    interface describe
        module procedure describe_basic_c
        module procedure describe_basic_s
        module procedure describe_bracketed_c
        module procedure describe_bracketed_s
        module procedure describe_with_input_c
        module procedure describe_with_input_s
        module procedure describe_with_input_brackted_c
        module procedure describe_with_input_brackted_s
        module procedure describe_with_transformer_c
        module procedure describe_with_transformer_s
        module procedure describe_with_transformer_bracketed_c
        module procedure describe_with_transformer_bracketed_s
    end interface

    interface given
        module procedure given_basic_c
        module procedure given_basic_s
        module procedure given_bracketed_c
        module procedure given_bracketed_s
        module procedure given_with_input_c
        module procedure given_with_input_s
        module procedure given_with_input_bracketed_c
        module procedure given_with_input_bracketed_s
        module procedure given_with_transformer_c
        module procedure given_with_transformer_s
        module procedure given_with_transformer_bracketed_c
        module procedure given_with_transformer_bracketed_s
    end interface

    interface it
        module procedure it_basic_c
        module procedure it_basic_s
        module procedure it_bracketed_c
        module procedure it_bracketed_s
        module procedure it_with_examples_c
        module procedure it_with_examples_s
        module procedure it_with_examples_bracketed_c
        module procedure it_with_examples_bracketed_s
        module procedure it_with_generator_c
        module procedure it_with_generator_s
        module procedure it_with_generator_bracketed_c
        module procedure it_with_generator_bracketed_s
    end interface

    interface it_
        module procedure it_input_c
        module procedure it_input_s
        module procedure it_input_bracketed_c
        module procedure it_input_bracketed_s
    end interface

    interface then_
        module procedure then_basic_c
        module procedure then_basic_s
        module procedure then_bracketed_c
        module procedure then_bracketed_s
        module procedure then_with_examples_c
        module procedure then_with_examples_s
        module procedure then_with_examples_bracketed_c
        module procedure then_with_examples_bracketed_s
        module procedure then_with_generator_c
        module procedure then_with_generator_s
        module procedure then_with_generator_bracketed_c
        module procedure then_with_generator_bracketed_s
    end interface

    interface then__
        module procedure then_input_c
        module procedure then_input_s
        module procedure then_input_bracketed_c
        module procedure then_input_bracketed_s
    end interface

    interface when
        module procedure when_basic_c
        module procedure when_basic_s
        module procedure when_bracketed_c
        module procedure when_bracketed_s
        module procedure when_with_input_c
        module procedure when_with_input_s
        module procedure when_with_input_bracketed_c
        module procedure when_with_input_bracketed_s
        module procedure when_with_transformer_c
        module procedure when_with_transformer_s
        module procedure when_with_transformer_bracketed_c
        module procedure when_with_transformer_bracketed_s
    end interface
contains
    function describe_basic_c(description, tests) result(item)
        character(len=*), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = test_item_t(simple_test_collection_t( &
                var_str(description), tests))
    end function

    function describe_basic_s(description, tests) result(item)
        type(varying_string), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = test_item_t(simple_test_collection_t( &
                description, tests))
    end function

    function describe_bracketed_c(description, tests, setup, teardown) result(item)
        character(len=*), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = test_item_t(simple_test_collection_t( &
                var_str(description), tests, setup, teardown))
    end function

    function describe_bracketed_s(description, tests, setup, teardown) result(item)
        type(varying_string), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = test_item_t(simple_test_collection_t( &
                description, tests, setup, teardown))
    end function

    function describe_with_input_c(description, input, tests) result(item)
        character(len=*), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = test_item_t(test_collection_with_input_t( &
                var_str(description), input, tests))
    end function

    function describe_with_input_s(description, input, tests) result(item)
        type(varying_string), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = test_item_t(test_collection_with_input_t( &
                description, input, tests))
    end function

    function describe_with_input_brackted_c( &
            description, input, tests, setup, teardown) result(item)
        character(len=*), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = test_item_t(test_collection_with_input_t( &
                var_str(description), input, tests, setup, teardown))
    end function

    function describe_with_input_brackted_s( &
            description, input, tests, setup, teardown) result(item)
        type(varying_string), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = test_item_t(test_collection_with_input_t( &
                description, input, tests, setup, teardown))
    end function

    function describe_with_transformer_c(description, transformer, tests) result(item)
        character(len=*), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = test_item_t(transforming_test_collection_t( &
                var_str(description), transformer, tests))
    end function

    function describe_with_transformer_s(description, transformer, tests) result(item)
        type(varying_string), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = test_item_t(transforming_test_collection_t( &
                description, transformer, tests))
    end function

    function describe_with_transformer_bracketed_c( &
            description, transformer, tests, setup, teardown) result(item)
        character(len=*), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = test_item_t(transforming_test_collection_t( &
                var_str(description), transformer, tests, setup, teardown))
    end function

    function describe_with_transformer_bracketed_s( &
            description, transformer, tests, setup, teardown) result(item)
        type(varying_string), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = test_item_t(transforming_test_collection_t( &
                description, transformer, tests, setup, teardown))
    end function

    function given_basic_c(description, tests) result(item)
        character(len=*), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("Given " // description, tests)
    end function

    function given_basic_s(description, tests) result(item)
        type(varying_string), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("Given " // description, tests)
    end function

    function given_bracketed_c(description, tests, setup, teardown) result(item)
        character(len=*), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = describe("Given " // description, tests, setup, teardown)
    end function

    function given_bracketed_s(description, tests, setup, teardown) result(item)
        type(varying_string), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = describe("Given " // description, tests, setup, teardown)
    end function

    function given_with_input_c(description, input, tests) result(item)
        character(len=*), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("Given " // description, input, tests)
    end function

    function given_with_input_s(description, input, tests) result(item)
        type(varying_string), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("Given " // description, input, tests)
    end function

    function given_with_input_bracketed_c( &
            description, input, tests, setup, teardown) result(item)
        character(len=*), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = describe("Given " // description, input, tests, setup, teardown)
    end function

    function given_with_input_bracketed_s( &
            description, input, tests, setup, teardown) result(item)
        type(varying_string), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = describe("Given " // description, input, tests, setup, teardown)
    end function

    function given_with_transformer_c(description, transformer, tests) result(item)
        character(len=*), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("Given " // description, transformer, tests)
    end function

    function given_with_transformer_s(description, transformer, tests) result(item)
        type(varying_string), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("Given " // description, transformer, tests)
    end function

    function given_with_transformer_bracketed_c( &
            description, transformer, tests, setup, teardown) result(item)
        character(len=*), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = describe("Given " // description, transformer, tests, setup, teardown)
    end function

    function given_with_transformer_bracketed_s( &
            description, transformer, tests, setup, teardown) result(item)
        type(varying_string), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = describe("Given " // description, transformer, tests, setup, teardown)
    end function

    function it_basic_c(description, test) result(item)
        character(len=*), intent(in) :: description
        procedure(simple_test_i) :: test
        type(test_item_t) :: item

        item = test_item_t(simple_test_case_t(var_str(description), test))
    end function

    function it_basic_s(description, test) result(item)
        type(varying_string), intent(in) :: description
        procedure(simple_test_i) :: test
        type(test_item_t) :: item

        item = test_item_t(simple_test_case_t(description, test))
    end function

    function it_bracketed_c(description, test, setup, teardown) result(item)
        character(len=*), intent(in) :: description
        procedure(simple_test_i) :: test
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = test_item_t(simple_test_case_t(var_str(description), test, setup, teardown))
    end function

    function it_bracketed_s(description, test, setup, teardown) result(item)
        type(varying_string), intent(in) :: description
        procedure(simple_test_i) :: test
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = test_item_t(simple_test_case_t(description, test, setup, teardown))
    end function

    function it_input_c(description, test) result(item)
        character(len=*), intent(in) :: description
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = test_item_t(input_test_case_t(var_str(description), test))
    end function

    function it_input_s(description, test) result(item)
        type(varying_string), intent(in) :: description
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = test_item_t(input_test_case_t(description, test))
    end function

    function it_input_bracketed_c(description, test, setup, teardown) result(item)
        character(len=*), intent(in) :: description
        procedure(input_test_i) :: test
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = test_item_t(input_test_case_t(var_str(description), test, setup, teardown))
    end function

    function it_input_bracketed_s(description, test, setup, teardown) result(item)
        type(varying_string), intent(in) :: description
        procedure(input_test_i) :: test
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = test_item_t(input_test_case_t(description, test, setup, teardown))
    end function

    function it_with_examples_c(description, examples, test) result(item)
        character(len=*), intent(in) :: description
        type(example_t), intent(in) :: examples(:)
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = test_item_t(test_case_with_examples_t( &
                var_str(description), examples, test))
    end function

    function it_with_examples_s(description, examples, test) result(item)
        type(varying_string), intent(in) :: description
        type(example_t), intent(in) :: examples(:)
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = test_item_t(test_case_with_examples_t( &
                description, examples, test))
    end function

    function it_with_examples_bracketed_c( &
            description, examples, test, setup, teardown) result(item)
        character(len=*), intent(in) :: description
        type(example_t), intent(in) :: examples(:)
        procedure(input_test_i) :: test
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = test_item_t(test_case_with_examples_t( &
                var_str(description), examples, test, setup, teardown))
    end function

    function it_with_examples_bracketed_s( &
            description, examples, test, setup, teardown) result(item)
        type(varying_string), intent(in) :: description
        type(example_t), intent(in) :: examples(:)
        procedure(input_test_i) :: test
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = test_item_t(test_case_with_examples_t( &
                description, examples, test, setup, teardown))
    end function

    function it_with_generator_c(description, generator, test) result(item)
        character(len=*), intent(in) :: description
        class(generator_t), intent(in) :: generator
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = test_item_t(test_case_with_generator_t( &
                var_str(description), generator, test))
    end function

    function it_with_generator_s(description, generator, test) result(item)
        type(varying_string), intent(in) :: description
        class(generator_t), intent(in) :: generator
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = test_item_t(test_case_with_generator_t( &
                description, generator, test))
    end function

    function it_with_generator_bracketed_c( &
            description, generator, test, setup, teardown) result(item)
        character(len=*), intent(in) :: description
        class(generator_t), intent(in) :: generator
        procedure(input_test_i) :: test
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = test_item_t(test_case_with_generator_t( &
                var_str(description), generator, test, setup, teardown))
    end function

    function it_with_generator_bracketed_s( &
            description, generator, test, setup, teardown) result(item)
        type(varying_string), intent(in) :: description
        class(generator_t), intent(in) :: generator
        procedure(input_test_i) :: test
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = test_item_t(test_case_with_generator_t( &
                description, generator, test, setup, teardown))
    end function

    function test_that(tests) result(item)
        type(test_item_t) :: tests(:)
        type(test_item_t) :: item

        item = describe("Test that", tests)
    end function

    function then_basic_c(description, test) result(item)
        character(len=*), intent(in) :: description
        procedure(simple_test_i) :: test
        type(test_item_t) :: item

        item = it("Then " // description, test)
    end function

    function then_basic_s(description, test) result(item)
        type(varying_string), intent(in) :: description
        procedure(simple_test_i) :: test
        type(test_item_t) :: item

        item = it("Then " // description, test)
    end function

    function then_bracketed_c(description, test, setup, teardown) result(item)
        character(len=*), intent(in) :: description
        procedure(simple_test_i) :: test
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = it("Then " // description, test, setup, teardown)
    end function

    function then_bracketed_s(description, test, setup, teardown) result(item)
        type(varying_string), intent(in) :: description
        procedure(simple_test_i) :: test
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = it("Then " // description, test, setup, teardown)
    end function

    function then_with_examples_c(description, examples, test) result(item)
        character(len=*), intent(in) :: description
        type(example_t), intent(in) :: examples(:)
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = it("Then " // description, examples, test)
    end function

    function then_with_examples_s(description, examples, test) result(item)
        type(varying_string), intent(in) :: description
        type(example_t), intent(in) :: examples(:)
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = it("Then " // description, examples, test)
    end function

    function then_with_examples_bracketed_c( &
            description, examples, test, setup, teardown) result(item)
        character(len=*), intent(in) :: description
        type(example_t), intent(in) :: examples(:)
        procedure(input_test_i) :: test
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = it("Then " // description, examples, test, setup, teardown)
    end function

    function then_with_examples_bracketed_s( &
            description, examples, test, setup, teardown) result(item)
        type(varying_string), intent(in) :: description
        type(example_t), intent(in) :: examples(:)
        procedure(input_test_i) :: test
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = it("Then " // description, examples, test, setup, teardown)
    end function

    function then_with_generator_c(description, generator, test) result(item)
        character(len=*), intent(in) :: description
        class(generator_t), intent(in) :: generator
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = it("Then " // description, generator, test)
    end function

    function then_with_generator_s(description, generator, test) result(item)
        type(varying_string), intent(in) :: description
        class(generator_t), intent(in) :: generator
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = it("Then " // description, generator, test)
    end function

    function then_with_generator_bracketed_c( &
            description, generator, test, setup, teardown) result(item)
        character(len=*), intent(in) :: description
        class(generator_t), intent(in) :: generator
        procedure(input_test_i) :: test
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = it("Then " // description, generator, test, setup, teardown)
    end function

    function then_with_generator_bracketed_s( &
            description, generator, test, setup, teardown) result(item)
        type(varying_string), intent(in) :: description
        class(generator_t), intent(in) :: generator
        procedure(input_test_i) :: test
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = it("Then " // description, generator, test, setup, teardown)
    end function

    function then_input_c(description, test) result(item)
        character(len=*), intent(in) :: description
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = it_("Then " // description, test)
    end function

    function then_input_s(description, test) result(item)
        type(varying_string), intent(in) :: description
        procedure(input_test_i) :: test
        type(test_item_t) :: item

        item = it_("Then " // description, test)
    end function

    function then_input_bracketed_c(description, test, setup, teardown) result(item)
        character(len=*), intent(in) :: description
        procedure(input_test_i) :: test
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = it_("Then " // description, test, setup, teardown)
    end function

    function then_input_bracketed_s(description, test, setup, teardown) result(item)
        type(varying_string), intent(in) :: description
        procedure(input_test_i) :: test
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = it_("Then " // description, test, setup, teardown)
    end function

    function when_basic_c(description, tests) result(item)
        character(len=*), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("When " // description, tests)
    end function

    function when_basic_s(description, tests) result(item)
        type(varying_string), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("When " // description, tests)
    end function

    function when_bracketed_c(description, tests, setup, teardown) result(item)
        character(len=*), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = describe("When " // description, tests, setup, teardown)
    end function

    function when_bracketed_s(description, tests, setup, teardown) result(item)
        type(varying_string), intent(in) :: description
        type(test_item_t), intent(in) :: tests(:)
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = describe("When " // description, tests, setup, teardown)
    end function

    function when_with_input_c(description, input, tests) result(item)
        character(len=*), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("When " // description, input, tests)
    end function

    function when_with_input_s(description, input, tests) result(item)
        type(varying_string), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("When " // description, input, tests)
    end function

    function when_with_input_bracketed_c( &
            description, input, tests, setup, teardown) result(item)
        character(len=*), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = describe("When " // description, input, tests, setup, teardown)
    end function

    function when_with_input_bracketed_s( &
            description, input, tests, setup, teardown) result(item)
        type(varying_string), intent(in) :: description
        class(input_t), intent(in) :: input
        type(test_item_t), intent(in) :: tests(:)
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = describe("When " // description, input, tests, setup, teardown)
    end function

    function when_with_transformer_c(description, transformer, tests) result(item)
        character(len=*), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("When " // description, transformer, tests)
    end function

    function when_with_transformer_s(description, transformer, tests) result(item)
        type(varying_string), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        type(test_item_t) :: item

        item = describe("When " // description, transformer, tests)
    end function

    function when_with_transformer_bracketed_c( &
            description, transformer, tests, setup, teardown) result(item)
        character(len=*), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = describe("When " // description, transformer, tests, setup, teardown)
    end function

    function when_with_transformer_bracketed_s( &
            description, transformer, tests, setup, teardown) result(item)
        type(varying_string), intent(in) :: description
        procedure(transformer_i) :: transformer
        type(test_item_t), intent(in) :: tests(:)
        procedure(computation_i) :: setup
        procedure(computation_i) :: teardown
        type(test_item_t) :: item

        item = describe("When " // description, transformer, tests, setup, teardown)
    end function
end module
