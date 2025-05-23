module caf_this_image_test
    use prif, only : prif_this_image_no_coarray, prif_num_images, prif_co_sum
    use veggies, only: result_t, test_item_t, assert_that, describe, it, succeed

    implicit none
    private
    public :: test_prif_this_image_no_coarray

contains
    function test_prif_this_image_no_coarray() result(tests)
        type(test_item_t) :: tests
    
        tests = describe( &
          "The prif_this_image_no_coarray function result", &
          [ it("is the proper member of the set {1,2,...,num_images()} when invoked as this_image()", check_this_image_set) &
        ])
    end function

    function check_this_image_set() result(result_)
        type(result_t) :: result_
        integer, allocatable :: image_numbers(:)
        integer i, me, ni

        allocate(image_numbers(0))

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(num_images=ni)
        image_numbers = [(merge(0, me, me/=i), i = 1, ni)]
        call prif_co_sum(image_numbers)
        result_ = assert_that(all(image_numbers == [(i, i = 1, ni)]) .and. size(image_numbers)>0, "correct image set")
    end function

end module caf_this_image_test
