module caf_this_image_test
    use caffeine_m, only : caf_this_image, caf_num_images, caf_co_sum
    use vegetables, only: result_t, test_item_t, assert_that, describe, it

    implicit none
    private
    public :: test_caf_this_image

contains
    function test_caf_this_image() result(tests)
        type(test_item_t) :: tests

        tests = describe( &
                "The caf_this_image function result", &
                [ it("is the proper member of the set {1,2,...,num_images()} when invoked as this_image()", check_this_image_set) &
                ])
    end function

    function check_this_image_set() result(result_)
        type(result_t) :: result_
        integer, allocatable :: image_numbers(:)
        integer i
 
        associate(me => caf_this_image(), ni => caf_num_images())
          image_numbers = [(merge(0, me, me/=i), i = 1, ni)]
          call caf_co_sum(image_numbers)
          result_ = assert_that(all(image_numbers == [(i, i = 1, ni)]) .and. size(image_numbers)>0, "correct image set")
        end associate
    end function

end module caf_this_image_test
