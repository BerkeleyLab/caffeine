module caf_image_queries_test
    use iso_c_binding, only: c_int
    use prif, only : prif_image_status, prif_stopped_images, prif_failed_images, PRIF_STAT_FAILED_IMAGE, PRIF_STAT_STOPPED_IMAGE
    use prif, only : prif_num_images
    use veggies, only: result_t, test_item_t, assert_that, describe, it, succeed

    implicit none
    private
    public :: test_prif_image_queries

contains
    function test_prif_image_queries() result(tests)
        type(test_item_t) :: tests
    
        tests = describe( &
          "PRIF image queries", [ &
           it("provide valid prif_image_status()", check_image_status), &
           it("provide valid prif_stopped_images()", check_stopped_images), &
           it("provide valid prif_failed_images()", check_failed_images) &
        ])
    end function

    function check_image_status() result(result_)
        type(result_t) :: result_
        integer(c_int) :: image_status
        
        call prif_image_status(1, image_status=image_status)
        result_ = assert_that(image_status == 0 .or. &
                              image_status == PRIF_STAT_FAILED_IMAGE .or. &
                              image_status == PRIF_STAT_STOPPED_IMAGE, "permitted image status")
    end function

    function valid_image_list(nums) result(result_)
        integer, allocatable, intent(in) :: nums(:)
        type(result_t) :: result_
        integer i, ni

        call prif_num_images(num_images=ni)
        result_ = assert_that( allocated(nums) .and. size(nums) <= ni .and. &
                               all([(nums(i) >= 1 .and. nums(i) <= ni, i = 1, size(nums))]) .and. &
                               all([(nums(i) < nums(i+1), i = 1, size(nums)-1)]), &
                               "valid stopped images")
    end function

    function check_stopped_images() result(result_)
        type(result_t) :: result_
        integer, allocatable :: nums(:)

        call prif_stopped_images(stopped_images=nums)
        result_ = valid_image_list(nums)
    end function

    function check_failed_images() result(result_)
        type(result_t) :: result_
        integer, allocatable :: nums(:)

        call prif_failed_images(failed_images=nums)
        result_ = valid_image_list(nums)
    end function

end module caf_image_queries_test
