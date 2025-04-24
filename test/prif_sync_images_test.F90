module caf_sync_images_test
    use iso_c_binding, only: c_int
    use prif, only : prif_sync_images, prif_this_image_no_coarray, prif_num_images, prif_sync_all
    use veggies, only: result_t, test_item_t, assert_that, describe, it, succeed

    implicit none
    private
    public :: test_prif_sync_images

    integer, parameter :: lim = 10

contains
    function test_prif_sync_images() result(tests)
        type(test_item_t) :: tests
    
        tests = describe( &
          "PRIF sync images", [ &
           it("pass serial prif_sync_images test", check_serial), &
           it("pass prif_sync_images neighbor test", check_neighbor), &
           it("pass prif_sync_images hot-spot test", check_hot) &
        ])
    end function

    function check_serial() result(result_)
        type(result_t) :: result_
        integer(c_int) :: me
        integer :: i

        call prif_this_image_no_coarray(this_image=me)
        call prif_sync_all
       
        ! synchronize with myself an image-dependent number of times:
        do i=1, lim*me
          call prif_sync_images([me])
        end do

        call prif_sync_all
        result_ = succeed("")
    end function


    function check_neighbor() result(result_)
        type(result_t) :: result_
        integer(c_int) :: me, num_imgs
        integer :: i

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(num_images=num_imgs)
        call prif_sync_all
       
        ! test based on F23 11.7.4 note 3
        do i=1, lim
          if (me > 1)        call prif_sync_images([me-1])
          if (me < num_imgs) call prif_sync_images([me+1])
        end do

        call prif_sync_all
        result_ = succeed("")
    end function

    function check_hot() result(result_)
        type(result_t) :: result_
        integer(c_int) :: me, num_imgs
        integer :: i

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(num_images=num_imgs)
        call prif_sync_all
       
        ! all images synchronize with 1
        if (me == 1) then
          block
            integer(c_int) :: everyone(num_imgs)
            everyone = [(i, i=1,num_imgs)]
            do i=1, lim
              ! SYNC IMAGES (*)
              call prif_sync_images()
            end do
            do i=1, lim
              call prif_sync_images(everyone)
            end do
          end block
        else
          do i=1, lim*2
            call prif_sync_images([1])
          end do
        endif

        call prif_sync_all
        result_ = succeed("")
    end function


end module 
