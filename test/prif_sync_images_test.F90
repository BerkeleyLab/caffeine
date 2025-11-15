module prif_sync_images_test_m
    use iso_c_binding, only: c_int
    use prif, only : prif_sync_images, prif_this_image_no_coarray, prif_num_images, prif_sync_all
    use julienne_m, only: test_description_t, test_diagnosis_t, test_result_t, test_t, operator(.expect.), usher

    implicit none
    private
    public :: prif_sync_images_test_t

    type, extends(test_t) :: prif_sync_images_test_t
    contains
      procedure, nopass, non_overridable :: subject
      procedure, nopass, non_overridable :: results
    end type
 
    integer, parameter :: lim = 10

contains

    pure function subject() result(test_subject)
      character(len=:), allocatable :: test_subject
      test_subject = "The prif_sync_images subroutine"
    end function

    function results() result(test_results)
        type(test_result_t), allocatable :: test_results(:)
        type(prif_sync_images_test_t) prif_sync_images_test

        test_results = prif_sync_images_test%run([ &
           test_description_t("synchronizing an image with itself", usher(check_serial)), &
           test_description_t("synchronizing with a neighbor", usher(check_neighbor)), &
           test_description_t("synchronizing every image with one image", usher(check_hot)) &
        ])
    end function

    function check_serial() result(test_diagnosis)
        type(test_diagnosis_t) test_diagnosis
        integer(c_int) :: me
        integer i

        call prif_this_image_no_coarray(this_image=me)
        call prif_sync_all
       
        ! synchronize with myself an image-dependent number of times:
        do i=1, lim*me
          call prif_sync_images([me])
        end do

        call prif_sync_all
        test_diagnosis = .expect. .true.
    end function


    function check_neighbor() result(test_diagnosis)
        type(test_diagnosis_t) test_diagnosis
        integer(c_int) me, num_imgs
        integer i

        call prif_this_image_no_coarray(this_image=me)
        call prif_num_images(num_images=num_imgs)
        call prif_sync_all
       
        ! test based on F23 11.7.4 note 3
        do i=1, lim
          if (me > 1)        call prif_sync_images([me-1])
          if (me < num_imgs) call prif_sync_images([me+1])
        end do

        call prif_sync_all
        test_diagnosis = .expect. .true.
    end function

    function check_hot() result(test_diagnosis)
        type(test_diagnosis_t) test_diagnosis
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
        test_diagnosis = .expect. .true.
    end function

end module prif_sync_images_test_m
