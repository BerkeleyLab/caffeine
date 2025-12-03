module prif_image_queries_test_m
    use iso_c_binding, only: c_int
    use prif, only : prif_image_status, prif_stopped_images, prif_failed_images, PRIF_STAT_FAILED_IMAGE, PRIF_STAT_STOPPED_IMAGE
    use prif, only : prif_num_images
    use julienne_m, only: &
       operator(//) &
      ,operator(.all.) &
      ,operator(.also.) &
      ,operator(.isAtLeast.) &
      ,operator(.isAtMost.) &
      ,operator(.lessThan.) &
      ,operator(.expect.) &
      ,usher &
      ,test_description_t &
      ,test_diagnosis_t &
      ,test_result_t &
      ,test_t

    implicit none
    private
    public :: prif_image_queries_test_t

    type, extends(test_t) :: prif_image_queries_test_t 
    contains
      procedure, nopass, non_overridable :: subject
      procedure, nopass, non_overridable :: results
    end type

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = "PRIF image query procedures"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(prif_image_queries_test_t) prif_image_queries_test

    test_results = prif_image_queries_test%run([ &
       test_description_t("providing valid prif_image_status()", usher(check_image_status)) &
      ,test_description_t("providing valid prif_stopped_images()", usher(check_stopped_images)) &
      ,test_description_t("providing valid prif_failed_images()", usher(check_failed_images)) &
    ])
  end function

  function check_image_status() result(diag)
      type(test_diagnosis_t) :: diag
      integer(c_int) :: image_status
      
      call prif_image_status(1, image_status=image_status)
      ! TODO: replace with .any. once Julienne issue #138 is implemented
      diag = .expect. (any(image_status == [0, PRIF_STAT_FAILED_IMAGE, PRIF_STAT_STOPPED_IMAGE])) &
                        // "permitted image status"
  end function

  function valid_image_list(nums) result(diag)
      integer, allocatable, intent(in) :: nums(:)
      type(test_diagnosis_t) :: diag
      integer ni

      call prif_num_images(num_images=ni)
      diag = &
         .expect. allocated(nums) .also. &
         (size(nums) .isAtMost. ni) .also. &
         (.all. (nums .isAtLeast. 1)) .also. &
         (.all. (nums .isAtMost. ni)) .also. &
         (.all. (nums(1:size(nums)-1) .lessThan. nums(2:size(nums)))) // "valid stopped image"
  end function

  function check_stopped_images() result(diag)
      type(test_diagnosis_t) :: diag
      integer, allocatable :: nums(:)

      call prif_stopped_images(stopped_images=nums)
      diag = valid_image_list(nums)
  end function

  function check_failed_images() result(diag)
      type(test_diagnosis_t) :: diag
      integer, allocatable :: nums(:)

      call prif_failed_images(failed_images=nums)
      diag = valid_image_list(nums)
  end function

end module prif_image_queries_test_m
