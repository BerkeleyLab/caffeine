module prif_types_test_m
    use iso_fortran_env, only: int8
    use prif, only: prif_team_type, prif_event_type, prif_notify_type, prif_lock_type, prif_critical_type
    use julienne_m, only: test_description_t, test_diagnosis_t, test_result_t, test_t, passing_test, usher &
       ,operator(.all.), operator(.also.), operator(.expect.), operator(.equalsExpected.), operator(.greaterThan.), operator(.isAtMost.), operator(//)

    implicit none
    private
    public :: prif_types_test_t


    type, extends(test_t) :: prif_types_test_t
    contains
      procedure, nopass, non_overridable :: subject
      procedure, nopass, non_overridable :: results
    end type

    type, private :: dummy_t
      private
      integer foo
      type(dummy_t), pointer :: p
    end type

    type :: pointer_wrapper_t
      private
      type(dummy_t), pointer :: info => null()
    end type

contains
    pure function subject()
      character(len=:), allocatable :: subject
      subject = "PRIF Types"
    end function

    function results() result(test_results)
      type(test_result_t), allocatable :: test_results(:)
      type(prif_types_test_t) prif_types_test

      test_results = prif_types_test%run([ &
            test_description_t("having a compliant prif_team_type representation", usher(check_team_type)) &
          , test_description_t("having a compliant prif_event_type representation", usher(check_event_type)) &
          , test_description_t("having a compliant prif_lock_type representation", usher(check_lock_type)) &
          , test_description_t("having a compliant prif_notify_type representation", usher(check_notify_type)) &
          , test_description_t("having a compliant prif_critical_type representation", usher(check_critical_type)) &
        ])
    end function

    function check_team_type() result(diag)
        type(test_diagnosis_t) :: diag
        type(prif_team_type) :: team
        type(pointer_wrapper_t) :: pointer_wrap
        type(dummy_t), target :: tgt

        diag = passing_test()

        ! size check
        diag = diag .also. &
          (storage_size(team) .equalsExpected. storage_size(pointer_wrap))

        ! default initialization check
        pointer_wrap%info => tgt
        pointer_wrap = transfer(team, pointer_wrap)
        diag = diag .also. &
          (.expect.(.not. associated(pointer_wrap%info)) // "default initialization to null")
    end function

    function check_event_type() result(diag)
        type(test_diagnosis_t) :: diag
        type(prif_event_type) :: event
        integer :: ssz
        integer(int8), allocatable :: bytes(:)

        diag = passing_test()

        ! size check
        ssz = storage_size(event)
        diag = diag .also. &
          (ssz .greaterThan. 0) .also. &
          (ssz .isAtMost. 64*8) 

        ! default initialization check
        allocate(bytes(64))
        bytes = transfer(event, bytes)
        diag = diag .also. &
          .all.(int(bytes) .equalsExpected. 0) // "default initialization to zero"
    end function

    function check_lock_type() result(diag)
        type(test_diagnosis_t) :: diag
        type(prif_lock_type) :: lock
        integer :: ssz
        integer(int8), allocatable :: bytes(:)

        diag = passing_test()

        ! size check
        ssz = storage_size(lock)
        diag = diag .also. &
          (ssz .greaterThan. 0) .also. &
          (ssz .isAtMost. 64*8) 

        ! default initialization check
        allocate(bytes(64))
        bytes = transfer(lock, bytes)
        diag = diag .also. &
          .all.(int(bytes) .equalsExpected. 0) // "default initialization to zero"
    end function

    function check_notify_type() result(diag)
        type(test_diagnosis_t) :: diag
        type(prif_notify_type) :: notify
        integer :: ssz
        integer(int8), allocatable :: bytes(:)

        diag = passing_test()

        ! size check
        ssz = storage_size(notify)
        diag = diag .also. &
          (ssz .greaterThan. 0) .also. &
          (ssz .isAtMost. 64*8) 

        ! default initialization check
        allocate(bytes(64))
        bytes = transfer(notify, bytes)
        diag = diag .also. &
          .all.(int(bytes) .equalsExpected. 0) // "default initialization to zero"
    end function

    function check_critical_type() result(diag)
        type(test_diagnosis_t) :: diag
        type(prif_critical_type) :: critical
        integer :: ssz
        integer(int8), allocatable :: bytes(:)

        diag = passing_test()

        ! size check
        ssz = storage_size(critical)
        diag = diag .also. &
          (ssz .greaterThan. 0) .also. &
          (ssz .isAtMost. 64*8) 

        ! default initialization check
        allocate(bytes(64))
        bytes = transfer(critical, bytes)
        diag = diag .also. &
          .all.(int(bytes) .equalsExpected. 0) // "default initialization to zero"
    end function

end module prif_types_test_m
