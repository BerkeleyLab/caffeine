module prif_types_test
    use iso_fortran_env, only: int8
    use prif
    use veggies, only: result_t, test_item_t, assert_that, assert_not, assert_equals, describe, it, succeed

    implicit none
    private
    public :: test_prif_types

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
    function test_prif_types() result(tests)
        type(test_item_t) :: tests
    
        tests = describe( &
          "PRIF types", &
          [ it("prif_team_type has a compliant representation", check_team_type) &
          , it("prif_event_type has a compliant representation", check_event_type) &
          , it("prif_lock_type has a compliant representation", check_lock_type) &
          , it("prif_notify_type has a compliant representation", check_notify_type) &
          , it("prif_critical_type has a compliant representation", check_critical_type) &
        ])
    end function

    function check_team_type() result(diag)
        type(result_t) :: diag
        type(prif_team_type) :: team
        type(pointer_wrapper_t) :: pointer_wrap
        type(dummy_t), target :: tgt

        diag = succeed("")

        ! size check
        diag = diag .and. &
          assert_equals(storage_size(team), storage_size(pointer_wrap))

        ! default initialization check
        pointer_wrap%info => tgt
        pointer_wrap = transfer(team, pointer_wrap)
        diag = diag .and. &
          assert_that(.not. associated(pointer_wrap%info))
    end function

    function check_event_type() result(diag)
        type(result_t) :: diag
        type(prif_event_type) :: event
        integer :: ssz
        integer(int8), allocatable :: bytes(:)

        diag = succeed("")

        ! size check
        ssz = storage_size(event)
        diag = diag .and. &
          assert_that(ssz > 0) .and. &
          assert_that(ssz <= 64*8) 

        ! default initialization check
        allocate(bytes(64))
        bytes = transfer(event, bytes)
        diag = diag .and. &
          assert_that( all(bytes == 0) )
    end function

    function check_lock_type() result(diag)
        type(result_t) :: diag
        type(prif_lock_type) :: lock
        integer :: ssz
        integer(int8), allocatable :: bytes(:)

        diag = succeed("")

        ! size check
        ssz = storage_size(lock)
        diag = diag .and. &
          assert_that(ssz > 0) .and. &
          assert_that(ssz <= 64*8) 

        ! default initialization check
        allocate(bytes(64))
        bytes = transfer(lock, bytes)
        diag = diag .and. &
          assert_that( all(bytes == 0) )
    end function

    function check_notify_type() result(diag)
        type(result_t) :: diag
        type(prif_notify_type) :: notify
        integer :: ssz
        integer(int8), allocatable :: bytes(:)

        diag = succeed("")

        ! size check
        ssz = storage_size(notify)
        diag = diag .and. &
          assert_that(ssz > 0) .and. &
          assert_that(ssz <= 64*8) 

        ! default initialization check
        allocate(bytes(64))
        bytes = transfer(notify, bytes)
        diag = diag .and. &
          assert_that( all(bytes == 0) )
    end function

    function check_critical_type() result(diag)
        type(result_t) :: diag
        type(prif_critical_type) :: critical
        integer :: ssz
        integer(int8), allocatable :: bytes(:)

        diag = succeed("")

        ! size check
        ssz = storage_size(critical)
        diag = diag .and. &
          assert_that(ssz > 0) .and. &
          assert_that(ssz <= 64*8) 

        ! default initialization check
        allocate(bytes(64))
        bytes = transfer(critical, bytes)
        diag = diag .and. &
          assert_that( all(bytes == 0) )
    end function

end module prif_types_test
