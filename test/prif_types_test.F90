#include "test-utils.F90"

module prif_types_test_m
    use iso_fortran_env, only: int8
    use iso_c_binding, only: c_ptr, c_loc, c_intptr_t
    use prif, only: prif_team_type, prif_event_type, prif_notify_type, prif_lock_type, prif_critical_type, prif_coarray_handle, prif_this_image_no_coarray
    use julienne_m, only: test_description_t, test_diagnosis_t, test_result_t, test_t, string_t, usher &
       ,operator(.all.), operator(.also.), operator(.equalsExpected.), operator(.greaterThan.), operator(.isAtMost.), operator(//)

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

    type :: cptr_wrapper_t
      private
      type(c_ptr) :: info
    end type

contains
    pure function subject()
      character(len=:), allocatable :: subject
      subject = "PRIF Types"
    end function

    function results() result(test_results)
      type(test_result_t), allocatable :: test_results(:)
      type(prif_types_test_t) prif_types_test

      allocate(test_results, source = prif_types_test%run([ &
            test_description_t("having a compliant prif_coarray_handle representation", usher(check_coarray_handle)) &
          , test_description_t("having a compliant prif_team_type representation", usher(check_team_type)) &
          , test_description_t("having a compliant prif_event_type representation", usher(check_event_type)) &
          , test_description_t("having a compliant prif_notify_type representation", usher(check_notify_type)) &
          , test_description_t("having a compliant prif_lock_type representation", usher(check_lock_type)) &
          , test_description_t("having a compliant prif_critical_type representation", usher(check_critical_type)) &
      ]))
    end function

    subroutine report_size(typename,sz, align_cptr)
      character(len=*), intent(in) :: typename
      integer, intent(in) :: sz
      type(c_ptr), intent(in) :: align_cptr(:)
      character(len=20) :: typestr
      integer :: me, i
      integer(c_intptr_t) :: cint, align
      call prif_this_image_no_coarray(this_image=me)
      if (me == 1) then
        typestr = typename
        align = 0
        do i=1,size(align_cptr)
          cint = transfer(align_cptr(i),cint)
          align = ior(align, cint)
        end do
        align = iand(align, -align)
        write(*,'(*(A,I3))') "   " // typestr // ": ", sz, " bytes, ", align, "-byte aligned"
      end if
    end subroutine

    ! declare typename variables in various ways to try and deduce minimum alignment requirement
    ! this heuristic might fail to discover the narrowest aligment, but does a reasonable job in practice
#   define ALIGN(typename) \
        integer :: aai ; \
        type :: align_check ; \
          integer(int8) :: a_pad ; \
          type(typename) :: t ; \
        end type ; \
        type(align_check), target :: align_arr(256) ; \
        integer(int8) :: a_pad2 ; \
        type(align_check), target :: align_s1 ; \
        integer(int8) :: a_pad3 ; \
        type(typename), target :: align_s2 ; \
        type(typename), target, save :: align_s3 ; \
        a_pad2 = 0 ; a_pad3 = 0 ; aai = a_pad2 + a_pad3 ! avoid unused warnings
#   define ALIGN_ARGS [ \
              c_loc(align_s1%t), c_loc(align_s2), c_loc(align_s3), \
            ( c_loc(align_arr(aai)%t), aai = 1,size(align_arr) ) \
           ]

    function check_team_type() result(diag)
        type(test_diagnosis_t) :: diag
        type(prif_team_type) :: team
        type(pointer_wrapper_t) :: pointer_wrap
        type(dummy_t), target :: tgt
        ALIGN(prif_team_type)

        diag = .true.

        ! size check
        call report_size("prif_team_type", storage_size(team)/8, ALIGN_ARGS)
        ALSO(storage_size(team) .equalsExpected. storage_size(pointer_wrap))

        ! default initialization check
        pointer_wrap%info => tgt
        pointer_wrap = transfer(team, pointer_wrap)
        ALSO2(.not. associated(pointer_wrap%info), "default initialization to null")
    end function

    function check_event_type() result(diag)
        type(test_diagnosis_t) :: diag
        type(prif_event_type) :: event
        integer :: ssz
        integer(int8), allocatable :: bytes(:)
        ALIGN(prif_event_type)

        diag = .true.

        ! size check
        call report_size("prif_event_type", storage_size(event)/8, ALIGN_ARGS)
        ssz = storage_size(event)
        ALSO(ssz .greaterThan. 0)
        ALSO(ssz .isAtMost. 64*8) 

        ! default initialization check
        allocate(bytes(64))
        bytes = transfer(event, bytes)
        ALSO2(.all.(int(bytes) .equalsExpected. 0), "default initialization to zero")
    end function

    function check_lock_type() result(diag)
        type(test_diagnosis_t) :: diag
        type(prif_lock_type) :: lock
        integer :: ssz
        integer(int8), allocatable :: bytes(:)
        ALIGN(prif_lock_type)

        diag = .true.

        ! size check
        call report_size("prif_lock_type", storage_size(lock)/8, ALIGN_ARGS)
        ssz = storage_size(lock)
        ALSO(ssz .greaterThan. 0)
        ALSO(ssz .isAtMost. 64*8) 

        ! default initialization check
        allocate(bytes(64))
        bytes = transfer(lock, bytes)
        ALSO2(.all.(int(bytes) .equalsExpected. 0), "default initialization to zero")
    end function

    function check_notify_type() result(diag)
        type(test_diagnosis_t) :: diag
        type(prif_notify_type) :: notify
        integer :: ssz
        integer(int8), allocatable :: bytes(:)
        ALIGN(prif_notify_type)

        diag = .true.

        ! size check
        call report_size("prif_notify_type", storage_size(notify)/8, ALIGN_ARGS)
        ssz = storage_size(notify)
        ALSO(ssz .greaterThan. 0)
        ALSO(ssz .isAtMost. 64*8) 

        ! default initialization check
        allocate(bytes(64))
        bytes = transfer(notify, bytes)
        ALSO2(.all.(int(bytes) .equalsExpected. 0), "default initialization to zero")
    end function

    function check_critical_type() result(diag)
        type(test_diagnosis_t) :: diag
        type(prif_critical_type) :: critical
        integer :: ssz
        integer(int8), allocatable :: bytes(:)
        ALIGN(prif_critical_type)

        diag = .true.

        ! size check
        call report_size("prif_critical_type", storage_size(critical)/8, ALIGN_ARGS)
        ssz = storage_size(critical)
        ALSO(ssz .greaterThan. 0)
        ALSO(ssz .isAtMost. 64*8) 

        ! default initialization check
        allocate(bytes(64))
        bytes = transfer(critical, bytes)
        ALSO2(.all.(int(bytes) .equalsExpected. 0), "default initialization to zero")
    end function

    function check_coarray_handle() result(diag)
        type(test_diagnosis_t) :: diag
        type(prif_coarray_handle) :: handle
        type(cptr_wrapper_t) :: cptr_wrap
        ALIGN(prif_coarray_handle)

        diag = .true.

        ! size check
        call report_size("prif_coarray_handle", storage_size(handle)/8, ALIGN_ARGS)
        ALSO(storage_size(handle) .equalsExpected. storage_size(cptr_wrap))
    end function

end module prif_types_test_m
