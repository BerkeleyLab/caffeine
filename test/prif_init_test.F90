module prif_init_test_m
    use prif, only : prif_init, PRIF_STAT_ALREADY_INIT
    use julienne_m, only: test_description_t, test_diagnosis_t, test_result_t, test_t, operator(.equalsExpected.), usher

    implicit none
    private
    public :: prif_init_test_t, check_caffeination

    type, extends(test_t) :: prif_init_test_t
    contains
      procedure, nopass, non_overridable :: subject
      procedure, nopass, non_overridable :: results
    end type

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = "prif_init"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable ::  test_results(:)
    type(prif_init_test_t) prif_init_test

    allocate(test_results, source = prif_init_test%run([ &
        test_description_t("completing successfully", usher(check_caffeination)) &
       ,test_description_t("returning PRIF_STAT_ALREADY_INIT on a subsequent call ", usher(check_subsequent_prif_init_call)) &
    ]))
  end function


  function check_caffeination() result(diag)
    ! this test needs to run very early at startup, so we memoize the result
    type(test_diagnosis_t) :: diag
    type(test_diagnosis_t), save :: memo
    logical, save :: first_pass = .true.

    if (first_pass) then
      first_pass = .false.
      block
#if HAVE_MULTI_IMAGE
        integer, parameter :: successful_initiation = PRIF_STAT_ALREADY_INIT
#else
        integer, parameter :: successful_initiation = 0
#endif
        integer init_exit_code

        call prif_init(init_exit_code)
        memo = init_exit_code .equalsExpected. successful_initiation
      end block
    endif 

    diag = memo
  end function

  function check_subsequent_prif_init_call() result(diag)
    type(test_diagnosis_t) :: diag
    integer stat

    call prif_init(stat)
    call prif_init(stat)
    diag = stat .equalsExpected. PRIF_STAT_ALREADY_INIT
  end function

end module prif_init_test_m
