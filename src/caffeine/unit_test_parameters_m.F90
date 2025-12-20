! Copyright (c), The Regents of the University
! Terms of use are as specified in LICENSE.txt
module unit_test_parameters_m
  use iso_c_binding, only: c_int
  use prif, only: prif_sync_all, prif_this_image_no_coarray
  !! Define values and utilities for consistent use throughout the test suite
  implicit none

  public

  integer(c_int), parameter :: expected_stop_code=99, expected_error_stop_code=100
      ! used in stop/error-stop unit tests and example/test-support supporting programs

  character(len=:), allocatable :: subjob_prefix
  character(len=:), allocatable :: fpm_driver

contains

  ! Retrieve an environment parameter or its default value
  subroutine getenv_withdefault(key, default, result)
    use iso_fortran_env, only: error_unit
    character(len=*), intent(in) :: key, default
    character(len=:), allocatable, intent(inout) :: result
    character(len=:), allocatable :: suffix

    character :: dummy
    integer :: len

    ! TODO: it would be preferable to consult the GASNet global environment, when available
    call get_environment_variable(key, dummy, len)
    if (len > 0) then
      allocate(character(len=len)::result)
      call get_environment_variable(key, result, len)
      result = trim(adjustl(result))
      suffix = ""
    else 
      result = default
      suffix = " (default)"
    endif

    ! report the envvar in verbose mode
    call get_environment_variable("GASNET_VERBOSEENV", dummy, len)
    if (len > 0) then
      write(error_unit, '(A, T64, A)') "ENV parameter: "//key//"='"//result//"'", suffix
    end if
  end subroutine

  ! subjob support used by stop/error-stop unit tests
  ! setup for subjob launch, initializes subjob_prefix and
  ! returns whether this is the first image
  function subjob_setup() result(result_)
    logical :: result_
    integer :: me
  
    if (.not. allocated(subjob_prefix)) then
      call getenv_withdefault("SUBJOB_PREFIX", "", subjob_prefix)
      if (len(subjob_prefix) > 0) subjob_prefix = subjob_prefix//" "
    end if
    if (.not. allocated(fpm_driver)) then
      call getenv_withdefault("FPM_DRIVER", "./run-fpm.sh", fpm_driver)
    end if
        
    call prif_sync_all()
    call prif_this_image_no_coarray(this_image=me)
    result_ = (me == 1) .and. (trim(subjob_prefix) /= "skip")
  end function 


end module unit_test_parameters_m
