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

contains

  ! subjob support used by stop/error-stop unit tests
  ! setup for subjob launch, initializes subjob_prefix and
  ! returns whether this is the first image
  function subjob_setup() result(result_)
    character(len=*), parameter :: envvar = "SUBJOB_PREFIX"
    logical :: result_
    integer :: me, len
    character :: dummy
  
    if (.not. allocated(subjob_prefix)) then
      call get_environment_variable(envvar, dummy, len)
      if (len > 0) then
        allocate(character(len=len+1)::subjob_prefix)
        call get_environment_variable(envvar, subjob_prefix, len)
      else 
        subjob_prefix = ""
      endif
      !print *,"SUBJOB_PREFIX='"//subjob_prefix//"' len=",len
    end if
        
    call prif_sync_all()
    call prif_this_image_no_coarray(this_image=me)
    result_ = (me == 1) .and. (subjob_prefix /= "skip")
  end function 


end module unit_test_parameters_m
