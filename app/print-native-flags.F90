program print_native_flags
  use iso_fortran_env, only: COMPILER_VERSION, COMPILER_OPTIONS, error_unit
  implicit none

  character(:), allocatable :: flags
  logical :: stand_alone

#if VERBOSE
  logical, parameter :: verbose = .true.
#else
  logical, parameter :: verbose = .false.
#endif
  if (verbose) then
    write(error_unit,'(A,A)') "COMPILER_VERSION=", COMPILER_VERSION()
    write(error_unit,'(A,A)') "COMPILER_OPTIONS=", COMPILER_OPTIONS()
  end if

  stand_alone = COMMAND_ARGUMENT_COUNT() > 0

  call write_flags

contains
subroutine write_flags
#if __flang__
#  if __flang_major__ == 22
     call set("-fcoarray")

     call no("COARRAY")

     ! issue #205953
     call no("NUM_IMAGES_TEAM")
     call no("THIS_IMAGE_TEAM")
     call no("TEAM_NUMBER_TEAM")

     call yes("EVENT_TYPE")
     call yes("LOCK_TYPE")
     call yes("NOTIFY_TYPE")

     call set("-DIGNORE_FAILURES=8") ! type checks for event, notify, lock, team
#  elif __flang_major__ == 23
     call set("-fcoarray")

     call no("COARRAY_INIT")
     call no("COARRAY_LOCAL_ACCESS")
     call no("PUTGET")

     call no("EVENT")
     call  yes("EVENT_TYPE")
     call no("LOCK")
     call  yes("LOCK_TYPE")
     call no("NOTIFY")
     call  yes("NOTIFY_TYPE")

     call set("-DIGNORE_FAILURES=6") ! type checks for event, notify, lock
#  elif __flang_major__ >= 24
     call set("-fcoarray")

     call no("COARRAY_INIT")
     call no("COARRAY_LOCAL_ACCESS")
     call no("PUTGET")

     call no("EVENT")
     call  yes("EVENT_TYPE")
     call no("LOCK")
     call  yes("LOCK_TYPE")
     call no("NOTIFY")
     call  yes("NOTIFY_TYPE")

     call set("-DIGNORE_FAILURES=6") ! type checks for event, notify, lock
#  endif
#elif __LFORTRAN__
#  if   __LFORTRAN_MAJOR__ == 0 && __LFORTRAN_MINOR__ <= 63
   ! no multi-image support before 0.64.0
#  else
     call set("--coarray=true")
     call no("EVENT")
     call no("LOCK")
     call no("NOTIFY")
#    if  __LFORTRAN_MAJOR__ == 0 && __LFORTRAN_MINOR__ == 64
     call no("TEAM")

     call no("CO_BROADCAST_DERIVED_POD")

     call no("ALLOC_COARRAY")
     call no("COARRAY_QUERY")
     call no("PUTGET_INTRINSIC_ARRAY_CONTIG")
#    elif  __LFORTRAN_MAJOR__ == 0 && __LFORTRAN_MINOR__ == 65
     call no("GET_TEAM")
     call no("NUM_IMAGES_TEAM")
     call no("THIS_IMAGE_TEAM")
     call no("TEAM_NUMBER")

     call no("CO_BROADCAST_DERIVED_POD")

     call no("ALLOC_COARRAY_CLEANUP")
     call no("IMAGE_INDEX")
     call no("THIS_IMAGE_COARRAY")
     call no("PUTGET_INTRINSIC_ARRAY_CONTIG")
#    else
   if (INDEX(COMPILER_VERSION(), 'version 0.66') /= 0 .and. &
       INDEX(COMPILER_VERSION(), '-g') == 0) then 
     ! LFortran release 0.66
     call no("GET_TEAM")
     call no("NUM_IMAGES_TEAM")
     call no("THIS_IMAGE_TEAM")
     call no("TEAM_NUMBER")

     call no("ALLOC_COARRAY_CLEANUP")
     call no("IMAGE_INDEX")
     call no("THIS_IMAGE_COARRAY")
     call no("PUTGET_INTRINSIC_ARRAY_CONTIG")
   else 
     ! LFortran git snapshot or newer, assume latest we know about
     call no("GET_TEAM")
     call no("NUM_IMAGES_TEAM")
     call no("THIS_IMAGE_TEAM")
     call no("TEAM_NUMBER")

     call no("ALLOC_COARRAY_CLEANUP")
     call no("IMAGE_INDEX")
     call no("THIS_IMAGE_COARRAY")
     call no("PUTGET_INTRINSIC_ARRAY_CONTIG")
   end if
#  endif
#  endif
#elif NAGFOR
   if (.not. stand_alone) return
#  if __NAG_COMPILER_RELEASE >= 72
     ! __NAG_COMPILER_BUILD contains build number
     !call set("-coarray=cosmp")     ! set in Makefile
     call set("-DTYPES_PRIF_COMPLIANT=0")

     call no("NOTIFY") ! missing F2023 feature
#  endif
#elif __GFORTRAN__
   if (.not. stand_alone) return
#  if __GNUC__ >= 16
     !call set("-fcoarray=lib")   ! set in Makefile
     call set("-DTYPES_PRIF_COMPLIANT=0")

     call no("NOTIFY") ! missing F2023 feature
     call no("IMAGE_INDEX_TEAM_NUMBER") ! https://gcc.gnu.org/bugzilla/show_bug.cgi?id=126777
     call no("NUM_IMAGES_TEAM")         ! https://gcc.gnu.org/bugzilla/show_bug.cgi?id=126781 
     call no("CO_MIN") ! https://gcc.gnu.org/bugzilla/show_bug.cgi?id=126776
     call no("CO_MAX") ! CO_MIN/CO_MAX(character) runtime crash
#  endif
#elif _CRAYFTN
   if (.not. stand_alone) return
   ! More details in _RELEASE_MINOR, _RELEASE_PATCHLEVEL, _RELEASE_STRING
#  if _RELEASE_MAJOR >= 18
     !call set("-hcaf")   ! set in Makefile
#    if _RELEASE_MAJOR < 19
       call no("NOTIFY") ! missing F2023 feature
#    endif
     call set("-DTYPES_PRIF_COMPLIANT=0")
     call no("COSHAPE") ! missing F2018 feature
     call set("-DIGNORE_FAILURES=2") ! CO_MIN(character) get wrong answer with > 9 images
#  endif
#elif __INTEL_COMPILER
   if (.not. stand_alone) return
#  if __INTEL_COMPILER >= 20250302
     !call set("-coarray")   ! set in Makefile
     call set("-DTYPES_PRIF_COMPLIANT=0")

     call no("NOTIFY") ! missing F2023 feature
     call no("TEAM_TYPE") ! avoid runtime errors from CHECK_TYPE_COMPLIANCE
     call set("-DIGNORE_FAILURES=4") ! CO_MIN/CO_MAX(character) get the wrong answer at runtime (no change)
#  endif
#endif

  if (allocated(flags)) write(*,'(A)') flags
end subroutine
subroutine yes(flag)
  character(*), intent(in) :: flag
  call define(flag, .true.)
end subroutine
subroutine no(flag)
  character(*), intent(in) :: flag
  call define(flag, .false.)
end subroutine
subroutine define(flag,val)
  character(*), intent(in) :: flag
  logical, intent(in) :: val
  character(:), allocatable :: tmp
  if (INDEX(flag,"HAVE") > 0 .or. INDEX(flag,"-D") > 0) error stop flag
  allocate(character(0) :: tmp)
  tmp = "-DHAVE_"
  tmp = tmp // flag
  if (.not. val) tmp = tmp // "=0"
  call set(tmp)
end subroutine
subroutine set(flag)
  character(*), intent(in) :: flag
  if (.not. allocated(flags)) then
    allocate(character(0) :: flags)
    flags = "-DHAVE_MULTI_IMAGE"
  end if
  flags = flags // " " // flag
end subroutine

end program
