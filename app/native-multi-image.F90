! This multi-image Fortran program just exercises basic calls into each of the
! native multi-image features from the Fortran level.
! This test requires a compiler with multi-image features (possibly via Caffeine).
! This program is NOT designed to evaluate runtime correctness, just to exercise
! some basic calls to the features.

program native_multi_image

#if HAVE_MULTI_IMAGE
! feature control:
#ifndef HAVE_SYNC
#define HAVE_SYNC 1
#endif
#ifndef HAVE_SYNC_ALL
#define HAVE_SYNC_ALL HAVE_SYNC
#endif
#ifndef HAVE_SYNC_MEMORY
#define HAVE_SYNC_MEMORY HAVE_SYNC
#endif
#ifndef HAVE_SYNC_IMAGES
#define HAVE_SYNC_IMAGES HAVE_SYNC
#endif

#ifndef HAVE_COLLECTIVES
#define HAVE_COLLECTIVES 1
#endif
#ifndef HAVE_CO_SUM
#define HAVE_CO_SUM HAVE_COLLECTIVES
#endif
#ifndef HAVE_CO_MIN
#define HAVE_CO_MIN HAVE_COLLECTIVES
#endif
#ifndef HAVE_CO_MAX
#define HAVE_CO_MAX HAVE_COLLECTIVES
#endif
#ifndef HAVE_CO_BROADCAST
#define HAVE_CO_BROADCAST HAVE_COLLECTIVES
#endif

#ifndef HAVE_TEAM
#define HAVE_TEAM 1
#endif
#ifndef HAVE_TEAM_TYPE
#define HAVE_TEAM_TYPE HAVE_TEAM
#endif
#ifndef HAVE_EVENT_TYPE
#define HAVE_EVENT_TYPE 1
#endif
#ifndef HAVE_LOCK_TYPE
#define HAVE_LOCK_TYPE 1
#endif
#ifndef HAVE_NOTIFY_TYPE
#define HAVE_NOTIFY_TYPE 1
#endif
! TYPES_IMPORT_PRIF: compiler imports the real PRIF definition of ISO_FORTRAN_ENV types
#ifndef TYPES_IMPORT_PRIF
#define TYPES_IMPORT_PRIF 0
#endif

#ifndef HAVE_COARRAY
#define HAVE_COARRAY 0
#endif
#ifndef HAVE_MAIN_COARRAY
#define HAVE_MAIN_COARRAY HAVE_COARRAY
#endif
#ifndef HAVE_ALLOC_COARRAY
#define HAVE_ALLOC_COARRAY HAVE_COARRAY
#endif

! Helper macros
#define CHECK_TYPE_COMPLIANCE(subject_type, subject, is_team, min_size) \
  BLOCK ; \
    integer(c_int8_t), allocatable, target :: bytes(:) ; \
    bytes = transfer(subject, bytes) ; \
    call check_type(#subject_type, is_team, min_size, \
                    storage_size(subject)/8, bytes); \
  END BLOCK

! Main program
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE, INTRINSIC :: ISO_C_BINDING, only: c_int8_t

  type :: dummy_team_descriptor
  end type
  type :: dummy_team_type
    type(dummy_team_descriptor), pointer :: info => null()
  end type

  integer :: me, ni, peer, tmp, fail_count = 0
  character(len=5) :: c
# if HAVE_TEAM
  integer :: team_id
  type(TEAM_TYPE) :: subteam, res
  type(TEAM_TYPE) :: default_team
# endif
# if HAVE_MAIN_COARRAY
  integer :: sca_int_1[*]
  integer :: sca_int_2[2,*]
  integer :: sca_int_3[2,3,*]
# endif
# if HAVE_EVENT_TYPE
      type(event_type), target :: default_event[*]
# endif
# if HAVE_NOTIFY_TYPE
      type(notify_type), target :: default_notify[*]
# endif
# if HAVE_LOCK_TYPE
      type(lock_type), target :: default_lock[*]
# endif

  me = THIS_IMAGE()
  ni = NUM_IMAGES()
  peer = MIN(IEOR(me-1,1)+1, ni)

  write(*,'(A,I1,A,I1,A)') "Hello, world! From image ", me, " of ", ni, " images"

# if SET_EXCEPTIONS
    block 
       ! deliberately trigger IEEE arithmetic exceptions: INEXACT and UNDERFLOW
       real :: r
       r = 1e-30
       r = r + r * r
      write (*,*) r
    end block
# endif

# if HAVE_SYNC_ALL
    call status("Testing SYNC ALL...")
    call sync_all
# endif

# if HAVE_SYNC_MEMORY
    call status("Testing SYNC MEMORY...")
    SYNC MEMORY
# endif

# if HAVE_SYNC_IMAGES
    call status("Testing SYNC IMAGES...")
    SYNC IMAGES(*)
    SYNC IMAGES(peer)
    SYNC IMAGES([peer])
    if (me /= peer) SYNC IMAGES([me, peer])
#endif

  tmp = me
  c = "hello"
# if HAVE_CO_SUM
    call status("Testing CO_SUM...")
    call CO_SUM(tmp)
    call CO_SUM(tmp,1)
# endif
# if HAVE_CO_MIN
    call status("Testing CO_MIN...")
    call CO_MIN(tmp)
    call CO_MIN(tmp,1)
    call CO_MIN(c)
    call CO_MIN(c,1)
# endif
# if HAVE_CO_MAX
    call status("Testing CO_MAX...")
    call CO_MAX(tmp)
    call CO_MAX(tmp,1)
    call CO_MAX(c)
    call CO_MAX(c,1)
# endif
# if HAVE_CO_BROADCAST
    call status("Testing CO_BROADCAST...")
    call CO_BROADCAST(tmp,1)
    call CO_BROADCAST(c,1)
# endif

# if HAVE_TEAM
#   if HAVE_TEAM_TYPE
      CHECK_TYPE_COMPLIANCE(TEAM_TYPE, default_team, .true., 0)
#   endif
  call status("Testing TEAMS...")
  res = GET_TEAM(CURRENT_TEAM)
  res = GET_TEAM(INITIAL_TEAM)
  res = GET_TEAM()
  write(*,'(A,I3)') "Initial team number is ", TEAM_NUMBER()

    team_id = merge(1, 2, me <= (ni+1)/2)

  FORM TEAM(team_id, subteam)
  SYNC TEAM(subteam)
  CHANGE TEAM(subteam)
    write(*,'(A,I3,A,I3,A,I3)') 'Inside CHANGE TEAM construct: ', THIS_IMAGE(), ' of ', NUM_IMAGES(), ' in team number ', TEAM_NUMBER()
  END TEAM
  call sync_all
  write(*,'(A,I3)') "After END TEAM statement, TEAM_NUMBER() is ", TEAM_NUMBER()
# endif

# if HAVE_EVENT_TYPE
  CHECK_TYPE_COMPLIANCE(EVENT_TYPE, default_event, .false., 64)
# endif

# if HAVE_LOCK_TYPE
  CHECK_TYPE_COMPLIANCE(LOCK_TYPE, default_lock, .false., 64)
# endif

# if HAVE_NOTIFY_TYPE
  CHECK_TYPE_COMPLIANCE(NOTIFY_TYPE, default_notify, .false., 64)
# endif

  call sync_all
  call test_allocatable_coarray
  call test_allocatable_coarray

  call sync_all
  write(*,'(A,I1,A,I1,A)') "Goodbye from image ", me, " of ", ni, " images"

  ! explicit flush for now until we have multi-image stop support
  call flush_all
  call sync_all
  if (fail_count > 0) then
    call status("ERROR: "//tostring(fail_count)//" tests FAILED.")
  else
    call status("All tests passed.")
  end if
#if IGNORE_FAILURES
  call status("WARNING: Ignoring "//tostring(IGNORE_FAILURES)//" failures.")
  fail_count = MAX(0, fail_count - IGNORE_FAILURES)
#endif
  stop fail_count

  contains
    subroutine sync_all
#   if HAVE_SYNC_ALL
      SYNC ALL
#   endif
    end subroutine
    subroutine flush_all
      flush output_unit
      flush error_unit
    end subroutine
    subroutine status(str)
      character(len=*) :: str
      call flush_all
      call sync_all
      if (THIS_IMAGE() == 1) write(*,'(A)') str
      call flush_all
      call sync_all
    end subroutine

    subroutine test_allocatable_coarray()
#   if HAVE_ALLOC_COARRAY
#   define CHECK_ALLOC(coarray, expect) \
      if (ALLOCATED(coarray) .neqv. expect) then ; \
        if (THIS_IMAGE() == 1) write(*,'(A)') __FILE__//":"//tostring(__LINE__)//": ERROR: " // \
           " ALLOCATED(" // #coarray // ") = " // MERGE("true ","false",ALLOCATED(coarray)) ; \
        fail_count = fail_count + 1 ; \
      end if

      logical, save :: once = .true.
      integer, allocatable :: aca_int_1[:]
      integer, allocatable :: aca_int_2[:,:]
      integer, save, allocatable :: aca_int_3[:,:,:]
      CHECK_ALLOC(aca_int_1, .false.)
      CHECK_ALLOC(aca_int_2, .false.)
      CHECK_ALLOC(aca_int_3, .false.)
      if (once) then
        once = .false.
        call status("Testing ALLOCATABLE coarrays...")
        ALLOCATE(aca_int_1[*])
        ALLOCATE(aca_int_2[2,*])
        ALLOCATE(aca_int_3[2,3,*])
        CHECK_ALLOC(aca_int_1, .true.)
        CHECK_ALLOC(aca_int_2, .true.)
        CHECK_ALLOC(aca_int_3, .true.)
      end if
#   endif
    end subroutine

    function tostring(int) result(res)
      integer :: int
      character(len=128) :: str
      character(len=:), allocatable :: res
      write(str, *) int
      res = trim(adjustl(str))
    end function

    function hexdump(arr) result(res)
      integer(c_int8_t), intent(in) :: arr(:)
      character(len=:), allocatable :: res
      character(len=4096) :: buf
      write(buf, '(*(Z2, 1X))') arr
      res = trim(buf)
    end function

    subroutine check_type(type_name, is_team, min_size, subject_size, default_bytes)
      character(len=*), intent(in) :: type_name
      logical, intent(in) :: is_team
      integer, intent(in) :: min_size, subject_size
      integer(c_int8_t), target, intent(in) :: default_bytes(:)
      character(len=:), allocatable :: diag
#   if HAVE_TEAM
      type(TEAM_TYPE) :: team_var
      type(dummy_team_type) :: dummy_team_type_var
      integer, parameter :: reference_size = storage_size(dummy_team_type_var)/8
#   endif

      call status("Testing " // type_name // "...")

      if (subject_size /= size(default_bytes)) ERROR STOP "INTERNAL ERROR: representation size mismatch"

      if (is_team) then
#     if HAVE_TEAM
        ! check size, should be an exact match
        if (subject_size == reference_size) then
          diag = "pass"
        else
          diag = "FAIL (should be exactly " // tostring(reference_size) // " bytes)"
          fail_count = fail_count + 1
        end if
        call status("  Size of " // type_name // ": " // tostring(subject_size) // " bytes ==> " // diag)

        ! check default initialization
        dummy_team_type_var = transfer(team_var, dummy_team_type_var)
        if (.not. associated(dummy_team_type_var%info)) then
          diag = "pass"
        else
          diag = "FAIL (not default-initialized to null(): " // hexdump(default_bytes)// ")"
          fail_count = fail_count + 1
        end if
        call status("  Default init of " // type_name // " ==> " // diag)
#     endif
      else
#     if TYPES_IMPORT_PRIF
        diag = "(validation skipped)"
#     else
        ! check size, should not be less than min_size
        if (subject_size >= min_size) then
          diag = "pass"
        else
          diag = "FAIL (should be >= " // tostring(min_size) // " bytes)"
          fail_count = fail_count + 1
        end if
#     endif
        call status("  Size of " // type_name // ": " // tostring(subject_size) // " bytes ==> " // diag)

        ! check default initialization
        if (all(default_bytes == 0)) then
          diag = "pass"
        else
          diag = "FAIL (non-zero value at byte " // tostring((findloc(default_bytes /= 0, .true., dim=1))) // ": " // &
                 hexdump(default_bytes) // ")"
          fail_count = fail_count + 1
        end if
        call status("  Default init of " // type_name // " ==> " // diag)
      end if
    end subroutine

#else
  stop "Native multi-image test disabled"
#endif
end program
