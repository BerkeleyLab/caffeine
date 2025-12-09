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

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  integer :: me, ni, peer, tmp
# if HAVE_TEAM
  integer :: team_id
# endif
  character(len=5) :: c
# if HAVE_TEAM
  type(team_type) :: subteam, res
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
  call status("Testing TEAMS...")
  res = GET_TEAM(CURRENT_TEAM)
  res = GET_TEAM(INITIAL_TEAM)
  res = GET_TEAM()
  write(*,'(A,I3)') "Initial team number is ", TEAM_NUMBER()

  if (ni == 1) then
    team_id = ni
  else
    team_id = merge(1, 2, me <= ni/2)
  end if

  FORM TEAM(team_id, subteam)
  SYNC TEAM(subteam)
  CHANGE TEAM(subteam)
    write(*,'(A,I3,A,I3,A,I3)') 'Inside CHANGE TEAM construct: ', THIS_IMAGE(), ' of ', NUM_IMAGES(), ' in team number ', TEAM_NUMBER()
  END TEAM
  call sync_all
  write(*,'(A,I3)') "After END TEAM statement, TEAM_NUMBER() is ", TEAM_NUMBER()
# endif

  call sync_all
  write(*,'(A,I1,A,I1,A)') "Goodbye from image ", me, " of ", ni, " images"

  ! explicit flush for now until we have multi-image stop support
  call flush_all
  call sync_all
  stop

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
      if (THIS_IMAGE() == 1) write(*,*) str
      call flush_all
      call sync_all
    end subroutine

#else
  stop "Native multi-image test disabled"
#endif
end program
