! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module collective_subroutines_m 
  implicit none

  private
  public :: caf_co_sum
  public :: caf_co_max
  public :: caf_co_min
  public :: caf_co_reduce
  public :: caf_co_broadcast

  interface
 
     module subroutine caf_co_sum(a, result_image, stat, errmsg)
       implicit none
       class(*), intent(inout), contiguous, target :: a(..)
       integer, intent(in), target, optional :: result_image
       integer, intent(out), target, optional :: stat
       character(len=*), intent(inout), target, optional :: errmsg
     end subroutine

     module subroutine caf_co_max(a, result_image, stat, errmsg)
       implicit none
       class(*), intent(inout), contiguous, target :: a(..)
       integer, intent(in), optional, target :: result_image
       integer, intent(out), optional, target :: stat
       character(len=*), intent(inout), optional, target :: errmsg
     end subroutine

     module subroutine caf_co_min(a, result_image, stat, errmsg)
       implicit none
       class(*), intent(inout), contiguous, target :: a(..)
       integer, intent(in), optional, target :: result_image
       integer, intent(out), optional, target :: stat
       character(len=*), intent(inout), optional, target :: errmsg
     end subroutine

     module subroutine caf_co_reduce(a, result_image, stat, errmsg)
       implicit none
       class(*), intent(inout), contiguous, target :: a(..)
       integer, intent(in), optional :: result_image
       integer, intent(out), optional, target :: stat
       character(len=*), intent(inout), optional, target :: errmsg
     end subroutine

     module subroutine caf_co_broadcast(a, source_image, stat, errmsg)
       implicit none
       class(*), intent(inout), contiguous, target :: a(..)
       integer, optional, intent(in) :: source_image
       integer, optional, intent(out), target :: stat
       character(len=*), intent(inout), optional, target :: errmsg
     end subroutine

  end interface

end module
