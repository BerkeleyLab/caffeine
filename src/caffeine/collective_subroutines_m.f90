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
       class(*), intent(inout) :: a(..)
       integer, optional, intent(in) :: result_image
       integer, optional, intent(out) :: stat
       character(len=*), intent(inout), optional :: errmsg
     end subroutine

     module subroutine caf_co_max(a, result_image, stat, errmsg)
       implicit none
       class(*), intent(inout) :: a(..)
       integer, optional, intent(in) :: result_image
       integer, optional, intent(out) :: stat
       character(len=*), intent(inout), optional :: errmsg
     end subroutine

     module subroutine caf_co_min(a, result_image, stat, errmsg)
       implicit none
       class(*), intent(inout) :: a(..)
       integer, optional, intent(in) :: result_image
       integer, optional, intent(out) :: stat
       character(len=*), intent(inout), optional :: errmsg
     end subroutine

     module subroutine caf_co_reduce(a, result_image, stat, errmsg)
       implicit none
       class(*), intent(inout) :: a(..)
       integer, optional, intent(in) :: result_image
       integer, optional, intent(out) :: stat
       character(len=*), intent(inout), optional :: errmsg
     end subroutine

     module subroutine caf_co_broadcast(a, source_image, stat, errmsg)
       implicit none
       class(*), intent(inout) :: a(..)
       integer, optional, intent(in) :: source_image
       integer, optional, intent(out) :: stat
       character(len=*), intent(inout), optional :: errmsg
     end subroutine

  end interface

end module
