! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module notify_m
  use iso_c_binding, only: c_intmax_t, c_int, c_ptr

  implicit none
  private
  public :: prif_notify_type, prif_notify_wait

  type :: prif_notify_type
  end type

  interface

     module subroutine prif_notify_wait(notify_var_ptr, until_count, stat, errmsg, errmsg_alloc)
       implicit none
       type(c_ptr), intent(in) :: notify_var_ptr
       integer(c_intmax_t), intent(in), optional :: until_count
       integer(c_int), intent(out), optional :: stat
       character(len=*), intent(inout), optional :: errmsg
       character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
     end subroutine

  end interface

end module notify_m
