! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module locks_m
  use iso_c_binding, only: c_intptr_t, c_int, c_bool

  implicit none
  private
  public :: prif_lock_type, prif_lock, prif_unlock

  type :: prif_lock_type
  end type

  interface

     module subroutine prif_lock(image_num, lock_var_ptr, acquired_lock, stat, errmsg, errmsg_alloc)
       implicit none
       integer(c_int), intent(in) :: image_num
       integer(c_intptr_t), intent(in) :: lock_var_ptr
       logical(c_bool), intent(out), optional :: acquired_lock
       integer(c_int), intent(out), optional :: stat
       character(len=*), intent(inout), optional :: errmsg
       character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
     end subroutine

     module subroutine prif_unlock(image_num, lock_var_ptr, stat, errmsg, errmsg_alloc)
       implicit none
       integer(c_int), intent(in) :: image_num
       integer(c_intptr_t), intent(in) :: lock_var_ptr
       integer(c_int), intent(out), optional :: stat
       character(len=*), intent(inout), optional :: errmsg
       character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
     end subroutine

  end interface

end module locks_m
