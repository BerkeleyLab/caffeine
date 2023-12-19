! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module events_m
  use iso_c_binding, only: c_intptr_t, c_int, c_intmax_t

  implicit none
  private
  public :: prif_event_post, prif_event_wait, prif_event_query

  interface

     module subroutine prif_event_post(image_num, event_var_ptr, stat, errmsg, errmsg_alloc)
       implicit none
       integer(c_int), intent(in) :: image_num
       integer(c_intptr_t), intent(in) :: event_var_ptr
       integer(c_int), intent(out), optional :: stat
       character(len=*), intent(inout), optional :: errmsg
       character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
     end subroutine

     module subroutine prif_event_wait(event_var_ptr, until_count, stat, errmsg, errmsg_alloc)
       implicit none
       integer(c_intptr_t), intent(in) :: event_var_ptr
       integer(c_intmax_t), intent(in), optional :: until_count
       integer(c_int), intent(out), optional :: stat
       character(len=*), intent(inout), optional :: errmsg
       character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
     end subroutine

     module subroutine prif_event_query(event_var_ptr, count, stat)
       implicit none
       integer(c_intptr_t), intent(in) :: event_var_ptr
       integer(c_intmax_t), intent(out) :: count
       integer(c_int), intent(out), optional :: stat
     end subroutine

  end interface

end module events_m
