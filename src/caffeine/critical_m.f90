! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module critical_m
  use allocation_m, only: prif_coarray_handle
  use iso_c_binding, only: c_int

  implicit none
  private
  public :: prif_critical, prif_end_critical

  interface

     module subroutine prif_critical(critical_coarray, stat, errmsg, errmsg_alloc)
       implicit none
       type(prif_coarray_handle), intent(in) :: critical_coarray
       integer(c_int), intent(out), optional :: stat
       character(len=*), intent(inout), optional :: errmsg
       character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
     end subroutine

     module subroutine prif_end_critical(critical_coarray)
       implicit none
       type(prif_coarray_handle), intent(in) :: critical_coarray
     end subroutine

  end interface

end module critical_m
