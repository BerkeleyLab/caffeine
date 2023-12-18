! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module atomic_m
  use iso_c_binding, only: c_intptr_t, c_int
  use iso_fortran_env, only: atomic_int_kind

  implicit none
  private
  public :: prif_atomic_add, prif_atomic_and

  interface

     module subroutine prif_atomic_add(atom_remote_ptr, image_num, value, stat)
       implicit none
       integer(c_intptr_t), intent(in) :: atom_remote_ptr
       integer(c_int), intent(in) :: image_num
       integer(atomic_int_kind), intent(in) :: value
       integer(c_int), intent(out), optional :: stat
     end subroutine

     module subroutine prif_atomic_and(atom_remote_ptr, image_num, value, stat)
       implicit none
       integer(c_intptr_t), intent(in) :: atom_remote_ptr
       integer(c_int), intent(in) :: image_num
       integer(atomic_int_kind), intent(in) :: value
       integer(c_int), intent(out), optional :: stat
     end subroutine

  end interface

end module atomic_m
