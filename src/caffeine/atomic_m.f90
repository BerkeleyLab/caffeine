! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module atomic_m
  use iso_c_binding, only: c_intptr_t, c_int
  use iso_fortran_env, only: atomic_int_kind, atomic_logical_kind

  implicit none
  private
  public :: prif_atomic_add, prif_atomic_and, prif_atomic_or, prif_atomic_xor, prif_atomic_cas, prif_atomic_fetch_add
  public :: prif_atomic_fetch_and, prif_atomic_fetch_or, prif_atomic_fetch_xor, prif_atomic_define, prif_atomic_ref

  interface prif_atomic_cas
     module procedure prif_atomic_cas_int
     module procedure prif_atomic_cas_logical
  end interface

  interface prif_atomic_define
     module procedure prif_atomic_define_int
     module procedure prif_atomic_define_logical
  end interface

  interface prif_atomic_ref
     module procedure prif_atomic_ref_int
     module procedure prif_atomic_ref_logical
  end interface

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

     module subroutine prif_atomic_or(atom_remote_ptr, image_num, value, stat)
       implicit none
       integer(c_intptr_t), intent(in) :: atom_remote_ptr
       integer(c_int), intent(in) :: image_num
       integer(atomic_int_kind), intent(in) :: value
       integer(c_int), intent(out), optional :: stat
     end subroutine

     module subroutine prif_atomic_xor(atom_remote_ptr, image_num, value, stat)
       implicit none
       integer(c_intptr_t), intent(in) :: atom_remote_ptr
       integer(c_int), intent(in) :: image_num
       integer(atomic_int_kind), intent(in) :: value
       integer(c_int), intent(out), optional :: stat
     end subroutine

     module subroutine prif_atomic_cas_int(atom_remote_ptr, image_num, old, compare, new, stat)
       implicit none
       integer(c_intptr_t), intent(in) :: atom_remote_ptr
       integer(c_int), intent(in) :: image_num
       integer(atomic_int_kind), intent(out) :: old
       integer(atomic_int_kind), intent(in) :: compare
       integer(atomic_int_kind), intent(in) :: new
       integer(c_int), intent(out), optional :: stat
     end subroutine

     module subroutine prif_atomic_cas_logical(atom_remote_ptr, image_num, old, compare, new, stat)
       implicit none
       integer(c_intptr_t), intent(in) :: atom_remote_ptr
       integer(c_int), intent(in) :: image_num
       logical(atomic_logical_kind), intent(out) :: old
       logical(atomic_logical_kind), intent(in) :: compare
       logical(atomic_logical_kind), intent(in) :: new
       integer(c_int), intent(out), optional :: stat
     end subroutine

     module subroutine prif_atomic_fetch_add(atom_remote_ptr, image_num, value, old, stat)
       implicit none
       integer(c_intptr_t), intent(in) :: atom_remote_ptr
       integer(c_int), intent(in) :: image_num
       integer(atomic_int_kind), intent(in) :: value
       integer(atomic_int_kind), intent(out) :: old
       integer(c_int), intent(out), optional :: stat
     end subroutine

     module subroutine prif_atomic_fetch_and(atom_remote_ptr, image_num, value, old, stat)
       implicit none
       integer(c_intptr_t), intent(in) :: atom_remote_ptr
       integer(c_int), intent(in) :: image_num
       integer(atomic_int_kind), intent(in) :: value
       integer(atomic_int_kind), intent(out) :: old
       integer(c_int), intent(out), optional :: stat
     end subroutine

     module subroutine prif_atomic_fetch_or(atom_remote_ptr, image_num, value, old, stat)
       implicit none
       integer(c_intptr_t), intent(in) :: atom_remote_ptr
       integer(c_int), intent(in) :: image_num
       integer(atomic_int_kind), intent(in) :: value
       integer(atomic_int_kind), intent(out) :: old
       integer(c_int), intent(out), optional :: stat
     end subroutine

     module subroutine prif_atomic_fetch_xor(atom_remote_ptr, image_num, value, old, stat)
       implicit none
       integer(c_intptr_t), intent(in) :: atom_remote_ptr
       integer(c_int), intent(in) :: image_num
       integer(atomic_int_kind), intent(in) :: value
       integer(atomic_int_kind), intent(out) :: old
       integer(c_int), intent(out), optional :: stat
     end subroutine

     module subroutine prif_atomic_define_int(atom_remote_ptr, image_num, value, stat)
       implicit none
       integer(c_intptr_t), intent(in) :: atom_remote_ptr
       integer(c_int), intent(in) :: image_num
       integer(atomic_int_kind), intent(in) :: value
       integer(c_int), intent(out), optional :: stat
     end subroutine

     module subroutine prif_atomic_define_logical(atom_remote_ptr, image_num, value, stat)
       implicit none
       integer(c_intptr_t), intent(in) :: atom_remote_ptr
       integer(c_int), intent(in) :: image_num
       logical(atomic_logical_kind), intent(in) :: value
       integer(c_int), intent(out), optional :: stat
     end subroutine

     module subroutine prif_atomic_ref_int(value, atom_remote_ptr, image_num, stat)
       implicit none
       integer(atomic_int_kind), intent(out) :: value
       integer(c_intptr_t), intent(in) :: atom_remote_ptr
       integer(c_int), intent(in) :: image_num
       integer(c_int), intent(out), optional :: stat
     end subroutine

     module subroutine prif_atomic_ref_logical(value, atom_remote_ptr, image_num, stat)
       implicit none
       logical(atomic_logical_kind), intent(out) :: value
       integer(c_intptr_t), intent(in) :: atom_remote_ptr
       integer(c_int), intent(in) :: image_num
       integer(c_int), intent(out), optional :: stat
     end subroutine

  end interface

end module atomic_m
