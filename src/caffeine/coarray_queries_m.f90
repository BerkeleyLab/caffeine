! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module coarray_queries_m
  use iso_c_binding, only: c_int, c_intmax_t, c_size_t
  use allocation_m, only: prif_coarray_handle
  use teams_m, only: prif_team_type

  implicit none
  private
  public :: prif_lcobound, prif_ucobound, prif_coshape, prif_image_index

  interface prif_lcobound
     module procedure prif_lcobound_with_dim
     module procedure prif_lcobound_no_dim
  end interface

  interface prif_ucobound
     module procedure prif_ucobound_with_dim
     module procedure prif_ucobound_no_dim
  end interface

  interface

     module subroutine prif_lcobound_with_dim(coarray_handle, dim, lcobound)
       type(prif_coarray_handle), intent(in) :: coarray_handle
       integer(kind=c_int), intent(in) :: dim
       integer(kind=c_intmax_t), intent(out) :: lcobound
     end subroutine

     module subroutine prif_lcobound_no_dim(coarray_handle, lcobounds)
       type(prif_coarray_handle), intent(in) :: coarray_handle
       integer(kind=c_intmax_t), intent(out) :: lcobounds(:)
     end subroutine

     module subroutine prif_ucobound_with_dim(coarray_handle, dim, ucobound)
       type(prif_coarray_handle), intent(in) :: coarray_handle
       integer(kind=c_int), intent(in) :: dim
       integer(kind=c_intmax_t), intent(out) :: ucobound
     end subroutine

     module subroutine prif_ucobound_no_dim(coarray_handle, ucobounds)
       type(prif_coarray_handle), intent(in) :: coarray_handle
       integer(kind=c_intmax_t), intent(out) :: ucobounds(:)
     end subroutine

     module subroutine prif_coshape(coarray_handle, sizes)
       implicit none
       type(prif_coarray_handle), intent(in) :: coarray_handle
       integer(c_size_t), intent(out) :: sizes(:)
     end subroutine

     module subroutine prif_image_index(coarray_handle, sub, team, team_number, image_index)
       implicit none
       type(prif_coarray_handle), intent(in) :: coarray_handle
       integer(c_intmax_t), intent(in) :: sub(:)
       type(prif_team_type), intent(in), optional :: team
       integer(c_int), intent(in), optional :: team_number
       integer(c_int), intent(out) :: image_index
     end subroutine

  end interface

end module coarray_queries_m
