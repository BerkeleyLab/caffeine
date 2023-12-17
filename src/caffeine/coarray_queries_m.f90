! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module coarray_queries_m
    implicit none
    private
    public ::  prif_lcobound, prif_ucobound

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

    end interface

end module coarray_queries_m
