! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module allocation_m
    use iso_c_binding, only: c_ptr, c_int, c_intmax_t, c_size_t, c_funptr
    implicit none
    private
    public :: prif_allocate


    type, public :: prif_coarray_handle
       type(c_ptr) :: ptr
    end type

    interface

       module subroutine prif_allocate( &
           lcobounds, ucobounds, lbounds, ubounds, element_length, final_func, coarray_handle, &
           allocated_memory, stat, errmsg, errmsg_alloc)
         implicit none
         integer(kind=c_intmax_t), dimension(:), intent(in) :: lcobounds, ucobounds
         integer(kind=c_intmax_t), dimension(:), intent(in) :: lbounds, ubounds
         integer(kind=c_size_t), intent(in) :: element_length
         type(c_funptr), intent(in) :: final_func
         type(prif_coarray_handle), intent(out) :: coarray_handle
         type(c_ptr), intent(out) :: allocated_memory
         integer(c_int), intent(out), optional :: stat
         character(len=*), intent(inout), optional :: errmsg
         character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
       end subroutine

    end interface

end module allocation_m
