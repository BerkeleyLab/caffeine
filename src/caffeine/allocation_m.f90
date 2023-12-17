! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module allocation_m
    use iso_c_binding, only: c_ptr, c_int, c_intmax_t, c_size_t, c_funptr
    implicit none
    private
    public :: prif_allocate, prif_allocate_non_symmetric, prif_deallocate, prif_deallocate_non_symmetric


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

       module subroutine prif_allocate_non_symmetric(size_in_bytes, allocated_memory, stat, errmsg, errmsg_alloc)
         implicit none
         integer(kind=c_size_t) :: size_in_bytes
         type(c_ptr), intent(out) :: allocated_memory
         integer(c_int), intent(out), optional :: stat
         character(len=*), intent(inout), optional :: errmsg
         character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
       end subroutine

       module subroutine prif_deallocate(coarray_handles, stat, errmsg, errmsg_alloc)
         implicit none
         type(prif_coarray_handle), intent(in) :: coarray_handles(:)
         integer(c_int), intent(out), optional :: stat
         character(len=*), intent(inout), optional :: errmsg
         character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
       end subroutine

       module subroutine prif_deallocate_non_symmetric(mem, stat, errmsg, errmsg_alloc)
         implicit none
         type(c_ptr), intent(in) :: mem
         integer(c_int), intent(out), optional :: stat
         character(len=*), intent(inout), optional :: errmsg
         character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
       end subroutine

    end interface

end module allocation_m
