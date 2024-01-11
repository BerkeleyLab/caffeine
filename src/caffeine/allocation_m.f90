! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module allocation_m
    use iso_c_binding, only: c_ptr, c_int, c_intmax_t, c_size_t, c_funptr

    implicit none
    private
    public :: &
        handle_data, &
        prif_coarray_handle, &
        prif_allocate, &
        prif_allocate_non_symmetric, &
        prif_deallocate, &
        prif_deallocate_non_symmetric

    ! TODO: Should these actually be interleaved if we use explicit size?
    type, bind(C) :: cobound_pair
      integer(c_intmax_t) :: lcobound, ucobound
    end type
    
    type, bind(C) :: handle_data
      type(c_ptr) :: coarray_data
      integer(c_int) :: corank
      integer(c_size_t) :: coarray_size
      type(c_funptr) :: final_func
      type(c_ptr) :: previous_handle, next_handle
      type(cobound_pair) :: cobounds(15)
    end type

    type :: prif_coarray_handle
      private
      type(handle_data), pointer :: info
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
         ! TODO: I think we need to add target here, to be able to pass to bind(C) final_func
         type(prif_coarray_handle), intent(inout) :: coarray_handles(:)
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
