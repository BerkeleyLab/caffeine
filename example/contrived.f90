! Original:
! program contrived
!     implicit none

!     type :: my_type
!       integer :: a
!       real, allocatable :: b(:)
!     end type

!     type(my_type), allocatable :: c[:]
!     real :: d
!     integer :: i, me, neighbor

!     me = this_image()
!     allocate(c[*])
!     c%a = me
!     allocate(c%b(me))
!     do i = 1, me
!         c%b(i) = i
!     end do
!     sync all
!     neighbor = me + 1
!     if (neighbor > num_images()) neighbor = 1
!     d = c[neighbor]%b(neighbor)
!     sync all
!     deallocate(c)
!     print *, "On image ", me, ", got ", d
! end program

program contrived
    use prif ! we'll need to use calls to prif to replace coarray features
    use iso_c_binding ! many of the prif interfaces involve C interop features

    implicit none

    type :: allocatable_descriptor
        logical :: is_allocated = .false.
        real, pointer :: elements(:) => null()
    end type

    type :: my_type
      integer :: a
      type(allocatable_descriptor) :: b
    end type

    type :: coarray_descriptor
        logical :: is_allocated = .false.
        type(my_type), pointer :: local_data => null()
        type(prif_coarray_handle) :: handle
    end type


    type(coarray_descriptor), target :: c

    real :: d
    integer :: i, me, neighbor

    block
        integer(c_int) :: stat
        call prif_init(stat)
        if (stat /= 0_c_int) error stop "initialization failed"
    end block

    call prif_this_image_no_coarray(this_image=me)

    block
        type(my_type) :: for_element_size
        integer(c_int) :: num_images
        type(c_ptr) :: allocated_memory
        call prif_num_images(num_images)
        call prif_allocate_coarray( &
                lcobounds = [1_c_intmax_t], &
                ucobounds = [int(num_images, c_intmax_t)], &
                lbounds = [integer(c_intmax_t)::], &
                ubounds = [integer(c_intmax_t)::], &
                element_size = int(storage_size(for_element_size)/8, c_size_t), &
                final_func = c_funloc(deallocate_components), &
                coarray_handel = c%handle, &
                allocated_memory = allocated_memory)
        call c_f_pointer(cptr = allocated_memory, fptr = c%local_data)
        c%is_allocated = .true.
        call prif_set_context_data(c%handle, c_loc(c))
    end block

    c%local_data%a = me

    block
        type(c_ptr) :: allocated_memory
        real :: for_element_size
        call prif_allocate( &
                size_in_bytes = int((storage_size(for_element_size)*me)/8, c_size_t), &
                allocated_memory = allocated_memory)
        call c_f_pointer(cptr = allocated_memory, fptr = c%local_data%b%elements, shape = [me])
        c%local_data%b%is_allocated = .true.
    end block

    do i = 1, me
        c%local_data%b%elements(i) = i
    end do

    call prif_sync_all()

    neighbor = me + 1

    block
        integer(c_int) :: num_images
        call prif_num_images(num_images = num_images)
        if (neighbor > num_images) neighbor = 1
    end block

    block
        type(c_ptr) :: buffer
        integer(c_size_t), parameter :: b_offset = 8_c_size_t ! compiler dependent
        integer(c_intptr_t), target :: b_pointer
        real, target :: rhs_temp
        buffer = c_loc(b_pointer)
        ! fetch address of allocatable array on other image
        call prif_get( &
                image_num = neighbor, &
                coarray_handle = c%handle, &
                offset = b_offset, &
                current_image_buffer = buffer, &
                size_in_bytes = int(storage_size(b_pointer)/8, c_size_t))
        ! calculate address of element we want to access
        b_pointer = b_pointer + (storage_size(rhs_temp)*neighbor)/8
        call prif_get_indirect( &
                image_num = neighbor, &
                remote_ptr = b_pointer, &
                current_image_buffer = c_loc(rhs_temp), &
                size_in_bytes = int(storage_size(rhs_temp)/8, c_size_t))
        d = rhs_temp
    end block

    call prif_sync_all()

    call prif_deallocate_coarray([c%handle])

    print *, "On image ", me, ", got ", d

    call prif_stop(quiet=.false._c_bool)
contains
    subroutine deallocate_components(handle, stat, errmsg) bind(C)
        type(prif_coarray_handle), pointer, intent(in) :: handle
        integer(c_int), intent(out) :: stat
        character(len=:), intent(out), allocatable :: errmsg

        type(coarray_descriptor), pointer :: coarray_var
        type(c_ptr) :: context_data

        call prif_get_context_data(handle, context_data)
        call c_f_pointer(cptr = context_data, fptr = coarray_var)
        if (coarray_var%local_data%b%is_allocated) then
            call prif_deallocate(c_loc(coarray_var%local_data%b%elements), stat, errmsg_alloc=errmsg)
            coarray_var%local_data%b%is_allocated = .false.
            nullify(coarray_var%local_data%b%elements)
        else
            stat = 0
            errmsg = ""
        end if
        if (stat == 0) then
            coarray_var%is_allocated = .false.
            nullify(coarray_var%local_data)
        end if
    end subroutine
end program

