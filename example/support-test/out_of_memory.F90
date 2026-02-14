program out_of_memory
  use iso_c_binding, only: c_bool, c_size_t, c_ptr, c_null_funptr, c_int64_t
  use prif
  implicit none

  integer :: init_exit_code, me, i
  integer(c_size_t) :: size_in_bytes = ishft(500_c_size_t, 40)
  type(c_ptr) :: allocated_memory
  logical :: coarray = .false.
  character(len=256) :: arg

  call prif_init(init_exit_code)
  if (init_exit_code /= 0 .and. init_exit_code /= PRIF_STAT_ALREADY_INIT) then
    call prif_error_stop(quiet=.false._c_bool, stop_code_char="program startup failed")
  end if
  call prif_this_image_no_coarray(this_image=me)

  do i = 1, command_argument_count()
    call get_command_argument(i, arg)
        
    if (trim(arg) == "--coarray" .or. trim(arg) == "-c") then
      coarray = .true.
    else
      read(arg, *) size_in_bytes
    end if
  end do  

  if (coarray) then
    if (me == 1) print *, "prif_allocate_coarray: ", size_in_bytes, " bytes"
    block
      integer(c_int64_t), dimension(1) :: lcobounds, ucobounds
      integer :: num_imgs
      type(prif_coarray_handle) :: coarray_handle
      
      call prif_num_images(num_images=num_imgs)
      lcobounds(1) = 1
      ucobounds(1) = num_imgs

      call prif_allocate_coarray( &
        lcobounds, ucobounds, size_in_bytes, c_null_funptr, &
        coarray_handle, allocated_memory)
    end block
  else
    if (me == 1) print *, "prif_allocate: ", size_in_bytes, " bytes"
    call prif_sync_all()
    call prif_allocate(size_in_bytes, allocated_memory)
  end if


  call prif_sync_all()
  call prif_error_stop(quiet=.false._c_bool, stop_code_char="test failed")

end program
