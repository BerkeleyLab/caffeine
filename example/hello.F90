program hello_world
  use iso_c_binding, only: c_bool
  use prif, only : &
     prif_init &
    ,prif_this_image_no_coarray &
    ,prif_num_images &
    ,prif_stop &
    ,prif_error_stop &
    ,PRIF_STAT_ALREADY_INIT
  implicit none

  integer :: init_exit_code, me, num_imgs
  logical(kind=c_bool), parameter :: false = .false._c_bool

  call prif_init(init_exit_code)
  if (init_exit_code /= 0 .and. init_exit_code /= PRIF_STAT_ALREADY_INIT) then
    call prif_error_stop(quiet=false, stop_code_char="program startup failed")
  end if

  call prif_this_image_no_coarray(this_image=me)
  call prif_num_images(num_images=num_imgs)
  print *, "Hello from image", me, "of", num_imgs

  call prif_stop(quiet=false)

end program
