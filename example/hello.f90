program hello_world
  use iso_c_binding, only: c_bool
  use prif, only : prif_init, this_image => prif_this_image_no_coarray, num_images => prif_num_images, prif_stop
  implicit none

  integer :: init_exit_code, me, num_imgs

  call prif_init(init_exit_code)
  if (init_exit_code /= 0) error stop "caffeinate returned a non-zero exit code"

  call this_image(this_image=me)
  call num_images(num_images=num_imgs)
  print *, "Hello from image", me, "of", num_imgs

  call prif_stop(.false._c_bool, stop_code_int=0) ! normal termination

end program
