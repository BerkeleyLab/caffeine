program hello_world
  use prif, only : prif_init, this_image => prif_this_image, num_images => prif_num_images, prif_stop
  implicit none

  if (prif_init() /= 0) error stop "caffeinate returned a non-zero exit code"
  
  print *, "Hello from image", this_image(), "of", num_images()

  call prif_stop(stop_code_int=0) ! normal termination

end program
