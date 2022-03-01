program hello_world
  use caffeine_m, only : caf_caffeinate, this_image => caf_this_image, num_images => caf_num_images, caf_stop
  implicit none

  if (caf_caffeinate() /= 0) error stop "caffeinate returned a non-zero exit code"
  
  print *, "Hello from image", this_image(), "of", num_images()

  call caf_stop(stop_code=0) ! normal termination

end program
