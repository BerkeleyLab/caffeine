program hello_world
  use caffeine_m, only : caffeinate, decaffeinate &
    , this_image => caf_this_image, num_images => caf_num_images
  implicit none

  if (caffeinate() /= 0) error stop "caffeinate returned a non-zero exit code"
  
  print *, "Hello from image", this_image(), "of", num_images()

  call decaffeinate(exit_code=0) ! normal termination

end program
