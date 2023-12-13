program hello_world
  use prif, only : prif_init, this_image => prif_this_image, num_images => prif_num_images, prif_stop
  implicit none

  integer :: me, num_imgs

  if (prif_init() /= 0) error stop "caffeinate returned a non-zero exit code"

  call this_image(image_index=me)
  call num_images(image_count=num_imgs)
  print *, "Hello from image", me, "of", num_imgs

  call prif_stop(stop_code_int=0) ! normal termination

end program
