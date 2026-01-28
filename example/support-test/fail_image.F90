program fail_image
  use iso_c_binding, only: c_bool, c_int
  use prif, only : &
     prif_init &
    ,prif_num_images &
    ,prif_error_stop &
    ,prif_fail_image
  implicit none

  integer init_exit_code
  integer(c_int) :: ni
  logical(kind=c_bool), parameter :: false = .false._c_bool

  call prif_init(init_exit_code)
  call prif_num_images(ni)
  if (ni > 1) PRINT *, "WARNING: This support test is only intended to be run with a single image"
  call prif_fail_image()
  call prif_error_stop(quiet=false) ! test fails if this line runs
end program 
