program fail_image
  use iso_c_binding, only: c_bool
  use prif, only : &
     prif_init &
    ,prif_error_stop &
    ,prif_fail_image
  implicit none

  integer init_exit_code
  logical(kind=c_bool), parameter :: false = .false._c_bool

  call prif_init(init_exit_code)
  call prif_fail_image()
  call prif_error_stop(quiet=false) ! test fails if this line runs
end program 
