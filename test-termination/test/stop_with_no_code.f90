program stop_with_no_code
  use iso_c_binding, only: c_bool
  use prif, only : &
     prif_init &
    ,prif_stop &
    ,prif_error_stop
  implicit none

  integer init_exit_code
  logical(kind=c_bool), parameter :: false = .false._c_bool

  call prif_init(init_exit_code)
  call prif_stop(false) ! a prif_stop test passes if this line executes normal termination
  call prif_error_stop(quiet=false) ! a prif_stop test fails if this line runs
end program 