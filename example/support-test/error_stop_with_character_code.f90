program error_stop_with_character_code
  use iso_c_binding, only: c_bool
  use prif, only : &
     prif_init &
    ,prif_stop &
    ,prif_error_stop
  implicit none

  integer init_exit_code
  logical(kind=c_bool), parameter :: false = .false._c_bool

  call prif_init(init_exit_code)
  call prif_error_stop(quiet=false, stop_code_char="USER_PROVIDED_STRING") ! a prif_error_stop unit test passes if this line executes error termination
  call prif_stop(quiet=false) ! a prif_error_stop unit test fails if this line runs
end program 
