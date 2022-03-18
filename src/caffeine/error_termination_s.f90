! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(error_termination_m) error_termination_s
  use iso_fortran_env, only : error_unit
  use iso_c_binding, only : c_int
  use caffeine_h_m, only : caf_c_decaffeinate
  implicit none

contains

  module procedure caf_error_stop_character

    integer(c_int), parameter :: error_occured = 1

    write(error_unit, *) stop_code
    flush error_unit

    call caf_error_stop_integer(error_occured)
 
  end procedure 

  module procedure caf_error_stop_integer
 
    integer exit_code
    
    if (.not. present(stop_code)) then

      call caf_c_decaffeinate(exit_code=1)

    else if (stop_code==0) then

      write(error_unit) stop_code
      flush error_unit 
      exit_code = 1
    else
      exit_code = stop_code
    end if

    call caf_c_decaffeinate(exit_code)

  end procedure 

end submodule error_termination_s