! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(normal_termination_m) normal_termination_s
  use iso_fortran_env, only : output_unit
  use iso_c_binding, only : c_int
  use caffeine_h_m, only : caf_c_decaffeinate
  implicit none

contains

  module procedure caf_stop_integer
 
    sync all
    

    write(output_unit, *) "caf_stop: stop code '", stop_code, "'"
    flush output_unit 

    if (.not. present(stop_code)) call caf_c_decaffeinate(exit_code=0_c_int) ! does not return
    call caf_c_decaffeinate(stop_code)

  end procedure 

  module procedure caf_stop_character

    sync all

    write(output_unit, *) "caf_stop: stop code '" // stop_code // "'"
    flush output_unit

    call caf_c_decaffeinate(exit_code=0_c_int)
 
  end procedure 


end submodule normal_termination_s
