! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) program_termination_s
  use iso_fortran_env, only : output_unit, error_unit
  use iso_c_binding, only : c_char
  implicit none

contains

  module procedure prif_stop

    !TODO: deal with argument `quiet`
    if (present(stop_code_char)) then
       call prif_stop_character(stop_code_char)
    else if (present(stop_code_int)) then
       call prif_stop_integer(stop_code_int)
    else
       call prif_stop_integer()
    end if

  contains

    subroutine prif_stop_integer(stop_code)
      !! synchronize, stop the executing image, and provide the stop_code, or 0 if not present, as the process exit status
      integer, intent(in), optional :: stop_code

      call prif_sync_all

      !write(output_unit, *) "caf_stop: stop code '", stop_code, "'"
      write(output_unit, *) stop_code
      flush output_unit

      if (.not. present(stop_code)) call caf_decaffeinate(exit_code=0_c_int) ! does not return
      call caf_decaffeinate(stop_code)

    end subroutine prif_stop_integer

    subroutine prif_stop_character(stop_code)
      !! synchronize, stop the executing image, and provide the stop_code as the process exit status
      character(len=*), intent(in) :: stop_code

      call prif_sync_all

      write(output_unit, *) "caf_stop: stop code '" // stop_code // "'"
      flush output_unit

      call caf_decaffeinate(exit_code=0_c_int) ! does not return

    end subroutine prif_stop_character

  end procedure prif_stop

  module procedure prif_error_stop

    !TODO: deal with argument `quiet`
    if (present(stop_code_char)) then
       call prif_error_stop_character(stop_code_char)
    else if (present(stop_code_int)) then
       call prif_error_stop_integer(stop_code_int)
    else
       call prif_error_stop_integer()
    end if
  end procedure prif_error_stop

  subroutine prif_error_stop_character(stop_code)
    !! stop all images and provide the stop_code as the process exit status
    character(len=*), intent(in) :: stop_code
    integer(c_int), parameter :: error_occured = 1

    write(error_unit, *) stop_code
    flush error_unit

    call prif_error_stop_integer(error_occured)
  end subroutine

  subroutine prif_error_stop_integer(stop_code)
    !! stop all images and provide the stop_code, or 0 if not present, as the process exit status
    integer, intent(in), optional :: stop_code

    interface
      subroutine caf_error_stop_integer_c(stop_code) bind(C, name = "caf_error_stop_integer_c")
        use, intrinsic :: iso_c_binding, only: c_int
        implicit none
        integer(c_int), intent(in) :: stop_code
      end subroutine
    end interface

    call caf_error_stop_integer_c(stop_code)
  end subroutine

  subroutine inner_caf_error_stop_integer(stop_code) bind(C, name = "inner_caf_error_stop_integer")
    integer, intent(in), optional :: stop_code

    integer exit_code

    if (.not. present(stop_code)) then

      call caf_decaffeinate(exit_code=1)

    else if (stop_code==0) then

      write(error_unit) stop_code
      flush error_unit
      exit_code = 1
    else
      exit_code = stop_code
    end if

    call caf_decaffeinate(exit_code) ! does not return

  end subroutine

  module procedure prif_fail_image
    call unimplemented("prif_fail_image")
  end procedure

end submodule program_termination_s
