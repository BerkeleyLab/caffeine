! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) program_termination_s
  use iso_fortran_env, only : output_unit, error_unit
  use iso_c_binding, only : c_char
  implicit none

contains

  module procedure prif_stop

    if (present(stop_code_char)) then
       call prif_stop_character(quiet, stop_code_char)
    else if (present(stop_code_int)) then
       call prif_stop_integer(quiet, stop_code_int)
    else
       call prif_stop_integer(quiet)
    end if

  contains

    subroutine prif_stop_integer(quiet, stop_code)
      !! synchronize, stop the executing image, and provide the stop_code, or 0 if not present, as the process exit status
      logical(c_bool), intent(in) :: quiet
      integer(c_int), intent(in), optional :: stop_code
      integer(c_int) :: exit_code

      call prif_sync_all

      if (present(stop_code)) then
        if (.not. quiet) then
          write(output_unit, *) "STOP ", stop_code
          flush output_unit
        end if
        exit_code = stop_code
      else
        if (.not. quiet) then
          write(output_unit, *) "STOP"
          flush output_unit
        end if
        exit_code = 0_c_int
      end if

      call caf_decaffeinate(exit_code)

    end subroutine prif_stop_integer

    subroutine prif_stop_character(quiet, stop_code)
      !! synchronize, stop the executing image, and provide the stop_code as the process exit status
      logical(c_bool), intent(in) :: quiet
      character(len=*), intent(in) :: stop_code

      call prif_sync_all

      if (.not. quiet) then
        write(output_unit, *) "STOP '" // stop_code // "'"
        flush output_unit
      end if

      call caf_decaffeinate(exit_code=0_c_int) ! does not return

    end subroutine prif_stop_character

  end procedure prif_stop

  module procedure prif_error_stop
    if (present(stop_code_char)) then
       call prif_error_stop_character(quiet, stop_code_char)
    else if (present(stop_code_int)) then
       call prif_error_stop_integer(quiet, stop_code_int)
    else
       call prif_error_stop_integer(quiet)
    end if
  end procedure prif_error_stop

  subroutine prif_error_stop_character(quiet, stop_code)
    !! stop all images and provide the stop_code as the process exit status
    logical(c_bool), intent(in) :: quiet
    character(len=*), intent(in) :: stop_code

    if (.not. quiet) then
      write(error_unit, *) stop_code
      flush error_unit
    end if

    call caf_decaffeinate(1_c_int) ! does not return
  end subroutine

  subroutine prif_error_stop_integer(quiet, stop_code)
    !! stop all images and provide the stop_code, or 1 if not present, as the process exit status
    logical(c_bool), intent(in) :: quiet
    integer(c_int), intent(in), optional :: stop_code
    integer(c_int) :: exit_code

    if (present(stop_code)) then
      if (.not.quiet) then
        write(error_unit) "ERROR STOP ", stop_code
        flush error_unit
      end if
      exit_code = stop_code
    else
      if (.not.quiet) then
        write(error_unit) "ERROR STOP"
        flush error_unit
      end if
      exit_code = 1_c_int
    end if

    call caf_decaffeinate(exit_code) ! does not return
  end subroutine

  module procedure prif_fail_image
    call unimplemented("prif_fail_image")
  end procedure

end submodule program_termination_s
