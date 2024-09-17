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
      integer(c_int), intent(in), optional :: stop_code

      call prif_sync_all

      write(output_unit, *) "STOP ", stop_code
      flush output_unit

      if (.not. present(stop_code)) call caf_decaffeinate(exit_code=0_c_int) ! does not return
      call caf_decaffeinate(stop_code)

    end subroutine prif_stop_integer

    subroutine prif_stop_character(stop_code)
      !! synchronize, stop the executing image, and provide the stop_code as the process exit status
      character(len=*), intent(in) :: stop_code

      call prif_sync_all

      write(output_unit, *) "STOP '" // stop_code // "'"
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

    write(error_unit, *) stop_code
    flush error_unit

    call caf_decaffeinate(1_c_int) ! does not return
  end subroutine

  subroutine prif_error_stop_integer(stop_code)
    !! stop all images and provide the stop_code, or 1 if not present, as the process exit status
    integer(c_int), intent(in), optional :: stop_code
    integer(c_int) :: exit_code

    ! TODO: Resolve test issue - writing to the error_unit, which is the semantics of PRIF
    ! breaks the current testing strategy for `prif_error_stop`
    ! We plan to change the testing strategy anyway, so once this is done, need to comment back
    ! in the code below related to the error_unit
    if (present(stop_code)) then
!      write(error_unit) "ERROR STOP ", stop_code
      exit_code = stop_code
   else
!      write(error_unit) "ERROR STOP"
      exit_code = 1_c_int
    end if

!    flush error_unit

    call caf_decaffeinate(exit_code) ! does not return
  end subroutine

  module procedure prif_fail_image
    call unimplemented("prif_fail_image")
  end procedure

end submodule program_termination_s
