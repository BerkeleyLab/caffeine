! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) program_termination_s
  use iso_fortran_env, only : output_unit, error_unit
  use iso_c_binding, only : c_char
  implicit none

  type :: callback_entry
    procedure(prif_stop_callback_interface), pointer, nopass :: callback
    type(callback_entry), pointer :: next => null()
  end type

  type(callback_entry), pointer :: callback_list => null()

contains

  module procedure prif_register_stop_callback
    type(callback_entry), pointer :: new_entry
    allocate(new_entry)
    new_entry%callback => callback
    if (associated(callback_list)) then
      new_entry%next => callback_list
    end if
    callback_list => new_entry
  end procedure

  module procedure prif_stop
    call prif_sync_all
    call run_callbacks(.false._c_bool, quiet, stop_code_int, stop_code_char)

    if (present(stop_code_char)) then
       call prif_stop_character(quiet, stop_code_char)
    else
       call prif_stop_integer(quiet, stop_code_int)
    end if

  contains

    subroutine prif_stop_integer(quiet, stop_code)
      !! synchronize, stop the executing image, and provide the stop_code, or 0 if not present, as the process exit status
      logical(c_bool), intent(in) :: quiet
      integer(c_int), intent(in), optional :: stop_code
      integer(c_int) :: exit_code

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

      if (.not. quiet) then
        write(output_unit, *) "STOP '" // stop_code // "'"
        flush output_unit
      end if

      call caf_decaffeinate(exit_code=0_c_int) ! does not return

    end subroutine prif_stop_character

  end procedure prif_stop

  module procedure prif_error_stop
    call run_callbacks(.true._c_bool, quiet, stop_code_int, stop_code_char)
    if (present(stop_code_char)) then
       call prif_error_stop_character(quiet, stop_code_char)
    else
       call prif_error_stop_integer(quiet, stop_code_int)
    end if
  end procedure prif_error_stop

  subroutine prif_error_stop_character(quiet, stop_code)
    !! stop all images and provide the stop_code as the process exit status
    logical(c_bool), intent(in) :: quiet
    character(len=*), intent(in) :: stop_code

    if (.not. quiet) then
      write(error_unit, *) "ERROR STOP '" // stop_code // "'"
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
        write(error_unit,'(A, I0)') "ERROR STOP ", stop_code
        flush error_unit
      end if
      exit_code = stop_code
    else
      if (.not.quiet) then
        write(error_unit,'(a)') "ERROR STOP"
        flush error_unit
      end if
      exit_code = 1_c_int
    end if

    call caf_decaffeinate(exit_code) ! does not return
  end subroutine

  module procedure prif_fail_image
    call unimplemented("prif_fail_image")
  end procedure

  subroutine run_callbacks(is_error_stop, quiet, stop_code_int, stop_code_char)
    logical(c_bool), intent(in) :: is_error_stop, quiet
    integer(c_int), intent(in), optional :: stop_code_int
    character(len=*), intent(in), optional :: stop_code_char

    type(callback_entry), pointer :: next_entry

    next_entry => callback_list
    do while (associated(next_entry))
      call next_entry%callback(is_error_stop, quiet, stop_code_int, stop_code_char)
      next_entry => next_entry%next
    end do
  end subroutine

end submodule program_termination_s
