! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(program_termination_m) program_termination_s
  use iso_fortran_env, only : output_unit, error_unit
  use iso_c_binding, only : c_char, c_int
  use caffeine_h_m, only : caf_c_decaffeinate
  implicit none

contains

  module procedure caf_stop_integer

    sync all

    !write(output_unit, *) "caf_stop: stop code '", stop_code, "'"
    write(output_unit, *) stop_code
    flush output_unit

    if (.not. present(stop_code)) call caf_c_decaffeinate(exit_code=0_c_int) ! does not return
    call caf_c_decaffeinate(stop_code)

  end procedure

  module procedure caf_stop_character

    sync all

    write(output_unit, *) "caf_stop: stop code '" // stop_code // "'"
    flush output_unit

    call caf_c_decaffeinate(exit_code=0_c_int) ! does not return

  end procedure



  module procedure caf_error_stop_character
    interface
      pure subroutine caf_error_stop_character_c(stop_code, length) bind(C, name = "caf_error_stop_character_c")
        use, intrinsic :: iso_c_binding, only: c_char, c_int
        implicit none
        integer(c_int), intent(in), value :: length
        character(len=1,kind=c_char), intent(in) :: stop_code(length)
      end subroutine
    end interface
    call caf_error_stop_character_c(f_c_string(stop_code), len(stop_code))
  end procedure

  subroutine inner_caf_error_stop_character(stop_code, length) bind(C, name = "inner_caf_error_stop_character")
    integer(c_int), intent(in) :: length
    character(len=1,kind=c_char), intent(in) :: stop_code(length)

    integer(c_int), parameter :: error_occured = 1

    write(error_unit, *) c_f_string(stop_code, length)
    flush error_unit

    call caf_error_stop_integer(error_occured)

  end subroutine

  module procedure caf_error_stop_integer
    interface
      pure subroutine caf_error_stop_integer_c(stop_code) bind(C, name = "caf_error_stop_integer_c")
        use, intrinsic :: iso_c_binding, only: c_int
        implicit none
        integer(c_int), intent(in) :: stop_code
      end subroutine
    end interface
    call caf_error_stop_integer_c(stop_code)
  end procedure

  subroutine inner_caf_error_stop_integer(stop_code) bind(C, name = "inner_caf_error_stop_integer")
    integer, intent(in), optional :: stop_code

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

    call caf_c_decaffeinate(exit_code) ! does not return

  end subroutine

  pure function f_c_string(f_string) result(c_string)
    character(len=*), intent(in) :: f_string
    character(len=1,kind=c_char) :: c_string(len(f_string))

    integer :: i

    do concurrent (i = 1:len(f_string))
      c_string(i) = f_string(i:i)
    end do
  end function

  pure function c_f_string(c_string, length) result(f_string)
    integer(c_int), intent(in) :: length
    character(len=1,kind=c_char), intent(in) :: c_string(length)
    character(len=length) :: f_string

    integer :: i
    do concurrent (i = 1:length)
      f_string(i:i) = c_string(i)
    end do
  end function

end submodule program_termination_s
