! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif) prif_private_s
  implicit none

  type(prif_team_type), target :: default_team
  type(prif_team_type), pointer :: current_team => null()
  type(c_ptr) :: non_symmetric_heap_mspace

  interface

    pure module subroutine assert(assertion, description, diagnostics)
      implicit none
      logical, intent(in) :: assertion
      character(len=*), intent(in) :: description
      class(*), intent(in), optional :: diagnostics
    end subroutine

  end interface

contains

  subroutine unimplemented(proc_name)
    character(len=*), intent(in) ::  proc_name
    call prif_error_stop(quiet=.false._c_bool, stop_code_char=proc_name // " is not yet implemented")
  end subroutine

  pure function optional_value(var) result(c_val)
    integer, intent(in), optional :: var
    integer(c_int) c_val
    if (present(var)) then
      c_val = var
    else
      c_val = 0_c_int
    end if
  end function

end submodule prif_private_s
