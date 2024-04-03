module utilities_m
 use iso_c_binding, only : c_ptr, c_null_ptr, c_int, c_loc
  implicit none
  public :: optional_value

contains

  pure function optional_value(var) result(c_val)
    integer, intent(in), optional :: var 
    integer(c_int) c_val
    if (present(var)) then
      c_val = var 
    else
      c_val = 0_c_int
    end if
  end function

end module utilities_m
