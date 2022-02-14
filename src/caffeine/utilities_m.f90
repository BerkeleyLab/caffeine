module utilities_m
 use iso_c_binding, only : c_ptr, c_null_ptr, c_int, c_loc
  implicit none
  public :: get_c_ptr, get_c_ptr_character, optional_value

contains

  pure function get_c_ptr(stat) result(ptr)
    integer, intent(in), optional, target :: stat
    type(c_ptr) ptr 
    if (present(stat)) then
      ptr = c_loc(stat)
    else
      ptr = c_null_ptr
    end if
  end function

  pure function get_c_ptr_character(errmsg) result(ptr)
    character(len=*), intent(in), optional, target :: errmsg
    type(c_ptr) ptr 
    if (present(errmsg)) then
      ptr = c_loc(errmsg)
    else
      ptr = c_null_ptr
    end if
  end function

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
