program testhello_f
  use iso_c_binding, only : c_int, c_char, c_null_char, c_ptr, c_loc
  implicit none

  interface

    integer(c_int) function c_caffeinate(argc, argv) bind(C)
      !! C function prototype: int testhello(int argc, char **argv)
      import c_int, c_ptr
      integer(c_int), value :: argc
      type(c_ptr) argv(*)
    end function

  end interface

  integer i

  associate(argc => int(command_argument_count(),c_int))
    associate(argv => [(c_loc(c_interop_arg(i)), i=0,argc)])
      if (c_caffeinate(argc, argv) /= 0) error stop "c_caffeinate returned a non-zero exit code"
    end associate
  end associate

contains
 
  function c_interop_arg(argnum) result(arg)
    integer, parameter :: max_arg_len = 1024
    integer, intent(in) :: argnum
    integer arglen
    character(kind=c_char, len=max_arg_len), pointer :: arg

    allocate(arg)
    call get_command_argument(argnum, arg, arglen) 
    if (arglen+1>max_arg_len) error stop "maximum argument length exceeded"
    arg(arglen+1:arglen+1) = c_null_char
  end function

end program
