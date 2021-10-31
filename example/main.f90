program testhello_f
  use iso_c_binding, only : c_int, c_char, c_null_char, c_ptr, c_loc
  implicit none


  interface

    integer(c_int) function testhello(argc, argv) bind(C)
      !! C function prototype: int testhello(int argc, char **argv)
      import c_int, c_char , c_ptr
      integer(c_int), value :: argc
      type(c_ptr) argv(*)
    end function

  end interface

  integer, parameter :: max_arg_len = 1024
  character(kind=c_char, len=max_arg_len), allocatable, target :: arg(:)
  integer argnum, arglen

  associate(argc => int(command_argument_count(),c_int))
    allocate( arg(0:argc))
    do argnum=0, argc
      call get_command_argument(argnum, arg(argnum), arglen) 
      if (arglen+1>max_arg_len) error stop "maximum argument length exceeded"
      arg(argnum)(arglen+1:arglen+1) = c_null_char
    end do
    associate(argv => [(c_loc(arg(argnum)),argnum=0,argc)])
      if (testhello(argc, argv) /= 0) error stop "testhello returned a non-zero exit code"
    end associate
  end associate

end program
