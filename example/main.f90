program testhello_f
  use iso_c_binding, only : c_int, c_char, c_ptr
  implicit none

  integer, parameter :: max_len = 1024

  interface

    integer(c_int) function testhello(argc, argv) bind(C)
      !! C function prototype: int testhello(int argc, char **argv)
      import c_int, c_char, max_len
      integer(c_int), value :: argc
      character(kind=c_char, len=max_len) argv(*)
    end function

  end interface

  character(kind=c_char, len=max_len), allocatable :: argv(:)
  integer argnum, arglen

  associate(argc => int(command_argument_count(),c_int))
    allocate(argv(argc))
    do argnum=1, argc
      call get_command_argument(argnum, argv, arglen) 
      if (arglen>max_len) error stop "maximum argument length exceeded"
    end do
    if (testhello(argc, argv) /= 0) error stop "testhello.c returned a non-zero exit code"
  end associate

end program
