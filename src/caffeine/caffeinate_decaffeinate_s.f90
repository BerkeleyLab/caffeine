submodule(caffeinate_decaffeinate_m) caffeinate_decaffeinate_s
  use iso_c_binding, only : c_int, c_ptr, c_loc, c_char, c_null_char
  implicit none

contains

  module procedure caffeinate

    interface

      function c_caffeinate(argc, argv) bind(C) result(exit_code)
        !! C function prototype: int testhello(int argc, char **argv)
        import c_int, c_ptr
        integer(c_int), value :: argc
        integer(c_int) :: exit_code
        type(c_ptr) argv(*)
      end function

    end interface

  integer i

  associate(argc => int(command_argument_count(),c_int))
    associate(argv => [(c_loc(c_interop_arg(i)), i=0,argc)])
      exit_code = c_caffeinate(argc, argv)
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
  
  end procedure

  module procedure decaffeinate
  end procedure

end submodule caffeinate_decaffeinate_s
