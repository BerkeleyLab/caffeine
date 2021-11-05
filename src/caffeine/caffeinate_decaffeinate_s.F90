submodule(caffeinate_decaffeinate_m) caffeinate_decaffeinate_s
  use iso_c_binding, only : c_int, c_ptr, c_loc, c_char, c_null_char
  implicit none

contains

  module procedure caffeinate

    interface

      subroutine c_caffeinate(argc, argv) bind(C)
        !! C function prototype: int testhello(int argc, char **argv)
        import c_int, c_ptr
        integer(c_int), value :: argc
        integer(c_int) :: exit_code
        type(c_ptr) argv(*)
      end subroutine

    end interface

  integer i
  integer, parameter :: max_arg_len = 1024

  associate(argc => int(command_argument_count(),c_int))
#ifndef __GFORTRAN__
    associate(argv => [(c_loc(c_interop_arg(i)), i=0,argc)])
      call c_caffeinate(argc, argv)
    end associate
#else
    workaround_gfortran_bug: &
    block 
      type ptr_array
        character(kind=c_char, len=max_arg_len), pointer :: arg_ptr
      end type
      type(ptr_array) argv(argc)
      do i=1, argc
        argv(i)%arg_ptr => c_interop_arg(i)
      end do
      call c_caffeinate(argc, [(c_loc(argv(i)%arg_ptr), i=1, argc)])
      do i=1, argc
        deallocate(argv(i)%arg_ptr)
      end do
    end block workaround_gfortran_bug
#endif
  end associate

  contains
 
    function c_interop_arg(argnum) result(arg)
      integer, intent(in) :: argnum
      integer arglen
      
#ifndef __GFORTRAN__
      character(kind=c_char, len=max_arg_len), target :: arg 
#else
      character(kind=c_char, len=max_arg_len), pointer :: arg 
      allocate(arg)
#endif
      call get_command_argument(argnum, arg, arglen) 
      if (arglen+1>max_arg_len) error stop "maximum argument length exceeded"
      arg(arglen+1:arglen+1) = c_null_char
    end function
  
  end procedure

  module procedure decaffeinate

    interface

      subroutine c_decaffeinate(exit_code) bind(C)
        import c_int 
        integer(c_int), value :: exit_code
      end subroutine
      
      subroutine c_sync_all() bind(C)
      end subroutine

    end interface
    
    call c_sync_all

    call c_decaffeinate(0) 
  end procedure

end submodule caffeinate_decaffeinate_s
