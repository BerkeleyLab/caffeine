! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(program_startup_m) program_startup_s
  use iso_c_binding, only : c_int, c_loc, c_char, c_null_char
  use synchronization_m, only : prif_sync_all
  use caffeine_h_m, only : caf_caffeinate, caf_decaffeinate
  use program_termination_m, only: prif_error_stop
  implicit none

contains

  module procedure prif_init

    integer i
    integer, parameter :: max_arg_len = 1024

    associate(argc => int(command_argument_count(),c_int))
      associate(argv => [(c_loc(c_interop_arg(i)), i=0,argc)])
        call caf_caffeinate(argc, argv)
      end associate
    end associate

    ! TODO: establish non-allocatable coarrays

    call prif_sync_all

    exit_code = 0

  contains

    function c_interop_arg(argnum) result(arg)
      integer, intent(in) :: argnum
      integer arglen
#ifndef __GFORTRAN__
      character(kind=c_char, len=max_arg_len), target :: arg
#else
      character(kind=c_char, len=max_arg_len), target :: targ  ! work around gfortran bug
      character(kind=c_char, len=max_arg_len), pointer :: arg
      arg => targ
#endif
      call get_command_argument(argnum, arg, arglen)
      if (arglen+1>max_arg_len) call prif_error_stop("maximum argument length exceeded")
      arg(arglen+1:arglen+1) = c_null_char
    end function

  end procedure

end submodule program_startup_s
