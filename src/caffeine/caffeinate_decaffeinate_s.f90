submodule(caffeinate_decaffeinate_m) caffeinate_decaffeinate_s
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

  if (c_caffeinate(argc, argv) /= 0) error stop "caffeinate: c_caffeinate returned non-zero exit code"

  end procedure

  module procedure decaffeinate
  end procedure

end submodule caffeinate_decaffeinate_s
