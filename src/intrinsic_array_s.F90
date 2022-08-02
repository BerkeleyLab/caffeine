submodule(caffeine_intrinsic_array_m) intrinsic_array_s
  use program_termination_m, only: caf_error_stop
  implicit none

contains

  module procedure construct

#ifndef NAGFOR
  select rank(array)
    rank(1)
#endif
      select type(array)
      type is(complex)
        intrinsic_array%complex_1D = array
      type is(integer)
        intrinsic_array%integer_1D = array
      type is(logical)
        intrinsic_array%logical_1D = array
      type is(real)
        intrinsic_array%real_1D = array
      type is(double precision)
        intrinsic_array%double_precision_1D = array
      class default
        call caf_error_stop("intrinsic_array_t construct: unsupported rank-2 type")
      end select
#ifndef NAGFOR
    rank(2)
      select type(array)
      type is(complex)
        intrinsic_array%complex_2D = array
      type is(integer)
        intrinsic_array%integer_2D = array
      type is(logical)
        intrinsic_array%logical_2D = array
      type is(real)
        intrinsic_array%real_2D = array
      type is(double precision)
        intrinsic_array%double_precision_2D = array
      class default
        call caf_error_stop("intrinsic_array_t construct: unsupported rank-2 type")
      end select

    rank(3)
      select type(array)
      type is(complex)
        intrinsic_array%complex_3D = array
      type is(integer)
        intrinsic_array%integer_3D = array
      type is(logical)
        intrinsic_array%logical_3D = array
      type is(real)
        intrinsic_array%real_3D = array
      type is(double precision)
        intrinsic_array%double_precision_3D = array
      class default
        call caf_error_stop("intrinsic_array_t construct: unsupported rank-3 type")
      end select

    rank default
      call caf_error_stop("intrinsic_array_t construct: unsupported rank")
    end select
#endif

  end procedure

  module procedure as_character
    integer, parameter :: single_number_width=32

    if (1 /= count( &
      [ allocated(self%complex_1D), allocated(self%complex_double_1D), allocated(self%integer_1D), &
        allocated(self%logical_1D), allocated(self%real_1D), &
        allocated(self%complex_2D), allocated(self%complex_double_2D), allocated(self%integer_2D), &
        allocated(self%logical_2D), allocated(self%real_2D), &
        allocated(self%complex_3D), allocated(self%complex_double_3D), allocated(self%integer_3D), &
        allocated(self%logical_3D), allocated(self%real_3D) &
      ])) call caf_error_stop("intrinsic_array_t as_character: ambiguous component allocation status.")

    if (allocated(self%complex_1D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%complex_1D))
      write(character_self, *) self%complex_1D
    else if (allocated(self%complex_double_1D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%complex_double_1D))
      write(character_self, *) self%complex_double_1D
    else if (allocated(self%integer_1D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%integer_1D))
      write(character_self, *) self%integer_1D
    else if (allocated(self%logical_1D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%logical_1D))
      write(character_self, *) self%logical_1D
    else if (allocated(self%real_1D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%real_1D))
      write(character_self, *) self%real_1D
    else if (allocated(self%double_precision_1D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%double_precision_1D))
      write(character_self, *) self%double_precision_1D
    else if (allocated(self%complex_2D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%complex_2D))
      write(character_self, *) self%complex_2D
    else if (allocated(self%complex_double_2D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%complex_double_2D))
      write(character_self, *) self%complex_double_2D
    else if (allocated(self%integer_2D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%integer_2D))
      write(character_self, *) self%integer_2D
    else if (allocated(self%logical_2D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%logical_1D))
      write(character_self, *) self%logical_2D
    else if (allocated(self%real_2D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%real_2D))
      write(character_self, *) self%real_2D
    else if (allocated(self%double_precision_2D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%double_precision_2D))
      write(character_self, *) self%double_precision_2D
    else if (allocated(self%complex_3D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%complex_3D))
      write(character_self, *) self%complex_3D
    else if (allocated(self%complex_double_3D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%complex_double_3D))
      write(character_self, *) self%complex_double_3D
    else if (allocated(self%integer_3D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%integer_3D))
      write(character_self, *) self%integer_3D
    else if (allocated(self%logical_3D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%logical_1D))
      write(character_self, *) self%logical_3D
    else if (allocated(self%real_3D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%real_3D))
      write(character_self, *) self%real_3D
    else if (allocated(self%double_precision_3D)) then
      character_self = repeat(" ", ncopies = single_number_width*size(self%double_precision_3D))
      write(character_self, *) self%double_precision_3D
    end if

    character_self = trim(adjustl(character_self))
  end procedure

end submodule intrinsic_array_s
