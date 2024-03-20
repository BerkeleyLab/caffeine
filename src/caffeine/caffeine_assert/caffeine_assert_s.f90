! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(caffeine_assert_m) caffeine_assert_s
  use prif, only : prif_error_stop
  use iso_c_binding, only : c_bool
  implicit none

contains

  module procedure assert
    character(len=:), allocatable :: tail

    if (assertions_) then 
      if (.not. present(diagnostics)) then
        tail = "."
      else
        tail = " with diagnostics " 
        select type(diagnostics)
          type is(character(len=*))
            tail = tail // diagnostics
          class default
            tail = tail // "of unsupported type."
        end select
      end if
      if (.not. assertion) call prif_error_stop(.false._c_bool, stop_code_char='Assertion "'// description // '" failed' // tail)
    end if
  end procedure

end submodule caffeine_assert_s
