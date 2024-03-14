! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(caffeine_assert_m) caffeine_assert_s
  implicit none

contains

  module procedure assert
    character(len=:), allocatable :: tail

    if (assertions_) then 
      if (.not. present(diagnostics)) then
        tail = "."
      else
        tail = " with diagnostic " 
        select type(diagnostics)
          type is(character(len=*))
            tail = tail // diagnostics
          class default
            tail = tail // "of unsupported type."
        end select
      end if
      if (.not. assertion) error stop 'Assertion "'// description // tail
    end if
  end procedure

end submodule caffeine_assert_s
