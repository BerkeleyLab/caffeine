! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) caffeine_assert_s
  implicit none

#if CAF_ASSERTIONS || !defined(CAF_ASSERTIONS)
  logical, parameter :: assertions_=.true.
#else
  logical, parameter :: assertions_=.false.
#endif

   !! Disable assertions with
   !! fpm test --flag "-DCAF_ASSERTIONS=0"

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
