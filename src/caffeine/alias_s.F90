! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"
#include "language-support.F90"

submodule(prif:prif_private_s) alias_s
  ! DO NOT ADD USE STATEMENTS HERE
  ! All use statements belong in prif_private_s.F90
  implicit none

contains

  module procedure prif_alias_create
    integer(c_int) :: corank

    ! validate inputs
    call_assert(coarray_handle_check(source_handle))
    corank = size(alias_lcobounds)
    call_assert(corank > 0)
    if (size(alias_ucobounds) == corank) then
      call_assert(all(alias_lcobounds <= alias_ucobounds))
      call_assert(product(alias_ucobounds - alias_lcobounds + 1) >= current_team%info%num_images)
    else
      call_assert(size(alias_ucobounds) == corank - 1)
      call_assert(all(alias_lcobounds(1:corank-1) <= alias_ucobounds))
    end if


    allocate(alias_handle%info)
    ! start with a copy of the source descriptor
    alias_handle%info = source_handle%info

#   if CAF_PRIF_VERSION >= 6
       alias_handle%info%coarray_data = &
         as_c_ptr(as_int(alias_handle%info%coarray_data) + data_pointer_offset)
#   endif

    ! apply provided cobounds
    alias_handle%info%corank = corank
    alias_handle%info%lcobounds(1:corank) = alias_lcobounds
    alias_handle%info%ucobounds(1:corank-1) = alias_ucobounds(1:corank-1)
    call compute_coshape_epp(alias_lcobounds, alias_ucobounds, &
                             alias_handle%info%coshape_epp(1:corank))
#   if ASSERTIONS
      ! The following entries are dead, but initialize them to help detect defects
      alias_handle%info%lcobounds(corank+1:15) = huge(0_c_int64_t)
      alias_handle%info%ucobounds(corank:14) = -huge(0_c_int64_t)
      alias_handle%info%coshape_epp(corank+1:15) = 0
#   endif

    ! reset some fields that are unused in aliases
    alias_handle%info%reserved = c_null_ptr 
    alias_handle%info%previous_handle = c_null_ptr
    alias_handle%info%next_handle = c_null_ptr
    alias_handle%info%final_func = c_null_funptr

    call_assert(coarray_handle_check(alias_handle))
  end procedure

  module procedure prif_alias_destroy
    type(prif_coarray_descriptor), pointer :: info 

    call_assert(coarray_handle_check(alias_handle))

    info => alias_handle%info
    call_assert(.not. c_associated(info%reserved))
    call_assert(.not. c_associated(info%previous_handle))
    call_assert(.not. c_associated(info%next_handle))
    call_assert(.not. c_associated(info%final_func))

    deallocate(info)
  end procedure

end submodule alias_s
