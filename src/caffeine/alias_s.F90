! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"
#include "version.h"

submodule(prif:prif_private_s) alias_s
  ! DO NOT ADD USE STATEMENTS HERE
  ! All use statements belong in prif_private_s.F90
  implicit none

contains

  module procedure prif_alias_create
    integer(c_int) :: corank
    type(prif_coarray_descriptor), pointer :: dp
    type(prif_coarray_descriptor), pointer :: alias_dp

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

    dp => handle_to_dp(source_handle)
    ! start with a copy of the source descriptor
    allocate(alias_dp, source=dp)

#   if CAF_PRIF_VERSION >= 6
       alias_dp%coarray_data = &
         as_c_ptr(as_int(alias_dp%coarray_data) + data_pointer_offset)
#   endif

    ! apply provided cobounds
    alias_dp%corank = corank
    alias_dp%lcobounds(1:corank) = alias_lcobounds
    alias_dp%ucobounds(1:corank-1) = alias_ucobounds(1:corank-1)
    call compute_coshape_epp(alias_lcobounds, alias_ucobounds, &
                             alias_dp%coshape_epp(1:corank))
#   if ASSERTIONS
      ! The following entries are dead, but initialize them to help detect defects
      alias_dp%lcobounds(corank+1:15) = huge(0_c_int64_t)
      alias_dp%ucobounds(corank:14) = -huge(0_c_int64_t)
      alias_dp%coshape_epp(corank+1:15) = 0
#   endif

    ! reset some fields that are unused in aliases
    alias_dp%reserved = c_null_ptr 
    alias_dp%previous_handle = c_null_ptr
    alias_dp%next_handle = c_null_ptr
    alias_dp%final_func = c_null_funptr

    alias_handle = dp_to_handle(alias_dp)
    call_assert(coarray_handle_check(alias_handle))
  end procedure

  module procedure prif_alias_destroy
    type(prif_coarray_descriptor), pointer :: info 

    call_assert(coarray_handle_check(alias_handle))

    info => handle_to_dp(alias_handle)
    call_assert(.not. c_associated(info%reserved))
    call_assert(.not. c_associated(info%previous_handle))
    call_assert(.not. c_associated(info%next_handle))
    call_assert(.not. c_associated(info%final_func))

    deallocate(info)
  end procedure

end submodule alias_s
