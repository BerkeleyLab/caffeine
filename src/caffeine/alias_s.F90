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
    type(prif_coarray_descriptor), pointer :: cdp
    type(prif_coarray_descriptor), pointer :: alias_cdp

    ! validate inputs
    call_assert(coarray_handle_check(source_handle))
    corank = size(alias_lcobounds)
    call_assert(corank > 0)
    call_assert(corank <= 15)
    if (size(alias_ucobounds) == corank) then
      call_assert(all(alias_lcobounds <= alias_ucobounds))
      call_assert(product(alias_ucobounds - alias_lcobounds + 1) >= current_team%info%num_images)
    else
      call_assert(size(alias_ucobounds) == corank - 1)
      call_assert(all(alias_lcobounds(1:corank-1) <= alias_ucobounds))
    end if

    cdp => handle_to_cdp(source_handle)
    ! start with a copy of the source descriptor
    allocate(alias_cdp, source=cdp)

#   if CAF_PRIF_VERSION >= 6
       alias_cdp%coarray_data = &
         as_c_ptr(as_int(alias_cdp%coarray_data) + data_pointer_offset)
#   endif

    ! apply provided cobounds
    alias_cdp%corank = corank
    alias_cdp%lcobounds(1:corank) = alias_lcobounds
    alias_cdp%ucobounds(1:corank-1) = alias_ucobounds(1:corank-1)
    call compute_coshape_epp(alias_lcobounds, alias_ucobounds, &
                             alias_cdp%coshape_epp(1:corank))
#   if ASSERTIONS
      ! The following entries are dead, but initialize them to help detect defects
      alias_cdp%lcobounds(corank+1:15) = huge(0_c_int64_t)
      alias_cdp%ucobounds(corank:14) = -huge(0_c_int64_t)
      alias_cdp%coshape_epp(corank+1:15) = 0
#   endif

    ! reset some fields that are unused in aliases
    alias_cdp%reserved = c_null_ptr 
    alias_cdp%previous_handle = c_null_ptr
    alias_cdp%next_handle = c_null_ptr
    alias_cdp%final_proc = c_null_funptr

    alias_handle = cdp_to_handle(alias_cdp)
    call_assert(coarray_handle_check(alias_handle))
  end procedure

  module procedure prif_alias_destroy
    type(prif_coarray_descriptor), pointer :: cdp 

    call_assert(coarray_handle_check(alias_handle))

    cdp => handle_to_cdp(alias_handle)
    call_assert(.not. c_associated(cdp%reserved))
    call_assert(.not. c_associated(cdp%previous_handle))
    call_assert(.not. c_associated(cdp%next_handle))
    call_assert(.not. c_associated(cdp%final_proc))

    deallocate(cdp)
  end procedure

end submodule alias_s
