! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(prif:prif_private_s) alias_s
  ! DO NOT ADD USE STATEMENTS HERE
  ! All use statements belong in prif_private_s.F90
  implicit none

contains

  module procedure prif_alias_create
    call_assert(coarray_handle_check(source_handle))

    call_assert(size(alias_lcobounds) == size(alias_ucobounds))
    call_assert(product(alias_ucobounds - alias_lcobounds + 1) >= current_team%info%num_images)

    allocate(alias_handle%info)
    ! start with a copy of the source descriptor
    alias_handle%info = source_handle%info

    ! apply provided cobounds
    alias_handle%info%corank = size(alias_lcobounds)
    alias_handle%info%lcobounds(1:size(alias_lcobounds)) = alias_lcobounds
    alias_handle%info%ucobounds(1:size(alias_ucobounds)) = alias_ucobounds

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
