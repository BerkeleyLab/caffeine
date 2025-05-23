! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(prif:prif_private_s)  co_broadcast_s
  ! DO NOT ADD USE STATEMENTS HERE
  ! All use statements belong in prif_private_s.F90
  implicit none

contains

  module procedure prif_co_broadcast
    call_assert(source_image >= 1 .and. source_image <= current_team%info%num_images)
    call contiguous_co_broadcast(a, source_image, stat, errmsg, errmsg_alloc)
  end procedure

  subroutine contiguous_co_broadcast(a, source_image, stat, errmsg, errmsg_alloc)
    type(*), intent(inout), target, contiguous :: a(..)
    integer(c_int), intent(in) :: source_image
    integer(c_int), intent(out), optional :: stat
    character(len=*), intent(inout), optional :: errmsg
    character(len=:), intent(inout), allocatable, optional :: errmsg_alloc

    if (present(stat)) stat=0
    call caf_co_broadcast(a, source_image, product(shape(a)), current_team%info%gex_team)
      ! With a compliant Fortran 2018 compiler, pass in c_sizeof(a) as the `Nelem` argument
      ! and eliminate the calculation of num_elements*sizeof(a) in caffeine.c.
  end subroutine

end submodule co_broadcast_s
