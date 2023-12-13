! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module allocation_m
    use iso_c_binding, only: c_ptr
    implicit none
    private

    type, public :: prif_coarray_handle
       type(c_ptr) :: ptr
    end type

end module allocation_m
