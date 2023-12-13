! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module synchronization_m
    use iso_c_binding, only: c_int
    implicit none
    private
    public :: prif_sync_all
    
    interface

      module subroutine prif_sync_all(stat, errmsg, errmsg_alloc)
        implicit none
        integer(c_int), intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
      end subroutine

    end interface

end module synchronization_m
