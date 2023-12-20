! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module synchronization_m
    use iso_c_binding, only: c_int
    use teams_m, only: prif_team_type
    implicit none
    private
    public :: prif_sync_all, prif_sync_images, prif_sync_team, prif_sync_memory

    interface

      module subroutine prif_sync_all(stat, errmsg, errmsg_alloc)
        implicit none
        integer(c_int), intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
      end subroutine

      module subroutine prif_sync_images(image_set, stat, errmsg, errmsg_alloc)
        implicit none
        integer(c_int), intent(in), optional :: image_set(:)
        integer(c_int), intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
      end subroutine

      module subroutine prif_sync_team(team, stat, errmsg, errmsg_alloc)
        implicit none
        type(prif_team_type), intent(in) :: team
        integer(c_int), intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
      end subroutine

      module subroutine prif_sync_memory(stat, errmsg, errmsg_alloc)
        implicit none
        integer(c_int), intent(out), optional :: stat
        character(len=*), intent(inout), optional :: errmsg
        character(len=:), intent(inout), allocatable, optional :: errmsg_alloc
      end subroutine

    end interface

end module synchronization_m
