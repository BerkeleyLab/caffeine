! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module normal_termination_m
    implicit none
    private
    public :: caf_stop
    
    interface caf_stop

      module subroutine caf_stop_integer(stop_code)
        !! synchronize, stop the executing image, and provide the stop_code (or 0 if not present) as the process exit status
        integer, intent(in), optional :: stop_code
      end subroutine

      module subroutine caf_stop_character(stop_code)
        !! synchronize, stop the executing image, and provide the stop_code as the process exit status
        character(len=*), intent(in) :: stop_code
      end subroutine

    end interface

end module normal_termination_m
