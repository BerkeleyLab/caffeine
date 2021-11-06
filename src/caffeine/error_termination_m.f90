module error_termination_m
    implicit none
    private
    public :: caf_error_stop
    
    interface caf_error_stop

      module subroutine caf_error_stop_integer(stop_code)
        !! stop all images and provide the stop_code as the process exit status
        integer, intent(in) :: stop_code
      end subroutine

      module subroutine caf_error_stop_character(stop_code)
        !! stop all images and provide the stop_code as the process exit status
        character(len=*), intent(in) :: stop_code
      end subroutine

    end interface

end module error_termination_m
