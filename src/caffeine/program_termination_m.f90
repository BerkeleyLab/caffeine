! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module program_termination_m
    use iso_c_binding, only: c_int, c_bool
    implicit none
    private
    public :: prif_stop, prif_error_stop, prif_fail_image

    interface

       module subroutine prif_stop(quiet, stop_code_int, stop_code_char)
         implicit none
         logical(c_bool), intent(in) :: quiet
         integer(c_int), intent(in), optional :: stop_code_int
         character(len=*), intent(in), optional :: stop_code_char
       end subroutine

       module pure subroutine prif_error_stop(quiet, stop_code_int, stop_code_char)
         logical(c_bool), intent(in) :: quiet
         integer(c_int), intent(in), optional :: stop_code_int
         character(len=*), intent(in), optional :: stop_code_char
       end subroutine

       module subroutine prif_fail_image()
         implicit none
       end subroutine

    end interface

end module program_termination_m
