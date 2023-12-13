! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module program_termination_m
    use iso_c_binding, only: c_int
    implicit none
    private
    public :: prif_stop, prif_error_stop

    interface

       module subroutine prif_stop(stop_code_int, stop_code_char, quiet)
         implicit none
         integer(c_int), intent(in), optional :: stop_code_int
         character(len=*), intent(in), optional :: stop_code_char
         logical, intent(in), optional :: quiet
       end subroutine

       module pure subroutine prif_error_stop(stop_code_int, stop_code_char, quiet)
         integer(c_int), intent(in), optional :: stop_code_int
         character(len=*), intent(in), optional :: stop_code_char
         logical, intent(in), optional :: quiet
       end subroutine

    end interface

end module program_termination_m
