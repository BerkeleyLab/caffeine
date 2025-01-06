program register_stop_callback
    use iso_c_binding, only: c_bool, c_int
    use prif, only : &
       prif_init, &
       prif_register_stop_callback, &
       prif_stop, &
       prif_stop_callback_interface
    implicit none
  
    integer init_exit_code
    logical(kind=c_bool), parameter :: false = .false._c_bool
    ! use of the pointer is unnecessary according to the standard,
    ! but gfortran complains without it
    procedure(prif_stop_callback_interface), pointer :: callback_ptr
    callback_ptr => callback
  
    call prif_init(init_exit_code)
    call prif_register_stop_callback(callback_ptr)
    call prif_stop(false)
contains
    subroutine callback(is_error_stop, quiet, stop_code_int, stop_code_char)
        logical(c_bool), intent(in) :: is_error_stop, quiet
        integer(c_int), intent(in), optional :: stop_code_int
        character(len=*), intent(in), optional :: stop_code_char

        print *, "callback invoked"
    end subroutine
end program