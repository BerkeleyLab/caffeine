module system_state_m
    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr
    implicit none

    type(c_ptr) :: symmetric_heap = c_null_ptr
end module