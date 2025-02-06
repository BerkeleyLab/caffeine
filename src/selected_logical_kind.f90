use iso_c_binding, only : c_int
implicit none
integer(c_int), parameter, public :: PRIF_ATOMIC_LOGICAL_KIND = selected_logical_kind(32)
end
