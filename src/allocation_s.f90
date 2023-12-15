! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(allocation_m) allocation_s

  implicit none

contains

  module procedure prif_allocate
    use caffeine_h_m, only: caf_allocate

    allocated_memory = caf_allocate( &
      product(ubounds-lbounds+1)*element_length, size(ucobounds), lcobounds, ucobounds, final_func, coarray_handle%ptr)
  end procedure

end submodule allocation_s
