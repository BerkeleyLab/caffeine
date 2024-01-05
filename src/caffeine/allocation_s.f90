! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(allocation_m) allocation_s
  use iso_c_binding, only: c_sizeof, c_f_pointer, c_loc
  use caffeine_h_m, only: caf_allocate
  use system_state_m, only: symmetric_heap

  implicit none

contains

  module procedure prif_allocate
    ! TODO: determining the size of the handle and where the coarray begins
    !       becomes a bit more complicated if we don't allocate space for
    !       15 cobounds
    type(c_ptr) :: whole_block
    integer(c_size_t) :: handle_size, coarray_size, total_size
    type(handle_data) :: unused
    type(handle_data), pointer :: unused2(:)

    coarray_size = product(ubounds-lbounds+1)*element_length
    handle_size = c_sizeof(unused)
    total_size = handle_size + coarray_size

    whole_block = caf_allocate(symmetric_heap, total_size)

    call c_f_pointer(whole_block, coarray_handle%info)
    call c_f_pointer(whole_block, unused2, [2])

    coarray_handle%info%coarray_data = c_loc(unused2(2))
    coarray_handle%info%corank = size(lcobounds)
    coarray_handle%info%coarray_size = coarray_size
    coarray_handle%info%final_func = final_func
    coarray_handle%info%cobounds(1:size(lcobounds))%lcobound = lcobounds
    coarray_handle%info%cobounds(1:size(lcobounds))%ucobound = ucobounds
    ! TODO: lookup current team and add this coarray to its list

    allocated_memory = coarray_handle%info%coarray_data
  end procedure

  module procedure prif_allocate_non_symmetric
  end procedure

  module procedure prif_deallocate
  end procedure

  module procedure prif_deallocate_non_symmetric
  end procedure

end submodule allocation_s
