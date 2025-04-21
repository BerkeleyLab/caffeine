! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"

submodule(prif) prif_private_s
  use assert_m

  use iso_fortran_env, only : &
        output_unit, &
        error_unit

  use iso_c_binding, only: &
        c_associated, &
        c_f_pointer, &
        c_f_procpointer, &
        c_funloc, &
        c_loc, &
        c_null_funptr, &
        c_sizeof, &
        ! DOB: The following is a gfortran-14 bug workaround. No idea why this works...
        c_funptr_ => c_funptr

  implicit none

  type(team_data), target :: initial_team
  type(prif_team_type) :: current_team
  type(c_ptr) :: non_symmetric_heap_mspace

  interface

    ! ________ Program initiation and finalization ___________

    subroutine caf_caffeinate( &
        symmetric_heap, &
        symmetric_heap_start, &
        symmetric_heap_size, &
        non_symmetric_heap, &
        initial_team) &
        bind(C)
      import c_ptr, c_intptr_t
      implicit none
      type(c_ptr), intent(out) :: symmetric_heap
      integer(c_intptr_t), intent(out) :: symmetric_heap_start, symmetric_heap_size
      type(c_ptr), intent(out) :: non_symmetric_heap
      type(c_ptr), intent(out) :: initial_team
    end subroutine

    subroutine caf_decaffeinate(exit_code) bind(C)
      !! void c_decaffeinate();
      import c_int
      implicit none
      integer(c_int), value :: exit_code
    end subroutine

    pure subroutine caf_fatal_error(str) bind(C)
      !! void caf_fatal_error( const CFI_cdesc_t* Fstr )
      import c_char
      implicit none
      character(kind=c_char,len=:), pointer, intent(in) :: str
    end subroutine
    ! _________________ Image enumeration ____________________

    function caf_this_image(gex_team) bind(C)
      !! int caf_this_image(gex_TM_t gex_team);
      import c_ptr, c_int
      implicit none
      type(c_ptr), value :: gex_team
      integer(c_int) caf_this_image
    end function

    pure function caf_num_images(gex_team) bind(C)
      !! int caf_num_images(gex_TM_t gex_team);
      import c_ptr, c_int
      implicit none
      type(c_ptr), value :: gex_team
      integer(c_int) caf_num_images
    end function

    ! _________________ Memory allocation ____________________

    function caf_allocate(mspace, bytes) result(ptr) bind(c)
       import c_size_t, c_ptr
       implicit none
       type(c_ptr), intent(in), value :: mspace
       integer(c_size_t), intent(in), value :: bytes
       type(c_ptr) :: ptr
    end function

    subroutine caf_allocate_remaining(mspace, allocated_space, allocated_size) bind(c)
      import c_size_t, c_ptr
      implicit none
      type(c_ptr), intent(in), value :: mspace
      type(c_ptr), intent(out) :: allocated_space
      integer(c_size_t), intent(out) :: allocated_size
    end subroutine

    subroutine caf_deallocate(mspace, mem) bind(c)
      import c_ptr
      implicit none
      type(c_ptr), intent(in), value :: mspace
      type(c_ptr), intent(in), value :: mem
    end subroutine

    subroutine caf_establish_mspace(mspace, mem, mem_size) bind(c)
      import c_size_t, c_ptr
      implicit none
      type(c_ptr), intent(out) :: mspace
      type(c_ptr), intent(in), value :: mem
      integer(c_size_t), intent(in), value :: mem_size
    end subroutine

    ! ___________________ PRIF Queries ______________________

    module function caf_convert_base_addr(addr, image) result(ptr) bind(c)
      implicit none
      type(c_ptr), intent(in), value :: addr
      integer(c_int), intent(in), value :: image
      integer(c_intptr_t) :: ptr
    end function


    ! _______________________ Contiguous RMA ____________________________
    subroutine caf_put(image, dest, src, size) bind(c)
      !! void caf_put(int image, intptr_t dest, void* src, size_t size)
      import c_ptr, c_int, c_intptr_t, c_size_t
      implicit none
      integer(c_int), intent(in), value :: image
      integer(c_intptr_t), intent(in), value :: dest
      type(c_ptr), intent(in), value :: src
      integer(c_size_t), intent(in), value :: size
    end subroutine

    subroutine caf_get(image, dest, src, size) bind(c)
      !! void caf_get(int image, void* dest, intptr_t src, size_t size)
      import c_ptr, c_int, c_intptr_t, c_size_t
      implicit none
      integer(c_int), intent(in), value :: image
      type(c_ptr), intent(in), value :: dest
      integer(c_intptr_t), intent(in), value :: src
      integer(c_size_t), intent(in), value :: size
    end subroutine

    ! _______________________ Strided RMA ____________________________
    subroutine caf_put_strided(dims, image_num, remote_ptr, remote_stride, &
                               current_image_buffer, current_image_stride, &
                               element_size, extent) bind(c)
      !! void caf_put_strided(int dims, int image_num, 
      !!                      intptr_t remote_ptr, void* remote_stride, 
      !!                      void *current_image_buffer, void * current_image_stride, 
      !!                      size_t element_size, void *extent)
      import c_ptr, c_int, c_intptr_t, c_size_t
      implicit none
      integer(c_int), intent(in), value :: dims
      integer(c_int), intent(in), value :: image_num
      integer(c_intptr_t), intent(in), value :: remote_ptr
      type(c_ptr), intent(in), value :: remote_stride
      type(c_ptr), intent(in), value :: current_image_buffer
      type(c_ptr), intent(in), value :: current_image_stride
      integer(c_size_t), intent(in), value :: element_size
      type(c_ptr), intent(in), value :: extent
    end subroutine

    subroutine caf_get_strided(dims, image_num, remote_ptr, remote_stride, &
                               current_image_buffer, current_image_stride, &
                               element_size, extent) bind(c)
      !! void caf_get_strided(int dims, int image_num, 
      !!                      intptr_t remote_ptr, void* remote_stride, 
      !!                      void *current_image_buffer, void * current_image_stride, 
      !!                      size_t element_size, void *extent)
      import c_ptr, c_int, c_intptr_t, c_size_t
      implicit none
      integer(c_int), intent(in), value :: dims
      integer(c_int), intent(in), value :: image_num
      integer(c_intptr_t), intent(in), value :: remote_ptr
      type(c_ptr), intent(in), value :: remote_stride
      type(c_ptr), intent(in), value :: current_image_buffer
      type(c_ptr), intent(in), value :: current_image_stride
      integer(c_size_t), intent(in), value :: element_size
      type(c_ptr), intent(in), value :: extent
    end subroutine

    ! __________________ SYNC Statements _____________________

    subroutine caf_sync_memory() bind(C)
      !! void caf_sync_memory();
    end subroutine

    subroutine caf_sync_team(team) bind(C)
      !! void caf_sync_team(gex_TM_t team);
       import c_ptr
       implicit none
       type(c_ptr), value :: team
    end subroutine

    ! ______________ Collective Subroutines __________________

     subroutine caf_co_broadcast(a, source_image, Nelem, team) bind(C)
       !! void c_co_broadcast(CFI_cdesc_t * a_desc, int source_image, int num_elements, gex_TM_t team);
       import c_int, c_ptr
       implicit none
       type(*) a(..)
       integer(c_int), value :: source_image, Nelem
       type(c_ptr), value :: team
     end subroutine

     subroutine caf_co_reduce(a, result_image, num_elements, Coll_ReduceSub, client_data, team) bind(C)
       !! void caf_co_reduce(CFI_cdesc_t* a_desc, int result_image, int num_elements, gex_Coll_ReduceFn_t* user_op, void* client_data)
       import c_int, c_ptr, c_size_t, c_funptr
       implicit none
       type(*) a(..)
       integer(c_int), value :: result_image
       type(c_ptr), value :: client_data
       type(c_funptr), value :: Coll_ReduceSub
       integer(c_size_t), value :: num_elements
       type(c_ptr), value :: team
     end subroutine

     subroutine caf_co_sum(a, result_image, num_elements, team) bind(C)
       !! void c_co_sum(CFI_cdesc_t* a_desc, int result_image, size_t num_elements, gex_TM_t team);
       import c_int, c_ptr, c_size_t
       implicit none
       type(*) a(..)
       integer(c_int), value :: result_image
       integer(c_size_t), value :: num_elements
       type(c_ptr), value :: team
     end subroutine

     subroutine caf_co_min(a, result_image, num_elements, team) bind(C)
       !! void c_co_min(CFI_cdesc_t* a_desc, int result_image, size_t num_elements, gex_TM_t team);
       import c_int, c_ptr, c_size_t
       implicit none
       type(*) a(..)
       integer(c_int), value :: result_image
       integer(c_size_t), value :: num_elements
       type(c_ptr), value :: team
     end subroutine

     subroutine caf_co_max(a, result_image, num_elements, team) bind(C)
       !! void c_co_max(CFI_cdesc_t* a_desc, int result_image, size_t num_elements, gex_TM_t team);
       import c_int, c_ptr, c_size_t
       implicit none
       type(*) a(..)
       integer(c_int), value :: result_image
       integer(c_size_t), value :: num_elements
       type(c_ptr), value :: team
     end subroutine

     subroutine caf_form_team(current_team, new_team, team_number, new_index) bind(C)
      !! void caf_form_team(gex_TM_t* current_team, gex_TM_t* new_team, int64_t team_number, int new_index);
      import c_ptr, c_int, c_int64_t
      type(c_ptr), intent(in), value :: current_team
      type(c_ptr), intent(out) :: new_team
      integer(c_int64_t), intent(in), value :: team_number
      integer(c_int), intent(in), value :: new_index
     end subroutine

  end interface

contains

  pure function as_int(ptr)
    type(c_ptr), intent(in) :: ptr
    integer(c_intptr_t) :: as_int

    call_assert(storage_size(ptr) == storage_size(as_int))

    as_int = transfer(ptr, as_int)
  end function

  pure function as_c_ptr(i)
    integer(c_intptr_t), intent(in) :: i
    type(c_ptr) :: as_c_ptr

    as_c_ptr = transfer(i, as_c_ptr)
  end function

  subroutine base_pointer(coarray_handle, image_num, ptr)
    type(prif_coarray_handle), intent(in) :: coarray_handle
    integer(c_int), intent(in) :: image_num
    integer(c_intptr_t), intent(out) :: ptr

    call_assert(coarray_handle_check(coarray_handle))
    call_assert_describe(image_num > 0 .and. image_num <= initial_team%num_images, "base_pointer: image_num not within valid range")
    ptr = caf_convert_base_addr(coarray_handle%info%coarray_data, image_num)
  end subroutine

  subroutine unimplemented(proc_name)
    character(len=*), intent(in) ::  proc_name
    call prif_error_stop(quiet=.false._c_bool, stop_code_char=proc_name // " is not yet implemented")
  end subroutine

  pure function optional_value(var) result(c_val)
    integer, intent(in), optional :: var
    integer(c_int) c_val
    if (present(var)) then
      c_val = var
    else
      c_val = 0_c_int
    end if
  end function

  ! verify state invariants for a coarray_handle
  ! Note this function validates invariants with deliberately UNconditional assertions
  ! Suggested caller usage for conditional validation is: 
  !   call_assert(coarray_handle_check(coarray_handle))
  elemental impure function coarray_handle_check(coarray_handle) result(result_)
    implicit none
    type(prif_coarray_handle), intent(in) :: coarray_handle
    logical :: result_
    integer(c_int) :: i

    call assert_always(associated(coarray_handle%info), "unassociated info pointer in prif_coarray_handle")
    associate(info => coarray_handle%info)
      call assert_always(info%corank >= 1, "invalid corank in prif_coarray_handle")
      call assert_always(info%corank <= size(info%ucobounds), "invalid corank in prif_coarray_handle")
      call assert_always(all([(info%lcobounds(i) <= info%ucobounds(i), i = 1, info%corank)]), &
                         "invalid cobounds in prif_coarray_handle")
      call assert_always(info%coarray_size > 0, "invalid data size in prif_coarray_handle")
      call assert_always(c_associated(info%coarray_data), "invalid data pointer in prif_coarray_handle")
    end associate

    result_ = .true.
  end function

  subroutine caf_establish_child_heap
    if (current_team%info%this_image == 1) then
      call caf_allocate_remaining( &
          current_team%info%heap_mspace, &
          current_team%info%child_heap_info%allocated_memory, &
          current_team%info%child_heap_info%size)
      current_team%info%child_heap_info%offset = &
          as_int(current_team%info%child_heap_info%allocated_memory) - current_team%info%heap_start
    end if
    call prif_co_broadcast(current_team%info%child_heap_info, 1)
  end subroutine

  logical function caf_have_child_teams()
    caf_have_child_teams = associated(current_team%info%child_heap_info)
  end function

end submodule prif_private_s
