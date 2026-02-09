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
        c_int32_t, &
        c_loc, &
        c_null_funptr, &
        c_sizeof, &
        ! DOB: The following is a gfortran-14 bug workaround. No idea why this works...
        c_funptr_ => c_funptr

  implicit none

  type(prif_team_descriptor), target :: initial_team
  type(prif_team_type) :: current_team
  integer(c_intptr_t) :: total_heap_size, non_symmetric_heap_size

  interface

    ! ________ Program initiation and finalization ___________

    subroutine caf_caffeinate( &
        total_heap_size, &
        symmetric_heap, &
        symmetric_heap_start, &
        symmetric_heap_size, &
        initial_team) &
        bind(C)
      import c_ptr, c_intptr_t
      implicit none
      integer(c_intptr_t), intent(out) :: total_heap_size, symmetric_heap_start, symmetric_heap_size
      type(c_ptr), intent(out) :: symmetric_heap
      type(c_ptr), intent(out) :: initial_team
    end subroutine

    subroutine caf_decaffeinate(exit_code) bind(C)
      !! void c_decaffeinate();
      import c_int
      implicit none
      integer(c_int), value :: exit_code
    end subroutine

    subroutine caf_fail_image() bind(C)
      !! void caf_fail_image();
      implicit none
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

    function caf_image_to_initial(gex_team, image_num) bind(C)
      !! int caf_image_to_initial(gex_TM_t tm, int image_num)
      import c_ptr, c_int
      implicit none
      type(c_ptr), value :: gex_team
      integer(c_int), value :: image_num
      integer(c_int) caf_image_to_initial
    end function

    function caf_image_from_initial(gex_team, image_num) bind(C)
      !! int caf_image_from_initial(gex_TM_t tm, int image_num)
      import c_ptr, c_int
      implicit none
      type(c_ptr), value :: gex_team
      integer(c_int), value :: image_num
      integer(c_int) caf_image_from_initial
    end function

    ! _________________ Memory allocation ____________________

    function caf_allocate(mspace, bytes) result(ptr) bind(c)
       import c_size_t, c_ptr
       implicit none
       type(c_ptr), intent(in), value :: mspace
       integer(c_size_t), intent(in), value :: bytes
       type(c_ptr) :: ptr
    end function

    function caf_allocate_non_symmetric(bytes) result(ptr) bind(c)
       import c_size_t, c_ptr
       implicit none
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

    subroutine caf_deallocate_non_symmetric(mem) bind(c)
      import c_ptr
      implicit none
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

    module subroutine sync_init()
    end subroutine

    subroutine caf_sync_memory() bind(C)
      !! void caf_sync_memory();
    end subroutine

    subroutine caf_sync_team(team) bind(C)
      !! void caf_sync_team(gex_TM_t team);
       import c_ptr
       implicit none
       type(c_ptr), value :: team
    end subroutine

    ! _______________________ Events  ____________________________
    subroutine caf_event_post(image, event_var_ptr, segment_boundary, release_fence) bind(c)
      !! void caf_event_post(int image, intptr_t event_var_ptr, int segment_boundary, int release_fence)
      import c_int, c_intptr_t
      implicit none
      integer(c_int), intent(in), value :: image
      integer(c_intptr_t), intent(in), value :: event_var_ptr
      integer(c_int), intent(in), value :: segment_boundary
      integer(c_int), intent(in), value :: release_fence
    end subroutine

    subroutine caf_event_wait(event_var_ptr, threshold, segment_boundary, acquire_fence, maybe_concurrent) bind(c)
      !! void caf_event_wait(void *event_var_ptr, int64_t threshold, int segment_boundary, int acquire_fence, int maybe_concurrent)
      import c_int64_t, c_ptr, c_int
      implicit none
      type(c_ptr), intent(in), value :: event_var_ptr
      integer(c_int64_t), intent(in), value :: threshold
      integer(c_int), intent(in), value :: segment_boundary, acquire_fence, maybe_concurrent
    end subroutine

    subroutine caf_event_query(event_var_ptr, count) bind(c)
      !! void caf_event_query(void *event_var_ptr, int64_t *count)
      import c_int64_t, c_ptr
      implicit none
      type(c_ptr), intent(in), value :: event_var_ptr
      integer(c_int64_t), intent(out) :: count
    end subroutine

    ! _______________________ Atomics  ____________________________

    subroutine caf_atomic_int(opcode, image, addr, result, operand1, operand2) bind(c)
      !! void caf_atomic_int(int opcode, int image, void* addr, int64_t *result, int64_t op1, int64_t op2)
      import c_int, c_intptr_t, PRIF_ATOMIC_INT_KIND
      implicit none
      integer(c_int), intent(in), value :: opcode
      integer(c_int), intent(in), value :: image
      integer(c_intptr_t), intent(in), value :: addr
      integer(PRIF_ATOMIC_INT_KIND), intent(out) :: result
      integer(PRIF_ATOMIC_INT_KIND), intent(in), value :: operand1
      integer(PRIF_ATOMIC_INT_KIND), intent(in), value :: operand2
    end subroutine

    subroutine caf_atomic_logical(opcode, image, addr, result, operand1, operand2) bind(c)
      !! void caf_atomic_logical(int opcode, int image, void* addr, int64_t *result, int64_t op1, int64_t op2)
      import c_int, c_intptr_t, PRIF_ATOMIC_LOGICAL_KIND
      implicit none
      integer(c_int), intent(in), value :: opcode
      integer(c_int), intent(in), value :: image
      integer(c_intptr_t), intent(in), value :: addr
      logical(PRIF_ATOMIC_LOGICAL_KIND), intent(out) :: result
      logical(PRIF_ATOMIC_LOGICAL_KIND), intent(in), value :: operand1
      logical(PRIF_ATOMIC_LOGICAL_KIND), intent(in), value :: operand2
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

  interface num_to_str
    module procedure num_to_str32
    module procedure num_to_str64
  end interface

contains

  pure function num_to_str32(num, is_mem_size) result(str)
    integer(c_int32_t), value :: num
    logical, intent(in), optional :: is_mem_size
    character(len=:), allocatable :: str
    
    str = num_to_str64(int(num, c_int64_t), is_mem_size)
  end function

  pure function num_to_str64(num, is_mem_size) result(str)
    integer(c_int64_t), value :: num
    logical, intent(in), optional :: is_mem_size
    character(len=:), allocatable :: str, unit
    character(len=40) num_str
    integer(c_int64_t) :: divisor

    if (present(is_mem_size)) then
    if (is_mem_size) then
      divisor = 1 
      ! Try to strike a compromise between digits and round off
#     define CAF_USE_DIV(div, unit_str)  \
        if ((num .ge. 10*div) .or. (num .ge. div .and. mod(num, div) == 0)) then ; \
          divisor = div; unit = unit_str; exit; \
        end if
      do
        CAF_USE_DIV(ishft(1_c_int64_t,40), " TiB")
        CAF_USE_DIV(ishft(1_c_int64_t,30), " GiB")
        CAF_USE_DIV(ishft(1_c_int64_t,20), " MiB")
        CAF_USE_DIV(ishft(1_c_int64_t,10), " KiB")
        CAF_USE_DIV(1_c_int64_t, " B");
        exit
      end do 
      num = num / divisor
#     undef CAF_USE_DIV
    end if
    end if

    write(num_str, '(i0)') num
    str = trim(adjustl(num_str))
    if (allocated(unit)) then
      str = str // unit
    end if
  end function

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

  ! Report the provided error stat/msg using the provided optional stat/errmsg args
  subroutine report_error(report_stat, report_msg, stat, errmsg, errmsg_alloc)
    integer(c_int), intent(in) :: report_stat
    character(len=*), intent(in) :: report_msg
    integer(c_int), intent(out), optional :: stat
    character(len=*), intent(inout), optional :: errmsg
    character(len=:), intent(inout), allocatable , optional :: errmsg_alloc

    call_assert(report_stat /= 0)
    if (.not. present(stat)) then
      call prif_error_stop(.false._c_bool, stop_code_char=report_msg)
    else
      stat = report_stat
      if (present(errmsg)) then
        errmsg = report_msg
      else if (present(errmsg_alloc)) then
        errmsg_alloc = report_msg
      end if
    end if
  end subroutine

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
