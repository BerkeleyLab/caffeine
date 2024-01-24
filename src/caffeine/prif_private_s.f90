! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif) prif_private_s
  implicit none

  type(team_data), target :: initial_team
  type(prif_team_type) :: current_team
  type(c_ptr) :: non_symmetric_heap_mspace

  interface

    pure module subroutine assert(assertion, description, diagnostics)
      implicit none
      logical, intent(in) :: assertion
      character(len=*), intent(in) :: description
      class(*), intent(in), optional :: diagnostics
    end subroutine

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

    ! _________________ Image enumeration ____________________

    function caf_this_image(team) bind(C)
      !! int caf_this_image();
      import c_ptr, c_int
      implicit none
      type(c_ptr), value :: team
      integer(c_int) caf_this_image
    end function

    pure function caf_num_images(team) bind(C)
      !! int caf_num_images();
      import c_ptr, c_int
      implicit none
      type(c_ptr), value :: team
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

    subroutine caf_deallocate(mspace, mem) bind(c)
      import c_ptr
      implicit none
      type(c_ptr), intent(in), value :: mspace
      type(c_ptr), intent(in), value :: mem
    end subroutine

    ! ___________________ PRIF Queries ______________________

    module function caf_convert_base_addr(addr, image) result(ptr) bind(c)
      implicit none
      type(c_ptr), intent(in), value :: addr
      integer(c_int), intent(in), value :: image
      integer(c_intptr_t) :: ptr
    end function


    ! _______________________ RMA ____________________________
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
    ! __________________ Synchronization _____________________

    subroutine caf_sync_all() bind(C)
      !! void caf_sync_all();
      import c_int
      implicit none
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

     logical(c_bool) pure function caf_same_cfi_type(a, b) bind(C)
       !! bool caf_same_cfi_type(CFI_cdesc_t* a_desc, CFI_cdesc_t* a_desc);
       import c_bool
       type(*), intent(in) :: a(..), b(..)
     end function

     logical(c_bool) pure function caf_numeric_type(a) bind(C)
       !! bool caf_numeric_type(CFI_cdesc_t* a_desc);
       import c_bool
       type(*), intent(in) :: a(..)
     end function

     logical(c_bool) pure function caf_is_f_string(a) bind(C)
       !! bool caf_is_f_string(CFI_cdesc_t* a_desc);
       import c_bool
       type(*), intent(in) :: a(..)
     end function

     pure function caf_elem_len(a) result(a_elem_len) bind(C)
       !! size_t caf_elem_len(CFI_cdesc_t* a_desc);
       import c_size_t
       type(*), intent(in) :: a(..)
       integer(c_size_t), target :: a_elem_len
     end function

  end interface

contains

  pure function as_int(ptr)
    type(c_ptr), intent(in) :: ptr
    integer(c_intptr_t) :: as_int

    ! the following snippet ensures at compile time that c_ptr and
    ! c_intptr_t are actually the same size
    integer, parameter :: int_ptr_check = merge(c_intptr_t, 0, storage_size(ptr) == storage_size(as_int))
    integer(int_ptr_check), parameter :: unused = 0_int_ptr_check

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

    integer(c_int) :: num_img

    ! TODO TEAMS: either move the assertion below into `caf_convert_base_addr()`
    ! (avoiding the need to call prif_num_images here to fetch initial team size)
    ! or cache the initial team size in a private module variable so we can just access it (issue #62)
    call prif_num_images(num_images=num_img)
    call assert(image_num > 0 .and. image_num <= num_img, "base_pointer: image_num not within valid range")
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

end submodule prif_private_s
