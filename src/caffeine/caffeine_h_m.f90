! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module caffeine_h_m
  ! Fortran module shadowing the caffeine.h header file
  use iso_c_binding, only : c_int, c_ptr, c_size_t, c_funptr, c_bool, c_size_t, c_intmax_t
  implicit none

  private
  public :: caf_caffeinate, caf_decaffeinate
  public :: caf_num_images, caf_this_image
  public :: caf_allocate
  public :: caf_sync_all
  public :: caf_co_broadcast, caf_co_sum, caf_co_min, caf_co_max, caf_co_reduce
  public :: caf_same_cfi_type, caf_elem_len, caf_numeric_type, caf_is_f_string

  interface

    ! ________ Program initiation and finalization ___________

    subroutine caf_caffeinate() bind(C)
      implicit none
    end subroutine

    subroutine caf_decaffeinate(exit_code) bind(C)
      !! void c_decaffeinate();
      import c_int
      implicit none
      integer(c_int), value :: exit_code
    end subroutine

    ! _________________ Image enumeration ____________________

    function caf_this_image() bind(C)
      !! int caf_this_image();
      import c_int
      implicit none
      integer(c_int) caf_this_image
    end function

    pure function caf_num_images() bind(C)
      !! int caf_num_images();
      import c_int
      implicit none
      integer(c_int) caf_num_images
    end function

    ! _________________ Memory allocation ____________________

    function caf_allocate(sz, corank, lcobounds, ucobounds, final_func, coarray_handle) result(ptr) bind(c)
       import c_int, c_size_t, c_intmax_t, c_funptr, c_ptr
       implicit none
       integer(kind=c_size_t), intent(in), value :: sz
       integer(kind=c_int), intent(in), value :: corank
       integer(kind=c_intmax_t), dimension(:), intent(in) :: lcobounds, ucobounds
       type(c_funptr), intent(in), value  :: final_func
       type(c_ptr), intent(out) :: coarray_handle
       type(c_ptr) :: ptr
     end function
    ! __________________ Synchronization _____________________

    subroutine caf_sync_all() bind(C)
      !! void caf_sync_all();
      import c_int
      implicit none
    end subroutine

    ! ______________ Collective Subroutines __________________

     subroutine caf_co_broadcast(a, source_image, stat, Nelem) bind(C)
       !! void c_co_broadcast(CFI_cdesc_t * a_desc, int source_image, int* stat, int num_elements);
       import c_int, c_ptr
       implicit none
       type(*) a(..)
       type(c_ptr), value :: stat
       integer(c_int), value :: source_image, Nelem
     end subroutine

     subroutine caf_co_reduce(a, result_image, c_loc_stat, c_loc_errmsg, num_elements, Coll_ReduceSub, client_data ) bind(C)
       !! void caf_co_reduce(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, int num_elements, gex_Coll_ReduceFn_t* user_op, void* client_data)
       import c_int, c_ptr, c_size_t, c_funptr
       implicit none 
       type(*) a(..)
       integer(c_int), value :: result_image
       type(c_ptr), value :: c_loc_stat, c_loc_errmsg, client_data
       type(c_funptr), value :: Coll_ReduceSub
       integer(c_size_t), value :: num_elements
     end subroutine

     subroutine caf_co_sum(a, result_image, c_loc_stat, c_loc_errmsg, num_elements) bind(C)
       !! void c_co_sum(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements);
       import c_int, c_ptr, c_size_t
       implicit none 
       type(*) a(..)
       integer(c_int), value :: result_image
       type(c_ptr), value :: c_loc_stat, c_loc_errmsg
       integer(c_size_t), value :: num_elements
     end subroutine

     subroutine caf_co_min(a, result_image, c_loc_stat, c_loc_errmsg, num_elements) bind(C)
       !! void c_co_min(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements);
       import c_int, c_ptr, c_size_t
       implicit none 
       type(*) a(..)
       integer(c_int), value :: result_image
       type(c_ptr), value :: c_loc_stat, c_loc_errmsg
       integer(c_size_t), value :: num_elements
     end subroutine

     subroutine caf_co_max(a, result_image, c_loc_stat, c_loc_errmsg, num_elements) bind(C)
       !! void c_co_max(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements);
       import c_int, c_ptr, c_size_t
       implicit none 
       type(*) a(..)
       integer(c_int), value :: result_image
       type(c_ptr), value :: c_loc_stat, c_loc_errmsg
       integer(c_size_t), value :: num_elements
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

end module caffeine_h_m
