! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module caffeine_h_m
  ! Fortran module shadowing the caffeine.h header file
  use iso_c_binding, only : c_int, c_ptr, c_size_t, c_funptr, c_bool
  implicit none

  private
  public :: caf_c_caffeinate, caf_c_decaffeinate
  public :: caf_c_num_images, caf_c_this_image
  public :: caf_c_sync_all
  public :: caf_c_co_broadcast, caf_c_co_sum, caf_c_co_min, caf_c_co_max, caf_c_co_reduce
  public :: caf_c_same_cfi_type, caf_c_elem_len, caf_c_numeric_type, caf_c_is_f_string

  interface

    ! ________ Program initiation and finalization ___________

    subroutine caf_c_caffeinate(argc, argv) bind(C)
      !! void c_caffeinate(int argc, char *argv[]);
      import c_int, c_ptr
      implicit none
      integer(c_int), value :: argc
      type(c_ptr) argv(*)
    end subroutine

    subroutine caf_c_decaffeinate(exit_code) bind(C)
      !! void c_decaffeinate();
      import c_int
      implicit none
      integer(c_int), value :: exit_code
    end subroutine

    ! _________________ Image enumeration ____________________

    pure function caf_c_this_image() bind(C)
      !! int caf_c_this_image();
      import c_int
      implicit none
      integer(c_int) caf_c_this_image
    end function

    pure function caf_c_num_images() bind(C)
      !! int caf_c_num_images();
      import c_int
      implicit none
      integer(c_int) caf_c_num_images
    end function

    ! __________________ Synchronization _____________________

    subroutine caf_c_sync_all() bind(C)
      !! void caf_c_sync_all();
      import c_int
      implicit none
    end subroutine

    ! ______________ Collective Subroutines __________________

     subroutine caf_c_co_broadcast(a, source_image, stat, Nelem) bind(C)
       !! void c_co_broadcast(CFI_cdesc_t * a_desc, int source_image, int* stat, int num_elements);
       import c_int, c_ptr
       implicit none
       type(*) a(..)
       type(c_ptr), value :: stat
       integer(c_int), value :: source_image, Nelem
     end subroutine

     subroutine caf_c_co_reduce(a, result_image, c_loc_stat, c_loc_errmsg, num_elements, Coll_ReduceSub, client_data ) bind(C)
       !! void caf_c_co_reduce(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, int num_elements, gex_Coll_ReduceFn_t* user_op, void* client_data)
       import c_int, c_ptr, c_size_t, c_funptr
       implicit none 
       type(*) a(..)
       integer(c_int), value :: result_image
       type(c_ptr), value :: c_loc_stat, c_loc_errmsg, client_data
       type(c_funptr), value :: Coll_ReduceSub
       integer(c_size_t), value :: num_elements
     end subroutine

     subroutine caf_c_co_sum(a, result_image, c_loc_stat, c_loc_errmsg, num_elements) bind(C)
       !! void c_co_sum(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements);
       import c_int, c_ptr, c_size_t
       implicit none 
       type(*) a(..)
       integer(c_int), value :: result_image
       type(c_ptr), value :: c_loc_stat, c_loc_errmsg
       integer(c_size_t), value :: num_elements
     end subroutine

     subroutine caf_c_co_min(a, result_image, c_loc_stat, c_loc_errmsg, num_elements) bind(C)
       !! void c_co_min(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements);
       import c_int, c_ptr, c_size_t
       implicit none 
       type(*) a(..)
       integer(c_int), value :: result_image
       type(c_ptr), value :: c_loc_stat, c_loc_errmsg
       integer(c_size_t), value :: num_elements
     end subroutine

     subroutine caf_c_co_max(a, result_image, c_loc_stat, c_loc_errmsg, num_elements) bind(C)
       !! void c_co_max(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements);
       import c_int, c_ptr, c_size_t
       implicit none 
       type(*) a(..)
       integer(c_int), value :: result_image
       type(c_ptr), value :: c_loc_stat, c_loc_errmsg
       integer(c_size_t), value :: num_elements
     end subroutine

     logical(c_bool) pure function caf_c_same_cfi_type(a, b) bind(C)
       !! bool caf_c_same_cfi_type(CFI_cdesc_t* a_desc, CFI_cdesc_t* a_desc);
       import c_bool
       type(*), intent(in) :: a(..), b(..)
     end function
  
     logical(c_bool) pure function caf_c_numeric_type(a) bind(C)
       !! bool caf_c_numeric_type(CFI_cdesc_t* a_desc);
       import c_bool
       type(*), intent(in) :: a(..)
     end function
  
     logical(c_bool) pure function caf_c_is_f_string(a) bind(C)
       !! bool caf_c_is_f_string(CFI_cdesc_t* a_desc);
       import c_bool
       type(*), intent(in) :: a(..)
     end function

     pure function caf_c_elem_len(a) result(a_elem_len) bind(C)
       !! size_t caf_c_elem_len(CFI_cdesc_t* a_desc);
       import c_size_t
       type(*), intent(in) :: a(..)
       integer(c_size_t), target :: a_elem_len
     end function
  
  end interface

end module caffeine_h_m
