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
  public :: caf_c_co_broadcast, caf_c_co_sum, caf_c_co_min, caf_c_co_max
  public :: caf_c_co_reduce_char, caf_c_co_reduce_int32, caf_c_co_reduce_bool, caf_c_co_reduce_float
  public :: caf_c_is_character

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

     subroutine caf_c_co_sum(a, result_image, c_loc_stat, c_loc_errmsg, num_elements) bind(C)
       !! void c_co_sum(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements);
       import c_int, c_ptr, c_size_t
       implicit none 
       type(*) :: a(..)
       integer(c_int), value :: result_image
       type(c_ptr), value :: c_loc_stat, c_loc_errmsg
       integer(c_size_t), value :: num_elements
     end subroutine

     subroutine caf_c_co_min(a, result_image, c_loc_stat, c_loc_errmsg, num_elements) bind(C)
       !! void c_co_min(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements);
       import c_int, c_ptr, c_size_t
       implicit none 
       type(*) :: a(..)
       integer(c_int), value :: result_image
       type(c_ptr), value :: c_loc_stat, c_loc_errmsg
       integer(c_size_t), value :: num_elements
     end subroutine

     subroutine caf_c_co_max(a, result_image, c_loc_stat, c_loc_errmsg, num_elements) bind(C)
       !! void c_co_max(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements);
       import c_int, c_ptr, c_size_t
       implicit none 
       type(*) :: a(..)
       integer(c_int), value :: result_image
       type(c_ptr), value :: c_loc_stat, c_loc_errmsg
       integer(c_size_t), value :: num_elements
     end subroutine

     subroutine caf_c_co_reduce_char(c_loc_a, Nelem, c_loc_stat, result_image, Coll_ReduceSub_c_char, client_data) bind(C)
       !! void caf_c_co_reduce_char(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation, void* client_data);
       import c_ptr, c_size_t, c_funptr, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat, Coll_ReduceSub_c_char, client_data
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
     end subroutine

     subroutine caf_c_co_reduce_int32(c_loc_a, Nelem, c_loc_stat, result_image, Coll_ReduceSub_c_int32_t) bind(C)
       !! void caf_c_co_reduce_int32(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation);
       import c_ptr, c_size_t, c_funptr, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
       type(c_funptr), value :: Coll_ReduceSub_c_int32_t
     end subroutine

     subroutine caf_c_co_reduce_bool(c_loc_a, Nelem, c_loc_stat, result_image, Coll_ReduceSub_c_bool) bind(C)
       !! void caf_c_co_reduce_bool(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation);
       import c_ptr, c_size_t, c_funptr, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
       type(c_funptr), value :: Coll_ReduceSub_c_bool
     end subroutine

     subroutine caf_c_co_reduce_float(c_loc_a, Nelem, c_loc_stat, result_image, Coll_ReduceSub_c_float) bind(C)
       !! void caf_c_co_reduce_float(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation);
       import c_ptr, c_size_t, c_funptr, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
       type(c_funptr), value :: Coll_ReduceSub_c_float
     end subroutine

     logical(c_bool) pure function caf_c_is_character(a) bind(C)
       ! bool caf_c_is_character(CFI_cdesc_t* a_desc)
       import c_bool
       type(*), intent(in) :: a(..)
     end function
  
  end interface

end module caffeine_h_m
