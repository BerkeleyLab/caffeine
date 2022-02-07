! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module caffeine_h_m
  ! Fortran module shadowing the caffeine.h header file
  use iso_c_binding, only : c_int, c_ptr, c_size_t, c_funptr
  implicit none

  private
  public :: c_caffeinate, c_decaffeinate
  public :: c_num_images, c_this_image
  public :: c_sync_all
  public :: c_co_broadcast, c_co_sum
  public :: c_co_min_int32, c_co_min_int64, c_co_min_float, c_co_min_double
  public :: c_co_max_int32, c_co_max_int64, c_co_max_float, c_co_max_double
  public :: c_co_reduce_char, c_co_reduce_int32, c_co_reduce_bool, c_co_reduce_float

  interface

    ! ________ Program initiation and finalization ___________

    subroutine c_caffeinate(argc, argv) bind(C)
      !! void c_caffeinate(int argc, char *argv[]);
      import c_int, c_ptr
      implicit none
      integer(c_int), value :: argc
      type(c_ptr) argv(*)
    end subroutine

    subroutine c_decaffeinate(exit_code) bind(C)
      !! void c_decaffeinate();
      import c_int
      implicit none
      integer(c_int), value :: exit_code
    end subroutine

    ! _________________ Image enumeration ____________________

    pure function c_this_image() bind(C)
      !! int c_this_image();
      import c_int
      implicit none
      integer(c_int) c_this_image
    end function

    pure function c_num_images() bind(C)
      !! int c_num_images();
      import c_int
      implicit none
      integer(c_int) c_num_images
    end function

    ! __________________ Synchronization _____________________

    subroutine c_sync_all() bind(C)
      !! void c_sync_all();
      import c_int
      implicit none
    end subroutine

    ! ______________ Collective Subroutines __________________

!void c_co_max_int32(void* c_loc_a, size_t Nelem, int* stat, int result_image);
!void c_co_max_int64(void* c_loc_a, size_t Nelem, int* stat, int result_image);
!void c_co_max_float(void* c_loc_a, size_t Nelem, int* stat, int result_image);
!void c_co_max_double(void* c_loc_a, size_t Nelem, int* stat, int result_image);
!

     subroutine c_co_broadcast(a, source_image, stat, Nelem) bind(C)
       !! void c_co_broadcast(CFI_cdesc_t * a_desc, int source_image, int* stat, int num_elements);
       import c_int, c_ptr
       implicit none
       type(*) a(..)
       type(c_ptr), value :: stat
       integer(c_int), value :: source_image, Nelem
     end subroutine

     subroutine c_co_sum(a, result_image, c_loc_stat, c_loc_errmsg, num_elements) bind(C)
       !! void c_co_sum(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements);
       import c_int, c_ptr, c_size_t
       implicit none 
       type(*) :: a(..)
       integer(c_int), value :: result_image
       type(c_ptr), value :: c_loc_stat, c_loc_errmsg
       integer(c_size_t), value :: num_elements
     end subroutine

     ! TODO: Replace the type-specific C interfaces below with assumed-type interfaces like the above co_{broadcast,sum} interfaces

     subroutine c_co_min_int32(c_loc_a, Nelem, c_loc_stat, result_image) bind(C)
       !! void c_co_min_int32(void* c_loc_a, size_t Nelem, int* stat, int result_image);
       import c_ptr, c_size_t, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
     end subroutine

     subroutine c_co_min_int64(c_loc_a, Nelem, c_loc_stat, result_image) bind(C)
       !! void c_co_min_int64(void* c_loc_a, size_t Nelem, int* stat, int result_image);
       import c_ptr, c_size_t, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
     end subroutine

     subroutine c_co_min_float(c_loc_a, Nelem, c_loc_stat, result_image) bind(C)
       !! void c_co_min_float(void* c_loc_a, size_t Nelem, int* stat, int result_image);
       import c_ptr, c_size_t, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
     end subroutine

     subroutine c_co_min_double(c_loc_a, Nelem, c_loc_stat, result_image) bind(C)
       !! void c_co_min_double(void* c_loc_a, size_t Nelem, int* stat, int result_image);
       import c_ptr, c_size_t, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
     end subroutine

     subroutine c_co_max_int32(c_loc_a, Nelem, c_loc_stat, result_image) bind(C)
       import c_ptr, c_size_t, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
     end subroutine 
       
     subroutine c_co_max_int64(c_loc_a, Nelem, c_loc_stat, result_image) bind(C)
       import c_ptr, c_size_t, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
     end subroutine 
       
     subroutine c_co_max_float(c_loc_a, Nelem, c_loc_stat, result_image) bind(C)
       !! void c_co_reduce_float(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation);
       import c_ptr, c_size_t, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
     end subroutine 
       
     subroutine c_co_max_double(c_loc_a, Nelem, c_loc_stat, result_image) bind(C)
       import c_ptr, c_size_t, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
     end subroutine

     subroutine c_co_reduce_char(c_loc_a, Nelem, c_loc_stat, result_image, Coll_ReduceSub_c_char, client_data) bind(C)
       !! void c_co_reduce_char(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation, void* client_data);
       import c_ptr, c_size_t, c_funptr, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat, Coll_ReduceSub_c_char, client_data
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
     end subroutine

     subroutine c_co_reduce_int32(c_loc_a, Nelem, c_loc_stat, result_image, Coll_ReduceSub_c_int32_t) bind(C)
       !! void c_co_reduce_int32(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation);
       import c_ptr, c_size_t, c_funptr, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
       type(c_funptr), value :: Coll_ReduceSub_c_int32_t
     end subroutine

     subroutine c_co_reduce_bool(c_loc_a, Nelem, c_loc_stat, result_image, Coll_ReduceSub_c_bool) bind(C)
       !! void c_co_reduce_bool(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation);
       import c_ptr, c_size_t, c_funptr, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
       type(c_funptr), value :: Coll_ReduceSub_c_bool
     end subroutine

     subroutine c_co_reduce_float(c_loc_a, Nelem, c_loc_stat, result_image, Coll_ReduceSub_c_float) bind(C)
       !! void c_co_reduce_float(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation);
       import c_ptr, c_size_t, c_funptr, c_int
       implicit none
       type(c_ptr), value :: c_loc_a, c_loc_stat
       integer(c_size_t), value :: Nelem
       integer(c_int), value :: result_image
       type(c_funptr), value :: Coll_ReduceSub_c_float
     end subroutine

  end interface

end module caffeine_h_m
    
    
