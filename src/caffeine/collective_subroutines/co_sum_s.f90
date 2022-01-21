! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(collective_subroutines_m) co_sum_s
  use iso_c_binding, only : c_int32_t, c_int64_t, c_ptr, c_size_t, c_loc, c_float, c_double
    
  implicit none

contains 

  module procedure caf_co_sum

   !! void c_co_sum_no_result_image_int32(void* c_loc_a, int Nelem)
   interface

     subroutine c_co_sum_no_result_image_int32(c_loc_a, Nelem) bind(C)
       import c_ptr, c_size_t
       type(c_ptr), value :: c_loc_a
       integer(c_size_t), value :: Nelem
     end subroutine

     subroutine c_co_sum_no_result_image_int64(c_loc_a, Nelem) bind(C)
       import c_ptr, c_size_t
       type(c_ptr), value :: c_loc_a
       integer(c_size_t), value :: Nelem
     end subroutine

     subroutine c_co_sum_no_result_image_float(c_loc_a, Nelem) bind(C)
       import c_ptr, c_size_t
       type(c_ptr), value :: c_loc_a
       integer(c_size_t), value :: Nelem
     end subroutine

     subroutine c_co_sum_no_result_image_double(c_loc_a, Nelem) bind(C)
       import c_ptr, c_size_t
       type(c_ptr), value :: c_loc_a
       integer(c_size_t), value :: Nelem
     end subroutine

   end interface

   select rank(a)
     rank(0) 
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_no_result_image_int32(c_loc(a), nelem=1_c_size_t)
         type is(integer(c_int64_t))
           call c_co_sum_no_result_image_int64(c_loc(a), nelem=1_c_size_t)
         type is(real(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=1_c_size_t)
         type is(real(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=1_c_size_t)
         type is(complex(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=2_c_size_t)
         type is(complex(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=2_c_size_t)
         class default
           error stop "caf_co_sum: unsupported type"
       end select
     rank(1) 
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_no_result_image_int32(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(integer(c_int64_t))
           call c_co_sum_no_result_image_int64(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         class default
           error stop "caf_co_sum: unsupported type"
       end select
     rank(2) 
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_no_result_image_int32(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(integer(c_int64_t))
           call c_co_sum_no_result_image_int64(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         class default
           error stop "caf_co_sum: unsupported type"
       end select
     rank(3) 
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_no_result_image_int32(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(integer(c_int64_t))
           call c_co_sum_no_result_image_int64(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         class default
           error stop "caf_co_sum: unsupported type"
       end select
     rank(4) 
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_no_result_image_int32(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(integer(c_int64_t))
           call c_co_sum_no_result_image_int64(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         class default
           error stop "caf_co_sum: unsupported type"
       end select
     rank(5) 
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_no_result_image_int32(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(integer(c_int64_t))
           call c_co_sum_no_result_image_int64(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         class default
           error stop "caf_co_sum: unsupported type"
       end select
     rank(6) 
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_no_result_image_int32(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(integer(c_int64_t))
           call c_co_sum_no_result_image_int64(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         class default
           error stop "caf_co_sum: unsupported type"
       end select
     rank(7) 
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_no_result_image_int32(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(integer(c_int64_t))
           call c_co_sum_no_result_image_int64(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         class default
           error stop "caf_co_sum: unsupported type"
       end select
     rank(8) 
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_no_result_image_int32(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(integer(c_int64_t))
           call c_co_sum_no_result_image_int64(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         class default
           error stop "caf_co_sum: unsupported type"
       end select
     rank(9) 
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_no_result_image_int32(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(integer(c_int64_t))
           call c_co_sum_no_result_image_int64(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         class default
           error stop "caf_co_sum: unsupported type"
       end select
     rank(10) 
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_no_result_image_int32(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(integer(c_int64_t))
           call c_co_sum_no_result_image_int64(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         class default
           error stop "caf_co_sum: unsupported type"
       end select
     rank(11) 
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_no_result_image_int32(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(integer(c_int64_t))
           call c_co_sum_no_result_image_int64(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         class default
           error stop "caf_co_sum: unsupported type"
       end select
     rank(12) 
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_no_result_image_int32(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(integer(c_int64_t))
           call c_co_sum_no_result_image_int64(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         class default
           error stop "caf_co_sum: unsupported type"
       end select
     rank(13) 
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_no_result_image_int32(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(integer(c_int64_t))
           call c_co_sum_no_result_image_int64(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         class default
           error stop "caf_co_sum: unsupported type"
       end select
     rank(14) 
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_no_result_image_int32(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(integer(c_int64_t))
           call c_co_sum_no_result_image_int64(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         class default
           error stop "caf_co_sum: unsupported type"
       end select
     rank(15) 
       select type(a)
         type is(integer(c_int32_t))
           call c_co_sum_no_result_image_int32(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(integer(c_int64_t))
           call c_co_sum_no_result_image_int64(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(real(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_float))
           call c_co_sum_no_result_image_float(c_loc(a), nelem=size(a,kind=c_size_t))
         type is(complex(c_double))
           call c_co_sum_no_result_image_double(c_loc(a), nelem=size(a,kind=c_size_t))
         class default
           error stop "caf_co_sum: unsupported type"
       end select
   end select

  end procedure

end submodule co_sum_s
