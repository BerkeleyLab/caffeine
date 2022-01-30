! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module collective_subroutines_m 
  use iso_c_binding, only : c_int32_t, c_float, c_char
  implicit none

  private
  public :: caf_co_sum
  public :: caf_co_max
  public :: caf_co_min
  public :: caf_co_reduce
  public :: caf_co_broadcast

  public :: c_int32_t_operation
  public :: c_float_operation
  public :: c_char_operation

  interface
 
     module subroutine caf_co_sum(a, result_image, stat, errmsg)
       implicit none
       class(*), intent(inout), contiguous, target :: a(..)
       integer, intent(in), target, optional :: result_image
       integer, intent(out), target, optional :: stat
       character(len=*), intent(inout), target, optional :: errmsg
     end subroutine

     module subroutine caf_co_max(a, result_image, stat, errmsg)
       implicit none
       class(*), intent(inout), contiguous, target :: a(..)
       integer, intent(in), optional, target :: result_image
       integer, intent(out), optional, target :: stat
       character(len=*), intent(inout), optional, target :: errmsg
     end subroutine

     module subroutine caf_co_min(a, result_image, stat, errmsg)
       implicit none
       class(*), intent(inout), contiguous, target :: a(..)
       integer, intent(in), optional, target :: result_image
       integer, intent(out), optional, target :: stat
       character(len=*), intent(inout), optional, target :: errmsg
     end subroutine


     module subroutine caf_co_broadcast(a, source_image, stat, errmsg)
       implicit none
       class(*), intent(inout), contiguous, target :: a(..)
       integer, optional, intent(in) :: source_image
       integer, optional, intent(out), target :: stat
       character(len=*), intent(inout), optional, target :: errmsg
     end subroutine

  end interface

  abstract interface 

    pure function c_int32_t_operation(lhs, rhs) result(lhs_op_rhs)
      import c_int32_t
      integer(c_int32_t), intent(in) :: lhs, rhs
      integer(c_int32_t) lhs_op_rhs
    end function

    pure function c_float_operation(lhs, rhs) result(lhs_op_rhs)
      import c_float
      real(c_float), intent(in) :: lhs, rhs
      real(c_float) lhs_op_rhs
    end function

    pure function c_char_operation(lhs, rhs) result(lhs_op_rhs)
      import c_char
      character(kind=c_char,len=*), intent(in) :: lhs, rhs
      character(kind=c_char,len=:), allocatable :: lhs_op_rhs
    end function

  end interface

  interface caf_co_reduce

     module subroutine caf_co_reduce_c_int32_t(a, operation, result_image, stat, errmsg)
       implicit none
       integer(c_int32_t), intent(inout), contiguous, target :: a(..)
       procedure(c_int32_t_operation), pointer, intent(in) :: operation
       integer, intent(in), optional, target :: result_image
       integer, intent(out), optional, target :: stat
       character(len=*), intent(inout), optional, target :: errmsg
     end subroutine

     module subroutine caf_co_reduce_c_float(a, operation, result_image, stat, errmsg)
       implicit none
       real(c_float), intent(inout), contiguous, target :: a(..)
       procedure(c_float_operation), pointer, intent(in) :: operation
       integer, intent(in), optional, target :: result_image
       integer, intent(out), optional, target :: stat
       character(len=*), intent(inout), optional, target :: errmsg
     end subroutine

     module subroutine caf_co_reduce_c_char(a, operation, result_image, stat, errmsg)
       implicit none
       character(kind=c_char, len=*), intent(inout), contiguous, target :: a(..)
       procedure(c_char_operation), pointer, intent(in) :: operation
       integer, intent(in), optional, target :: result_image
       integer, intent(out), optional, target :: stat
       character(len=*), intent(inout), optional, target :: errmsg
     end subroutine

   end interface

end module
