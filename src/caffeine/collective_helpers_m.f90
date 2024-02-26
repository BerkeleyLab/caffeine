! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module collective_helpers_m
  use iso_c_binding, only : c_int, c_int32_t, c_int64_t, c_float, c_char, c_bool, c_double
  implicit none

  private

  public :: c_int32_t_operation
  public :: c_int64_t_operation
  public :: c_float_operation
  public :: c_double_operation
  public :: c_bool_operation
  public :: c_char_operation
  public :: c_float_complex_operation
  public :: c_double_complex_operation

  abstract interface 

    pure function c_int32_t_operation(lhs, rhs) result(lhs_op_rhs)
      import c_int32_t
      implicit none
      integer(c_int32_t), intent(in) :: lhs, rhs
      integer(c_int32_t) lhs_op_rhs
    end function

    pure function c_int64_t_operation(lhs, rhs) result(lhs_op_rhs)
      import c_int64_t
      implicit none
      integer(c_int64_t), intent(in) :: lhs, rhs
      integer(c_int64_t) lhs_op_rhs
    end function

    pure function c_float_operation(lhs, rhs) result(lhs_op_rhs)
      import c_float
      implicit none
      real(c_float), intent(in) :: lhs, rhs
      real(c_float) lhs_op_rhs
    end function

    pure function c_double_operation(lhs, rhs) result(lhs_op_rhs)
      import c_double
      implicit none
      real(c_double), intent(in) :: lhs, rhs
      real(c_double) lhs_op_rhs
    end function

    pure function c_bool_operation(lhs, rhs) result(lhs_op_rhs)
      import c_bool
      implicit none
      logical(c_bool), intent(in) :: lhs, rhs
      logical(c_bool) lhs_op_rhs
    end function

    function c_char_operation(lhs, rhs) result(lhs_op_rhs)
      import c_char
      implicit none
      character(kind=c_char,len=*), intent(in) :: lhs, rhs
      character(kind=c_char,len=:), allocatable :: lhs_op_rhs
    end function

    pure function c_float_complex_operation(lhs, rhs) result(lhs_op_rhs)
      import c_float
      implicit none
      complex(c_float), intent(in) :: lhs, rhs
      complex(c_float) lhs_op_rhs
    end function

    pure function c_double_complex_operation(lhs, rhs) result(lhs_op_rhs)
      import c_double
      implicit none
      complex(c_double), intent(in) :: lhs, rhs
      complex(c_double) lhs_op_rhs
    end function

  end interface

end module
