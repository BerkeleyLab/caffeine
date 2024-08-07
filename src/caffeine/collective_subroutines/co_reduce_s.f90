! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) co_reduce_s
  use iso_c_binding, only : &
    c_loc, c_null_ptr, c_funloc, c_associated, c_f_pointer, c_f_procpointer, c_char, c_int64_t, c_double, &
    c_float, c_int32_t

  implicit none

  character(kind=c_char,len=5), parameter :: dummy = "     "

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

contains

  module procedure prif_co_reduce

    procedure(c_int32_t_operation), pointer :: int32_op => null()
    procedure(c_int64_t_operation), pointer :: int64_op => null()
    procedure(c_float_operation), pointer :: float_op => null()
    procedure(c_double_operation), pointer :: double_op => null()
    procedure(c_bool_operation), pointer :: bool_op => null()
    procedure(c_char_operation), pointer :: char_op => null()
    procedure(c_float_complex_operation), pointer :: float_complex_op => null()
    procedure(c_double_complex_operation), pointer :: double_complex_op => null()

    if (present(stat)) stat=0
    call assert(c_associated(operation), "caf_co_reduce: c_associated(operation)")

    if (caf_same_cfi_type(a, 0)) then
      call c_f_procpointer(operation, int32_op)
      call caf_co_reduce(a, optional_value(result_image), &
        int(product(shape(a)), c_size_t), c_funloc(Coll_ReduceSub_c_int32_t), c_null_ptr, current_team%info%gex_team)
    else if (caf_same_cfi_type(a, 0_c_int64_t)) then
      call c_f_procpointer(operation, int64_op)
      call caf_co_reduce(a, optional_value(result_image), &
        int(product(shape(a)), c_size_t), c_funloc(Coll_ReduceSub_c_int64_t), c_null_ptr, current_team%info%gex_team)
    else if (caf_same_cfi_type(a, 1._c_double)) then
      call c_f_procpointer(operation, double_op)
      call caf_co_reduce(a, optional_value(result_image), &
        int(product(shape(a)), c_size_t), c_funloc(Coll_ReduceSub_c_double), c_null_ptr, current_team%info%gex_team)
    else if (caf_same_cfi_type(a, 1._c_float)) then
      call c_f_procpointer(operation, float_op)
      call caf_co_reduce(a, optional_value(result_image), &
        int(product(shape(a)), c_size_t), c_funloc(Coll_ReduceSub_c_float), c_null_ptr, current_team%info%gex_team)
    else if (caf_same_cfi_type(a, .true._c_bool)) then
      call c_f_procpointer(operation, bool_op)
      call caf_co_reduce(a, optional_value(result_image), &
        int(product(shape(a)), c_size_t), c_funloc(Coll_ReduceSub_c_bool), c_null_ptr, current_team%info%gex_team)
    else if (caf_is_f_string(a)) then
      block
        integer(c_size_t), target :: len_a
        len_a = caf_elem_len(a)
        call c_f_procpointer(operation, char_op)
        call caf_co_reduce(a, optional_value(result_image), &
          int(product(shape(a)), c_size_t), c_funloc(Coll_ReduceSub_c_char), c_loc(len_a), current_team%info%gex_team)
      end block
    else if (caf_same_cfi_type(a, (0._c_float, 0._c_float))) then
      call c_f_procpointer(operation, float_complex_op)
      call caf_co_reduce(a, optional_value(result_image), &
        int(product(shape(a)), c_size_t), c_funloc(Coll_ReduceSub_c_float_complex), c_null_ptr, current_team%info%gex_team)
    else if (caf_same_cfi_type(a, (0._c_double, 0._c_double))) then
      call c_f_procpointer(operation, double_complex_op)
      call caf_co_reduce(a, optional_value(result_image), &
        int(product(shape(a)), c_size_t), c_funloc(Coll_ReduceSub_c_double_complex), c_null_ptr, current_team%info%gex_team)
    else
      call prif_error_stop(.false._c_bool, stop_code_char="caf_co_reduce: unsupported type")
    end if

  contains

    subroutine Coll_ReduceSub_c_int32_t(arg1, arg2_and_out, count, cdata) bind(C)
      type(c_ptr), value :: arg1         !! "Left" operands
      type(c_ptr), value :: arg2_and_out !! "Right" operands and result
      integer(c_size_t), value :: count  !! Operand count
      type(c_ptr), value ::  cdata       !! Client data
      integer(c_int32_t), pointer :: lhs(:)=>null(), rhs_and_result(:)=>null()
      integer(c_size_t) i

      call assert(all([c_associated(arg1), c_associated(arg2_and_out)]), "Coll_ReduceSub_c_int32_t: operands associated")

      call c_f_pointer(arg1, lhs, [count])
      call c_f_pointer(arg2_and_out, rhs_and_result, [count])

      do concurrent(i=1:count)
        rhs_and_result(i) = int32_op(lhs(i), rhs_and_result(i))
      end do
    end subroutine

    subroutine Coll_ReduceSub_c_int64_t(arg1, arg2_and_out, count, cdata) bind(C)
      type(c_ptr), value :: arg1         !! "Left" operands
      type(c_ptr), value :: arg2_and_out !! "Right" operands and result
      integer(c_size_t), value :: count  !! Operand count
      type(c_ptr), value ::  cdata       !! Client data
      integer(c_int64_t), pointer :: lhs(:)=>null(), rhs_and_result(:)=>null()
      integer(c_size_t) i

      call assert(all([c_associated(arg1), c_associated(arg2_and_out)]), "Coll_ReduceSub_c_int64_t: operands associated")

      call c_f_pointer(arg1, lhs, [count])
      call c_f_pointer(arg2_and_out, rhs_and_result, [count])

      do concurrent(i=1:count)
        rhs_and_result(i) = int64_op(lhs(i), rhs_and_result(i))
      end do
    end subroutine

    subroutine Coll_ReduceSub_c_double(arg1, arg2_and_out, count, cdata) bind(C)
      type(c_ptr), value :: arg1         !! "Left" operands
      type(c_ptr), value :: arg2_and_out !! "Right" operands and result
      integer(c_size_t), value :: count  !! Operand count
      type(c_ptr), value ::  cdata       !! Client data
      real(c_double), pointer :: lhs(:)=>null(), rhs_and_result(:)=>null()
      integer(c_size_t) i

      call assert(all([c_associated(arg1), c_associated(arg2_and_out)]), "Coll_ReduceSub_c_double: operands associated")

      call c_f_pointer(arg1, lhs, [count])
      call c_f_pointer(arg2_and_out, rhs_and_result, [count])

      do concurrent(i=1:count)
        rhs_and_result(i) = double_op(lhs(i), rhs_and_result(i))
      end do
    end subroutine

    subroutine Coll_ReduceSub_c_float(arg1, arg2_and_out, count, cdata) bind(C)
      type(c_ptr), value :: arg1         !! "Left" operands
      type(c_ptr), value :: arg2_and_out !! "Right" operands and result
      integer(c_size_t), value :: count  !! Operand count
      type(c_ptr), value ::  cdata       !! Client data
      real(c_float), pointer :: lhs(:)=>null(), rhs_and_result(:)=>null()
      integer(c_size_t) i

      call assert(all([c_associated(arg1), c_associated(arg2_and_out)]), "Coll_ReduceSub_c_float: operands associated")

      call c_f_pointer(arg1, lhs, [count])
      call c_f_pointer(arg2_and_out, rhs_and_result, [count])

      do concurrent(i=1:count)
        rhs_and_result(i) = float_op(lhs(i), rhs_and_result(i))
      end do
    end subroutine

    subroutine Coll_ReduceSub_c_double_complex(arg1, arg2_and_out, count, cdata) bind(C)
      type(c_ptr), value :: arg1         !! "Left" operands
      type(c_ptr), value :: arg2_and_out !! "Right" operands and result
      integer(c_size_t), value :: count  !! Operand count
      type(c_ptr), value ::  cdata       !! Client data
      complex(c_double), pointer :: lhs(:)=>null(), rhs_and_result(:)=>null()
      integer(c_size_t) i

      call assert(all([c_associated(arg1), c_associated(arg2_and_out)]), "Coll_ReduceSub_c_dobule_complex: operands associated")

      call c_f_pointer(arg1, lhs, [count])
      call c_f_pointer(arg2_and_out, rhs_and_result, [count])

      do concurrent(i=1:count)
        rhs_and_result(i) = double_complex_op(lhs(i), rhs_and_result(i))
      end do
    end subroutine

    subroutine Coll_ReduceSub_c_float_complex(arg1, arg2_and_out, count, cdata) bind(C)
      type(c_ptr), value :: arg1         !! "Left" operands
      type(c_ptr), value :: arg2_and_out !! "Right" operands and result
      integer(c_size_t), value :: count  !! Operand count
      type(c_ptr), value ::  cdata       !! Client data
      complex(c_float), pointer :: lhs(:)=>null(), rhs_and_result(:)=>null()
      integer(c_size_t) i

      call assert(all([c_associated(arg1), c_associated(arg2_and_out)]), "Coll_ReduceSub_c_float_complex: operands associated")

      call c_f_pointer(arg1, lhs, [count])
      call c_f_pointer(arg2_and_out, rhs_and_result, [count])

      do concurrent(i=1:count)
        rhs_and_result(i) = float_complex_op(lhs(i), rhs_and_result(i))
      end do
    end subroutine

    subroutine Coll_ReduceSub_c_bool(arg1, arg2_and_out, count, cdata) bind(C)
      type(c_ptr), value :: arg1         !! "Left" operands
      type(c_ptr), value :: arg2_and_out !! "Right" operands and result
      integer(c_size_t), value :: count  !! Operand count
      type(c_ptr), value ::  cdata       !! Client data
      logical(c_bool), pointer :: lhs(:)=>null(), rhs_and_result(:)=>null()
      integer(c_size_t) i

      call assert(all([c_associated(arg1), c_associated(arg2_and_out)]), "Coll_ReduceSub_c_bool: operands associated")

      call c_f_pointer(arg1, lhs, [count])
      call c_f_pointer(arg2_and_out, rhs_and_result, [count])

      do concurrent(i=1:count)
        rhs_and_result(i) = bool_op(lhs(i), rhs_and_result(i))
      end do
    end subroutine

    subroutine Coll_ReduceSub_c_char(arg1, arg2_and_out, count, cdata) bind(C)
      type(c_ptr), value :: arg1         !! "Left" operands
      type(c_ptr), value :: arg2_and_out !! "Right" operands and result
      integer(c_size_t), value :: count  !! Operand count
      type(c_ptr), value ::  cdata       !! Client data

      character(kind=c_char, len=:), allocatable, target :: prototype(:)
      character(kind=c_char, len=:), pointer :: lhs(:)=>null(), rhs_and_result(:)=>null()
      integer(c_int), pointer :: arglen=>null()

      associate(c_associated_args => [c_associated(arg1), c_associated(arg2_and_out), c_associated(cdata)])
        call assert(all(c_associated_args), "Coll_ReduceSub_c_char: all(c_associated_args)")
      end associate

      call c_f_pointer(cdata, arglen)
      allocate(character(kind=c_char, len=arglen) :: prototype(count))
      lhs => prototype ! set string length
      rhs_and_result => prototype ! set string length

      call c_f_pointer(arg1, lhs, [count])
      call c_f_pointer(arg2_and_out, rhs_and_result, [count])

      block
        integer(c_size_t) i

        do i=1, count
          rhs_and_result(i) = char_op(lhs(i), rhs_and_result(i))
        end do
      end block

    end subroutine

  end procedure

end submodule co_reduce_s
