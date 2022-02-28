! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(collective_subroutines_m) co_reduce_s
  use iso_c_binding, only : &
    c_ptr, c_size_t, c_loc, c_null_ptr, c_funloc, c_associated, c_f_pointer, c_int, c_f_procpointer
  use assert_m, only : assert
  use intrinsic_array_m, only : intrinsic_array_t
  use utilities_m, only : get_c_ptr, get_c_ptr_character, optional_value
  use caffeine_h_m, only : caf_c_co_reduce, caf_c_same_cfi_type, caf_c_elem_len, caf_c_is_f_string
  implicit none

  character(kind=c_char,len=5), parameter :: dummy = "     "

contains
 
  module procedure caf_co_reduce

    type(c_ptr) :: stat_ptr = c_null_ptr, errmsg_ptr = c_null_ptr

    procedure(c_int32_t_operation), pointer :: int32_op => null()
    procedure(c_int64_t_operation), pointer :: int64_op => null()
    procedure(c_float_operation), pointer :: float_op => null()
    procedure(c_double_operation), pointer :: double_op => null()
    procedure(c_bool_operation), pointer :: bool_op => null()
    procedure(c_char_operation), pointer :: char_op => null()
    procedure(c_float_complex_operation), pointer :: float_complex_op => null()
    procedure(c_double_complex_operation), pointer :: double_complex_op => null()

    call assert(c_associated(operation), "caf_co_reduce: c_associated(operation)")

    stat_ptr = get_c_ptr(stat)
    errmsg_ptr = get_c_ptr_character(errmsg)

    if (caf_c_same_cfi_type(a, 0)) then
      call c_f_procpointer(operation, int32_op)
      call caf_c_co_reduce(a, optional_value(result_image), stat_ptr, errmsg_ptr, &
        int(product(shape(a)), c_size_t), c_funloc(Coll_ReduceSub_c_int32_t), c_null_ptr)
    else if (caf_c_same_cfi_type(a, 0_c_int64_t)) then
      call c_f_procpointer(operation, int64_op)
      call caf_c_co_reduce(a, optional_value(result_image), stat_ptr, errmsg_ptr, &
        int(product(shape(a)), c_size_t), c_funloc(Coll_ReduceSub_c_int64_t), c_null_ptr)
    else if (caf_c_same_cfi_type(a, 1._c_double)) then
      call c_f_procpointer(operation, double_op)
      call caf_c_co_reduce(a, optional_value(result_image), stat_ptr, errmsg_ptr, &
        int(product(shape(a)), c_size_t), c_funloc(Coll_ReduceSub_c_double), c_null_ptr)
    else if (caf_c_same_cfi_type(a, 1._c_float)) then
      call c_f_procpointer(operation, float_op)
      call caf_c_co_reduce(a, optional_value(result_image), stat_ptr, errmsg_ptr, &
        int(product(shape(a)), c_size_t), c_funloc(Coll_ReduceSub_c_float), c_null_ptr)
    else if (caf_c_same_cfi_type(a, .true._c_bool)) then
      call c_f_procpointer(operation, bool_op)
      call caf_c_co_reduce(a, optional_value(result_image), stat_ptr, errmsg_ptr, &
        int(product(shape(a)), c_size_t), c_funloc(Coll_ReduceSub_c_bool), c_null_ptr)
    else if (caf_c_is_f_string(a)) then
      block
        integer(c_size_t), target :: len_a
        len_a = caf_c_elem_len(a)
        call c_f_procpointer(operation, char_op)
        call caf_c_co_reduce(a, optional_value(result_image), stat_ptr, errmsg_ptr, &
          int(product(shape(a)), c_size_t), c_funloc(Coll_ReduceSub_c_char), c_loc(len_a))
      end block
    else if (caf_c_same_cfi_type(a, (0._c_float, 0._c_float))) then
      call c_f_procpointer(operation, float_complex_op)
      call caf_c_co_reduce(a, optional_value(result_image), stat_ptr, errmsg_ptr, &
        int(product(shape(a)), c_size_t), c_funloc(Coll_ReduceSub_c_float_complex), c_null_ptr)
    else if (caf_c_same_cfi_type(a, (0._c_double, 0._c_double))) then
      call c_f_procpointer(operation, double_complex_op)
      call caf_c_co_reduce(a, optional_value(result_image), stat_ptr, errmsg_ptr, &
        int(product(shape(a)), c_size_t), c_funloc(Coll_ReduceSub_c_double_complex), c_null_ptr)
    else
      error stop "caf_co_reduce: unsupported type"
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
        call assert(all(c_associated_args), "Coll_ReduceSub_c_char: all(c_associated_args)", intrinsic_array_t(c_associated_args))
      end associate

      call c_f_pointer(cdata, arglen)
      allocate(character(kind=c_char, len=arglen) :: prototype(count))
      lhs => prototype ! set string length
      rhs_and_result => prototype ! set string length

      call c_f_pointer(arg1, lhs, [count])
      call c_f_pointer(arg2_and_out, rhs_and_result, [count])

      block 
        integer(c_size_t) i

        do concurrent(i=1:count)
          rhs_and_result(i) = char_op(lhs(i), rhs_and_result(i))
        end do
      end block

    end subroutine

  end procedure

end submodule co_reduce_s
