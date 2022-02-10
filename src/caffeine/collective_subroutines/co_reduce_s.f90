! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(collective_subroutines_m) co_reduce_s
  use iso_c_binding, only : &
    c_int64_t, c_ptr, c_size_t, c_loc, c_double, c_null_ptr, c_funptr, c_funloc, c_associated, c_f_pointer, c_int
  use assert_m, only : assert
  use intrinsic_array_m, only : intrinsic_array_t
  use utilities_m, only : get_c_ptr, optional_value
  use caffeine_h_m, only : caf_c_co_reduce_char, caf_c_co_reduce_int32, caf_c_co_reduce_bool, caf_c_co_reduce_float
  implicit none

contains
 
  module procedure caf_co_reduce_c_char

    type(c_ptr) stat_ptr
    procedure(c_char_operation), pointer :: c_char_op_ptr

    call assert(associated(operation), "caf_co_reduce_c_char: operation associated")

    c_char_op_ptr => operation

    stat_ptr = get_c_ptr(stat)

    select rank(a)
      rank(0)
        block
          integer(c_size_t), target :: len_a
          len_a = int(len(a), c_size_t)
          call caf_c_co_reduce_char( &
            c_loc(a), len_a, stat_ptr, optional_value(result_image), c_funloc(Coll_ReduceSub_c_char), c_loc(len_a))
        end block
      rank default
         error stop "caf_co_reduce_c_char: unsupported rank"
    end select

  contains

    subroutine Coll_ReduceSub_c_char(arg1, arg2_and_out, count, cdata) bind(C)
      type(c_ptr), value :: arg1         !! "Left" operands
      type(c_ptr), value :: arg2_and_out !! "Right" operands and result
      integer(c_size_t), value :: count  !! Operand count
      type(c_ptr), value ::  cdata       !! Client data

      character(kind=c_char, len=:), pointer :: lhs(:), rhs_and_result(:)
      character(kind=c_char, len=:), allocatable, target :: prototype(:)
      integer(c_int), pointer :: arglen
    
      associate(c_associated_args => [c_associated(arg1), c_associated(arg2_and_out), c_associated(cdata)])
        call assert(all(c_associated_args), "Coll_ReduceSub_c_char: all(c_associated_args)", intrinsic_array_t(c_associated_args))
      end associate

      call c_f_pointer(cdata, arglen)
      allocate(character(kind=c_char, len=arglen) :: prototype(count))
      lhs => prototype ! set string length
      rhs_and_result => prototype ! set string length

      call c_f_pointer(arg1, lhs, [count])
      call c_f_pointer(arg2_and_out, rhs_and_result, [count])

      call assert(all([associated(lhs), associated(rhs_and_result)]), "Coll_ReduceSub_c_char: associated lhs & rhs_and_result")
    
      block 
        integer i

        do i=1, count
          rhs_and_result(i) = c_char_op_ptr(lhs(i), rhs_and_result(i))
        end do
      end block

    end subroutine

  end procedure

  module procedure caf_co_reduce_c_int32_t

    type(c_ptr) stat_ptr
    procedure(c_int32_t_operation), pointer :: c_int32_t_op_ptr

    call assert(associated(operation), "caf_co_reduce_c_int32_t: operation associated")

    c_int32_t_op_ptr => operation

    stat_ptr = get_c_ptr(stat)

    select rank(a)
      rank(0)
        call caf_c_co_reduce_int32(c_loc(a), 1_c_size_t, stat_ptr, optional_value(result_image), c_funloc(Coll_ReduceSub_c_int32_t))
      rank default
        error stop "caf_co_reduce_c_int32_t: unsupported rank"
    end select

  contains

    subroutine Coll_ReduceSub_c_int32_t(arg1, arg2_and_out, count, cdata) bind(C)
      type(c_ptr), value :: arg1         !! "Left" operands
      type(c_ptr), value :: arg2_and_out !! "Right" operands and result
      integer(c_size_t), value :: count  !! Operand count
      type(c_ptr), value ::  cdata       !! Client data
      integer(c_int32_t), pointer :: lhs(:), rhs_and_result(:)
      integer i

      call assert(all([c_associated(arg1), c_associated(arg2_and_out)]), "Coll_ReduceSub_c_int32t: operands associated")
      call c_f_pointer(arg1, lhs, [count])
      call c_f_pointer(arg2_and_out, rhs_and_result, [count])
      call assert(all([associated(lhs), associated(rhs_and_result)]), "Coll_ReduceSub_c_int32t: operands associated")
      do i=1, count
        rhs_and_result(i) = c_int32_t_op_ptr(lhs(i), rhs_and_result(i))
      end do
    end subroutine

  end procedure

  module procedure caf_co_reduce_c_float

    type(c_ptr) stat_ptr
    procedure(c_float_operation), pointer :: c_float_op_ptr

    call assert(associated(operation), "caf_co_reduce_c_float: operation associated")

    c_float_op_ptr => operation

    stat_ptr = get_c_ptr(stat)

    select rank(a)
      rank(0)
         call caf_c_co_reduce_float(c_loc(a), 1_c_size_t, stat_ptr, optional_value(result_image), c_funloc(Coll_ReduceSub_c_float))
      rank default
         error stop "caf_co_reduce_c_float: unsupported rank"
    end select

  contains 

    subroutine Coll_ReduceSub_c_float(arg1, arg2_and_out, count, cdata) bind(C)
      type(c_ptr), value :: arg1         !! "Left" operands
      type(c_ptr), value :: arg2_and_out !! "Right" operands and result
      integer(c_size_t), value :: count  !! Operand count
      type(c_ptr), value ::  cdata       !! Client data
      real(c_float), pointer :: lhs, rhs_and_result
      
      call assert(all([c_associated(arg1), c_associated(arg2_and_out)]), "Coll_ReduceSub_c_float: operands associated")
      call c_f_pointer(arg1, lhs)
      call c_f_pointer(arg2_and_out, rhs_and_result)
      call assert(all([associated(lhs), associated(rhs_and_result)]), "Coll_ReduceSub_c_float: operands associated")
      rhs_and_result = c_float_op_ptr(lhs, rhs_and_result)
    end subroutine

  end procedure

  module procedure caf_co_reduce_c_bool

    type(c_ptr) stat_ptr
    procedure(c_bool_operation), pointer :: c_bool_op_ptr

    call assert(associated(operation), "caf_co_reduce_c_bool: operation associated")

    c_bool_op_ptr => operation

    stat_ptr = get_c_ptr(stat)

    select rank(a)
      rank(0)
         call caf_c_co_reduce_bool(c_loc(a), 1_c_size_t, stat_ptr, optional_value(result_image), c_funloc(Coll_ReduceSub_c_bool))
      rank default
         error stop "caf_co_reduce_c_bool: unsupported rank"
    end select

  contains 

    subroutine Coll_ReduceSub_c_bool(arg1, arg2_and_out, count, cdata) bind(C)
      type(c_ptr), value :: arg1         !! "Left" operands
      type(c_ptr), value :: arg2_and_out !! "Right" operands and result
      integer(c_size_t), value :: count  !! Operand count
      type(c_ptr), value ::  cdata       !! Client data
      logical(c_bool), pointer :: lhs, rhs_and_result
      
      call assert(all([c_associated(arg1), c_associated(arg2_and_out)]), "Coll_ReduceSub_c_bool: operands associated")
      call c_f_pointer(arg1, lhs)
      call c_f_pointer(arg2_and_out, rhs_and_result)
      call assert(all([associated(lhs), associated(rhs_and_result)]), "Coll_ReduceSub_c_bool: operands associated")
      rhs_and_result = c_bool_op_ptr(lhs, rhs_and_result)
    end subroutine

  end procedure

end submodule co_reduce_s
