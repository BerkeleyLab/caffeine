! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "assert_macros.h"
#include "caffeine-internal.h"

submodule(prif:prif_private_s) atomic_s
  ! DO NOT ADD USE STATEMENTS HERE
  ! All use statements belong in prif_private_s.F90
  implicit none

  ! placeholder variables that simplify the macro logic below
  integer(PRIF_ATOMIC_INT_KIND) :: dummyti
  logical(PRIF_ATOMIC_LOGICAL_KIND) :: dummytl
  integer(PRIF_ATOMIC_INT_KIND), parameter :: dummyvi = 0 
  logical(PRIF_ATOMIC_LOGICAL_KIND), parameter :: dummyvl = .false. 
contains

#define ATOMIC_OP(OPNAME, OPCODE, caf_op) \
  module procedure CAF_CONCAT2(prif_atomic_,OPNAME) ; \
    integer(c_intptr_t) :: remote_base; \
    call_assert(offset >= 0); \
    call base_pointer(coarray_handle, image_num, remote_base); \
    call CAF_CONCAT3(prif_atomic_,OPNAME,_indirect) \
      ( image_num, remote_base + offset, OPPASSI, stat ); \
  end procedure ; \
  module procedure CAF_CONCAT3(prif_atomic_,OPNAME,_indirect) ; \
    call_assert(c_sizeof(dummyti) == 8); call_assert(c_sizeof(dummytl) == 8); \
    call_assert_describe(image_num > 0 .and. image_num <= initial_team%num_images, "image_num not within valid range"); \
    call caf_op(CAF_CONCAT2(CAF_OP_,OPCODE), image_num, atom_remote_ptr, OPPASSC); \
    if (present(stat)) stat = 0; \
  end procedure

! Extra arg below is another workaround gfortran's sub-standard preprocessor
#define ATOMIC_INT_OP(OPNAME,_,OPCODE) ATOMIC_OP(OPNAME,OPCODE,caf_atomic_int)
#define ATOMIC_LOG_OP(OPNAME,_,OPCODE) ATOMIC_OP(OPNAME,OPCODE,caf_atomic_logical)

#undef  OPPASSI
#define OPPASSI value
#undef  OPPASSC
#define OPPASSC value, dummyvi, dummyvi
  ATOMIC_INT_OP(ref_int,        ,GET)
#undef  OPPASSC
#define OPPASSC value, dummyvl, dummyvl
  ATOMIC_LOG_OP(ref_logical,    ,GET)
#undef  OPPASSC
#define OPPASSC dummytl, value, dummyvl
  ATOMIC_LOG_OP(define_logical, ,SET)
#undef  OPPASSC
#define OPPASSC dummyti, value, dummyvi
  ATOMIC_INT_OP(define_int,     ,SET)

  ATOMIC_INT_OP(add,            ,ADD)
  ATOMIC_INT_OP(and,            ,AND)
  ATOMIC_INT_OP(or,             ,OR)
  ATOMIC_INT_OP(xor,            ,XOR)

#undef  OPPASSI
#define OPPASSI value, old
#undef  OPPASSC
#define OPPASSC old, value, dummyvi
  ATOMIC_INT_OP(fetch_add,      ,FADD)
  ATOMIC_INT_OP(fetch_and,      ,FAND)
  ATOMIC_INT_OP(fetch_or,       ,FOR)
  ATOMIC_INT_OP(fetch_xor,      ,FXOR)

#undef  OPPASSI
#define OPPASSI old, compare, new
#undef  OPPASSC
#define OPPASSC old, compare, new
  ATOMIC_INT_OP(cas_int,        ,FCAS)
  ATOMIC_LOG_OP(cas_logical,    ,FCAS)

end submodule atomic_s
