// Copyright (c), The Regents of the University of California
// Terms of use are as specified in LICENSE.txt

#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <gasnetex.h>
#include <gasnet_coll.h>
#include <gasnet_vis.h>
#include "gasnet_safe.h"
#include <gasnet_tools.h>
#include <gasnet_portable_platform.h>
#include <ISO_Fortran_binding.h>
#include "../dlmalloc/dl_malloc_caf.h"
#include "../dlmalloc/dl_malloc.h"

enum {
  UNRECOGNIZED_TYPE,
  ERRMSG_TOO_SHORT
};

static gex_Client_t myclient;
static gex_EP_t myep;
static gex_Rank_t rank, size;
static gex_Segment_t mysegment;
static gex_TM_t myworldteam;

typedef void(*final_func_ptr)(void*, size_t) ;
typedef uint8_t byte;

#if PLATFORM_COMPILER_GNU && PLATFORM_COMPILER_VERSION_LT(12,0,0)
  #define float_Complex_workaround  2052
  #define double_Complex_workaround 4100
#else
  #define float_Complex_workaround  CFI_type_float_Complex
  #define double_Complex_workaround CFI_type_double_Complex
#endif

// ---------------------------------------------------
int caf_this_image(gex_TM_t gex_team)
{
  return gex_TM_QueryRank(gex_team) + 1;
}
int caf_num_images(gex_TM_t gex_team)
{
  return gex_TM_QuerySize(gex_team);
}
// ---------------------------------------------------
// NOTE: gex_TM_T is a typedef to a C pointer, so the `gex_TM_t* initial_team` arg in the C signature matches the BIND(C) interface of an `intent(out)` arg of type `c_ptr` for the same argument
void caf_caffeinate(
  mspace* symmetric_heap,
  intptr_t* symmetric_heap_start,
  intptr_t* symmetric_heap_size,
  mspace* non_symmetric_heap,
  gex_TM_t* initial_team
) {
  GASNET_SAFE(gex_Client_Init(&myclient, &myep, initial_team, "caffeine", NULL, NULL, 0));

  // query largest possible segment GASNet can give us of the same size across all processes:
  size_t max_seg = gasnet_getMaxGlobalSegmentSize();
  // impose a reasonable default size
  #ifndef CAF_DEFAULT_HEAP_SIZE
  #define CAF_DEFAULT_HEAP_SIZE (128*1024*1024) // 128 MiB
  #endif
  size_t default_seg = MIN(max_seg, CAF_DEFAULT_HEAP_SIZE);
  // retrieve user preference, defaulting to the above and units of MiB
  size_t segsz = gasnett_getenv_int_withdefault("CAF_HEAP_SIZE",
                                                default_seg, 1024*1024);
  // cap user request to the largest available:
  // TODO: issue a console warning here instead of silently capping
  segsz = MIN(segsz,max_seg);

  GASNET_SAFE(gex_Segment_Attach(&mysegment, *initial_team, segsz));

  *symmetric_heap_start = (intptr_t)gex_Segment_QueryAddr(mysegment);
  size_t total_heap_size = gex_Segment_QuerySize(mysegment);

  #ifndef CAF_DEFAULT_COMP_FRAC
  #define CAF_DEFAULT_COMP_FRAC 0.1f // 10%
  #endif
  float default_comp_frac = MAX(MIN(0.99f, CAF_DEFAULT_COMP_FRAC), 0.01f);
  float non_symmetric_fraction = gasnett_getenv_dbl_withdefault("CAF_COMP_FRAC", default_comp_frac);
  assert(non_symmetric_fraction > 0 && non_symmetric_fraction < 1); // TODO: real error reporting

  size_t non_symmetric_heap_size = total_heap_size * non_symmetric_fraction;
  *symmetric_heap_size = total_heap_size - non_symmetric_heap_size;
  intptr_t non_symmetric_heap_start = *symmetric_heap_start + *symmetric_heap_size;

  if (caf_this_image(*initial_team) == 1) {
    *symmetric_heap = create_mspace_with_base((void*)*symmetric_heap_start, *symmetric_heap_size, 0);
    mspace_set_footprint_limit(*symmetric_heap, *symmetric_heap_size);
  }
  *non_symmetric_heap = create_mspace_with_base((void*)non_symmetric_heap_start, non_symmetric_heap_size, 0);
  mspace_set_footprint_limit(*non_symmetric_heap, non_symmetric_heap_size);
  myworldteam = *initial_team;
}

void caf_decaffeinate(int exit_code)
{
  gasnet_exit(exit_code);
}

void caf_fatal_error( const CFI_cdesc_t* Fstr )
{
  const char *msg = (char *)Fstr->base_addr;
  int len = Fstr->elem_len;
  //printf("%p:%i\n",msg,len); fflush(0);
  gasnett_fatalerror_nopos("%.*s", len, msg);
}

void* caf_allocate(mspace heap, size_t bytes)
{
   void* allocated_space = mspace_memalign(heap, 8, bytes);
   if (!allocated_space) // uh-oh, something went wrong..
     gasnett_fatalerror("caf_allocate failed to mspace_memalign(%"PRIuSZ")", 
                        bytes);
   return allocated_space;
}

void caf_allocate_remaining(mspace heap, void** allocated_space, size_t* allocated_size)
{
  // The following doesn't necessarily give us all remaining space
  // nor necessarily the largest open space, but in practice is likely
  // to work out that way
  struct mallinfo heap_info = mspace_mallinfo(heap);
  *allocated_size = heap_info.keepcost * 0.9f;
  *allocated_space = mspace_memalign(heap, 8, *allocated_size);
  if (!*allocated_space) // uh-oh, something went wrong..
    gasnett_fatalerror("caf_allocate_remaining failed to mspace_memalign(%"PRIuSZ")", 
                       *allocated_size);
}

void caf_deallocate(mspace heap, void* mem)
{
  mspace_free(heap, mem);
}

void caf_establish_mspace(mspace* heap, void* heap_start, size_t heap_size)
{
  *heap = create_mspace_with_base(heap_start, heap_size, 0);
  mspace_set_footprint_limit(*heap, heap_size);
}

// take address in a segment and convert to an address on given image
intptr_t caf_convert_base_addr(void* addr, int image)
{
   ptrdiff_t offset = (byte*)addr - (byte*)gex_Segment_QueryAddr(mysegment);
   void* segment_start_remote_image = NULL;
   gex_Event_Wait(gex_EP_QueryBoundSegmentNB(myworldteam, image - 1, &segment_start_remote_image, NULL, NULL, 0));
   return (intptr_t)((byte*)segment_start_remote_image + offset);
}

// _______________________ Contiguous RMA ____________________________
void caf_put(int image, intptr_t dest, void* src, size_t size)
{
  gex_RMA_PutBlocking(myworldteam, image-1, (void*)dest, src, size, 0);
}

void caf_get(int image, void* dest, intptr_t src, size_t size)
{
  gex_RMA_GetBlocking(myworldteam, dest, image-1, (void*)src, size, 0);
}

// _______________________ Strided RMA ____________________________
void caf_put_strided(int dims, int image_num, 
                     intptr_t remote_ptr, void* remote_stride, 
                     void *current_image_buffer, void * current_image_stride, 
                     size_t element_size, void *extent) {
  gex_VIS_StridedPutBlocking(myworldteam, 
                             image_num-1,
                             (void *)remote_ptr, remote_stride,
                             current_image_buffer, current_image_stride,
                             element_size, extent, dims, 0);
}

void caf_get_strided(int dims, int image_num, 
                     intptr_t remote_ptr, void* remote_stride, 
                     void *current_image_buffer, void * current_image_stride,
                     size_t element_size, void *extent) {
  gex_VIS_StridedGetBlocking(myworldteam, 
                             current_image_buffer, current_image_stride,
                             image_num-1,
                             (void *)remote_ptr, remote_stride,
                             element_size, extent, dims, 0);
}

//-------------------------------------------------------------------

void caf_sync_memory() {
  // we may eventually need more than this if/when we relax our memory model..
  gasnett_local_mb();
}

void caf_sync_all()
{
  gasnet_barrier_notify(0,GASNET_BARRIERFLAG_ANONYMOUS);
  gasnet_barrier_wait(0,GASNET_BARRIERFLAG_ANONYMOUS);
}

void caf_sync_team( gex_TM_t team ) {
  gex_Event_Wait( gex_Coll_BarrierNB(team, 0) );
}

//-------------------------------------------------------------------

void caf_co_reduce(
  CFI_cdesc_t* a_desc, int result_image, size_t num_elements, gex_Coll_ReduceFn_t user_op, void* client_data, gex_TM_t team
) {
  assert(a_desc);
  assert(result_image >= 0);
  assert(num_elements > 0);
  assert(user_op);
#if PLATFORM_COMPILER_GNU
  // gfortran 13.2 & 14 - c_funloc is non-compliant
  // it erroneously generates a non-callable pointer to a pointer to the subroutine
  // Here we undo that incorrect extra level of indirection
  user_op = *(gex_Coll_ReduceFn_t *)user_op; 
#endif
  char* a_address = (char*) a_desc->base_addr;
  size_t c_sizeof_a = a_desc->elem_len;
  gex_Event_t ev;

  if (result_image) {
    ev = gex_Coll_ReduceToOneNB(
      team, result_image-1, a_address, a_address, GEX_DT_USER, c_sizeof_a, num_elements, GEX_OP_USER, user_op, &c_sizeof_a, 0
    );
  } else {
    ev = gex_Coll_ReduceToAllNB(
      team,                 a_address, a_address, GEX_DT_USER, c_sizeof_a, num_elements, GEX_OP_USER, user_op, &c_sizeof_a, 0
    );
  }
  gex_Event_Wait(ev);
}

void caf_co_broadcast(CFI_cdesc_t * a_desc, int source_image, int num_elements, gex_TM_t team)
{
  char* c_loc_a = (char*) a_desc->base_addr;
  size_t c_sizeof_a = a_desc->elem_len;
  int nbytes = num_elements * c_sizeof_a;

  int data_type = a_desc->type;

  gex_Event_t ev
    = gex_Coll_BroadcastNB(team, source_image-1, c_loc_a, c_loc_a, nbytes, 0);
  gex_Event_Wait(ev);
}

//-------------------------------------------------------------------
// Typed computational collective subroutines
//-------------------------------------------------------------------

// Convert CFI_type_t to the corresponding GEX reduction data type
// returns the size of the native type
static size_t CFI_to_GEX_DT(CFI_type_t cfi_type, gex_DT_t *gex_dt, int *complex_scale) {
  assert(gex_dt);

  if_pf (complex_scale) *complex_scale = 1;

  switch (cfi_type) {
    // real cases
    case CFI_type_float:            *gex_dt = GEX_DT_FLT; return 4;
    case CFI_type_double:           *gex_dt = GEX_DT_DBL; return 8;

    // complex cases
    case float_Complex_workaround:  *gex_dt = GEX_DT_FLT; 
      if (!complex_scale) gasnett_fatalerror("This operation does not support complex types");
      *complex_scale = 2;
      return 8;
    case double_Complex_workaround: *gex_dt = GEX_DT_DBL; 
      if (!complex_scale) gasnett_fatalerror("This operation does not support complex types");
      *complex_scale = 2;
      return 16;
    // no support for CFI_type_long_double or CFI_type_long_double_Complex
  }

  // integer types
  #define CFI_INT_CASE(cfi_type_constant, c_type) \
    else if (cfi_type == cfi_type_constant) { \
      if (sizeof(c_type) == 4) *gex_dt = GEX_DT_I32;  \
      else if (sizeof(c_type) > 8) \
         gasnett_fatalerror("Unsupported wide integer type: %d", (int)cfi_type); \
      else                     *gex_dt = GEX_DT_I64;  \
      return sizeof(c_type); \
    }
  // these must be handled outside the switch because there are duplicates
  // for the same reason, start with the most likely candidates
  if (0) ;
  CFI_INT_CASE(CFI_type_int64_t, int64_t)
  CFI_INT_CASE(CFI_type_int32_t, int32_t)
  CFI_INT_CASE(CFI_type_int16_t, int16_t)
  CFI_INT_CASE(CFI_type_int8_t, int8_t)
  CFI_INT_CASE(CFI_type_Bool, _Bool)
  CFI_INT_CASE(CFI_type_char, char)
  CFI_INT_CASE(CFI_type_signed_char, signed char)
  CFI_INT_CASE(CFI_type_short, short int)
  CFI_INT_CASE(CFI_type_int, int)
  CFI_INT_CASE(CFI_type_long, long int)
  CFI_INT_CASE(CFI_type_long_long, long long int)
  CFI_INT_CASE(CFI_type_size_t, size_t)
  CFI_INT_CASE(CFI_type_int_least8_t, int_least8_t)
  CFI_INT_CASE(CFI_type_int_least16_t, int_least16_t)
  CFI_INT_CASE(CFI_type_int_least32_t, int_least32_t)
  CFI_INT_CASE(CFI_type_int_least64_t, int_least64_t)
  CFI_INT_CASE(CFI_type_int_fast8_t, int_fast8_t)
  CFI_INT_CASE(CFI_type_int_fast16_t, int_fast16_t)
  CFI_INT_CASE(CFI_type_int_fast32_t, int_fast32_t)
  CFI_INT_CASE(CFI_type_int_fast64_t, int_fast64_t)
  CFI_INT_CASE(CFI_type_intmax_t, intmax_t)
  CFI_INT_CASE(CFI_type_intptr_t, intptr_t)
  CFI_INT_CASE(CFI_type_ptrdiff_t, ptrdiff_t)
  #undef CFI_INT_CASE

  gasnett_fatalerror("Unrecognized type: %d", (int)cfi_type);
}

// widen an 8- or 16-bit integer array to 64-bit 
static int64_t *widen_from_array(CFI_cdesc_t* a_desc, size_t num_elements) {
  assert(a_desc);
  int64_t *res = malloc(8 * num_elements);
  assert(res);
  if (a_desc->elem_len == 1) {
    int8_t *src = a_desc->base_addr;
    for (size_t i=0; i < num_elements; i++) res[i] = src[i];
  } else if (a_desc->elem_len == 2) {
    int16_t *src = a_desc->base_addr;
    for (size_t i=0; i < num_elements; i++) res[i] = src[i];
  } else gasnett_fatalerror("Logic error in widen_from_array: %i", a_desc->elem_len);
  return res;
}

// narrow a 64-bit integer array result back to 8- or 16-bit
static void narrow_to_array(CFI_cdesc_t* a_desc, int64_t *src, size_t num_elements) {
  assert(a_desc);
  assert(src);
  if (a_desc->elem_len == 1) {
    int8_t *dst = a_desc->base_addr;
    for (size_t i=0; i < num_elements; i++) dst[i] = src[i];
  } else if (a_desc->elem_len == 2) {
    int16_t *dst = a_desc->base_addr;
    for (size_t i=0; i < num_elements; i++) dst[i] = src[i];
  } else gasnett_fatalerror("Logic error in narrow_to_array: %i", a_desc->elem_len);
  free(src);
}

GASNETT_INLINE(caf_co_common)
void caf_co_common(CFI_cdesc_t* a_desc, int result_image, size_t num_elements, gex_TM_t team, gex_OP_t g_op) {

  int complex_scale = 1;
  gex_DT_t g_dt;
  size_t elem_sz = CFI_to_GEX_DT(a_desc->type, &g_dt, 
                                 (g_op == GEX_OP_ADD ? &complex_scale : NULL));

  int64_t * bounce_buffer = NULL;
  void * g_addr =  a_desc->base_addr;
  size_t g_elem_sz = a_desc->elem_len;
  assert(g_elem_sz == elem_sz);

  if_pf (complex_scale != 1) { // complex input, only permitted in prif_co_sum
    assert(g_op == GEX_OP_ADD);
    assert(complex_scale == 2);
    assert(g_elem_sz == 8 || g_elem_sz == 16);
    g_elem_sz >>= 1;
    num_elements <<= 1; 
  } else if_pf(elem_sz < 4) {
    bounce_buffer = widen_from_array(a_desc, num_elements);
    assert(g_dt == GEX_DT_I64);
    g_elem_sz = 8;
    g_addr = bounce_buffer;
  }

  gex_Event_t ev;
  if (result_image) {
    ev = gex_Coll_ReduceToOneNB(team, result_image-1, g_addr, g_addr, g_dt, g_elem_sz, num_elements, g_op, NULL, NULL, 0);
  } else {
    ev = gex_Coll_ReduceToAllNB(team,                 g_addr, g_addr, g_dt, g_elem_sz, num_elements, g_op, NULL, NULL, 0);
  }
  gex_Event_Wait(ev);

  if_pf(bounce_buffer) narrow_to_array(a_desc, bounce_buffer, num_elements);
}



void caf_co_max(CFI_cdesc_t* a_desc, int result_image, size_t num_elements, gex_TM_t team) {
  caf_co_common(a_desc, result_image, num_elements, team, GEX_OP_MAX);
}

void caf_co_min(CFI_cdesc_t* a_desc, int result_image, size_t num_elements, gex_TM_t team) {
  caf_co_common(a_desc, result_image, num_elements, team, GEX_OP_MIN);
}

void caf_co_sum(CFI_cdesc_t* a_desc, int result_image, size_t num_elements, gex_TM_t team) {
  caf_co_common(a_desc, result_image, num_elements, team, GEX_OP_ADD);
}

//-------------------------------------------------------------------
void caf_form_team(gex_TM_t current_team, gex_TM_t* new_team, int64_t team_number, int new_index)
{
   // GASNet color argument is int (32-bit), check for value truncation:
  assert((unsigned int)team_number == team_number);
  gex_TM_Split(new_team, current_team, team_number, new_index, NULL, 0, GEX_FLAG_TM_NO_SCRATCH);
}

