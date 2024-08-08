// Copyright (c), The Regents of the University of California
// Terms of use are as specified in LICENSE.txt

#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <gasnetex.h>
#include <gasnet_coll.h>
#include "gasnet_safe.h"
#include <gasnet_tools.h>
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

#if __GNUC__ >= 12
  const int float_Complex_workaround = CFI_type_float_Complex;
  const int double_Complex_workaround = CFI_type_double_Complex;
#else
  const int float_Complex_workaround = 2052;
  const int double_Complex_workaround =4100;
#endif

int caf_this_image(gex_TM_t team)
{
  return gex_TM_QueryRank(team) + 1;
}

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

int caf_num_images(gex_TM_t team)
{
  return gex_TM_QuerySize(team);
}

void* caf_allocate(mspace heap, size_t bytes)
{
   return mspace_memalign(heap, 8, bytes);
}

void* caf_allocate_remaining(mspace heap, void** allocated_space, size_t* allocated_size)
{
  // The following doesn't necessarily give us all remaining space
  // nor necessarily the largest open space, but in practice is likely
  // to work out that way
  struct mallinfo heap_info = mspace_mallinfo(heap);
  *allocated_size = heap_info.keepcost * 0.5f;
  *allocated_space = mspace_memalign(heap, 8, *allocated_size);
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

void caf_put(int image, intptr_t dest, void* src, size_t size)
{
  gex_RMA_PutBlocking(myworldteam, image-1, (void*)dest, src, size, 0);
}

void caf_get(int image, void* dest, intptr_t src, size_t size)
{
  gex_RMA_GetBlocking(myworldteam, dest, image-1, (void*)src, size, 0);
}

void caf_sync_all()
{
  gasnet_barrier_notify(0,GASNET_BARRIERFLAG_ANONYMOUS);
  gasnet_barrier_wait(0,GASNET_BARRIERFLAG_ANONYMOUS);
}

void caf_co_reduce(
  CFI_cdesc_t* a_desc, int result_image, int num_elements, gex_Coll_ReduceFn_t user_op, void* client_data, gex_TM_t team
)
{
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

void caf_co_max(CFI_cdesc_t* a_desc, int result_image, size_t num_elements, gex_TM_t team)
{
  gex_DT_t a_type;

  switch (a_desc->type)
  {
    case CFI_type_int32_t:          a_type = GEX_DT_I32; break;
    case CFI_type_int64_t:          a_type = GEX_DT_I64; break;
    case CFI_type_float:            a_type = GEX_DT_FLT; break;
    case CFI_type_double:           a_type = GEX_DT_DBL; break;
    default:
      gasnett_fatalerror("Unrecognized type: %d", (int)a_desc->type);
  }

  char* a_address = (char*) a_desc->base_addr;

  size_t c_sizeof_a = a_desc->elem_len;

  gex_Event_t ev;

  if (result_image) {
    ev = gex_Coll_ReduceToOneNB(team, result_image-1, a_address, a_address, a_type, c_sizeof_a, num_elements, GEX_OP_MAX, NULL, NULL, 0);
  } else {
    ev = gex_Coll_ReduceToAllNB(team,                 a_address, a_address, a_type, c_sizeof_a, num_elements, GEX_OP_MAX, NULL, NULL, 0);
  }
  gex_Event_Wait(ev);
}

void caf_co_min(CFI_cdesc_t* a_desc, int result_image, size_t num_elements, gex_TM_t team)
{
  gex_DT_t a_type;

  switch (a_desc->type)
  {
    case CFI_type_int32_t:          a_type = GEX_DT_I32; break;
    case CFI_type_int64_t:          a_type = GEX_DT_I64; break;
    case CFI_type_float:            a_type = GEX_DT_FLT; break;
    case CFI_type_double:           a_type = GEX_DT_DBL; break;
    default:
      gasnett_fatalerror("Unrecognized type: %d", (int)a_desc->type);
  }

  char* a_address = (char*) a_desc->base_addr;

  size_t c_sizeof_a = a_desc->elem_len;

  gex_Event_t ev;

  if (result_image) {
    ev = gex_Coll_ReduceToOneNB(team, result_image-1, a_address, a_address, a_type, c_sizeof_a, num_elements, GEX_OP_MIN, NULL, NULL, 0);
  } else {
    ev = gex_Coll_ReduceToAllNB(team,                 a_address, a_address, a_type, c_sizeof_a, num_elements, GEX_OP_MIN, NULL, NULL, 0);
  }
  gex_Event_Wait(ev);
}

void caf_co_sum(CFI_cdesc_t* a_desc, int result_image, size_t num_elements, gex_TM_t team)
{
  gex_DT_t a_type;

  size_t c_sizeof_a = a_desc->elem_len;

  switch (a_desc->type)
  {
    case CFI_type_int32_t:          a_type = GEX_DT_I32; break;
    case CFI_type_int64_t:          a_type = GEX_DT_I64; break;
    case CFI_type_float:            a_type = GEX_DT_FLT; break;
    case CFI_type_double:           a_type = GEX_DT_DBL; break;
    case float_Complex_workaround:  a_type = GEX_DT_FLT; num_elements *= 2; c_sizeof_a /= 2; break;
    case double_Complex_workaround: a_type = GEX_DT_DBL; num_elements *= 2; c_sizeof_a /= 2; break;
    default:
      gasnett_fatalerror("Unrecognized type: %d", (int)a_desc->type);
  }

  char* a_address = (char*) a_desc->base_addr;

  gex_Event_t ev;

  if (result_image) {
    ev = gex_Coll_ReduceToOneNB(team, result_image-1, a_address, a_address, a_type, c_sizeof_a, num_elements, GEX_OP_ADD, NULL, NULL, 0);
  } else {
    ev = gex_Coll_ReduceToAllNB(team,                 a_address, a_address, a_type, c_sizeof_a, num_elements, GEX_OP_ADD, NULL, NULL, 0);
  }
  gex_Event_Wait(ev);
}

bool caf_same_cfi_type(CFI_cdesc_t* a_desc, CFI_cdesc_t* b_desc)
{
  if (a_desc->type == b_desc->type) return true;
  return false;
}

size_t caf_elem_len(CFI_cdesc_t* a_desc)
{
  return a_desc->elem_len;
}

void caf_form_team(gex_TM_t current_team, gex_TM_t* new_team, intmax_t team_number, int new_index)
{
  gex_TM_Split(new_team, current_team, team_number, new_index, NULL, 0, GEX_FLAG_TM_NO_SCRATCH);
}

bool caf_numeric_type(CFI_cdesc_t* a_desc)
{
  switch (a_desc->type)
  {
    case CFI_type_int32_t:          return true;
    case CFI_type_int64_t:          return true;
    case CFI_type_float:            return true;
    case CFI_type_double:           return true;
    case float_Complex_workaround:  return true;
    case double_Complex_workaround: return true;
    default:                        return false;
  }
}

#ifdef __GNUC__
bool caf_is_f_string(CFI_cdesc_t* a_desc){
  if ( (a_desc->type - 5) % 256 == 0) return true;
  return false;
}
#else // The code below is untested but believed to conform with the Fortran 2018 standard.
bool caf_is_f_string(CFI_cdesc_t* a_desc){
  if (a_desc->type == CFI_type_char) return true;
  return false;
}
#endif
