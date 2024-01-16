// Copyright (c), The Regents of the University of California
// Terms of use are as specified in LICENSE.txt

#include "caffeine.h"
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include "gasnet_safe.h"
#include <gasnet_tools.h>

static gex_Client_t myclient;
static gex_EP_t myep;
static gex_Rank_t rank, size;

#if __GNUC__ >= 12
  const int float_Complex_workaround = CFI_type_float_Complex;
  const int double_Complex_workaround = CFI_type_double_Complex;
#else
  const int float_Complex_workaround = 2052;
  const int double_Complex_workaround =4100;
#endif

void caf_caffeinate(mspace* symmetric_heap, intptr_t* symmetric_heap_start, gex_TM_t* initial_team)
{
  GASNET_SAFE(gex_Client_Init(&myclient, &myep, initial_team, "caffeine", NULL, NULL, 0));

  size_t segsz = GASNET_PAGESIZE; // TODO: how big can we make this?

  gex_Segment_t mysegment;
  GASNET_SAFE(gex_Segment_Attach(&mysegment, *initial_team, segsz));
  // TODO: split into symmetric and non-symmetric heaps
  *symmetric_heap_start = (intptr_t)gex_Segment_QueryAddr(mysegment);
  if (caf_this_image(*initial_team) == 1) {
    *symmetric_heap = create_mspace_with_base(gex_Segment_QueryAddr(mysegment), gex_Segment_QuerySize(mysegment), 0);
    mspace_set_footprint_limit(*symmetric_heap, gex_Segment_QuerySize(mysegment));
  }
}

void caf_decaffeinate(int exit_code)
{
  gasnet_exit(exit_code);
}

int caf_this_image(gex_TM_t team)
{
  return gex_TM_QueryRank(team) + 1;
}

int caf_num_images(gex_TM_t team)
{
  return gex_TM_QuerySize(team);
}


void* caf_allocate(mspace heap, size_t bytes)
{
   return mspace_memalign(heap, 8, bytes);
}

void caf_deallocate(mspace heap, void* mem)
{
  mspace_free(heap, mem);
}

void caf_sync_all()
{
  gasnet_barrier_notify(0,GASNET_BARRIERFLAG_ANONYMOUS);
  gasnet_barrier_wait(0,GASNET_BARRIERFLAG_ANONYMOUS);
}

void caf_co_reduce(
  CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, int num_elements, gex_Coll_ReduceFn_t* user_op, void* client_data, gex_TM_t team
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

  if (stat != NULL) *stat = 0;
}

void caf_co_broadcast(CFI_cdesc_t * a_desc, int source_image, int* stat, int num_elements, gex_TM_t team)
{
  char* c_loc_a = (char*) a_desc->base_addr;
  size_t c_sizeof_a = a_desc->elem_len;
  int nbytes = num_elements * c_sizeof_a;

  int data_type = a_desc->type;

  gex_Event_t ev
    = gex_Coll_BroadcastNB(team, source_image-1, c_loc_a, c_loc_a, nbytes, 0);
  gex_Event_Wait(ev);

  if (stat != NULL) *stat = 0;
}

void set_stat_errmsg_or_abort(int* stat, char* errmsg, const int return_stat, const char* return_message)
{
  if (stat == NULL && errmsg == NULL) gasnett_fatalerror("%s", return_message);

  if (stat != NULL) *stat = return_stat;

  if (errmsg != NULL) {
    if (strlen(errmsg) >= strlen(return_message)) {
      // TODO: Figure out how/whether to handle repositioning of the null terminator.
      errmsg = return_message;
    } else {
      // TODO: Figure out how to replace this with an assignment of a truncated error message.
      gasnett_fatalerror("%s", "caffeine.c: strlen(errmsg) too small");
    }
  }
}

void caf_co_max(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements, gex_TM_t team)
{
  gex_DT_t a_type;

  switch (a_desc->type)
  {
    case CFI_type_int32_t:          a_type = GEX_DT_I32; break;
    case CFI_type_int64_t:          a_type = GEX_DT_I64; break;
    case CFI_type_float:            a_type = GEX_DT_FLT; break;
    case CFI_type_double:           a_type = GEX_DT_DBL; break;
    default:
      set_stat_errmsg_or_abort(stat, errmsg, UNRECOGNIZED_TYPE, "");
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

  if (stat != NULL) *stat = 0;
}

void caf_co_min(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements, gex_TM_t team)
{
  gex_DT_t a_type;

  switch (a_desc->type)
  {
    case CFI_type_int32_t:          a_type = GEX_DT_I32; break;
    case CFI_type_int64_t:          a_type = GEX_DT_I64; break;
    case CFI_type_float:            a_type = GEX_DT_FLT; break;
    case CFI_type_double:           a_type = GEX_DT_DBL; break;
    default:
      set_stat_errmsg_or_abort(stat, errmsg, UNRECOGNIZED_TYPE, "");
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

  if (stat != NULL) *stat = 0;
}

void caf_co_sum(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements, gex_TM_t team)
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
      set_stat_errmsg_or_abort(stat, errmsg, UNRECOGNIZED_TYPE, "");
  }

  char* a_address = (char*) a_desc->base_addr;

  gex_Event_t ev;

  if (result_image) {
    ev = gex_Coll_ReduceToOneNB(team, result_image-1, a_address, a_address, a_type, c_sizeof_a, num_elements, GEX_OP_ADD, NULL, NULL, 0);
  } else {
    ev = gex_Coll_ReduceToAllNB(team,                 a_address, a_address, a_type, c_sizeof_a, num_elements, GEX_OP_ADD, NULL, NULL, 0);
  }
  gex_Event_Wait(ev);

  if (stat != NULL) *stat = 0;
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

intptr_t as_int(void* ptr) {
  return (intptr_t)ptr;
}

void* as_c_ptr(intptr_t i) {
  return (void*)i;
}
