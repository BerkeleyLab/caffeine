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
static gex_TM_t myteam;
static gex_Rank_t rank, size;

#if __GNUC__ >= 12
  const int float_Complex_workaround = CFI_type_float_Complex;
  const int double_Complex_workaround = CFI_type_double_Complex;
#else
  const int float_Complex_workaround = 2052;
  const int double_Complex_workaround =4100;
#endif


#define HANDLE_SIZE(corank) (sizeof(intmax_t) + (sizeof(intmax_t) * (corank) * 2) + sizeof(final_func_ptr) + sizeof(size_t) + sizeof(int64_t) + sizeof(char*))

// macros that access a field of the coarray handle (CAH), an internal coarray metadata object

// since the size of the handle is dependent on the value of corank, the first field in the handle, each accessor
// dereferences corank and uses its value to correctly calculate the memory locations of the other fields
#define CAH_CORANK(coarray_handle) *((intmax_t*)(coarray_handle))
#define CAH_LCOBOUNDS(coarray_handle) (intmax_t*)((char*)(coarray_handle) + sizeof(intmax_t))
#define CAH_UCOBOUNDS(coarray_handle) (intmax_t*)((char*)(coarray_handle) + sizeof(intmax_t) + (sizeof(intmax_t) * *(intmax_t*)(coarray_handle)))
#define CAH_FINAL_FUNC(coarray_handle) *(final_func_ptr*)((char*)(coarray_handle) + sizeof(intmax_t) + (sizeof(intmax_t) * *(intmax_t*)(coarray_handle) * 2))
#define CAH_OBJECT_SIZE(coarray_handle) *(size_t*)((char*)(coarray_handle) + sizeof(intmax_t) + (sizeof(intmax_t) * *(intmax_t*)(coarray_handle) * 2) + sizeof(final_func_ptr))
#define CAH_ALLOCATOR(coarray_handle) *(int64_t*)((char*)(coarray_handle) + sizeof(intmax_t) + (sizeof(intmax_t) * *(intmax_t*)(coarray_handle) * 2) + sizeof(final_func_ptr) + sizeof(size_t))

#define CAH_OBJECT_BASE_ADDR(coarray_handle) *(char**)((char*)(coarray_handle) + sizeof(intmax_t) + (sizeof(intmax_t) * *(intmax_t*)(coarray_handle) * 2) + sizeof(final_func_ptr) + sizeof(size_t) + sizeof(int64_t))

#define PTR_DIFFERENCE_BYTES(first_element_addr, fortran_object_base_addr) ((char*)(first_element_addr) - (char*)(fortran_object_base_addr))

static void setupCoarrayHandle(void* handle_mem, int corank, intmax_t* lcobounds, intmax_t* ucobounds, final_func_ptr final_func, size_t sz, int64_t allocator, void* object_base_addr_ptr);

void caf_caffeinate(mspace* symmetric_heap)
{
  GASNET_SAFE(gex_Client_Init(&myclient, &myep, &myteam, "caffeine", NULL, NULL, 0));

  size_t segsz = GASNET_PAGESIZE;

  gex_Segment_t mysegment;
  GASNET_SAFE(gex_Segment_Attach(&mysegment, myteam, segsz));
  *symmetric_heap = create_mspace_with_base(gex_Segment_QueryAddr(mysegment), gex_Segment_QuerySize(mysegment), 0);
  mspace_set_footprint_limit(*symmetric_heap, gex_Segment_QuerySize(mysegment));
}

void caf_decaffeinate(int exit_code)
{
  gasnet_exit(exit_code);
}

int caf_this_image()
{
  return gex_TM_QueryRank(myteam) + 1;
}

int caf_num_images()
{
  return gex_TM_QuerySize(myteam);
}

// allocate memory for the Fortran object plus memory for a header which contains the coarray handle information
void* caf_allocate(size_t sz, int corank, CFI_cdesc_t* desc_lcobounds, CFI_cdesc_t* desc_ucobounds, final_func_ptr final_func, void** coarray_handle, mspace symmetric_heap)
{
   // coarray handle contains
   //      corank (scalar intmax_t)
   //      lcobounds (intmax_t array with size corank)
   //      ucobounds (intmax_t array with size corank)
   //      final function pointer
   //      object_sz (size_t)   ! should only be used in the finalizer, in other cases where coarray_handle can be accessed, the object_sz may not reflect the size of the data that we are dealing with
   //      allocator (int64_t)
   //      object_base_addr_ptr (char*)
   // TODO: add pointer to beginning of next coarray handle's addr

   // aliased coarray handle will have all of the elements of a coarray handle filled in
   // expect it will have no data in the Fortran object elements
   // the pointer to where the data begins will point back to the memory in the original
   // coarray handle where memory was allocated for the data

   // every time one accesses a coarray, need to traverse an extra level of indirection to get to the elements
   //

   // currently unused allocator field
   //      which allocator it came from
   //            1 - symmetric allocator with initial team
   //            2 - stack of allocators for each child team
   //            ...
   //                maybe need more for each team

   // TODO: Do we need to add alignment padding here for the elements? such as when dealing with c_long_double?
   //       How do you ensure that memory given to c_f_pointer is aligned correctly for Fortran for the given datatype?
   //       - will become global setting that is decided at runtime, or add arg to caf_allocate for minimum alignment

   intmax_t* lcobounds = desc_lcobounds->base_addr;
   intmax_t* ucobounds = desc_ucobounds->base_addr;

   assert(corank >= 1);                                                    // corank must be 1 or more (coarray)
   assert(desc_lcobounds->rank == 1 && desc_ucobounds->rank == 1);         // the lcobounds and ucobounds arrays must be 1d
   assert(desc_lcobounds->dim[0].extent == desc_ucobounds->dim[0].extent); // size of each cobounds array must be the same
   assert(desc_lcobounds->dim[0].extent == corank);                        // size of cobounds arrays must be equal to corank
   for(int i = 0; i < corank; i++) {
      assert(lcobounds[i] <= ucobounds[i]);                                // lcobounds must not be greater than ucobounds
   }

   size_t handle_sz = HANDLE_SIZE(corank);
   void* allocated_mem = mspace_memalign(symmetric_heap, 8, sz + handle_sz);

   setupCoarrayHandle(allocated_mem, corank, lcobounds, ucobounds, final_func, sz, -1, (char*)allocated_mem + handle_sz);

   *coarray_handle = allocated_mem;  // Return the address of the handle to the caller through the `coarray_handle` argument
   return (void*)((char*)allocated_mem + handle_sz); // Return the address of the Fortran object
}

static void setupCoarrayHandle(void* handle_mem, int corank, intmax_t* lcobounds, intmax_t* ucobounds, final_func_ptr final_func, size_t sz, int64_t allocator, void* object_base_addr_ptr)
{
   size_t cobounds_arr_sz = sizeof(intmax_t) * corank;

   // fill in coarray handle
   CAH_CORANK(handle_mem) = (intmax_t)corank;
   memcpy(CAH_LCOBOUNDS(handle_mem), lcobounds, cobounds_arr_sz);
   memcpy(CAH_UCOBOUNDS(handle_mem), ucobounds, cobounds_arr_sz);
   CAH_FINAL_FUNC(handle_mem) = final_func;
   CAH_OBJECT_SIZE(handle_mem) = sz;
   CAH_ALLOCATOR(handle_mem) = allocator;
   CAH_OBJECT_BASE_ADDR(handle_mem)= object_base_addr_ptr;
}

void caf_sync_all()
{
  gasnet_barrier_notify(0,GASNET_BARRIERFLAG_ANONYMOUS);
  gasnet_barrier_wait(0,GASNET_BARRIERFLAG_ANONYMOUS);
}

void caf_co_reduce(
  CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, int num_elements, gex_Coll_ReduceFn_t* user_op, void* client_data
)
{
  char* a_address = (char*) a_desc->base_addr;
  size_t c_sizeof_a = a_desc->elem_len;
  gex_Event_t ev;

  if (result_image) {
    ev = gex_Coll_ReduceToOneNB(
      myteam, result_image-1, a_address, a_address, GEX_DT_USER, c_sizeof_a, num_elements, GEX_OP_USER, user_op, &c_sizeof_a, 0
    );
  } else {
    ev = gex_Coll_ReduceToAllNB(
      myteam,                 a_address, a_address, GEX_DT_USER, c_sizeof_a, num_elements, GEX_OP_USER, user_op, &c_sizeof_a, 0
    );
  }
  gex_Event_Wait(ev);

  if (stat != NULL) *stat = 0;
}

void caf_co_broadcast(CFI_cdesc_t * a_desc, int source_image, int* stat, int num_elements)
{
  char* c_loc_a = (char*) a_desc->base_addr;
  size_t c_sizeof_a = a_desc->elem_len;
  int nbytes = num_elements * c_sizeof_a;

  int data_type = a_desc->type;

  gex_Event_t ev
    = gex_Coll_BroadcastNB(myteam, source_image-1, c_loc_a, c_loc_a, nbytes, 0);
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

void caf_co_max(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements)
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
    ev = gex_Coll_ReduceToOneNB(myteam, result_image-1, a_address, a_address, a_type, c_sizeof_a, num_elements, GEX_OP_MAX, NULL, NULL, 0);
  } else {
    ev = gex_Coll_ReduceToAllNB(myteam,                 a_address, a_address, a_type, c_sizeof_a, num_elements, GEX_OP_MAX, NULL, NULL, 0);
  }
  gex_Event_Wait(ev);

  if (stat != NULL) *stat = 0;
}

void caf_co_min(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements)
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
    ev = gex_Coll_ReduceToOneNB(myteam, result_image-1, a_address, a_address, a_type, c_sizeof_a, num_elements, GEX_OP_MIN, NULL, NULL, 0);
  } else {
    ev = gex_Coll_ReduceToAllNB(myteam,                 a_address, a_address, a_type, c_sizeof_a, num_elements, GEX_OP_MIN, NULL, NULL, 0);
  }
  gex_Event_Wait(ev);

  if (stat != NULL) *stat = 0;
}

void caf_co_sum(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements)
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
    ev = gex_Coll_ReduceToOneNB(myteam, result_image-1, a_address, a_address, a_type, c_sizeof_a, num_elements, GEX_OP_ADD, NULL, NULL, 0);
  } else {
    ev = gex_Coll_ReduceToAllNB(myteam,                 a_address, a_address, a_type, c_sizeof_a, num_elements, GEX_OP_ADD, NULL, NULL, 0);
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
