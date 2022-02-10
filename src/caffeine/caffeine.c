// Copyright (c), The Regents of the University of California
// Terms of use are as specified in LICENSE.txt

#include "caffeine.h"
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h> 
#include "gasnet_safe.h"

static gex_Client_t myclient;
static gex_EP_t myep;
static gex_TM_t myteam;
static gex_Rank_t rank, size;

#if __GNUC__ >= 12
  const int float_Complex_workaround = CFI_type_float_Complex;
  const int double_Complex_workaround = CFI_type_double_Complex;
#else
  const int float_Complex_workaround = 2052;
  const int double_Complex_workaround = 4011;
#endif

void c_caffeinate(int argc, char *argv[])
{
  GASNET_SAFE(gex_Client_Init(&myclient, &myep, &myteam, "caffeine", &argc, &argv, 0));
  
  size_t segsz = GASNET_PAGESIZE;  
  
  int argi = 1;
  if (argi < argc) {
    if (!strcmp(argv[argi], "-m")) {
      segsz = gasnet_getMaxLocalSegmentSize();
    } else {
      size_t tmp = atol(argv[argi]);
      if (tmp) segsz = tmp;
    }
    ++argi;
  }
    
  gex_Segment_t mysegment;
  GASNET_SAFE(gex_Segment_Attach(&mysegment, myteam, segsz));
}

void c_decaffeinate(int exit_code)
{
  gasnet_exit(exit_code);
}

int c_this_image()
{
  return gex_TM_QueryRank(myteam) + 1;
}

int c_num_images()
{
  return gex_TM_QuerySize(myteam);
}

void c_sync_all()
{
  gasnet_barrier_notify(0,GASNET_BARRIERFLAG_ANONYMOUS);
  gasnet_barrier_wait(0,GASNET_BARRIERFLAG_ANONYMOUS);
}

void c_co_reduce_universal(void* c_loc_a, size_t Nelem, int* stat, int result_image, int dt, size_t sizeof_type, int op, gex_Coll_ReduceFn_t* user_op, void* client_data)
{
     gex_Event_t ev;
     const int not_present=0;

     if (result_image == not_present) {
       ev = gex_Coll_ReduceToAllNB(myteam, c_loc_a, c_loc_a, dt, sizeof_type, Nelem, op, user_op, client_data, 0);
     } else {
       ev = gex_Coll_ReduceToOneNB(myteam, result_image-1, c_loc_a, c_loc_a, dt, sizeof_type, Nelem, op, user_op, client_data, 0);
     }
     gex_Event_Wait(ev);  

     if (stat != NULL) *stat = 0;
}

void c_co_min_int32(void* c_loc_a, size_t Nelem, int* stat, int result_image)
{
     c_co_reduce_universal(c_loc_a, Nelem, stat, result_image, GEX_DT_I32, sizeof(int32_t), GEX_OP_MIN, NULL, NULL);
}

void c_co_min_int64(void* c_loc_a, size_t Nelem, int* stat, int result_image)
{
     c_co_reduce_universal(c_loc_a, Nelem, stat, result_image, GEX_DT_I64, sizeof(int64_t), GEX_OP_MIN, NULL, NULL);
}

void c_co_min_float(void* c_loc_a, size_t Nelem, int* stat, int result_image)
{
     c_co_reduce_universal(c_loc_a, Nelem, stat, result_image, GEX_DT_FLT, sizeof(float), GEX_OP_MIN, NULL, NULL);
}

void c_co_min_double(void* c_loc_a, size_t Nelem, int* stat, int result_image)
{
     c_co_reduce_universal(c_loc_a, Nelem, stat, result_image, GEX_DT_DBL, sizeof(double), GEX_OP_MIN, NULL, NULL);
}

void c_co_max_int32(void* c_loc_a, size_t Nelem, int* stat, int result_image)
{
     c_co_reduce_universal(c_loc_a, Nelem, stat, result_image, GEX_DT_I32, sizeof(int32_t), GEX_OP_MAX, NULL, NULL);
}

void c_co_max_int64(void* c_loc_a, size_t Nelem, int* stat, int result_image)
{
     c_co_reduce_universal(c_loc_a, Nelem, stat, result_image, GEX_DT_I64, sizeof(int64_t), GEX_OP_MAX, NULL, NULL);
}

void c_co_max_float(void* c_loc_a, size_t Nelem, int* stat, int result_image)
{
     c_co_reduce_universal(c_loc_a, Nelem, stat, result_image, GEX_DT_FLT, sizeof(float), GEX_OP_MAX, NULL, NULL);
}

void c_co_max_double(void* c_loc_a, size_t Nelem, int* stat, int result_image)
{
     c_co_reduce_universal(c_loc_a, Nelem, stat, result_image, GEX_DT_DBL, sizeof(double), GEX_OP_MAX, NULL, NULL);
}

void c_co_reduce_int32(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation)
{
     c_co_reduce_universal(c_loc_a, Nelem, stat, result_image, GEX_DT_I32, sizeof(int32_t), GEX_OP_USER, operation, NULL);
}

void c_co_reduce_float(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation)
{
     c_co_reduce_universal(c_loc_a, Nelem, stat, result_image, GEX_DT_FLT, sizeof(float), GEX_OP_USER, operation, NULL);
}

void c_co_reduce_char(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation, void* client_data)
{
     c_co_reduce_universal(c_loc_a, Nelem, stat, result_image, GEX_DT_USER, sizeof(char), GEX_OP_USER, operation, client_data);
}

void c_co_reduce_bool(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation)
{
     c_co_reduce_universal(c_loc_a, Nelem, stat, result_image, GEX_DT_I32, sizeof(bool), GEX_OP_USER, operation, NULL);
}

void c_co_broadcast(CFI_cdesc_t * a_desc, int source_image, int* stat, int num_elements)
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

void set_stat_errmsg_or_abort(int* stat, char* errmsg, const int return_stat, const char const return_message[])
{
  if (stat == NULL && errmsg == NULL) {
    printf("%s\n", *return_message);
    abort();
  }

  if (stat != NULL) *stat = return_stat;

  if (errmsg != NULL) {
    if (strlen(errmsg) >= strlen(return_message)) {
      // TODO: Figure out how/whether to handle repositioning of the null terminator.
      errmsg = return_message; 
    } else {
      // TODO: Figure out how to replace this with an assignment of a truncated error message.
      printf("caffeine.c: strlen(errmsg)=%d too short\n", strlen(errmsg));
      abort();  
    }
  }
}

void c_co_sum(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements)
{
  CFI_type_t a_type = a_desc->type;

  switch (a_type)
  {
    case CFI_type_int32_t:        a_type = GEX_DT_I32; break;
    case CFI_type_int64_t:        a_type = GEX_DT_I64; break;
    case CFI_type_float:          a_type = GEX_DT_FLT; break;
    case CFI_type_double:         a_type = GEX_DT_DBL; break;
    case float_Complex_workaround:  a_type = GEX_DT_FLT; num_elements *= 2; break;
    case double_Complex_workaround: a_type = GEX_DT_DBL; num_elements *= 2; break;
    default:
      set_stat_errmsg_or_abort(stat, errmsg, UNRECOGNIZED_TYPE, "");
  }

  char* a_address = (char*) a_desc->base_addr;

  size_t c_sizeof_a = a_desc->elem_len;

  if (a_type == float_Complex_workaround || a_type == double_Complex_workaround) c_sizeof_a /= 2;

  gex_Event_t ev;

  if (result_image) {
    ev = gex_Coll_ReduceToOneNB(myteam, result_image-1, a_address, a_address, a_type, c_sizeof_a, num_elements, GEX_OP_ADD, NULL, NULL, 0);
  } else {
    ev = gex_Coll_ReduceToAllNB(myteam,                 a_address, a_address, a_type, c_sizeof_a, num_elements, GEX_OP_ADD, NULL, NULL, 0);
  }
  gex_Event_Wait(ev);  

  if (stat != NULL) *stat = 0;
}
