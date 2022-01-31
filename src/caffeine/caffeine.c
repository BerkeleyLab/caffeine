// Copyright (c), The Regents of the University of California
// Terms of use are as specified in LICENSE.txt

#include <stdint.h>
#include <gasnetex.h>
#include <gasnet_coll.h>
#include <stdio.h>
#include "gasnet_safe.h"
#include <stdbool.h> 

static gex_Client_t myclient;
static gex_EP_t myep;
static gex_TM_t myteam;
static gex_Rank_t rank, size;

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

void c_co_sum_int32(void* c_loc_a, size_t Nelem, int* stat, int* result_image)
{
     c_co_reduce_universal(c_loc_a, Nelem, stat, result_image, GEX_DT_I32, sizeof(int32_t), GEX_OP_ADD, NULL, NULL);
}

void c_co_sum_int64(void* c_loc_a, size_t Nelem, int* stat, int* result_image)
{
     c_co_reduce_universal(c_loc_a, Nelem, stat, result_image, GEX_DT_I64, sizeof(int64_t), GEX_OP_ADD, NULL, NULL);
}

void c_co_sum_float(void* c_loc_a, size_t Nelem, int* stat, int* result_image)
{
     c_co_reduce_universal(c_loc_a, Nelem, stat, result_image, GEX_DT_FLT, sizeof(float), GEX_OP_ADD, NULL, NULL);
}

void c_co_sum_double(void* c_loc_a, size_t Nelem, int* stat, int* result_image)
{
     c_co_reduce_universal(c_loc_a, Nelem, stat, result_image, GEX_DT_DBL, sizeof(double), GEX_OP_ADD, NULL, NULL);
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
