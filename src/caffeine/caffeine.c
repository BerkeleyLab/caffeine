// Copyright (c), The Regents of the University of California
// Terms of use are as specified in LICENSE.txt

#include <stdint.h>
#include <gasnetex.h>
#include <gasnet_coll.h>
#include <stdio.h>
#include "gasnet_safe.h"

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
void c_co_sum_no_result_image_int32(void* c_loc_a, size_t Nelem, int* stat)
{
      gex_Event_t ev
        = gex_Coll_ReduceToAllNB(myteam, c_loc_a, c_loc_a, GEX_DT_I32, sizeof(int32_t), Nelem, GEX_OP_ADD, NULL, NULL, 0);
      gex_Event_Wait(ev);  

     if (stat != NULL) *stat = 0;
}

void c_co_sum_no_result_image_int64(void* c_loc_a, size_t Nelem, int* stat)
{
      gex_Event_t ev
        = gex_Coll_ReduceToAllNB(myteam, c_loc_a, c_loc_a, GEX_DT_I64, sizeof(int64_t), Nelem, GEX_OP_ADD, NULL, NULL, 0);
      gex_Event_Wait(ev);  

     if (stat != NULL) *stat = 0;
}

void c_co_sum_no_result_image_float(void* c_loc_a, size_t Nelem, int* stat)
{
      gex_Event_t ev
        = gex_Coll_ReduceToAllNB(myteam, c_loc_a, c_loc_a, GEX_DT_FLT, sizeof(float), Nelem, GEX_OP_ADD, NULL, NULL, 0);
      gex_Event_Wait(ev);  

      if (stat != NULL) *stat = 0;
}

void c_co_sum_no_result_image_double(void* c_loc_a, size_t Nelem, int* stat)
{
      gex_Event_t ev
        = gex_Coll_ReduceToAllNB(myteam, c_loc_a, c_loc_a, GEX_DT_DBL, sizeof(double), Nelem, GEX_OP_ADD, NULL, NULL, 0);
      gex_Event_Wait(ev);  

      if (stat != NULL) *stat = 0;
}

void c_co_min_no_result_image_int32(void* c_loc_a, size_t Nelem)
{
      gex_Event_t ev
        = gex_Coll_ReduceToAllNB(myteam, c_loc_a, c_loc_a, GEX_DT_I32, sizeof(int32_t), Nelem, GEX_OP_MIN, NULL, NULL, 0);
      gex_Event_Wait(ev);  
}

void c_co_min_no_result_image_int64(void* c_loc_a, size_t Nelem)
{
      gex_Event_t ev
        = gex_Coll_ReduceToAllNB(myteam, c_loc_a, c_loc_a, GEX_DT_I64, sizeof(int64_t), Nelem, GEX_OP_MIN, NULL, NULL, 0);
      gex_Event_Wait(ev);  
}

void c_co_min_no_result_image_float(void* c_loc_a, size_t Nelem)
{
      gex_Event_t ev
        = gex_Coll_ReduceToAllNB(myteam, c_loc_a, c_loc_a, GEX_DT_FLT, sizeof(float), Nelem, GEX_OP_MIN, NULL, NULL, 0);
      gex_Event_Wait(ev);  
}

void c_co_min_no_result_image_double(void* c_loc_a, size_t Nelem)
{
      gex_Event_t ev
        = gex_Coll_ReduceToAllNB(myteam, c_loc_a, c_loc_a, GEX_DT_DBL, sizeof(double), Nelem, GEX_OP_MIN, NULL, NULL, 0);
      gex_Event_Wait(ev);  
}

void c_co_max_no_result_image_int32(void* c_loc_a, size_t Nelem, int* stat)
{
      gex_Event_t ev
        = gex_Coll_ReduceToAllNB(myteam, c_loc_a, c_loc_a, GEX_DT_I32, sizeof(int32_t), Nelem, GEX_OP_MAX, NULL, NULL, 0);
      gex_Event_Wait(ev);  

      if (stat != NULL) *stat = 0;
}

void c_co_max_no_result_image_int64(void* c_loc_a, size_t Nelem, int* stat)
{
      gex_Event_t ev
        = gex_Coll_ReduceToAllNB(myteam, c_loc_a, c_loc_a, GEX_DT_I64, sizeof(int64_t), Nelem, GEX_OP_MAX, NULL, NULL, 0);
      gex_Event_Wait(ev);  

      if (stat != NULL) *stat = 0;
}

void c_co_max_no_result_image_float(void* c_loc_a, size_t Nelem, int* stat)
{
      gex_Event_t ev
        = gex_Coll_ReduceToAllNB(myteam, c_loc_a, c_loc_a, GEX_DT_FLT, sizeof(float), Nelem, GEX_OP_MAX, NULL, NULL, 0);
      gex_Event_Wait(ev);  

      if (stat != NULL) *stat = 0;
}

void c_co_max_no_result_image_double(void* c_loc_a, size_t Nelem, int* stat)
{
      gex_Event_t ev
        = gex_Coll_ReduceToAllNB(myteam, c_loc_a, c_loc_a, GEX_DT_DBL, sizeof(double), Nelem, GEX_OP_MAX, NULL, NULL, 0);
      gex_Event_Wait(ev);  

      if (stat != NULL) *stat = 0;
}
