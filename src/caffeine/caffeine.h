// Copyright (c), The Regents of the University of California
// Terms of use are as specified in LICENSE.txt

#ifndef CAFFEINE_H
#define CAFFEINE_H

#include <gasnetex.h>
#include <gasnet_coll.h>
#include <ISO_Fortran_binding.h>

static gex_Client_t myclient;
static gex_EP_t myep;
static gex_TM_t myteam;
static gex_Rank_t rank, size;

enum {
  UNRECOGNIZED_TYPE, 
  ERRMSG_TOO_SHORT
};

// Program launch and finalization
void c_caffeinate(int argc, char *argv[]);
void c_decaffeinate(int exit_code);

// Image enumeration
int c_this_image();
int c_num_images();

// Synchronization 
void c_sync_all();

// _______ Collective Subroutines _______ 

void c_co_min_int32(void* c_loc_a, size_t Nelem, int* stat, int result_image);
void c_co_min_int64(void* c_loc_a, size_t Nelem, int* stat, int result_image);
void c_co_min_float(void* c_loc_a, size_t Nelem, int* stat, int result_image);
void c_co_min_double(void* c_loc_a, size_t Nelem, int* stat, int result_image);

void c_co_max_int32(void* c_loc_a, size_t Nelem, int* stat, int result_image);
void c_co_max_int64(void* c_loc_a, size_t Nelem, int* stat, int result_image);
void c_co_max_float(void* c_loc_a, size_t Nelem, int* stat, int result_image);
void c_co_max_double(void* c_loc_a, size_t Nelem, int* stat, int result_image);

void c_co_reduce_int32(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation);
void c_co_reduce_float(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation);
void c_co_reduce_char(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation, void* client_data);
void c_co_reduce_bool(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation);

// co_broadcast
void c_co_broadcast(CFI_cdesc_t * a_desc, int source_image, int* stat, int num_elements);

// co_sum
void c_co_sum(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements);

#endif // CAFFEINE_H
