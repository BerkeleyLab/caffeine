// Copyright (c), The Regents of the University of California
// Terms of use are as specified in LICENSE.txt

#ifndef CAFFEINE_H
#define CAFFEINE_H

#include <gasnetex.h>
#include <gasnet_coll.h>
#include <ISO_Fortran_binding.h>

enum {
  UNRECOGNIZED_TYPE, 
  ERRMSG_TOO_SHORT
};

// Program launch and finalization
void caf_c_caffeinate(int argc, char *argv[]);
void caf_c_decaffeinate(int exit_code);

// Image enumeration
int caf_c_this_image();
int caf_c_num_images();

// Synchronization 
void caf_c_sync_all();

// _______ Collective Subroutines _______ 

void caf_c_co_min_int32(void* c_loc_a, size_t Nelem, int* stat, int result_image);
void caf_c_co_min_int64(void* c_loc_a, size_t Nelem, int* stat, int result_image);
void caf_c_co_min_float(void* c_loc_a, size_t Nelem, int* stat, int result_image);
void caf_c_co_min_double(void* c_loc_a, size_t Nelem, int* stat, int result_image);

void caf_c_co_max_int32(void* c_loc_a, size_t Nelem, int* stat, int result_image);
void caf_c_co_max_int64(void* c_loc_a, size_t Nelem, int* stat, int result_image);
void caf_c_co_max_float(void* c_loc_a, size_t Nelem, int* stat, int result_image);
void caf_c_co_max_double(void* c_loc_a, size_t Nelem, int* stat, int result_image);

void caf_c_co_reduce_int32(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation);
void caf_c_co_reduce_float(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation);
void caf_c_co_reduce_char(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation, void* client_data);
void caf_c_co_reduce_bool(void* c_loc_a, size_t Nelem, int* stat, int result_image, gex_Coll_ReduceFn_t* operation);

// co_broadcast
void caf_c_co_broadcast(CFI_cdesc_t * a_desc, int source_image, int* stat, int num_elements);

// co_sum
void caf_c_co_sum(CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements);

#endif // CAFFEINE_H
