// Copyright (c), The Regents of the University of California
// Terms of use are as specified in LICENSE.txt

#ifndef CAFFEINE_H
#define CAFFEINE_H

#include <stdbool.h>
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

void caf_c_co_reduce(
  CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, int num_elements, gex_Coll_ReduceFn_t* user_op, void* client_data
);

void caf_c_co_broadcast(
  CFI_cdesc_t * a_desc, int source_image, int* stat, int num_elements
);

void caf_c_co_sum(
  CFI_cdesc_t* a_desc,  int result_image, int* stat, char* errmsg, size_t num_elements
);

void caf_c_co_min(
  CFI_cdesc_t* a_desc,  int result_image, int* stat, char* errmsg, size_t num_elements
);

void caf_c_co_max(
  CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements
);

// ____________ Utilities ____________ 

bool caf_c_same_cfi_type(CFI_cdesc_t* a_desc, CFI_cdesc_t* b_desc);

bool caf_c_numeric_type(CFI_cdesc_t* a_desc);

bool caf_c_is_f_string(CFI_cdesc_t* a_desc);

size_t caf_c_elem_len(CFI_cdesc_t* a_desc);

#endif // CAFFEINE_H
