// Copyright (c), The Regents of the University of California
// Terms of use are as specified in LICENSE.txt

#ifndef CAFFEINE_H
#define CAFFEINE_H

#include <stdbool.h>
#include <gasnetex.h>
#include <gasnet_coll.h>
#include <ISO_Fortran_binding.h>
#include "../dlmalloc/dl_malloc_caf.h"
#include "../dlmalloc/dl_malloc.h"

enum {
  UNRECOGNIZED_TYPE, 
  ERRMSG_TOO_SHORT
};

typedef void(*final_func_ptr)(void*, size_t) ;

// Program launch and finalization

void caf_caffeinate();
void caf_decaffeinate(int exit_code);

// Image enumeration

int caf_this_image();
int caf_num_images();

// Memory allocation

void* caf_allocate(size_t sz, int corank, CFI_cdesc_t* desc_co_lbounds, CFI_cdesc_t* desc_co_ubounds, final_func_ptr final_func, void** coarray_handle);

// Synchronization 

void caf_sync_all();

// _______ Collective Subroutines _______ 

void caf_co_reduce(
  CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, int num_elements, gex_Coll_ReduceFn_t* user_op, void* client_data
);

void caf_co_broadcast(
  CFI_cdesc_t * a_desc, int source_image, int* stat, int num_elements
);

void caf_co_sum(
  CFI_cdesc_t* a_desc,  int result_image, int* stat, char* errmsg, size_t num_elements
);

void caf_co_min(
  CFI_cdesc_t* a_desc,  int result_image, int* stat, char* errmsg, size_t num_elements
);

void caf_co_max(
  CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements
);

// ____________ Utilities ____________ 

bool caf_same_cfi_type(CFI_cdesc_t* a_desc, CFI_cdesc_t* b_desc);

bool caf_numeric_type(CFI_cdesc_t* a_desc);

bool caf_is_f_string(CFI_cdesc_t* a_desc);

size_t caf_elem_len(CFI_cdesc_t* a_desc);

#endif // CAFFEINE_H
