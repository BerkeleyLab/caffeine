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

void caf_caffeinate(mspace* symmetric_heap, gex_TM_t** initial_team);
void caf_decaffeinate(int exit_code);

// Image enumeration

int caf_this_image(gex_TM_t* team);
int caf_num_images(gex_TM_t* team);

// Memory allocation

void* caf_allocate(mspace heap, size_t bytes);

// Synchronization 

void caf_sync_all();

// _______ Collective Subroutines _______ 

void caf_co_reduce(
  CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, int num_elements, gex_Coll_ReduceFn_t* user_op, void* client_data, gex_TM_t* team
);

void caf_co_broadcast(
  CFI_cdesc_t * a_desc, int source_image, int* stat, int num_elements, gex_TM_t* team
);

void caf_co_sum(
  CFI_cdesc_t* a_desc,  int result_image, int* stat, char* errmsg, size_t num_elements, gex_TM_t* team
);

void caf_co_min(
  CFI_cdesc_t* a_desc,  int result_image, int* stat, char* errmsg, size_t num_elements, gex_TM_t* team
);

void caf_co_max(
  CFI_cdesc_t* a_desc, int result_image, int* stat, char* errmsg, size_t num_elements, gex_TM_t* team
);

// ____________ Utilities ____________ 

bool caf_same_cfi_type(CFI_cdesc_t* a_desc, CFI_cdesc_t* b_desc);

bool caf_numeric_type(CFI_cdesc_t* a_desc);

bool caf_is_f_string(CFI_cdesc_t* a_desc);

size_t caf_elem_len(CFI_cdesc_t* a_desc);

#endif // CAFFEINE_H
