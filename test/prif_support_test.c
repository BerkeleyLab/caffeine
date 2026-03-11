#include <stdio.h>
#include "ISO_Fortran_binding.h"
#include <assert.h>

struct coarray_handle {
  void *info;
};

extern void coarray_cleanup_simple(struct coarray_handle handle, int* stat, CFI_cdesc_t* errmsg);

extern void coarray_cleanup_simple_c(struct coarray_handle handle, int* stat, CFI_cdesc_t* errmsg) {
#if VERBOSE
  printf("Hello from coarray_cleanup_simple_c in C!\n"); fflush(0);
#endif

  // dispatch back to the test cleanup function written in Fortran:
  coarray_cleanup_simple(handle, stat, errmsg);

  assert(*stat == 0);
}
