#include <stdio.h>
#include "ISO_Fortran_binding.h"
#include <assert.h>

extern int ff_count;

struct coarray_handle {
  void *info;
};

extern void coarray_cleanup_simple(struct coarray_handle handle);
extern void prif_get_context_data(struct coarray_handle handle, void **context_data);
extern void prif_set_context_data(struct coarray_handle handle, void *context_data);
extern void prif_local_data_pointer(struct coarray_handle handle, void **local_data);
extern void prif_size_bytes(struct coarray_handle handle, size_t *data_size);

extern void coarray_cleanup_simple_c(struct coarray_handle handle) {
#if VERBOSE
  printf("Hello from coarray_cleanup_simple_c in C! ff_count=%i\n", ff_count); fflush(0);
#endif

  ff_count++;

  size_t sz = 0;
  prif_size_bytes(handle, &sz);
  assert(sz == sizeof(int));

  void *data = 0;
  prif_local_data_pointer(handle, &data);
  assert(data);
  assert(*(int*)data == 42);
 
  void *p = 0;
  prif_get_context_data(handle, &p);
  assert(p == &ff_count);
  prif_set_context_data(handle, &p);
  prif_get_context_data(handle, &p);
  assert(p == &p);

  // dispatch back to the test cleanup function written in Fortran:
  coarray_cleanup_simple(handle);
}
