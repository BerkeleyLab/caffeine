#ifndef _485f7f27_ce8a_4829_a04c_aaa8182adab9
#define _485f7f27_ce8a_4829_a04c_aaa8182adab9

// Added for caffeine:
#define ONLY_MSPACES 1
#if CAFI_ASSERT_ENABLED
  #define DEBUG 1
#else
  #undef DEBUG
#endif

/*
 * Added for caffeine. This block of defines name shifts dlmalloc functions to have
 * a cafi_ prefix. Since dlmalloc is a commonly used library, name clashes can
 * occur when two libraries that use dlmalloc are linked to the same application
 * causing linker errors as they both define the dlmalloc symbols.
 */
#define create_mspace cafi_create_mspace
#define create_mspace_with_base cafi_create_mspace_with_base
#define destroy_mspace cafi_destroy_mspace
#define mspace_bulk_free cafi_mspace_bulk_free
#define mspace_calloc cafi_mspace_calloc
#define mspace_footprint cafi_mspace_footprint
#define mspace_footprint_limit cafi_mspace_footprint_limit
#define mspace_free cafi_mspace_free
#define mspace_independent_calloc cafi_mspace_independent_calloc
#define mspace_independent_comalloc cafi_mspace_independent_comalloc
#define mspace_mallinfo cafi_mspace_mallinfo
#define mspace_malloc cafi_mspace_malloc
#define mspace_malloc_stats cafi_mspace_malloc_stats
#define mspace_mallopt cafi_mspace_mallopt
#define mspace_max_footprint cafi_mspace_max_footprint
#define mspace_memalign cafi_mspace_memalign
#define mspace_realloc cafi_mspace_realloc
#define mspace_realloc_in_place cafi_mspace_realloc_in_place
#define mspace_set_footprint_limit cafi_mspace_set_footprint_limit
#define mspace_track_large_chunks cafi_mspace_track_large_chunks
#define mspace_trim cafi_mspace_trim
#define mspace_usable_size cafi_mspace_usable_size

#endif
