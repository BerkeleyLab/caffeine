set(prif_sources ${prif_sources}
  ../prif.F90
  ./alias_s.F90
  ./allocation_s.F90
  ./atomic_s.F90
  ./co_broadcast_s.F90
  ./co_max_s.F90
  ./co_min_s.F90
  ./co_reduce_s.F90
  ./co_sum_s.F90
  ./coarray_access_s.F90
  ./coarray_queries_s.F90
  ./critical_s.F90
  ./events_s.F90
  ./image_queries_s.F90
  ./locks_s.F90
  ./prif_private_s.F90
  ./program_startup_s.F90
  ./program_termination_s.F90
  ./sync_stmt_s.F90
  ./teams_s.F90
  ./unit_test_parameters_m.F90

  ./caffeine.c 
  ../dlmalloc/dl_malloc.c
)

add_library(caffeine ${prif_sources})
