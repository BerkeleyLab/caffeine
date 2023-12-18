! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module prif
  use program_startup_m, only : prif_init
  use program_termination_m, only : prif_stop, prif_error_stop, prif_fail_image
  use allocation_m, only: prif_coarray_handle, prif_allocate
  use coarray_queries_m, only: prif_lcobound, prif_ucobound, prif_coshape, prif_image_index
  use image_queries_m, only : prif_this_image, prif_num_images
  use collective_subroutines_m, only : prif_co_sum, prif_co_max, prif_co_min, prif_co_reduce, prif_co_broadcast
  use teams_m, only: prif_form_team, prif_change_team, prif_end_team, prif_team_type, prif_get_team, prif_team_number
  use synchronization_m, only : prif_sync_all, prif_sync_images, prif_sync_team, prif_sync_memory
  use locks_m, only: prif_lock_type, prif_lock, prif_unlock
  use critical_m, only: prif_critical, prif_end_critical
  use events_m, only: prif_event_post, prif_event_wait, prif_event_query
  use atomic_m, only: prif_atomic_add, prif_atomic_and, prif_atomic_or, prif_atomic_xor, prif_atomic_cas, prif_atomic_fetch_add
  use atomic_m, only: prif_atomic_fetch_and, prif_atomic_fetch_or, prif_atomic_fetch_xor, prif_atomic_define, prif_atomic_ref
  implicit none
end module prif
