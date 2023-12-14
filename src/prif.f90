! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module prif
  use program_startup_m, only : prif_init
  use program_termination_m, only : prif_stop, prif_error_stop
  use allocation_m, only: prif_coarray_handle, prif_allocate
  use image_enumeration_m, only : prif_this_image, prif_num_images
  use collective_subroutines_m, only : prif_co_sum, prif_co_max, prif_co_min, prif_co_reduce, prif_co_broadcast
  use team_type_m, only: prif_form_team, prif_change_team, prif_end_team, prif_team_type
  use synchronization_m, only : prif_sync_all
  implicit none
end module prif