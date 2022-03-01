! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module caffeine_m
  use program_termination_m, only : caf_stop, caf_error_stop
  use image_enumeration_m, only : caf_this_image, caf_num_images
  use collective_subroutines_m, only : caf_co_sum, caf_co_max, caf_co_min, caf_co_reduce, caf_co_broadcast
  use caffeinate_decaffeinate_m, only : caf_caffeinate, caf_decaffeinate
  use team_type_m, only: caf_form_team, caf_change_team, caf_end_team, team_type
  use synchronization_m, only : caf_sync_all
  implicit none
end module caffeine_m
