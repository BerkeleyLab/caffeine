! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(prif:prif_private_s) events_s

  implicit none

contains

  module procedure prif_event_post
  end procedure

  module procedure prif_event_wait
  end procedure

  module procedure prif_event_query
  end procedure

end submodule events_s
