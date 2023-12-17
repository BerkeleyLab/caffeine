! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(coarray_queries_m) coarray_queries_s

  implicit none

contains

  module procedure prif_lcobound_with_dim
  end procedure

  module procedure prif_lcobound_no_dim
  end procedure

  module procedure prif_ucobound_with_dim
  end procedure

  module procedure prif_ucobound_no_dim
  end procedure prif_ucobound_no_dim

  module procedure prif_coshape
  end procedure

  module procedure prif_image_index
  end procedure

end submodule coarray_queries_s
