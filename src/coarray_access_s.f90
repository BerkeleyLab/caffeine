! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
submodule(coarray_access_m) coarray_access_s

  implicit none

contains

  module procedure prif_put
  end procedure

  module procedure prif_put_strided
  end procedure

  module procedure prif_put_raw
  end procedure

  module procedure prif_put_raw_strided
  end procedure

  module procedure prif_get
  end procedure

  module procedure prif_get_raw
  end procedure

  module procedure prif_get_raw_strided
  end procedure

end submodule coarray_access_s
