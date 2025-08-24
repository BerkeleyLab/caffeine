#include "language-support.F90"
implicit none

#if (GCC_VERSION > 140200)
  print *,"(GCC_VERSION > 140200)"
#else
  print *,"! (GCC_VERSION > 140200)"
#endif

end
