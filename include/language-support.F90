! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#ifndef HAVE_SELECTED_LOGICAL_KIND
  ! Define whether the compiler supports standard intrinsic function selected_logical_kind(), 
  ! a feature introduced in Fortran 2023 clause 16.9.182.
  #if defined(_CRAYFTN) || defined(NAGFOR) || defined(__flang__)
    #define HAVE_SELECTED_LOGICAL_KIND 
  #endif  
#endif

#ifndef HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  ! Define whether the compiler supports associating a procedure pointer dummy argument with an
  ! actual argument that is a valid target for the pointer dummy in a procedure assignment, a 
  ! feature introduced in Fortran 2008 and described in Fortran 2023 clause 15.5.2.10 paragraph 5.
  #if defined(_CRAYFTN) || defined(__INTEL_COMPILER) || defined(NAGFOR) || defined(__flang__)
    #define HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  #endif  
#endif

#ifndef HAVE_MULTI_IMAGE_SUPPORT
  ! Define whether the compiler supports the parallel features associated with multi-image execution,
  ! a feature set introduced in Fortran 2008 and commonly referred to as Coarray Fortran.
  #if defined(__GFORTRAN__) || defined(_CRAYFTN) || defined(__INTEL_COMPILER) || defined(NAGFOR)
    #define HAVE_MULTI_IMAGE_SUPPORT
  #endif
#endif
