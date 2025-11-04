! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#ifndef HAVE_SELECTED_LOGICAL_KIND
  ! Define whether the compiler supports standard intrinsic function selected_logical_kind(), 
  ! a feature introduced in Fortran 2023 clause 16.9.182.
#if defined(_CRAYFTN) || defined(NAGFOR) || defined(__flang__)
#define HAVE_SELECTED_LOGICAL_KIND 1
#else
#define HAVE_SELECTED_LOGICAL_KIND 0
#endif  
#endif

#ifndef HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
  ! Define whether the compiler supports associating a procedure pointer dummy argument with an
  ! actual argument that is a valid target for the pointer dummy in a procedure assignment, a 
  ! feature introduced in Fortran 2008 and described in Fortran 2023 clause 15.5.2.10 paragraph 5.
#if defined(_CRAYFTN) || defined(__INTEL_COMPILER) || defined(NAGFOR) || defined(__flang__)
#define HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY 1
#else
#define HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY 0
#endif  
#endif

! ISO_FORTRAN_ENV constant value control:
! The following knobs influence Caffeine's choice of value for the named constants 
! specified by PRIF for ISO_FORTRAN_ENV. 
! CAF_IMPORT_{ATOMIC,STAT,TEAM}_CONSTANTS: Import PRIF constant values 
!     of the given category from compiler's ISO_FORTRAN_ENV
! CAF_IMPORT_CONSTANTS: Provides a default value each category-specific knob above, 
!     which take precedence
! NOTE: In all cases imported constant values are silently assumed to satisfy
! the requirements for the corresponding PRIF named constant.
! Additionally, the ATOMIC KIND constants are assumed to denote a 64-bit interoperable type
#ifndef CAF_IMPORT_CONSTANTS
#  if defined(__flang__)
#    define CAF_IMPORT_CONSTANTS 1
#  else
#    define CAF_IMPORT_CONSTANTS 0
#  endif
#endif
#ifndef CAF_IMPORT_ATOMIC_CONSTANTS
#define CAF_IMPORT_ATOMIC_CONSTANTS CAF_IMPORT_CONSTANTS
#endif
#ifndef CAF_IMPORT_STAT_CONSTANTS
#define CAF_IMPORT_STAT_CONSTANTS   CAF_IMPORT_CONSTANTS
#endif
#ifndef CAF_IMPORT_TEAM_CONSTANTS
#define CAF_IMPORT_TEAM_CONSTANTS   CAF_IMPORT_CONSTANTS
#endif

