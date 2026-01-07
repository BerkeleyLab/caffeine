! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#ifndef CAF_INCLUDED_LANGUAGE_SUPPORT
#define CAF_INCLUDED_LANGUAGE_SUPPORT

#ifdef __GNUC__
#  define HAVE_GCC_VERSION (__GNUC__ * 10000 + __GNUC_MINOR__ * 100 + __GNUC_PATCHLEVEL__)
#endif

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
#if defined(_CRAYFTN) || defined(__INTEL_COMPILER) || defined(NAGFOR) || defined(__flang__) || (HAVE_GCC_VERSION > 140200)
#  define HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY 1
#else
#  define HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY 0
#endif  
#endif

! ISO_FORTRAN_ENV constant value control:
! The following knobs influence Caffeine's choice of value for the named constants 
! specified by PRIF for ISO_FORTRAN_ENV:
! * CAF_IMPORT_{ATOMIC,STAT,TEAM}_CONSTANTS: Import PRIF constant values 
!   of the given category from the compiler's ISO_FORTRAN_ENV
! * CAF_IMPORT_CONSTANTS: Provides a default value for each category-specific knob above, 
!   which take precedence
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

! PRIF specification version override and control
! By default, Caffeine provides the latest ratified version of the PRIF specification.
! Clients can optionally define one of the FORCE_* macros below to force compliance
! with a different revision of the PRIF specification. These override settings are
! NOT officially supported and may be removed at any time without notice.
#define   CAF_PRIF_VERSION_MAJOR 0
#if   FORCE_PRIF_0_5
#  define CAF_PRIF_VERSION_MINOR 5
#elif FORCE_PRIF_0_6
#  define CAF_PRIF_VERSION_MINOR 6
#elif FORCE_PRIF_0_7
#  define CAF_PRIF_VERSION_MINOR 7
#elif FORCE_PRIF_0_8
#  define CAF_PRIF_VERSION_MINOR 8
#else
#  define CAF_PRIF_VERSION_MINOR 7
#endif
#define CAF_PRIF_VERSION (100 * CAF_PRIF_VERSION_MAJOR + CAF_PRIF_VERSION_MINOR)

#endif
