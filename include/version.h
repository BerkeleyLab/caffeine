# /* Copyright (c), The Regents of the University of California */
# /* Terms of use are as specified in LICENSE.txt */

# /* NOTE: this is a dual-language header file, */
# /*       and should ONLY contain portable preprocessor directives. */

#ifndef CAF_INCLUDED_VERSION_H
#define CAF_INCLUDED_VERSION_H

# /* Caffeine software package versioning */
#define CAF_RELEASE_VERSION_MAJOR 0
#define CAF_RELEASE_VERSION_MINOR 7
#define CAF_RELEASE_VERSION_PATCH 1
#define CAF_RELEASE_VERSION (1000*CAF_RELEASE_VERSION_MAJOR + 100*CAF_RELEASE_VERSION_MINOR + CAF_RELEASE_VERSION_PATCH)

#if 0
! PRIF specification version override and control
! By default, Caffeine provides the latest ratified version of the PRIF specification.
! Clients can optionally define one of the FORCE_* macros below to force compliance
! with a different revision of the PRIF specification. These override settings are
! NOT officially supported and may be removed at any time without notice.
#endif
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
