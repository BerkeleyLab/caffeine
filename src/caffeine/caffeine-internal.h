# /* Copyright (c), The Regents of the University of California */
# /* Terms of use are as specified in LICENSE.txt */

# /* NOTE: this is a dual-language header file, */
# /*       and should ONLY contain portable preprocessor directives. */

# /* define some macro portability helpers */
#if defined(__GFORTRAN__) || defined(_CRAYFTN) || defined(NAGFOR)
#  define CAF_CONCAT2(x,y)     x/**/y
#  define CAF_CONCAT3(x,y,z)   x/**/y/**/z
#  define CAF_STRINGIFY_HELPER(x) "x"
#else
#  define CAF_CONCAT2(x,y)   x##y
#  define CAF_CONCAT3(x,y,z) x##y##z
#  define CAF_STRINGIFY_HELPER(x) #x
#endif
#define CAF_STRINGIFY(x) CAF_STRINGIFY_HELPER(x)

# /* AMO support defines */

#define CAF_OP_GET	0
#define CAF_OP_SET	1
#define CAF_OP_ADD	2
#define CAF_OP_AND	3
#define CAF_OP_OR	4
#define CAF_OP_XOR	5
#define CAF_OP_FADD	6
#define CAF_OP_FAND	7
#define CAF_OP_FOR	8
#define CAF_OP_FXOR	9
#define CAF_OP_FCAS	10

