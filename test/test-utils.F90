#ifndef CPP_STRINGIFY_SOURCE
# if defined(__GFORTRAN__) || defined(_CRAYFTN) || defined(NAGFOR) || defined(__LFORTRAN__)
#  define CPP_STRINGIFY_SOURCE(x) "x"
# else
#  define CPP_STRINGIFY_SOURCE(x) #x
# endif
#endif

#ifndef  CPP_LINE_STRING
# if defined(__GFORTRAN__)
    ! work-around Gfortran's defective preprocessor
#   define CPP_LINE_STRING string_t(__LINE__)
# else
#   define CPP_LINE_STRING_HELPER(n) CPP_STRINGIFY_SOURCE(n)
#   define CPP_LINE_STRING CPP_LINE_STRING_HELPER(__LINE__)
# endif
#endif

#define ALSO(exp) ALSO2(exp, "expression: (" // CPP_STRINGIFY_SOURCE(exp) // ")")
#define ALSO2(exp,desc) diag = diag .also. (exp) // NEW_LINE(' ') // \
                        __FILE__ // ":" // CPP_LINE_STRING // ": FAILED: " // desc

