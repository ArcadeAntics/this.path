#include "Rversion.h"


#if defined(R_VERSION) && R_VERSION >= R_Version(3, 3, 0)
#define requirethispathhelper                                  \
    do {                                                       \
        SEXP expr10 = lang4(                                   \
            requireNamespaceSymbol,                            \
            mkString("this.path.helper"),                      \
            lang3(cSymbol, findVarInFrame(findVarInFrame(R_NamespaceRegistry, this_pathSymbol), libnameSymbol), lang1(_libPathsSymbol)),\
            ScalarLogical(TRUE)                                \
        );                                                     \
        SET_TAG(CDDDR(expr10), quietlySymbol);                 \
        PROTECT(expr10);                                       \
        eval(expr10, R_BaseEnv);                               \
        UNPROTECT(1);                                          \
    } while (0)
#else
#define requirethispathhelper do {} while (0)
#endif
