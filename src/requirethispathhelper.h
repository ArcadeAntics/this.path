#include "Rversion.h"


#if defined(R_VERSION) && R_VERSION >= R_Version(3, 3, 0)
#define requirethispathhelper                                  \
    do {                                                       \
        SEXP expr10 = allocList(4);                            \
        PROTECT(expr10);                                       \
        SET_TYPEOF(expr10, LANGSXP);                           \
        SETCAR(expr10, requireNamespaceSymbol);                \
        SETCADR(expr10, mkString("this.path.helper"));         \
        SEXP expr11 = allocList(3);                            \
        PROTECT(expr11);                                       \
        SET_TYPEOF(expr11, LANGSXP);                           \
        SETCAR(expr11, cSymbol);                               \
        SETCADR(expr11, findVarInFrame(findVarInFrame(R_NamespaceRegistry, this_pathSymbol), libnameSymbol));\
        SETCADDR(expr11, lang1(_libPathsSymbol));              \
        SETCADDR(expr10, expr11);                              \
        SET_TAG(CDDDR(expr10), quietlySymbol);                 \
        SETCADDDR(expr10, ScalarLogical(TRUE));                \
        eval(expr10, R_BaseEnv);                               \
        UNPROTECT(2);                                          \
    } while (0)
#else
#define requirethispathhelper do {} while (0)
#endif
