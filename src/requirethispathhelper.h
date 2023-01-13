#include "Rversion.h"


#if defined(R_VERSION) && R_VERSION >= R_Version(3, 3, 0)
#define requirethispathhelper                                  \
    do {                                                       \
        SEXP expr10 = lang1(require_this_path_helperSymbol);   \
        PROTECT(expr10);                                       \
        eval(expr10, mynamespace);                             \
        UNPROTECT(1);                                          \
    } while (0)
#else
#define requirethispathhelper do {} while (0)
#endif
