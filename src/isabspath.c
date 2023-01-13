#include <R.h>
#include <Rinternals.h>


#include "drivewidth.h"
#include "translations.h"


#define isabspath(windows)                                     \
{                                                              \
    SEXP path = CADR(args);                                    \
    int n;                                                     \
    if (TYPEOF(path) != STRSXP)                                \
        error(_("a character vector argument expected"));      \
    SEXP value = allocVector(LGLSXP, n = LENGTH(path));        \
    PROTECT(value);                                            \
    int *lvalue = LOGICAL(value);                              \
    if (windows) {                                             \
        for (int i = 0; i < n; i++) {                          \
            lvalue[i] = is_abs_path_windows(CHAR(STRING_ELT(path, i)));\
        }                                                      \
    } else {                                                   \
        for (int i = 0; i < n; i++) {                          \
            lvalue[i] = is_abs_path_unix(CHAR(STRING_ELT(path, i)));\
        }                                                      \
    }                                                          \
    UNPROTECT(1);                                              \
    return value;                                              \
}


SEXP do_windowsisabspath(SEXP call, SEXP op, SEXP args, SEXP rho) isabspath(1)


SEXP do_unixisabspath(SEXP call, SEXP op, SEXP args, SEXP rho) isabspath(0)


#undef isabspath


SEXP do_isabspath(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef _WIN32
    return do_windowsisabspath(call, op, args, rho);
#else
    return do_unixisabspath(call, op, args, rho);
#endif
}
