#include <R.h>
#include <Rinternals.h>


#include "drivewidth.h"
#include "thispathbackports.h"
#include "translations.h"


#define do_isabspath_body(windows)                             \
    SEXP path = CAR(args);                                     \
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
    return value


SEXP do_windowsisabspath do_formals
{
    do_start("windowsisabspath", 1);
    do_isabspath_body(TRUE);
}


SEXP do_unixisabspath do_formals
{
    do_start("unixisabspath", 1);
    do_isabspath_body(FALSE);
}


SEXP do_isabspath do_formals
{
    do_start("isabspath", 1);
#ifdef _WIN32
    do_isabspath_body(TRUE);
#else
    do_isabspath_body(FALSE);
#endif
}


#undef do_isabspath_body
