#include <R.h>
#include <Rinternals.h>


#include "backports.h"
#include "drivewidth.h"
#include "translations.h"


static R_INLINE
SEXP isabspath(int windows, SEXP args)
{
    SEXP path = CAR(args);
    int n;
    if (TYPEOF(path) != STRSXP)
        error(_("a character vector argument expected"));
    SEXP value = allocVector(LGLSXP, n = LENGTH(path));
    PROTECT(value);
    int *lvalue = LOGICAL(value);
    if (windows) {
        for (int i = 0; i < n; i++) {
            lvalue[i] = is_abs_path_windows(CHAR(STRING_ELT(path, i)));
        }
    } else {
        for (int i = 0; i < n; i++) {
            lvalue[i] = is_abs_path_unix(CHAR(STRING_ELT(path, i)));
        }
    }
    UNPROTECT(1);
    return value;
}


SEXP do_windowsisabspath do_formals
{
    do_start_no_call_op_rho("windowsisabspath", 1);
    return isabspath(TRUE, args);
}


SEXP do_unixisabspath do_formals
{
    do_start_no_call_op_rho("unixisabspath", 1);
    return isabspath(FALSE, args);
}


SEXP do_isabspath do_formals
{
    do_start_no_call_op_rho("isabspath", 1);
#ifdef _WIN32
    return isabspath(TRUE, args);
#else
    return isabspath(FALSE, args);
#endif
}
