#include "drivewidth.h"
#include "thispathdefn.h"


#ifdef _WIN32
int is_clipboard(const char *url)
{
    return strcmp (url, "clipboard"     ) == 0 ||
           strncmp(url, "clipboard-", 10) == 0;
}
const char *must_not_be_clipboard_message = "must not be \"clipboard\" nor start with \"clipboard-\"";
#else
int is_clipboard(const char *url)
{
    return strcmp(url, "clipboard"    ) == 0 ||
           strcmp(url, "X11_primary"  ) == 0 ||
           strcmp(url, "X11_secondary") == 0 ||
           strcmp(url, "X11_clipboard") == 0;
}
const char *must_not_be_clipboard_message = "must not be \"clipboard\", \"X11_primary\", \"X11_secondary\", nor \"X11_clipboard\"";
#endif


int is_file_uri(const char *url)
{
    return strncmp(url, "file://", 7) == 0;
}


int is_url(const char *url)
{
    if      (strncmp(url, "http://" , 7) == 0)
        return 7;
    else if (strncmp(url, "https://", 8) == 0)
        return 8;
    else if (strncmp(url, "ftp://"  , 6) == 0)
        return 6;
    else if (strncmp(url, "ftps://" , 7) == 0)
        return 7;
    else
        return 0;
}





SEXP do_is_clipboard do_formals
{
    do_start_no_call_op_rho("is_clipboard", 1);


    SEXP file = CAR(args);
    if (TYPEOF(file) != STRSXP)
        error(_("a character vector argument expected"));
    int n = LENGTH(file);
    SEXP value = allocVector(LGLSXP, n);
    PROTECT(value);
    int *ivalue = INTEGER(value);
    for (int i = 0; i < n; i++)
        ivalue[i] = is_clipboard(CHAR(STRING_ELT(file, i)));
    UNPROTECT(1);
    return value;
}





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


SEXP do_windows_is_abs_path do_formals
{
    do_start_no_call_op_rho("windows_is_abs_path", 1);
    return isabspath(TRUE, args);
}


SEXP do_unix_is_abs_path do_formals
{
    do_start_no_call_op_rho("unix_is_abs_path", 1);
    return isabspath(FALSE, args);
}


SEXP do_is_abs_path do_formals
{
    do_start_no_call_op_rho("is_abs_path", 1);
#ifdef _WIN32
    return isabspath(TRUE, args);
#else
    return isabspath(FALSE, args);
#endif
}
