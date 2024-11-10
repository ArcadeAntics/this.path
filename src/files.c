#include "thispathdefn.h"





int drive_width_windows(const char *s, int nchar)
{
    /* there are three types of absolute paths on windows
     *
     * there are those starting with d:/ or some other letter
     * we call these drives
     *
     * there are those starting with //host/share
     * we call these network shares (for accessing remote data)
     *
     * and specifically for R, there are those starting with ~
     * for functions such as dirname() and basename(), we would normally expand
     * those filenames with R_ExpandFileName(), but for path.join() we don't
     * want to modify the inputs
     *
     * unlike unix-alikes, a path starting with / is NOT an absolute path.
     * try this for yourself:

setwd("C:/")
normalizePath("/path/to/file")

setwd("D:/")
normalizePath("/path/to/file")

     * which should do something like this:

> setwd("C:/")
> normalizePath("/path/to/file")
[1] "C:\\path\\to\\file"
>
> setwd("D:/")
> normalizePath("/path/to/file")
[1] "D:\\path\\to\\file"
>

     * this function will return the width of the drive specification of the
     * path, or 0 if no drive specification exists or is invalid
     *
     * when I say drive specification, I am referring to all three of the above
     * d:           is a drive specification
     * //host/share is a drive specification
     * ~            is a drive specification
     *
     * as a short-form, you can call it a drivespec
     *
     * the path specification of a path is the portion of the string
     * immediately following a possible drivespec
     *
     * you can call it a pathspec for short
     *
     *
     *
     * Arguments:
     *
     * s
     *
     *     the string in which we are looking for a drivespec
     *
     * nchar
     *
     *     the length of the string. this argument exists purely so that you
     *     don't have to calculate strlen(s) twice (assuming you're using nchar
     *     somewhere else in your program)
     */


    if (nchar <= 0) return 0;


    /* s starts with d: or similar */
    if (nchar >= 2 && *s <= 0x7f && *(s + 1) == ':') return 2;


    if (*s == '~' &&             /* s starts with ~ */
        (
            nchar == 1       ||  /* s is exactly ~   */
            *(s + 1) == '/'  ||  /* s starts with ~/ */
            *(s + 1) == '\\'     /* s starts with ~\ */
        ))
    {
        return 1;
    }


    /* 5 characters is the minimum required for a network share
     * the two slashes at the start, at least one for the host name,
     * a slash between the host name and share name,
     * and at least one for the share name
     */
    if (nchar < 5) return 0;


    const char *p = s;
    if (*p != '/' && *p != '\\') return 0;  /* first character must be / or \ */
    p++;
    if (*p != '/' && *p != '\\') return 0;  /* second character must be / or \ */
    p++;


    /* third character must NOT be slash nor backslash
     * this is the start of the host name of the network share
     */
    if (*p == '/' || *p == '\\') return 0;


    /* look for path separators */
    const char *slash     = strchr(p, '/'),
               *backslash = strchr(p, '\\');
    if (slash) {  /* slash was found */
        if (backslash) {  /* backslash was also found */
            if (slash < backslash)  /* slash found before backslash */
                p = slash;
            else p = backslash;  /* backslash found before slash */
        }
        else p = slash;  /* backslash was not found */
    }
    else {  /* slash was not found */
        if (backslash)  /* backslash was found */
            p = backslash;
        else return 0;
    }
    p++;


    /* look for a non-slash and non-backslash character
     * this is the start of the share name of the network path
     */
    int found_share_name = 0;


    /* the condition *p can be also written as *p != '\0',
     * which is to say that p does NOT point to the end of the string
     * using *p is simply shorter
     */
    for (; *p; p++) {
        if (*p != '/' && *p != '\\') {
            found_share_name = 1;
            break;
        }
    }
    if (!found_share_name) return 0;


    /* again, look for a slash or backslash */
    slash     = strchr(p, '/');
    backslash = strchr(p, '\\');
    if (slash) {  /* slash was found */
        if (backslash) {  /* backslash was also found */
            if (slash < backslash)  /* slash found before backslash */
                return slash - s;
            else return backslash - s;  /* backslash found before slash */
        }
        else return slash - s;  /* backslash was not found */
    }
    else {  /* slash was not found */
        if (backslash)  /* backslash was found */
            return backslash - s;
        else return nchar;
    }
}


int drive_width_unix(const char *s, int nchar)
{
    /* similar to the above drive_width_windows() but specifically for
     * unix-alikes where a drivespec only really makes sense in terms of a
     * network share */


    /* 5 characters is the minimum required for a network share
     * the two slashes at the start, at least one for the host name,
     * a slash between the host name and share name,
     * and at least one for the share name
     */
    if (nchar < 5) return 0;


    const char *p = s;
    if (*p != '/') return 0;  /* first character must be / */
    p++;
    if (*p != '/') return 0;  /* second character must be / */
    p++;


    /* third character must NOT be /
     * this is the start of the host name of the network share
     */
    if (*p == '/') return 0;


    /* look for a slash */
    p = strchr(p, '/');
    if (!p) return 0;  /* slash was not found */
    p++;


    /* look for a non-slash character
     * this is the start of the share name of the network share
     */
    int found_share_name = 0;


    for (; *p; p++) {
        if (*p != '/') {
            found_share_name = 1;
            break;
        }
    }
    if (!found_share_name) return 0;


    /* again, look for a slash */
    p = strchr(p, '/');
    if (p)  /* slash was found */
        return p - s;
    else return nchar;
}


int is_abs_path_windows(const char *s)
{
    int nchar = strlen(s);
    if (nchar <= 0) return 0;


    /* s starts with d:/ or similar */
    if (nchar >= 3 && *s <= 0x7f && *(s + 1) == ':' &&
        (*(s + 2) == '/' || *(s + 2) == '\\'))
    {
        return 1;
    }


    if (*s == '~' &&             /* s starts with ~ */
        (
            nchar == 1       ||  /* s is exactly ~   */
            *(s + 1) == '/'  ||  /* s starts with ~/ */
            *(s + 1) == '\\'     /* s starts with ~\ */
        ))
    {
        return 1;
    }


    /* 5 characters is the minimum required for a network share
     * the two slashes at the start, at least one for the host name,
     * a slash between the host name and share name,
     * and at least one for the share name
     */
    if (nchar < 5) return 0;


    const char *p = s;
    if (*p != '/' && *p != '\\') return 0;  /* first character must be / or \ */
    p++;
    if (*p != '/' && *p != '\\') return 0;  /* second character must be / or \ */
    p++;


    /* third character must NOT be slash nor backslash
     * this is the start of the host name of the network share
     */
    if (*p == '/' || *p == '\\') return 0;


    /* look for path separators */
    const char *slash     = strchr(p, '/'),
               *backslash = strchr(p, '\\');
    if (slash) {  /* slash was found */
        if (backslash) {  /* backslash was also found */
            if (slash < backslash)  /* slash found before backslash */
                p = slash;
            else p = backslash;  /* backslash found before slash */
        }
        else p = slash;  /* backslash was not found */
    }
    else {  /* slash was not found */
        if (backslash)  /* backslash was found */
            p = backslash;
        else return 0;
    }
    p++;


    /* the condition *p can be also written as *p != '\0',
     * which is to say that p does NOT point to the end of the string
     * using *p is simply shorter
     */
    for (; *p; p++) {
        if (*p != '/' && *p != '\\') {
            /* this means we found a share name, so the path is absolute */
            return 1;
        }
    }
    return 0;
}


int is_abs_path_unix(const char *s)
{
    int nchar = strlen(s);
    if (nchar <= 0) return 0;


    /* tests for absolute paths:
     * if it starts with /
     * if it is equal to ~
     * if it starts with ~/
     */
    if (*s == '/') return 1;  /* path starts with / */
    if (*s == '~') {
        if (nchar == 1) return 1;  /* path equals ~ */
        if (*(s + 1) == '/') return 1;  /* path starts with ~/ */
        if (*R_ExpandFileName(s) == '/') return 1;  /* path expands to an absolute path, e.g. ~iris/foo might expand to /home/iris/foo */
    }


    return 0;
}





#if defined(_WIN32)
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
        Rf_error(_("a character vector argument expected"));
    int n = LENGTH(file);
    SEXP value = Rf_allocVector(LGLSXP, n);
    Rf_protect(value);
    int *ivalue = INTEGER(value);
    for (int i = 0; i < n; i++)
        ivalue[i] = is_clipboard(R_CHAR(STRING_ELT(file, i)));
    Rf_unprotect(1);
    return value;
}





static R_INLINE
SEXP isabspath(int windows, SEXP args)
{
    SEXP path = CAR(args);
    int n;
    if (TYPEOF(path) != STRSXP)
        Rf_error(_("a character vector argument expected"));
    SEXP value = Rf_allocVector(LGLSXP, n = LENGTH(path));
    Rf_protect(value);
    int *lvalue = LOGICAL(value);
    if (windows) {
        for (int i = 0; i < n; i++) {
            lvalue[i] = is_abs_path_windows(R_CHAR(STRING_ELT(path, i)));
        }
    } else {
        for (int i = 0; i < n; i++) {
            lvalue[i] = is_abs_path_unix(R_CHAR(STRING_ELT(path, i)));
        }
    }
    Rf_unprotect(1);
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
#if defined(_WIN32)
    return isabspath(TRUE, args);
#else
    return isabspath(FALSE, args);
#endif
}


SEXP do_fixslash do_formals
{
    do_start_no_call_op_rho("fixslash", 1);


    SEXP file = CAR(args);
    if (TYPEOF(file) != STRSXP)
        Rf_error(_("a character vector argument expected"));


    R_xlen_t n = XLENGTH(file);
    SEXP value = Rf_allocVector(STRSXP, n);
    Rf_protect(value);
    for (int i = 0; i < n; i++) {
        SEXP cs = STRING_ELT(file, i);
#if defined(_WIN32)
        const char *s = R_CHAR(cs);
        int nchar = (int) strlen(s);
        char buf[nchar + 1];
        char *p = buf;
        memcpy(p, s, nchar);
        p[nchar] = '\0';
        for (; *p; p++) if (*p == '\\') *p = '/';
        /* preserve network shares */
        if (buf[0] == '/' && buf[1] == '/') buf[0] = buf[1] = '\\';
        SET_STRING_ELT(value, i, Rf_mkCharLenCE(buf, nchar, Rf_getCharCE(cs)));
#else
        SET_STRING_ELT(value, i, cs);
#endif
    }
    Rf_unprotect(1);
    return value;
}


SEXP do_fixbackslash do_formals
{
    do_start_no_call_op_rho("fixbackslash", 1);


    SEXP file = CAR(args);
    if (TYPEOF(file) != STRSXP)
        Rf_error(_("a character vector argument expected"));


    R_xlen_t n = XLENGTH(file);
    SEXP value = Rf_allocVector(STRSXP, n);
    Rf_protect(value);
    for (int i = 0; i < n; i++) {
        SEXP cs = STRING_ELT(file, i);
#if defined(_WIN32)
        const char *s = R_CHAR(cs);
        int nchar = (int) strlen(s);
        char buf[nchar + 1];
        char *p = buf;
        memcpy(p, s, nchar);
        p[nchar] = '\0';
        for (; *p; p++) if (*p == '/') *p = '\\';
        SET_STRING_ELT(value, i, Rf_mkCharLenCE(buf, nchar, Rf_getCharCE(cs)));
#else
        SET_STRING_ELT(value, i, cs);
#endif
    }
    Rf_unprotect(1);
    return value;
}


SEXP do_file_URL_path do_formals
{
    do_start_no_call_op_rho("file_URL_path", 1);


    SEXP path = CAR(args);
    if (TYPEOF(path) != STRSXP)
        Rf_error(_("a character vector argument expected"));


    R_xlen_t n = XLENGTH(path);
    SEXP value = Rf_allocVector(STRSXP, n);
    Rf_protect(value);
    for (int i = 0; i < n; i++) {
        SEXP cs = STRING_ELT(path, i);
        const char *s = R_CHAR(cs);
        if (strncmp(s, "file://", 7) == 0) {
            int nh = 7;
#if defined(_WIN32)
            if (strlen(s) > 9 && s[7] == '/' && s[9] == ':') nh = 8;
#endif
            SET_STRING_ELT(value, i, Rf_mkCharCE(s + nh, Rf_getCharCE(cs)));
        }
        else
            SET_STRING_ELT(value, i, cs);
    }
    Rf_unprotect(1);
    return value;
}
