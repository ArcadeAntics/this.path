/*
this.path : Get Executing Script's Path
Copyright (C) 2024-2026   Iris Simmons
 */


#include "thispathdefn.h"





int _drive_width_windows(const char *s, int consider_tilde)
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
     */


    if (*s == '\0') return 0;


    /* s starts with d: or similar */
    if (*s <= 0x7f && *(s + 1) == ':') return 2;


    if (consider_tilde &&
        *s == '~' &&             /* s starts with ~ */
        (
            *(s + 1) == '\0' ||  /* s is exactly ~   */
            *(s + 1) == '/'  ||  /* s starts with ~/ */
            *(s + 1) == '\\'     /* s starts with ~\ */
        ))
    {
        return 1;
    }


    const char *p = s;
    if (*p != '/' && *p != '\\') return 0;  /* first character must be / or \ */
    p++;
    if (*p != '/' && *p != '\\') return 0;  /* second character must be / or \ */
    p++;


    /* third character must NOT be slash nor backslash
     * this is the start of the host name of the network share
     */
    if (*p == '/' || *p == '\\') return 0;


    int is_host_question_mark = 0;
    int is_share_UNC = 0;
    const char *tmp;


    tmp = p;
    /* look for a path separator */
    for (; *p; p++) if (*p == '/' || *p == '\\') break;
    if (*p); else return 0; /* did not find a path separator */
    if (p - tmp == 1 && *tmp == '?') is_host_question_mark = 1;


    /* look for non-(path separator) (the share name of the network path) */
    for (; *p; p++) if (*p != '/' && *p != '\\') break;
    if (*p); else return 0;


    tmp = p;
    /* look for a path separator */
    for (; *p; p++) if (*p == '/' || *p == '\\') break;
    if (
        is_host_question_mark &&
        p - tmp == 3 &&
        (*tmp       == 'U' || *tmp       == 'u') &&
        (*(tmp + 1) == 'N' || *(tmp + 1) == 'n') &&
        (*(tmp + 2) == 'C' || *(tmp + 2) == 'c')
    ) is_share_UNC = 1;
    /* if the path is not of the form "//?/UNC", return */
    if (!is_share_UNC) return p - s;


    /* look for non-(path separator) (the host name of the network path) */
    for (; *p; p++) if (*p != '/' && *p != '\\') break;
    if (*p); else return 0;


    /* look for a path separator */
    for (; *p; p++) if (*p == '/' || *p == '\\') break;
    if (*p); else return 0; /* did not find a path separator */


    /* look for non-(path separator) (the share name of the network path) */
    for (; *p; p++) if (*p != '/' && *p != '\\') break;
    if (*p); else return 0;


    /* look for a path separator */
    for (; *p; p++) if (*p == '/' || *p == '\\') break;
    return p - s;
}


int drive_width_windows(const char *s)
{
    return _drive_width_windows(s, 1);
}


int drive_width_no_tilde_windows(const char *s)
{
    return _drive_width_windows(s, 0);
}


int drive_width_unix(const char *s)
{
    /* similar to the above drive_width_windows() but specifically for
     * unix-alikes where a drivespec only really makes sense in terms of a
     * network share */


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


    /* look for non-(path separator) (the share name of the network path) */
    for (; *p; p++) if (*p != '/') break;
    if (*p); else return 0;


    p = strchr(p, '/');
    if (p) return p - s; else return strlen(s);
}


int is_abs_path_windows(const char *s)
{
    int drivewidth = _drive_width_windows(s, /* consider_tilde */ 1);


    if (drivewidth <= 0) return 0;


    /* s starts with d: or similar */
    if (drivewidth == 2) {
        /* s starts with d:/ or similar */
        if (*(s + 2) == '/' || *(s + 2) == '\\')
            return 1;
        else return 0;
    }


    return 1;
}


int is_abs_path_unix(const char *s)
{
    if (*s == '\0') return 0;


    /* tests for absolute paths:
     * if it starts with /
     * if it is equal to ~
     * if it starts with ~/
     */
    if (*s == '/') return 1;  /* path starts with / */
    if (*s == '~') {
        if (*(s + 1) == '\0') return 1;  /* path equals ~ */
        if (*(s + 1) == '/' ) return 1;  /* path starts with ~/ */
        if (*R_ExpandFileName(s) == '/') return 1;  /* path expands to an absolute path, e.g. ~iris/foo might expand to /home/iris/foo */
    }


    return 0;
}





int is_clipboard_windows(const char *url)
{
    return strcmp (url, "clipboard"     ) == 0 ||
           strncmp(url, "clipboard-", 10) == 0;
}


int is_clipboard_unix(const char *url)
{
    return strcmp(url, "clipboard"    ) == 0 ||
           strcmp(url, "X11_primary"  ) == 0 ||
           strcmp(url, "X11_secondary") == 0 ||
           strcmp(url, "X11_clipboard") == 0;
}


int is_clipboard(const char *url)
{
#if defined(_WIN32)
    return is_clipboard_windows(url);
#else
    return is_clipboard_unix(url);
#endif
}
const char *must_not_be_clipboard_message =
#if defined(_WIN32)
    "must not be \"clipboard\" nor start with \"clipboard-\""
#else
    "must not be \"clipboard\", \"X11_primary\", \"X11_secondary\", nor \"X11_clipboard\""
#endif
    ;


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





static R_INLINE
SEXP _do_is_clipboard(int windows, SEXP args)
{
    SEXP file = CAR(args);
    if (TYPEOF(file) != STRSXP)
        Rf_error(_("a character vector argument expected"));
    R_xlen_t n = Rf_xlength(file);
    SEXP value = Rf_allocVector(LGLSXP, n);
    Rf_protect(value);
    int *lvalue = LOGICAL(value);
    if (windows) {
        for (R_xlen_t i = 0; i < n; i++)
            lvalue[i] = is_clipboard_windows(R_CHAR(STRING_ELT(file, i)));
    } else {
        for (R_xlen_t i = 0; i < n; i++)
            lvalue[i] = is_clipboard_unix(R_CHAR(STRING_ELT(file, i)));
    }
    Rf_unprotect(1);
    return value;
}


SEXP do_is_clipboard_windows do_formals
{
    do_start_no_call_op_rho("is_clipboard_windows", 1);
    return _do_is_clipboard(TRUE, args);
}


SEXP do_is_clipboard_unix do_formals
{
    do_start_no_call_op_rho("is_clipboard_unix", 1);
    return _do_is_clipboard(FALSE, args);
}


SEXP do_is_clipboard do_formals
{
    do_start_no_call_op_rho("is_clipboard", 1);
#if defined(_WIN32)
    return _do_is_clipboard(TRUE, args);
#else
    return _do_is_clipboard(FALSE, args);
#endif
}





static R_INLINE
SEXP _do_is_abs_path(int windows, SEXP args)
{
    SEXP path = CAR(args);
    if (TYPEOF(path) != STRSXP)
        Rf_error(_("a character vector argument expected"));
    R_xlen_t n = Rf_xlength(path);
    SEXP value = Rf_allocVector(LGLSXP, n);
    Rf_protect(value);
    int *lvalue = LOGICAL(value);
    if (windows) {
        for (R_xlen_t i = 0; i < n; i++)
            lvalue[i] = is_abs_path_windows(R_CHAR(STRING_ELT(path, i)));
    } else {
        for (R_xlen_t i = 0; i < n; i++)
            lvalue[i] = is_abs_path_unix(R_CHAR(STRING_ELT(path, i)));
    }
    Rf_unprotect(1);
    return value;
}


SEXP do_is_abs_path_windows do_formals
{
    do_start_no_call_op_rho("is_abs_path_windows", 1);
    return _do_is_abs_path(TRUE, args);
}


SEXP do_is_abs_path_unix do_formals
{
    do_start_no_call_op_rho("is_abs_path_unix", 1);
    return _do_is_abs_path(FALSE, args);
}


SEXP do_is_abs_path do_formals
{
    do_start_no_call_op_rho("is_abs_path", 1);
#if defined(_WIN32)
    return _do_is_abs_path(TRUE, args);
#else
    return _do_is_abs_path(FALSE, args);
#endif
}


SEXP do_fixslash do_formals
{
    do_start_no_call_op_rho("fixslash", 1);


    SEXP file = CAR(args);
    if (TYPEOF(file) != STRSXP)
        Rf_error(_("a character vector argument expected"));


    R_xlen_t n = Rf_xlength(file);
    SEXP value = Rf_allocVector(STRSXP, n);
    Rf_protect(value);
    for (R_xlen_t i = 0; i < n; i++) {
        SEXP cs = STRING_ELT(file, i);
#if defined(_WIN32)
        const char *s = R_CHAR(cs);
        char buf[strlen(s) + 1];
        strcpy(buf, s);
        for (char *p = buf; *p; p++) if (*p == '\\') *p = '/';
        /* preserve network shares */
        if (buf[0] == '/' && buf[1] == '/') buf[0] = buf[1] = '\\';
        SET_STRING_ELT(value, i, Rf_mkCharCE(buf, Rf_getCharCE(cs)));
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


    R_xlen_t n = Rf_xlength(file);
    SEXP value = Rf_allocVector(STRSXP, n);
    Rf_protect(value);
    for (R_xlen_t i = 0; i < n; i++) {
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


    R_xlen_t n = Rf_xlength(path);
    SEXP value = Rf_allocVector(STRSXP, n);
    Rf_protect(value);
    for (R_xlen_t i = 0; i < n; i++) {
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
