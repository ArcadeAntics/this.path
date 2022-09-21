#include <R.h>
#include <Rinternals.h>


extern int get_drive_width(const char *s, int nchar);
extern int get_drive_width_unix(const char *s, int nchar);





SEXP do_windowsbasename2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nprotect = 0;


    SEXP path = CADR(args);
    const char *ptr;
    char *buf, *p, *ptr_slash, *ptr_backslash;
    int n, i, nchar, drivewidth;


    if (TYPEOF(path) != STRSXP)
        error("a character vector argument expected");
    SEXP value = PROTECT(allocVector(STRSXP, n = LENGTH(path))); nprotect++;
    for (i = 0; i < n; i++) {
        if (STRING_ELT(path, i) == NA_STRING)
            SET_STRING_ELT(value, i, NA_STRING);
        else {
            ptr = R_ExpandFileName(translateCharUTF8(STRING_ELT(path, i)));
            nchar = strlen(ptr);
            if (nchar == 0) {
                SET_STRING_ELT(value, i, mkChar(""));
                continue;
            }
            drivewidth = get_drive_width(ptr, nchar);
            nchar -= drivewidth;  /* number of characters EXCLUDING the drive */
            if (nchar == 0) {
                SET_STRING_ELT(value, i, mkChar(""));
                continue;
            }
            char _buf[nchar];  /* allocate a buffer to hold the basename */
            buf = _buf;
            strcpy(buf, ptr + drivewidth);  /* copy the path specification from ptr to buf */
            /* point to the last character of the buf */
            p = buf + (nchar - 1);
            /* remove the trailing / and \ */
            while (p >= buf &&
                   (
                       *p == '/' ||
                       *p == '\\'
                   ))
            {
                *(p--) = '\0';
            }


            /* if the path specification was only comprised of / and \
             * then the basename is non-existent, return empty string */
            if (!strlen(p)) {
                SET_STRING_ELT(value, i, mkChar(""));
                continue;
            }


            /* find the last slash/backslash */
            ptr_slash     = strrchr(buf, '/');
            ptr_backslash = strrchr(buf, '\\');


            if (ptr_slash) {  /* slash was found */
                if (ptr_backslash) {  /* backslash was also found */
                    if (ptr_slash < ptr_backslash)  /* slash found before backslash */
                        buf = ptr_backslash + 1;
                    else buf = ptr_slash + 1;  /* backslash found before slash */
                }
                else buf = ptr_slash + 1;  /* backslash was not found */
            }
            else {  /* slash was not found */
                if (ptr_backslash)  /* backslash was found */
                    buf = ptr_backslash + 1;
            }
            SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
        }
    }


    UNPROTECT(nprotect);
    return value;
}


SEXP do_unixbasename2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nprotect = 0;


    SEXP path = CADR(args);
    const char *ptr;
    char *buf, *p, *ptr_slash;
    int n, i, nchar, drivewidth;


    if (TYPEOF(path) != STRSXP)
        error("a character vector argument expected");
    SEXP value = PROTECT(allocVector(STRSXP, n = LENGTH(path))); nprotect++;
    for (i = 0; i < n; i++) {
        if (STRING_ELT(path, i) == NA_STRING)
            SET_STRING_ELT(value, i, NA_STRING);
        else {
            ptr = R_ExpandFileName(translateCharUTF8(STRING_ELT(path, i)));
            nchar = strlen(ptr);
            if (nchar == 0) {
                SET_STRING_ELT(value, i, mkChar(""));
                continue;
            }
            drivewidth = get_drive_width_unix(ptr, nchar);
            nchar -= drivewidth;
            if (nchar == 0) {
                SET_STRING_ELT(value, i, mkChar(""));
                continue;
            }
            char _buf[nchar];  /* allocate a buffer to hold the basename */
            buf = _buf;
            strcpy(buf, ptr + drivewidth);
            /* point to the last character of the buf */
            p = buf + (nchar - 1);
            /* remove the trailing / */
            while (p >= buf && *p == '/') {
                *(p--) = '\0';
            }


            /* if the path specification was only comprised of / and \
             * then the basename is non-existent, return empty string */
            if (!strlen(p)) {
                SET_STRING_ELT(value, i, mkChar(""));
                continue;
            }


            /* find the last slash */
            ptr_slash = strrchr(buf, '/');
            if (ptr_slash)
                buf = ptr_slash + 1;


            SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
        }
    }


    UNPROTECT(nprotect);
    return value;
}


SEXP do_basename2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int windows = asLogical(eval(install("os.windows"), rho));
    if (windows == NA_LOGICAL)
        error("invalid 'os.windows'; should never happen, please report!");
    if (windows)
        return do_windowsbasename2(call, op, args, rho);
    else return do_unixbasename2(call, op, args, rho);
}





SEXP do_windowsdirname2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nprotect = 0;


    SEXP path;
    int times;


    int nargs = length(args) - 1;
    if (nargs == 1) {
        path = CADR(args);
        if (TYPEOF(path) != STRSXP)
            error("a character vector argument expected");
        times = 0;
    }
    else if (nargs == 2) {
        path = CADR(args);
        if (TYPEOF(path) != STRSXP)
            error("a character vector argument expected");
        times = asInteger(CADDR(args));
        if (times == NA_INTEGER || times < 0)
            errorcall(call, "invalid second argument, must be coercible to non-negative integer");
    }
    else errorcall(call, "%d arguments passed to 'C_basename2' which requires 1 or 2 arguments", nargs);


    const char *ptr, *cbuf;
    char *buf, *p, *ptr_slash, *ptr_backslash;
    int n, i, j, nchar, drivewidth, skip;


    SEXP value = PROTECT(allocVector(STRSXP, n = LENGTH(path))); nprotect++;
    for (i = 0; i < n; i++) {
        if (STRING_ELT(path, i) == NA_STRING) {
            SET_STRING_ELT(value, i, NA_STRING);
            continue;
        }


        ptr = R_ExpandFileName(translateCharUTF8(STRING_ELT(path, i)));
        nchar = strlen(ptr);
        if (nchar == 0) {
            /* don't bother assigning an empty string, should already be empty */
            // SET_STRING_ELT(value, i, mkChar(""));
            continue;
        }
        drivewidth = get_drive_width(ptr, nchar);
        if (drivewidth == nchar) {
            SET_STRING_ELT(value, i, mkCharCE(ptr, CE_UTF8));
            continue;
        }


        char _buf[nchar];  /* allocate a buffer to hold the dirname */
        buf = _buf;
        strcpy(buf, ptr);
        cbuf = buf + drivewidth;  /* point to the start of the path spec */


        /* point to the last character of buf */
        p = buf + (nchar - 1);


        skip = 0;
        for (j = -1; j < times; j++) {


            /* remove the trailing path separators */
            for (; p >= cbuf; p--) {
                if (*p == '/' || *p == '\\') {}
                else {
                    *(p + 1) = '\0';
                    break;
                }
            }


            /* if the path specification was only comprised of path separators
             * then the dirname is just the whole path */
            if (p < cbuf) {
                SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
                skip = 1;
                break;
            }


            /* find the last path separator */
            ptr_slash     = strrchr(cbuf, '/');
            ptr_backslash = strrchr(cbuf, '\\');


            if (ptr_slash) {  /* slash was found */
                if (ptr_backslash) {  /* backslash was also found */
                    if (ptr_slash < ptr_backslash)  /* slash found before backslash */
                        p = ptr_backslash;
                    else p = ptr_slash;  /* backslash found before slash */
                }
                else p = ptr_slash;  /* backslash was not found */
            }
            else {  /* slash was not found */
                if (ptr_backslash)  /* backslash was found */
                    p = ptr_backslash;
                else {


                    /* for a letter drive with a path without a path separator
                     * e.g. d:test
                     */
                    if (drivewidth) {
                        p = _buf;
                        *(p + drivewidth) = '\0';
                        SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
                    }
                    else SET_STRING_ELT(value, i, mkChar("."));
                    skip = 1;
                    break;
                }
            }


            /* remove the basename */
            *(p + 1) = '\0';


            /* pointer should already point to the last character of buf */
        }
        if (skip) continue;


        /* remove the trailing / and \ */
        for (; p >= cbuf; p--) {
            if (*p == '/' || *p == '\\') {}
            else {
                *(p + 1) = '\0';
                break;
            }
        }


        SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
    }


    UNPROTECT(nprotect);
    return value;
}


SEXP do_unixdirname2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nprotect = 0;


    SEXP path;
    int times;


    int nargs = length(args) - 1;
    if (nargs == 1) {
        path = CADR(args);
        if (TYPEOF(path) != STRSXP)
            error("a character vector argument expected");
        times = 0;
    }
    else if (nargs == 2) {
        path = CADR(args);
        if (TYPEOF(path) != STRSXP)
            error("a character vector argument expected");
        times = asInteger(CADDR(args));
        if (times == NA_INTEGER || times < 0)
            errorcall(call, "invalid second argument, must be coercible to non-negative integer");
    }
    else errorcall(call, "%d arguments passed to 'C_basename2' which requires 1 or 2 arguments", nargs);


    const char *ptr, *cbuf;
    char *buf, *p, *ptr_slash;
    int n, i, j, nchar, drivewidth, skip;


    SEXP value = PROTECT(allocVector(STRSXP, n = LENGTH(path))); nprotect++;
    for (i = 0; i < n; i++) {
        if (STRING_ELT(path, i) == NA_STRING) {
            SET_STRING_ELT(value, i, NA_STRING);
            continue;
        }


        ptr = R_ExpandFileName(translateCharUTF8(STRING_ELT(path, i)));
        nchar = strlen(ptr);
        if (nchar == 0) continue;
        drivewidth = get_drive_width_unix(ptr, nchar);
        if (drivewidth == nchar) {
            SET_STRING_ELT(value, i, mkCharCE(ptr, CE_UTF8));
            continue;
        }


        char _buf[nchar];  /* allocate a buffer to hold the dirname */
        buf = _buf;
        strcpy(buf, ptr);
        cbuf = buf + drivewidth;  /* point to the start of the path spec */


        /* point to the last character of buf */
        p = buf + (nchar - 1);


        skip = 0;
        for (j = -1; j < times; j++) {


            /* remove the trailing path separator */
            for (; p >= cbuf; p--) {
                if (*p == '/') {}
                else {
                    *(p + 1) = '\0';
                    break;
                }
            }


            /* if the path specification was only comprised of path separators
             * then the dirname is just the whole path */
            if (p < cbuf) {
                SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
                skip = 1;
                break;
            }


            /* find the last path separator */
            ptr_slash = strrchr(cbuf, '/');


            if (ptr_slash)  /* slash was found */
                p = ptr_slash;
            else {
                SET_STRING_ELT(value, i, mkChar("."));
                skip = 1;
                break;
            }


            /* remove the basename */
            *(p + 1) = '\0';


            /* pointer should already point to the last character of buf */
        }
        if (skip) continue;


        /* remove the trailing path separator */
        for (; p >= cbuf; p--) {
            if (*p == '/') {}
            else {
                *(p + 1) = '\0';
                break;
            }
        }


        SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
    }


    UNPROTECT(nprotect);
    return value;
}


SEXP do_dirname2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int windows = asLogical(eval(install("os.windows"), rho));
    if (windows == NA_LOGICAL)
        error("invalid 'os.windows'; should never happen, please report!");
    if (windows)
        return do_windowsdirname2(call, op, args, rho);
    else return do_unixdirname2(call, op, args, rho);
}
