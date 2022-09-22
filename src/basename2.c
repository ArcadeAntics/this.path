#include <R.h>
#include <Rinternals.h>


#define debug


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
#ifdef debug
    Rprintf("in C_basename2\n\n");
#endif
    for (i = 0; i < n; i++) {
#ifdef debug
        Rprintf("string %d: ", i + 1);
#endif
        if (STRING_ELT(path, i) == NA_STRING) {
            SET_STRING_ELT(value, i, NA_STRING);
#ifdef debug
            Rprintf("NA\n");
            Rprintf("assigning: NA\n\n");
#endif
        }
        else {
            ptr = R_ExpandFileName(translateCharUTF8(STRING_ELT(path, i)));
            nchar = strlen(ptr);
#ifdef debug
            Rprintf("%s", ptr); Rprintf("\n");
            Rprintf("%d bytes long\n", nchar);
#endif
            if (nchar == 0) {
                /* don't bother assigning an empty string, should already be empty */
                // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
                Rprintf("assigning: \n\n");
#endif
                continue;
            }
            drivewidth = get_drive_width(ptr, nchar);
#ifdef debug
            Rprintf("drivespec is %d bytes long\n", drivewidth);
#endif
            nchar -= drivewidth;  /* number of characters EXCLUDING the drive */
            if (nchar == 0) {
                /* don't bother assigning an empty string, should already be empty */
                // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
                Rprintf("pathspec is 0 bytes long\n");
                Rprintf("assigning: \n\n");
#endif
                continue;
            }
            /* allocate a buffer to hold the basename
             * I made this mistake before, don't forget to add 1 for the trailing '\0'
             */
            char _buf[nchar + 1];
            buf = _buf;
            strcpy(buf, ptr + drivewidth);  /* copy the path specification from ptr to buf */
#ifdef debug
            Rprintf("made a buffer %d bytes long\n", nchar);
            Rprintf("copied to buffer: %s", ptr + drivewidth); Rprintf("\n");
            if (nchar != strlen(ptr + drivewidth)) error("allocated a buffer %d bytes long, but copied %d bytes instead", nchar, strlen(ptr + drivewidth));
#endif
            /* point to the last character of the buf */
            p = buf + (nchar - 1);
#ifdef debug
            Rprintf("made variable p pointing to last byte\n");
            Rprintf("p = %s", p); Rprintf("\n");
#endif
            /* remove the trailing / and \ */
            while (p >= buf &&
                   (
                       *p == '/' ||
                       *p == '\\'
                   ))
            {
                *(p--) = '\0';
            }
#ifdef debug
            Rprintf("after removing trailing slashes: %s", buf); Rprintf("\n");
#endif


            /* if the path specification was only comprised of / and \
             * then the basename is non-existent, return empty string */
            if (p < buf) {
                /* don't bother assigning an empty string, should already be empty */
                // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
                Rprintf("pathspec was /\n");
                Rprintf("assigning: \n\n");
#endif
                continue;
            }


            /* find the last slash/backslash */
            ptr_slash     = strrchr(buf, '/');
            ptr_backslash = strrchr(buf, '\\');


            if (ptr_slash) {  /* slash was found */
                if (ptr_backslash) {  /* backslash was also found */
                    if (ptr_slash < ptr_backslash)  /* slash found before backslash */
                        p = ptr_backslash + 1;
                    else p = ptr_slash + 1;  /* backslash found before slash */
                }
                else p = ptr_slash + 1;  /* backslash was not found */
            }
            else {  /* slash was not found */
                if (ptr_backslash)  /* backslash was found */
                    p = ptr_backslash + 1;
                else p = buf;
            }
            SET_STRING_ELT(value, i, mkCharCE(p, CE_UTF8));
#ifdef debug
            Rprintf("assigning: %s", p); Rprintf("\n\n");
#endif
        }
    }
#ifdef debug
    Rprintf("\n");
#endif


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
#ifdef debug
    Rprintf("in C_basename2\n\n");
#endif
    for (i = 0; i < n; i++) {
#ifdef debug
        Rprintf("string %d: ", i + 1);
#endif
        if (STRING_ELT(path, i) == NA_STRING) {
            SET_STRING_ELT(value, i, NA_STRING);
#ifdef debug
            Rprintf("NA\n");
            Rprintf("assigning: NA\n\n");
#endif
        }
        else {
            ptr = R_ExpandFileName(translateCharUTF8(STRING_ELT(path, i)));
            nchar = strlen(ptr);
#ifdef debug
            Rprintf("%s", ptr); Rprintf("\n");
            Rprintf("%d bytes long\n", nchar);
#endif
            if (nchar == 0) {
                /* don't bother assigning an empty string, should already be empty */
                // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
                Rprintf("assigning: \n\n");
#endif
                continue;
            }
            drivewidth = get_drive_width_unix(ptr, nchar);
#ifdef debug
            Rprintf("drivespec is %d bytes long\n", drivewidth);
#endif
            nchar -= drivewidth;
            if (nchar == 0) {
                /* don't bother assigning an empty string, should already be empty */
                // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
                Rprintf("pathspec is 0 bytes long\n");
                Rprintf("assigning: \n\n");
#endif
                continue;
            }
            char _buf[nchar + 1];  /* allocate a buffer to hold the basename */
            buf = _buf;
            strcpy(buf, ptr + drivewidth);
#ifdef debug
            Rprintf("made a buffer %d bytes long\n", nchar);
            Rprintf("copied to buffer: %s", ptr + drivewidth); Rprintf("\n");
            if (nchar != strlen(ptr + drivewidth)) error("allocated a buffer %d bytes long, but copied %d bytes instead", nchar, strlen(ptr + drivewidth));
#endif
            /* point to the last character of the buf */
            p = buf + (nchar - 1);
#ifdef debug
            Rprintf("made variable p pointing to last byte\n");
            Rprintf("p = %s", p); Rprintf("\n");
#endif
            /* remove the trailing / */
            while (p >= buf && *p == '/') {
                *(p--) = '\0';
            }
#ifdef debug
            Rprintf("after removing trailing slashes: %s", buf); Rprintf("\n");
#endif


            /* if the path specification was only comprised of /
             * then the basename is non-existent, return empty string */
            if (p < buf) {
                /* don't bother assigning an empty string, should already be empty */
                // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
                Rprintf("pathspec was /\n");
                Rprintf("assigning: \n\n");
#endif
                continue;
            }


            /* find the last slash */
            ptr_slash = strrchr(buf, '/');
            if (ptr_slash)
                p = ptr_slash + 1;
            else p = buf;


            SET_STRING_ELT(value, i, mkCharCE(p, CE_UTF8));
#ifdef debug
            Rprintf("assigning: %s", p); Rprintf("\n\n");
#endif
        }
    }
#ifdef debug
    Rprintf("\n");
#endif


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


    /* it's not documented in the R function dirname2() or in man/dirname2.Rd
     * but dirname2() actually accepts 1 or 2 arguments
     *
     * the first argument is always the 'path' argument
     * the second argument (optional) is the number of additional times to
     * calculate dirname2(). for example:
     *
     * .External2(C_dirname2, path, 2)
     *
     * will calculate the dirname() once, then calculate it 2 more times
     * afterward
     *
     * this saves miniscule time (because you don't have to setup a for loop
     * at the R level) but it was easy enough to do so why not?
     */
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
#ifdef debug
    Rprintf("in C_dirname2\n\n");
#endif
    for (i = 0; i < n; i++) {
#ifdef debug
        Rprintf("string %d: ", i + 1);
#endif
        if (STRING_ELT(path, i) == NA_STRING) {
            SET_STRING_ELT(value, i, NA_STRING);
#ifdef debug
            Rprintf("NA\n");
            Rprintf("assigning: NA\n\n");
#endif
            continue;
        }


        ptr = R_ExpandFileName(translateCharUTF8(STRING_ELT(path, i)));
        nchar = strlen(ptr);
#ifdef debug
            Rprintf("%s", ptr); Rprintf("\n");
            Rprintf("%d bytes long\n", nchar);
#endif
        if (nchar == 0) {
            /* don't bother assigning an empty string, should already be empty */
            // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
                Rprintf("assigning: \n\n");
#endif
            continue;
        }
        drivewidth = get_drive_width(ptr, nchar);
#ifdef debug
            Rprintf("drivespec is %d bytes long\n", drivewidth);
#endif
        if (drivewidth == nchar) {
            SET_STRING_ELT(value, i, mkCharCE(ptr, CE_UTF8));
#ifdef debug
                Rprintf("pathspec is 0 bytes long\n");
                Rprintf("assigning: %s", ptr); Rprintf("\n\n");
#endif
            continue;
        }


        char _buf[nchar + 1];  /* allocate a buffer to hold the dirname */
        buf = _buf;
        strcpy(buf, ptr);
#ifdef debug
        Rprintf("made a buffer %d bytes long\n", nchar);
        Rprintf("copied to buffer: %s", ptr); Rprintf("\n");
        if (nchar != strlen(ptr)) error("allocated a buffer %d bytes long, but copied %d bytes instead", nchar, strlen(ptr));
#endif
        cbuf = buf + drivewidth;  /* point to the start of the path spec */


        /* point to the last character of buf */
        p = buf + (nchar - 1);
#ifdef debug
        Rprintf("made variable p pointing to last byte\n");
        Rprintf("p = %s", p); Rprintf("\n");
        Rprintf("made variable cbuf pointing to start of pathspec\n");
        Rprintf("cbuf                           : %s", cbuf); Rprintf("\n");
#endif


        skip = 0;
        for (j = -1; j < times; j++) {


            /* remove the trailing path separators */
            /* we remove the trailing separators a little different than
             * do_basename2()
             * in this case, we only want to remove trailing slashes
             * if there is a non-slash before those trailing slashes
             */
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
#ifdef debug
                Rprintf("pathspec was /\n");
                Rprintf("assigning: %s", buf); Rprintf("\n\n");
#endif
                skip = 1;
                break;
            }
#ifdef debug
            Rprintf("after removing trailing slashes: %s", cbuf); Rprintf("\n");
#endif


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


                    /* for a drive with a pathspec without a path separator
                     * e.g. d:test
                     */
                    if (drivewidth) {
                        p = _buf;
                        *(p + drivewidth) = '\0';
                        SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
#ifdef debug
                Rprintf("path is of the form d:file\n");
                Rprintf("assigning: %s", buf); Rprintf("\n\n");
#endif
                    }
                    else {
                        SET_STRING_ELT(value, i, mkChar("."));
#ifdef debug
                Rprintf("pathspec has no path separators\n");
                Rprintf("assigning: .\n\n");
#endif
                    }
                    skip = 1;
                    break;
                }
            }


            /* remove the basename */
            *(p + 1) = '\0';
#ifdef debug
            Rprintf("after removing basename        : %s", cbuf); Rprintf("\n");
#endif


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
#ifdef debug
        if (p >= cbuf) {
            Rprintf("after removing trailing slashes: %s", cbuf); Rprintf("\n");
        }
#endif


        SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
#ifdef debug
        Rprintf("assigning: %s", buf); Rprintf("\n\n");
#endif
    }
#ifdef debug
    Rprintf("\n");
#endif


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
#ifdef debug
    Rprintf("in C_dirname2\n\n");
#endif
    for (i = 0; i < n; i++) {
#ifdef debug
        Rprintf("string %d: ", i + 1);
#endif
        if (STRING_ELT(path, i) == NA_STRING) {
            SET_STRING_ELT(value, i, NA_STRING);
#ifdef debug
            Rprintf("NA\n");
            Rprintf("assigning: NA\n\n");
#endif
            continue;
        }


        ptr = R_ExpandFileName(translateCharUTF8(STRING_ELT(path, i)));
        nchar = strlen(ptr);
#ifdef debug
            Rprintf("%s", ptr); Rprintf("\n");
            Rprintf("%d bytes long\n", nchar);
#endif
        if (nchar == 0) {
            /* dont bother assigning an empty string, should already be empty */
            // SET_STRING_ELT(value, i, mkChar(""));
#ifdef debug
                Rprintf("assigning: \n\n");
#endif
            continue;
        }
        drivewidth = get_drive_width_unix(ptr, nchar);
#ifdef debug
            Rprintf("drivespec is %d bytes long\n", drivewidth);
#endif
        if (drivewidth == nchar) {
            SET_STRING_ELT(value, i, mkCharCE(ptr, CE_UTF8));
#ifdef debug
                Rprintf("pathspec is 0 bytes long\n");
                Rprintf("assigning: %s", ptr); Rprintf("\n\n");
#endif
            continue;
        }


        char _buf[nchar + 1];  /* allocate a buffer to hold the dirname */
        buf = _buf;
        strcpy(buf, ptr);
#ifdef debug
        Rprintf("made a buffer %d bytes long\n", nchar);
        Rprintf("copied to buffer: %s", ptr); Rprintf("\n");
        if (nchar != strlen(ptr)) error("allocated a buffer %d bytes long, but copied %d bytes instead", nchar, strlen(ptr));
#endif
        cbuf = buf + drivewidth;  /* point to the start of the path spec */


        /* point to the last character of buf */
        p = buf + (nchar - 1);
#ifdef debug
        Rprintf("made variable p pointing to last byte\n");
        Rprintf("p = %s", p); Rprintf("\n");
        Rprintf("made variable cbuf pointing to start of pathspec\n");
        Rprintf("cbuf                           : %s", cbuf); Rprintf("\n");
#endif


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
#ifdef debug
                Rprintf("pathspec was /\n");
                Rprintf("assigning: %s", buf); Rprintf("\n\n");
#endif
                skip = 1;
                break;
            }
#ifdef debug
            Rprintf("after removing trailing slashes: %s", cbuf); Rprintf("\n");
#endif


            /* find the last path separator */
            ptr_slash = strrchr(cbuf, '/');


            if (ptr_slash)  /* slash was found */
                p = ptr_slash;
            else {
                SET_STRING_ELT(value, i, mkChar("."));
#ifdef debug
                Rprintf("pathspec has no path separators\n");
                Rprintf("assigning: .\n\n");
#endif
                skip = 1;
                break;
            }


            /* remove the basename */
            *(p + 1) = '\0';
#ifdef debug
            Rprintf("after removing basename        : %s", cbuf); Rprintf("\n");
#endif


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
#ifdef debug
        if (p >= cbuf) {
            Rprintf("after removing trailing slashes: %s", cbuf); Rprintf("\n");
        }
#endif


        SET_STRING_ELT(value, i, mkCharCE(buf, CE_UTF8));
#ifdef debug
        Rprintf("assigning: %s", buf); Rprintf("\n\n");
#endif
    }
#ifdef debug
    Rprintf("\n");
#endif


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
